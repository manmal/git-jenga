const std = @import("std");
const types = @import("../types.zig");
const strings = @import("../utils/strings.zig");

/// Parse a YAML plan file strictly
pub fn parsePlan(allocator: std.mem.Allocator, content: []const u8) !types.Plan {
    var parser = Parser.init(allocator, content);
    return parser.parse();
}

const Parser = struct {
    allocator: std.mem.Allocator,
    content: []const u8,
    lines: std.mem.SplitIterator(u8, .scalar),
    line_number: usize,
    current_line: ?[]const u8,
    peeked_line: ?[]const u8, // For "unreading" a line

    fn init(allocator: std.mem.Allocator, content: []const u8) Parser {
        return .{
            .allocator = allocator,
            .content = content,
            .lines = std.mem.splitScalar(u8, content, '\n'),
            .line_number = 0,
            .current_line = null,
            .peeked_line = null,
        };
    }

    fn nextLine(self: *Parser) ?[]const u8 {
        return self.nextLineImpl(true);
    }

    /// Get next line, optionally skipping empty lines and comments
    fn nextLineImpl(self: *Parser, skip_empty: bool) ?[]const u8 {
        // If we have a peeked line, return it first
        if (self.peeked_line) |line| {
            self.peeked_line = null;
            self.current_line = line;
            return line;
        }
        
        while (self.lines.next()) |line| {
            self.line_number += 1;
            if (skip_empty) {
                const trimmed = strings.trim(line);
                if (trimmed.len == 0 or trimmed[0] == '#') {
                    continue;
                }
            }
            self.current_line = line;
            return line;
        }
        self.current_line = null;
        return null;
    }
    
    /// Get next line including empty/whitespace lines (for literal blocks)
    fn nextLineRaw(self: *Parser) ?[]const u8 {
        return self.nextLineImpl(false);
    }
    
    fn unreadLine(self: *Parser, line: []const u8) void {
        self.peeked_line = line;
    }

    fn parse(self: *Parser) !types.Plan {
        var version: u32 = 1;
        var generated: []const u8 = "";
        var repository: []const u8 = "";
        var verify_cmd: ?[]const u8 = null;
        var simulation: ?types.PlanSimulation = null;
        var errors: std.ArrayListUnmanaged(types.PlanError) = .{};
        var conflicts: std.ArrayListUnmanaged(types.PlanConflict) = .{};
        var branches: std.ArrayListUnmanaged(types.StackBranch) = .{};
        var head_branch: []const u8 = "";
        var head_commit: []const u8 = "";
        var base_branch: []const u8 = "";
        var base_commit: []const u8 = "";
        var base_tip: []const u8 = "";

        while (self.nextLine()) |line| {
            const key = self.getKey(line) orelse continue;
            const value = self.getValue(line);

            if (std.mem.eql(u8, key, "version")) {
                version = std.fmt.parseInt(u32, value, 10) catch 1;
            } else if (std.mem.eql(u8, key, "generated")) {
                generated = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "repository")) {
                repository = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "verify_cmd")) {
                verify_cmd = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "simulation")) {
                simulation = try self.parseSimulation();
            } else if (std.mem.eql(u8, key, "errors")) {
                errors = try self.parseErrors();
            } else if (std.mem.eql(u8, key, "conflicts")) {
                conflicts = try self.parseConflicts();
            } else if (std.mem.eql(u8, key, "source")) {
                const source = try self.parseSource();
                head_branch = source.head_branch;
                head_commit = source.head_commit;
                base_branch = source.base_branch;
                base_commit = source.base_commit;
                base_tip = source.base_tip;
            } else if (std.mem.eql(u8, key, "stack")) {
                branches = try self.parseStack();
            }
        }

        return types.Plan{
            .version = version,
            .generated = generated,
            .repository = repository,
            .verify_cmd = verify_cmd,
            .simulation = simulation,
            .errors = try errors.toOwnedSlice(self.allocator),
            .conflicts = try conflicts.toOwnedSlice(self.allocator),
            .stack = .{
                .branches = try branches.toOwnedSlice(self.allocator),
                .base_branch = base_branch,
                .base_commit = base_commit,
                .base_tip = if (base_tip.len > 0) base_tip else try strings.copy(self.allocator, base_commit),
                .head_branch = head_branch,
                .head_commit = head_commit,
            },
        };
    }

    fn getKey(self: *Parser, line: []const u8) ?[]const u8 {
        _ = self;
        const trimmed = strings.trim(line);
        if (std.mem.indexOfScalar(u8, trimmed, ':')) |idx| {
            return trimmed[0..idx];
        }
        return null;
    }

    fn getValue(self: *Parser, line: []const u8) []const u8 {
        _ = self;
        const trimmed = strings.trim(line);
        if (std.mem.indexOfScalar(u8, trimmed, ':')) |idx| {
            if (idx + 1 < trimmed.len) {
                return strings.trim(trimmed[idx + 1 ..]);
            }
        }
        return "";
    }

    fn parseQuotedString(self: *Parser, value: []const u8) ![]const u8 {
        var v = value;
        if (v.len >= 2 and v[0] == '"' and v[v.len - 1] == '"') {
            v = v[1 .. v.len - 1];
        }
        return strings.copy(self.allocator, v);
    }

    fn parseErrors(self: *Parser) !std.ArrayListUnmanaged(types.PlanError) {
        var errors: std.ArrayListUnmanaged(types.PlanError) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent == 0) {
                self.unreadLine(line);
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "[]")) {
                break;
            }

            if (std.mem.startsWith(u8, trimmed, "- type:")) {
                var err = types.PlanError{
                    .error_type = .unmapped_file,
                    .path = "",
                    .message = "",
                };

                const type_str = self.getValue(line);
                if (std.mem.eql(u8, type_str, "unmapped_file")) {
                    err.error_type = .unmapped_file;
                } else if (std.mem.eql(u8, type_str, "outside_ancestry")) {
                    err.error_type = .outside_ancestry;
                } else if (std.mem.eql(u8, type_str, "ambiguous_branch")) {
                    err.error_type = .ambiguous_branch;
                }

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.unreadLine(next_line);
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;
                    const next_value = self.getValue(next_line);

                    if (std.mem.eql(u8, next_key, "path")) {
                        err.path = try self.parseQuotedString(next_value);
                    } else if (std.mem.eql(u8, next_key, "message")) {
                        err.message = try self.parseQuotedString(next_value);
                    }
                }

                try errors.append(self.allocator, err);
            }
        }

        return errors;
    }

    fn parseSimulation(self: *Parser) !types.PlanSimulation {
        var mode: types.PlanMode = .worktree;
        var worktree_path: ?[]const u8 = null;
        var plan_branch: ?[]const u8 = null;
        var backups: std.ArrayListUnmanaged(types.BackupBranch) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent == 0) {
                self.unreadLine(line);
                break;
            }

            const key = self.getKey(line) orelse continue;
            const value = self.getValue(line);

            if (std.mem.eql(u8, key, "mode")) {
                mode = self.parsePlanMode(value);
            } else if (std.mem.eql(u8, key, "worktree_path")) {
                if (!std.mem.eql(u8, value, "null")) {
                    worktree_path = try self.parseQuotedString(value);
                }
            } else if (std.mem.eql(u8, key, "plan_branch")) {
                if (!std.mem.eql(u8, value, "null")) {
                    plan_branch = try self.parseQuotedString(value);
                }
            } else if (std.mem.eql(u8, key, "backup_branches")) {
                backups = try self.parseBackupBranches(indent);
            }
        }

        return types.PlanSimulation{
            .mode = mode,
            .worktree_path = worktree_path,
            .plan_branch = plan_branch,
            .backup_branches = try backups.toOwnedSlice(self.allocator),
        };
    }

    fn parseBackupBranches(self: *Parser, parent_indent: usize) !std.ArrayListUnmanaged(types.BackupBranch) {
        var backups: std.ArrayListUnmanaged(types.BackupBranch) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent <= parent_indent) {
                self.unreadLine(line);
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "[]")) {
                break;
            }

            if (std.mem.startsWith(u8, trimmed, "- source:")) {
                var backup = types.BackupBranch{
                    .source = try self.parseQuotedString(self.getValue(line)),
                    .backup = "",
                };

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.unreadLine(next_line);
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;
                    const next_value = self.getValue(next_line);

                    if (std.mem.eql(u8, next_key, "backup")) {
                        backup.backup = try self.parseQuotedString(next_value);
                    }
                }

                try backups.append(self.allocator, backup);
            }
        }

        return backups;
    }

    fn parseConflicts(self: *Parser) !std.ArrayListUnmanaged(types.PlanConflict) {
        var conflicts: std.ArrayListUnmanaged(types.PlanConflict) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent == 0) {
                self.unreadLine(line);
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "[]")) {
                break;
            }

            if (std.mem.startsWith(u8, trimmed, "- kind:")) {
                var conflict = types.PlanConflict{
                    .kind = self.parseConflictKind(self.getValue(line)),
                    .branch = "",
                    .commit = null,
                    .subject = null,
                    .files = try self.allocator.alloc(types.ConflictFile, 0),
                };

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.unreadLine(next_line);
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;
                    const next_value = self.getValue(next_line);

                    if (std.mem.eql(u8, next_key, "branch")) {
                        conflict.branch = try self.parseQuotedString(next_value);
                    } else if (std.mem.eql(u8, next_key, "commit")) {
                        if (!std.mem.eql(u8, next_value, "null")) {
                            conflict.commit = try self.parseQuotedString(next_value);
                        }
                    } else if (std.mem.eql(u8, next_key, "subject")) {
                        if (!std.mem.eql(u8, next_value, "null")) {
                            conflict.subject = try self.parseQuotedString(next_value);
                        }
                    } else if (std.mem.eql(u8, next_key, "files")) {
                        self.allocator.free(conflict.files);
                        var files_list = try self.parseConflictFiles(next_indent);
                        conflict.files = try files_list.toOwnedSlice(self.allocator);
                    }
                }

                try conflicts.append(self.allocator, conflict);
            }
        }

        return conflicts;
    }

    fn parseConflictFiles(self: *Parser, parent_indent: usize) !std.ArrayListUnmanaged(types.ConflictFile) {
        var files: std.ArrayListUnmanaged(types.ConflictFile) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent <= parent_indent) {
                self.unreadLine(line);
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "[]")) {
                break;
            }

            if (std.mem.startsWith(u8, trimmed, "- path:")) {
                var file = types.ConflictFile{
                    .path = try self.parseQuotedString(self.getValue(line)),
                    .conflict_diff = try strings.copy(self.allocator, ""),
                    .resolution = .{
                        .present = true,
                        .encoding = .text,
                        .content = try strings.copy(self.allocator, ""),
                    },
                };

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.unreadLine(next_line);
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;

                    if (std.mem.eql(u8, next_key, "conflict_diff")) {
                        file.conflict_diff = try self.parseLiteralBlock(next_indent);
                    } else if (std.mem.eql(u8, next_key, "resolution")) {
                        file.resolution = try self.parseResolution(next_indent);
                    }
                }

                try files.append(self.allocator, file);
            }
        }

        return files;
    }

    fn parseResolution(self: *Parser, parent_indent: usize) !types.ConflictResolution {
        var resolution = types.ConflictResolution{
            .present = false,
            .encoding = .text,
            .content = try strings.copy(self.allocator, ""),
        };

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent <= parent_indent) {
                self.unreadLine(line);
                break;
            }

            const key = self.getKey(line) orelse continue;
            const value = self.getValue(line);

            if (std.mem.eql(u8, key, "present")) {
                resolution.present = std.mem.eql(u8, value, "true");
            } else if (std.mem.eql(u8, key, "encoding")) {
                resolution.encoding = self.parseResolutionEncoding(value);
            } else if (std.mem.eql(u8, key, "content")) {
                self.allocator.free(resolution.content);
                resolution.content = try self.parseLiteralBlock(indent);
            }
        }

        return resolution;
    }

    const SourceResult = struct {
        head_branch: []const u8,
        head_commit: []const u8,
        base_branch: []const u8,
        base_commit: []const u8,
        base_tip: []const u8,
    };

    fn parseSource(self: *Parser) !SourceResult {
        var result: SourceResult = .{
            .head_branch = "",
            .head_commit = "",
            .base_branch = "",
            .base_commit = "",
            .base_tip = "",
        };

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent == 0) {
                self.unreadLine(line);
                break;
            }

            const key = self.getKey(line) orelse continue;
            const value = self.getValue(line);

            if (std.mem.eql(u8, key, "head_branch")) {
                result.head_branch = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "head_commit")) {
                result.head_commit = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "base_branch")) {
                result.base_branch = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "base_commit")) {
                result.base_commit = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "base_tip")) {
                result.base_tip = try self.parseQuotedString(value);
            }
        }

        return result;
    }

    fn parseStack(self: *Parser) !std.ArrayListUnmanaged(types.StackBranch) {
        var branches: std.ArrayListUnmanaged(types.StackBranch) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent == 0) {
                self.unreadLine(line);
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "- branch:")) {
                var branch = types.StackBranch{
                    .name = try self.parseQuotedString(self.getValue(line)),
                    .commit_sha = "",
                    .parent_branch = null,
                    .commits_from_parent = 0,
                    .needs_fix = false,
                    .fix = null,
                    .step_status = .pending,
                };

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.unreadLine(next_line);
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;
                    const next_value = self.getValue(next_line);

                    if (std.mem.eql(u8, next_key, "commit")) {
                        branch.commit_sha = try self.parseQuotedString(next_value);
                    } else if (std.mem.eql(u8, next_key, "parent_branch")) {
                        if (!std.mem.eql(u8, next_value, "null")) {
                            branch.parent_branch = try self.parseQuotedString(next_value);
                        }
                    } else if (std.mem.eql(u8, next_key, "commits_from_parent")) {
                        branch.commits_from_parent = std.fmt.parseInt(u32, next_value, 10) catch 0;
                    } else if (std.mem.eql(u8, next_key, "needs_fix")) {
                        branch.needs_fix = std.mem.eql(u8, next_value, "true");
                    } else if (std.mem.eql(u8, next_key, "step_status")) {
                        branch.step_status = self.parseStepStatus(next_value);
                    } else if (std.mem.eql(u8, next_key, "fix")) {
                        branch.fix = try self.parseFix(next_indent);
                    }
                }

                try branches.append(self.allocator, branch);
            }
        }

        return branches;
    }

    fn parseFix(self: *Parser, parent_indent: usize) !types.Fix {
        var commit_message: []const u8 = "";
        var files: std.ArrayListUnmanaged(types.FileChange) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent <= parent_indent) {
                self.unreadLine(line);
                break;
            }

            const key = self.getKey(line) orelse continue;

            if (std.mem.eql(u8, key, "commit_message")) {
                commit_message = try self.parseLiteralBlock(indent);
            } else if (std.mem.eql(u8, key, "files")) {
                files = try self.parseFiles(indent);
            }
        }

        return types.Fix{
            .commit_message = commit_message,
            .files = try files.toOwnedSlice(self.allocator),
        };
    }

    fn parseFiles(self: *Parser, parent_indent: usize) !std.ArrayListUnmanaged(types.FileChange) {
        var files: std.ArrayListUnmanaged(types.FileChange) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent <= parent_indent) {
                self.unreadLine(line);
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "- path:")) {
                var file = types.FileChange{
                    .path = try self.parseQuotedString(self.getValue(line)),
                    .change_type = .modified,
                    .diff = "",
                    .staged = true,
                };

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.unreadLine(next_line);
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;
                    const next_value = self.getValue(next_line);

                    if (std.mem.eql(u8, next_key, "change_type")) {
                        file.change_type = self.parseChangeType(next_value);
                    } else if (std.mem.eql(u8, next_key, "staged")) {
                        file.staged = std.mem.eql(u8, next_value, "true");
                    } else if (std.mem.eql(u8, next_key, "diff")) {
                        file.diff = try self.parseLiteralBlock(next_indent);
                    }
                }

                try files.append(self.allocator, file);
            }
        }

        return files;
    }

    fn parseLiteralBlock(self: *Parser, parent_indent: usize) ![]const u8 {
        var buffer: std.ArrayListUnmanaged(u8) = .{};
        errdefer buffer.deinit(self.allocator);

        // For YAML literal blocks (|), we need to determine the base indentation
        // from the first content line and strip exactly that many spaces from each line.
        // This preserves meaningful leading spaces in the content (like diff context lines).
        var base_indent: ?usize = null;

        // Use nextLineRaw to include empty/whitespace lines which are significant in literal blocks
        while (self.nextLineRaw()) |line| {
            const indent = self.getIndent(line);
            
            // For lines that appear empty or whitespace-only, we still need to check
            // if they're part of the block and preserve any content after base indent
            const trimmed = strings.trim(line);
            
            // Block ends when we see a non-empty line at or below the parent indent level
            if (trimmed.len > 0 and indent <= parent_indent) {
                self.unreadLine(line);
                break;
            }

            // Set base indentation from first non-empty content line
            if (base_indent == null and trimmed.len > 0) {
                base_indent = indent;
            }

            // For truly empty lines or lines before we establish base_indent, just add newline
            if (base_indent == null) {
                try buffer.append(self.allocator, '\n');
                continue;
            }

            // Strip exactly base_indent spaces, preserving any additional content
            // This is important for diff context lines that may be just a space
            const strip_count = @min(base_indent.?, line.len);
            const content = line[strip_count..];
            try buffer.appendSlice(self.allocator, content);
            try buffer.append(self.allocator, '\n');
        }

        // Keep trailing newline - important for git patches
        return buffer.toOwnedSlice(self.allocator);
    }

    fn getIndent(self: *Parser, line: []const u8) usize {
        _ = self;
        var count: usize = 0;
        for (line) |c| {
            if (c == ' ') {
                count += 1;
            } else {
                break;
            }
        }
        return count;
    }

    fn parseStepStatus(self: *Parser, value: []const u8) types.StepStatus {
        _ = self;
        if (std.mem.eql(u8, value, "pending")) return .pending;
        if (std.mem.eql(u8, value, "cherry_picked")) return .cherry_picked;
        if (std.mem.eql(u8, value, "fixed")) return .fixed;
        if (std.mem.eql(u8, value, "verified")) return .verified;
        if (std.mem.eql(u8, value, "skipped")) return .skipped;
        return .pending;
    }

    fn parsePlanMode(self: *Parser, value: []const u8) types.PlanMode {
        _ = self;
        return types.PlanMode.fromString(value) orelse .worktree;
    }

    fn parseConflictKind(self: *Parser, value: []const u8) types.ConflictKind {
        _ = self;
        return types.ConflictKind.fromString(value) orelse .cherry_pick;
    }

    fn parseResolutionEncoding(self: *Parser, value: []const u8) types.ResolutionEncoding {
        _ = self;
        return types.ResolutionEncoding.fromString(value) orelse .text;
    }

    fn parseChangeType(self: *Parser, value: []const u8) types.ChangeType {
        _ = self;
        if (std.mem.eql(u8, value, "added")) return .added;
        if (std.mem.eql(u8, value, "deleted")) return .deleted;
        return .modified;
    }
};

/// Parse execution state from JSON
pub fn parseState(allocator: std.mem.Allocator, content: []const u8) !types.ExecutionState {
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, content, .{});
    defer parsed.deinit();

    const root = parsed.value.object;

    var completed: std.ArrayListUnmanaged([]const u8) = .{};
    if (root.get("completed_branches")) |branches| {
        for (branches.array.items) |item| {
            try completed.append(allocator, try strings.copy(allocator, item.string));
        }
    }

    // Parse verify_cmd if present and not null
    var verify_cmd: ?[]const u8 = null;
    if (root.get("verify_cmd")) |v| {
        if (v != .null) {
            verify_cmd = try strings.copy(allocator, v.string);
        }
    }

    // Parse mode if present, default to exec for backwards compatibility
    var mode: types.ExecutionMode = .exec;
    if (root.get("mode")) |v| {
        if (v != .null) {
            mode = types.ExecutionMode.fromString(v.string) orelse .exec;
        }
    }

    // Parse current_commit_index if present, default to 0
    var current_commit_index: u32 = 0;
    if (root.get("current_commit_index")) |v| {
        current_commit_index = @intCast(v.integer);
    }

    return types.ExecutionState{
        .plan_file = try strings.copy(allocator, root.get("plan_file").?.string),
        .plan_hash = try strings.copy(allocator, root.get("plan_hash").?.string),
        .worktree_path = try strings.copy(allocator, root.get("worktree_path").?.string),
        .current_step_index = @intCast(root.get("current_step_index").?.integer),
        .current_commit_index = current_commit_index,
        .started_at = try strings.copy(allocator, root.get("started_at").?.string),
        .last_updated = try strings.copy(allocator, root.get("last_updated").?.string),
        .status = parseExecutionStatus(root.get("status").?.string),
        .completed_branches = try completed.toOwnedSlice(allocator),
        .verify_cmd = verify_cmd,
        .mode = mode,
    };
}

fn parseExecutionStatus(value: []const u8) types.ExecutionStatus {
    if (std.mem.eql(u8, value, "pending")) return .pending;
    if (std.mem.eql(u8, value, "in_progress")) return .in_progress;
    if (std.mem.eql(u8, value, "conflict")) return .conflict;
    if (std.mem.eql(u8, value, "completed")) return .completed;
    if (std.mem.eql(u8, value, "failed")) return .failed;
    if (std.mem.eql(u8, value, "aborted")) return .aborted;
    return .pending;
}
