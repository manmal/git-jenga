const std = @import("std");

pub const StackBranch = struct {
    name: []const u8,
    commit_sha: []const u8,
    parent_branch: ?[]const u8,
    commits_from_parent: u32,
    needs_fix: bool = false,
    fix: ?Fix = null,
    step_status: StepStatus = .pending,

    pub fn deinit(self: *StackBranch, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.commit_sha);
        if (self.parent_branch) |p| allocator.free(p);
        if (self.fix) |*f| f.deinit(allocator);
    }
};

pub const Fix = struct {
    commit_message: []const u8,
    files: []FileChange,

    pub fn deinit(self: *Fix, allocator: std.mem.Allocator) void {
        allocator.free(self.commit_message);
        for (self.files) |*f| f.deinit(allocator);
        allocator.free(self.files);
    }
};

pub const FileChange = struct {
    path: []const u8,
    change_type: ChangeType,
    diff: []const u8,
    staged: bool,

    pub fn deinit(self: *FileChange, allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        allocator.free(self.diff);
    }
};

pub const ChangeType = enum {
    modified,
    deleted,
    added,

    pub fn toString(self: ChangeType) []const u8 {
        return switch (self) {
            .modified => "modified",
            .deleted => "deleted",
            .added => "added",
        };
    }
};

pub const StepStatus = enum {
    pending,
    cherry_picked,
    fixed,
    verified,
    skipped,

    pub fn toString(self: StepStatus) []const u8 {
        return switch (self) {
            .pending => "pending",
            .cherry_picked => "cherry_picked",
            .fixed => "fixed",
            .verified => "verified",
            .skipped => "skipped",
        };
    }
};

pub const Stack = struct {
    branches: []StackBranch,
    base_branch: []const u8,
    base_commit: []const u8,
    head_branch: []const u8,
    head_commit: []const u8,

    pub fn deinit(self: *Stack, allocator: std.mem.Allocator) void {
        for (self.branches) |*b| b.deinit(allocator);
        allocator.free(self.branches);
        allocator.free(self.base_branch);
        allocator.free(self.base_commit);
        allocator.free(self.head_branch);
        allocator.free(self.head_commit);
    }
};

pub const PlanError = struct {
    error_type: ErrorType,
    path: []const u8,
    message: []const u8,

    pub fn deinit(self: *PlanError, allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        allocator.free(self.message);
    }
};

pub const ErrorType = enum {
    unmapped_file,
    outside_ancestry,
    ambiguous_branch,

    pub fn toString(self: ErrorType) []const u8 {
        return switch (self) {
            .unmapped_file => "unmapped_file",
            .outside_ancestry => "outside_ancestry",
            .ambiguous_branch => "ambiguous_branch",
        };
    }
};

pub const Plan = struct {
    version: u32 = 1,
    generated: []const u8,
    repository: []const u8,
    verify_cmd: ?[]const u8 = null,
    errors: []PlanError,
    stack: Stack,

    pub fn deinit(self: *Plan, allocator: std.mem.Allocator) void {
        allocator.free(self.generated);
        allocator.free(self.repository);
        if (self.verify_cmd) |cmd| allocator.free(cmd);
        for (self.errors) |*e| e.deinit(allocator);
        allocator.free(self.errors);
        self.stack.deinit(allocator);
    }
};

pub const ExecutionMode = enum {
    exec,
    step,

    pub fn toString(self: ExecutionMode) []const u8 {
        return switch (self) {
            .exec => "exec",
            .step => "step",
        };
    }

    pub fn fromString(s: []const u8) ?ExecutionMode {
        if (std.mem.eql(u8, s, "exec")) return .exec;
        if (std.mem.eql(u8, s, "step")) return .step;
        return null;
    }
};

pub const ExecutionState = struct {
    plan_file: []const u8,
    plan_hash: []const u8,
    worktree_path: []const u8,
    current_step_index: u32,
    current_commit_index: u32 = 0,
    started_at: []const u8,
    last_updated: []const u8,
    status: ExecutionStatus,
    completed_branches: [][]const u8,
    verify_cmd: ?[]const u8 = null,
    mode: ExecutionMode = .exec,
};

pub const ExecutionStatus = enum {
    pending,
    in_progress,
    conflict,
    completed,
    failed,
    aborted,

    pub fn toString(self: ExecutionStatus) []const u8 {
        return switch (self) {
            .pending => "pending",
            .in_progress => "in_progress",
            .conflict => "conflict",
            .completed => "completed",
            .failed => "failed",
            .aborted => "aborted",
        };
    }
};

pub const JengaError = error{
    GitCommandFailed,
    GitNotInsideRepository,
    NotAGitRepo,
    BaseBranchNotFound,
    CurrentBranchUnknown,
    BranchAmbiguous,
    UnmappedFiles,
    ProcessError,
    ParseError,
    OutOfMemory,
    PlanHasErrors,
    StateFileCorrupted,
    WorktreeExists,
    ConflictDetected,
    PlanNotFound,
    InvalidPlan,
};
