<p align="center">
  <img src="assets/jenga.png" width="420" alt="git-jenga" />
</p>

# git-jenga

[![CI](https://github.com/manmal/git-jenga/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/manmal/git-jenga/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Zig](https://img.shields.io/badge/Zig-0.15.2-f7a41d?logo=zig&logoColor=white)](https://ziglang.org/)

Stack-aware restacking for Git feature branch hierarchies. git-jenga creates a deterministic plan, captures conflict resolutions, and replays fixes on top of each branch without rerere.

## Why

- Detect conflicts early while planning.
- Resolve conflicts once and replay the resolution across exec runs.
- Restack large branch stacks without guessing which fixes belong where.

## Quick start

```sh
zig build
./zig-out/bin/git-jenga plan
./zig-out/bin/git-jenga exec --force
./zig-out/bin/git-jenga apply
```

## Workflow

1. Make changes on the current stack head.
2. `git-jenga plan` creates a plan file and captures conflicts.
3. Inspect `.git/git-jenga/plan.yml`.
4. `git-jenga exec --force` creates `-fix` branches with your changes.
5. `git-jenga apply` updates original branches to the `-fix` tips.

## Conflict resolution

- Conflicts are detected during `plan` by replaying the stack on top of the base branch.
- `--mergetool <tool>` chooses a specific tool; git config and `GIT_MERGETOOL` are respected.
- Resolutions are stored in `plan.yml` so `exec` is non-interactive.

## Real-world test suite

Run the full suite of real-world scenarios locally or in CI:

```sh
./scripts/run_real_world_tests.sh
```

Notes:
- Tests create temp repos in the parent directory of this repo.
- Submodule scenarios require `protocol.file.allow=always` (set automatically in the script).

## CI

CI runs on Linux only and executes:
- `zig build`
- `./test/run_integration_tests.sh`
- `./scripts/run_real_world_tests.sh`

## License

MIT. See `LICENSE`.
