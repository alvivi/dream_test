# Contributing to dream_test

Thank you for your interest in contributing! This guide covers everything you need to get started.

---

## Quick Reference

| Need to know...    | Read...                                   |
| ------------------ | ----------------------------------------- |
| API reference      | [Hexdocs](https://hexdocs.pm/dream_test/) |
| Quick start        | [README.md](README.md)                    |
| Coding conventions | [STANDARDS.md](STANDARDS.md)              |

---

## Code of Conduct

Be respectful. Be inclusive. Focus on what's best for the project. Accept constructive criticism gracefully.

---

## Getting Started

### Prerequisites

- [Gleam](https://gleam.run/) v1.0.0+
- Erlang/OTP 26+
- Git
- Make (optional but recommended)

### Setup

```bash
# Clone your fork
git clone https://github.com/YOUR_USERNAME/dream_test.git
cd dream_test

# Add upstream remote
git remote add upstream https://github.com/TrustBound/dream_test.git

# Build and test
make all
```

If everything passes, you're ready to contribute.

---

## Development Workflow

### 1. Create a branch

```bash
git checkout main
git pull upstream main
git checkout -b feature/your-feature-name
```

Use `feature/` for new features, `fix/` for bug fixes.

### 2. Make changes

- Write clear, focused commits
- Follow [STANDARDS.md](STANDARDS.md) conventions
- Add tests for new functionality
- Update docs if the public API changes

### 3. Test

```bash
make all          # Full build + test + format
make test         # Tests only
make build        # Compile only
make format       # Format code
```

### 4. Commit

```bash
git add .
git commit -m "Add feature: brief description"
```

**Commit message format:**

- Present tense ("Add" not "Added")
- Imperative mood ("Move cursor to..." not "Moves cursor to...")
- First line ≤ 50 characters
- Reference issues with `#123`

```
Add timeout support to test runner

Implements configurable timeouts for individual tests.
Tests that exceed their timeout are marked as failed.

Closes #123
```

### 5. Push and open PR

```bash
git push origin feature/your-feature-name
```

Then open a pull request on GitHub.

---

## Writing Tests

All new functionality must include tests.

### Test location

Add tests in the appropriate `test/dream_test/` subdirectory matching the source structure.

### Test structure

```gleam
import dream_test/unit.{describe, it}
import dream_test/assertions/should.{should, equal, fail_with, or_fail_with}
import dream_test/types.{AssertionOk, MatchFailed, MatchOk}

pub fn tests() {
  describe("Feature name", [
    it("does something when condition", fn() {
      let result = some_function()

      case result {
        MatchOk(_) -> AssertionOk
        MatchFailed(_) -> fail_with("Expected success")
      }
    }),
  ])
}
```

### What to test

- Success cases
- Failure cases
- Edge cases
- Error messages (when relevant)

See [hexdocs](https://hexdocs.pm/dream_test/) for the complete assertion API.

---

## Documentation

### Code comments

All public functions need `///` doc comments:

````gleam
/// Checks if the value equals the expected value.
///
/// Returns `MatchOk` if equal, `MatchFailed` with details otherwise.
///
/// ## Example
///
/// ```gleam
/// 42
/// |> should()
/// |> equal(42)
/// |> or_fail_with("Should be 42")
/// ```
pub fn equal(result: MatchResult(a), expected: a) -> MatchResult(a) {
  // ...
}
````

### Updating docs

| Changed...     | Update...                      |
| -------------- | ------------------------------ |
| Public API     | Hexdocs (update code comments) |
| Major features | [README.md](README.md)         |
| Coding rules   | [STANDARDS.md](STANDARDS.md)   |

---

## Pull Request Process

### Before submitting

- [ ] Tests pass (`make all`)
- [ ] Code follows [STANDARDS.md](STANDARDS.md)
- [ ] New code has tests
- [ ] Docs updated if needed
- [ ] Commits are clear and atomic

### PR template

```markdown
## Summary

What does this PR do?

## Motivation

Why is this change needed?

## Changes

- Change 1
- Change 2

## Testing

How was this tested?

## Related

Closes #123
```

### Review process

1. Maintainer reviews your PR
2. Address feedback
3. PR gets approved and merged
4. 🎉

---

## What We're Looking For

### High priority

- **Bug fixes** with test cases demonstrating the issue
- **Documentation** improvements
- **New matchers** for common assertion patterns
- **Reporters** (JSON, JUnit, TAP)
- **Test coverage** for untested paths

### Welcome additions

- Performance optimizations (with benchmarks)
- Better error messages
- Real-world examples
- Integration guides

### Discuss first

Open an issue before starting work on:

- Architecture changes
- Breaking API changes
- New dependencies
- Large features

---

## Questions?

| Type              | Where                              |
| ----------------- | ---------------------------------- |
| General questions | GitHub Discussions                 |
| Bug reports       | GitHub Issues                      |
| Feature requests  | GitHub Issues (label: enhancement) |
| Security issues   | Email maintainers directly         |

---

<div align="center">
  <sub>Thank you for contributing! 💚</sub>
</div>
