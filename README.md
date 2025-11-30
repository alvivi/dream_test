<div align="center">
  <img src="https://raw.githubusercontent.com/TrustBound/dream/main/ricky_and_lucy.png" alt="Dream Logo" width="180">
  <h1>dream_test</h1>
  <p><strong>A testing framework for Gleam that gets out of your way.</strong></p>

  <a href="https://hex.pm/packages/dream_test">
    <img src="https://img.shields.io/hexpm/v/dream_test?color=8e4bff&label=hex" alt="Hex.pm">
  </a>
  <a href="https://hexdocs.pm/dream_test/">
    <img src="https://img.shields.io/badge/docs-hexdocs-8e4bff" alt="Documentation">
  </a>
  <a href="https://github.com/TrustBound/dream_test/blob/main/LICENSE">
    <img src="https://img.shields.io/badge/license-MIT-blue" alt="License">
  </a>
</div>

<br>

```gleam
import dream_test/unit.{describe, it}
import dream_test/assertions/should.{should, equal, be_ok, or_fail_with}

pub fn tests() {
  describe("User authentication", [
    it("accepts valid credentials", fn() {
      authenticate("alice", "correct-password")
      |> should()
      |> be_ok()
      |> or_fail_with("Valid credentials should authenticate")
    }),

    it("rejects invalid passwords", fn() {
      authenticate("alice", "wrong-password")
      |> should()
      |> equal(Error("Invalid credentials"))
      |> or_fail_with("Wrong password should fail")
    }),
  ])
}
```

```
User authentication
  ✓ accepts valid credentials
  ✓ rejects invalid passwords

2 tests, 0 failures
```

---

## Installation

```toml
# gleam.toml
[dev-dependencies]
dream_test = "~> 0.1"
```

## Why dream_test?

**Gleam-native.** Pipe-first assertions that feel natural. No macros, no reflection, no magic.

**Familiar syntax.** If you've used Jest, RSpec, or Mocha, you already know how to write tests.

**Type-safe.** Your tests are just Gleam code. The compiler catches mistakes before you run anything.

**Self-hosting.** dream_test tests itself. We eat our own cooking.

---

## Writing Tests

### Structure with `describe` and `it`

Group related tests with `describe`. Define individual cases with `it`.

```gleam
import dream_test/unit.{describe, it}

pub fn tests() {
  describe("String", [
    describe("trim", [
      it("removes leading whitespace", fn() { ... }),
      it("removes trailing whitespace", fn() { ... }),
    ]),

    describe("split", [
      it("splits on delimiter", fn() { ... }),
      it("returns original when delimiter not found", fn() { ... }),
    ]),
  ])
}
```

### Assertions with `should`

Every assertion starts with `should()` and ends with `or_fail_with()`:

```gleam
import dream_test/assertions/should.{should, equal, or_fail_with}

// The pattern: value |> should() |> matcher() |> or_fail_with("message")

result
|> should()
|> equal(42)
|> or_fail_with("Result should be 42")
```

### Chaining Matchers

Matchers can be chained. Each matcher passes its unwrapped value to the next:

```gleam
import dream_test/assertions/should.{should, be_some, be_ok, equal, or_fail_with}

// Unwrap Some, then check the value
Some(42)
|> should()
|> be_some()
|> equal(42)
|> or_fail_with("Should contain 42")

// Unwrap Ok, then check the value
Ok("hello")
|> should()
|> be_ok()
|> equal("hello")
|> or_fail_with("Should be Ok with 'hello'")
```

### Available Matchers

| Category        | Matchers                                                                                    |
| --------------- | ------------------------------------------------------------------------------------------- |
| **Equality**    | `equal`, `not_equal`                                                                        |
| **Boolean**     | `be_true`, `be_false`                                                                       |
| **Option**      | `be_some`, `be_none`                                                                        |
| **Result**      | `be_ok`, `be_error`                                                                         |
| **Collections** | `contain`, `not_contain`, `have_length`, `be_empty`                                         |
| **Comparison**  | `be_greater_than`, `be_less_than`, `be_at_least`, `be_at_most`, `be_between`, `be_in_range` |
| **String**      | `start_with`, `end_with`, `contain_string`                                                  |

### Explicit Failures

When you need to fail unconditionally:

```gleam
import dream_test/assertions/should.{fail_with}

case result {
  Ok(_) -> fail_with("Should have returned an error")
  Error(_) -> ...
}
```

---

## Running Tests

### Basic Setup

```gleam
// test/my_app_test.gleam
import dream_test/unit.{describe, it, to_test_cases}
import dream_test/runner.{run_all}
import dream_test/reporter/bdd.{report}
import gleam/io

pub fn main() {
  tests()
  |> to_test_cases("my_app_test")
  |> run_all()
  |> report(io.println)
}

pub fn tests() {
  describe("MyApp", [
    it("works", fn() { ... }),
  ])
}
```

```sh
gleam test
```

### How It Works

1. **Define** tests with `describe`/`it` → returns a test tree
2. **Convert** with `to_test_cases` → flattens to runnable cases
3. **Run** with `run_all` → executes and collects results
4. **Report** with your choice of reporter → formats output

No hidden globals. No test discovery magic. You control the flow.

---

## Custom Matchers

Matchers are just functions. Write your own:

```gleam
import dream_test/types.{
  type MatchResult, AssertionFailure, CustomMatcherFailure,
  MatchFailed, MatchOk,
}
import dream_test/assertions/should.{type MatchResult, MatchOk, MatchFailed}
import gleam/option.{Some}
import gleam/int

pub fn be_even(result: MatchResult(Int)) -> MatchResult(Int) {
  case result {
    MatchFailed(failure) -> MatchFailed(failure)
    MatchOk(value) -> {
      case value % 2 == 0 {
        True -> MatchOk(value)
        False -> MatchFailed(AssertionFailure(
          operator: "be_even",
          message: "",
          payload: Some(CustomMatcherFailure(
            actual: int.to_string(value),
            description: "expected even number",
          )),
        ))
      }
    }
  }
}

// Usage:
42 |> should() |> be_even() |> or_fail_with("Should be even")
```

---

## Documentation

| Document                               | Audience                              |
| -------------------------------------- | ------------------------------------- |
| **[INTERFACE.md](INTERFACE.md)**       | Test authors — complete API reference |
| **[DESIGN.md](DESIGN.md)**             | Contributors — design philosophy      |
| **[ARCHITECTURE.md](ARCHITECTURE.md)** | Contributors — internal structure     |
| **[CONTRIBUTING.md](CONTRIBUTING.md)** | Contributors — how to help            |

---

## Status

**Pre-release** — The API is stabilizing but may change before v1.0.

|                            |            |
| -------------------------- | ---------- |
| Core DSL (`describe`/`it`) | ✅ Stable  |
| Assertions (`should.*`)    | ✅ Stable  |
| BDD Reporter               | ✅ Stable  |
| Process isolation          | 🚧 Planned |
| Async tests                | 🚧 Planned |

---

## Contributing

```sh
git clone https://github.com/TrustBound/dream_test
cd dream_test
make all  # build, test, format
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

---

## License

MIT

---

<div align="center">
  <sub>Part of the <a href="https://github.com/TrustBound/dream">Dream</a> ecosystem for Gleam</sub>
</div>
