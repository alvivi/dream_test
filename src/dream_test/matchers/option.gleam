import dream_test/types.{
  type MatchResult, AssertionFailure, MatchFailed, MatchOk, OptionFailure,
}
import gleam/option.{type Option, None, Some}
import gleam/string

/// Assert that `actual` is Some, returning the inner value in a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_some()
pub fn be_some(value_or_result: MatchResult(Option(a))) -> MatchResult(a) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual {
        Some(value) -> MatchOk(value)

        None -> {
          let payload = OptionFailure(actual: "None", expected_some: True)

          MatchFailed(AssertionFailure(
            operator: "be_some",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` is None, returning a MatchResult(Nil).
///
/// Intended usage with pipes:
///   value |> should |> should.be_none()
pub fn be_none(value_or_result: MatchResult(Option(a))) -> MatchResult(Nil) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual {
        None -> MatchOk(Nil)

        Some(value) -> {
          let payload =
            OptionFailure(
              actual: "Some(" <> string.inspect(value) <> ")",
              expected_some: False,
            )

          MatchFailed(AssertionFailure(
            operator: "be_none",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

// Note: be_some_and is removed as chaining replaces it naturally:
// Some(42) |> should |> be_some() |> equal(42)
