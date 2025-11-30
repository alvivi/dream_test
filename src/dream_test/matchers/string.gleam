import dream_test/types.{
  type MatchResult, AssertionFailure, MatchFailed, MatchOk, StringMatchFailure,
}
import gleam/option.{Some}
import gleam/string

/// Assert that `actual` starts with `prefix`, returning a MatchResult.
///
/// Intended usage with pipes:
///   text |> should |> should.start_with("Hello")
pub fn start_with(
  value_or_result: MatchResult(String),
  prefix: String,
) -> MatchResult(String) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case string.starts_with(actual, prefix) {
        True -> MatchOk(actual)

        False -> {
          let payload =
            StringMatchFailure(
              actual: actual,
              pattern: prefix,
              operation: "start_with",
            )

          MatchFailed(AssertionFailure(
            operator: "start_with",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` ends with `suffix`, returning a MatchResult.
///
/// Intended usage with pipes:
///   text |> should |> should.end_with("world")
pub fn end_with(
  value_or_result: MatchResult(String),
  suffix: String,
) -> MatchResult(String) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case string.ends_with(actual, suffix) {
        True -> MatchOk(actual)

        False -> {
          let payload =
            StringMatchFailure(
              actual: actual,
              pattern: suffix,
              operation: "end_with",
            )

          MatchFailed(AssertionFailure(
            operator: "end_with",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` contains `substring`, returning a MatchResult.
///
/// Intended usage with pipes:
///   text |> should |> should.contain_string("middle")
pub fn contain_string(
  value_or_result: MatchResult(String),
  substring: String,
) -> MatchResult(String) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case string.contains(actual, substring) {
        True -> MatchOk(actual)

        False -> {
          let payload =
            StringMatchFailure(
              actual: actual,
              pattern: substring,
              operation: "contain_string",
            )

          MatchFailed(AssertionFailure(
            operator: "contain_string",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}
