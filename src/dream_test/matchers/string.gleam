import dream_test/types.{
  type AssertionResult, AssertionFailed, AssertionFailure, AssertionOk, Location,
  StringMatchFailure,
}
import gleam/option.{Some}
import gleam/string

/// Assert that `actual` starts with `prefix`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   text |> should.start_with("Hello")
pub fn start_with(actual: String, prefix: String) -> AssertionResult {
  case string.starts_with(actual, prefix) {
    True -> AssertionOk

    False -> {
      let payload =
        StringMatchFailure(
          actual: actual,
          pattern: prefix,
          operation: "start_with",
        )

      AssertionFailed(AssertionFailure(
        operator: "start_with",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}

/// Assert that `actual` ends with `suffix`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   text |> should.end_with("world")
pub fn end_with(actual: String, suffix: String) -> AssertionResult {
  case string.ends_with(actual, suffix) {
    True -> AssertionOk

    False -> {
      let payload =
        StringMatchFailure(
          actual: actual,
          pattern: suffix,
          operation: "end_with",
        )

      AssertionFailed(AssertionFailure(
        operator: "end_with",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}

/// Assert that `actual` contains `substring`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   text |> should.contain_string("middle")
pub fn contain_string(actual: String, substring: String) -> AssertionResult {
  case string.contains(actual, substring) {
    True -> AssertionOk

    False -> {
      let payload =
        StringMatchFailure(
          actual: actual,
          pattern: substring,
          operation: "contain_string",
        )

      AssertionFailed(AssertionFailure(
        operator: "contain_string",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}
