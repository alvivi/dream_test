import dream_test/types.{
  type AssertionResult, AssertionFailed, AssertionFailure, AssertionOk,
  EqualityFailure, Location,
}
import gleam/option.{Some}
import gleam/string

/// Assert that `actual` equals `expected`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.equal(expected)
pub fn equal(actual: a, expected: a) -> AssertionResult {
  case actual == expected {
    True -> AssertionOk

    False -> {
      let payload =
        EqualityFailure(
          actual: inspect_value(actual),
          expected: inspect_value(expected),
        )

      AssertionFailed(AssertionFailure(
        operator: "equal",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}

/// Assert that `actual` does not equal `unexpected`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.not_equal(unexpected)
pub fn not_equal(actual: a, unexpected: a) -> AssertionResult {
  case actual != unexpected {
    True -> AssertionOk

    False -> {
      let payload =
        EqualityFailure(
          actual: inspect_value(actual),
          expected: "not " <> inspect_value(unexpected),
        )

      AssertionFailed(AssertionFailure(
        operator: "not_equal",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}

fn inspect_value(value: a) -> String {
  // For now we rely on Gleam's built-in debug representation via string.inspect.
  // This can be refined later for prettier diffs.
  string.inspect(value)
}
