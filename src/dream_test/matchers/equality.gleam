import dream_test/types.{
  type MatchResult, AssertionFailure, EqualityFailure, MatchFailed, MatchOk,
}
import gleam/option.{Some}
import gleam/string

/// Assert that `actual` equals `expected`, returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.equal(expected)
pub fn equal(value_or_result: MatchResult(a), expected: a) -> MatchResult(a) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual == expected {
        True -> MatchOk(actual)

        False -> {
          let payload =
            EqualityFailure(
              actual: inspect_value(actual),
              expected: inspect_value(expected),
            )

          MatchFailed(AssertionFailure(
            operator: "equal",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` does not equal `unexpected`, returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.not_equal(unexpected)
pub fn not_equal(
  value_or_result: MatchResult(a),
  unexpected: a,
) -> MatchResult(a) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual != unexpected {
        True -> MatchOk(actual)

        False -> {
          let payload =
            EqualityFailure(
              actual: inspect_value(actual),
              expected: "not " <> inspect_value(unexpected),
            )

          MatchFailed(AssertionFailure(
            operator: "not_equal",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

fn inspect_value(value: a) -> String {
  // For now we rely on Gleam's built-in debug representation via string.inspect.
  // This can be refined later for prettier diffs.
  string.inspect(value)
}
