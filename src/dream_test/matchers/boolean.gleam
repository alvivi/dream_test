import dream_test/types.{
  type MatchResult, AssertionFailure, BooleanFailure, MatchFailed, MatchOk,
}
import gleam/option.{Some}

/// Assert that `actual` is True, returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_true()
pub fn be_true(value_or_result: MatchResult(Bool)) -> MatchResult(Bool) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual {
        True -> MatchOk(True)

        False -> {
          let payload = BooleanFailure(actual: False, expected: True)

          MatchFailed(AssertionFailure(
            operator: "be_true",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` is False, returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_false()
pub fn be_false(value_or_result: MatchResult(Bool)) -> MatchResult(Bool) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual {
        False -> MatchOk(False)

        True -> {
          let payload = BooleanFailure(actual: True, expected: False)

          MatchFailed(AssertionFailure(
            operator: "be_false",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}
