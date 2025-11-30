import dream_test/types.{
  type MatchResult, AssertionFailure, MatchFailed, MatchOk, ResultFailure,
}
import gleam/option.{Some}
import gleam/string

/// Assert that `actual` is Ok, returning the inner value in a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_ok()
pub fn be_ok(value_or_result: MatchResult(Result(a, e))) -> MatchResult(a) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual {
        Ok(value) -> MatchOk(value)

        Error(error) -> {
          let payload =
            ResultFailure(
              actual: "Error(" <> string.inspect(error) <> ")",
              expected_ok: True,
            )

          MatchFailed(AssertionFailure(
            operator: "be_ok",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` is Error, returning the error value in a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_error()
pub fn be_error(value_or_result: MatchResult(Result(a, e))) -> MatchResult(e) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual {
        Error(error) -> MatchOk(error)

        Ok(value) -> {
          let payload =
            ResultFailure(
              actual: "Ok(" <> string.inspect(value) <> ")",
              expected_ok: False,
            )

          MatchFailed(AssertionFailure(
            operator: "be_error",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

// Note: be_ok_and is removed as chaining replaces it naturally:
// Ok(42) |> should |> be_ok() |> equal(42)
