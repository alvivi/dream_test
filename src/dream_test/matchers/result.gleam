import gleam/string
import gleam/option.{Some}
import dream_test/types.{
  type AssertionResult, AssertionFailed, AssertionFailure, AssertionOk,
  Location, ResultFailure,
}

/// Assert that `actual` is Ok, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_ok()
pub fn be_ok(actual: Result(a, e)) -> AssertionResult {
  case actual {
    Ok(_) -> AssertionOk

    Error(error) -> {
      let payload = ResultFailure(
        actual: "Error(" <> string.inspect(error) <> ")",
        expected_ok: True,
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_ok",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is Error, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_error()
pub fn be_error(actual: Result(a, e)) -> AssertionResult {
  case actual {
    Error(_) -> AssertionOk

    Ok(value) -> {
      let payload = ResultFailure(
        actual: "Ok(" <> string.inspect(value) <> ")",
        expected_ok: False,
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_error",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is Ok and the inner value satisfies the given matcher.
///
/// Intended usage with pipes:
///   value |> should.be_ok_and(should.equal(42))
pub fn be_ok_and(
  actual: Result(a, e),
  matcher: fn(a) -> AssertionResult,
) -> AssertionResult {
  case actual {
    Ok(value) -> matcher(value)

    Error(error) -> {
      let payload = ResultFailure(
        actual: "Error(" <> string.inspect(error) <> ")",
        expected_ok: True,
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_ok_and",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

