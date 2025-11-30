import gleam/string
import gleam/option.{type Option, None, Some}
import dream_test/types.{
  type AssertionResult, AssertionFailed, AssertionFailure, AssertionOk,
  Location, OptionFailure,
}

/// Assert that `actual` is Some, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_some()
pub fn be_some(actual: Option(a)) -> AssertionResult {
  case actual {
    Some(_) -> AssertionOk

    None -> {
      let payload = OptionFailure(actual: "None", expected_some: True)

      AssertionFailed(
        AssertionFailure(
          operator: "be_some",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is None, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_none()
pub fn be_none(actual: Option(a)) -> AssertionResult {
  case actual {
    None -> AssertionOk

    Some(value) -> {
      let payload = OptionFailure(
        actual: "Some(" <> string.inspect(value) <> ")",
        expected_some: False,
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_none",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is Some and the inner value satisfies the given matcher.
///
/// Intended usage with pipes:
///   value |> should.be_some_and(should.equal(42))
pub fn be_some_and(
  actual: Option(a),
  matcher: fn(a) -> AssertionResult,
) -> AssertionResult {
  case actual {
    Some(value) -> matcher(value)

    None -> {
      let payload = OptionFailure(actual: "None", expected_some: True)

      AssertionFailed(
        AssertionFailure(
          operator: "be_some_and",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

