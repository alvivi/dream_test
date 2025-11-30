import dream_test/types.{
  type AssertionResult, AssertionFailed, AssertionFailure, AssertionOk,
  BooleanFailure, Location,
}
import gleam/option.{Some}

/// Assert that `actual` is True, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_true()
pub fn be_true(actual: Bool) -> AssertionResult {
  case actual {
    True -> AssertionOk

    False -> {
      let payload = BooleanFailure(actual: False, expected: True)

      AssertionFailed(AssertionFailure(
        operator: "be_true",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}

/// Assert that `actual` is False, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_false()
pub fn be_false(actual: Bool) -> AssertionResult {
  case actual {
    False -> AssertionOk

    True -> {
      let payload = BooleanFailure(actual: True, expected: False)

      AssertionFailed(AssertionFailure(
        operator: "be_false",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}
