import gleam/int
import gleam/float
import gleam/option.{Some}
import dream_test/types.{
  type AssertionResult, AssertionFailed, AssertionFailure, AssertionOk,
  ComparisonFailure, Location,
}

/// Assert that `actual` is greater than `threshold`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_greater_than(5)
pub fn be_greater_than(actual: Int, threshold: Int) -> AssertionResult {
  case actual > threshold {
    True -> AssertionOk

    False -> {
      let payload = ComparisonFailure(
        actual: int.to_string(actual),
        expected: "> " <> int.to_string(threshold),
        operator: "be_greater_than",
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_greater_than",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is less than `threshold`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_less_than(10)
pub fn be_less_than(actual: Int, threshold: Int) -> AssertionResult {
  case actual < threshold {
    True -> AssertionOk

    False -> {
      let payload = ComparisonFailure(
        actual: int.to_string(actual),
        expected: "< " <> int.to_string(threshold),
        operator: "be_less_than",
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_less_than",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is at least `minimum` (>=), returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_at_least(5)
pub fn be_at_least(actual: Int, minimum: Int) -> AssertionResult {
  case actual >= minimum {
    True -> AssertionOk

    False -> {
      let payload = ComparisonFailure(
        actual: int.to_string(actual),
        expected: ">= " <> int.to_string(minimum),
        operator: "be_at_least",
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_at_least",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is at most `maximum` (<=), returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_at_most(10)
pub fn be_at_most(actual: Int, maximum: Int) -> AssertionResult {
  case actual <= maximum {
    True -> AssertionOk

    False -> {
      let payload = ComparisonFailure(
        actual: int.to_string(actual),
        expected: "<= " <> int.to_string(maximum),
        operator: "be_at_most",
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_at_most",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is between `min` and `max` (exclusive), returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_between(5, 10)  // 5 < value < 10
pub fn be_between(actual: Int, min: Int, max: Int) -> AssertionResult {
  case actual > min && actual < max {
    True -> AssertionOk

    False -> {
      let payload = ComparisonFailure(
        actual: int.to_string(actual),
        expected: int.to_string(min) <> " < value < " <> int.to_string(max),
        operator: "be_between",
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_between",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` is in the range `min` to `max` (inclusive), returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_in_range(5, 10)  // 5 <= value <= 10
pub fn be_in_range(actual: Int, min: Int, max: Int) -> AssertionResult {
  case actual >= min && actual <= max {
    True -> AssertionOk

    False -> {
      let payload = ComparisonFailure(
        actual: int.to_string(actual),
        expected: int.to_string(min) <> " <= value <= " <> int.to_string(max),
        operator: "be_in_range",
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_in_range",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` (float) is greater than `threshold`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_greater_than_float(5.5)
pub fn be_greater_than_float(actual: Float, threshold: Float) -> AssertionResult {
  case actual >. threshold {
    True -> AssertionOk

    False -> {
      let payload = ComparisonFailure(
        actual: float.to_string(actual),
        expected: "> " <> float.to_string(threshold),
        operator: "be_greater_than_float",
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_greater_than_float",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

/// Assert that `actual` (float) is less than `threshold`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should.be_less_than_float(10.5)
pub fn be_less_than_float(actual: Float, threshold: Float) -> AssertionResult {
  case actual <. threshold {
    True -> AssertionOk

    False -> {
      let payload = ComparisonFailure(
        actual: float.to_string(actual),
        expected: "< " <> float.to_string(threshold),
        operator: "be_less_than_float",
      )

      AssertionFailed(
        AssertionFailure(
          operator: "be_less_than_float",
          message: "",
          location: Location("unknown", "unknown", 0),
          payload: Some(payload),
        ),
      )
    }
  }
}

