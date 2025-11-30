import dream_test/types.{
  type MatchResult, AssertionFailure, ComparisonFailure, MatchFailed,
  MatchOk,
}
import gleam/float
import gleam/int
import gleam/option.{Some}

/// Assert that `actual` is greater than `threshold`, returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_greater_than(5)
pub fn be_greater_than(
  value_or_result: MatchResult(Int),
  threshold: Int,
) -> MatchResult(Int) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual > threshold {
        True -> MatchOk(actual)

        False -> {
          let payload =
            ComparisonFailure(
              actual: int.to_string(actual),
              expected: "> " <> int.to_string(threshold),
              operator: "be_greater_than",
            )

          MatchFailed(AssertionFailure(
            operator: "be_greater_than",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` is less than `threshold`, returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_less_than(10)
pub fn be_less_than(
  value_or_result: MatchResult(Int),
  threshold: Int,
) -> MatchResult(Int) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual < threshold {
        True -> MatchOk(actual)

        False -> {
          let payload =
            ComparisonFailure(
              actual: int.to_string(actual),
              expected: "< " <> int.to_string(threshold),
              operator: "be_less_than",
            )

          MatchFailed(AssertionFailure(
            operator: "be_less_than",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` is at least `minimum` (>=), returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_at_least(5)
pub fn be_at_least(
  value_or_result: MatchResult(Int),
  minimum: Int,
) -> MatchResult(Int) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual >= minimum {
        True -> MatchOk(actual)

        False -> {
          let payload =
            ComparisonFailure(
              actual: int.to_string(actual),
              expected: ">= " <> int.to_string(minimum),
              operator: "be_at_least",
            )

          MatchFailed(AssertionFailure(
            operator: "be_at_least",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` is at most `maximum` (<=), returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_at_most(10)
pub fn be_at_most(
  value_or_result: MatchResult(Int),
  maximum: Int,
) -> MatchResult(Int) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual <= maximum {
        True -> MatchOk(actual)

        False -> {
          let payload =
            ComparisonFailure(
              actual: int.to_string(actual),
              expected: "<= " <> int.to_string(maximum),
              operator: "be_at_most",
            )

          MatchFailed(AssertionFailure(
            operator: "be_at_most",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` is between `min` and `max` (exclusive), returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_between(5, 10)
pub fn be_between(
  value_or_result: MatchResult(Int),
  min: Int,
  max: Int,
) -> MatchResult(Int) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual > min && actual < max {
        True -> MatchOk(actual)

        False -> {
          let payload =
            ComparisonFailure(
              actual: int.to_string(actual),
              expected: int.to_string(min) <> " < value < " <> int.to_string(max),
              operator: "be_between",
            )

          MatchFailed(AssertionFailure(
            operator: "be_between",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` is in the range `min` to `max` (inclusive), returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_in_range(5, 10)
pub fn be_in_range(
  value_or_result: MatchResult(Int),
  min: Int,
  max: Int,
) -> MatchResult(Int) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual >= min && actual <= max {
        True -> MatchOk(actual)

        False -> {
          let payload =
            ComparisonFailure(
              actual: int.to_string(actual),
              expected: int.to_string(min) <> " <= value <= " <> int.to_string(max),
              operator: "be_in_range",
            )

          MatchFailed(AssertionFailure(
            operator: "be_in_range",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` (float) is greater than `threshold`, returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_greater_than_float(5.5)
pub fn be_greater_than_float(
  value_or_result: MatchResult(Float),
  threshold: Float,
) -> MatchResult(Float) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual >. threshold {
        True -> MatchOk(actual)

        False -> {
          let payload =
            ComparisonFailure(
              actual: float.to_string(actual),
              expected: "> " <> float.to_string(threshold),
              operator: "be_greater_than_float",
            )

          MatchFailed(AssertionFailure(
            operator: "be_greater_than_float",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual` (float) is less than `threshold`, returning a MatchResult.
///
/// Intended usage with pipes:
///   value |> should |> should.be_less_than_float(10.5)
pub fn be_less_than_float(
  value_or_result: MatchResult(Float),
  threshold: Float,
) -> MatchResult(Float) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual) -> {
      case actual <. threshold {
        True -> MatchOk(actual)

        False -> {
          let payload =
            ComparisonFailure(
              actual: float.to_string(actual),
              expected: "< " <> float.to_string(threshold),
              operator: "be_less_than_float",
            )

          MatchFailed(AssertionFailure(
            operator: "be_less_than_float",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}
