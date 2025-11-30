import dream_test/types.{
  type AssertionResult, AssertionFailed, AssertionFailure, AssertionOk,
  CollectionFailure, Location,
}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string

/// Assert that `actual_list` contains `expected_item`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   list |> should.contain(item)
pub fn contain(actual_list: List(a), expected_item: a) -> AssertionResult {
  case list.contains(actual_list, expected_item) {
    True -> AssertionOk

    False -> {
      let payload =
        CollectionFailure(
          actual: string.inspect(actual_list),
          expected: string.inspect(expected_item),
          operation: "contain",
        )

      AssertionFailed(AssertionFailure(
        operator: "contain",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}

/// Assert that `actual_list` does not contain `unexpected_item`, returning an AssertionResult.
///
/// Intended usage with pipes:
///   list |> should.not_contain(item)
pub fn not_contain(actual_list: List(a), unexpected_item: a) -> AssertionResult {
  case list.contains(actual_list, unexpected_item) {
    False -> AssertionOk

    True -> {
      let payload =
        CollectionFailure(
          actual: string.inspect(actual_list),
          expected: "not " <> string.inspect(unexpected_item),
          operation: "not_contain",
        )

      AssertionFailed(AssertionFailure(
        operator: "not_contain",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}

/// Assert that `actual_list` has the expected length, returning an AssertionResult.
///
/// Intended usage with pipes:
///   list |> should.have_length(3)
pub fn have_length(
  actual_list: List(a),
  expected_length: Int,
) -> AssertionResult {
  let actual_length = list.length(actual_list)

  case actual_length == expected_length {
    True -> AssertionOk

    False -> {
      let payload =
        CollectionFailure(
          actual: "list with length " <> int.to_string(actual_length),
          expected: "list with length " <> int.to_string(expected_length),
          operation: "have_length",
        )

      AssertionFailed(AssertionFailure(
        operator: "have_length",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}

/// Assert that `actual_list` is empty, returning an AssertionResult.
///
/// Intended usage with pipes:
///   list |> should.be_empty()
pub fn be_empty(actual_list: List(a)) -> AssertionResult {
  case actual_list {
    [] -> AssertionOk

    _ -> {
      let payload =
        CollectionFailure(
          actual: string.inspect(actual_list),
          expected: "[]",
          operation: "be_empty",
        )

      AssertionFailed(AssertionFailure(
        operator: "be_empty",
        message: "",
        location: Location("unknown", "unknown", 0),
        payload: Some(payload),
      ))
    }
  }
}
