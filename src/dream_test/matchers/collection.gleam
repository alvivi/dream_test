import dream_test/types.{
  type MatchResult, AssertionFailure, CollectionFailure, MatchFailed, MatchOk,
}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string

/// Assert that `actual_list` contains `expected_item`, returning a MatchResult.
///
/// Intended usage with pipes:
///   list |> should |> should.contain(item)
pub fn contain(
  value_or_result: MatchResult(List(a)),
  expected_item: a,
) -> MatchResult(List(a)) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual_list) -> {
      case list.contains(actual_list, expected_item) {
        True -> MatchOk(actual_list)

        False -> {
          let payload =
            CollectionFailure(
              actual: string.inspect(actual_list),
              expected: string.inspect(expected_item),
              operation: "contain",
            )

          MatchFailed(AssertionFailure(
            operator: "contain",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual_list` does not contain `unexpected_item`, returning a MatchResult.
///
/// Intended usage with pipes:
///   list |> should |> should.not_contain(item)
pub fn not_contain(
  value_or_result: MatchResult(List(a)),
  unexpected_item: a,
) -> MatchResult(List(a)) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual_list) -> {
      case list.contains(actual_list, unexpected_item) {
        False -> MatchOk(actual_list)

        True -> {
          let payload =
            CollectionFailure(
              actual: string.inspect(actual_list),
              expected: "not " <> string.inspect(unexpected_item),
              operation: "not_contain",
            )

          MatchFailed(AssertionFailure(
            operator: "not_contain",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual_list` has the expected length, returning a MatchResult.
///
/// Intended usage with pipes:
///   list |> should |> should.have_length(3)
pub fn have_length(
  value_or_result: MatchResult(List(a)),
  expected_length: Int,
) -> MatchResult(List(a)) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual_list) -> {
      let actual_length = list.length(actual_list)

      case actual_length == expected_length {
        True -> MatchOk(actual_list)

        False -> {
          let payload =
            CollectionFailure(
              actual: "list with length " <> int.to_string(actual_length),
              expected: "list with length " <> int.to_string(expected_length),
              operation: "have_length",
            )

          MatchFailed(AssertionFailure(
            operator: "have_length",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}

/// Assert that `actual_list` is empty, returning a MatchResult.
///
/// Intended usage with pipes:
///   list |> should |> should.be_empty()
pub fn be_empty(value_or_result: MatchResult(List(a))) -> MatchResult(List(a)) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)

    MatchOk(actual_list) -> {
      case actual_list {
        [] -> MatchOk(actual_list)

        _ -> {
          let payload =
            CollectionFailure(
              actual: string.inspect(actual_list),
              expected: "[]",
              operation: "be_empty",
            )

          MatchFailed(AssertionFailure(
            operator: "be_empty",
            message: "",
            payload: Some(payload),
          ))
        }
      }
    }
  }
}
