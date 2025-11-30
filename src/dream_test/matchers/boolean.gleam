//// Boolean matchers for dream_test.
////
//// These matchers check boolean values.
//// They're re-exported through `dream_test/assertions/should`.
////
//// ## Usage
////
//// ```gleam
//// import dream_test/assertions/should.{should, be_true, be_false, or_fail_with}
////
//// is_valid(input)
//// |> should()
//// |> be_true()
//// |> or_fail_with("Input should be valid")
////
//// is_empty(list)
//// |> should()
//// |> be_false()
//// |> or_fail_with("List should not be empty")
//// ```

import dream_test/types.{
  type MatchResult, AssertionFailure, BooleanFailure, MatchFailed, MatchOk,
}
import gleam/option.{Some}

/// Assert that a value is `True`.
///
/// ## Example
///
/// ```gleam
/// is_valid(input)
/// |> should()
/// |> be_true()
/// |> or_fail_with("Input should be valid")
/// ```
///
pub fn be_true(value_or_result: MatchResult(Bool)) -> MatchResult(Bool) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)
    MatchOk(actual) -> check_is_true(actual)
  }
}

fn check_is_true(actual: Bool) -> MatchResult(Bool) {
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

/// Assert that a value is `False`.
///
/// ## Example
///
/// ```gleam
/// is_empty(list)
/// |> should()
/// |> be_false()
/// |> or_fail_with("List should not be empty")
/// ```
///
pub fn be_false(value_or_result: MatchResult(Bool)) -> MatchResult(Bool) {
  case value_or_result {
    MatchFailed(failure) -> MatchFailed(failure)
    MatchOk(actual) -> check_is_false(actual)
  }
}

fn check_is_false(actual: Bool) -> MatchResult(Bool) {
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
