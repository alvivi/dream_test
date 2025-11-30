import dream_test/matchers/boolean
import dream_test/matchers/collection
import dream_test/matchers/comparison
import dream_test/matchers/equality
import dream_test/matchers/option
import dream_test/matchers/result
import dream_test/matchers/string
import dream_test/types.{
  type AssertionResult, type MatchResult, AssertionFailed, AssertionFailure,
  AssertionOk, MatchFailed, MatchOk,
}
import gleam/option as gleam_option

/// Unified public API for assertions.
///
/// This module re-exports all matchers from the internal matchers/ directory
/// with the `should.*` naming convention.
///
/// Usage:
///   value |> should |> should.equal(expected)
///   Some(1) |> should |> should.be_some() |> should.equal(1)
/// Start an assertion chain.
///
/// This wraps a value in a `MatchResult` so it can be piped into matchers.
pub fn should(value: a) -> MatchResult(a) {
  MatchOk(value)
}

// Re-export equality matchers
pub const equal = equality.equal

pub const not_equal = equality.not_equal

// Re-export boolean matchers
pub const be_true = boolean.be_true

pub const be_false = boolean.be_false

// Re-export option matchers
pub const be_some = option.be_some

pub const be_none = option.be_none

// Re-export result matchers
pub const be_ok = result.be_ok

pub const be_error = result.be_error

// Re-export collection matchers
pub const contain = collection.contain

pub const not_contain = collection.not_contain

pub const have_length = collection.have_length

pub const be_empty = collection.be_empty

// Re-export comparison matchers
pub const be_greater_than = comparison.be_greater_than

pub const be_less_than = comparison.be_less_than

pub const be_at_least = comparison.be_at_least

pub const be_at_most = comparison.be_at_most

pub const be_between = comparison.be_between

pub const be_in_range = comparison.be_in_range

pub const be_greater_than_float = comparison.be_greater_than_float

pub const be_less_than_float = comparison.be_less_than_float

// Re-export string matchers
pub const start_with = string.start_with

pub const end_with = string.end_with

pub const contain_string = string.contain_string

/// Override the message on a failed assertion.
/// If the result is already Ok, it is returned unchanged.
///
/// This is a terminal operation that returns an AssertionResult.
///
/// Intended usage with pipes:
///   value |> should |> should.equal(expected) |> should.or_fail_with("message")
pub fn or_fail_with(result: MatchResult(a), message: String) -> AssertionResult {
  case result {
    MatchOk(_) -> AssertionOk

    MatchFailed(failure) ->
      AssertionFailed(AssertionFailure(..failure, message: message))
  }
}

/// Explicitly fail a test with a message.
///
/// Use this when you need to fail a test in a conditional branch:
///   case result {
///     Expected -> AssertionOk
///     Unexpected -> fail_with("Should not have gotten Unexpected")
///   }
pub fn fail_with(message: String) -> AssertionResult {
  AssertionFailed(AssertionFailure(
    operator: "fail_with",
    message: message,
    payload: gleam_option.None,
  ))
}
