import dream_test/core/types as types
import dream_test/assertions/context as context

/// Pipe-first assertion helpers.
///
/// These functions operate on a TestContext and a value under test,
/// building up a list of AssertionFailure values without throwing.

/// Assert that `actual` equals `expected`, returning an updated context.
///
/// Intended usage with pipes:
///   value |> should.equal(context, expected)
pub fn equal(actual: a, context: context.TestContext(a), expected: a) -> context.TestContext(a) {
  case actual == expected {
    True ->
      context

    False -> {
      let failure = types.AssertionFailure(
        actual: actual,
        expected: expected,
        operator: "equal",
        message: "",
        // For now, callers must supply a Location-aware wrapper if they
        // want accurate locations. We'll improve this later.
        location: types.Location("unknown", "unknown", 0),
      )

      context.add_failure(context, failure)
    }
  }
}

/// Override the message on the most recent failure in the context.
/// If there are no failures, the context is returned unchanged.
///
/// Intended usage with pipes:
///   context |> should.or_fail_with("message")
pub fn or_fail_with(test_context: context.TestContext(a), message: String) -> context.TestContext(a) {
  case context.failures(test_context) {
    [] ->
      test_context

    [first, ..rest] -> {
      let updated = types.AssertionFailure(..first, message: message)
      context.TestContext(failures: [updated, ..rest])
    }
  }
}
