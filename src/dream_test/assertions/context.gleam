import dream_test/core/types as types

/// Per-test context carrying assertion failures and any other
/// per-test metadata we may need later.
///
/// This is the core state threaded through assertions.
pub type TestContext(a) {
  TestContext(
    failures: List(types.AssertionFailure(a)),
  )
}

pub fn new() -> TestContext(a) {
  TestContext(failures: [])
}

pub fn failures(context: TestContext(a)) -> List(types.AssertionFailure(a)) {
  context.failures
}

pub fn add_failure(context: TestContext(a), failure: types.AssertionFailure(a)) -> TestContext(a) {
  TestContext(failures: [failure, ..context.failures])
}
