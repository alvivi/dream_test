/// Tests for the sandbox module that provides process isolation.
///
/// These tests verify that:
/// - Tests run in isolated processes
/// - Timeouts work correctly
/// - Crashes are handled gracefully
import dream_test/sandbox.{
  SandboxCompleted, SandboxConfig, SandboxCrashed, SandboxTimedOut,
}
import dream_test/types.{AssertionFailed, AssertionFailure, AssertionOk}
import dream_test/unit.{describe, it}
import gleam/erlang/process
import gleam/option.{None}

pub fn tests() {
  describe("Sandbox", [
    describe("run_isolated", [
      it("returns SandboxCompleted for a passing test", fn() {
        // Arrange
        let config = SandboxConfig(timeout_ms: 1000)
        let test_function = fn() { AssertionOk }

        // Act
        let result = sandbox.run_isolated(config, test_function)

        // Assert
        case result {
          SandboxCompleted(AssertionOk) -> AssertionOk
          _ -> make_failure("Expected SandboxCompleted(AssertionOk)")
        }
      }),

      it("returns SandboxCompleted with failure for a failing test", fn() {
        // Arrange
        let config = SandboxConfig(timeout_ms: 1000)
        let failure =
          AssertionFailure(
            operator: "equal",
            message: "test failure",
            payload: None,
          )
        let test_function = fn() { AssertionFailed(failure) }

        // Act
        let result = sandbox.run_isolated(config, test_function)

        // Assert
        case result {
          SandboxCompleted(AssertionFailed(_)) -> AssertionOk
          _ -> make_failure("Expected SandboxCompleted(AssertionFailed)")
        }
      }),

      it("returns SandboxTimedOut for a test that exceeds timeout", fn() {
        // Arrange
        let config = SandboxConfig(timeout_ms: 50)
        let test_function = fn() {
          // Sleep longer than timeout
          process.sleep(200)
          AssertionOk
        }

        // Act
        let result = sandbox.run_isolated(config, test_function)

        // Assert
        case result {
          SandboxTimedOut -> AssertionOk
          _ -> make_failure("Expected SandboxTimedOut")
        }
      }),

      it("returns SandboxCrashed for a test that panics", fn() {
        // Arrange
        let config = SandboxConfig(timeout_ms: 1000)
        let test_function = fn() { panic as "intentional crash" }

        // Act
        let result = sandbox.run_isolated(config, test_function)

        // Assert
        case result {
          SandboxCrashed(_) -> AssertionOk
          _ -> make_failure("Expected SandboxCrashed")
        }
      }),

      it("isolates crashes from the parent process", fn() {
        // Arrange - if we get here, we weren't crashed by the child
        let config = SandboxConfig(timeout_ms: 1000)
        let test_function = fn() {
          panic as "this should not crash the test runner"
        }

        // Act
        let _result = sandbox.run_isolated(config, test_function)

        // Assert - if we reach this point, isolation worked
        AssertionOk
      }),
    ]),
  ])
}

/// Helper to create a failure with a message.
fn make_failure(message: String) -> types.AssertionResult {
  AssertionFailed(AssertionFailure(
    operator: "sandbox_test",
    message: message,
    payload: None,
  ))
}
