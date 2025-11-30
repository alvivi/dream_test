import dream_test/assertions/should.{equal, fail_with, or_fail_with, should}
import dream_test/runner.{SingleTestConfig, run_single_test}
import dream_test/types.{
  AssertionFailed, AssertionFailure, AssertionOk, EqualityFailure, Failed,
  Passed, Unit,
}
import dream_test/unit.{describe, it}
import gleam/option.{Some}

pub fn tests() {
  describe("Runner", [
    describe("run_single_test", [
      it("produces Passed status for passing test", fn() {
        // Arrange
        let config =
          SingleTestConfig(
            name: "passing test",
            full_name: ["bootstrap", "runner_core"],
            tags: ["bootstrap", "runner"],
            kind: Unit,
            run: fn() { AssertionOk },
          )
        let expected = Passed

        // Act
        let result = run_single_test(config)

        // Assert
        result.status
        |> should()
        |> equal(expected)
        |> or_fail_with("Passing test should have Passed status")
      }),

      it("produces empty failures list for passing test", fn() {
        // Arrange
        let config =
          SingleTestConfig(
            name: "passing test",
            full_name: ["bootstrap", "runner_core"],
            tags: ["bootstrap", "runner"],
            kind: Unit,
            run: fn() { AssertionOk },
          )
        let expected = []

        // Act
        let result = run_single_test(config)

        // Assert
        result.failures
        |> should()
        |> equal(expected)
        |> or_fail_with("Passing test should have no failures")
      }),

      it("produces Failed status for failing test", fn() {
        // Arrange
        let failure =
          AssertionFailure(
            operator: "equal",
            message: "",
            payload: Some(EqualityFailure(actual: "1", expected: "2")),
          )
        let config =
          SingleTestConfig(
            name: "failing test",
            full_name: ["bootstrap", "runner_core"],
            tags: ["bootstrap", "runner"],
            kind: Unit,
            run: fn() { AssertionFailed(failure) },
          )
        let expected = Failed

        // Act
        let result = run_single_test(config)

        // Assert
        result.status
        |> should()
        |> equal(expected)
        |> or_fail_with("Failing test should have Failed status")
      }),

      it("records at least one failure for failing test", fn() {
        // Arrange
        let failure =
          AssertionFailure(
            operator: "equal",
            message: "",
            payload: Some(EqualityFailure(actual: "1", expected: "2")),
          )
        let config =
          SingleTestConfig(
            name: "failing test",
            full_name: ["bootstrap", "runner_core"],
            tags: ["bootstrap", "runner"],
            kind: Unit,
            run: fn() { AssertionFailed(failure) },
          )

        // Act
        let result = run_single_test(config)

        // Assert
        case result.failures {
          [] -> fail_with("Failing test should have at least one failure")
          [_, ..] -> AssertionOk
        }
      }),
    ]),
  ])
}
