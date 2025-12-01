//// README: Explicit failures

import dream_test/assertions/should.{fail_with}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_all}
import dream_test/types.{AssertionOk}
import dream_test/unit.{describe, it, to_test_cases}
import gleam/io
import snippets.{divide}

pub fn tests() {
  describe("Explicit failures", [
    it("fails explicitly when needed", fn() {
      let result = divide(10, 2)
      case result {
        Ok(_) -> AssertionOk
        Error(_) -> fail_with("Should have succeeded")
      }
    }),
  ])
}

pub fn main() {
  to_test_cases("explicit_failures", tests())
  |> run_all()
  |> report(io.print)
}
