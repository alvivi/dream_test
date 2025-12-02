//// README: Explicit failures

import dream_test/assertions/should.{fail_with, succeed}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_all}
import dream_test/unit.{describe, it, to_test_cases}
import gleam/io
import snippets.{divide}

pub fn tests() {
  describe("Explicit failures", [
    it("succeeds explicitly when division works", fn() {
      let result = divide(10, 2)
      case result {
        Ok(_) -> succeed()
        Error(_) -> fail_with("Should have succeeded")
      }
    }),
    it("fails explicitly when expecting an error", fn() {
      let result = divide(10, 0)
      case result {
        Ok(_) -> fail_with("Should have returned an error")
        Error(_) -> succeed()
      }
    }),
  ])
}

pub fn main() {
  to_test_cases("explicit_failures", tests())
  |> run_all()
  |> report(io.print)
}
