//// README: Hook failure behavior

import dream_test/assertions/should.{fail_with}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_suite}
import dream_test/types.{AssertionOk}
import dream_test/unit.{before_all, describe, it, to_test_suite}
import gleam/io

fn connect_to_database() {
  Ok(Nil)
}

pub fn tests() {
  describe("Handles failures", [
    before_all(fn() {
      case connect_to_database() {
        Ok(_) -> AssertionOk
        Error(e) -> fail_with("Database connection failed: " <> e)
      }
    }),
    // If before_all fails, these tests are marked SetupFailed (not run)
    it("test1", fn() { AssertionOk }),
    it("test2", fn() { AssertionOk }),
  ])
}

pub fn main() {
  to_test_suite("hook_failure", tests())
  |> run_suite()
  |> report(io.print)
}
