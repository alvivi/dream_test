//// README: Execution modes (flat vs suite)

import dream_test/assertions/should.{equal, or_fail_with, should}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_all, run_suite}
import dream_test/unit.{describe, it, to_test_cases, to_test_suite}
import gleam/io

pub fn tests() {
  describe("Execution modes demo", [
    it("works in both modes", fn() {
      1 + 1
      |> should()
      |> equal(2)
      |> or_fail_with("Math works")
    }),
  ])
}

// Flat mode - simpler, faster
pub fn run_flat_mode() {
  to_test_cases("my_test", tests())
  |> run_all()
  |> report(io.print)
}

// Suite mode - preserves group structure for before_all/after_all
pub fn run_suite_mode() {
  to_test_suite("my_test", tests())
  |> run_suite()
  |> report(io.print)
}

pub fn main() {
  run_flat_mode()
}
