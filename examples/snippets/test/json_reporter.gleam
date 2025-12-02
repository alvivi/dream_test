//// README: JSON reporter example

import dream_test/assertions/should.{succeed}
import dream_test/reporter/bdd.{report}
import dream_test/reporter/json
import dream_test/runner.{exit_on_failure, run_all}
import dream_test/unit.{describe, it, to_test_cases}
import gleam/io

pub fn tests() {
  describe("JSON Reporter", [
    it("outputs JSON format", fn() {
      // The json.report function outputs machine-readable JSON
      // while bdd.report outputs human-readable text
      succeed()
    }),
    it("includes test metadata", fn() {
      // JSON output includes name, full_name, status, duration, tags
      succeed()
    }),
  ])
}

pub fn main() {
  to_test_cases("json_reporter", tests())
  |> run_all()
  |> report(io.print)
  |> json.report_pretty(io.println)
  |> exit_on_failure()
}
