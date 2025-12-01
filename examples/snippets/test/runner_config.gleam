//// README: Runner config

import dream_test/assertions/should.{equal, or_fail_with, should}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{RunnerConfig, run_all_with_config}
import dream_test/unit.{describe, it, to_test_cases}
import gleam/io

pub fn tests() {
  describe("Runner config demo", [
    it("runs with custom config", fn() {
      1 + 1
      |> should()
      |> equal(2)
      |> or_fail_with("Math works")
    }),
  ])
}

pub fn main() {
  let config = RunnerConfig(max_concurrency: 8, default_timeout_ms: 10_000)

  let test_cases = to_test_cases("runner_config", tests())
  run_all_with_config(config, test_cases)
  |> report(io.print)
}
