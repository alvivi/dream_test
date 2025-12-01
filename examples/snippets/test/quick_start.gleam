//// README: Quick Start example

import dream_test/assertions/should.{equal, or_fail_with, should}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_all}
import dream_test/unit.{describe, it, to_test_cases}
import gleam/io
import gleam/string

pub fn tests() {
  describe("String utilities", [
    it("trims whitespace", fn() {
      "  hello  "
      |> string.trim()
      |> should()
      |> equal("hello")
      |> or_fail_with("Should remove surrounding whitespace")
    }),
    it("finds substrings", fn() {
      "hello world"
      |> string.contains("world")
      |> should()
      |> equal(True)
      |> or_fail_with("Should find 'world' in string")
    }),
  ])
}

pub fn main() {
  to_test_cases("quick_start", tests())
  |> run_all()
  |> report(io.print)
}
