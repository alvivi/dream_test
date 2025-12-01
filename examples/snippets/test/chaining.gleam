//// README: Chaining matchers

import dream_test/assertions/should.{be_ok, be_some, equal, or_fail_with, should}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_all}
import dream_test/unit.{describe, it, to_test_cases}
import gleam/io
import gleam/option.{Some}

pub fn tests() {
  describe("Chaining matchers", [
    // Unwrap Some, then check the value
    it("unwraps Option", fn() {
      Some(42)
      |> should()
      |> be_some()
      |> equal(42)
      |> or_fail_with("Should contain 42")
    }),
    // Unwrap Ok, then check the value
    it("unwraps Result", fn() {
      Ok("success")
      |> should()
      |> be_ok()
      |> equal("success")
      |> or_fail_with("Should be Ok with 'success'")
    }),
  ])
}

pub fn main() {
  to_test_cases("chaining", tests())
  |> run_all()
  |> report(io.print)
}
