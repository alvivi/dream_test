//// README: Lifecycle hooks

import dream_test/assertions/should.{be_empty, or_fail_with, should, succeed}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_suite}
import dream_test/unit.{
  after_all, after_each, before_all, before_each, describe, it, to_test_suite,
}
import gleam/io

pub fn tests() {
  describe("Database tests", [
    before_all(fn() {
      // Start database once for all tests
      succeed()
    }),
    before_each(fn() {
      // Begin transaction before each test
      succeed()
    }),
    it("creates a record", fn() {
      []
      |> should()
      |> be_empty()
      |> or_fail_with("Placeholder test")
    }),
    it("queries records", fn() {
      []
      |> should()
      |> be_empty()
      |> or_fail_with("Placeholder test")
    }),
    after_each(fn() {
      // Rollback transaction after each test
      succeed()
    }),
    after_all(fn() {
      // Stop database after all tests
      succeed()
    }),
  ])
}

pub fn main() {
  to_test_suite("lifecycle_hooks", tests())
  |> run_suite()
  |> report(io.print)
}
