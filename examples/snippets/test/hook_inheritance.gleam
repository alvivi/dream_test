//// README: Hook inheritance

import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_suite}
import dream_test/types.{AssertionOk}
import dream_test/unit.{after_each, before_each, describe, it, to_test_suite}
import gleam/io

pub fn tests() {
  describe("Outer", [
    before_each(fn() {
      io.println("1. outer setup")
      AssertionOk
    }),
    after_each(fn() {
      io.println("4. outer teardown")
      AssertionOk
    }),
    describe("Inner", [
      before_each(fn() {
        io.println("2. inner setup")
        AssertionOk
      }),
      after_each(fn() {
        io.println("3. inner teardown")
        AssertionOk
      }),
      it("test", fn() {
        io.println("(test)")
        AssertionOk
      }),
    ]),
  ])
}

pub fn main() {
  to_test_suite("hook_inheritance", tests())
  |> run_suite()
  |> report(io.print)
}
