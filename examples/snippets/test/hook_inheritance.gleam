//// README: Hook inheritance

import dream_test/assertions/should.{succeed}
import dream_test/reporter/bdd.{report}
import dream_test/runner.{run_suite}
import dream_test/unit.{after_each, before_each, describe, it, to_test_suite}
import gleam/io

pub fn tests() {
  describe("Outer", [
    before_each(fn() {
      io.println("1. outer setup")
      succeed()
    }),
    after_each(fn() {
      io.println("4. outer teardown")
      succeed()
    }),
    describe("Inner", [
      before_each(fn() {
        io.println("2. inner setup")
        succeed()
      }),
      after_each(fn() {
        io.println("3. inner teardown")
        succeed()
      }),
      it("test", fn() {
        io.println("(test)")
        succeed()
      }),
    ]),
  ])
}

pub fn main() {
  to_test_suite("hook_inheritance", tests())
  |> run_suite()
  |> report(io.print)
}
