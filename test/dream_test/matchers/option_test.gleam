import dream_test/assertions/should
import dream_test/types.{AssertionFailed, AssertionOk}
import dream_test/unit.{describe, it}
import gleam/option.{None, Some}

pub fn tests() {
  describe("Option Matchers", [
    describe("be_some", [
      it("returns AssertionOk when value is Some", fn() {
        let result = Some(42) |> should.be_some()

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) -> should.fail_with("be_some should pass for Some")
        }
      }),
      it("returns AssertionFailed when value is None", fn() {
        let result = None |> should.be_some()

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk -> should.fail_with("be_some should fail for None")
        }
      }),
    ]),
    describe("be_none", [
      it("returns AssertionOk when value is None", fn() {
        let result = None |> should.be_none()

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) -> should.fail_with("be_none should pass for None")
        }
      }),
      it("returns AssertionFailed when value is Some", fn() {
        let result = Some(42) |> should.be_none()

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk -> should.fail_with("be_none should fail for Some")
        }
      }),
    ]),
    describe("be_some_and", [
      it("returns AssertionOk when Some and inner value matches", fn() {
        let result = Some(42) |> should.be_some_and(fn(v) { should.equal(v, 42) })

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with("be_some_and should pass for Some with matching value")
        }
      }),
      it("returns AssertionFailed when Some but inner value differs", fn() {
        let result = Some(42) |> should.be_some_and(fn(v) { should.equal(v, 100) })

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk ->
            should.fail_with(
              "be_some_and should fail for Some with non-matching value",
            )
        }
      }),
      it("returns AssertionFailed when value is None", fn() {
        let result = None |> should.be_some_and(fn(v) { should.equal(v, 42) })

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk -> should.fail_with("be_some_and should fail for None")
        }
      }),
    ]),
  ])
}
