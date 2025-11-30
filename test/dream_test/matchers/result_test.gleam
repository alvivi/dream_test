import dream_test/assertions/should
import dream_test/types.{AssertionFailed, AssertionOk}
import dream_test/unit.{describe, it}

pub fn tests() {
  describe("Result Matchers", [
    describe("be_ok", [
      it("returns AssertionOk when value is Ok", fn() {
        let result: Result(Int, String) = Ok(42)
        let check = result |> should.be_ok()

        case check {
          AssertionOk -> AssertionOk
          AssertionFailed(_) -> should.fail_with("be_ok should pass for Ok")
        }
      }),
      it("returns AssertionFailed when value is Error", fn() {
        let result: Result(Int, String) = Error("failed")
        let check = result |> should.be_ok()

        case check {
          AssertionFailed(_) -> AssertionOk
          AssertionOk -> should.fail_with("be_ok should fail for Error")
        }
      }),
    ]),
    describe("be_error", [
      it("returns AssertionOk when value is Error", fn() {
        let result: Result(Int, String) = Error("failed")
        let check = result |> should.be_error()

        case check {
          AssertionOk -> AssertionOk
          AssertionFailed(_) -> should.fail_with("be_error should pass for Error")
        }
      }),
      it("returns AssertionFailed when value is Ok", fn() {
        let result: Result(Int, String) = Ok(42)
        let check = result |> should.be_error()

        case check {
          AssertionFailed(_) -> AssertionOk
          AssertionOk -> should.fail_with("be_error should fail for Ok")
        }
      }),
    ]),
    describe("be_ok_and", [
      it("returns AssertionOk when Ok and inner value matches", fn() {
        let result: Result(Int, String) = Ok(42)
        let check = result |> should.be_ok_and(fn(v) { should.equal(v, 42) })

        case check {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with("be_ok_and should pass for Ok with matching value")
        }
      }),
      it("returns AssertionFailed when Ok but inner value differs", fn() {
        let result: Result(Int, String) = Ok(42)
        let check = result |> should.be_ok_and(fn(v) { should.equal(v, 100) })

        case check {
          AssertionFailed(_) -> AssertionOk
          AssertionOk ->
            should.fail_with("be_ok_and should fail for Ok with non-matching value")
        }
      }),
      it("returns AssertionFailed when value is Error", fn() {
        let result: Result(Int, String) = Error("failed")
        let check = result |> should.be_ok_and(fn(v) { should.equal(v, 42) })

        case check {
          AssertionFailed(_) -> AssertionOk
          AssertionOk -> should.fail_with("be_ok_and should fail for Error")
        }
      }),
    ]),
  ])
}
