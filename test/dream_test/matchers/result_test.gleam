import dream_test/assertions/should.{be_error, be_ok, equal, fail_with, or_fail_with, should}
import dream_test/types.{AssertionOk, MatchFailed, MatchOk}
import dream_test/unit.{describe, it}

pub fn tests() {
  describe("Result Matchers", [
    describe("be_ok", [
      it("returns MatchOk with inner value when value is Ok", fn() {
        let result: Result(Int, String) = Ok(42)
        let check = result |> should() |> be_ok()

        case check {
          MatchOk(value) ->
            value
            |> should()
            |> equal(42)
            |> or_fail_with("should equal 42")
          MatchFailed(_) -> fail_with("be_ok should pass for Ok")
        }
      }),
      it("returns MatchFailed when value is Error", fn() {
        let result: Result(Int, String) = Error("failed")
        let check = result |> should() |> be_ok()

        case check {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) -> fail_with("be_ok should fail for Error")
        }
      }),
    ]),
    describe("be_error", [
      it("returns MatchOk with error value when value is Error", fn() {
        let result: Result(Int, String) = Error("failed")
        let check = result |> should() |> be_error()

        case check {
          MatchOk(error) ->
            error
            |> should()
            |> equal("failed")
            |> or_fail_with("should equal 'failed'")
          MatchFailed(_) -> fail_with("be_error should pass for Error")
        }
      }),
      it("returns MatchFailed when value is Ok", fn() {
        let result: Result(Int, String) = Ok(42)
        let check = result |> should() |> be_error()

        case check {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) -> fail_with("be_error should fail for Ok")
        }
      }),
    ]),
    describe("chaining", [
      it("chains be_ok with equal", fn() {
        let result: Result(Int, String) = Ok(42)
        let check =
          result
          |> should()
          |> be_ok()
          |> equal(42)

        case check {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) ->
            fail_with("chaining be_ok |> equal should pass")
        }
      }),
      it("fails chain if inner value differs", fn() {
        let result: Result(Int, String) = Ok(42)
        let check =
          result
          |> should()
          |> be_ok()
          |> equal(100)

        case check {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) ->
            fail_with(
              "chaining be_ok |> equal should fail if value differs",
            )
        }
      }),
      it("fails chain if value is Error", fn() {
        let result: Result(Int, String) = Error("failed")
        let check =
          result
          |> should()
          |> be_ok()
          |> equal(42)

        case check {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) ->
            fail_with("chaining be_ok |> equal should fail for Error")
        }
      }),
    ]),
  ])
}
