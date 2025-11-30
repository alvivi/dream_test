import dream_test/assertions/should.{be_none, be_some, equal, fail_with, or_fail_with, should}
import dream_test/types.{AssertionOk, MatchFailed, MatchOk}
import dream_test/unit.{describe, it}
import gleam/option.{None, Some}

pub fn tests() {
  describe("Option Matchers", [
    describe("be_some", [
      it("returns MatchOk with inner value when value is Some", fn() {
        let result = Some(42) |> should() |> be_some()

        case result {
          MatchOk(value) ->
            value
            |> should()
            |> equal(42)
            |> or_fail_with("should contain 42")
          MatchFailed(_) -> fail_with("be_some should pass for Some")
        }
      }),
      it("returns MatchFailed when value is None", fn() {
        let result = None |> should() |> be_some()

        case result {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) -> fail_with("be_some should fail for None")
        }
      }),
    ]),
    describe("be_none", [
      it("returns MatchOk when value is None", fn() {
        let result = None |> should() |> be_none()

        case result {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) -> fail_with("be_none should pass for None")
        }
      }),
      it("returns MatchFailed when value is Some", fn() {
        let result = Some(42) |> should() |> be_none()

        case result {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) -> fail_with("be_none should fail for Some")
        }
      }),
    ]),
    describe("chaining", [
      it("chains be_some with equal", fn() {
        let result =
          Some(42)
          |> should()
          |> be_some()
          |> equal(42)

        case result {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) ->
            fail_with("chaining be_some |> equal should pass")
        }
      }),
      it("fails chain if inner value differs", fn() {
        let result =
          Some(42)
          |> should()
          |> be_some()
          |> equal(100)

        case result {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) ->
            fail_with(
              "chaining be_some |> equal should fail if value differs",
            )
        }
      }),
      it("fails chain if value is None", fn() {
        let result =
          None
          |> should()
          |> be_some()
          |> equal(42)

        case result {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) ->
            fail_with("chaining be_some |> equal should fail for None")
        }
      }),
    ]),
  ])
}
