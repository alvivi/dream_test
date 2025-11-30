import dream_test/assertions/should.{equal, fail_with, not_equal, should}
import dream_test/types.{AssertionOk, MatchFailed, MatchOk}
import dream_test/unit.{describe, it}

pub fn tests() {
  describe("Equality Matchers", [
    describe("equal", [
      it("returns MatchOk when values match", fn() {
        let result = 5 |> should |> equal(5)

        case result {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) -> fail_with("equal should pass for matching values")
        }
      }),
      it("returns MatchFailed when values differ", fn() {
        let result = 5 |> should |> equal(10)

        case result {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) -> fail_with("equal should fail for non-matching values")
        }
      }),
      it("works with strings", fn() {
        let result = "hello" |> should |> equal("hello")

        case result {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) -> fail_with("equal should work with strings")
        }
      }),
      it("works with lists", fn() {
        let result = [1, 2, 3] |> should |> equal([1, 2, 3])

        case result {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) -> fail_with("equal should work with lists")
        }
      }),
    ]),
    describe("not_equal", [
      it("returns MatchOk when values differ", fn() {
        let result = 5 |> should |> not_equal(10)

        case result {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) ->
            fail_with("not_equal should pass for different values")
        }
      }),
      it("returns MatchFailed when values match", fn() {
        let result = 5 |> should |> not_equal(5)

        case result {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) -> fail_with("not_equal should fail for matching values")
        }
      }),
    ]),
  ])
}
