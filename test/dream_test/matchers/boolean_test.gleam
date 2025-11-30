import dream_test/assertions/should.{be_false, be_true, fail_with, should}
import dream_test/types.{AssertionOk, MatchFailed, MatchOk}
import dream_test/unit.{describe, it}

pub fn tests() {
  describe("Boolean Matchers", [
    describe("be_true", [
      it("returns MatchOk when value is True", fn() {
        let result = True |> should() |> be_true()

        case result {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) -> fail_with("be_true should pass for True")
        }
      }),
      it("returns MatchFailed when value is False", fn() {
        let result = False |> should() |> be_true()

        case result {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) -> fail_with("be_true should fail for False")
        }
      }),
    ]),
    describe("be_false", [
      it("returns MatchOk when value is False", fn() {
        let result = False |> should() |> be_false()

        case result {
          MatchOk(_) -> AssertionOk
          MatchFailed(_) -> fail_with("be_false should pass for False")
        }
      }),
      it("returns MatchFailed when value is True", fn() {
        let result = True |> should() |> be_false()

        case result {
          MatchFailed(_) -> AssertionOk
          MatchOk(_) -> fail_with("be_false should fail for True")
        }
      }),
    ]),
  ])
}
