import dream_test/assertions/should
import dream_test/types.{AssertionFailed, AssertionOk}
import dream_test/unit.{describe, it}

pub fn tests() {
  describe("Boolean Matchers", [
    describe("be_true", [
      it("returns AssertionOk when value is True", fn() {
        let result = True |> should.be_true()

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) -> should.fail_with("be_true should pass for True")
        }
      }),
      it("returns AssertionFailed when value is False", fn() {
        let result = False |> should.be_true()

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk -> should.fail_with("be_true should fail for False")
        }
      }),
    ]),
    describe("be_false", [
      it("returns AssertionOk when value is False", fn() {
        let result = False |> should.be_false()

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with("be_false should pass for False")
        }
      }),
      it("returns AssertionFailed when value is True", fn() {
        let result = True |> should.be_false()

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk -> should.fail_with("be_false should fail for True")
        }
      }),
    ]),
  ])
}
