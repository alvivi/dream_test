import dream_test/assertions/should
import dream_test/types.{AssertionFailed, AssertionOk}
import dream_test/unit.{describe, it}

pub fn tests() {
  describe("String Matchers", [
    describe("start_with", [
      it("returns AssertionOk when string starts with prefix", fn() {
        let result = "Hello, world!" |> should.start_with("Hello")

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with("start_with should pass when string starts with prefix")
        }
      }),
      it("returns AssertionFailed when string does not start with prefix", fn() {
        let result = "Hello, world!" |> should.start_with("world")

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk ->
            should.fail_with(
              "start_with should fail when string does not start with prefix",
            )
        }
      }),
      it("works with empty prefix", fn() {
        let result = "Hello, world!" |> should.start_with("")

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with("start_with should pass for empty prefix")
        }
      }),
    ]),
    describe("end_with", [
      it("returns AssertionOk when string ends with suffix", fn() {
        let result = "Hello, world!" |> should.end_with("world!")

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with("end_with should pass when string ends with suffix")
        }
      }),
      it("returns AssertionFailed when string does not end with suffix", fn() {
        let result = "Hello, world!" |> should.end_with("Hello")

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk ->
            should.fail_with(
              "end_with should fail when string does not end with suffix",
            )
        }
      }),
    ]),
    describe("contain_string", [
      it("returns AssertionOk when string contains substring", fn() {
        let result = "Hello, world!" |> should.contain_string(", ")

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with(
              "contain_string should pass when string contains substring",
            )
        }
      }),
      it("returns AssertionFailed when string does not contain substring", fn() {
        let result = "Hello, world!" |> should.contain_string("xyz")

        case result {
          AssertionFailed(_) -> AssertionOk
          AssertionOk ->
            should.fail_with(
              "contain_string should fail when string does not contain substring",
            )
        }
      }),
      it("finds substring at start of string", fn() {
        let result = "Hello, world!" |> should.contain_string("Hello")

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with("contain_string should find substring at start")
        }
      }),
      it("finds substring at end of string", fn() {
        let result = "Hello, world!" |> should.contain_string("world!")

        case result {
          AssertionOk -> AssertionOk
          AssertionFailed(_) ->
            should.fail_with("contain_string should find substring at end")
        }
      }),
    ]),
  ])
}
