/// Tests for process helpers demonstrating BEAM isolation.
///
/// These tests show that:
/// - Each test gets its own isolated counter
/// - Counters don't share state across tests
/// - Spawned processes are automatically cleaned up
import dream_test/assertions/should.{equal, or_fail_with, should}
import dream_test/process as test_process
import dream_test/types.{AssertionOk}
import dream_test/unit.{describe, it}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/otp/actor

/// Custom message type for start_actor test
pub type AccumulatorMessage {
  Add(Int)
  GetTotal(Subject(Int))
}

pub fn tests() {
  describe("Process Helpers", [
    describe("start_counter", [
      it("starts with count 0", fn() {
        let counter = test_process.start_counter()
        let count = test_process.get_count(counter)

        count
        |> should()
        |> equal(0)
        |> or_fail_with("Counter should start at 0")
      }),
      it("increments correctly", fn() {
        let counter = test_process.start_counter()

        test_process.increment(counter)
        test_process.increment(counter)
        test_process.increment(counter)

        let count = test_process.get_count(counter)

        count
        |> should()
        |> equal(3)
        |> or_fail_with("Counter should be 3 after 3 increments")
      }),
      it("decrements correctly", fn() {
        let counter = test_process.start_counter_with(10)

        test_process.decrement(counter)
        test_process.decrement(counter)

        let count = test_process.get_count(counter)

        count
        |> should()
        |> equal(8)
        |> or_fail_with("Counter should be 8 after 2 decrements from 10")
      }),
      it("sets value correctly", fn() {
        let counter = test_process.start_counter()

        test_process.set_count(counter, 42)

        let count = test_process.get_count(counter)

        count
        |> should()
        |> equal(42)
        |> or_fail_with("Counter should be 42 after set")
      }),
    ]),
    describe("Counter Isolation", [
      it("test A: counter is independent (increment to 5)", fn() {
        // Each test gets its own counter
        let counter = test_process.start_counter()

        test_process.increment(counter)
        test_process.increment(counter)
        test_process.increment(counter)
        test_process.increment(counter)
        test_process.increment(counter)

        let count = test_process.get_count(counter)

        count
        |> should()
        |> equal(5)
        |> or_fail_with("Counter should be 5")
      }),
      it("test B: counter is independent (increment to 2)", fn() {
        // This runs in parallel with test A, but has its own counter
        let counter = test_process.start_counter()

        test_process.increment(counter)
        test_process.increment(counter)

        let count = test_process.get_count(counter)

        // Even though test A incremented 5 times, this counter is isolated
        count
        |> should()
        |> equal(2)
        |> or_fail_with("Counter should be 2 - isolated from test A")
      }),
      it("test C: fresh counter after previous tests", fn() {
        // This test runs after A and B, but gets a fresh counter
        let counter = test_process.start_counter()

        let count = test_process.get_count(counter)

        count
        |> should()
        |> equal(0)
        |> or_fail_with("Fresh counter should start at 0")
      }),
    ]),
    describe("start_actor", [
      it("spawns a stateful actor with custom handler", fn() {
        // Note: handler receives (state, message) and returns Next(state, msg)
        let acc =
          test_process.start_actor(0, fn(total: Int, msg: AccumulatorMessage) {
            case msg {
              Add(n) -> actor.continue(total + n)
              GetTotal(reply_to) -> {
                process.send(reply_to, total)
                actor.continue(total)
              }
            }
          })

        // Add some values
        process.send(acc, Add(10))
        process.send(acc, Add(5))
        process.send(acc, Add(3))

        // Get the total using call_actor
        let total = test_process.call_actor(acc, GetTotal, 1000)

        total
        |> should()
        |> equal(18)
        |> or_fail_with("Accumulator should sum to 18")
      }),
    ]),
    describe("unique_port", [
      it("generates ports in valid range", fn() {
        let port = test_process.unique_port()

        case port >= 10_000 && port < 60_000 {
          True -> AssertionOk
          False ->
            int.to_string(port)
            |> should()
            |> equal("port in range 10000-60000")
            |> or_fail_with("Port should be in valid range")
        }
      }),
      it("generates different ports on subsequent calls", fn() {
        // Generate several ports and check they're not all the same
        let port1 = test_process.unique_port()
        let port2 = test_process.unique_port()
        let port3 = test_process.unique_port()

        // At least one should be different (extremely unlikely to get 3 same)
        let all_same = port1 == port2 && port2 == port3

        case all_same {
          False -> AssertionOk
          True ->
            { "all same: " <> int.to_string(port1) }
            |> should()
            |> equal("different ports")
            |> or_fail_with("Ports should vary")
        }
      }),
    ]),
    describe("await_ready", [
      it("returns Ready immediately when condition is true", fn() {
        let config = test_process.PollConfig(timeout_ms: 1000, interval_ms: 10)

        let result = test_process.await_ready(config, fn() { True })

        case result {
          test_process.Ready(True) -> AssertionOk
          _ ->
            "unexpected result"
            |> should()
            |> equal("Ready(True)")
            |> or_fail_with("Should return Ready immediately")
        }
      }),
      it("returns Ready when condition becomes true", fn() {
        // Use a counter to track calls - becomes true on 3rd check
        let counter = test_process.start_counter()
        let config = test_process.PollConfig(timeout_ms: 1000, interval_ms: 10)

        let result =
          test_process.await_ready(config, fn() {
            test_process.increment(counter)
            test_process.get_count(counter) >= 3
          })

        case result {
          test_process.Ready(True) -> AssertionOk
          _ ->
            "unexpected result"
            |> should()
            |> equal("Ready(True)")
            |> or_fail_with("Should return Ready after condition becomes true")
        }
      }),
      it("returns TimedOut when condition never becomes true", fn() {
        // Very short timeout to make test fast
        let config = test_process.PollConfig(timeout_ms: 50, interval_ms: 10)

        let result = test_process.await_ready(config, fn() { False })

        case result {
          test_process.TimedOut -> AssertionOk
          _ ->
            "unexpected result"
            |> should()
            |> equal("TimedOut")
            |> or_fail_with("Should return TimedOut")
        }
      }),
    ]),
    describe("await_some", [
      it("returns Ready with value when Ok is returned", fn() {
        let config = test_process.PollConfig(timeout_ms: 1000, interval_ms: 10)

        let result = test_process.await_some(config, fn() { Ok(42) })

        case result {
          test_process.Ready(42) -> AssertionOk
          test_process.Ready(other) ->
            int.to_string(other)
            |> should()
            |> equal("42")
            |> or_fail_with("Should return Ready(42)")
          test_process.TimedOut ->
            "TimedOut"
            |> should()
            |> equal("Ready(42)")
            |> or_fail_with("Should not time out")
        }
      }),
      it("returns Ready when Ok is eventually returned", fn() {
        let counter = test_process.start_counter()
        let config = test_process.PollConfig(timeout_ms: 1000, interval_ms: 10)

        let result =
          test_process.await_some(config, fn() {
            test_process.increment(counter)
            let count = test_process.get_count(counter)
            case count >= 3 {
              True -> Ok(count)
              False -> Error(Nil)
            }
          })

        case result {
          test_process.Ready(value) -> assert_value_at_least_3(value)
          test_process.TimedOut ->
            "TimedOut"
            |> should()
            |> equal("Ready")
            |> or_fail_with("Should not time out")
        }
      }),
      it("returns TimedOut when Error is always returned", fn() {
        let config = test_process.PollConfig(timeout_ms: 50, interval_ms: 10)

        let result =
          test_process.await_some(config, fn() { Error("not ready") })

        case result {
          test_process.TimedOut -> AssertionOk
          _ ->
            "unexpected result"
            |> should()
            |> equal("TimedOut")
            |> or_fail_with("Should return TimedOut")
        }
      }),
    ]),
  ])
}

fn assert_value_at_least_3(value: Int) -> types.AssertionResult {
  case value >= 3 {
    True -> AssertionOk
    False ->
      int.to_string(value)
      |> should()
      |> equal("at least 3")
      |> or_fail_with("Should return value >= 3")
  }
}
