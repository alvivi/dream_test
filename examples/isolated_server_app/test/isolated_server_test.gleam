/// Tests demonstrating BEAM process isolation with Dream servers.
///
/// **THE KEY INSIGHT**: Each test starts a server on the SAME port (9999).
/// This only works because of BEAM process isolation:
///
/// 1. Test A runs, starts server on port 9999
/// 2. Test A ends → test process dies → server is automatically killed → port freed
/// 3. Test B runs, starts server on port 9999 (works because port is now free!)
///
/// Without process isolation, the second test would fail with "port already in use".
/// This demonstrates that linked processes (like the server) are automatically
/// cleaned up when the test process terminates.

import dream_http_client/client
import dream_test/assertions/should.{equal, or_fail_with, should}
import dream_test/process as test_process
import dream_test/reporter/bdd
import dream_test/runner
import dream_test/unit.{describe, it, to_test_cases}
import gleam/erlang/process
import gleam/io
import isolated_server_app.{start_server}

/// Fixed port for ALL tests - this proves isolation!
const test_port = 9999

/// Helper to make HTTP GET request and return body as string.
fn http_get(path: String) -> Result(String, Nil) {
  let result =
    client.new
    |> client.host("localhost")
    |> client.port(test_port)
    |> client.path(path)
    |> client.send()

  case result {
    Ok(body) -> Ok(body)
    Error(_) -> Error(Nil)
  }
}

pub fn tests() {
  describe("BEAM Process Isolation Demo", [
    describe("Same Port, Different Servers (run sequentially)", [
      // These tests ALL use port 9999!
      // They only work because each test's server is killed when the test ends.
      it("test 1: first server on port 9999 - starts fresh at 0", fn() {
        let assert Ok(_counter) = start_server(test_port)
        process.sleep(50)

        let assert Ok(count) = http_get("/count")
        count
        |> should()
        |> equal("0")
        |> or_fail_with("Server should start at 0")
      }),
      it("test 2: second server on port 9999 - also starts fresh!", fn() {
        // This test uses the SAME PORT as test 1!
        // It works because test 1's server was killed when test 1 ended.
        let assert Ok(_counter) = start_server(test_port)
        process.sleep(50)

        // Increment a few times
        let _ = http_get("/increment")
        let _ = http_get("/increment")
        let _ = http_get("/increment")

        let assert Ok(count) = http_get("/count")
        count
        |> should()
        |> equal("3")
        |> or_fail_with("Count should be 3 after incrementing")
      }),
      it("test 3: third server on port 9999 - fresh again!", fn() {
        // Even though test 2 incremented to 3, this server starts at 0
        // because test 2's server was killed and this is a NEW server.
        let assert Ok(_counter) = start_server(test_port)
        process.sleep(50)

        let assert Ok(count) = http_get("/count")
        // This proves the previous server was cleaned up!
        count
        |> should()
        |> equal("0")
        |> or_fail_with("New server should start at 0 - proves cleanup worked!")
      }),
      it("test 4: can increment on fresh server", fn() {
        let assert Ok(_counter) = start_server(test_port)
        process.sleep(50)

        let _ = http_get("/increment")
        let _ = http_get("/increment")

        let assert Ok(count) = http_get("/count")
        count
        |> should()
        |> equal("2")
        |> or_fail_with("Should be 2 after 2 increments on fresh server")
      }),
      it("test 5: final proof - fresh server on same port", fn() {
        let assert Ok(_counter) = start_server(test_port)
        process.sleep(50)

        let assert Ok(count) = http_get("/count")
        count
        |> should()
        |> equal("0")
        |> or_fail_with("Fifth server on port 9999 starts at 0 - isolation works!")
      }),
    ]),
    describe("Direct Counter Access", [
      it("counter actor is also cleaned up with test process", fn() {
        let assert Ok(counter) = start_server(test_port)
        process.sleep(50)

        // Increment via the actor directly
        test_process.increment(counter)
        test_process.increment(counter)

        let count = test_process.get_count(counter)
        count
        |> should()
        |> equal(2)
        |> or_fail_with("Direct counter access works")
        // When this test ends, both the server AND counter actor are killed
      }),
    ]),
  ])
}

pub fn main() {
  let all_tests = tests()
  let test_cases = to_test_cases("isolated_server_test", all_tests)

  // Run SEQUENTIALLY to demonstrate same-port isolation
  // (parallel tests on the same port would conflict at the OS level)
  let config = runner.sequential_config()
  let results = runner.run_all_with_config(config, test_cases)

  bdd.report(results, io.print)
}
