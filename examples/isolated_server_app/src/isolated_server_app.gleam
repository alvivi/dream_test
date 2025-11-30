/// Simple server demonstrating BEAM test isolation using Dream.
///
/// This module provides an HTTP counter server that can be started on any port.
/// In tests, each test process spawns its own server instance, demonstrating
/// that the BEAM provides complete process isolation.

import dream/http.{ok, text_response}
import dream/http/request.{Get}
import dream/router.{route, router}
import dream/servers/mist/server
import dream_test/process as test_process
import gleam/erlang/process.{type Subject}
import gleam/int

/// Services available to Dream controllers
pub type Services {
  Services(counter: Subject(test_process.CounterMessage))
}

/// Controller: GET /count - returns current count
fn count_controller(_request, _context, services: Services) {
  let count = test_process.get_count(services.counter)
  text_response(ok, int.to_string(count))
}

/// Controller: GET /increment - increments counter and returns confirmation
fn increment_controller(_request, _context, services: Services) {
  test_process.increment(services.counter)
  text_response(ok, "incremented")
}

/// Controller: GET / - index route
fn index_controller(_request, _context, _services) {
  text_response(ok, "Hello from isolated server!")
}

/// Start a Dream HTTP server on the given port with a counter.
/// Returns the counter subject for interacting with the server's state.
///
/// The counter is created using dream_test/process.start_counter(),
/// which uses gleam_otp actors. When the test process terminates,
/// all linked processes (including the counter and server) are cleaned up.
pub fn start_server(
  port: Int,
) -> Result(Subject(test_process.CounterMessage), Nil) {
  // Start the counter actor using dream_test/process helper
  let counter = test_process.start_counter()
  let services = Services(counter: counter)

  let app_router =
    router()
    |> route(
      method: Get,
      path: "/",
      controller: index_controller,
      middleware: [],
    )
    |> route(
      method: Get,
      path: "/count",
      controller: count_controller,
      middleware: [],
    )
    |> route(
      method: Get,
      path: "/increment",
      controller: increment_controller,
      middleware: [],
    )

  server.new()
  |> server.services(services)
  |> server.router(app_router)
  |> server.bind("localhost")
  |> server.listen(port)

  Ok(counter)
}

pub fn main() {
  let assert Ok(_counter) = start_server(3000)
  process.sleep_forever()
}
