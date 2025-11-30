/// Parallel test execution with configurable concurrency.
///
/// This module provides concurrent test execution while maintaining
/// deterministic result ordering. Tests run in isolated processes
/// using the sandbox module.
import dream_test/types.{
  type AssertionResult, type SingleTestConfig, type TestCase, type TestResult,
  AssertionFailed, AssertionOk, Failed, TestCase, TestResult, TimedOut,
  status_from_failures,
}
import gleam/erlang/process.{
  type Pid, type Selector, type Subject, kill, monitor, new_selector,
  new_subject, select, selector_receive, send, spawn_unlinked,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string

/// Configuration for parallel test execution.
pub type ParallelConfig {
  ParallelConfig(max_concurrency: Int, default_timeout_ms: Int)
}

/// Default parallel configuration: 4 concurrent tests, 5 second timeout.
pub fn default_config() -> ParallelConfig {
  ParallelConfig(max_concurrency: 4, default_timeout_ms: 5000)
}

/// Internal state for tracking parallel execution.
type ExecutionState {
  ExecutionState(
    pending: List(IndexedTest),
    running: List(RunningTest),
    completed: List(IndexedResult),
    results_subject: Subject(WorkerResult),
    config: ParallelConfig,
  )
}

/// A test paired with its original index for ordering.
type IndexedTest {
  IndexedTest(index: Int, test_case: TestCase)
}

/// A running test with its worker info.
type RunningTest {
  RunningTest(
    index: Int,
    test_case: TestCase,
    worker_pid: Pid,
    worker_monitor: process.Monitor,
  )
}

/// A completed result paired with its original index.
type IndexedResult {
  IndexedResult(index: Int, result: TestResult)
}

/// Message from a worker process.
type WorkerResult {
  WorkerCompleted(index: Int, assertion_result: AssertionResult)
  WorkerDown(index: Int, reason: String)
}

/// Run tests in parallel with the given configuration.
///
/// Tests are executed concurrently up to max_concurrency.
/// Results are returned in the same order as the input tests.
pub fn run_parallel(
  config: ParallelConfig,
  test_cases: List(TestCase),
) -> List(TestResult) {
  let indexed_tests = index_tests(test_cases, 0, [])
  let results_subject = new_subject()

  let initial_state =
    ExecutionState(
      pending: indexed_tests,
      running: [],
      completed: [],
      results_subject: results_subject,
      config: config,
    )

  let final_state = execute_loop(initial_state)
  sort_results(final_state.completed)
}

/// Add indices to tests for ordering.
fn index_tests(
  tests: List(TestCase),
  index: Int,
  accumulated: List(IndexedTest),
) -> List(IndexedTest) {
  case tests {
    [] -> list.reverse(accumulated)
    [head, ..tail] -> {
      let indexed = IndexedTest(index: index, test_case: head)
      index_tests(tail, index + 1, [indexed, ..accumulated])
    }
  }
}

/// Main execution loop.
fn execute_loop(state: ExecutionState) -> ExecutionState {
  let state_with_workers = start_workers_up_to_limit(state)

  case is_execution_complete(state_with_workers) {
    True -> state_with_workers
    False -> {
      let updated_state = wait_for_result(state_with_workers)
      execute_loop(updated_state)
    }
  }
}

/// Check if all tests are complete.
fn is_execution_complete(state: ExecutionState) -> Bool {
  list.is_empty(state.pending) && list.is_empty(state.running)
}

/// Start workers up to the concurrency limit.
fn start_workers_up_to_limit(state: ExecutionState) -> ExecutionState {
  let current_running = list.length(state.running)
  let slots_available = state.config.max_concurrency - current_running

  case slots_available > 0 && !list.is_empty(state.pending) {
    False -> state
    True -> {
      let state_with_worker = start_next_worker(state)
      start_workers_up_to_limit(state_with_worker)
    }
  }
}

/// Start the next pending test as a worker.
fn start_next_worker(state: ExecutionState) -> ExecutionState {
  case state.pending {
    [] -> state
    [indexed_test, ..remaining_pending] -> {
      let running_test =
        spawn_test_worker(state.results_subject, indexed_test, state.config)

      ExecutionState(..state, pending: remaining_pending, running: [
        running_test,
        ..state.running
      ])
    }
  }
}

/// Spawn a worker process for a test.
fn spawn_test_worker(
  results_subject: Subject(WorkerResult),
  indexed_test: IndexedTest,
  _config: ParallelConfig,
) -> RunningTest {
  let test_index = indexed_test.index
  let test_case = indexed_test.test_case

  let worker_pid =
    spawn_unlinked(fn() {
      let assertion_result = run_test_directly(test_case)
      send(results_subject, WorkerCompleted(test_index, assertion_result))
    })

  let worker_monitor = monitor(worker_pid)

  RunningTest(
    index: test_index,
    test_case: test_case,
    worker_pid: worker_pid,
    worker_monitor: worker_monitor,
  )
}

/// Run a test case directly in the current process.
fn run_test_directly(test_case: TestCase) -> AssertionResult {
  case test_case {
    TestCase(single_config) -> single_config.run()
  }
}

/// Wait for a result from any running worker.
fn wait_for_result(state: ExecutionState) -> ExecutionState {
  let selector = build_results_selector(state)

  // Wait indefinitely - workers have their own timeouts
  case selector_receive(selector, 60_000) {
    Ok(worker_result) -> handle_worker_result(state, worker_result)
    Error(Nil) -> handle_selector_timeout(state)
  }
}

/// Build a selector for worker results and monitor events.
fn build_results_selector(state: ExecutionState) -> Selector(WorkerResult) {
  new_selector()
  |> select(state.results_subject)
  |> add_monitor_handlers(state.running)
}

/// Add monitor handlers for all running workers.
fn add_monitor_handlers(
  selector: Selector(WorkerResult),
  running: List(RunningTest),
) -> Selector(WorkerResult) {
  case running {
    [] -> selector
    [running_test, ..rest] -> {
      let updated_selector =
        process.select_specific_monitor(
          selector,
          running_test.worker_monitor,
          fn(down) { WorkerDown(running_test.index, format_down_reason(down)) },
        )
      add_monitor_handlers(updated_selector, rest)
    }
  }
}

/// Format a down message reason as a string.
fn format_down_reason(down: process.Down) -> String {
  case down.reason {
    process.Normal -> "normal"
    process.Killed -> "killed"
    process.Abnormal(reason) -> string.inspect(reason)
  }
}

/// Handle a result from a worker.
fn handle_worker_result(
  state: ExecutionState,
  worker_result: WorkerResult,
) -> ExecutionState {
  let index = get_worker_result_index(worker_result)
  let running_test = find_running_test(state.running, index)

  case running_test {
    None -> state
    // Spurious message, ignore
    Some(running) -> {
      let test_result = convert_worker_result(running, worker_result)
      let indexed_result = IndexedResult(index: index, result: test_result)

      ExecutionState(
        ..state,
        running: remove_running_test(state.running, index),
        completed: [indexed_result, ..state.completed],
      )
    }
  }
}

/// Extract the index from a worker result.
fn get_worker_result_index(result: WorkerResult) -> Int {
  case result {
    WorkerCompleted(index, _) -> index
    WorkerDown(index, _) -> index
  }
}

/// Find a running test by index.
fn find_running_test(
  running: List(RunningTest),
  index: Int,
) -> Option(RunningTest) {
  case running {
    [] -> None
    [running_test, ..rest] -> find_running_test_check(running_test, rest, index)
  }
}

fn find_running_test_check(
  running_test: RunningTest,
  rest: List(RunningTest),
  index: Int,
) -> Option(RunningTest) {
  case running_test.index == index {
    True -> Some(running_test)
    False -> find_running_test(rest, index)
  }
}

/// Remove a running test by index.
fn remove_running_test(
  running: List(RunningTest),
  index: Int,
) -> List(RunningTest) {
  list.filter(running, fn(running_test) { running_test.index != index })
}

/// Convert a worker result to a TestResult.
fn convert_worker_result(
  running_test: RunningTest,
  worker_result: WorkerResult,
) -> TestResult {
  case running_test.test_case {
    TestCase(config) -> convert_worker_result_with_config(config, worker_result)
  }
}

fn convert_worker_result_with_config(
  config: SingleTestConfig,
  worker_result: WorkerResult,
) -> TestResult {
  case worker_result {
    WorkerCompleted(_, assertion_result) ->
      assertion_result_to_test_result(config, assertion_result)
    WorkerDown(_, reason) -> make_crashed_result(config, reason)
  }
}

/// Convert an AssertionResult to a TestResult.
fn assertion_result_to_test_result(
  config: SingleTestConfig,
  assertion_result: AssertionResult,
) -> TestResult {
  let failures = case assertion_result {
    AssertionOk -> []
    AssertionFailed(failure) -> [failure]
  }

  let status = status_from_failures(failures)

  TestResult(
    name: config.name,
    full_name: config.full_name,
    status: status,
    duration_ms: 0,
    tags: config.tags,
    failures: failures,
    kind: config.kind,
  )
}

/// Create a TestResult for a timed-out test.
fn make_timeout_result(config: SingleTestConfig) -> TestResult {
  TestResult(
    name: config.name,
    full_name: config.full_name,
    status: TimedOut,
    duration_ms: 0,
    tags: config.tags,
    failures: [],
    kind: config.kind,
  )
}

/// Create a TestResult for a crashed test.
fn make_crashed_result(config: SingleTestConfig, reason: String) -> TestResult {
  let failure =
    types.AssertionFailure(
      operator: "crash",
      message: "Test process crashed: " <> reason,
      payload: None,
    )

  TestResult(
    name: config.name,
    full_name: config.full_name,
    status: Failed,
    duration_ms: 0,
    tags: config.tags,
    failures: [failure],
    kind: config.kind,
  )
}

/// Handle selector timeout (should not happen in normal operation).
fn handle_selector_timeout(state: ExecutionState) -> ExecutionState {
  // Kill all running workers and mark them as timed out
  kill_all_running(state.running)

  let timeout_results =
    list.map(state.running, fn(running_test) {
      case running_test.test_case {
        TestCase(config) -> {
          IndexedResult(
            index: running_test.index,
            result: make_timeout_result(config),
          )
        }
      }
    })

  ExecutionState(
    ..state,
    running: [],
    completed: list.append(timeout_results, state.completed),
  )
}

/// Kill all running worker processes.
fn kill_all_running(running: List(RunningTest)) -> Nil {
  case running {
    [] -> Nil
    [running_test, ..rest] -> {
      kill(running_test.worker_pid)
      kill_all_running(rest)
    }
  }
}

/// Sort completed results by their original index.
fn sort_results(completed: List(IndexedResult)) -> List(TestResult) {
  completed
  |> list.sort(fn(a, b) { compare_indices(a.index, b.index) })
  |> list.map(fn(indexed) { indexed.result })
}

/// Compare two indices for sorting.
fn compare_indices(a: Int, b: Int) -> order.Order {
  case a < b {
    True -> order.Lt
    False -> compare_indices_not_less(a, b)
  }
}

fn compare_indices_not_less(a: Int, b: Int) -> order.Order {
  case a > b {
    True -> order.Gt
    False -> order.Eq
  }
}
