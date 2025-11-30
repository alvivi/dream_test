//// BDD-style test reporter for dream_test.
////
//// This reporter formats test results in a hierarchical, spec-like format
//// that mirrors your `describe`/`it` structure. It's inspired by RSpec, Jest,
//// and Mocha.
////
//// ## Example Output
////
//// ```text
//// Calculator
////   add
////     ✓ adds positive numbers
////     ✓ handles zero
////   subtract
////     ✓ subtracts positive numbers
////     ✗ handles negative results
////       equal
////         Message: Should handle negative subtraction
////         Expected: -5
////         Actual:   5
////
//// Summary: 4 run, 1 failed, 3 passed
//// ```
////
//// ## Usage
////
//// ```gleam
//// import dream_test/unit.{describe, it, to_test_cases}
//// import dream_test/runner.{run_all}
//// import dream_test/reporter/bdd.{report}
//// import gleam/io
////
//// pub fn main() {
////   tests()
////   |> to_test_cases("my_test")
////   |> run_all()
////   |> report(io.print)
//// }
//// ```
////
//// ## Status Markers
////
//// | Status    | Marker | Meaning                        |
//// |-----------|--------|--------------------------------|
//// | Passed    | ✓      | All assertions succeeded       |
//// | Failed    | ✗      | One or more assertions failed  |
//// | Skipped   | -      | Test was skipped               |
//// | Pending   | ~      | Test is a placeholder          |
//// | TimedOut  | !      | Test exceeded timeout          |

import dream_test/types.{
  type AssertionFailure, type Status, type TestResult, EqualityFailure, Failed,
  Passed, Pending, Skipped, TimedOut,
}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string

/// Format test results as a BDD-style report string.
///
/// Returns the complete report including:
/// - Hierarchical test results with status markers
/// - Failure details with messages and diffs
/// - Summary line with counts
///
/// Use this when you need the report as a string (e.g., for testing the
/// reporter itself or writing to a file).
///
/// ## Example
///
/// ```gleam
/// let report_string = format(results)
/// file.write("test-results.txt", report_string)
/// ```
///
pub fn format(results: List(TestResult)) -> String {
  let formatted_results = format_all_results(results, [], "")
  let summary_text = format_summary(results)
  string.concat([formatted_results, "\n", summary_text])
}

/// Print test results using a provided writer function.
///
/// This is the main entry point for most test runs. The writer function
/// receives the formatted report string and can print it, log it, or
/// handle it however needed.
///
/// ## Example
///
/// ```gleam
/// // Print to stdout
/// results |> report(io.print)
///
/// // Print each line separately (for flushing)
/// results |> report(io.println)
///
/// // Custom handling
/// results |> report(fn(s) { logger.info(s) })
/// ```
///
/// ## Parameters
///
/// - `results` - List of test results from the runner
/// - `write` - Function that handles the formatted output string
///
pub fn report(results: List(TestResult), write: fn(String) -> Nil) {
  write(format(results))
}

fn format_all_results(
  results: List(TestResult),
  previous_path: List(String),
  accumulated: String,
) -> String {
  case results {
    [] -> accumulated
    [result, ..rest] -> {
      let formatted = format_one_result(result, previous_path)
      let updated = string.concat([accumulated, formatted])
      let new_path = extract_describe_segments(result.full_name)
      format_all_results(rest, new_path, updated)
    }
  }
}

fn format_one_result(result: TestResult, previous_path: List(String)) -> String {
  let current_path = extract_describe_segments(result.full_name)
  let common_depth = count_common_prefix(previous_path, current_path, 0)
  let new_segments = list.drop(current_path, common_depth)
  let headers = format_header_segments(new_segments, common_depth, "")
  let test_line = format_test_line(result)
  string.concat([headers, test_line])
}

fn count_common_prefix(
  previous: List(String),
  current: List(String),
  depth: Int,
) -> Int {
  case previous, current {
    [prev_head, ..prev_rest], [curr_head, ..curr_rest] ->
      count_common_prefix_check(
        prev_head,
        curr_head,
        prev_rest,
        curr_rest,
        depth,
      )
    _, _ -> depth
  }
}

fn count_common_prefix_check(
  prev_head: String,
  curr_head: String,
  prev_rest: List(String),
  curr_rest: List(String),
  depth: Int,
) -> Int {
  case prev_head == curr_head {
    True -> count_common_prefix(prev_rest, curr_rest, depth + 1)
    False -> depth
  }
}

fn extract_describe_segments(full_name: List(String)) -> List(String) {
  case list.reverse(full_name) {
    [] -> []
    [_] -> []
    [_, ..rest] -> list.reverse(rest)
  }
}

fn format_header_segments(
  segments: List(String),
  depth: Int,
  accumulated: String,
) -> String {
  case segments {
    [] -> accumulated
    [segment, ..rest] -> {
      let indent = build_indent(depth)
      let header = string.concat([indent, segment, "\n"])
      let updated = string.concat([accumulated, header])
      format_header_segments(rest, depth + 1, updated)
    }
  }
}

fn format_test_line(result: TestResult) -> String {
  let depth = calculate_test_depth(result.full_name)
  let indent = build_indent(depth)
  let marker = status_marker(result.status)
  let name = extract_test_name(result.full_name)
  let test_line = string.concat([indent, marker, " ", name, "\n"])
  let failure_text = format_failure_details(result, depth)
  string.concat([test_line, failure_text])
}

fn calculate_test_depth(full_name: List(String)) -> Int {
  case full_name {
    [] -> 0
    [_] -> 0
    _ -> list.length(full_name) - 1
  }
}

fn build_indent(level: Int) -> String {
  build_indent_recursive(level, "")
}

fn build_indent_recursive(level: Int, accumulated: String) -> String {
  case level {
    0 -> accumulated
    n -> build_indent_recursive(n - 1, string.concat([accumulated, "  "]))
  }
}

fn extract_test_name(full_name: List(String)) -> String {
  case list.reverse(full_name) {
    [last, ..] -> last
    [] -> ""
  }
}

fn status_marker(status: Status) -> String {
  case status {
    Passed -> "✓"
    Failed -> "✗"
    Skipped -> "-"
    Pending -> "~"
    TimedOut -> "!"
  }
}

fn format_failure_details(result: TestResult, indent_level: Int) -> String {
  case result.status {
    Failed -> format_all_failures(result.failures, indent_level, "")
    _ -> ""
  }
}

fn format_all_failures(
  failures: List(AssertionFailure),
  indent_level: Int,
  accumulated: String,
) -> String {
  case failures {
    [] -> accumulated
    [failure, ..rest] -> {
      let formatted = format_one_failure(failure, indent_level)
      let updated = string.concat([accumulated, formatted])
      format_all_failures(rest, indent_level, updated)
    }
  }
}

fn format_one_failure(failure: AssertionFailure, indent_level: Int) -> String {
  let base_indent = build_indent(indent_level)

  let header = string.concat([base_indent, "  ", failure.operator, "\n"])
  let message_text = format_failure_message(failure.message, base_indent)
  let payload_text = format_failure_payload(failure.payload, base_indent)

  string.concat([header, message_text, payload_text])
}

fn format_failure_message(message: String, base_indent: String) -> String {
  case message {
    "" -> ""
    _ -> string.concat([base_indent, "    Message: ", message, "\n"])
  }
}

fn format_failure_payload(
  payload: option.Option(types.FailurePayload),
  base_indent: String,
) -> String {
  case payload {
    Some(EqualityFailure(actual, expected)) ->
      string.concat([
        base_indent,
        "    Expected: ",
        expected,
        "\n",
        base_indent,
        "    Actual:   ",
        actual,
        "\n",
      ])
    _ -> ""
  }
}

fn format_summary(results: List(TestResult)) -> String {
  let total = list.length(results)
  let failed = count_by_status(results, Failed)
  let skipped = count_by_status(results, Skipped)
  let pending = count_by_status(results, Pending)
  let timed_out = count_by_status(results, TimedOut)
  let passed = total - failed - skipped - pending - timed_out

  string.concat([
    "Summary: ",
    int.to_string(total),
    " run, ",
    int.to_string(failed),
    " failed, ",
    int.to_string(passed),
    " passed",
    build_summary_suffix(skipped, pending, timed_out),
    "\n",
  ])
}

fn count_by_status(results: List(TestResult), wanted: Status) -> Int {
  count_matching_status(results, wanted, 0)
}

fn count_matching_status(
  results: List(TestResult),
  wanted: Status,
  count: Int,
) -> Int {
  case results {
    [] -> count
    [result, ..rest] -> {
      let next_count = increment_if_matches(result.status, wanted, count)
      count_matching_status(rest, wanted, next_count)
    }
  }
}

fn increment_if_matches(status: Status, wanted: Status, count: Int) -> Int {
  case status == wanted {
    True -> count + 1
    False -> count
  }
}

fn build_summary_suffix(skipped: Int, pending: Int, timed_out: Int) -> String {
  let parts =
    []
    |> add_summary_part_if_nonzero(skipped, " skipped")
    |> add_summary_part_if_nonzero(pending, " pending")
    |> add_summary_part_if_nonzero(timed_out, " timed out")

  format_summary_parts(parts)
}

fn format_summary_parts(parts: List(String)) -> String {
  case parts {
    [] -> ""
    _ -> string.concat([", ", string.join(parts, ", ")])
  }
}

fn add_summary_part_if_nonzero(
  parts: List(String),
  count: Int,
  label: String,
) -> List(String) {
  case count {
    0 -> parts
    _ -> [string.concat([int.to_string(count), label]), ..parts]
  }
}
