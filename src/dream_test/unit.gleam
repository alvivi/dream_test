//// Unit test DSL for dream_test.
////
//// This module provides the `describe` and `it` functions for defining tests
//// in a familiar, BDD-style syntax. Tests are organized hierarchically and
//// then flattened into `TestCase` values for the runner.
////
//// ## Quick Start
////
//// ```gleam
//// import dream_test/unit.{describe, it, to_test_cases}
//// import dream_test/assertions/should.{should, equal, or_fail_with}
//// import dream_test/runner.{run_all}
//// import dream_test/reporter/bdd.{report}
//// import gleam/io
////
//// pub fn main() {
////   tests()
////   |> to_test_cases("my_module_test")
////   |> run_all()
////   |> report(io.print)
//// }
////
//// pub fn tests() {
////   describe("Calculator", [
////     describe("add", [
////       it("adds positive numbers", fn() {
////         add(2, 3)
////         |> should()
////         |> equal(5)
////         |> or_fail_with("2 + 3 should equal 5")
////       }),
////       it("handles zero", fn() {
////         add(0, 5)
////         |> should()
////         |> equal(5)
////         |> or_fail_with("0 + 5 should equal 5")
////       }),
////     ]),
////   ])
//// }
//// ```
////
//// ## Output
////
//// ```text
//// Calculator
////   add
////     ✓ adds positive numbers
////     ✓ handles zero
////
//// Summary: 2 run, 0 failed, 2 passed
//// ```
////
//// ## Nesting
////
//// You can nest `describe` blocks as deeply as needed. Each level adds to
//// the test's `full_name`, which the reporter uses for grouping output.
////
//// ```gleam
//// describe("User", [
////   describe("authentication", [
////     describe("with valid credentials", [
////       it("returns the user", fn() { ... }),
////       it("sets the session", fn() { ... }),
////     ]),
////     describe("with invalid credentials", [
////       it("returns an error", fn() { ... }),
////     ]),
////   ]),
//// ])
//// ```

import dream_test/types.{
  type AssertionResult, type TestCase, SingleTestConfig, TestCase, Unit,
}
import gleam/list
import gleam/option.{None}

/// A node in the test tree.
///
/// This type represents either a single test (`ItTest`) or a group of tests
/// (`DescribeGroup`). You typically don't construct these directly—use
/// `it` and `describe` instead.
///
/// ## Variants
///
/// - `ItTest(name, run)` - A single test with a name and body function
/// - `DescribeGroup(name, children)` - A group of tests under a shared name
pub type UnitTest {
  ItTest(name: String, run: fn() -> AssertionResult)
  DescribeGroup(name: String, children: List(UnitTest))
}

/// Define a single test case.
///
/// The test body is a function that returns an `AssertionResult`. Use the
/// `should` API to build assertions that produce this result.
///
/// ## Example
///
/// ```gleam
/// it("calculates the sum correctly", fn() {
///   add(2, 3)
///   |> should()
///   |> equal(5)
///   |> or_fail_with("Expected 2 + 3 to equal 5")
/// })
/// ```
///
/// ## Naming Conventions
///
/// Good test names describe the expected behavior:
/// - ✓ "returns the user when credentials are valid"
/// - ✓ "rejects empty passwords"
/// - ✗ "test1"
/// - ✗ "works"
///
pub fn it(name: String, run: fn() -> AssertionResult) -> UnitTest {
  ItTest(name, run)
}

/// Group related tests under a common description.
///
/// Groups can be nested to any depth. The group names form a hierarchy that
/// appears in test output and failure messages.
///
/// ## Example
///
/// ```gleam
/// describe("String utilities", [
///   describe("trim", [
///     it("removes leading spaces", fn() { ... }),
///     it("removes trailing spaces", fn() { ... }),
///   ]),
///   describe("split", [
///     it("splits on delimiter", fn() { ... }),
///   ]),
/// ])
/// ```
///
/// ## Output
///
/// ```text
/// String utilities
///   trim
///     ✓ removes leading spaces
///     ✓ removes trailing spaces
///   split
///     ✓ splits on delimiter
/// ```
///
pub fn describe(name: String, children: List(UnitTest)) -> UnitTest {
  DescribeGroup(name, children)
}

/// Convert a test tree into a flat list of runnable test cases.
///
/// This function walks the `UnitTest` tree and produces `TestCase` values
/// that the runner can execute. Each test case includes:
///
/// - `name` - The test's own name (from `it`)
/// - `full_name` - The complete path including all `describe` ancestors
/// - `tags` - Currently empty (tag support coming soon)
/// - `kind` - Set to `Unit` for all tests from this DSL
///
/// ## Example
///
/// ```gleam
/// let test_cases =
///   describe("Math", [
///     it("adds", fn() { ... }),
///     it("subtracts", fn() { ... }),
///   ])
///   |> to_test_cases("math_test")
///
/// // test_cases is now a List(TestCase) ready for run_all()
/// ```
///
/// ## Parameters
///
/// - `module_name` - The name of the test module (used for identification)
/// - `root` - The root `UnitTest` node (typically from `describe`)
///
pub fn to_test_cases(module_name: String, root: UnitTest) -> List(TestCase) {
  to_test_cases_from_unit_test(module_name, [], root, [])
}

fn to_test_cases_from_unit_test(
  module_name: String,
  name_prefix: List(String),
  node: UnitTest,
  accumulated: List(TestCase),
) -> List(TestCase) {
  case node {
    ItTest(name, run) ->
      build_it_test_case(module_name, name_prefix, name, run, accumulated)

    DescribeGroup(name, children) -> {
      let new_prefix = list.append(name_prefix, [name])
      to_test_cases_from_list(module_name, new_prefix, children, accumulated)
    }
  }
}

fn build_it_test_case(
  _module_name: String,
  name_prefix: List(String),
  name: String,
  run: fn() -> AssertionResult,
  accumulated: List(TestCase),
) -> List(TestCase) {
  let full_name = list.append(name_prefix, [name])
  let config =
    SingleTestConfig(
      name: name,
      full_name: full_name,
      tags: [],
      kind: Unit,
      run: run,
      timeout_ms: None,
    )
  let test_case = TestCase(config)
  [test_case, ..accumulated]
}

fn to_test_cases_from_list(
  module_name: String,
  name_prefix: List(String),
  remaining: List(UnitTest),
  accumulated: List(TestCase),
) -> List(TestCase) {
  case remaining {
    [] -> list.reverse(accumulated)

    [head, ..tail] -> {
      let updated =
        to_test_cases_from_unit_test(
          module_name,
          name_prefix,
          head,
          accumulated,
        )
      to_test_cases_from_list(module_name, name_prefix, tail, updated)
    }
  }
}
