# Simple Makefile for dream_test

.PHONY: test bootstrap bootstrap-core bootstrap-core-types bootstrap-should all

# Run gleeunit tests
test:
	gleam test

# Run all bootstrap checks
bootstrap: bootstrap-core bootstrap-core-types bootstrap-should

bootstrap-core:
	gleam run -m dream_test/bootstrap/bootstrap_core_assert

bootstrap-core-types:
	gleam run -m dream_test/bootstrap/bootstrap_core_types

bootstrap-should:
	gleam run -m dream_test/bootstrap/bootstrap_should

# Run everything: unit tests + all bootstraps
all: test bootstrap
