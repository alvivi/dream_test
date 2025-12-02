# Simple Makefile for dream_test

.PHONY: test examples all

# Run dream_test tests
test:
	gleam test

# Run example project tests
examples:
	cd examples/snippets && gleam test
	cd examples/shopping_cart && gleam test
	cd examples/cache_app && gleam test

# Run everything: dream_test tests + examples
all: test examples
