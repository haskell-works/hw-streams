#!/usr/bin/env bash

STACK_FLAGS="
--ghc-options \"-ddump-simpl\"
"

cmd="$1"

shift

case $cmd in
  build)
    stack build \
      --test --no-run-tests --bench --no-run-benchmarks \
      $STACK_FLAGS "$@"
    ;;

  test)
    stack test \
      $STACK_FLAGS "$@"
    ;;

  bench)
    stack bench \
      $STACK_FLAGS "$@"
    ;;

  repl)
    stack repl \
      $STACK_FLAGS "$@"
    ;;
esac
