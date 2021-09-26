#!/bin/bash -x

stty echo

CMD_TIMEOUT=1m
[ -n "${MAX_JUNIT_TESTS}" ] && [ $MAX_JUNIT_TESTS -lt 10 ] && (
  CMD_TIMEOUT=10s
  echo "Warn: (MAX_JUNIT_TESTS < 10) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)


export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*[0-9]*.pl"
(
# lmoo-junit "$GLOB"
)
stty echo

