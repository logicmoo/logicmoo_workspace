#!/bin/bash -x

stty echo

CMD_TIMEOUT=1m
[ -n "${MAX_TEST_SUITE_TIME}" ] && [ $MAX_TEST_SUITE_TIME -lt 5 ] && (
  export CMD_TIMEOUT=10s
  echo "Warn: (MAX_TEST_SUITE_TIME < 5m) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)


export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*[0-9]*.pl"
(
# lmoo-junit "$GLOB"
)
stty echo

