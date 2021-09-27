#!/bin/bash

CMD_TIMEOUT=5m 

[ -n "${MAX_TEST_SUITE_TIME}" ] && [ $MAX_TEST_SUITE_TIME -lt 5 ] && (
  CMD_TIMEOUT=40s
  echo "Warn: (MAX_TEST_SUITE_TIME < 5m) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)

export GLOB="$*"
(
lmoo-junit "*_01*.pl"
)
stty echo

