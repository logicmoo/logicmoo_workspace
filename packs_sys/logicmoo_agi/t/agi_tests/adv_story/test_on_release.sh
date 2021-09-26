#!/bin/bash

CMD_TIMEOUT=5m

[ -n "${MAX_JUNIT_SUITES}" ] && [ $MAX_JUNIT_SUITES -lt 10 ] && (
  export CMD_TIMEOUT=40s
  echo "Warn: (MAX_JUNIT_SUITES < 10) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)

export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*01.*"
(
lmoo-junit "$GLOB"
)
stty echo

