#!/bin/bash

CMD_TIMEOUT=5m

[ ! -z "${MAX_JUNIT_TESTS}" ] && [ ${MAX_JUNIT_TESTS} -lt 10 ] && (
  export CMD_TIMEOUT=40s
  echo "Warn: (MAX_JUNIT_TESTS < 10) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)

export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*01.*"
(
lmoo-junit "$GLOB"
)
stty echo

