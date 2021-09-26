#!/bin/bash -x

CMD_TIMEOUT=1m

[ ! -z "${MAX_JUNIT_TESTS}" ] && [ ${MAX_JUNIT_TESTS} -lt 10 ] && (
  CMD_TIMEOUT=10s
  echo "Warn: (MAX_JUNIT_TESTS < 10) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)

lmoo-junit  "$*"


