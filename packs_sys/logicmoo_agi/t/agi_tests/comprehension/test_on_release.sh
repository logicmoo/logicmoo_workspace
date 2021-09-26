#!/bin/bash

CMD_TIMEOUT=5m 

[ ! -z "${MAX_JUNIT_TESTS}" ] && [ ${MAX_JUNIT_TESTS} -lt 10 ] && (
  CMD_TIMEOUT=40s
  echo "Warn: (MAX_JUNIT_TESTS < 10) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)

export GLOB="$*"
(
lmoo-junit "*_01*.pl"
)
stty echo

