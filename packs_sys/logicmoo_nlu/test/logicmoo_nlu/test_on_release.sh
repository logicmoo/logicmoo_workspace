#!/bin/bash -x

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

(
cd $DIR0

if [ ! -f /.dockerenv ]; then
   lmoo $DIR0/$(basename "${BASH_SOURCE[0]}") $*
   return 0 2>/dev/null
   exit 0
fi

CMD_TIMEOUT=5m 

[ -n "${MAX_JUNIT_SUITES}" ] && [ $MAX_JUNIT_SUITES -lt 10 ] && (
  CMD_TIMEOUT=10s
  echo "Warn: (MAX_JUNIT_SUITES < 10) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)


export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*0*.*"
lmoo-junit "$GLOB"

)
