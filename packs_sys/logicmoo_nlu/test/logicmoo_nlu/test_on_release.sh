#!/bin/bash -x

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

(
cd $DIR0

if [ ! -f /.dockerenv ]; then
   lmoo $DIR0/$(basename "${BASH_SOURCE[0]}") $*
   return 0 2>/dev/null
   exit 0
fi

export CMD_TIMEOUT=5m 
export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*0*.*"
lmoo-junit "$GLOB"

)
