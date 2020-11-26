#!/bin/sh

PORT=${1:-2213}
DIR=${INFRADIR:-${HOME}/infra}
SWIPL=pl
if type -t swipl >/dev/null
then 
    SWIPL=swipl    
fi

${SWIPL} -G128M -L128M -T64M -f ${DIR}/src/load.pl -g start\(${PORT}\)

