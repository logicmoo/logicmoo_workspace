#!/bin/bash
#
# Variables PIE and TPTPDirectory must be set
#
# Example Calls: 
#
# cm-prove-tptp-problem.sh 100 SWB009+3
# cm-prove-tptp-problem.sh 100 --options="[proof=pp]" SWB009+3
# cm-prove-tptp-problem.sh 100 --options="[cm_cfg=lean]" SWB009+3
# cm-prove-tptp-problem.sh 100 --options="[proof=pp,file='/tmp/cm.out']" SWB009+3
# cm-prove-tptp-problem.sh 100 --options="[add_cm_options=[hd]-[hd1]]" SWB009+3
#
PIE=${PIE?}
SWIPL=swipl

echo TRYING PROBLEM $* 1>&2
date 1>&2

# SWIOPTIONS="-O -G3g -L3g -T500m"
SWIOPTIONS="-O --stack_limit=12G"

echo SWI Options: ${SWIOPTIONS} 1>&2
echo ULIMITS: 1>&2

# Not effective on OS/X:
ulimit -t ${1}
ulimit -a 1>&2


${SWIPL} ${SWIOPTIONS} -f ${PIE}/folelim/load.pl -g cmprove -t 'halt(1)' \
	 -- ${2} ${3}

echo FINISHED $* 1>&2
