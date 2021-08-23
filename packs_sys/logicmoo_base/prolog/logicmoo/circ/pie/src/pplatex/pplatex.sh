#!/bin/sh
#
# Usage example:
#
# pplatex.sh --options='[maxpos=2]' ~/tmp/f1.pl ~/tmp/f2.pl
#
# See pplatex_demo.tex for an application.
#
SWIPL="swipl -q -t main -f "
LOADFILE=${PIE}/pplatex/load_pplatex.pl
${SWIPL} ${LOADFILE} -- $*


