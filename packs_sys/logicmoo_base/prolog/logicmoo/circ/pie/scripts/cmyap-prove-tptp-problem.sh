#!/bin/bash
#
# Example calls: see cm-prove-tptp-problem.sh
#
# YAP specials: 
# - YAP needs the file option (since "compile_term" seems not possible)
# - cm_option ff effects nonpretty but faster file output
# - cm_option r4a_max(10) limits number of constraint disequations which
#   might lead to bodies that seem too large for yap
# YAP 6.2.2 got stuck at some problems, seemingly due to an error in YAP
# YAP 6.3.4 worked on these but also seems unstable
#
# cmyap-prove-tptp-problem.sh 100 --options="[file='/tmp/cmyap.pl',add_cm_options=[ff,r4a_max(10)]]" SWB009+3 
#
PIE=${PIE?}
YAP=/Users/ch/get/yap/usr/local/bin/yap

echo TRYING PROBLEM $* 1>&2
date 1>&2

${YAP} -s3000M -h3000M -t500M -f none -g cmprove -t 'halt(1)' \
    -l ${PIE}/folelim/load_yap.pl \
    -- ${2} ${3}

echo FINISHED $* 1>&2


