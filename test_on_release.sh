#!/bin/bash

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
(
cd $DIR0
source ./logicmoo_env.sh -v

WAS_PWD=$PWD

export keep_going="-k"

#unset TESTING_TEMP

[ -z "$TESTING_TEMP" ] && [ -d "$(pwd)/test_results" ] && export TESTING_TEMP=$(pwd)/test_results/$(whoami)
[ -z "$TESTING_TEMP" ] && [ -d "${LOGICMOO_WS}/test_results" ] && export TESTING_TEMP=${LOGICMOO_WS}/test_results/$(whoami)
[ -z "$TESTING_TEMP" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)
export TESTING_TEMP
mkdir -p $TESTING_TEMP/   


find $TESTING_TEMP -type f -name "*-junit.xml" -delete
find $TESTING_TEMP -type f -name "*-rollup.html" -delete
rm -f $TESTING_TEMP/???*

TEST_PARAMS="$*"
if [ -z "$TEST_PARAMS" ]; then 
  TEST_PARAMS="*0*.*"
fi

echo -e "Running release (all) tests\nTESTING_TEMP=$TESTING_TEMP\n( cd $PWD ; $BASH_SOURCE $TEST_PARAMS )"

lmoo-make 2>&1 | grep -2 -i 'WARN\|ERROR'

find $WAS_PWD -mindepth 2 -name "test_on_*.sh" -execdir {} "$TEST_PARAMS" \;

lmoo jenkins-minor "$TEST_PARAMS"

#echo "<testsuites>" > $TESTING_TEMP/junit.xml
#find $TESTING_TEMP -name "Report-*.xml" -exec sed -e "s/<testsuites>//g" -e "s|</testsuites>||g" {} >> $TESTING_TEMP/junit.xml  \;
#echo "</testsuites>" >> $TESTING_TEMP/junit.xml

)
