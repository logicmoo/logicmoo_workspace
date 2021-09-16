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

JENKINS_BUILD_RESULT=/var/lib/jenkins/jobs/logicmoo_workspace/builds/${BUILD_NUMBER}

# source bin/build_xml.text > $JENKINS_BUILD_RESULT/build.xml

rm -f $TESTING_TEMP/???*.*???

TEST_PARAMS="$*"
if [ -z "$TEST_PARAMS" ]; then 
  TEST_PARAMS="*0*.*"
  #TEST_PARAMS="*f*_01.p*"
fi

echo -e "Running release (all) tests\nTESTING_TEMP=$TESTING_TEMP\n( cd $PWD ; $BASH_SOURCE $TEST_PARAMS )"

lmoo-make 2>&1 | grep -1 -i 'WARN\|ERROR'

find $WAS_PWD -mindepth 2 -name "test_on_*.sh" -execdir {} "$TEST_PARAMS" \;

lmoo jenkins-minor "$TEST_PARAMS"

cd $TESTING_TEMP/ && find . -print | zip jenkin_results_${BUILD_NUMBER}.zip -@

)
