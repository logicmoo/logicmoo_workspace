#!/bin/bash -x

apt update
apt install -y gawk

cd /opt/logicmoo_workspace
export TESTING_TEMP=$(pwd)/test_results/jenkins

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
(
cd $DIR0/../
source ./logicmoo_env.sh -v

WAS_PWD=$PWD

#unset TESTING_TEMP


[ -z "$TESTING_TEMP" ] && [ -d "$(pwd)/test_results" ] && export TESTING_TEMP=$(pwd)/test_results/$(whomai)
[ -z "$TESTING_TEMP" ] && [ -d "${LOGICMOO_WS}/test_results" ] && export TESTING_TEMP=${LOGICMOO_WS}/test_results/$(whomai)
[ -z "$TESTING_TEMP" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)
export TESTING_TEMP
mkdir -p $TESTING_TEMP/

TEST_PARAMS="$*"
if [ -z "$TEST_PARAMS" ]; then 
  TEST_PARAMS="*0*.p*"
fi

echo -e "Running release (all) tests\nTESTING_TEMP=$TESTING_TEMP\n( cd $PWD ; $BASH_SOURCE $TEST_PARAMS )"

find $WAS_PWD -mindepth 2 -name "test_from_docker.sh" -execdir bash -c "source '{}' \"$TEST_PARAMS\"" \;

)

