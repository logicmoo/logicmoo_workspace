#!/bin/bash -x

source ./logicmoo_env.sh -v

unset TESTING_TEMP

[ -z "$TESTING_TEMP" ] && [ -d "$(pwd)/test_results" ] && export TESTING_TEMP=$(pwd)/test_results/$(whoami)
[ -z "$TESTING_TEMP" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)

mkdir -p $TESTING_TEMP
# find $TESTING_TEMP -type f -name "Report-*" -delete
rm -f $TESTING_TEMP/?*

TEST_PARAMS="$*"
if [ -z "$TEST_PARAMS" ]; then 
  TEST_PARAMS="*04.p*"
  TEST_PARAMS=attvar_04.pl
fi

echo -e "Running release (all) tests\nTESTING_TEMP=$TESTING_TEMP\n( cd $PWD ; $BASH_SOURCE $TEST_PARAMS )"

# base ./packs_sys/pfc/t
find ./packs_sys/pfc/t -mindepth 2 -name "test_on_*.sh" -execdir bash -c "source '{}' \"$TEST_PARAMS\"" \;

#echo "<testsuites>" > $TESTING_TEMP/junit.xml
#find $TESTING_TEMP -name "Report-*.xml" -exec sed -e "s/<testsuites>//g" -e "s|</testsuites>||g" {} >> $TESTING_TEMP/junit.xml  \;
#echo "</testsuites>" >> $TESTING_TEMP/junit.xml


