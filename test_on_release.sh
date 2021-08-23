#!/bin/bash -x

[ -z "$TESTING_TEMP" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)
mkdir -p $TESTING_TEMP

source ./logicmoo_env.sh -v

find $TESTING_TEMP -name "Report-*ml" -delete

TEST_PARAMS="$*"
if [ -z "$TEST_PARAMS" ]; then 
  TEST_PARAMS="*04.p*"
fi

echo -e "Running release (all) tests\nTESTING_TEMP=$TESTING_TEMP\n( cd $PWD ; $BASH_SOURCE $TEST_PARAMS )"

find . -mindepth 2 -name "test_on_*.sh" -execdir bash -c "source '{}' $TEST_PARAMS" \;

echo "<testsuites>" > $TESTING_TEMP/junit.xml
find $TESTING_TEMP -name "Report-*.xml" -exec sed -e "s/<testsuites>//g" -e "s|</testsuites>||g" {} >> $TESTING_TEMP/junit.xml  \;
echo "</testsuites>" >> $TESTING_TEMP/junit.xml
cp $TESTING_TEMP/junit.xml /tmp/junit.xml


