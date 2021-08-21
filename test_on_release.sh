
echo -e "Running release (all) tests\n ( cd $PWD ; $BASH_SOURCE )"

source ./logicmoo_env.sh -v

find -name "Report-*ml" -delete

TEST_PARAMS="$*"
if [ -z "$TEST_PARAMS" ]; then 
  TEST_PARAMS="*04.p*"
fi

find . -mindepth 2 -name "test_on_*.sh" -execdir bash '{}' $TEST_PARAMS \;

echo "<testsuites>" > junit.xml
find -name "Report-*.xml" -exec sed -e "s/<testsuites>//g" -e "s|</testsuites>||g" {} >> junit.xml  \;
echo "</testsuites>" >> junit.xml


