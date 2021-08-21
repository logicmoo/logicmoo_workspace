
echo -e "Running release (all) tests\n ( cd $PWD ; $BASH_SOURCE )"

source ./logicmoo_env.sh -v

find -name "Report-*ml" -delete

TEST_PARAMS="$*"

find . -mindepth 2 -name "test_on_*.sh" -execdir bash '{}' $TEST_PARAMS \;

echo "<testsuites>" > junit.xml
find -name "Report-*.xml" -exec sed -e "s/<testsuites>//g" -e "s|</testsuites>||g" {}  \;
echo "</testsuites>" >> junit.xml


