
echo -e "Running release (all) tests\n ( cd $PWD ; $BASH_SOURCE )"

mkdir -p /tmp/logicmoo_testing/
cat /dev/null > /tmp/logicmoo_testing/failures.ansi
cat /dev/null > /tmp/logicmoo_testing/successes.ansi
cat /dev/null > /tmp/logicmoo_testing/test_results.ansi

export TEST_STEM=test-stem-$(echo "$*" | sed -e "s/[^a-Z0-9]/-/g" )
echo "TEST_STEM=${TEST_STEM}"
test_prolog.sh -k "fc_01*" | tee >> /tmp/logicmoo_testing/test_results.ansi
cat /tmp/logicmoo_testing/test_results.ansi | ansi2html.sh > ./README.html
sed -r "s/\x1B\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g" /tmp/logicmoo_testing/test_results.ansi > ./$TEST_STEM-junit.xml
junit2html ./$TEST_STEM-junit.xml ./$TEST_STEM-junit.html 
junit-viewer ./$TEST_STEM-junit.xml --save=./$TEST_STEM-junit-viewer.html 
echo "TEST_STEM=${TEST_STEM}"

