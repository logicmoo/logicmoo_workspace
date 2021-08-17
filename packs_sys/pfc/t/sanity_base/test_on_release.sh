
echo -e "Running release (all) tests\n ( cd $PWD ; $BASH_SOURCE )"

mkdir -p /tmp/logicmoo_testing/
cat /dev/null > /tmp/logicmoo_testing/failures.ansi
cat /dev/null > /tmp/logicmoo_testing/successes.ansi
cat /dev/null > /tmp/logicmoo_testing/test_results.ansi

./test_prolog.sh -k $* | tee >> /tmp/logicmoo_testing/test_results.ansi
cat /tmp/logicmoo_testing/test_results.ansi | ansi2html.sh > ./README.html

