
echo -e "Running release (all) tests\n ( cd $PWD ; $BASH_SOURCE )"

./test_prolog.sh | tee test_results.ansi
cat test_results.ansi | ansi2html.sh > README.html

