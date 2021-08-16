
echo -e "Running release (all) tests\n ( cd $PWD ; $BASH_SOURCE )"

find . -mindepth 2 -name "test_on_*.sh" -execdir bash '{}' \;
