
echo "-------------------------------------------------------"
echo "--- Running tests/test_concurr.sh                   ---"
echo "-------------------------------------------------------"

while test 1=1
do
    case "$1" in
     *-opt*)
	    shift
	    options=$1
	    shift
	    ;;
      *)
	    break
	    ;;
    esac
done

XEMU=$1
NUM=$2

echo "Num is $NUM"

# May make more general later.
#-----------------------------
ALLTESTS="assert_tests io_tests prolog_tests symbol_tests table_tests comp_test negation_tests shared_tests"

if [ -z "$TESTS" ]
then
	TESTS=$ALLTESTS
fi

#-----------------------------

for t_dir in $TESTS
do
	if [ -d $t_dir -a -f $t_dir/test.sh ]
	then
		(	echo "trying $t_dir"
			cd $t_dir
			if test -f core ; then
			    rm -f core
			fi
			sh ./test.sh "$XEMU" $NUM "$options"
			cd ..
		)
	fi
done 
