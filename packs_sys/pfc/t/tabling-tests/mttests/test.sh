#! /bin/sh

XEMU=$1
NUM=$2
options=$3

echo "-------------------------------------------------------"
echo "--- Running table_tests/test.sh w. $NUM threads    ---"
echo "-------------------------------------------------------"

#------------------------------------
# tests involving definite tabling
#------------------------------------
# XEMU and options must be together in quotes
# convention is that to use test_single, leave the last argument of test
# goal uninstantiated to unify with the stream.

../gentest.sh "$XEMU $options" tableread "tableread(1000,_)" $NUM
../gentest.sh "$XEMU $options" tableread2 "tableread(1000,_)" $NUM
#../gentest.sh "$XEMU $options" tableread "tableread(10000,_)" $NUM
#../gentest.sh "$XEMU $options" tableread2 "tableread(10000,_)" $NUM
../gentest.sh "$XEMU $options" tableread3 "tableread(10,_)" $NUM
# ../gentest.sh "$XEMU $options" tablereadcond "tableread(10000,_)" $NUM
 ../gentest.sh "$XEMU $options" tablereadcond "tableread(1000,_)" $NUM
#../gentest.sh "$XEMU $options" tableread4 "tableread(1000,_)" $NUM

#../gentest.sh "$XEMU $options" tableread_dyn "tableread(10000,_)" $NUM

../gentest.sh "$XEMU $options" tablewrite "test(_)" $NUM
../gentest.sh "$XEMU $options" tablewrite2 "test(_)" $NUM
../gentest.sh "$XEMU $options" tablewrite3 "test(_)" $NUM
../gentest.sh "$XEMU $options" tablewrite4 "tablewrite(100,_)" $NUM
../gentest.sh "$XEMU $options" tablewrite5 "tablewrite(100,_)" $NUM
../gentest.sh "$XEMU $options" tablewrite6 "tablewrite(100,_)" $NUM

../gentest.sh "$XEMU $options" tablewrites1 "tablewrite(100,_)" $NUM
../gentest.sh "$XEMU $options" tablewrites2 "tablewrite(100,_)" $NUM
../gentest.sh "$XEMU $options" tablewrites3 "tablewrite(100,_)" $NUM
../gentest.sh "$XEMU $options" tablewrites4 "tablewrite(100,_)" $NUM
../gentest.sh "$XEMU $options" tablewrites5 "tablewrite(100,_)" $NUM

../gentest.sh "$XEMU $options" tablewrites6 "tablewrite(100,_)" $NUM
../gentest.sh "$XEMU $options" tablewrites7 "tablewrite(100,_)" $NUM

# test of private subsumption.
../seqgentest.sh "$XEMU $options" subsumption1 "test." 

../seqgentest.sh "$XEMU $options" abol_test "test." # test of ast/apt with stack ch
../seqgentest.sh "$XEMU $options" abol_test2 "test." # test of ast/apt with stack ch
../seqgentest.sh "$XEMU $options" abol_test3 "test." # test of atp w. mult threads.
../seqgentest.sh "$XEMU $options" abol_test4 "test."  # test of atp w. mult threads.
../seqgentest.sh "$XEMU $options" abol_test5 "test."  # test of gc w. mult threads

../seqgentest.sh "$XEMU $options" abol_test3_call "test." # test of atc w. mult threads.
../seqgentest.sh "$XEMU $options" abol_test4_call "test." # test of atc w. mult threads.
../seqgentest.sh "$XEMU $options" abol_test5_call "test." # test of atc w. mult threads.

../seqgentest.sh "$XEMU $options" abolish_cascade_conc "test." # test of cascading atc w. mult threads.
../seqgentest.sh "$XEMU $options" abolish_cascade_pred_conc "test." # test of cascading atp w. mult threads.

../seqgentest.sh "$XEMU $options" test_new_intern "test." 
