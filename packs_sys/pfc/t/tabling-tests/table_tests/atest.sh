#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running table_tests/test.sh using call abstraction    ---"
echo "-------------------------------------------------------"

XEMU=$1
options=$2
valgrind=$3
#========================================================================
# Force some files to be compiled before running the tests.  cycle.P
# will always be compiled, because there was a bug on Linux in compiling
# this file and we want to catch it.  Since this step is performed only
# so that the corresponding object files exist, it is not included as part
# of the garbage collection test.

$XEMU -m 4000 -c 4000 -g none << EOF

compile(lists).
compile(sets).
compile(correct).
compile(utils).
compile(cycle).

EOF
#========================================================================

#--------------------------------------------------
# Tests get_calls and get_returns and tries as code
#--------------------------------------------------
    # XEMU and options must be together in quotes
../agentest.sh "$XEMU $options" concomp "t, d1, d2."
#--------------------------------------------------
../agentest.sh "$XEMU $options" testdyntable "test."
#--------------------------------------------------
../agentest.sh "$XEMU -l $options" aggregs_test "test."
#--------------------------------------------------
../agentest.sh "$XEMU $options" trassert "test."
#--------------------------------------------------
../agentest.sh "$XEMU $options" trdyntest "test."
#--------------------------------------------------
../agentest.sh "$XEMU $options" trdynctest "test."
#--------------------------------------------------
../agentest.sh "$XEMU $options" ins "test."
#--------------------------------------------------
../agentest.sh "$XEMU $options" float "test."
#-------------------------------------------------
../agentest.sh "$XEMU $options" tabbug1 "test."
#-------------------------------------------------
../agentest.sh "$XEMU $options" expand "test."
#-------------------------------------------------
../agentest.sh "$XEMU $options" internt "test."
#-------------------------------------------------
../agentest.sh "$XEMU $options" grammarlrk3 "test."
#-------------------------------------------------
../agentest.sh "$XEMU $options" lrbug "test."
#-------------------------------------------------
../agentest.sh "$XEMU $options" unw_tr1 "test."
#-------------------------------------------------
../agentest.sh "$XEMU $options" trdyn "test".
#-------------------------------------------------
../agentest.sh "$XEMU $options" tfinda "test".
#-------------------------------------------------
# test heap reclamation after check complete
../agentest.sh "$XEMU $options" testh "time2048."
#-------------------------------------------------
../agentest.sh "$XEMU $options" empty_answer "test".
#------------------------------------------------- atp/atc basic
../agentest.sh "$XEMU $options" abol_test "test".
#------------------------------------------------- a module t basic
../agentest.sh "$XEMU $options" abol_test2 "test".
#------------------------------------------------- a module t gc diff preds
../agentest.sh "$XEMU $options" abol_test2a "test".
#------------------------------------------------- atp gc same preds
../agentest.sh "$XEMU $options" abol_test3 "test".
#------------------------------------------------- atp gc diff preds
../agentest.sh "$XEMU $options" abol_test3a "test".
#------------------------------------------------- atp gc diff preds + valid
../agentest.sh "$XEMU $options" abol_test3b "test".
#------------------------------------------------- atp gc diff preds + valid + multiple gcs
../agentest.sh "$XEMU $options" abol_test3c "test".
#-------------------------------------------------
../agentest.sh "$XEMU $options" atc_test "test".
#------------------------------------------------- cascading abolish for subgoals with gc etc.
../agentest.sh "$XEMU $options" abolish_cascade "test". 
#------------------------------------------------- cascading abolish for preds with gc etc.
../agentest.sh "$XEMU $options" abolish_cascade "test". 
#-------------------------------------------------
../agentest.sh "$XEMU $options" recursive_aboltest "test".
#------------------------------------------------- 
../agentest.sh "$XEMU $options" flora1 "test".
#-------------------------------------------------
../agentest.sh "$XEMU $options" pps "test".
#-------------------------------------------------
../agentest.sh "$XEMU $options" dipti "test".
#-------------------------------------------------
../agentest.sh "$XEMU $options" terrys_trie_test "test".
#-------------------------------------------------
../agentest.sh "$XEMU $options" test_trie_property "test".
#-------------------------------------------------
../agentest.sh "$XEMU $options" bulk_trie_test "test".
#-------------------------------------------------
../agentest.sh "$XEMU $options" test_intern_seq "test".
#------------------------------------------------- tests dynamic tables
../agentest.sh "$XEMU $options" test_dynamic "test".
#-------------------------------------------------
../agentest.sh "$XEMU -l $options" aggregs_test_2 "test."
#--------------------------------------------------
../agentest.sh "$XEMU -l $options" test_calldepth "test."
#--------------------------------------------------
../agentest.sh "$XEMU -l $options" test_answerdepth "test."
#--------------------------------------------------
../agentest.sh "$XEMU -l $options" get_resid "test."
#--------------------------------------------------
../agentest.sh "$XEMU -l $options" test_trievars "test."
#--------------------------------------------------
../agentest.sh "$XEMU -l $options" test_table_dump "test."
#--------------------------------------------------
../agentest.sh "$XEMU -l $options" test_ec "test."
#--------------------------------------------------
../agentest.sh "$XEMU -l $options" test_cyclic_tabling "test."
#--------------------------------------------------

if test "$valgrind" = "true"; then
	echo "Skipping test_just in call abstraction"
else
../agentest.sh "$XEMU -l $options" test_just "test."
#--------------------------------------------------
fi