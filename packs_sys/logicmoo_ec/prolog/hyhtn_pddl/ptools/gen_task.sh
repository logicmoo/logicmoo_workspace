#!/usr/bin/csh

echo compile\($1\). >input
echo gen_full\($2\). >>input
echo halt.>>input
# Check arguments
sicstus -l gentasks.pl< input
rm input

