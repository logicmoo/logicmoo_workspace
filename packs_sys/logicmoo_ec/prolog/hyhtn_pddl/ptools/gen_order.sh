#!/usr/bin/csh

echo compile\($argv\). >input
echo produce_sort_info.>>input
echo oprecede. >>input
echo halt.>>input
# Check arguments
sicstus -l precede.pl< input
rm input

