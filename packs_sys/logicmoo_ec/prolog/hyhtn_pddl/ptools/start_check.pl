#!/usr/bin/csh

echo compile. >input
echo halt.>>input
cp $argv test_domain.pl
# Check arguments
sicstus -l syn_checker.pl< input > check_result
rm test_domain.pl
rm input

