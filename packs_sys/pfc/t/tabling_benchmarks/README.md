# Benchmarks for delimited continuation based tabling

Checkout next to
[tabling_library](https://github.com/JanWielemaker/tabling_library). Run
one of

 - `./test.sh file ...` where file is a `*-hprolog.pl` file
 - `./test.sh` to run all benchmarks

To evaluate a single one in detail, run, e.g.,

    swipl 20kpingpong-hprolog.pl
    ?- profile(go).
