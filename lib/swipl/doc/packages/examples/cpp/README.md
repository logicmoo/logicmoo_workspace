# SWI-Prolog C++ interface

## Embedding SWI-Prolog in a C++ program

The files likes.pl and  likes.cpp  provide   a  simple  example emedding
SWI-Prolog. To compile, run

    swipl-ld -o likes likes.cpp likes.pl

Next, run as e.g.

    ./likes john
    ./likes -happy

## Extending SWI-Prolog using C++ code

The files `test.pl` and `test.cpp` add foreign predicates to SWI-Prolog.
To compile, run

    swipl-ld -o test -shared test.cpp

Next, run as e.g.

    swipl test.pl
    ?- hello(world).
    Hello world
    true.
