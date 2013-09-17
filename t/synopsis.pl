:- use_module(library(regex)).
:- use_module(library(tap)).

synopsis :-
    '99 Bottles of Beer' =~ '[0-9]+ bottles'/i,
    writeln('Take one down...').
