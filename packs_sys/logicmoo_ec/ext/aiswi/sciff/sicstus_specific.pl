% Code specific to the host Prolog language
% Version for SICStus Prolog 4

:- module(sicstus_specific,
    [writeln/1, text_style/1]).

writeln(X):-
    write(X), nl.

text_style(Num):-
    write('\e['),
    write(Num),
    write('m').
