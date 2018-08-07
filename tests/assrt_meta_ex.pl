:- module(assrt_meta_ex,
          [amtest/0,
           amtestf/0,
           metapred/4,
	   amtestf2/0]).

:- use_module(library(assertions)).
:- use_module(library(globprops)).
:- use_module(library(typeprops)).
:- use_module(library(edinburgh)).
:- use_module(library(mapnargs)).

:- meta_predicate metapred(+,-,?,1).

:- pred metapred(+atm, ?, ?, ?).

:- comp metapred/4 + meta_modes.

amtest :-
    metapred(patata, Output, IO, display),
    display(Output-IO).

amtestf :-
    level1(Output, IO), !,
    display(Output-IO).
amtestf.

level1(Output, IO) :-
    metapred(patata(2), Output, IO, undefined_proc),
    display(done),
    nl.

metapred(Input, Output, IO, Call) :-
    call(Call, Input),
    display(Input),
    Output=out(Input),
    IO=(Input-Output).

display1(N, T) :- writeln(N-T).

amtestf2 :-
    Data = data(a, b, c),
    mapnargs(display1, Data).
