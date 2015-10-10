:- module(assrt_meta_ex,
	  [amtest/0,
	   amtestf/0,
	   metapred/4]).

:- use_module(assertions(assertions)).
:- use_module(assertions(basicprops)).
:- use_module(assertions(nativeprops)).
:- use_module(library(edinburgh)).
:- use_module(rtchecks(rtchecks)).

:- meta_predicate metapred(+,-,?,1).

:- pred metapred(+atm, ?, ?, ?).

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
