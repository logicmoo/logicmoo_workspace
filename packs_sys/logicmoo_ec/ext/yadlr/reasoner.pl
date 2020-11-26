%% Usage examples usage of multi-valued DL reasoner
%% This file contains the kb of the domain.
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 6-2-2007
%% This file is in the public domain.

% yadlr alias
user:file_search_path(yadlr, X) :- getenv('LOGICMOO_HOME',MOO),atom_concat(MOO,'/src_modules/yadlr/pl', X).

% aleph alias must resolve to the directory where aleph.pl exists.
% you can download aleph from http://www.comlab.ox.ac.uk/oucl/research/areas/machlearn/Aleph/aleph.pl
user:file_search_path(aleph, X) :- getenv('LOGICMOO_HOME',MOO),atom_concat(MOO,'/src_modules/aleph', X).

user:file_search_path(logicmoo, X) :- getenv('LOGICMOO_HOME',MOO),atom_concat(MOO,'/src', X).

:- visible(+all), leash(-exit),leash(-fail),leash(-call),leash(-redo).

dmsg(X):-fmt(';; ~q. ~n',[X]).

prolog_engine(swi).

db_recorded(X,Y,Z):-recorded(X,Y,Z),dmsg(recorded(X,Y,Z)).
db_recorda(X,Y,Z):-recorda(X,Y,Z),dmsg(recorda(X,Y,Z)).
db_recordz(X,Y,Z):-recordz(X,Y,Z),dmsg(recordz(X,Y,Z)).


%%:- use_module('../../src/logicmoo_util/logicmoo_util_all.pl').
assert_if_new(X):-catch(X,_,fail),!.
assert_if_new(X):-assertz(X).

fmt(X,Y):-'format'(X,Y).
fmt(X,Y,Z):-'format'(X,Y,Z).

remove_duplicates([], []).
remove_duplicates([Elem|L], [Elem|NL]) :- delete(L, Elem, Temp), remove_duplicates(Temp, NL).

:- assert_if_new( use_inference_engine(resolution) ).
:- assert_if_new( use_algebra(alg_lukasiewicz) ).
:- ensure_loaded('pl/dllearn').
:- use_module('pl/yadlr').
%:- use_module('pl/resolution').
%:- use_module('pl/prodlr').

:- consult(domain).

/*
CURRENTLY SUPPORTED SYNTAX:
F :== dlnot(F)
F :== dland(F, F)
F :== dlor(F, F)
F :== dlimplies(F, F)
F :== dlequiv(F, F)
F :== C(I)
C :== PredicateName  
I :== InstanceName(s)

TODO:
F :== all(X, F)
F :== exists(X, F)
F :==  atmost(X, N, F)
F :== atleast(X, N, F)
F :== box F
F :== dia F
F :== cir F
F :== until(F, F)

X :== VariableName
N :== Integer
*/

:-consult('abstract-shoiq.pl').
% :-consult('abstract-alc.pl').
% :-consult('simple-alc.pl').

askres( KB, Query):- prove( KB, Query, _FuzzyDegree, _Open, _Restr ).

:-preparation.
