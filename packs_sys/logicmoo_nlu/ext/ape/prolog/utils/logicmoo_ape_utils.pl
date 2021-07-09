% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2013, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
%
% The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by the Free Software
% Foundation, either version 3 of the License, or (at your option) any later version.
%
% The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE. See the GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with the Attempto
% Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.

:- module(logicmoo_ape_utils, [
		unnumbervars/2,
		ape_numbervars/3,
    ape_numbervars/4
	]).


% :- use_module(logicmoo_ape_utils, [unnumbervars/2, ape_numbervars/3]).




/*
:- debug(verbose).
:- debug(that).
:- debug(toplevel).
:- debug(npcoord).
*/


%:- use_module(logicmoo_ape_utils).

:- if(\+ current_predicate(unnumbervars/2)).

unnumbervars(X, YO) :- notrace(unnumbervars5(X, [], _, YO)),!.

unnumbervars5(Var, Vs, Vs, Var) :- \+ compound(Var), !.
unnumbervars5(Var, Vs, Vs, Var) :- compound_name_arity(Var, _, 0), !.
unnumbervars5((I, TermIn),VsIn,NewVs,(O, TermOut)) :- !,
    unnumbervars5(I, VsIn, VsM, O),
    unnumbervars5(TermIn, VsM, NewVs, TermOut).
unnumbervars5(I:TermIn,VsIn,NewVs,O:TermOut) :-!,
    unnumbervars5(I, VsIn, VsM, O),
    unnumbervars5(TermIn, VsM, NewVs, TermOut).
unnumbervars5([I|TermIn],VsIn,NewVs,[O|TermOut]) :- !,
    unnumbervars5(I, VsIn, VsM, O),
    unnumbervars5(TermIn, VsM, NewVs, TermOut).
unnumbervars5('$VAR'(Name), VsIn, NewVs, Var) :- nonvar(Name),!,
    (   member(Name=Var, VsIn)
    ->  NewVs=VsIn
    ;   NewVs=[Name=Var|VsIn]), !,
    put_attr(Var, vn, Name).
unnumbervars5(PTermIn, VsIn, NewVs, PTermOut) :-
    compound_name_arguments(PTermIn, F, TermIn),
    unnumbervars5(TermIn, VsIn, NewVs, TermOut),
    compound_name_arguments(PTermOut, F, TermOut).
:- endif.

:- if( \+ current_predicate(ape_numbervars/3)).
ape_numbervars(DRSCopy, Zero, N) :- 
    ape_numbervars(DRSCopy, Zero, N, []).
:- endif.

:- if( \+ current_predicate(ape_numbervars/4)).
ape_numbervars(DRSCopy, Zero, N, Options) :-
    numbervars(DRSCopy, Zero, N, [attvar(skip)|Options]).
:- export(ape_numbervars/3).
:- system:import(ape_numbervars/3).
:- endif.


