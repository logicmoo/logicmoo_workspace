%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(formutils_fol,
	  [disjoin/3,
	   conjoin/3,
	   allquant/3,
	   exquant/3,
	   negate/2]).

disjoin(true, _, true) :- !.
disjoin(_, true, true) :- !.
disjoin(false, F, F) :- !.
disjoin(F, false, F) :- !.
disjoin(F, G, F) :- F == G, !.
disjoin(F, ~G, true) :- F == G, !.
disjoin(~F, G, true) :- F == G, !.
disjoin(F, G, (F ; G)) :- !.

conjoin(false, _, false) :- !.
conjoin(_, false, false) :- !.
conjoin(true, F, F) :- !.
conjoin(F, true, F) :- !.
conjoin(F, G, F) :- F == G, !.
conjoin(F, ~G, false) :- F == G, !.
conjoin(~F, G, false) :- F == G, !.
conjoin(F, G, (F , G)) :- !.

negate(~F, F) :- !.
negate((F,G), (F1;G1)) :- !, negate(F, F1), negate(G, G1).
negate((F;G), (F1,G1)) :- !, negate(F, F1), negate(G, G1).
negate((F->G), (F,G1)) :- !, negate(G, G1).
negate((F<-G), (F1,G)) :- !, negate(F, F1).
negate((F <-> ~G),(F<->G)) :- !.
negate((~F <-> G),(F<->G)) :- !.
negate((F <-> G), (F <-> G1)) :- !, negate(G, G1).
negate(all(X, F), ex(X, F1)) :-	!, negate(F, F1).
negate(ex(X, F), all(X, F1)) :-	!, negate(F, F1).
negate(all2(X, F), ex2(X, F1)) :- !, negate(F, F1).
negate(ex2(X, F), all2(X, F1)) :- !, negate(F, F1).
negate(F, ~F).

exquant(_, true, true) :- !.
exquant(_, false, false) :- !.
exquant(V, F, F1) :-
	var(V),
	!,
	exquant([V], F, F1).
exquant([], F, F) :- !.
exquant(V, F, ex(V, F)).

allquant(_, true, true) :- !.
allquant(_, false, false) :- !.
allquant(V, F, F1) :-
	var(V),
	!,
	allquant([V], F, F1).
allquant([], F, F) :- !.
allquant(V, F, all(V, F)).	
	   