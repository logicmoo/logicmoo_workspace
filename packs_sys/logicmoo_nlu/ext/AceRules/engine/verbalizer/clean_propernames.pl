% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(clean_propernames, [
		clean_propernames/2  % +FactsIn, -FactsOut
	]).

/** <module> Clean up proper names

This module removes the facts from a factset that denote unrelated proper names.
The ACE verbalizer cannot handle such unrelated proper names ("There is John" is
not a valid ACE sentence).

@author Tobias Kuhn
@version 2007-06-06
*/


%% clean_propernames(+FactsIn, -FactsOut)
%
% Removes all the unrelated proper names.

clean_propernames(FactsIn, FactsOut) :-
    collect_needed_propernames(FactsIn, PN),
    filter_propernames(FactsIn, PN, FactsOut).


collect_needed_propernames([], []).

collect_needed_propernames([Fact|Rest], PN) :-
    Fact = object(_,_,named,_,_,_),
    !,
    collect_needed_propernames(Rest, PN).

collect_needed_propernames([Fact|Rest], PN) :-
    collect_propernames(Fact, PN1),
    collect_needed_propernames(Rest, PNRest),
    append(PN1, PNRest, PN).


collect_propernames(v(P), [P]) :-
    atom(P),
    !.

collect_propernames([], []) :-
	!.

collect_propernames([H|T], PN) :-
	!,
	collect_propernames(H, PN1),
	collect_propernames(T, PN2),
	append(PN1, PN2, PN).

collect_propernames(Term, []) :-
	Term =.. [Term],
	!.

collect_propernames(Term, PN) :-
	!,
	Term =.. List,
	collect_propernames(List, PN).


filter_propernames([], _, []).

filter_propernames([Fact|RestIn], PN, Out) :-
    Fact = object(_,P,named,_,_,_),
    \+ member(P, PN),
    !,
    filter_propernames(RestIn, PN, Out).

filter_propernames([Fact|RestIn], PN, [Fact|RestOut]) :-
    filter_propernames(RestIn, PN, RestOut).
