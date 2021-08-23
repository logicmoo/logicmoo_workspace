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

:- module(prune, [coverof/3, 
                  prune_instances/2,
		  key_coverof/3,
		  key_prune_instances/2]).

:- use_module(swilib(term_handling)).

:- module_transparent(coverof/3).
:- module_transparent(key_coverof/3).

%% 
%% coverof(?Term, +Goal, ?List)
%% 
%% Succeeds if List is the (non-empty) list of all the most general instances
%% of Term such that Goal is provable.
%% 
%% Description
%%    Unifies List with the list (not ordered, duplicates removed, pruned) of
%%    all instances of Term such that Goal is satisfied.  prune_instances/2 is
%%    used on the list of solutions, with the result that no element of List
%%    is an instance of any other element.
%%
%% prune_instances(+List, ?PrunedList)
%% 
%% Description
%%    Used to get the smallest list PrunedList whose elements subsume elements
%%    of the list List.  List must not contain variables.  If List contains
%%    elements which are variants of each other, then of these, PrunedList
%%    will only contain the first element found.  If List contains element(s)
%%    which are instances of another element, then of these, PrunedList will
%%    only contain the latter.
%% 

coverof(Term, Goal, List) :-
	bagof(Term, Goal, List1),
	prune_instances(List1, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% modified from lib/sorts.pl. prune_instances as implemented by eclipse
%%%% doesn't work with an instance_handler for meta attributes since it is
%%%% based on compare_instances/3. Is this a Eclipse bug???
%%%% 
%%%% NOTE: coverof correctly calls the instance_handler 
%%%%
%%%% NOTE: THIS ECLIPSE BUG SEEMS FIXED IN 3.6.1 
%%%% (prune_instances is implemented
%%%% differently there)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prune_instances(List, Pruned) :-
        prune(List, [], Pruned).

% :- mode prune1(?,+,?).
prune([First|Rest], SoFar, Result) :-
        prune1(First, SoFar, NewSoFar),
        prune(Rest, NewSoFar, Result).
prune([], Result, Result).
 
% :- mode prune1(?,+,-).
prune1(Elem, [], [Elem]) :-
	!.
prune1(Elem, [First|Rest], Result) :-
        % compare_instances(Order, First, Elem),
        % !,
	( subsumes_chk(First, Elem) ->      %% Order = > or =
	  Result = [First|Rest]
        ; subsumes_chk(Elem, First) ->      %% Order = <
	  prune1(Elem, Rest, Result)
        ),
	!.
prune1(Elem, [First|Rest], [First|Res]) :-
        prune1(Elem, Rest, Res).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% key_prune_instances/2
%%%%
%%%% A version of prune instances that takes a list of pairs Key-Value and
%%%% ignores the keys in comparing the terms.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

key_prune_instances(List, Pruned) :-
        key_prune(List, [], Pruned).
 
% :- mode key_prune1(?,+,?).
key_prune([First|Rest], SoFar, Result) :-
        key_prune1(First, SoFar, NewSoFar),
        key_prune(Rest, NewSoFar, Result).
key_prune([], Result, Result).
 
% :- mode key_prune1(?,+,-).
key_prune1(Elem, [], [Elem]).
key_prune1(Elem, [First|Rest], Result) :-
        % compare_instances(Order, First, Elem),
        % !,
	_-ElemVal = Elem,
	_-FirstVal = First,
	( subsumes_chk(FirstVal, ElemVal) ->      %% Order = > or =
	  Result = [First|Rest]
        ; subsumes_chk(ElemVal, FirstVal) ->      %% Order = <
	  key_prune1(Elem, Rest, Result)
        ),
	!.
key_prune1(Elem, [First|Rest], [First|Res]) :-
        key_prune1(Elem, Rest, Res).

key_coverof(A, B, C) :-
	bagof(A, B, C1),
	key_prune_instances(C1, C).

