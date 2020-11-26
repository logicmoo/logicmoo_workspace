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


:- module(list_utils, [
		remove/3,             % +ListIn, +Element, -ListOut
		remove_exact/3,       % +ListIn, +Element, -ListOut
		remove_all/3,         % +ListIn, +Element, -ListOut
		remove_all_exact/3,   % +ListIn, +Element, -ListOut
		remove_dublicates/2,  % +ListIn, -ListIn
		is_member/2,          % +Member, +List
		is_exact_member/2,    % +Member, +List
		delete_pattern/3,     % +ListIn, +Pattern, -ListOut
		is_open_list/1,       % ?List
		close_list/1,         % ?List
		concat_list/2,        % +List, -Atom
		subtract_list/3       % +List1, +List2, -ListOut
	]).

/** <module> Utilities for list handling

Some utility predicates for list handling.

@author Tobias Kuhn
@version 2007-02-06

@see utils.pl
*/


%% is_member(+Element, +List)
%
% Succeeds if Element is contained in List. Unlike the built-in member/2,
% Element and List stay unchanged. No instantiation takes place.

is_member(Member1, [Member2|_]) :-
    \+ \+ Member1 = Member2.  % prevent instantiation

is_member(Member, [_|Rest]) :-
    is_member(Member, Rest).


%% is_exact_member(+Element, +List)
%
% Succeeds if the list contains an exact occurrence of Element. Thus, the matching criterion
% is equality (==), not unification.

is_exact_member(Member1, [Member2|_]) :-
    Member1 == Member2.

is_exact_member(Member, [_|Rest]) :-
    is_exact_member(Member, Rest).


%% remove(+ListIn, +Element, -ListOut)
%
% Removes the first occurence of Element from the list ListIn and stores the result in ListOut.
% The matching criterion is unification (=). Unification is only checked, but not applied.

remove([E|Rest], D, Rest) :-
	\+ \+ E = D,
	!.

remove([E|RestIn], D, [E|RestOut]) :-
	remove(RestIn, D, RestOut).


%% remove_exact(+ListIn, +Element, -ListOut)
%
% Removes the first occurence of Element from the list ListIn and stores the result in ListOut.
% The matching criterion is equality (==), not unification.

remove_exact([E|Rest], D, Rest) :-
	E == D,
	!.

remove_exact([E|RestIn], D, [E|RestOut]) :-
	remove_exact(RestIn, D, RestOut).


%% remove_all(+ListIn, +Element, -ListOut)
%
% Removes all occurences of Element from the list ListIn and stores the result in ListOut.
% The matching criterion is unification (=). Unification is only checked, but not applied.

remove_all(List, [], List).

remove_all(ListIn, [D|Rest], ListOut) :-
	remove(ListIn, D, ListTemp),
	remove_all(ListTemp, Rest, ListOut).


%% remove_all_exact(+ListIn, +Element, -ListOut)
%
% Removes all occurences of Element from the list ListIn and stores the result in ListOut.
% The matching criterion is equality (==), not unification.

remove_all_exact(List, [], List).

remove_all_exact(ListIn, [D|Rest], ListOut) :-
	remove_exact(ListIn, D, ListTemp),
	remove_all_exact(ListTemp, Rest, ListOut).


%% remove_dublicates(+ListIn, -ListOut)
%
% Removes all the dublicates from the list. The matching criterion is equality (==).

remove_dublicates([], []).

remove_dublicates([Dublicate|RestIn], Out) :-
    is_exact_member(Dublicate, RestIn),
    !,
    remove_dublicates(RestIn, Out).

remove_dublicates([First|RestIn], [First|RestOut]) :-
    remove_dublicates(RestIn, RestOut).


%% delete_pattern(+ListIn, +Pattern, -ListOut)
%
% Removes all occurences of the pattern from the list. An element matches the
% pattern if it is equivalent to the pattern or if the pattern is more general
% than the element.
%
% Example:
%
% ==
% ?- delete_pattern([A,p(B,C),p(a,D),p(E,b),p(a,b)], p(a,_), ListOut).
% ListOut = [A,p(B,C),p(E,b)]
% ==

delete_pattern([], _, []).

delete_pattern([Element|RestIn], Pattern, Out) :-
    copy_term(Element, ElementC),
    numbervars(ElementC, 1, _, [functor_name('$delete_pattern$')]),
       % using this strange functor name in order to prevent any conflicts
       % with other calls of numbervars.
    \+ \+ ElementC = Pattern,  % prevent instantiation
    !,
    delete_pattern(RestIn, Pattern, Out).

delete_pattern([Element|RestIn], Pattern, [Element|RestOut]) :-
    delete_pattern(RestIn, Pattern, RestOut).


%% is_open_list(?List)
%
% Succeeds if the list does not have a restricted length (i.e. the tail is not instantiated
% at some point).

is_open_list(List) :-
    var(List),
    !.

is_open_list([_|Tail]) :-
    is_open_list(Tail).


%% close_list(?List)
%
% A list with an unrestricted length is given a definite length. If the tail is not instantiated
% at some point, this tail is unified with the empty list.

close_list([]) :-
    !.

close_list([_|Rest]) :-
    close_list(Rest).


%% concat_list(+AtomList, -Atom).
%
% The atoms of AtomList are concatenated to a single atom.

concat_list([], '').

concat_list([Atom|Rest], Concat) :-
    concat_list(Rest, ConcatRest),
    atom_concat(Atom, ConcatRest, Concat).


%% subtract_list(+ListIn, +Minus, -ListOut)
%
% All elements of Minus are removed from ListIn and the result is stored in ListOut.

subtract_list([], _, []).

subtract_list([First|RestIn], Minus, Out) :-
    member(First, Minus),
    !,
    subtract_list(RestIn, Minus, Out).

subtract_list([First|RestIn], Minus, [First|RestOut]) :-
	subtract_list(RestIn, Minus, RestOut).
