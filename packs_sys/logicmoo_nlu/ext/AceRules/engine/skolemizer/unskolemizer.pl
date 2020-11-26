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


:- module(unskolemize, [
				 unskolemize/3   % +Functor, +TermIn, -TermOut
				]).

/** <module> Unskolemizer

This module can undo a skolemization. This means that skolem-constants are replaced
by variables. Skolem-constants that are identical are replaced by the same variable.

@author Tobias Kuhn
@version 2007-02-08
*/


%% unskolemize(+Functor, +TermIn, -TermOut)
%
% Replaces the skolem-constants in TermIn by variables. The skolem-constants need to
% have the form f(c) where f corresponds to Functor and c is an atom.

unskolemize(Functor, TermIn, TermOut) :-
    unskolemize(Functor, TermIn, TermOut, [], _).


unskolemize(_Functor, Var, Var, Map, Map) :-
    var(Var),
    !.

unskolemize(Functor, TermIn, Var, MapIn, MapOut) :-
    TermIn =.. [Functor, N],
    !,
    (member((N, V), MapIn) ->
       Var = V,
       MapOut = MapIn
    ;
       MapOut = [(N, Var)|MapIn]
    ).

unskolemize(_Functor, [], [], Map, Map) :-
	!.

unskolemize(Functor, [H1|T1], [H2|T2], MapIn, MapOut) :-
	!,
	unskolemize(Functor, H1, H2, MapIn, MapTemp),
	unskolemize(Functor, T1, T2, MapTemp, MapOut).

unskolemize(_Functor, Term, Term, Map, Map) :-
	Term =.. [Term],
	!.

unskolemize(Functor, Term1, Term2, MapIn, MapOut) :-
	!,
	Term1 =.. List1,
	unskolemize(Functor, List1, List2, MapIn, MapOut),
	Term2 =.. List2.
