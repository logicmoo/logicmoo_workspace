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


:- module(poor_to_rich, [
		poor_to_rich/2  % +PoorRules, -RichRules
	]).

:- use_module('../list_utils').
:- use_module(atom_map).

/** <module> Poor to rich converter

Maps a rule representation using rich format into poor format. The module rich_to_poor.pl
does the reverse transformation

See stable_interpreter.pl for details.

@author Tobias Kuhn
@version 2007-02-09

@see rich_to_poor.pl
@see stable_interpreter.pl
*/


%% poor_to_rich(+PoorRules, -RichRules)
%
% Translates the rules in poor format (PoorRules) into rich format (RichRules).
%
% @see stable_interpreter.pl

poor_to_rich(Poor, Rich) :-
    transform(Poor, Rich).


transform(Var, Var) :-
    var(Var),
    !.

transform(Number, Number) :-
    number(Number),
    !.

transform(PoorAtom, RichAtom) :-
    richatom_pooratom(RichAtom, PoorAtom),
    !.

transform(Atom, AtomDecoded) :-
    atom(Atom),
    atom_codes(Atom, [120,120,120,95|AtomCodes]),
    !,
    decode(AtomCodes, AtomDecoded).

transform(Atom, Atom) :-
    atom(Atom),
    !.

transform(List1, List2) :-
    List1 =.. [xxx_list|ListTemp],
	!,
	transform_list(ListTemp, List2).

transform(Term, Term) :-
	Term =.. [Term],
	!.

transform(Term1, Term2) :-
	!,
	Term1 =.. List1,
	transform_list(List1, List2),
	Term2 =.. List2.


transform_list([], []).

transform_list([T1|Rest1], [T2|Rest2]) :-
    transform(T1, T2),
    transform_list(Rest1, Rest2).


decode([], '').

decode([D1,D2,D3|Rest], Atom) :-
    format(atom(NA), '~s', [[D1,D2,D3]]),
    atom_number(NA, N),
    atom_chars(A, [N]),
    decode(Rest, AtomRest),
    atom_concat(A, AtomRest, Atom).
