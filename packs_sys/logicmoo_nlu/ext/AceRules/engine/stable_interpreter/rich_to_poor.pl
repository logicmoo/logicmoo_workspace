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


:- module(rich_to_poor, [
		rich_to_poor/2  % +RichRules, -PoorRules
	]).

:- use_module(atom_map).

/** <module> Poor to rich converter

Maps a rule representation using poor format into rich format. The module poor_to_rich.pl
does the reverse transformation

See stable_interpreter.pl for details.

@author Tobias Kuhn
@version 2007-02-09

@see poor_to_rich.pl
@see stable_interpreter.pl
*/


%% rich_to_poor(+RichRules, -PoorRules)
%
% Translates the rules in rich format (RichRules) into poor format (PoorRules).
%
% @see stable_interpreter.pl

rich_to_poor(Rules, SmRules) :-
    transform_list(Rules, SmRules),
    numbervars(SmRules, 0, _).


transform(Var, Var) :-
    var(Var),
    !.

transform(Number, Number) :-
    number(Number),
    !.

transform(RichAtom, PoorAtom) :-
    richatom_pooratom(RichAtom, PoorAtom),
    !.

transform(Atom, AtomEncoded) :-
    atom(Atom),
    format(atom(AtomQ), '~q', [Atom]),
    atom_codes(AtomQ,  [39|_]),
    !,
    encode_atom(Atom, AtomEncoded).

transform(Atom, Atom) :-
    atom(Atom),
    !.

transform((_,H1,B1), ('',H2,B2)) :-
    !,
    transform(H1, H2),
    transform_list(B1, B2).

transform(List1, List2) :-
    List1 = [_|_],
	!,
	transform_list(List1, ListTemp),
	List2 =.. [xxx_list|ListTemp].

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


encode_atom(In, Out) :-
    atom_codes(In, InCodes),
    encode(InCodes, OutCodes),
    atom_codes(Out, [120,120,120,95|OutCodes]).


encode([], []).

encode([Number|RestIn], [D1,D2,D3|RestOut]) :-
    format(atom(NumberAtom), '~w', [Number]),
    atom_codes(NumberAtom, NumberCodes),
    three_digits(NumberCodes, D1, D2, D3),
    encode(RestIn, RestOut).

three_digits([D3], 48, 48, D3).
three_digits([D2,D3], 48, D2, D3).
three_digits([D1,D2,D3], D1, D2, D3).
