
%---------------------------------------------------------------

:- module(generic_numbers,
	  [make_numbers_in_feat_generic/3,
	   make_numbers_in_atom_and_feat_generic/4,
	   remove_numbers_from_list/2,
	   number_type_subterm/2,
	   coerce_list_elements_to_atoms_if_necessary/2,
	   coerce_list_elements_to_numbers_if_possible/2,
	   coerce_to_number_if_possible/2,
	   coerce_to_atom/2,
	   generic_replacement_for_number_type/3]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

/*

Rather than e.g. learn associations between the atom '5' and the unigram [five], we want to
back off to a generic atom '*number*' and a generic class unigram ['*number*']. Similar
considerations apply to other semantic atoms containing numbers, like times. These
routines are called in both training and decoding.

coerce_to_number_if_possible(+Atom, -NumberTypeTerm)

Atom is a Prolog atom, and NumberTypeTerm is the corresponding number type term. So for
example '5' (atom) gets turned into 5 (number), '2.3' (atom) gets turned into [decimal, 2, 3],
and so on.

---------------------------------------------------------------

coerce_list_elements_to_numbers_if_possible(+InList, -OutList)

Apply coerce_to_number_if_possible/2 to all members of list.

---------------------------------------------------------------

coerce_to_atom(+NumberTypeTerm, -Atom)

Inverse of coerce_to_number_if_possible/2.

---------------------------------------------------------------

coerce_list_elements_to_atoms_if_necessary(+InList, -OutList)

Apply coerce_to_atom/2 to all members of list.

---------------------------------------------------------------

generic_replacement_for_number_type(?NumberType, ?SameOrOther, ?GenericNumber)

Each generic number comes in two flavours. 'same' means we are
matching a number in the feature with the SAME number in the semantic
atom, e.g. 2.15 with [decimal, 2, 15]. 'other' means we are matching
a number in the feature with a DIFFERENT number in the semantic atom,
e.g. 2.15 with [decimal, 2, 50]. So we end up training e.g. an
association (hopefully strong) between '*number*' and '*number*',
and another association (hopefully weak) between '*number*' and '*other_number*'.

NumberType is one of [number, decimal_number, time, range]
SameOrOther is one of [same, other]
GenericNumber is the same/other generic placeholder, e.g. '*number*', '*other_number*'.

---------------------------------------------------------------

number_type_subterm(+Subterm, ?NumberType)

Holds if Subterm is a term representing a number-type object of type NumberType.
NumberType is one of [number, decimal_number, time, range].

---------------------------------------------------------------

remove_numbers_from_list(+ListIn, -ListOut)

ListOut is ListIn with all the elements satisying number_type_subterm removed.

---------------------------------------------------------------

make_numbers_in_atom_and_feat_generic(+Atom, +Feat, -Atom1, -Feat1)

If Atom and Feat each contain at most one number-type-subterm, replace
them with generic placeholders like '*number*' and '*other_number*' in
the intended way, i.e. using '*number*' etc if things match and
'*other_number*' if they don't.

---------------------------------------------------------------

make_numbers_in_feat_generic(+Feat, -Feat1, -Substitution)

If Feat contains at most one number-type-subterm, replace
it with a generic placeholders like '*number*'. Record the
substitution in Substitution in the form SpecificNumber/GenericNumber.
So for example if Feat is 5, Feat1 will be '*number*' and
Substitution will be 5/'*number*'. If no substitution has
taken place, Substitution is no_substitution.

*/

make_numbers_in_feat_generic(Feat, Feat, no_substitution) :-
	no_substitution_feat(Feat),
	!.
make_numbers_in_feat_generic(Feat, Feat1, Substitution) :-
	replace_numbers_in_term_with_vars(Feat, Feat1, []-FeatSubsts),
	FeatSubsts = [Substitution],
	Substitution = SpecificNumber/GenericNumber,
	generic_replacement_for_number_type_subterm(SpecificNumber, same, GenericNumber),
	!.
make_numbers_in_feat_generic(Feat, Feat, no_substitution) :-
	!.

%---------------------------------------------------------------

make_numbers_in_atom_and_feat_generic(Atom, Feat, Atom1, Feat1) :-
	replace_numbers_in_term_with_vars(Atom, Atom1, []-AtomSubsts),
	(   no_substitution_feat(Feat) ->
	    Feat1 = Feat,
	    FeatSubsts = [] ;
	    replace_numbers_in_term_with_vars(Feat, Feat1, []-FeatSubsts)
	),	
	make_numbers_in_atom_and_feat_generic1(AtomSubsts, FeatSubsts, Atom, Feat).

% There are no numbers in either Atom or Feats: nothing to do.
make_numbers_in_atom_and_feat_generic1(AtomSubsts, FeatSubsts, _Atom, _Feat) :-
	AtomSubsts = [],
	FeatSubsts = [],
	!.
% Feats contains one number, Atom doesn't contain any: replace the number in Feats with *number*
make_numbers_in_atom_and_feat_generic1(AtomSubsts, FeatSubsts, _Atom, _Feat) :-
	AtomSubsts = [],
	FeatSubsts = [SpecificNumber/GenericNumber],
	!,
	generic_replacement_for_number_type_subterm(SpecificNumber, same, GenericNumber).
% Atom contains one number, Feats doesn't contain any: replace the number in Atom with *other_number*
make_numbers_in_atom_and_feat_generic1(AtomSubsts, FeatSubsts, _Atom, _Feat) :-
	AtomSubsts = [SpecificNumber/GenericNumber],
	FeatSubsts = [],
	!,
	generic_replacement_for_number_type_subterm(SpecificNumber, other, GenericNumber).
% Atom and Feats each contain the same number: replace it in both with *number*
make_numbers_in_atom_and_feat_generic1(AtomSubsts, FeatSubsts, _Atom, _Feat) :-
	AtomSubsts = [SpecificNumber/GenericNumber],
	FeatSubsts = [SpecificNumber/GenericNumber],
	!,
	generic_replacement_for_number_type_subterm(SpecificNumber, same, GenericNumber).
% Atom and Feats contain different numbers: replace it with *number* in Feats and *other_number* in Atom
make_numbers_in_atom_and_feat_generic1(AtomSubsts, FeatSubsts, _Atom, _Feat) :-
	AtomSubsts = [SpecificNumberAtom/GenericNumberAtom],
	FeatSubsts = [SpecificNumberFeat/GenericNumberFeat],
	dif(SpecificNumberAtom, SpecificNumberFeat),
	!,
	generic_replacement_for_number_type_subterm(SpecificNumberFeat, same, GenericNumberFeat),
	generic_replacement_for_number_type_subterm(SpecificNumberAtom, other, GenericNumberAtom).
make_numbers_in_atom_and_feat_generic1(AtomSubsts, FeatSubsts, _Atom, _Feat) :-
	%format('~N*** Warning: more than one number in feats ~q, generic substitutions will be incorrect.~n', [Feat]),
	fill_in_generic_replacement_for_number_type_subterm_in_list(AtomSubsts, same),
	fill_in_generic_replacement_for_number_type_subterm_in_list(FeatSubsts, other).

%---------------------------------------------------------------

fill_in_generic_replacement_for_number_type_subterm_in_list([], _SameOrOther) :-
	!.
fill_in_generic_replacement_for_number_type_subterm_in_list([SpecificNumber/GenericNumber | R], SameOrOther) :-
	generic_replacement_for_number_type_subterm(SpecificNumber, SameOrOther, GenericNumber),
	fill_in_generic_replacement_for_number_type_subterm_in_list(R, SameOrOther),
	!.
fill_in_generic_replacement_for_number_type_subterm_in_list(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [fill_in_generic_replacement_for_number_type_subterm_in_list(X, Y)]),
	fail.

%---------------------------------------------------------------

remove_numbers_from_list([], []).
remove_numbers_from_list([F | R], R1) :-
	number_type_subterm(F, _Type),
	!,
	remove_numbers_from_list(R, R1).
remove_numbers_from_list([F | R], [F | R1]) :-
	!,
	remove_numbers_from_list(R, R1).

%---------------------------------------------------------------

replace_numbers_in_term_with_vars(N, Var, In-Out) :-
	number_type_subterm(N, _Type),
	(   member(N/Var, In) ->
	    Out = In ;
	    Out = [N/Var | In]
	),
	!.
replace_numbers_in_term_with_vars(Var, Var, In-In) :-
	var(Var),
	!.
replace_numbers_in_term_with_vars(Atom, Atom, In-In) :-
	atom(Atom),
	!.
replace_numbers_in_term_with_vars(T, T1, In-Out) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	replace_numbers_in_term_with_vars_args(N, T, T1, In-Out).

replace_numbers_in_term_with_vars_args(0, _T, _T1, In-In).
replace_numbers_in_term_with_vars_args(I, T, T1, In-Out) :-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	replace_numbers_in_term_with_vars(Arg, Arg1, In-Next),
	I1 is I - 1,
	!,
	replace_numbers_in_term_with_vars_args(I1, T, T1, Next-Out).

%---------------------------------------------------------------

generic_replacement_for_number_type_subterm(N, SameOrOther, GenericN) :-
	number_type_subterm(N, Type),
	generic_replacement_for_number_type(Type, SameOrOther, GenericN),
	!.

%---------------------------------------------------------------

generic_replacement_for_number_type(number, same, '*number*').
generic_replacement_for_number_type(number, other, '*other_number*').
generic_replacement_for_number_type(decimal_number, same, '*decimal_number*').
generic_replacement_for_number_type(decimal_number, other, '*other_decimal_number*').
generic_replacement_for_number_type(time, same, '*time*').
generic_replacement_for_number_type(time, other, '*other_time*').
generic_replacement_for_number_type(range, same, '*range*').
generic_replacement_for_number_type(range, other, '*other_range*').
generic_replacement_for_number_type(row, same, '*row*').
generic_replacement_for_number_type(row, other, '*other_row*').
generic_replacement_for_number_type(column, same, '*column*').
generic_replacement_for_number_type(column, other, '*other_column*').
%generic_replacement_for_number_type(row_or_column, same, '*row_or_column*').
%generic_replacement_for_number_type(row_or_column, other, '*other_row_or_column*').
generic_replacement_for_number_type(typed_object(Type), same, '*typed_object*'(Type)).
generic_replacement_for_number_type(typed_object(Type), other, '*other_typed_object*'(Type)).

%------------------------------------------------------------------------------------
	
coerce_list_elements_to_numbers_if_possible([], []).
coerce_list_elements_to_numbers_if_possible([F | R], [F1 | R1]) :-
	coerce_to_number_if_possible(F, F1),
	coerce_list_elements_to_numbers_if_possible(R, R1).

%------------------------------------------------------------------------------------

coerce_list_elements_to_atoms_if_necessary(Annotations, AtomList) :-
	coerce_list_elements_to_atoms_if_necessary1(Annotations, AtomList),
	!.
coerce_list_elements_to_atoms_if_necessary(Annotations, AtomList) :-
	format('~N*** Error: bad call: ~w~n', [coerce_list_elements_to_atoms_if_necessary(Annotations, AtomList)]),
	fail.

coerce_list_elements_to_atoms_if_necessary1([], []).
coerce_list_elements_to_atoms_if_necessary1([F | R], [F1 | R1]) :-
	coerce_to_atom(F, F1),
	!,
	coerce_list_elements_to_atoms_if_necessary1(R, R1).

%---------------------------------------------------------------

coerce_to_number_if_possible(TimeAtom, Time) :-
	coerce_to_time(TimeAtom, Time),
	!.
coerce_to_number_if_possible(NumAtom, Num) :-
	coerce_to_decimal_number(NumAtom, Num),
	!.
coerce_to_number_if_possible(NumAtom, Num) :-
	coerce_to_number(NumAtom, Num),
	!.
coerce_to_number_if_possible(NumAtom, Num) :-
	coerce_to_range(NumAtom, Num),
	!.
coerce_to_number_if_possible(NumAtom, Num) :-
	coerce_to_row(NumAtom, Num),
	!.
coerce_to_number_if_possible(NumAtom, Num) :-
	coerce_to_column(NumAtom, Num),
	!.
%coerce_to_number_if_possible(NumAtom, Num) :-
%	coerce_to_row_and_column(NumAtom, Num),
%	!.
coerce_to_number_if_possible(NumAtom, Num) :-
	coerce_to_typed_object(NumAtom, Num),
	!.
coerce_to_number_if_possible(Other, Other).

%--------------------------------------------------------------------------------------------

coerce_to_time(TimeAtom, Time) :-
	split_atom_into_words(TimeAtom, 0':, TimeComponents),
	TimeComponents = [HoursAtom, MinsAtom],
	coerce_to_number(HoursAtom, H),
	coerce_to_number(MinsAtom, M),
	Time = time(H, M),
	!.

coerce_to_decimal_number(NumAtom, Num) :-
	split_atom_into_words(NumAtom, 0'., NumComponents),
	NumComponents = [NAtom1, NAtom2],
	coerce_to_number(NAtom1, N1),
	coerce_to_number(NAtom2, N2),
	Num = [decimal, N1, N2],
	!.

coerce_to_number(NumAtom, Num) :-
	atom(NumAtom),
	atom_chars(NumAtom, Chars),
	number_chars(Num, Chars),
	!.

coerce_to_range(NumAtom, Num) :-
	split_atom_into_words(NumAtom, 0'-, NumComponents),
	NumComponents = [NAtom1, NAtom2],
	( coerce_to_decimal_number(NAtom1, N1) ; coerce_to_number(NAtom1, N1) ),
	( coerce_to_decimal_number(NAtom2, N2) ; coerce_to_number(NAtom2, N2) ),
	Num = [range, N1, N2],
	!.

coerce_to_row(NumAtom, Num) :-
	split_atom_into_words(NumAtom, 0'-, NumComponents),
	NumComponents = [row, NAtom1],
	coerce_to_number(NAtom1, N1),
	Num = row(N1),
	!.

coerce_to_column(NumAtom, Num) :-
	split_atom_into_words(NumAtom, 0'-, NumComponents),
	NumComponents = [column, NAtom1],
	coerce_to_number(NAtom1, N1),
	Num = column(N1),
	!.

/*
coerce_to_row_and_column(NumAtom, Num) :-
	split_atom_into_words(NumAtom, 0'-, NumComponents),
	NumComponents = [row_and_column, NAtom1, NAtom2],
	coerce_to_number(NAtom1, N1),
	coerce_to_number(NAtom2, N2),
	Num = row_and_column(N1, N2),
	!.
*/

coerce_to_typed_object(NumAtom, Num) :-
	split_atom_into_words(NumAtom, 0'/, NumComponents),
	NumComponents = [NAtom1, NAtom2],
	Num = typed_object(NAtom1, NAtom2),
	!.

%--------------------------------------------------------------------------------------------

coerce_to_atom(time(H, M), TimeAtom) :-
	coerce_to_two_digit_number(H, H1),
	coerce_to_two_digit_number(M, M1),
	append_atoms([H1, M1], 0':, TimeAtom),
	!.
coerce_to_atom([decimal, N1, N2], NumAtom) :-
	append_atoms([N1, N2], 0'., NumAtom),
	!.
coerce_to_atom([range, N1, N2], Atom) :-
	coerce_to_atom(N1, N1Atom),
	coerce_to_atom(N2, N2Atom),
	append_atoms([N1Atom, N2Atom], 0'-, Atom),
	!.
coerce_to_atom(typed_object(NAtom1, NAtom2), Atom) :-
	append_atoms([NAtom1, NAtom2], 0'/, Atom),
	!.
coerce_to_atom(row(N), Atom) :-
	coerce_to_atom(N, NAtom),
	append_atoms([row, NAtom], 0'-, Atom),
	!.
coerce_to_atom(column(N), Atom) :-
	coerce_to_atom(N, NAtom),
	append_atoms([column, NAtom], 0'-, Atom),
	!.
/*
coerce_to_atom(row_and_column(N1, N2), Atom) :-
	coerce_to_atom(N1, NAtom1),
	coerce_to_atom(N2, NAtom2),
	append_atoms([row_and_column, NAtom1, NAtom2], 0'-, Atom),
	!.
*/
coerce_to_atom(Number, Atom) :-
	number(Number),
	number_chars(Number, Chars),
	atom_chars(Atom, Chars),
	!.
coerce_to_atom(Atom, Atom) :-
	atom(Atom),
	!.
coerce_to_atom(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [coerce_to_atom(X, Y)]),
	fail.

%--------------------------------------------------------------------------------------------

coerce_to_two_digit_number(N, Atom) :-
	integer(N),
	10 =< N, N =< 99,
	number_chars(N, Chars),
	atom_chars(Atom, Chars),
	!.
coerce_to_two_digit_number(N, Atom) :-
	integer(N),
	0 =< N, N =< 9,
	number_chars(N, Chars),
	atom_chars(Atom, [0'0 | Chars]),
	!.
coerce_to_two_digit_number(N, Atom) :-
	format('~N*** Error: bad call: ~w~n', [coerce_to_two_digit_number(N, Atom)]),
	fail.

%---------------------------------------------------------------

number_type_subterm(N, number) :-
	number(N),
	!.
number_type_subterm(N, time) :-
	nonvar(N),
	N = time(H, M),
	number(H),
	number(M),
	!.
number_type_subterm(N, decimal_number) :-
	nonvar(N),
	N = [decimal, N1, N2],
	number(N1),
	number(N2),
	!.
number_type_subterm(N, row) :-
	nonvar(N),
	N = row(N1),
	number(N1),
	!.
number_type_subterm(N, column) :-
	nonvar(N),
	N = column(N1),
	number(N1),
	!.
number_type_subterm(N, range) :-
	nonvar(N),
	N = [range, Start, End],
	(  number_type_subterm(Start, number) ; number_type_subterm(Start, decimal_number) ),
	(  number_type_subterm(End, number) ; number_type_subterm(End, decimal_number) ),
	!.
number_type_subterm(N, typed_object(NAtom1)) :-
	nonvar(N),
	N = typed_object(NAtom1, NAtom2),
	atom(NAtom1),
	atom(NAtom2).

%---------------------------------------------------------------

no_substitution_feat(confidence(_, _)).
