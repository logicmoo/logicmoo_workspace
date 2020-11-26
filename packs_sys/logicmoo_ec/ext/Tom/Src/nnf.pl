%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nnf.pl,v 1.5 1995/01/27 13:45:38 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1995 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************

:- module_interface(nnf). /*%-------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.5 $}

The filter {\tt nnf} is a filter which produces the negation normal form
of a formula. The usual de Morgan laws are applied, includeing those for
quantifiers and modalities.

\PL*/
:- export nnf/2,
          make_nnf/3.

:- begin_module(nnf).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our identification as a filter for \ProTop.

\PL*/
info(filter,"$Revision: 1.0 $","normal form filter").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We need some definitions from the system.

\PL*/
:-	lib(op_def).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate nnf/2(+Stream, +OutStream).

This is an empty filter. No action is performed except that terms are
transferred from the input stream |Stream| to the output stream
|OutStream|.

\PL*/
nnf(Stream,OutStream) :-
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	;   writeclause(OutStream,Term),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_nnf/3(+Formula, ?Polarity, -NormalizedFormula).

As the translation assumes a formula in its negation normalform,
we have to transform the formula before translating it. This has been
done in a rather conventional way, analysing the formula structure and
eventually changing the operators to their duals, according to the polarity.

\PL*/
make_nnf(Formula,Polarity,NormalFormula):-
	var(Formula),
	!,
	( Polarity = 0 ->
	    NormalFormula = Formula
	; Polarity = 1 ->
	    NormalFormula =.. [not, Formula]
	).

make_nnf( equivalent(Form1, Form2), 0, and(NormForm1, NormForm2)):-
	!,
	make_nnf(implies(Form1, Form2), 0, NormForm1),
	make_nnf(implies(Form2, Form1), 0, NormForm2).

make_nnf( equivalent(Form1, Form2), 1, or(NormForm1, NormForm2)):-
	!,
	make_nnf(implies(Form1, Form2), 1, NormForm1),
	make_nnf(implies(Form2, Form1), 1, NormForm2).

make_nnf( implies(Formula1, Formula2), 0, or(NormalFormula1,NormalFormula2)):-
	!,
	make_nnf(not Formula1, 0, NormalFormula1),
	make_nnf(Formula2, 0, NormalFormula2).

make_nnf( implies(Formula1, Formula2), 1, and(NormalFormula1,NormalFormula2)):-
	!,
	make_nnf(Formula1, 0, NormalFormula1),
	make_nnf(not Formula2, 0, NormalFormula2).

make_nnf( and(Formula1, Formula2), 0, and(NormalFormula1, NormalFormula2)):-
	!,
	make_nnf(Formula1, 0, NormalFormula1),
	make_nnf(Formula2, 0, NormalFormula2).

make_nnf( and(Formula1, Formula2), 1, or(NormalFormula1, NormalFormula2)):-
	!,
	make_nnf(not Formula1, 0, NormalFormula1),
	make_nnf(not Formula2, 0, NormalFormula2).

make_nnf( or(Formula1, Formula2), 0, or(NormalFormula1, NormalFormula2)):-
	!,
	make_nnf(Formula1, 0, NormalFormula1),
	make_nnf(Formula2, 0, NormalFormula2).

make_nnf( or(Formula1, Formula2), 1, and(NormalFormula1, NormalFormula2)):-
	!,
	make_nnf(not Formula1, 0, NormalFormula1),
	make_nnf(not Formula2, 0, NormalFormula2).

make_nnf( not(Formula1), Polarity, NormalFormula):-
	!,
	( Formula1 =.. [not, NewFormula] ->
	    make_nnf(NewFormula, Polarity, NormalFormula)
	; NewPolarity is (Polarity + 1) mod 2,
	    make_nnf(Formula1, NewPolarity, NormalFormula)
	).

make_nnf(forall(:(Var, Formula1)), 0 , forall(:(Var, NewFormula1))):-
	!,
	make_nnf(Formula1, 0, NewFormula1).

make_nnf(forall(:(Var, Formula1)), 1 , exists(:(Var, NewFormula1))):-
	!,
	make_nnf(Formula1, 1, NewFormula1).

make_nnf(exists(:(Var, Formula1)), 0 , exists(:(Var, NewFormula1))):-
	!,
	make_nnf(Formula1, 0, NewFormula1).

make_nnf(exists(:(Var, Formula1)), 1 , forall(:(Var, NewFormula1))):-
	!,
	make_nnf(Formula1, 1, NewFormula1).

make_nnf(box(:(Sort,Formula1)), 0, box(:(Sort,NewFormula1))):-
	!,
	make_nnf(Formula1, 0, NewFormula1).

make_nnf(box(:(Sort,Formula1)), 1, diamond(:(Sort,NewFormula1))):-
	!,
	make_nnf(Formula1, 1, NewFormula1).

make_nnf(box(Formula1), 0, box(NewFormula1)):-
	!,
	make_nnf(Formula1, 0, NewFormula1).

make_nnf(box(Formula1), 1, diamond(NewFormula1)):-
	!,
	make_nnf(Formula1, 1, NewFormula1).

make_nnf(diamond(:(Sort,Formula1)), 0, diamond(:(Sort,NewFormula1))):-
	!,
	make_nnf(Formula1, 0, NewFormula1).

make_nnf(diamond(:(Sort,Formula1)), 1, box(:(Sort,NewFormula1))):-
	!,
	make_nnf(Formula1, 1, NewFormula1).

make_nnf(diamond(Formula1), 0, diamond(NewFormula1)):-
	!,
	make_nnf(Formula1, 0, NewFormula1).

make_nnf(diamond(Formula1), 1, box(NewFormula1)):-
	!,
	make_nnf(Formula1, 1, NewFormula1).

make_nnf(Formula, Polarity, NormalFormula):-
	( Polarity = 0 ->
	    NormalFormula = Formula
	; Polarity = 1 ->
	    NormalFormula =.. [not, Formula]
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */

