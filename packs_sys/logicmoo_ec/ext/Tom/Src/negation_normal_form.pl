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

/*%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\chapter
[Die Datei {\tt tom\_negation\_normal\_form}]
{Die Datei {\Huge \tt tom\_negation\_normal\_form}}

\Predicate negation_normal_form/2(+Formula, -NormalizedFormula).

The transformation of a formula to its negation normal form. This one
is the top level predicate for the one to follow. The transformation can
be described by a function which we will call {\em nnf}.

\[ nnf(\varphi) = nnf(\varphi,0) \]

\PL*/

negation_normal_form(Formula, NormalizedFormula):-
	negation_normal_form(Formula, 0, NormalizedFormula).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate negation_normal_form/3(+Formula, ?Polarity, -NormalizedFormula).

As the translation assumes a formula in its negation normalform,
we have to transform the formula before translating it. This has been
done in a rather conventional way, analysing the formula structure and
eventually changing the operators to their duals, according to the polarity.
The function {\em nnf} will specify the behaviour.

\PL*/

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\varphi \leftrightarrow \psi,P) = \left\{ \begin{array}{ll} nnf(\varphi \to \psi,0) \wedge nnf(\psi \to \varphi,0) & \mbox{ if \(P
= 0\)}\\ nnf(\varphi \to \psi,1) \vee nnf(\psi \to \varphi, 1)& \mbox{ otherwise} \end{array} \right. \]

The corresponding logical equivalences are:
\begin{eqnarray*}
 \varphi \leftrightarrow \psi & = & (\varphi \to \psi) \wedge (\psi \to \varphi) \\
 \neg(\varphi \leftrightarrow \psi) & = & \neg(\varphi \to \psi) \vee \neg(\psi \to \varphi)
\end{eqnarray*}

\PL*/

negation_normal_form( equivalent(Form1, Form2), 0,
	                             and(NormForm1, NormForm2)):-
	!,
	negation_normal_form(implies(Form1, Form2), 0, NormForm1),
	negation_normal_form(implies(Form2, Form1), 0, NormForm2).

negation_normal_form( equivalent(Form1, Form2), 1,
	                             or(NormForm1, NormForm2)):-
	!,
	negation_normal_form(implies(Form1, Form2), 1, NormForm1),
	negation_normal_form(implies(Form2, Form1), 1, NormForm2).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\varphi \to \psi,P) = \left\{ \begin{array}{ll} nnf(\neg \varphi,0) \vee nnf(\psi,0) & \mbox{ if \(P
= 0\)}\\ nnf(\varphi,0) \wedge nnf(\neg\psi, 0)& \mbox{ otherwise} \end{array} \right. \]

The corresponding logical equivalences are:
\begin{eqnarray*}
 \varphi \to \psi & = & \neg \varphi \vee \psi\\
 \neg(\varphi \to \psi) & = & \varphi \wedge \neg \psi
\end{eqnarray*}

\PL*/

negation_normal_form( implies(Formula1, Formula2), 0,
	                             or(NormalFormula1,NormalFormula2)):-
	!,
	negation_normal_form(not(Formula1), 0, NormalFormula1),
	negation_normal_form(Formula2, 0, NormalFormula2).

negation_normal_form( implies(Formula1, Formula2), 1,
	                             and(NormalFormula1,NormalFormula2)):-
	!,
	negation_normal_form(Formula1, 0, NormalFormula1),
	negation_normal_form(not(Formula2), 0, NormalFormula2).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\varphi \wedge \psi,P) = \left\{ \begin{array}{ll} nnf(\varphi,0) \wedge nnf(\psi,0) & \mbox{ if \(P
= 0\)}\\ nnf(\neg \varphi,0) \vee nnf(\neg\psi, 0)& \mbox{ otherwise} \end{array} \right. \]

The corresponding logical equivalence is one of de Morgan's laws:
\begin{eqnarray*}
 \neg(\varphi \wedge \psi) & = & \neg \varphi \vee \neg \psi
\end{eqnarray*}

\PL*/

negation_normal_form( and(Formula1, Formula2), 0,
	                             and(NormalFormula1, NormalFormula2)):-
	!,
	negation_normal_form(Formula1, 0, NormalFormula1),
	negation_normal_form(Formula2, 0, NormalFormula2).

negation_normal_form( and(Formula1, Formula2), 1,
	                             or(NormalFormula1, NormalFormula2)):-
	!,
	negation_normal_form(not(Formula1), 0, NormalFormula1),
	negation_normal_form(not(Formula2), 0, NormalFormula2).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\varphi \vee \psi,P) = \left\{ \begin{array}{ll} nnf(\varphi,0) \vee nnf(\psi,0) & \mbox{ if \(P
= 0\)}\\ nnf(\neg \varphi,0) \wedge nnf(\neg\psi, 0)& \mbox{ otherwise} \end{array} \right. \]

The corresponding logical equivalence is one of de Morgan's laws:
\begin{eqnarray*}
 \neg(\varphi \vee \psi) & = & \neg \varphi \wedge \neg \psi
\end{eqnarray*}

\PL*/

negation_normal_form( or(Formula1, Formula2), 0,
	                             or(NormalFormula1, NormalFormula2)):-
	!,
	negation_normal_form(Formula1, 0, NormalFormula1),
	negation_normal_form(Formula2, 0, NormalFormula2).

negation_normal_form( or(Formula1, Formula2), 1,
	                             and(NormalFormula1, NormalFormula2)):-
	!,
	negation_normal_form(not(Formula1), 0, NormalFormula1),
	negation_normal_form(not(Formula2), 0, NormalFormula2).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\neg \varphi,P) = \left\{ \begin{array}{ll} nnf(\varphi',0) & \mbox{ if \(\varphi = \neg \varphi'\)}\\ nnf(\varphi,(P + 1) \bmod 2) & \mbox{ otherwise} \end{array} \right. \]

\PL*/

negation_normal_form( not(Formula1), Polarity, NormalFormula):-
	!,
	( Formula1 = not(NewFormula) ->
	    negation_normal_form(NewFormula, Polarity, NormalFormula)
	; NewPolarity is (Polarity + 1) mod 2,
	    negation_normal_form(Formula1, NewPolarity, NormalFormula)
	).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\forall X \: \varphi,P) = \left\{ \begin{array}{ll} \forall X \: nnf(\varphi,0) & \mbox{ if \(P = 0\)}\\ \exists X \: nnf(\varphi,1) & \mbox{ otherwise} \end{array} \right. \]

The corresponding logical equivalence is the duality of the quantifiers:
\[ \neg \forall X \: \varphi = \exists X \: \neg \varphi \]

\PL*/

negation_normal_form(forall(:(Var, Formula1)), 0 ,
	                             forall(:(Var, NewFormula1))):-
	!,
	negation_normal_form(Formula1, 0, NewFormula1).

negation_normal_form(forall(:(Var, Formula1)), 1 ,
	                             exists(:(Var, NewFormula1))):-
	!,
	negation_normal_form(Formula1, 1, NewFormula1).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\exists X \: \varphi,P) = \left\{ \begin{array}{ll} \exists X \: nnf(\varphi,0) & \mbox{ if \(P = 0\)}\\ \forall X \: nnf(\varphi,1) & \mbox{ otherwise} \end{array} \right. \]

Again, the duality of the quantifiers is applied.

\PL*/

negation_normal_form(exists(:(Var, Formula1)), 0 ,
	                             exists(:(Var, NewFormula1))):-
	!,
	negation_normal_form(Formula1, 0, NewFormula1).

negation_normal_form(exists(:(Var, Formula1)), 1 ,
	                             forall(:(Var, NewFormula1))):-
	!,
	negation_normal_form(Formula1, 1, NewFormula1).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\Box_a \: \varphi,P) = \left\{ \begin{array}{ll} \Box_a\: nnf(\varphi,0) & \mbox{ if \(P = 0\)}\\ \Diamond_a\: nnf(\varphi,1) & \mbox{ otherwise} \end{array} \right. \]

The corresponding logical equivalence is the duality of the modal operators:
\[ \neg \Box X \: \varphi = \Diamond X \: \neg \varphi \]

\PL*/

negation_normal_form(box(:(Sort,Formula1)), 0,
	                             box(:(Sort,NewFormula1))):-
	!,
	negation_normal_form(Formula1, 0, NewFormula1).

negation_normal_form(box(:(Sort,Formula1)), 1,
	                             diamond(:(Sort,NewFormula1))):-
	!,
	negation_normal_form(Formula1, 1, NewFormula1).

negation_normal_form(box(Formula1), 0, box(NewFormula1)):-
	!,
	negation_normal_form(Formula1, 0, NewFormula1).

negation_normal_form(box(Formula1), 1, diamond(NewFormula1)):-
	!,
	negation_normal_form(Formula1, 1, NewFormula1).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\[ nnf(\Diamond_a \: \varphi,P) = \left\{ \begin{array}{ll} \Diamond_a \: nnf(\varphi,0) & \mbox{ if \(P = 0\)}\\ \Box_a\: nnf(\varphi,1) & \mbox{ otherwise} \end{array} \right. \]

Again, the duality of the modal operators is used.

\PL*/

negation_normal_form(diamond(:(Sort,Formula1)), 0,
	                             diamond(:(Sort,NewFormula1))):-
	!,
	negation_normal_form(Formula1, 0, NewFormula1).

negation_normal_form(diamond(:(Sort,Formula1)), 1,
	                             box(:(Sort,NewFormula1))):-
	!,
	negation_normal_form(Formula1, 1, NewFormula1).

negation_normal_form(diamond(Formula1), 0, diamond(NewFormula1)):-
	!,
	negation_normal_form(Formula1, 0, NewFormula1).

negation_normal_form(diamond(Formula1), 1, box(NewFormula1)):-
	!,
	negation_normal_form(Formula1, 1, NewFormula1).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If none of the above cases applies, we use the following clause:
\[ nnf(\varphi,P) = \left\{ \begin{array}{ll} \varphi & \mbox{ if \(P = 0\)}\\ \neg \varphi & \mbox{ otherwise} \end{array} \right. \]
This is because apparently we are at the literal level.

\PL*/

negation_normal_form(Formula, Polarity, NormalFormula):-
	( Polarity = 0 ->
	    NormalFormula = Formula
	; NormalFormula = not(Formula)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */

