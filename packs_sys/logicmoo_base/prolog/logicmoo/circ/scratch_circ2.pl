/**


\title{On Circumscription}
\author{Walter Forkel\\walter.forkel@tu-dresden.de}

\maketitle

\tableofcontents
\clearpage

\begin{abstract}
In this document we want to compare the classic version of circumscription \cite{mccarthy1981circumscription} against the version proposed in
\cite{forkel2017CircRev}.
We choose the murderer world as an example that illustrates the different behaviours.
\end{abstract}

\noindent Definition of predicate circumscription 2.0.  Formalized with the \textit{PIE}
\footnote{\href{http://cs.christophwernhard.com/pie/}{http://cs.christophwernhard.com/pie/}} system.

*/
:- multifile(support_macrokb:ppl_pl/2).
:- dynamic(support_macrokb:ppl_pl/2).

:- use_module(folelim(support_scratch)).
:- use_module(folelim(support_macrokb)).
:- use_module(folelim(logop_fol)).
:- use_module(library(lists)).
:- use_module(circumscription_helpers).

write_extra_preamble :-
	S = '\\usepackage{framed}\n\c
	     \\renewcommand{\\pplIsValid}[1]\n\c
	     {\\begin{framed}\\noindent This formula is valid: $#1$\\end{framed}}\n\c
	     \\renewcommand{\\pplFailedToValidate}[1]\n\c
	     {\\begin{framed}\\noindent \c
	     Failed to validate this formula: $#1$\\end{framed}}\n',
	write(S).
:- ppl_override_options(ppl, [before_begin_document=write_extra_preamble]).



/**
\section{Parallel Predicate Circumscription}
*/

def(xcirc(M, V, F)) :: (F, ~ex2(Args1, (F2, Min))) ::-
	% rename minimized and varying predicates
	mac_sc_rename_free_predicates(M, F, M1, F1),
	mac_sc_rename_free_predicates(V, F1, V1, F2),
	sc_to_scsa(M1, F2, Ma),
	mac_transfer_clauses_strict_subset(Ma, M, Min),
	flatten([M1, V1], Args1).

/**
Circumscription, as defined in \cite{mccarthy1981circumscription}, takes the original formula and adds additionally the Circumscription Axiom. This axiom says that
a model is only a model for the whole expression, iff it satisfies the original formula and additionally there
exists no other model that also satisfies the formula, but has a smaller extension of the minimized predicates.
The axiom forbids all non-minimal models to be models of the whole expression. The second order quantification
is what introduces the non-montonic behavior of circumscription.
*/

/**
\section{Simulations Of Interpretations}

For the definition of the alternative formulation of circumscription we need to
formalize how an interpretation for a formula can be embedded in a parent interpretation and formula.
We first introduce the relativization of a formula to then define the simulation of an interpretation.
*/




def(relat(P, F)) :: F1 ::-
	relativize(P, F, F1).
/**
Relativization of a formula to a specific (domain) predicate $\Phi/1$ is achieved by relativizing all quantifiers in the formula.
Each universial quantifier needs to hold only for the elements of $\Phi$ and similarly for each existential quantifier a valid assignment
needs to be available using only elements of $\Phi$.
*/




def(sim(P, F0)) :: FRes ::-
	mac_expand(F0, F),
	mkAtom(P, x, T),
	F2 = ex(x, T),
	relativize(P, F, F3),
	fun_closures(P, F, F4),
	(F4 = true ->
		FRes = (F2, F3, F4)
	; FRes = (F3, F4)).

/**
An interpretation can be simulated or encoded inside another interpretation.
This is accomplished by using a domain predicate $P$. It needs to be non-empty and the domain needs to be closed under function application.
*/


/**
\section{Generalized Parallel Predicate Circumscription}
*/



circ_axiom(F0, Dom1, Dom2, M, V, CircAxioms) :-
	mac_expand(F0, F),
	mac_free_predicates_as_scsa(F, Ps),
	union(M, V, NonFixed),
	sc_to_scsa(NonFixed, F, NonFixedWithArities),
	subtract(Ps, NonFixedWithArities, FixedWithArities),
	circ_axiom(Dom1, Dom2, FixedWithArities , CircAxioms),
	!.

%% Generate the Circumscription Axioms
circ_axiom(_, _, [], []).
circ_axiom(Dom1, Dom2, [P/N|Ps], [CircAxiom|Rest]) :-
	CircAxiom = all(Args, (P_with_args -> (LeftT <-> RightT))),
	mac_make_fresh_args(N, Args),
	P_with_args =.. [P| Args],

	maplist(mkAtom(Dom1), Args, Left),
	maplist(mkAtom(Dom2), Args, Right),
	list2tuple(Left, LeftT),
	list2tuple(Right, RightT),
	circ_axiom(Dom1, Dom2, Ps, Rest).




def(xcirc2(M, V, F)) :: (genSpaceAxiom('\\Phi'), sim('\\Phi', F), genCircAxiom('\\Phi', '\\psi', M,V,F)).

def(genCircAxiom(DomPred1, DomPred2, M, V, F)) :: ~ex2(Args, (CircAxiomT, sim(DomPred2, F2), Min)) ::-
	% rename minimized and varying predicates
	mac_sc_rename_free_predicates(M, F, M1, F1),
	mac_sc_rename_free_predicates(V, F1, V1, F2),
	sc_to_scsa(M1, F2, Ma),
	% generate CircAxiom
	circ_axiom(F, DomPred1, DomPred2, M, V, CircAxiom),
	list2tuple(CircAxiom, CircAxiomT),
	mac_transfer_clauses_strict_subset(DomPred2, DomPred1, Ma, M, Min),
	flatten([DomPred2, M1, V1], Args).

def(genSpaceAxiom(DomP)) :: F ::-
	spaceAxiom('nat', DomP, F).
/**
The space axioms force the domain to contain infinite elements that are not elements of the given predicate DomP.
This is achieved by encoding the natural numbers greater-than relation on the elements of the space predicate.
The purpose of this is to ensure that there is always enough space to compare the first interpretation against any
other different sized interpretation. Without the space axiom the murderer world example does not work.


In principle one can also let the function symbols vary. Since the PIE system has no
build in support for function symbol (un-)skolemization, this cannot be
implemented in PIE, yet.
*/


spaceAxiom(Dom, Subdom, (FInf, FDisj)) :-
	mkAtom(Dom, x, Dom_A),
	mkAtom(Subdom, x, Subdom_A),
	FDisj = all(x, Dom_A -> ~Subdom_A),
	Dom = 'nat',
	nat_num(FInf).

nat_num(F) :-
	F = (nat(null),
		all(x,(nat(x) -> ex(y,(nat(y), gt(x,y))))),
		all(x,(nat(x) -> ~gt(x, null))),
		all([x,y,z], (nat(x), nat(y), nat(z), gt(x,y), gt(y,z) -> gt(x,z))),
		all([x,y], (nat(x), nat(y), gt(x,y) -> ~gt(y,x)))).



:- ppl_set_options(elim, [style=full]).

/**
\subsection{Tests Of Helper Functions}
In this part we want to do some sanity checks on simulations and relativizations.
The results match exactly their expected definitions, see~\cite{forkel2017CircRev}.

\subsubsection{Tests Of Simulation}
Test with function symbols and quantor:
*/


:- ppl_printtime(ppl_elim(sim(dom, all(x, ((m(f(x)), g(x)) -> ab(x)))))).

%:- ppl_printtime(ppl_valid(last_result <->
%	(ex(x, dom(x)),
%		all([x], (dom(x) -> m(f(x)), g(x) -> ab(x))),
%		all(x, (dom(x) -> dom(f(x))))))).



/**
\bigskip
Test with constant function symbols and existential quantor.
\bigskip
*/
:- ppl_printtime(ppl_elim(sim(dom, ex(x, ((m(f), g(s(x,x))) -> ab(x)))))).

/**
\subsection{Classic Circumscription Examples}
In this section we want to go through some of the examples presented \cite{lifschitz1985computing}. The examples basically test if primitive formulas
yield the intuitively expected results when applying circumscription.

\bigskip
The expanded circumscription formula looks like the following:
*/


:- ppl_printtime(ppl_print(xcirc2([p], [], p(a)))).

/**
If we circumscribe a predicate $p$ in the formula $p(a)$ we expect that
$a$ is the only member of $p$. Note that we have to test this relativized to the domain predicate $\Phi$.
*/
:- ppl_printtime(ppl_elim(xcirc2([p], [], p(a)))).

:- ppl_printtime(ppl_valid(last_result ->
(relat('\\Phi', all(x, (p(x) -> x = a)))))).


/**
If we circumscribe a predicate $p$ in the formula $\neg p(a)$ we expect that
no element is member of $p$:
*/

:- ppl_printtime(ppl_elim(xcirc2([p], [], ~p(a)), [prettify=cnflike])).


:- ppl_printtime(ppl_valid(last_result ->
	(relat('\\Phi', all(x, ~p(x) ))))).



/**
If we circumscribe a predicate $p$ in the formula $p(a) \land p(b)$ we expect that
$a$ and $b$ are the only members of $p$. Note that we do not know wheather $a$ and $b$
refer to different objects.
*/

:- ppl_printtime(ppl_elim(xcirc2([p], [], (p(a) , p(b))))).


:- ppl_printtime(ppl_valid(last_result ->
(relat('\\Phi', (p(a), p(b), all(x, (p(x) -> (x = a; x = b)))))))).



/**
In the case of disjunction we expect that either $a$ or $b$ is are the only members of $p$.
Note that both can be members, if the refer to the same object.
*/
:- ppl_printtime(ppl_elim(xcirc2([p], [], (p(a) ; p(b))))).



:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi',
			   (all(x, p(x) <-> x=a) ;
			     all(x, p(x) <-> x=b)))))).



/**
\subsection{The Murderer World}
See \cite{forkel2017CircRev} for the background story and motivation of the murderer world.
The underlying problem is that the domain size influences circumscription:
There exists the possibility that only one element exists in the domain. In
this case an abnormality can not be avoided. Since entailment is based on all models,
we can not conclude that there are no abnormalities, even though it seems reasonable.
\bigskip

*/

def(murderer_world) ::
	(	all(y, (murderer(y),good(y) -> abnormal(y))),
		ex(x,murderer(x)),
		good(sam)	).



:- ppl_printtime(ppl_elim(xcirc2([abnormal], [good,murderer], murderer_world))).


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (all(x, ~abnormal(x)), ~murderer(sam)))))).


/**
\bigskip
When not using the SpaceAxiom (hence McCarthy's formulation of circumscription), we can not draw the same conclusion anymore:
*/


:- ppl_printtime(ppl_elim(xcirc([abnormal], [good,murderer], murderer_world))).



:- ppl_printtime(ppl_valid(last_result -> (all(x, ~abnormal(x)), ~murderer(sam)) )).


%
%
%testFormula(F) :- F = (all(y, (murderer(y),good(y) -> abnormal(y))), ex(x,murderer(x)), good(sam)).
%
% ?- mac_expand(xcirc2([abnormal], [good,murderer], (all(y, (murderer(y),good(y) -> abnormal(y))), ex(x,murderer(x)), good(sam)))).

/**
\subsection{Fixing The Domain Again}
So far we introduced a space axiom, which forces the domain to be countably infinite.
This allows the interpretations of the circumscribed formula to be minimized even
between varying domains. Sometimes this may not be desired and comparison should just be possible between interpretations with the same domain.
The generalized version of circumscription can be used to simulate a fixed domain in the following way:
*/


def(xcirc2FixedDomain(M, V, F, P)) :: xcirc2(M, V, (all(x, Px), F) ) ::-
	mkAtom(P, x, Px).

def(xcirc2FixedDomain(M, V, F)) :: xcirc2FixedDomain(M, V, F, P) ::-
	logform_gen_predicate(P).


/**
\bigskip
Again if we fix the domain, we can not draw the same conclusion anymore.
*/


:- ppl_printtime(ppl_elim(xcirc2FixedDomain([abnormal], [good,murderer], murderer_world))).


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (all(x, ~abnormal(x)), ~murderer(sam)))))).



/**
\bigskip
If we add the artifially fixed domain to the varying predicates, it again becomes effectless and we again can conclude
that Sam is not a murderer.
*/
:- ppl_printtime(ppl_elim(xcirc2FixedDomain([abnormal], [good,murderer, fixed_dom], murderer_world, fixed_dom))).


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (all(x, ~abnormal(x)), ~murderer(sam)))) )).




/**
Note that in a similar way it is possible to exploit fixed predicates to refix function symbols.

*/

/**
\subsection{Simulating Domain Circumscription}
Is it possible to emulate domain circumscription? We already have domain predicates, why not simply minimize them?
For this we need to omit the function closure for fixed functions.
*/

% FIXME: Do we need the mac_expand here or not?
def(xdomcirc2(V, F)) :: (sim('Dom1', F), ~ex2(Args, (A, ~all(x, ('Dom1'(x) -> 'Dom2'(x))) )))  ::-
	mac_expand(genCircAxiom('Dom1', 'Dom2', [],V,F), ~ex2(Args, A)).

:- assert(ppl_pl(mac_expand(A, B), Options) :-
	format('~@ = ~@ ',
	       [write_form(A, Options), write_form(B, Options)])).

/**
Using this we now force the domain to have just one element:
*/

:- ppl_printtime(ppl_form((xdomcirc2([p], p(a))), [expand=true])).
:- ppl_printtime(ppl_elim(xdomcirc2([p], p(a)))).

:- ppl_printtime(ppl_valid(last_result ->
(relat('Dom1', all(x, (x = a)))))).


/**
In the case of the murderer world this means that Sam is the only existing entity.
*/
:- ppl_printtime(ppl_elim(xdomcirc2([abnormal, good, murderer], murderer_world))).


:- ppl_printtime(ppl_valid(last_result ->
(relat('Dom1', all(x, (x = sam)))))).


/**
\subsection{Prioritized Circumscription With Domain Circumscription}
\paragraph{Domain circumscription} can be activated by adding the domain predicate $\Phi$ to the minimized predicates and if needed adding the required tuples to the given partial order.

In the following we extend the definition of xcirc2 by allowing a partial order as first argument.
*/


def(xcirc2(Ord, M, V, F)) :: (genSpaceAxiom('\\Phi'), sim('\\Phi', F), genCircAxiom('\\Phi', '\\psi', Ord, M,V,F)).

def(genCircAxiom(DomPred1, DomPred2, Ord, M, V, F)) :: ~ex2(Args, (CircAxiomT, sim(DomPred2, F2), Min1, Min2)) ::-
	% rename minimized and varying predicates
	delete(M, DomPred1, M0),
	mac_sc_rename_free_predicates(M0, F, M1, F1),
	mac_sc_rename_free_predicates(V, F1, V1, F2),
	sc_to_scsa(M1, F2, Ma),
	% if also domain circumscription, then add the pair of the domain predicates to the minimization.
	( nth0(Ind, M, DomPred1) ->
		insertAtPos(Ma, DomPred2/1, Ind, Ma1)
	; Ma1 = Ma),
	% generate CircAxiom
	circ_axiom(F, DomPred1, DomPred2, M0, V, CircAxiom),
	list2tuple(CircAxiom, CircAxiomT),
	prio_strictness(DomPred1, DomPred2, M, Ma1, Min1),
	prio_subset_rel(DomPred1, DomPred2, Ord, M, Ma1, Min2),
	flatten([DomPred2, M1, V1], Args).



%:- ppl_printtime(ppl_print((xcirc2([], [p], [], p(a))))).
:- ppl_printtime(ppl_elim(xcirc2([], [p], [], p(a)))).


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', all(x, (p(x) -> x = a)))))).


%:- ppl_printtime(ppl_print((xcirc2([(p, q)], [p,q], [], (p(a), q(a)) )))).
:- ppl_printtime(ppl_elim((xcirc2([(p, q)], [p,q], [], (p(a), q(a)) )))).


/**
\subsubsection{Prioritized Circumscription With Disjunction}
*/

%:- ppl_printtime(ppl_print((xcirc2([], [p], [], (p(a); p(b)) )))).
:- ppl_printtime(ppl_elim((xcirc2([], [p], [], (p(a); p(b)) )))).


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (all(x, (p(x) -> x=a)) ; all(x, (p(x) -> x = b))))))).


/**
Unfortunately, disjunction does not work with priorization so far:
*/
%:- ppl_printtime(ppl_print((xcirc2([(p, q)], [p,q], [], (p(a); q(a)) )))).
:- ppl_printtime(ppl_elim((xcirc2([(p, q)], [p,q], [], (p(a); q(a)) )))).


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (all(x, ~p(x)), all(x, (q(x) -> x = a))))))).


/**
\subsubsection{Prioritized Circumscription With Conjunction In The Murderer World}
This time we want to minimize abnormal/1 again, but additionally we want to have as few as possible murderers and good people.
*/

%:- ppl_printtime(ppl_print(xcirc2([(abnormal, good), (abnormal, murderer)], [abnormal, good, murderer], [], murderer_world))).
:- ppl_printtime(ppl_elim(xcirc2([(abnormal, good), (abnormal, murderer)], [abnormal, good, murderer], [], murderer_world))).
/**
Still there should be no abnormal person, additionally there should be the minimum number of good and murderers in the solution.
*/


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (	all(x, ~abnormal(x)), ~murderer(sam)))))).

:- ppl_printtime(ppl_elim(xcirc2([(abnormal, good), (abnormal, murderer)], [abnormal, good, murderer], [], murderer_world), [printing=false])).

:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (all(x, (good(x) -> x = sam))))))).

:- ppl_printtime(ppl_elim(xcirc2([(abnormal, good), (abnormal, murderer)], [abnormal, good, murderer], [], murderer_world), [printing=false])).


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (ex(x, all(y, (murderer(y) -> x=y)))))))).


/**
On the other hand the domain can still have arbitrary size, causing the assertion below to fail:
*/

:- ppl_printtime(ppl_elim(xcirc2([(abnormal, good), (abnormal, murderer)], [abnormal, good, murderer], [], murderer_world), [printing=false])).

:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (ex([x,y], (~(x=y), all(z, (z=x ; z=y) )))))))).





/**
\subsection{Combining Domain and Predicate Circumscription}

We can combine domain and predicate circumscription by adding the domain predicate $\Phi$
to the minimized predicates and minimizing it with least priority:
*/


:- ppl_printtime(ppl_elim(xcirc2([(p, '\\Phi')], [p, '\\Phi'], [], (p(a)) ))).
/**
The expected solution is that only constant $a$ is contained in $p$ and the domain has size one:
*/


:- ppl_printtime(ppl_valid(last_result -> (relat('\\Phi', (all(x, (x=a, p(x)))))))).



/**
In the case of the murderer world we could state that we want to minimze abnormal and the domain.
Since they are in conflict, the domain should still be forced to be of at least size two.
*/
%:- ppl_printtime(ppl_print(xcirc2([(abnormal, '\\Phi')], [abnormal, '\\Phi'], [good, murderer], murderer_world ))).
:- ppl_printtime(ppl_elim(xcirc2([(abnormal, '\\Phi')], [abnormal, '\\Phi'], [good, murderer], murderer_world ))).
/**
Since we are putting the highest priority on minimizing abnormal, there should still be no abnormalities.
*/


:- ppl_printtime(ppl_valid(last_form_result -> (relat('\\Phi', (	all(x, ~abnormal(x))	))))).	  				% There is no abnormal person




%:- ppl_printtime(ppl_elim(xcirc2([(abnormal, '\\Phi')], [abnormal, '\\Phi'], [good, murderer], murderer_world ))).

/**
When minimizing abnormal and the domain, the expected solution is, that still there is no abnormality and therefore there has to
be at least one other individual than Sam. On the other hand, the domain should be as small as possible.
This implies that there exist exactly two people: Sam and the murderer:
*/



:- ppl_printtime(ppl_valid(last_form_result -> (relat('\\Phi', (	ex([x,y], ( ~(x = y), murderer(x), good(y), all(z, (z = x ; z = y)))))	)))).						% These is exactly one murderer and sam





/**
\subsection{Auxiliary Macros}
*/




def(last_result) :: X ::- last_ppl_result(X).
def(last_form_result) :: X ::- last_ppl_form_result(X).

%:- ppl_set_verbosity(100).
:- ppl_set_source.
:- mac_install.

/**


\bibliographystyle{unsrt}
\bibliography{mybib}


*/
