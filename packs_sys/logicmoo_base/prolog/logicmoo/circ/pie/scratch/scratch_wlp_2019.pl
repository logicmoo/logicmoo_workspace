/** \title{\textit{PIE} Document: WLP 2019 Examples}
  \date{Revision: August 29, 2019; Rendered: \today}
  \maketitle

  \noindent Examples from the DECLARE/WLP 2019 presentation \textit{PIE --
  Proving, Interpolating and Eliminating on the Basis of First-Order Logic}.
  
*/

:- use_module(folelim(support_scratch)).
:- use_module(folelim(simp_fol)).
:- set_info_verbosity(50).
:- ppl_set_verbosity(7).

:- set_prolog_flag(allow_variable_name_as_functor, true).

:- ppl_override_options([style=full, qstyle=quant, maxpos=50]).

latex_header_hook :-
	writeln('\\usepackage{graphicx}'),
	writeln('\\usepackage{amssymb}').
:- ppl_override_options(ppl, [before_begin_document=latex_header_hook]).

%%%% 
%%%% For use in \prolog
%%%% 
prf(F) :-
	mark_installed_macros(F, F1),
	pp_form(F1, [format=latex,style=full,qstyle=quant]).
prm(X) :-
	pp_form('$macro'(X), [format=latex,style=full]).
prc(X) :-
	pp_form(X, [format=latex,style=full,qstyle=quant,qstyle=quant]).

/** \section{Abduction with the Weakest Sufficient Condition}
*/

def(kb1) ::
(sprinkler_was_on -> wet(grass)),
(rained_last_night -> wet(grass)),
(wet(grass) -> wet(shoes)).

/** A knowledge base.
*/

def(explanation(Kb, Na, Obs)) ::
all2(Na, (Kb -> Obs)).

/** The weakest sufficient condition of observation $\prolog{prc(Obs)}$ on
  the complement of $\prolog{prc(Na)}$ as
  assumables within knowledge base $\prolog{prc(Kb)}$.
  \par\medskip\noindent
  The expression $\prolog{prf(explanation(kb1,[wet],wet(shoes)))}$ expands into:

*/

:- ppl_printtime(ppl_form(explanation(kb1,[wet],wet(shoes)),
			  [expand=true])).


/** \par\medskip\noindent Second-order quantifier elimination
    computes the abductive explanation: \par\medskip
*/

:- ppl_printtime(ppl_elim(explanation(kb1,[wet],wet(shoes)))).


/** \noindent The following is shown by invoking a first-order prover, Prover9
    by default: \par\medskip
*/

:- ppl_printtime(ppl_valid((kb1, rained_last_night
			   -> wet(shoes)))).

/** \section{A Simple Example of Second-Order Quantifier Elimination}
  */

:- ppl_printtime(ppl_elim(ex2(p, (all(x, (q(x) -> p(x))),
				  all(x, (p(x) -> r(x))))))).



/** \section{Predicate Circumscription}
  */

def(circ(P, F)) ::
F, ~ex2(P_p, (F_p, T1, ~T2)) ::-
	mac_rename_free_predicate(F, P, pn, F_p, P_p),
	mac_get_arity(P, F, A),
	mac_transfer_clauses([P/A-n], p, [P_p], T1),
	mac_transfer_clauses([P/A-n], n, [P_p], T2).

/**
   Predicate circumscription of a single predicate. The formula
   $\prolog{prf(circ(p,p(a)))}$, for example, expands into:
*/
  
:- ppl_printtime(ppl_form(circ(p,p(a)), [expand=true])).

/** \par\medskip\noindent Second-order quantifier elimination can be applied
    to to compute that circumscription:\par\medskip
*/

:- ppl_printtime(ppl_elim(circ(p,p(a)))).

/** \par\medskip\noindent Similarly we can compute the circumscription of
    $\prolog{prf(wet)}$ in $\prolog{prf(kb1)}$:\par\medskip
  */

:- ppl_printtime(ppl_elim(circ(wet,kb1), [simp_result=[c6]])).

/**
  \section{Computing Modal Correspondences}  
*/

/** $\Box p \imp p$, known as axiom $M$ or $T$, corresponds to reflexivity of
   the accessibility relation:
*/

:- ppl_printtime(ppl_elim(all2(p, all(v, (all(w, (r(v,w) -> p(w))) -> p(v)))))).


/**\par\medskip\noindent
  $\Box p \imp \Box \Box p$, known as axiom $4$, corresponds to transitivity:

*/

:- ppl_printtime(ppl_elim(all2(p, all(v, (all(w, (r(v,w) -> p(w))) ->
					  all(w, (r(v,w) ->
						  (all(w1, (r(w,w1)
							   -> p(w1))))))))))).


/**
  \section{Craig Interpolation}
*/

/** A simple propositional example for Craig interpolation:

*/

def(ip1) :: 
p, q -> (p ; r).

:- ppl_printtime(ppl_ipol(ip1)).

/** \par\bigskip\noindent A first-order example for Craig interpolation with
    combined universal and existential quantification:

*/

def(ip2) :: 
all(x, p(a,x)), q -> (ex(x, p(x,b)) ; r).

:- ppl_printtime(ppl_ipol(ip2)).

/** \par\bigskip\noindent
    A simple first-order example for Craig interpolation, with
    displayed underlying tableau:

*/

def(ip3) ::
all(x, p(x)), all(x, (p(x) -> q(x))) -> q(c).

:- ppl_printtime(ppl_ipol(ip3, [ip_dotgraph='/tmp/tmp_pie_dotgraph01.png',
				ip_simp_sides=false])).

/**
The clausal tableau proof underlying interpolant extraction can be visualized,
colors representing the sides with respect to interpolation. The color of the
closing marks indicate the side of the connection partner:

\begin{center}
\includegraphics[width=10em]{/tmp/tmp_pie_dotgraph01}
\end{center}
*/



/** \section{Definientia, Definability}
*/

def(definiens(G,F,P)) ::
ex2(P, (F,G)) -> all2(P, (F -> G)).

/**
  The interpolants of the left and right side of this implication are exactly
  the definientia of $G$ in $F$ in terms of all predicates not in $P$.  The
  implication is valid if and only if definability holds.
  \par\bigskip\noindent Her is an example:
*/

def(kb2) ::
all(x, (p(x) -> q(x), s(x))),
all(x, (s(x) -> r(x))),
all(x, (q(x), r(x) -> p(x))).

/** We verify definability of $p$:\smallskip\par
*/

  :- ppl_printtime(ppl_valid(definiens(p(a), kb2, [p,s]))).

/** \par\medskip\noindent Craig interpolation can now be applied to compute a definiens:\smallskip\par
  */

:- ppl_printtime(ppl_ipol(definiens(p(a), kb2, [p,s]))).

/** Since $\f{a}$ does not occur free in $\prolog{prm(kb2)}$, it may be
    considered as representing a variable in a definition of $\f{p}$.
    That is, we can verify:\smallskip
  
  */


:- ppl_printtime(ppl_valid(kb2 -> all(a, (p(a) <-> q(a), r(a))))).


/** \section{Graph Colorability}
  */

def(col2(E)) ::
ex2([r,g],
    ( all(x, (r(x) ; g(x))),
      all([x,y], (E(x,y) -> (~((r(x), r(y))),
 			     ~((g(x), g(y)))))))).

/** 2-colorability as an existential second-order formula.  The predicate
  describing the graph is exported as a parameter. Predicate parameters may be
  instantiated with a constant or a $\lambda$-expression. For example,
  $\prolog{prf(col2(lambda([u,v],((u=1,v=2); (u=2,v=3)))))}$ expands into:
  
*/

:- ppl_printtime(ppl_form(col2(lambda([u,v],((u=1,v=2); (u=2,v=3)))),
			  [expand=true])).

/**  
    \par\bigskip\noindent We now perform some computations by elimination
    based on the inner first-order component of the above specification of
    2-colorability:
*/

def(fo_col2(E)) ::
all(x, (r(x) ; g(x))),
all([x,y], (E(x,y) -> (~((r(x), r(y))),
		       ~((g(x), g(y)))))).

/** We can instantiate the graph with a predicate constant and
    eliminate one of the color predicates:\par\medskip
*/

:- ppl_printtime(ppl_elim(ex2([g],fo_col2(e)))).

/** \par\medskip\noindent We can evaluate 2-colorability for a given graph by
    second-order quantifier elimination. In the current version of
    \textit{PIE} this does not suceed in a single call to elimination, but
    works in two steps with different elimination configurations:
*/

def(ex_rg(F)) :: ex2([r], F1) ::-
 	ppl_elim(ex2([g], F), [elim_options=[pre=[c6]],
			       printing=false,
			       r=F1]).

:- ppl_printtime(ppl_elim(ex_rg(fo_col2(lambda([u,v],((u=1,v=2); (u=2,v=3))))),
			  [elim_options=[pre=[d6]]])).

:- ppl_set_source.
:- mac_install.
