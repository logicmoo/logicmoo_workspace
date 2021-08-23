/**
\title{Definientia}
\date{Revision: May 9, 2016; Rendered: \today}
\maketitle

\noindent Definability in terms of projections, for computing definientia by
interpolation.  Makes use of scratch\_forgetting. Formalized with the
\href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.
*/

:- ensure_loaded(scratch_forgetting).

/**
\section{Definientia}

The following formula is valid if and only if formula $G$ is definable in
terms of predicates $S$ within formula $F$. Definientia are exactly
the interpolants of its antecedent and consequent.
*/

def(definiens(G,F,S)) ::
proj(S, (F , G)) -> ~proj(S, (F, ~(G))).

/**
The following specification based on literal projection
allows to restrict the polarity of the predicates in $S$:
*/

def(definiens_lit(G,F,S)) ::
projlit(S, (F , G)) -> ~projlit(S1, (F, ~(G))) ::-
 	sc_duals(S, S1).

/**
$\mathit{definiens\_lit\_lemma}$ is an incomplete version of
$\mathit{definiens\_lit}$ that yields formulas which are more efficient to
handle:
*/
def(definiens_lit_lemma(G,F,S)) ::
lemma_projlit(S, (F , G)) -> ~lemma_projlit(S1, (F, ~(G))) ::-
	sc_duals(S, S1).

/**
Definability of a single predicate in terms of a given set of predicates:
*/
def(predicate_definiens(P,F,S)) :: definiens(P_X,F,S) ::-
	mac_get_arity(P, F, N),
	mac_make_fresh_args(N, X),
	mac_make_atom(P, X, P_X).

/**
Definability of a single predicate in terms of all other predicates:
*/
def(predicate_definiens(P,F)) ::
ex2(P, (F , P_X)) -> ~ex2(P, (F, ~P_X)) ::-
	mac_get_arity(P, F, N),
	mac_make_fresh_args(N, X),
	mac_make_atom(P, X, P_X).

/**
\subsection{Definientia: Examples}
*/

def(ex_definiens_1) ::
definiens(p(a), (all(x, (p(x) <-> q(x))), all(x, (p(x) <-> r(x)))), [q]).
		   
:- ppl_printtime(ppl_ipol(ex_definiens_1)).

:- ppl_set_source.
:- mac_install.
