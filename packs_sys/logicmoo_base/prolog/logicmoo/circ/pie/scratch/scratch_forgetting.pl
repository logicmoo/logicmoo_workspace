/**
\title{Forgetting and Projection}
\date{Revision: March 3, 2017; Rendered: \today}
\maketitle

\noindent Definitions of projection, literal forgetting, literal projection,
and approximate versions of the latter two.  Formalized with the
\href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.
*/

:- use_module(folelim(support_scratch)).

:- set_default_brief_options([predicates=[p,q,a,b],
			      constants=[x,y,z]]).

/**
\section{Literal Forgetting}
*/

def(forglit(P-p, F)) :: ex2(Q, (G, all(X, (P_X -> Q_X)))) ::-
	mac_rename_free_predicate(F, P, pn, G, Q),
	mac_get_arity(P, F, N),
	mac_make_args(N, X),
	mac_make_atom(Q, X, Q_X),
	mac_make_atom(P, X, P_X).
def(forglit(P-n, F)) :: ex2(Q, (G, all(X, (Q_X -> P_X)))) ::-
	mac_rename_free_predicate(F, P, pn, G, Q),
	mac_get_arity(P, F, N),
	mac_make_args(N, X),
	mac_make_atom(Q, X, Q_X),
	mac_make_atom(P, X, P_X).
def(forglit(P-pn, F)) :: ex2(P, F).

def(forglit([P|Ps], F)) :: forglit(P, forglit(Ps, F)).
def(forglit([], F)) :: F.

/**
\subsection{Literal Forgetting: Examples}
*/

def(ex_basic, [syntax=brief]) ::
all(x, (ax -> px)), all(x, (px -> bx)).

:- ppl_printtime(ppl_elim(forglit([p-p], ex_basic))).
:- ppl_printtime(ppl_elim(forglit([p-n], ex_basic))).
:- ppl_printtime(ppl_elim(forglit([p-p,p-n], ex_basic))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
\section{Projection}
*/

def(proj(S,F)) :: ex2(S1, F) ::-
	mac_free_predicates(F, S2),
	mac_subtract(S2, S, S1).

def(projlit(S, F)) :: forglit(S1, F) ::-
	sc_to_scse(S, S2),
	mac_free_predicates_as_scse(F, S3),
	mac_subtract(S3, S2, S4),
	scse_add_duals(S4, S5),
	mac_subtract(S5, S2, S6),
	scse_to_scsp(S6, S1).

/**
Here we subtract, add duals and subtract again to avoid \emph{literal}
forgetting induced by occurences in the formula in just a specific
polarity. Semantically we could just subtract as realized in the following
version:

*/

def(projlit_s(S, F)) :: forglit(S1, F) ::-
	sc_to_scse(S, S2),
	mac_free_predicates_as_scse(F, S3),
	mac_subtract(S3, S2, S4),
	scse_to_scsp(S4, S1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
\section{Approximate Version of Literal Forgetting}

Existentially quantifying upon all occurrences with specified polarity yields
a possibly weaker formula than literal forgetting that might be simpler to
process (see application in scratch\_definientia). Also a version of
projection, based on the weakened forgetting is specified.
*/

def(lemma_projlit(S, F)) :: lemma_forglit(S1, F) ::-
	sc_to_scse(S, S2),
	mac_free_predicates_as_scse(F, S3),
	mac_subtract(S3, S2, S4),
	scse_to_scsp(S4, S1).

def(lemma_forglit(P-p, F)) :: ex2(Q, G) ::-
	mac_rename_free_predicate(F, P, p, G, Q).
def(lemma_forglit(P-n, F)) :: ex2(Q, G) ::-
	mac_rename_free_predicate(F, P, n, G, Q).
def(lemma_forglit(P-pn, F)) :: ex2(P, F).

def(lemma_forglit([P|Ps], F)) :: lemma_forglit(P, lemma_forglit(Ps, F)).
def(lemma_forglit([], F)) :: F.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ppl_set_source.
:- mac_install.
