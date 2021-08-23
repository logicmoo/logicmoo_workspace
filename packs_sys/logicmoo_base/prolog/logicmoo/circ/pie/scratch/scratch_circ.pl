/**
\title{Circumscription}
\date{Revision: February 13, 2018; Rendered: \today}
\maketitle

\noindent Definition of predicate circumscription.  Formalized with the
\href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.

*/

:- use_module(folelim(support_scratch)).

/**
\section{Definition of Predicate Circumscription}

*/

def(xcirc(S, F)) :: (F, ~xraise(S,F)).

/**
A version of parallel predicate circumscription
\cite{lifschitz:circumscription:94}.  The $F$ parameter is the circumscribed
formula.  The $S$ parameter specifies the roles of the predicates in the
circumscription.  It is a list of specifiers of the following form, where $p$
is a predicate and the second component indicates positive, negative, or both
polarities:

\begin{center}
\begin{tabular}{ll}
$p$ is to be minimized & $p \mathsf{\textrm{-}n}$\\
$p$ is to be maximized & $p \mathsf{\textrm{-}p}$\\
$p$ is varying & $p\mathsf{\textrm{-}pn}$\\
$p$ is fixed & $p$ is not mentioned in $S$
\end{tabular}
\end{center}
%
In some cases it might be necessary to specify explicitly also the arity of
the respective predicates, e.g. $p \mathsf{/1\textrm{-}n}$.

The $S$ argument is considered complementary to the scope argument to
$\pplmacro{circ}$ in \cite{cw-projcirc} (like in forgetting instead of
projection).

*/


def(xraise(S, F)) :: ex2(Q, (F1, T1, ~T2)) ::-
	mac_sc_rename_free_predicates(S, F, Q, F1),
	sc_to_scsap(S, F, S2),
	mac_transfer_clauses(S2, p, Q, T1),
	mac_transfer_clauses(S2, n, Q, T2).

/**
The second-order subformula on which $\pplmacro{xcirc}$ is based, Similar
to $\pplmacro{raise}$ \cite{cw-projcirc}, however the scope argument $S$ is
considered complementary (like in forgetting instead of projection).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
\section{Some Examples}

These are examples from \cite{lifschitz:circumscription:94}.

\bigskip

*/


:- ppl_printtime(ppl_elim(xcirc([p-n], p(a)))).

:- ppl_printtime(ppl_elim(xcirc([p-n], ~p(a)), [prettify=cnflike])).

:- ppl_printtime(ppl_elim(xcirc([p-n], (p(a) , p(b))))).

:- ppl_printtime(ppl_elim(xcirc([p-n], (p(a) ; p(b))))).

:- ppl_printtime(ppl_valid(last_result <->
			   (all(x, p(x) <-> x=a) ;
			     all(x, p(x) <-> x=b)))).
			   
/**

\medskip

*/

:- ppl_printtime(ppl_elim(xcirc([p-n], (~p(a) ; p(b))),
			  [prettify=cnflike])).

:- ppl_printtime(ppl_elim(xcirc([p-n], (p(a) ; (p(b), p(c)))))).

:- ppl_printtime(ppl_valid(last_result <->
			   ( all(x, (p(x) <-> x=a))
			   ; ( all(x, (p(x) <-> (x=b ; x=c))),
			       ~(a=b),
			       ~(a=c))))).

/**

\medskip

*/

:- ppl_printtime(ppl_elim(xcirc([p-n], all(x, p(x))))).

:- ppl_printtime(ppl_elim(xcirc([p-n], all(x, (q(x) -> p(x)))))).

:- ppl_printtime(ppl_elim(xcirc([p-n], ex(x, p(x))))).

:- ppl_printtime(ppl_valid(last_result <->
			   ex(x, all(y, (p(y) <-> x=y))))).

/**

\medskip

*/

%%
%% ?- ppl_elim(xcirc([p-n], (p(a) , all(x, (p(x) -> p(f(x))))))).
%% leads to failure - it is not eliminable
%%

:- ppl_printtime(ppl_elim(xcirc([p-n], all(x, p(x,x))))).

% def(lif_12) ::
% ( all([x,y], (q(x,y) -> p(x,y))),
%   all([x,y,z], (p(x,y), p(y,z) -> p(x,z)))
% ).

:- ppl_printtime(ppl_elim(xcirc([p-n, q-pn], all(x, (q(x) -> p(x)))))).

:- ppl_printtime(ppl_valid(last_result <->
			   (all(x, ~p(x)),  all(x, ~q(x))))).

/**

\medskip

*/

def(block_axioms) ::
( all(x, (block(x), ~ab(x) -> ontable(x))),
  ~ontable(b1),
  block(b1), block(b2), ~(b1 = b2) ).

:- ppl_printtime(ppl_elim(xcirc([ab-n,ontable-pn], block_axioms))).

:- ppl_printtime(ppl_valid(last_result <->
			   (block_axioms, all(x, (ab(x) <-> x=b1))))).


/**

\subsection{Auxiliary Macros}

*/
def(last_result) :: X ::- last_ppl_result(X).


/**
\bibliographystyle{alpha}
\bibliography{bibscratch03}
*/

:- ppl_set_source.
:- mac_install.
