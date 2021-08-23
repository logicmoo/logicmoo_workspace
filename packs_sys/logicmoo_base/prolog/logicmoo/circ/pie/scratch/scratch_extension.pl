/**
\title{Conservative and Definitional Extensions}
\date{Revision: May 10, 2016; Rendered: \today}
\maketitle

\noindent Conservative and definitional extension (see, e.g.,
\cite{hodges:shorter}). Actually, a generalization of definitional extension
is presented here. Formalized with the
\href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.
*/

:- use_module(folelim(support_scratch)).
:- use_module(swilib(err)).

:- ensure_loaded(scratch_forgetting).

/**
\section{Conservative Extension}

Formula $G$ is a \textit{conservative extension} of formula $F$ if and only if
the following biconditional is valid. The right-to-left direction can be
expressed as first-order validity, since second-order quantification is only
in the antecedent and only existential there. The left-to-right direction in
general requires second-order reasoning.  */

def(conservative_extension(F, G)) :: F <-> proj(S, G) ::-
	mac_free_predicates(F, S).

/**
\subsection{Examples for Conservative Extensions}
*/

def(f1) :: (a -> b).

def(ex_ce_1) ::
conservative_extension(f1, (f1, (p <-> a))).

:- ppl_printtime(ppl_valid(ex_ce_1, [elim=true, prover=cm, split_iff=true])).

def(ex_ce_2) ::
conservative_extension(f1, (f1, (p <- a))).

:- ppl_printtime(ppl_valid(ex_ce_2, [elim=true, prover=cm, split_iff=true])).

/**
\subsection{Counterexamples for Conservative Extensions}
*/

def(ex_ce_3) ::
conservative_extension(f1, (f1, (b -> p), (p -> a))).

:- ppl_printtime(ppl_valid(ex_ce_3, [elim=true, prover=cm, split_iff=true])).

def(def_extx(F, G)) :: predicate_definiens(P, (F,G)) ::-
	mac_free_predicates(F, S_F),
	mac_free_predicates(G, S_G),
	mac_subtract(S_G, S_F, S_X),
	singleton_to_member(S_X, P).

singleton_to_member(S, P) :-
	( S = [P] ->
	  true
	; err('No unit set: ~q', [S])
	).

/**
\section{Implicit Definitional Extensions}

We define the following concept: Formula $G$ is an \textit{implicit definitional
extension} of formula $F$ by unary predicate $p$ iff \begin{enumerate}

\item $p$ does not occur in $F$.

\item There exists a formula $Dx$ with no occurrences of $p$ and with no bound
occurrences of $x$ such that $G \entails \forall x\, px \equi Dx$.

\item \label{item-ide-ce} $F \equiv \exists p\, G$.
\end{enumerate}

That property can be verified with just first-order reasoning: $Dx$ is a
definiens of $p$ that can be computed by interpolation. The right-to left
direction of the conservative extension property,
condition~(\ref{item-ide-ce}), can generally be expressed as first-order
validity. Also the left-to-right condition can be expressed as first-order
validity, as shown by the following equivalences:
\[
\begin{array}{r@{\hspace{1em}}l}
& F \entails \exists p\, G[p]\\
\mathrm{ iff } & F \entails \exists p\, G[p] \land \forall x\, px \equi Dx\\
\mathrm{ iff } & F \entails \exists p\, G[D] \land \forall x\, px \equi Dx\\
\mathrm{ iff } & F \entails G[D],
\end{array}
\]
where $G[p] = G$ and $G[D]$ stands for $G$ with all occurrences of $p$
replaced by $Dx$ with $x$ instantiated to the argument of $p$ at the respective occurrence.
The follwing entailment is another equivvalent way to express the above
entailments. It is first-order expressible and might be more convenient
since the replacement of the occurrences of $p$ does not have to be explicitly performed:
\[F \entails \forall p\,  \lnot (\forall x\, px \equi Dx) \lor G[p].\]
*/


def(predicate_definiens_xyz(P, F)) ::
ex2(P, (F , P_X)) -> ~ex2(P, (F, ~P_X)) ::-
	mac_get_arity(P, F, N),
	mac_make_args(N, X),
	mac_make_atom(P, X, P_X).
/**
A version of predicate\_definiens with fixed arguments (as obtained by
mac\_make\_args). It is assumed that these are not used as constants
elsewhere.
*/

/**

\bigskip

The following predicate implements the sketched method for verifying the
implicit definitional extension property by means of first-order
reasoning. The formula arguments are passed to the ppl\_ predicates that
perform macro expansion.  The predicate succeeds iff the property holds
and returns a definiens for the argument predicate as binding of $D$.
*/

ppl_list_predicate(implicit_definitional_extension/4).

implicit_definitional_extension(F, G, P, D) :-
	ppl_valid((ex2(P, G) -> F), [prover=cm, printing=false]),
	last_ppl_result(true),
	ppl_ipol(predicate_definiens_xyz(P, G), [prover=cm, printing=false]),
	last_ppl_result(D),
	mac_get_arity(P, G, N),
	mac_make_args(N, X),
	mac_make_atom(P, X, P_X),
	ppl_valid((F -> all2(p, (all(X, (P_X <-> D)) -> G))),
		  [prover=cm, printing=false]),
	last_ppl_result(true).

def(f2) :: (f1, (p -> a), (a,b -> p)).
def(f3) :: (f1, (p -> a)).

/**

Both formulas $f_2$ and $f_3$ are conservative extensions of $f_1$:

*/

:- ppl_printtime(ppl_valid(conservative_extension(f1, f2),
			   [elim=true, prover=cm, split_iff=true])).

/** */

:- ppl_printtime(ppl_valid(conservative_extension(f1, f3),
			   [elim=true, prover=cm, split_iff=true])).

/**

\bigskip

We can test the predicate \texttt{implicit\_definitional\_extension} with these calls:

\begin{verbatim}
?- implicit_definitional_extension(f1, f2, p, D).  % succeeds
?- implicit_definitional_extension(f1, f3, p, D).  % fails
\end{verbatim}

Only formula $f_2$ but not $f_3$ is an implicit definitional extension of $f_1$.
The following formula is a computed definiens $D$ for
\begin{center}
\texttt{implicit\_definitional\_extension(f1, f2, p, D)}:
\end{center}

*/
:- ppl_printtime((implicit_definitional_extension(f1, f2, p, D), ppl_form(D))).



/**
\bibliographystyle{alpha}
\bibliography{bibscratch03}
*/

:- ppl_set_source.
:- mac_install.
