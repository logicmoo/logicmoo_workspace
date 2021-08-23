/**
\title{Problems and Axiom Sets from the TPTP}
\date{Revision: May 10, 2016; Rendered: \today}
\maketitle

\noindent Macros that make problems and axiom sets from the TPTP or in TPTP
FOF or CNF format conveniently available as subformulas.  Formalized with the
\href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.
*/

:- use_module(folelim(support_scratch)).
:- use_module(folelim(tptpio)).

def(tptp(ProblemSpec)) :: F ::-
	tptp_problem(ProblemSpec, [validate=false], Format, T, A),
	( Format = fof ->
	  F = (T <- A)
	; Format = cnf ->
	  matrix_to_form(T, T1),
	  matrix_to_form(A, A1),
	  F = (~T1 <- A1)
	; err('Unsupported TPTP format: ~q', [Format])
	).
/**
$F$ represents the TPTP problem as reverse implication $\mathit{Theorem}
\leftarrow \mathit{Axioms}$. Here are some examples for using the
$\mathit{tptp}$ macro:

\begin{verbatim}
?- mac_expand(tptp('PUZ001+1')).
?- ppl_form(tptp(' PUZ001+1'), [expand=true, style=full]).
?- ppl_valid(tptp('PUZ001+1'), [prover=cm]).
?- ppl_ipol(tptp('PUZ001+1'), [style=full]).
?- ppl_ipol(tptp('PUZ001+1'), [style=full,debug=10]).
\end{verbatim}
*/

def(tptp_axioms(AxiomsSpec)) :: F ::-
	tptp_problem(axioms(AxiomsSpec), [validate=false], Format, T, A),
	( Format = fof ->
	  ( T \== true ->
	    err('TPTP fof axioms with conjecture: ~q', [AxiomsSpec])
	  ; true
	  ),
	  F = A
	; Format = cnf ->
	  ( T \== [] ->
	    err('TPTP cnf axioms with conjecture: ~q', [AxiomsSpec])
	  ; true
	  ),
	  matrix_to_form(A, F)
	).
	
:- ppl_set_source.
:- mac_install.
