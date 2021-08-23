/** \title{\textit{PIE} Example Document}
  \date{Revision: March 28, 2020; Rendered: \today}
  \maketitle
*/

:- use_module(folelim(support_scratch)).
:- set_info_verbosity(50).
:- ppl_set_verbosity(7).
:- ppl_set_source.
:- initialization(mac_install).

latex_header_hook :- writeln('\\usepackage{graphicx}').
:- ppl_override_options(ppl, [before_begin_document=latex_header_hook]).

/** \section{Projection and Definientia} */

def(project(S,F)) :: ex2(S1, F) ::-
        mac_free_predicates(F, S2),
        mac_subtract(S2, S, S1).

/* mac_free_predicates/2 and mac_subtract/2 are utility functions
   for use in macro definitions
*/

def(definiens(G,F,S)) ::
project(S, (F , G)) -> ~project(S, (F, ~(G))).

/** \section{Obtaining a Definiens with Interpolation} */

/**
We specify a background knowledge base:
*/

def(kb_1, [syntax=brief]) ::
all(x, ((qx->px), (px->rx), (rx->qx))).

/**
\bigskip

To obtain a definiens of $\exists x\, \f{p}x$ in terms $\{\f{q}, \f{r}\}$
within $\mathit{kb_1}$ we specify an implication with the $\mathit{definiens}$
macro:
*/

def(ex_1, [syntax=brief]) ::
definiens(ex(x, px), kb_1, [q,r]).

/**
\bigskip

We now invoke a utility predicate that computes and prints for a given valid
implication an interpolant of its antecedent and consequent:
*/

:- ppl_printtime(ppl_ipol(ex_1, [ip_dotgraph='/tmp/tmp_pie_dotgraph01.png'])).

/**
The proof underlying interpolant extraction can be visualized, colors
representing the sides with respect to interpolation. The color of
the closing marks indicate the side of the connection partner:

\begin{center}
\includegraphics[width=15em]{/tmp/tmp_pie_dotgraph01}
\end{center}
*/

/**
Before we leave that example, we take a look at the expansion of
$\mathit{ex}_1$:
*/

:- ppl_printtime(ppl_form(ex_1, [expand=true])).

/**
  And we invoke an external prover (\textit{Prover9}) to validate it:

*/

:- ppl_printtime(ppl_valid(ex_1)).

/*
  An external prover that supports the TPTP format such as E can be
  invoked as follows (requires that TPTP2X is properly installed):

:- ppl_printtime(ppl_valid(ex_1, [prover=tptp(eprover)])).  
*/


/** \section{Obtaining the Strongest Definiens with Elimination} */

/**
The antecedent of the implication in the expansion of $\mathit{definiens}$
specifies the strongest necessary condition of $G$ on $S$ within $F$.  In case
definability holds (that is, the implication is valid), this antecedent denotes
the strongest definiens. In the example it has a first-order equivalent
that can be computed by second-order quantifier elimination.
*/

def(ex_2, [syntax=brief]) ::
project([q,r], (kb_1, ex(x, px))).

:- ppl_printtime(ppl_elim(ex_2)).

