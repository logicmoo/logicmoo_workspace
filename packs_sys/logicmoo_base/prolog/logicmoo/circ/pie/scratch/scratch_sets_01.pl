/**
\title{Virtual Classes}
\date{Revision: May 10, 2016; Rendered: \today}  
\maketitle

\noindent Quine's virtual classes and related concepts.  Virtual classes are
straightforwardly expressible as macros.  Formalized with the
\href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.
*/

:- use_module(folelim(support_scratch)).

:- ensure_loaded(scratch_forgetting).
:- ensure_loaded(scratch_definientia).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_default_brief_options([predicates=[p,q,r,s,d,n,a,b,i],
			      constants=[x,y,z,u,v,w,a,b,c,d,s,k]]).

:- register_pplatex_op(400, xfx, inv, '\\in_v').
:- register_pplatex_op(400, xfx, inq, '\\in_q').

%% This is needed for term position in variants setof:
:- register_pplatex_op(1000, xfy, (,), '\\land').
:- register_pplatex_op(1000, xfy, (;), '\\lor').
:- register_pplatex_op(700, xfx, subseteq_v, '\\subseteq_v').
:- register_pplatex_op(700, xfx, subset_v, '\\subset_v').
:- register_pplatex_op(700, xfx, eq_v, '=_v').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
\section{Quine's Virtual Classes and Virtual Relations}

See
\cite[Chap.~5 The Scope of Logic]{quine:1970:philosophy-of-logic},
\cite[Chap.~I]{quine:1969:set-theory-and-its-logic-revised}.
Quine notes there that identity (that is, equality) can be ``simulated''
with subtitutivity axioms for the finite vocabulary of a given formula and
asks whether set theory can be handled analogousy.
Virtual classes and their generalization to virtual relations provide a
translation that applies to set abstraction on the right side of $\in$.

Remark: Quine cites Behmann's book \cite{beh:27} on
\cite[p.~19]{quine:1969:set-theory-and-its-logic-revised}. He notes that
Behmann also introduces operations on classes and relations as mere variant
notation for sentence connectives, as in the virtual theory of classes, but
that there is a crucial difference as Behmanns assumes classes and relations
as \emph{values of quantifiable variables}.
*/

/**
\subsection{Virtual Classes and Relations: Abstraction and Operations}

The following macros apply to both, virtual classes and relations. Relations
are like sets, except that the first argument of $\in$ and the first argument
of set abstraction are lists.  */

def(inv(Y, setof(X,F_X))) :: F_Y ::-
	mac_rename_free_functions(F_X, X, Y, F_Y).

def(inv(X, complem(Y))) :: ~inv(X, Y).
def(inv(X, isect(Y,Z))) :: (inv(X, Y), inv(X,Z)).
def(inv(X, union(Y,Z))) :: (inv(X, Y); inv(X,Z)).
def(inv(X, empty)) :: false.
def(inv(X, full)) :: true.
def(inv(X, unit(Y))) :: X=Y.
def(inv(X, upair(Y,Z))) :: inv(X, union(unit(Y),unit(Z))).

/**
\subsection{Predicates of Virtual Classes}

The following macros apply to virtual \emph{classes}
only.
*/

def(subseteq_v(X, Y)) :: all(Z, inv(Z, X) -> inv(Z, Y)) ::-
	mac_make_fresh_arg(Z).
def(subset_v(X, Y)) :: (subseteq_v(X, Y), ~subseteq_v(Y, X)).
def(eq_v(X,Y)) :: all(Z, inv(Z, X) <-> inv(Z, Y)) ::- 
	mac_make_fresh_arg(Z).

/**
\subsection{Predicates of Virtual Relations}

The following macros apply to virtual relations. The
relation arity has to be supplied as first argument $N$.
*/

def(subseteq_v(N, X, Y)) :: all(Z, inv(Z, X) -> inv(Z, Y)) ::-
	mac_make_fresh_args(N, Z).
def(subset_v(N, X, Y)) :: (subseteq_v(N, X, Y), ~subseteq_v(N, Y, X)).
def(eq_v(N, X,Y)) :: all(Z, inv(Z, X) <-> inv(Z, Y)) ::- 
	mac_make_fresh_arg(N, Z).


/**
\subsection{Operations on Binary Virtual Relations}

The macros in the following group apply to binary relations.
*/

def(inv([X,Y], product_of_classes(A,B))) :: inv(X,A), inv(Y,B).
def(inv([X,Y], converse(R))) :: inv([Y,X], R).
def(inv([X,Z], resultant(Q,R))) :: ex(Y, (inv([X,Y], Q), inv([Y,Z], R))).
def(inv(X, image(R,A))) :: ex(Y, (inv([X,Y], R), inv(Y, A))).
def(inv([X,Y], identity)) :: X = Y.

/**
\subsection{Properties of Binary Virtual Relations}


Further properties of relations can be defined as macros in terms of the
previously defined operations -- see
\cite[p.~22f]{quine:1969:set-theory-and-its-logic-revised}. For example:
*/

def(irreflexive_v(R)) :: subseteq_v(2,R,complem(identity)).


/**
\section{Quine's Set Abstraction in Element Position}

This is discussed in \cite[p.~64ff]{quine:1970:philosophy-of-logic}.
*/

def(inq(setof_q(X,F_X),Y)) ::
ex(Z, (inq(Z,Y), all(X, inq(X,Z) <-> F_X))) ::-
	mac_make_fresh_arg(Z).

def(setopsq) ::
all(y, complem_q(y) = setof_q(x, ~inq(x,y))),
all([y,z], isect_q(y,z) = setof_q(x, (inq(x,y), inq(x,z)))),
all([y,z], union_q(y,z) = setof_q(x, (inq(x,y); inq(x,z)))),
empty_q = setof_q(x, false),
full_q = setof_q(x, true),
all(y, unit_q(y) = setof_q(x, x=y)),
all([y,z], upair_q(y,z) = union_q(unit_q(y),unit_q(z))).

def(Y=setof_q(X,F_X)) ::
all(X, inq(X,Y) <-> F_X).

/**
\bigskip

The expansion of \textit{setopsq} is now:
*/

:- ppl_printtime(ppl_form(setopsq, [expand=true])).

/**
\bibliographystyle{alpha}
\bibliography{bibscratch03}
*/

:- ppl_set_source.
:- mac_install.
