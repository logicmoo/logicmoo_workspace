/**
\title{Access Predicates -- Examples from the Literature}
\date{Revision: March 28, 2019; Rendered: \today}
\maketitle

\noindent Some examples of solutions to view-based query processing and query
optimization tasks from the literature. Makes use of scratch\_forgetting and
scratch\_definientia. Formalized with the
\href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.

\tableofcontents
*/

:- use_module(folelim(support_scratch)).

:- ensure_loaded(scratch_forgetting).
:- ensure_loaded(scratch_definientia).

% :- set_info_verbosity(2).
:- set_info_verbosity(50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_default_brief_options([predicates=[p,q,r,s,d,n,a,b,i],
			      constants=[x,y,z,u,v,w,a,b,c,d,s,k]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
\section{Examples from Benedikt, ten Cate and Tsamoura: Generating Low-Cost
Plans from Proofs}

These examples stem from \cite{benedikt:etal:2014:generating}.  The numbering
of examples refers to that paper.  The representation here is different from
the paper -- (it seem new and is called here ``SV-modeling''). Also the
methods and solutions are different.
*/

/**
\subsection{Example 1}

Note: $\f{ia}$ in the background formula represents that the name (``smith'' in
the paper) is given. However $\f{ia}$ is actually not used to compute the
interpolant.
*/

def(exbct_1_a) ::
all([n,o,e], (i(e) -> (profinfo_a(n,o,e) <-> profinfo(n,o,e)))),
all([n,o,e], (profinfo(n,o,e) -> i(n),i(o),i(e))).

def(exbct_1_b) ::
all([n,e], (udirect(n,e) -> i(n), i(e))),
all([n,o,e], (profinfo(n,o,e) -> udirect(n,e))).

def(exbtc_1_c) ::
definiens(profinfo(a,b,c),
	  ( exbct_1_a,
	    exbct_1_b,
	    i(a)),
	  [profinfo_a,udirect]).

:- ppl_printtime(ppl_ipol(exbtc_1_c, [style=full])).

/**
\noindent In the following formula the query is expressed more accurately with
existential midle argument, as described in Example~3 of
\cite{benedikt:etal:2014:generating}.
*/

def(exbtc_1_d) ::
definiens(ex(o, profinfo(a,o,c)),
	  ( exbct_1_a,
	    exbct_1_b,
	    i(a)), 
	  [profinfo_a,udirect]).

:- ppl_printtime(ppl_ipol(exbtc_1_d, [style=full])).


/**
\subsection{Variant of Example 1}

This is the variant of Example~1 from \cite[p.~101 Left
Column]{benedikt:etal:2014:generating}.
It leads to different interpolants, obtained with the enum\_ips option.
Here the first 3 interpolants are shown.
*/

def(exbtc_1_e) ::
definiens(profinfo(a,b,c),
	  ( exbct_1_a,
	    all([n,e], (udirect_1(n,e) -> i(n), i(e))),
	    all([n,o,e], (profinfo(n,o,e) -> udirect_1(n,e))),
	    all([n,e], (udirect_2(n,e) -> i(n), i(e))),
	    all([n,o,e], (profinfo(n,o,e) -> udirect_2(n,e))),
	    i(a) ),
	  [profinfo_a,udirect_1,udirect_2]).

:- ppl_printtime(ppl_ipol(exbtc_1_e, [style=full,enum_ips=3,add_cm_options=[]-[r8(_)]])).

%%%% 
%%%% Note: Option r8(_) [a kind of antilemmas] must be deleted, because it
%%%% prevents enumeration of the intended other proofs, leading to
%%%% nontermination.
%%%% 

% ppl_ipol(exbtc_1_e, [style=full,enum_ips=true]).

/**
\subsection{Example~4}

Note that we have the id as last argument of profinfo, following the natural
language description of Example~1 of the paper. In the formal version, of
Example~4 in the paper the id is the first argument.
The $i(a)$ has been dropped here.
*/

def(exbtc_4_a) ::
definiens(ex([c], profinfo(a,o,c)),
	  ( exbct_1_a,
	    exbct_1_b ),
	  [profinfo_a,udirect]).

:- ppl_printtime(ppl_ipol(exbtc_4_a, [style=full])).

/**
\subsection{Example~5}

Note that we have the id as last argument of profinfo, following the natural
language description of Example~1 of the paper. In the formal version, of
Examples~4 and 5 in the paper the id is the first argument.

Here we need $i(b)$ because, as described in the paper, the access to profinfo
requires all arguments bound, where only the first and last can be bound at
all by udirect. Perhaps this is a bug in the paper.

The point of the example seems that the method of the paper involves all three
udirect accesses (in some order), but it is hard to see why this is useful,
when the query could be answered by accessing just one of them.
*/

def(exbtc_5_a) ::
definiens(ex(c, profinfo(a,b,c)),
	  ( all([n,o,e],
		(i(n), i(o), i(e) ->
		  (profinfo_a(n,o,e) <-> profinfo(n,o,e)))),
	    all([n,o,e], (profinfo(n,o,e) -> i(n),i(o),i(e))),
	    exbct_1_a,
	    all([n,e], (udirect_1(n,e) -> i(n), i(e))),
	    all([n,o,e], (profinfo(n,o,e) -> udirect_1(n,e))),
	    all([n,e], (udirect_2(n,e) -> i(n), i(e))),
	    all([n,o,e], (profinfo(n,o,e) -> udirect_2(n,e))),
	    all([n,e], (udirect_3(n,e) -> i(n), i(e))),
	    all([n,o,e], (profinfo(n,o,e) -> udirect_3(n,e))),
	    i(b) ),
	  [profinfo_a,udirect_1,udirect_2,udirect_3]).

:- ppl_printtime(ppl_ipol(exbtc_5_a, [style=full,enum_ips=2,add_cm_options=[]-[r8(_)]])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
\subsection{Example~2}

There seems a bug in the paper: Actually a referential constraint from direct2
into direct1 w.r.t. $n,a$ is required here. The paper suggests the reverse direction.
*/

def(exbtc_2_schema) ::
all([n,a,u], (i(n), i(u) -> (direct1_a(n,a,u) <-> direct1(n,a,u)))),
all([n,a,u], (direct1(n, a, u) -> i(n), i(a), i(u))),
all([n,a,u], (direct1(n, a, u) -> ids(u))),
all([u], (ids(u) -> i(u))),

all([n,a,p], (i(n), i(a) -> (direct2_a(n,a,p) <-> direct2(n,a,p)))),
all([n,a,p], (direct2(n, a, p) -> i(n), i(a), i(p))),
all([n,a,p], (direct2(n, a, p) -> names(n))),
all([n], (names(n) -> i(n))),

all([n,a,p], (direct2(n,a,p) -> ex(u, direct1(n,a,u)))).

def(exbtc_2_a) ::
definiens(ex([n,a], direct2(n,a,p)),
	  (exbtc_2_schema),
	  [direct1_a, direct2_a, ids, names]).

:- ppl_printtime(ppl_ipol(exbtc_2_a, [style=full])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
\section{Examples from Toman and Wedell: Fundamentals of
Physical Design and Query Compilation}

These examples are from \cite[Chapters~3 and~5]{toman:wedell:book}.
*/


/**
\subsection{Example~5.14}
*/

def(extw_514_a) ::
all([x,y], (v1(x,y) <-> ex([u,w], (r(u,x),r(u,w),r(w,y))))),
all([x,y], (v2(x,y) <-> ex([u,w], (r(x,u),r(u,w),r(w,y))))),
all([x,y], (v3(x,y) <-> ex([u], (r(x,u), r(u,y))))).

def(extw_514_b) ::
definiens(ex([u,v,w], (r(u,x),r(u,w),r(w,v),r(v,y))),
	  extw_514_a,
	  [v1,v2,v3]).


%% this gets much slower with the ordp option:

:- ppl_printtime(ppl_ipol(extw_514_b, [style=full])).

/**
Notes: The book shows this solution, but also another, longer formula which is
then used as basis for plan generation. The longer formula seems not easily to
obtain as alternative interpolant with CM prover.
*/

def(extw_514_altsol) ::
ex([u,v],(v1(x, u),v3(v, u),v2(v, y),all([w],(~v3(w, u);v2(w, y))))).

def(extw_514_query) ::
ex([u,v,w], (r(u,x),r(u,w),r(w,v),r(v,y))).

def(extw_514_check_altsol_1) ::
extw_514_a -> (extw_514_altsol <- extw_514_query).

def(extw_514_check_altsol_2) ::
extw_514_a -> (extw_514_altsol -> extw_514_query).

/**
The next formula uses literal forgetting for $v3$.  This seems hard for the
CM prover (12 sec, 3 of them in the last depth 7). When literal forgetting is
used to restrict polarities for all three access paths, i.e.,
$[v1-p,v2-p,v3-n]$, the CM prover does not succeed in a few minutes.
*/

def(extw_514_c) ::
definiens_lit(ex([u,v,w], (r(u,x),r(u,w),r(w,v),r(v,y))),
	  extw_514_a,
	  [v1-pn,v2-pn,v3-n]).

def(extw_514_d) ::
definiens_lit_lemma(ex([u,v,w], (r(u,x),r(u,w),r(w,v),r(v,y))),
	  extw_514_a,
	  [v1-pn,v2-pn,v3-n]).

def(extw_514_e) ::
definiens_lit_lemma(ex([u,v,w], (r(u,x),r(u,w),r(w,v),r(v,y))),
	  extw_514_a,
	  [v1-p,v2-p,v3-n]).

% :- ppl_printtime(ppl_ipol(extw_514_c, [style=full])).

/**
\subsection{Examples~3.2, 3.4, 3.5}

These examples are also discussed in the book in Examples~5.3 and 5.4.

The access paths have arguments \textit{Salary}, \textit{Number},
\textit{Name}. The employee/3 relation has these arguments in the ordering
\textit{Number}, \textit{Name} \textit{Salary}.

We give the access paths here different number suffixes than in the book to
utilize that lexically smaller predicates are preferred by the interpolant
computation (the ordp option of the CM prover) and thus returned in earlier
solutions.

NOTE: Currently the ordp option prefers solution with lexically smaller
predicates, however for the price of possibly choosing a larger clause, which
might introduce redundancies. The ordp option is not required if access
patterns are disjoint.
*/

def(extw_3_schema) ::
all([x,y,z], emp_array_3(z,x,y) <-> employee(x,y,z)),
all([x,y,z], i(z) -> (emp_array_2(z,x,y) <-> employee(x,y,z))),
all([x,y,z], (i(z), i(x) -> (emp_array_1(z,x,y) <-> employee(x,y,z)))),
all([x,y,z], employee(x,y,z) -> (i(x), i(y), i(z))).

def(extw_32) ::
definiens( employee(x,y,z),
	   extw_3_schema,
	   [emp_array_1, emp_array_2, emp_array_3] ).

def(extw_34) ::
definiens( employee(x,y,z),
	   ( extw_3_schema, i(z) ),
	   [emp_array_1, emp_array_2, emp_array_3] ).

def(extw_35) ::
definiens( employee(x,y,z),
	   ( extw_3_schema, i(x), i(z) ),
	   [emp_array_1, emp_array_2, emp_array_3] ).


:- ppl_printtime(ppl_ipol(extw_32, [style=full, add_cm_options=[ordp]])).
:- ppl_printtime(ppl_ipol(extw_34, [style=full, add_cm_options=[ordp]])).
:- ppl_printtime(ppl_ipol(extw_35, [style=full, add_cm_options=[ordp]])).


/**
\subsection{Alternate Modeling, Similar to ``Option 3''}

This is just similar to ``Option~3'' in the book, but we use employee numbers
directly as IDs of employees. We map the schema to employee/3:
*/

def(extw_3_o3_schema_addition) ::
all(x, emp(x) <-> ex([y,z], employee(x,y,z))),
all([x,y], name(x,y) <-> ex(z, employee(x,y,z))),
all([x,y], salary(x,y) <-> ex(z, employee(x,z,y))).

%%%% 
%%%% def(tst1) ::
%%%% extw_3_o3a_schema_addition ->
%%%% all([x,y,z], (emp(x), name(x,y), salary(x,z) -> employee(x,y,z))).
%%%% 

def(extw_o3_32) ::
definiens( (emp(x), name(x,y), salary(x,z)),
	   (extw_3_schema, extw_3_o3_schema_addition),
	   [emp_array_1, emp_array_2, emp_array_3] ).

def(extw_o3_34) ::
definiens( (emp(x), name(x,y), salary(x,z)),
	   (extw_3_schema, extw_3_o3_schema_addition, i(z)),
	   [emp_array_1, emp_array_2, emp_array_3] ).

def(extw_o3_35) ::
definiens( (emp(x), name(x,y), salary(x,z)),
	   (extw_3_schema, extw_3_o3_schema_addition, i(x), i(z)),
	   [emp_array_1, emp_array_2, emp_array_3] ).

:- ppl_printtime(ppl_ipol(extw_o3_32, [style=full, add_cm_options=[ordp]])).
:- ppl_printtime(ppl_ipol(extw_o3_34, [style=full, add_cm_options=[ordp]])).
:- ppl_printtime(ppl_ipol(extw_o3_35, [style=full, add_cm_options=[ordp]])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
\bibliographystyle{alpha}
\bibliography{bibscratch03}
*/

:- ppl_set_source.
:- mac_install.
