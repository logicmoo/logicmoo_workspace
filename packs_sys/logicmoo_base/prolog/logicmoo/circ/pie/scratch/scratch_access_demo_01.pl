/**
\title{Access Predicates Demo}
\date{Revision: March 7, 2017; Rendered: \today}
\maketitle

\noindent Illustration of the computation of access predicates for an RDF fact
base. Makes use of scratch\_forgetting and scratch\_definientia. Formalized
with the \href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.
*/

:- use_module(folelim(support_scratch)).
:- ensure_loaded(scratch_forgetting).
:- ensure_loaded(scratch_definientia).

% :- set_info_verbosity(10).
:- set_info_verbosity(50).

:- ppl_set_options(ipol, [style=full,add_cm_options=[hs]-[hd1,r8(_)]]).
% :- ppl_set_options(ipol, [style=full]).

/**

\section{Introduction}

We assume a RDF knowledge base with facts like 

\begin{center}
\begin{tabular}{l}
\texttt{rdf\_triple(p1, name, 'Sulzer').}\\
\texttt{rdf\_triple(p1, yob, 1720).}
\end{tabular}
\end{center}

\section{Specifications of Access Predicates}

Specifications of access predicates for \texttt{person\_name} and
\texttt{person\_yob}. In practice, these would be automatically generated from
the schema of the knowledge base.

*/
def(accessor_spec) ::
all([p,n], (b(p) -> (person_name(p, n) <-> person_name_bf(p, n)))),
all([p,n], (b(n) -> (person_name(p, n) <-> person_name_fb(p, n)))),
all([p,n], (b(p) -> (person_yob(p, n) <-> person_yob_bf(p, n)))),
all([p,n], (b(n) -> (person_yob(p, n) <-> person_yob_fb(p, n)))),
all([p,n], (person_name(p, n) -> b(p), b(n))),
all([p,n], (person_yob(p, n) -> b(p), b(n))).

def(accessor_spec_1) ::
all([p,n], (b(p) -> (person_name(p, n) <-> person_name_bf(p, n)))),
all([p,n], (b(n) -> (person_name(p, n) <-> person_name_fb(p, n)))),
all([p,n], (person_name(p, n) -> b(p), b(n))).


/**
\section{Some Example Queries, Processed by Interpolation}

$b(n)$ in the background formula effects that $\texttt{person\_name}$ is
rewritten \texttt{to person\_name\_fb}:

*/
def(rewrite_1) ::
definiens( person_name(p, n),
	   ( accessor_spec_1, b(n) ),
	   [person_name_bf, person_name_fb] ).

:- ppl_printtime(ppl_ipol(rewrite_1)).

/*
To see the expansion of rewrite\_1:

mac_expand(rewrite_1).

To create an image of the tableau for interpolation:

ppl_ipol(rewrite_1, [style=full,add_cm_options=[hs]-[hd1,r8(_)],ip_dotgraph='/tmp/d1.gif']).

*/

/**
\noindent
This is the macro expansion of \texttt{rewrite\_1}:
*/

:- ppl_printtime(ppl_form(rewrite_1, [expand=true])).

/**
\noindent
For given name compute years in which a person with that name has been born:
*/
def(rewrite_2) ::
definiens( ex(p, (person_name(p, n), person_yob(p, y))),
	   ( accessor_spec, b(n) ),
	   [person_name_bf, person_name_fb, person_yob_bf, person_yob_fb] ).

:- ppl_printtime(ppl_ipol(rewrite_2)).

/**
\section{Referential Constraints}

The predicate \texttt{person} can be accessed to ``enumerate''
all persons. There is a referential constraint from
\texttt{person\_name} to \texttt{person}.
*/

def(person_spec) ::
all([p], (person(p) -> b(p))),
all([p,n], (person_name(p,n) -> person(p))).

def(rewrite_3) ::
definiens( ex(p, (person_name(p, n))),
	   ( person_spec, accessor_spec ),
	   [person, person_name_bf, person_name_fb] ).

:- ppl_printtime(ppl_ipol(rewrite_3)).

/**

\section{Different Proofs -- Different Interpolants}

Here only the second returned interpolant is without the redundant occurrence
of \texttt{person}.
*/

def(rewrite_4) ::
definiens( ex(p, (person_name(p, n), person_yob(p, y))),
	   ( person_spec, accessor_spec, b(n) ),
	   [person, person_name_bf, person_name_fb, person_yob_bf, person_yob_fb] ).

:- ppl_printtime(ppl_ipol(rewrite_4, [enum_ips=2,style=full,add_cm_options=[hs]-[hd1,r8(_)]])).

:- ppl_set_source.
:- mac_install.
