/**
\title{Axiomatizations of Orderings}
\date{Revision: August 24, 2018; Rendered: \today}
\maketitle

\noindent Formalized with the
\href{http://cs.christophwernhard.com/pie/}{\textit{PIE}} system.
*/

:- use_module(folelim(support_scratch)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_default_brief_options([predicates=[p,q,r],
			      constants=[x,y,z,u,v,w]]).

/**
\section{General Properties of Binary Relations}
*/

def(p-is_reflexive(p), [syntax=brief]) ::
all(x, pxx).

def(p-is_irreflexive(p), [syntax=brief]) ::
all(x, ~pxx).

def(p-is_symmetric(p), [syntax=brief]) ::
all(xy, (pxy -> pyx)).

def(p-is_asymmetric(p), [syntax=brief]) ::
all(xy, (pxy -> ~pyx)).

def(p-is_antisymmetric(p), [syntax=brief]) ::
all(xy, (pxy, pyx -> x=y)).

def(p-is_total(p), [syntax=brief]) ::
all(xy, (pxy ; pyx)).

def(p-is_transitive(p), [syntax=brief]) ::
all(xyz, (pxy, pyz -> pxz)).

def(p-is_trichotomous(p), [syntax=brief]) ::
all(xy, ( (pxy, ~pyx, ~(x=y))
	; (~pxy, pyx, ~(x=y))
	; (~pxy, ~pyx, (x=y)) )).

def(p-is_connected(p), [syntax=brief]) ::
all(xy, (pxy ; pyx ; x=y)).

/**
\section{Orderings of Binary Relations}
*/

def(p-is_total_order(p)) ::
is_antisymmetric(p), is_transitive(p), is_total(p).

def(p-is_strict_total_order(p)) ::
is_irreflexive(p),	
is_transitive(p),
is_connected(p).

def(p-is_strict_total_order_v2(p)) ::
is_transitive(p),
is_trichotomous(p).

def(p-is_partial_order(p)) ::
is_antisymmetric(p), is_transitive(p), is_reflexive(p).

def(p-is_strict_partial_order(p)) ::
is_irreflexive(p), is_transitive(p).

/**
Some properties of orderings, for testing with theorem provers.  Not all these
problems are for all provers as easy as they seem -- see comments in the
source.
*/

%%%% 
%%%% The problems can be tested with provers e.g. by
%%%% 
%%%% ?- ppl_valid(test_orderings_1, [prover=tptp(eprover)]).
%%%% ?- ppl_valid(test_orderings_1, [prover=cm]).
%%%% ?- ppl_valid(test_orderings_1, [prover=prover9]).
%%%%

def(test_orderings_1) ::
is_asymmetric(p) <-> (is_antisymmetric(p), is_irreflexive(p)).

def(test_orderings_2) ::
is_transitive(p) -> (is_asymmetric(p) <-> is_irreflexive(p)).

def(test_orderings_3) ::
is_irreflexive(p), is_transitive(p) ->
(is_connected(p) <-> is_trichotomous(p)).

def(test_orderings_4) ::
is_strict_total_order_v2(p) <- is_strict_total_order(p).

%% Hard for eprover (20s)
%% Easy for prover9, cm
def(test_orderings_5) ::
is_strict_total_order_v2(p) -> is_strict_total_order(p).

%% Hard for eprover, prover9, cm
%% Easy for [prover=cm,cm_cfg=lem] or [prover=cm, add_cm_options=[l]])
def(test_orderings_6) ::
is_strict_total_order_v2(p) <-> is_strict_total_order(p).

def(test_orderings_7) ::
is_total_order(p) -> is_partial_order(p).

def(test_orderings_8) ::
is_strict_total_order(p) -> is_strict_partial_order(p).

def(test_orderings_9) ::
is_strict_partial_order(p) -> ~is_partial_order(p).

def(test_orderings_10) ::
is_strict_total_order(p) -> ~is_total_order(p).

:- ppl_set_source.
:- mac_install.
