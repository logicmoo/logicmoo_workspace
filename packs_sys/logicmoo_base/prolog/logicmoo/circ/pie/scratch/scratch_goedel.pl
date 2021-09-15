/** \title{Inspecting Gödel's Ontological Proof\\
 {\Large ``Literate automated theorem proving'' document\\ created with
  \name{\href{http://cs.christophwernhard.com/pie}{PIE}}}\\
  -- Draft --}
  \author{Christoph Wernhard}
  \date{Revision: May 10, 2019; Rendered: \today}
  \maketitle

*/

/*

  Required external systems: Prover9/Mace4, Graphviz

*/


:- use_module(folelim(support_scratch)).

:- set_info_verbosity(50).
:- ppl_set_verbosity(9).

%%%%
%%%% fast_mode - if true omit invocations of bibtex, second call to pdflatex,
%%%% dot to create graphs etc. to speed-up "ppl" for debugging and
%%%% experimenting
%%%%
fast_mode :- false.
% fast_mode. 

latex_header_hook :-
	latex_biblio(Biblio),
	latex_custom(Custom),
	Ls = ['\\usepackage{graphicx}',
	      '\\usepackage{amssymb}',
	      '\\usepackage{filecontents}',
	      '\\begin{filecontents}{\\jobname.bib}',
	      Biblio,
	      '\\end{filecontents}'|
	      Custom],
	( member(L, Ls),
	  writeln(L),
	  fail
	; true
	).

:- ppl_override_options(ppl,
			[before_begin_document=latex_header_hook,
			 deflabels=true,
			 latex_processing=[pdflatex,bibtex,pdflatex,pdflatex]
			]).

:- (fast_mode -> ppl_override_options(ppl, [latex_processing=[pdflatex]]) ; true).


:- ppl_override_options([style=full, qstyle=quant]).

:- set_prolog_flag(allow_variable_name_as_functor, true).

:- register_ppl_pl_method(quote(A,B), Options,
			  format('~@\\; \\assign\\; \\ulcorner ~@\\urcorner',
				 [write_form(B, Options),
				  write_form(A, Options)])).

:- register_pplatex_functor(ax_1_lr, 'ax_{1}^{\\rightarrow}', 6).
:- register_pplatex_functor(ax_1_rl, 'ax_{1}^{\\leftarrow}', 6).
:- register_pplatex_functor(top, '\\top', 1).
:- register_pplatex_functor(bot, '\\bot', 1).
:- register_pplatex_functor(bot_q, '\\quoted{\\bot}', 3).
:- register_pplatex_functor(not_top_q, '\\quoted{\\lnot\\top}', 4).
:- register_pplatex_functor(g_q, '\\quoted{g}', 3).
:- register_pplatex_functor(ne_q, '\\quoted{ne}', 4).
:- register_pplatex_functor(p_q, '\\quoted{p}', 3).
:- register_pplatex_functor(pre_lemma_1_alt, 'pre\\_lemma_{1}\\_alt', 15).
:- register_pplatex_functor(def_1_lr, 'def_{1}^{\\rightarrow}', 6).
:- register_pplatex_functor(def_1_lrn, 'def_{1}^{\\rightarrow \\lnot}', 6).
:- register_pplatex_functor(def_3_lr, 'def_{3}^{\\rightarrow}', 6).
:- register_pplatex_functor(lemma_2_simp, 'lemma_{2}\\_simp', 7).
:- register_pplatex_functor(thm_3_simp, 'thm_{3}\\_simp', 9).
:- register_pplatex_functor(pre_thm_3_simp, 'pre\\_thm_{3}\\_simp', 13).
:- register_pplatex_functor(pre_thm_3_simp_inst,
			    'pre\\_thm_{3}\\_simp\\_inst', 18).
:- register_pplatex_functor(pre_thm_3_inst, 'pre\\_thm_{3}\\_inst', 13).
:- register_pplatex_functor(pre_thm_3_tight, 'pre\\_thm_{3}\\_tight', 14).


%%%% 
%%%% For use in \prolog
%%%% 
prf(F) :-
	mark_installed_macros(F, F1),
	pp_form(F1, [format=latex,style=full,qstyle=quant]).
prm(X) :-
	pp_form('$macro'(X), [format=latex,style=full]).
prc(X) :-
	pp_form(X, [format=latex,style=full,qstyle=quant,qstyle=quant]).

/**
  \tableofcontents
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
\subsection*{Remark: Current Status of this Document}

  This is a draft of inspecting Gödel's ontological proof with the \name{PIE}
  (\name{Proving, Interpolating, Eliminating}) system.  It gives an example of
  applying the system's ``literate automated theorem proving'' interface to
  formalize and investigate a nontrivial theory. The source code demonstrates
  for several recently added system features how these can be used.  With
  respect to the subject, the analysis of Gödel's proof, this version of the
  document is to be seen just a very first draft. Nevertheless, some aspects
  might already be of interest, in particular those made possible through
  second-order quantifier elimination such as ``reduced'' views on
  \name{essence} and \name{necessary existence} as well as approaches to find
  weakest sufficient frame conditions.
  
*/

/**
  \section{Introduction}

  \subsection{Background}
  
  Gödel bequeathed a short text with an ontological proof of the existence of
  God.  In 1970 he showed the proof to Scott, who also recorded it in a
  slightly different version. Transcripts of both handwritten manuscripts have
  been published later by Sobel \cite{sobel:1987:goedel}. From this starting
  point, a number of variations of Gödel's axiomatization have since been
  suggested in the literature.  Comprehensive background and discussion is
  provided in Sobel's book \cite{sobel:theism}, which also reproduces the
  transcripts, and in Fitting's book \cite{fitting:god}.  Both books present
  formalizations in modal predicate logic, along with formal proofs, in a
  natural deduction system and in the framework of analytic tableaux,
  respectively.

  The investigation of Gödel's proof with automated systems was initiated by
  Benz\-müller and Woltzenlogel Paleo in \cite{benzmueller:etal:2014:goedel}.
  A higher-order modal logic is embedded there into classical higher-order
  logic, which, in turn, is supported by a combination of automated theorem
  proving and verification systems. In several follow-up works variations of
  Gödel's proof have been analyzed with different techniques and automated
  systems (see \cite{kirchner:etal:2019:metaphysics} for an overview).  The
  automated approach enforces precise and detailed formalizations. Together
  with the possibility to test for vast numbers of combinations of axioms
  whether they entail candidate theorems this led to many new observations.
  
  Here we approach Gödel's proof with an automated system that is centered
  round \emph{first-order} theorem proving, which it extends by second-order
  quantifier elimination and the support for expressing first-order
  formalizations by means of schemas or macros.  The impact of these
  techniques on the analysis of axiomatizations and proofs is can be
  summarized as follows:

  \paragraph{Classical First-Order Logic as Basis.}

  Compared to a higher-order setting, immediate limitations are that
  quantification upon predicate symbols is not permitted, predicates are not
  allowed to occur in argument position, and there is no abstraction mechanism
  that allows to construct predicates from formulas.\footnote{Similar remarks
  also hold for functions in addition to predicates.}  The first aspect,
  quantification upon predicates, is supported to some degree in our framework
  with second-order quantifier elimination, discussed below.  The other
  aspects, predicates as arguments and construction of predicates through
  abstraction are in Gödel's proof actually only required with respect to
  specific instances that can be expressed in first-order logic.  A potential
  reward for the explicit creation of instances is that information about
  which instances are used in proofs is then trivially available.  Explicit
  instantiation by predicates and in some contexts also individuals suggests
  to use first-order logic together with schemas, as common in mathematics.
  Our framework supports this approach with a mechanism to specify formula
  macros.  We represent modal formulas directly in their standard translation,
  which facilitates the consideration of frame conditions that are represented
  directly by first- or second-order formulas.  First-order logic is
  well-known, ensuring that the results of investigations do not reflect
  unnoticed features of some special underlying logic.
  
  \paragraph{Second-Order Quantifier Elimination.}

  Second-order quantifier elimination \cite{soqe:book} is the computational
  task of computing for a given second-order formula an equivalent first-order
  formula. Since not all second-order formulas have a first-order equivalent,
  this task is inherently incomplete.  A traditional application field of
  second-order quantifier elimination is to compute from a given modal axiom
  the corresponding frame property. Consider, for example, the axiom $\Box p
  \imp p$, known as $M$ or $T$. Its correspondence to reflexivity of the
  accessibility relation $\f{r}$ can be automatically established by
  second-order quantifier elimination:

  \smallskip
  
*/

:- ppl_printtime(ppl_elim(all2(p, all(v, (all(w, (r(v,w) -> p(w))) -> p(v)))))).

/**
  \noindent The elimination result extracts from the modal axiom what it
  states about the accessibility relation.  In general, the extraction of
  knowledge about a subvocabulary by second-order quantifier elimination can
  be useful to gain insight into the meaning of axioms and defined concepts.

  \paragraph{Computing Weakest Sufficient Conditions.}
  \label{sec-wsc-informal}
  
  The \emph{weakest sufficient condition}
  \cite{lin:snc,dls:snc,cw:2012:projcirc} of formula $G$ on a set~$Q$ of
  predicates within a formula $F$ can be characterized as the second-order
  formula \[\forall p_1 \ldots \forall p_n\, (F \imp G),\] where $p_1, \ldots,
  p_n$ are all predicates that occur free in $F \imp G$ and are not members of
  $Q$.  This second-order formula denotes the weakest (with respect to
  entailment) formula~$H$ in which only predicates in $Q$ occur free such that
  $F \land H \imp G$ is valid, or equivalently, such that $H \imp (F \imp G)$
  is valid.  Second-order quantifier elimination can be applied to ``compute''
  a weakest sufficient condition, that is, converting it to a first-order
  formula, which, of course, is inherently not possible in all cases.  This
  application pattern of second-order quantifier elimination seems
  particularly useful in the inspection of theories, as it allows to
  characterize in a backward, goal-oriented or abductive way the requirements
  about predicates $Q$ that are missing to conclude from some given axioms~$F$
  a given theorem~$G$.
  
  \subsection{Technical Notes}

  \begin{enumerate}

  \item This document is processed by the \name{PIE} system, described in
  \cite{cw:2016:pie}. The formal macro definitions are read by the system.
  Macros without parameters play the role of formula names.  The system
  invokes reasoners on proving, elimination and interpolation tasks.  Their
  outputs are presented with phrases such as \name{This formula is valid},
  \name{This formula is not valid}, and \name{Result of elimination}.
  
  \item We write formulas of modal predicate logic as formulas of classical
  first-order logic by applying the standard translation from
  \cite[Sec.~11.4]{benthem:open} and \cite[Chap.~XII]{benthem:mlcl}.  The
  binary predicates $\f{r}$ and $\f{e}$ are used for world accessibility and
  membership in the domain of a world.
  
  \item As target logic we do not use a two-sorted logic nor encode
  two-sortedness explicitly with relativizer predicates. However, the
  translation of modal formula yields formulas in which all quantifications
  are relativized by $\f{r}$ or by $\f{e}$, which seems to subsume the effect
  of such relativizer predicates.  To express that free individual symbols are
  of sort \name{world} we use the unary predicate
  $\f{world}$. Macros~\ref{def:r:world} and \ref{def:r:world:1} defined below
  can be used as axioms that relate $\f{world}$ and $\f{r}$ as far as needed
  here.

  \item The used standard translation realizes with respect to the represented
  modal logic \name{varying domain semantics} (\name{actualist notion of
  quantification}), expressed with the existence predicate $\f{e}$. Axioms
  that state domain increase and decrease can be used to to obtain
  \name{constant domain semantics} (\name{possibilist notion of
  quantification}).

  \item As technical basis for Gödel's proof we use the presentation of
  Scott's version \cite[Chapter~IV, Appendix B]{sobel:theism} in
  \cite[Fig.~1]{benzmueller:etal:2017:assisted}. The axiom and theorem
  numbering follow these documents.  In
  \cite[Fig.~1]{benzmueller:etal:2017:assisted} there are two additional
  lemmas, \name{L1} and \name{L2}. Of these, we only use \name{L1} and call it
  \name{Lemma~2}, reserving \name{Lemma~1} for another lemma, used in an
  earlier proof stage.

  \item The \LaTeX\ presentation of formulas and macro definitions bears some
  footprint from Prolog's syntax, since the underlying system \name{PIE} uses
  a Prolog-based syntax for logical formulas and supports interaction through
  the Prolog interpreter: Macro parameters and bound logical variables that
  are to be bound to fresh symbols at macro expansion are represented in the
  system by Prolog variables, and thus start with capital letters.
  Where-clauses in macro definitions are used to display in abstracted form
  special Prolog code that is executed at macro expansion.

  \item The available automated deduction techniques include the following:

  \begin{itemize}

  \item First-order theorem proving, in particular with
  resolution/paramodulation (\name{Prover9}) and clausal tableaux (\name{CM}),
  as well as finding finite first-order ``countermodels''
  (\name{Mace4}).\footnote{Other first-order systems that support the TPTP
  format as well as propositional systems that support the DIMACS format could
  also be integrated.} The clausal tableau prover is weak with equality, as it
  operates in a goal-oriented way, sometimes quite sensitive to settings like
  the particular division of a problem into axiom and theorem part, and has no
  means to ensure that a problem is (counter-) satisfiable like \name{Mace4}.
  It outputs clausal tableaux that can be graphically displayed.

  \item Second-order quantifier elimination with an implementation of the
  \name{DLS} algorithm \cite{dls} that is based on Ackermann's Lemma.

  \item Various methods for formula simplification, clausification and
  unskolemization that are applied in preprocessing, inprocessing, and for
  output presentation. (The latter seems a major issue by itself that is far
  from being solved.)

  \item First-order Craig interpolation on the basis of clausal tableaux
  (currently not used in this document).

  \end{itemize}
  
  \end{enumerate}

  \subsection{Structure of the Document}

  Sections~\ref{sec-positiveness}--\ref{sec-thm-3} each discuss a stage of
  Gödel's argument, roughly following the division in
  \cite[Chapter~11]{fitting:god}.  Further aspects and variants are discussed
  in Sections~\ref{sec-special-thm-3}--\ref{sec-special-last}.
  Section~\ref{sec-aux} is for auxiliary definitions of merely technical
  system related character.  Observations that seem to be of particular
  interest for further investigation are highlighted with
  ``\raisebox{0.2ex}[0pt][0pt]{$\blacktriangleright$}''.
  
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
  
  \section{Positiveness}
  \label{sec-positiveness}
  
  \subsection{Auxiliary Sort Inference Predicate}

  To express that free individual symbols are of sort \name{world} we use the
  unary predicate $\f{world}$. The following formulas can be used as axioms
  that leads from $\f{r}(v,w)$ to $\f{world}(v)$ and $\f{world}(w)$ or, just
  to $\f{world}(w)$, respectively. The latter, weaker, formula is sufficient
  in some of the considered contexts.
  
*/
  
def(r_world) ::
all([v,w], (r(v,w) -> world(v), world(w))).

def(r_world_1) ::
all([v,w], (r(v,w) -> world(w))).

/**

  \subsection{Representing Verum and Falsum}
  \label{sec-assoc-constant}
*/

quote(~P, P1) :-
	!,
	format(atom(P1), 'not_~w_q', [P]).
quote(P, P1) :-
	!,
	format(atom(P1), '~w_q', [P]).

/**

  \name{Positiveness} is in Gödel's theory a predicate that applies to
  predicates.  In the actual proof, however, it is used only in a small number
  of instances with specific argument predicates: $\lambda x . x = x$,
  $\lambda x . x \neq x$, and an arbitrary but fixed predicate.  In
  correspondence with the standard translation, we represent $\lambda x . x =
  x$ and $\lambda x . x \neq x$ by binary predicates~$\top$ and~$\bot$, where
  the first argument is a world.  These predicates may be defined
  follows:\footnote{Perhaps there are other possibilities to define them. The
  interplay of these predicates with the existence predicate seems not
  straightforward.}

*/

def(topbot_def) ::
all([v,x], (world(v) -> (top(v,x) <-> e(v,x)))),
all([v,x], (world(v) -> (bot(v,x) <-> ~e(v,x)))).

/**

  \noindent The following formula expresses equivalence of the binary
  predicate $\top$ and $\lambda v x . \lnot \bot(v,x)$:
*/

def(topbot_equiv) ::
all([v,x], (world(v) -> (top(v,x) <-> ~bot(v,x)))).

:- ppl_printtime(ppl_valid(topbot_def -> topbot_equiv)).

/**

  \medskip
  
  \noindent In our first-order framework we do not admit a predicate that has
  a predicate as argument.  But for the purpose of Gödel's proof, this can be
  simulated it by a predicate that is applied instead to an individual
  constant representing the argument predicate.  We use the constants
  $\quoted{\top}$, $\quoted{\bot}$ and $\quoted{\lnot \top}$ to designate the
  individuals associated with $\top$, $\bot$ and $\lambda v x . \lnot \top(v,
  x)$, respectively.  The following axiom leads from the equivalence expressed
  by Macro~\ref{def:topbot:equiv} to equality of the associated individuals
  $\quoted{\bot}$ and $\quoted{\lnot \top}$:

*/

def(topbot_equiv_equal) ::
topbot_equiv -> bot_q = not_top_q.


/**

  \noindent Equality is here understood with respect to first-order logic, not
  qualified by a world parameter.  In Section~\ref{sec-lemma-1-pre-weaker}
  below an alternative is shown, where in essence the equality is replaced by
  a weaker substitutivity property.

*/


/**

  \subsection{Proving Theorem~1}
  \label{sec-thm-1}
  
  \noindent The left-to-right direction of Axiom~1 of Scott's version is
  rendered by the following macro. (The right-to-left direction is stated
  below as Macro~\ref{def:ax:1:rl:2}.) We represent \name{is positive} by the
  binary predicate $\f{pos}$ which has a world and an individual representing
  a predicate as argument.

  At macro expansion, the individual constants $P'$ and $N'$ associated with
  the supplied predicate symbol~$P$ and with $\lambda v x . \lnot P(v,x)$,
  respectively, are determined by the code in the where clause. This technique
  is also used in further macro definitions.

  In general, we expose the current world as a macro parameter~$V$. This
  facilitates to identify proofs steps where axioms are not applied just with
  respect to the initially given current world but to some other reachable
  world.
  
*/

def(ax_1_lr(V, P)) ::
world(V) -> (pos(V,N_p) -> ~pos(V, P_p)) ::-
	quote(~P, N_p),
	quote(P, P_p).

/**
  \medskip
  
  \noindent The following macro renders Axiom~2 of Scott's version:

*/


def(ax_2(V,P,Q)) ::
world(V) ->
(( pos(V,P_p),
   all(W, (r(V,W) -> all(X, (e(W,X) -> (P(W,X) -> Q(W,X))))))) ->
 pos(V,Q_p)) ::-
	quote(P, P_p),
	quote(Q, Q_p).


/**
  \medskip
  
  \highlightpar We can now derive the following lemma, called here Lemma~1 (it
  is not explicitly present in Scott's version), using just a single instance
  of each of $\prolog{prm(ax_1_lr)}$ and $\prolog{prm(ax_2)}$, where
  $\top$ and $\bot$ are the only predicates used for instantiating:
  
*/

def(lemma_1(V)) ::
world(V) -> ~pos(V, bot_q).

def(pre_lemma_1(V)) ::
r_world_1,
topbot_def,
topbot_equiv_equal,
ax_1_lr(V,top),
ax_2(V,bot,top).

:- ppl_printtime(ppl_valid(pre_lemma_1(v) -> lemma_1(v),
			   [mace=false])).

/**

  \medskip

  \noindent Theorem~1 of Scott's version can be rendered as a macro with a
  predicate parameter:

*/

def(thm_1(V,P)) ::
world(V) ->
(pos(V,P_p) ->
 ex(W, (r(V,W), ex(X, (e(W,X), P(W,X)))))) ::-
	quote(P, P_p).

/**
  \medskip \noindent Instances of $\prolog{prm(thm_1(V,P))}$ can be proven for
  arbitrary worlds $V$ and predicates $P$, from the respective instance of the
  axioms $\prolog{prm(pre_thm_1(V,P))}$.  A further instance of
  $\prolog{prm(ax_2)}$ (beyond that used to prove $\prolog{prm(lemma_1)}$) is
  now required, with respect to $\false$ and the given predicate~$P$.
*/
  
def(pre_thm_1(V,P)) ::
lemma_1(V),
ax_2(V,P,bot).

:- ppl_printtime(ppl_valid(pre_thm_1(v,p) -> thm_1(v,p), [mace=false])).

/**
  \medskip \noindent When expanded, the formula $\prolog{prf(pre_thm_1(v,p) ->
  thm_1(v,p))}$, whose validity has just been shown, looks as follows:

*/
:- ppl_printtime(ppl_form(pre_thm_1(v,g) -> thm_1(v,g), [expand=true])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**

  \section{Possibly God Exists}

  \subsection{A Corollary of Theorem~1}
  
  Axiom~3 of Scott's version states that the predicate \name{god-like} has the
  property \name{is positive}.  Together with Theorem~1 instantiated by
  $\name{god-like}$ it is used to derive corollary \name{Coro}. This is
  rendered in the following formula definitions and validity statement, where
  \name{god-like} is represented by $\f{g}$.  Scott lets the definition of
  \name{god-like} precede Axiom~3.  Since that definition is not required to
  prove $\name{Coro}$, we postpone its discussion to Section~\ref{sec-def-g}.
*/

def(ax_3(V)) ::
world(V) -> pos(V, g_q).


def(coro(V)) ::
world(V) -> ex(W, (r(V,W), ex(X, (e(W,X), g(W,X))))).

def(pre_coro(V)) ::
thm_1(V,g), ax_3(V).

:- ppl_printtime(ppl_valid(pre_coro(v) -> coro(v), [mace=false])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**

  \section{Essence}

  \subsection{Fragments of the Definition of God-Like}
  \label{sec-def-g}

  With macros $\prolog{prm(def_1_lr)}$ and $\prolog{prm(def_1_lrn)}$, defined
  now, we represent the left-to-right direction of the definition of
  \name{god-like} in Scott's version.

  \highlightpar Actually, only this direction is used in the proof of the
  existence of God.

  \noindent The macros have a predicate as parameter that would be universally
  quantified in a higher-order version. The first macro expands into a formula
  in which the supplied predicate~$P$ and its associated constant (see
  Section~\ref{sec-assoc-constant}) do occur. In the second macro, their
  respective places are taken by the negated supplied predicate and the
  corresponding constant, that is, the constant associated with $\lambda v x
  . \lnot P(v,x)$.

*/  

def(def_1_lr(V,X,P)) ::
g(V,X) -> (pos(V,P_p) -> P(V,X)) ::-
	quote(P, P_p).

def(def_1_lrn(V,X,P)) ::
g(V,X) -> (pos(V,P_p) -> ~P(V,X)) ::-
	quote(~P, P_p).

/**
  
  \subsection{The Essence of an Individual}

  The following macro $\prolog{prm(val_ess)}$ renders the definiens of the
  \name{Ess}, or \name{essence of}, relationship between a predicate and an
  individual in Scott's version.  It is originally a formula with predicate
  quantification, but without application of a predicate to a predicate.  Our
  macro exposes the universally quantified predicate as parameter~$Q$, which
  permits to use it just instantiated with some specific predicate. The
  quantified version can be still be expressed simply by prefixing a predicate
  quantifier upon~$Q$.
    
*/

def(val_ess(V,P,X,Q)) ::
P(V,X),
(Q(V,X) ->
 all(W, (r(V,W) -> all(Y, (e(W,Y) -> (P(W,Y) -> Q(W,Y))))))).


/**
  \medskip

  \highlightpar Eliminating the quantified predicate gives another view on
  \name{essence}:
  
*/

:- ppl_printtime(ppl_elim(all2(q, val_ess(v,p,x,q)), [simp_result=c5])).

/**
  
  \medskip \noindent We define a predicate $\prolog{prc(ess)}$ in terms of the
  macro $\prolog{prm(val_ess)}$. This facilitates combining propositions that
  depend on the definiens with propositions that can be established
  independently from it:
  
*/

def(def_ess(V,P)) ::
world(V) -> all(X, (ess(V,P_p,X) <-> all2(Q, val_ess(V,P,X,Q)))) ::-
	quote(P, P_p).


/**

  \medskip
  
  \noindent The following two observations are mentioned as \name{Note} in
  Scott's version. We express them with the predicate version
  $\prolog{prc(ess)}$ of $\name{Ess}$ to facilitate their use as
  axioms in other statements:
*/


def(note_1(V,P,Q)) ::
world(V) ->
( ex(X, (ess(V,P_p,X), ess(V,Q_p,X))) ->
  all(W, (r(V,W) -> all(Y, (e(W,Y) -> (P(W,Y) <-> Q(W,Y))))))) ::-
	quote(P, P_p),
	quote(Q, Q_p).

:- ppl_printtime(ppl_valid(( def_ess(v,p1), def_ess(v,p2) ->
			     note_1(v,p1,p2)), [elim=true])).


def(note_2(V,P,X)) ::
world(V) ->
( ess(V,P_p,X) ->
  all(W, (r(V,W) -> all(Y, (e(W,Y) -> (P(W,Y) -> Y=X)))))) ::-
	quote(P, P_p).


:- ppl_printtime(ppl_valid((def_ess(v,p) -> note_2(v,p,x)), [elim=true])).

/**
  
  \subsection{Deriving Theorem~2 -- Almost}

  The right-to-left direction of Axiom~1 and Axiom~4 of Scott's version are
  rendered by macros $\prolog{prm(ax_1_rl)}$ and $\prolog{prm(ax_1_rl)}$,
  respectively, which are defined now. Both original axioms involve a
  universally quantified predicate that appears only in argument role. In the
  macro, that predicate appears simply as a parameter.
  
*/

def(ax_1_rl(V, P)) ::
world(V) -> (~pos(V,P_p) -> pos(V,N_p)) ::-
	quote(~P, N_p),
	quote(P, P_p).

def(ax_4(V,P)) ::
world(V) -> (pos(V,P_p) -> all(W, (r(V,W) -> pos(W,P_p)))) ::-
	quote(P, P_p).

/**

  \medskip

  \noindent The following macro renders the rudiment of Theorem~2 of Scott's
  version. Originally, the $Q$ parameter is a universally quantified predicate
  inherited from the definiens of \name{Ess}.
  
  */

def(raw_thm_2(V,X,Q)) ::
world(V) -> (e(V,X) -> (g(V,X) -> val_ess(V,g,X,Q))).

def(pre_thm_2(V,X,Q)) ::
ax_1_rl(V,Q),
all(W, (r(V,W) -> all(X, (e(W,X) -> def_1_lr(W,X,Q))))),
def_1_lrn(V,X,Q),
ax_4(V,Q).

/**

  \medskip \noindent Theorem~2 would correspond to \[\prolog{prf(all([q, v,
  x], raw_thm_2(v,x,q)))}.\] The following statement can be proven for
  arbitrary individual symbols $\f{v}, \f{x}$ and predicate
  symbols~$\f{q}$. It is sufficient to derive a particular instance of the
  universally quantified Theorem~2 from a corresponding instance of the
  required axioms:

  \smallskip
*/  

:- ppl_printtime(ppl_valid(pre_thm_2(v,x,q) -> raw_thm_2(v,x,q))).

/**

  \medskip
  \noindent Moving a bit more to the full quantified version of Theorem~2, we
  can also prove:
  
*/

def(derive_almost_thm_2) :: all2(q, (all([v,x], ex([q_q, not_q_q],
pre_thm_2(v,x,q))) -> all([v,x], raw_thm_2(v,x,q)))).

:- ppl_printtime(ppl_valid(derive_almost_thm_2)).

/**

  \medskip \noindent The following statement represents that Theorem~2 is
  implied by the required axioms $\prolog{prm(pre_thm_2)}$, also under
  universal quantifications of its parameters and existential quantification
  of the predicate representatives:
  
*/

def(derive_thm_2) ::
all2(q, all([v,x], ex([q_q, not_q_q], pre_thm_2(v,x,q)))) ->
all2(q, all([v,x], raw_thm_2(v,x,q))).

/**

  \highlightpar The validity of $\prolog{prm(derive_thm_2)}$ seems derivable
  from the validity of $\prolog{prm(derive_almost_thm_2)}$ quite easily on a
  shallow level by Boolean reasoning and quantifier manipulation.  The current
  version of \name{PIE}, however, would try to prove validity of
  $\prolog{prm(derive_thm_2)}$ by eliminating the universal predicate
  quantifier in the antecedent, on which it does not succeed.  Thus, at this
  point, with the current version of \name{PIE} there is a gap in the formal
  proof, which, however, should be resolvable in principle.
  
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**

  \section{If God Exists, then Necessarily God Exists}

  \subsection{Necessary Existence}

  The property \name{NE} in Scott's version applies to an individual and means
  that it necessarily exists if it has an essential property.  The definiens
  of \name{NE} is rendered here by the macro $\prolog{prm(val_ne)}$.  It is
  originally expressed as a formula with predicate quantification, inherited
  from the definiens of \name{Ess} but without application of a predicate to a
  predicate.
  
*/  


def(val_ne(V,X)) ::
all2(P, (all2(Q, val_ess(V,P,X,Q)) ->
	 all(W, (r(V,W) -> ex(Y, (e(W,Y), P(W,Y))))))).


/**
  \medskip

  \highlightpar Eliminating the quantified predicate gives another view on
  \name{NE}:
  
*/

:- ppl_printtime(ppl_elim(val_ne(v,x), [simp_result=c5])).


/**
  
  \medskip \noindent We define a predicate $\prolog{prc(ne)}$ in terms of the
  macro $\prolog{prm(val_ne)}$, in analogy to the definition of
  the predicate $\prolog{prc(ess)}$:
  
*/

def(def_ne(V,X)) ::
world(V) -> (e(V,X) -> (ne(V,X) <-> val_ne(V,X))).

/**

  \subsection{Deriving that if God Exists, then Necessarily God Exists}

*/

/**
  The statement $\exists x\, \f{g}(x) \imp \Box \exists x\, \f{g}(x)$ is used
  as an unlabelled lemma in Scott's version. In
  \cite[Fig.~1]{benzmueller:etal:2017:assisted} it is called \name{L1}.  We
  call it here \name{Lemma~2} and render it below in
  Macro~\ref{def:lemma:2:1} as $\prolog{prm(lemma_2)}$.  In Scott's version
  it can be derived from Theorem~2, the definitions of $\name{NE}$ and
  $\name{Ess}$ as well as a further axiom, \name{Axiom~5}. Actually, the proof
  from these preconditions is largely independent from the definientia of
  $\name{NE}$ and $\name{Ess}$.  reconstruction.  The following formula
  renders \name{Theorem 2}, now expressed in terms of the predicate $\f{ess}$:
  
*/


def(thm_2(V,X)) ::
world(V) -> (e(V,X) -> (g(V,X) -> ess(V,g_q,X))).

/**

  \medskip \noindent The following formula renders a fragment of the
  definition of \name{NE} on a ``shallow'' level, that is, in terms of just
  the predicates $\f{ess}$ and $\f{ne}$, without referring to
  $\prolog{prm(val_ess)}$ and $\prolog{prm(val_ne)}$:
  
*/

def(def_3_lr(V,X,P)) ::
world(V) -> (e(V,X) ->
	     ( ne(V,X) ->
	       ( ess(V,P_p,X) ->
		 all(W, (r(V,W) -> ex(Y, (e(W,Y), P(W,Y)))))))) ::-
	    quote(P, P_p).

/**

  \noindent Correctness of $\prolog{prm(def_3_lr)}$ can be established by
  showing that it follows from definitions of $\f{ess}$ and $\f{ne}$ with
  definientia according to $\prolog{prm(val_ess)}$ and $\prolog{prm(val_ne)}$:

  \smallskip
*/

:- ppl_printtime(ppl_valid((def_ess(v,g), def_ne(v,x) ->
			    def_3_lr(v,x,g)),
			   [elim=true])).

/**
  \medskip \noindent (Validating this implication revealed a subtle
  shortcoming of the current version of \name{PIE}: If the biconditional signs
  in $\prolog{prm(def_ess)}$ and $\prolog{prm(def_ess)}$ would be replaced by
  implication signs, the implication just shown should also be valid. Although
  elimination on the involved predicate quantifiers should in principle
  succeed as it does in the variant with biconditionals, \name{PIE} currently
  seems to fail there.)

  \medskip
  
  The remaining macros in this section render \name{Axiom~5} of Scott's
  version, the lemma $\exists x\, \f{g}(x) \imp \Box \exists x\, \f{g}(x)$
  mentioned above and preconditions for proving it.
*/

def(ax_5(V)) ::
world(V) -> pos(V, ne_q).

def(lemma_2(V)) ::
world(V) ->
(ex(X, (e(V,X), g(V,X))) ->
 all(W, (r(V,W) -> ex(Y, (e(W,Y), g(W,Y)))))).

def(pre_lemma_2(V,X)) ::
ax_5(V),
def_1_lr(V,X,ne),
def_3_lr(V,X,g),
thm_2(V,X).

:- ppl_printtime(ppl_valid(all(v, (all(x, pre_lemma_2(v,x)) ->
				   lemma_2(v))),
			   [mace=false])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**

  \section{Necessarily God Exists}
  \label{sec-thm-3}
  
  \subsection{Proving the Main Result, Theorem~3}
  
  The following formula states \name{Theorem~3} of Scott's version, the
  overall result to show:

*/  

def(thm_3(V)) ::
world(V) -> all(W, (r(V,W) -> ex(Y, (e(W,Y), g(W,Y))))).

/**

  \noindent In proving \name{Theorem~3}, Scott proceeds from the lemma called
  here $\prolog{prm(lemma_2)}$ (Macro~\ref{def:lemma:2:1}) and the corollary
  \name{Coro}, which corresponds to our Macro~\ref{def:coro:1}.  He applies
  the modal axiom~\name{E} (or~\name{5}), which expresses that the
  accessibility relation is Euclidean. As shown apparently first in
  \cite{benzmueller:etal:2014:goedel}, \name{Theorem~3} can not be just proven
  in the modal logic~S5, but also in~KB, whose accessibility relation is less
  constrained.  In particular, the modal axiom~\name{B}, which expresses that
  the accessibility relation is symmetric, holds in KB.  We show that the
  proof is possible for a Euclidean as well as a symmetric accessibility
  relation in a single statement by presupposing the disjunction of both
  properties:
*/

def(euclidean) ::
all([x,y,z], (r(x,y), r(x,z) -> r(z,y))).

def(symmetric) ::
all([x,y], (r(x,y) -> r(y,x))).

def(pre_thm_3(V)) ::
r_world_1, all(v, lemma_2(v)), coro(V).


:- ppl_printtime(ppl_valid(((symmetric ; euclidean) ->
			    (pre_thm_3(v) -> thm_3(v))), [mace=false])).


/**
  \medskip \noindent Precondition $\prolog{prm(pre_thm_3)}$ includes
  $\prolog{prm(coro)}$ instantiated with just the current world and
  $\prolog{prm(lemma_2)}$ with a universal quantifier upon the world
  parameter. If fact, using $\prolog{prm(lemma_2)}$ there just instantiated
  with the current world would not be sufficient to derive
  $\prolog{prm(thm_3)}$:

  \medskip
*/

:- ppl_printtime(ppl_valid(((symmetric ; euclidean) ->
			    ( r_world_1, lemma_2(v), coro(v) ->
			      thm_3(v))), [mace=true])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**

  \section{Monotheism}

  In Fitting's system the proposition $\exists x \forall y\, (\f{g}(y) \equi
  y=x)$ can be derived \cite[Section~7.1]{fitting:god}. This can be proven in
  our system from $\prolog{prm(thm_2)}$, $\prolog{prm(note_2)}$ and
  $\prolog{prm(thm_3)}$ under the additional assumption of reflexivity of the
  accessibility relation. Without that assumption, it can be shown that $\Box
  \exists x \Box \forall y\, (\f{g}(y) \equi y=x)$:
*/

def(pre_monotheism) ::
all([x,v], thm_2(v,x)),
all([x,v], note_2(v,g,x)),
all([x,v], thm_3(v)),
r_world.

def(monotheism) ::
all(v, ex(x, (e(v,x), all(y, (e(v, y) -> (g(v, y) <-> y=x)))))).

:- ppl_printtime(ppl_valid((pre_monotheism, reflexive) -> monotheism,
			   [mace=false])).

def(nec_monotheism) ::
all([v,w], (r(v,w) -> ex(x, (e(w,x), all(w1, (r(w,w1) -> all(y,
(e(w1,y) -> (g(w1,y) <-> y=x))))))))).

:- ppl_printtime(ppl_valid(pre_monotheism -> nec_monotheism,
			   [mace=false])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**

  \section{Modal Collapse}

  A well-known objection to Gödel's theory is that it implies modal collapse
  \cite{sobel:1987:goedel}.
  
*/  

def(collapse) ::
all([x,y], (r(x,y) -> y=x)).


/**

  \medskip \noindent In our system, modal collapse can be derived from the
  following preconditions, selected after Fitting's reconstruction
  \cite[Chapter~11, Section~8]{fitting:god} of Sobel's proof
  \cite{sobel:theism,sobel:1987:goedel}:
  
*/  

def(pre_collapse) ::
all([x,v], thm_2(v,x)),
all([x,v], thm_3(v)),
all([v], def_ess(v,g)),
r_world,
reflexive.

:- ppl_printtime(ppl_valid((pre_collapse -> collapse),
			   [elim=true, mace=false])).

/**
  \medskip \noindent In presence of \name{collapse}, the choice between frame
  conditions \name{symmetric} and \name{euclidean} (or the modal logics KB and
  S5) becomes immaterial, as both properties are implied by \name{collapse}.
  Also Axiom~4 is in presence of \name{collapse} redundant.

  \smallskip
  
*/  


:- ppl_printtime(ppl_valid(collapse -> (symmetric,euclidean),
			   [mace=false])).

:- ppl_printtime(ppl_valid(collapse-> ax_4(v,p))).


/**

  \section{Alternate Weaker Preconditions for Lemma 1}
  \label{sec-lemma-1-pre-weaker}

  The precondition $\prolog{prm(pre_lemma_1)}$ used in Section
  \ref{sec-positiveness} to derive $\prolog{prm(lemma_1)}$ includes
  \[\prolog{prf((topbot_def, topbot_equiv_equal))}.\] The following formula is
  a weaker formula that is also sufficient for deriving
  $\prolog{prm(lemma_1)}$:
  
*/


def(topbot_alt_1) ::
all([v], (world(v) -> all(x, (e(v,x) -> top(v,x))))),
all([v], (world(v) -> (pos(v, bot_q) -> pos(v, not_top_q)))).

:- ppl_printtime(ppl_valid((topbot_def, topbot_equiv_equal -> topbot_alt_1),
		 [mace=false])).

def(pre_lemma_1_drop_topbot(V)) :: F ::-
	mac_like(pre_lemma_1(V),
		 [replace(topbot_def, true),
		  replace(topbot_equiv_equal, true)],
		 F).

:- ppl_printtime(ppl_valid((topbot_alt_1, pre_lemma_1_drop_topbot(v) ->
			   lemma_1(v)), [mace=false])).

/**
  
  \medskip \noindent A third possibility to derive $\prolog{prm(pre_lemma_1)}$
  is with the formula $\prolog{prm(topbot_alt_2)}$ defined below, which is
  like $\prolog{prm(topbot_alt_1)}$ except that $\top$ in the first conjunct
  is replaced by $\lnot \bot$:
  
*/  

def(topbot_alt_2) :: F ::-
	mac_like(topbot_alt_1,
		 [replace(top(V,X), ~bot(V,X))],
		 F).

:- ppl_printtime(ppl_valid((topbot_def, topbot_equiv_equal -> topbot_alt_2),
		 [mace=false])).

:- ppl_printtime(ppl_valid((topbot_alt_2, pre_lemma_1_drop_topbot(v) ->
			    lemma_1(v)),
			   [mace=false])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**

  \section{Frame Conditions for Deriving Theorem~3}
  \label{sec-special-thm-3}
  \label{sec-special-last}

  \subsection{The Weakest Sufficient Condition}

  We turn again to the proof of $\prolog{prm(thm_3)}$
  (Macro~\ref{def:thm:3:1}) from $\prolog{prm(pre_thm_3)}$
  (Macro~\ref{def:pre:thm:3:1}) in Section~\ref{sec-thm-3}, where we used the
  additional frame condition $\prolog{prf(euclidean ; symmetric)}$.  The
  question is now, whether it is possible to find a weaker frame condition for
  deriving $\prolog{prm(thm_3)}$.  Actually, the weakest such frame condition
  can be characterized as a second-order formula, the \name{weakest sufficient
  condition}, described briefly in Section~\ref{sec-wsc-informal}, on the
  accessibility relation and possibly the domain membership relation:
*/

def(wsc_thm_3) ::
all2(g, all(v, (pre_thm_3(v) -> thm_3(v)))).

/**
  However, elimination fails for this formula (at least with the current
  version of \name{PIE}).  The idea is now to replace
  $\prolog{prm(pre_thm_3)}$ with a weaker formula such that elimination
  becomes possible. We investigate this first in a simplified scenario.
  
  \subsection{Frame Conditions in a Modal Propositional Setting}
  
  The following macros specify versions of the formulas involved in deriving
  $\prolog{prm(thm_3)}$ from $\prolog{prm(lemma_2)}$ which are simplified in
  that they are just for propositional modal logics:
*/

def(lemma_2_simp(V)) ::
g(V) -> all(W, (r(V,W) -> g(W))).

def(coro_simp(V)) ::
ex(W, (r(V,W), g(W))).

def(thm_3_simp(V)) ::
all(W, (r(V,W) -> g(W))).

def(pre_thm_3_simp(V)) ::
all(v, lemma_2_simp(v)), coro_simp(V).

/**

  \medskip \noindent However, elimination to obtain the weakest sufficient
  frame condition as a first-order formula still fails for the simplified
  scenario (at least with \name{PIE}). The corresponding second-order formula
  is $\prolog{prf(all2(g, (pre_thm_3_simp(v) -> thm_3_simp(v))))}$.  The issue
  is now to find a weaker formula in which elimination succeed.  We inspect
  the clausal tableau proof of the following task:
*/

:- ppl_printtime(( fast_mode ->
		   ppl_valid((symmetric ->
			      (pre_thm_3_simp(v) ->thm_3_simp(v))),
			     [mace=false])
		 ; ppl_valid((symmetric ->
			      (pre_thm_3_simp(v) ->thm_3_simp(v))),
			     [prover=cm, proof=ignore,
			      tabx_dotgraph=printstyle('/tmp/tmp_pie_dotgraph_01.png')]))).

/**
  It is shown in Figure~\ref{fig:proof:simp:thm:3}.  Actually only two
  instances of $\prolog{prf(all(v, lemma_2_simp(v)))}$ are used in the proof.
  The following formula is a version of $\prolog{prm(pre_thm_3_simp)}$ with
  the required two instances, the second one inserted into an unfolding of
  $\prolog{prm(coro_simp)}$:
  
  \newsavebox{\figaux}
  \savebox{\figaux}{$\prolog{prf(all(v, lemma_2_simp(v)))}$}

  \begin{figure}
  \centering
  \includegraphics[width=25em]{/tmp/tmp_pie_dotgraph_01}
  \caption{Clausal tableau proof -- see discussion following
  Macro~\ref{def:pre:thm:3:simp:1}.  The two instances of \usebox{\figaux}
  appear in the clausal tableau as the two ternary clauses.}
  \label{fig:proof:simp:thm:3}
  \end{figure}
*/

def(pre_thm_3_simp_inst(V)) ::
lemma_2_simp(V), ex(W, (r(V,W), g(W), lemma_2_simp(W))).

/**
  The instantiated preconditions $\prolog{prm(pre_thm_3_simp_inst)}$ are
  indeed implied by the original preconditions:

  \smallskip
*/

:- ppl_printtime(ppl_valid(pre_thm_3_simp(v) -> pre_thm_3_simp_inst(v))).

/**
  
  \medskip \noindent The instantiated preconditions
  $\prolog{prm(pre_thm_3_simp_inst)}$ are sufficiently strong to derive
  $\prolog{prm(thm_3_simp)}$, under the additional precondition
  $\prolog{prm(symmetric)}$, and, alternatively, also under the additional
  precondition $\prolog{prm(euclidean)}$:

  \smallskip
*/

:- ppl_printtime(ppl_valid((symmetric ->
			    (pre_thm_3_simp_inst(v) -> thm_3_simp(v))))).
:- ppl_printtime(ppl_valid((euclidean ->
			    (pre_thm_3_simp_inst(v) -> thm_3_simp(v))))).

/**
  \medskip \highlightpar With $\prolog{prm(pre_thm_3_simp_inst)}$ as
  precondition for $\prolog{prm(thm_3_simp)}$ elimination to obtain the
  weakest sufficient frame condition as a first-order formula now succeeds:

  \smallskip
*/  

:- ppl_printtime(ppl_elim(all2(g,
			       all(v, (pre_thm_3_simp_inst(v) ->
				       thm_3_simp(v)))), [simp_result=c5])).

/**
  \noindent We write the resulting first-order formula in a slightly different
  form and give it a name:

*/

def(frame_cond_simp) ::
all([x, y, z],(r(x, y),r(x, z),~(y=x),~(z=y)->(r(y, x);r(y, z)))).

:- ppl_printtime(ppl_valid(frame_cond_simp <-> last_result)).

/**

  \medskip \highlightpar The obtained frame condition is under the assumption
  of reflexivity of the accessibility relation equivalent to
  $\prolog{prf(symmetric;euclidean)}$, and without that assumption strictly
  weaker:

*/

def(reflexive) ::
all(x, r(x,x)).

:- ppl_printtime(ppl_valid(reflexive ->
			   ( (symmetric;euclidean) <->
			     frame_cond_simp), [mace=false])).
:- ppl_printtime(ppl_valid((symmetric;euclidean) -> frame_cond_simp)).
:- ppl_printtime(ppl_valid(frame_cond_simp -> (symmetric;euclidean))).

/**
  \medskip \highlightpar Thus we have shown for the propositional modal
  setting that the first-order formula $\prolog{prm(frame_cond_simp)}$ is the
  weakest frame condition to derive $\prolog{prm(thm_3_simp(v))}$ from
  $\prolog{prm(pre_thm_3_simp_inst(v))}$.  Under the assumption of reflexivity
  this condition is equivalent to $\prolog{prf(symmetric;euclidean)}$. Without
  that assumption it is strictly weaker.  As a corollary it follows that this
  condition is also sufficient as frame condition to derive
  $\prolog{prm(thm_3_simp(v))}$ from $\prolog{prm(pre_thm_3_simp(v))}$, but in
  in this case it is not necessarily the weakest such frame condition.

  \medskip \highlightpar The pattern in which we proceeded here might possibly
  be also applicable in other situations. It can be described as follows: Our
  original problem involved a universal second-order quantifier, for which
  elimination fails. We considered a stronger universal second-order formula
  on which elimination succeeds.  Since $\forall$ can be represented by $\lnot
  \exists \lnot$, with respect to \emph{existential} predicate quantification,
  this corresponds to considering a \emph{weaker} second-order formula. We
  obtained a solution of the modified problem that also provides a solution of
  the original problem, although not necessarily the ``best'' solution
  (\emph{weakest} sufficient condition, in our case).

*/

/**

  \subsection{Considering Modal Predicate Logic Again}

  We now turn back to the problem of finding weak frame conditions for
  \[\prolog{prf(pre_thm_3(v) -> thm_3(v))}.\]
  \highlightpar In fact, the frame condition obtained for the propositional
  case also works in this case:

  \smallskip
*/

:- ppl_printtime(ppl_valid((frame_cond_simp ->
			    (pre_thm_3(v) -> thm_3(v))),
			   [mace=false])).

/**

  \medskip \noindent Can further results be obtained for the modal predicate
  logic case?  Our first attempt is to form $\prolog{prm(pre_thm_3_inst)}$ in
  analogy to $\prolog{prm(pre_thm_3_simp_inst)}$:

  */
def(pre_thm_3_inst(V)) ::
r_world_1,
lemma_2(V),
(world(V) -> ex(W, (r(V,W), ex(X, (e(W,X), g(W,X))), lemma_2(W)))).

/**
  \medskip \noindent Unfortunately, however, elimination on
  \[\prolog{prf(all2(g, all(v, (pre_thm_3_inst(v) -> thm_3(v)))))}\] does not
  succeed with \name{PIE}. We thus build a formula with a more tight
  integration of $\prolog{prm(lemma_2)}$ and $\prolog{prm(coro)}$:
*/

def(pre_thm_3_tight(V)) ::
lemma_2(V),
(world(V) -> ex(W, (r(V,W),
		    ex(X, (e(W,X), g(W,X))),
		    all(W1, (r(W,W1) -> ex(Y, (e(W1,Y), g(W1,Y)))))))).

/**
  It satisfies our basic requirements:

  \smallskip
  */

:- ppl_printtime(ppl_valid(pre_thm_3(v) ->
			   pre_thm_3_tight(v), [mace=false])).
:- ppl_printtime(ppl_valid((frame_cond_simp ->
			    (pre_thm_3_tight(v) -> thm_3(v))), [mace=false])).

/**
  \medskip \noindent And, it permits elimination. Since the result formula
  obtained from the elimination procedure looks clumsy, here it is simplified
  ``by hand'' and mechanically verified:
*/


def(frame_cond_tight) ::
all(v,
    (world(v) ->
     all(w,
	 (r(v, w), ~(w = v), ex(x, e(w, x)) ->
	  all(w1,
	      (r(v, w1), ~(w = w1) ->
		ex(w2,  (r(w, w2), (ex(v, e(w2, v))->(w1=w2;v=w2)))))))))).

:- ppl_printtime(ppl_valid(frame_cond_tight <->
			   all2(g, all(v, (pre_thm_3_tight(v) ->
					   thm_3(v)))),
			   [elim=true, split_iff=true, mace=false])).
/**
  
  \medskip\noindent Under the preconditions $\prolog{prm(r_world)}$, and
  $\prolog{prm(nonempty)}$, which expresses that all worlds have a nonempty
  domain, the frame condition $\prolog{prm(frame_cond_tight)}$ is equivalent
  to the frame condition $\prolog{prm(frame_cond_simp)}$
  (Macro~\ref{def:frame:cond:simp}) of the simplified scenario:
  
*/

def(nonempty) ::
all(v, (world(v) -> ex(x, e(v,x)))).

:- ppl_printtime(ppl_valid((r_world, nonempty ->
			    (frame_cond_tight <-> frame_cond_simp)),
			   [split_iff=true, mace=false])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/** \section{Auxiliary Definitions}
  \label{sec-aux}
*/

def(last_result) :: X ::- last_ppl_result(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
\bibliographystyle{alpha}
\bibliography{\jobname}
*/

latex_custom(X) :-
	X =
	['\\newcommand{\\name}[1]{\\textit{#1}}',
	 '\\newcommand{\\g}[1]{\\mathit{#1}}',
	 '\\newcommand{\\quoted}[1]{\\ulcorner #1\\urcorner}',
	 '\\newcommand{\\highlightpar}{\\noindent\\raisebox{0.2ex}[0pt][0pt]{$\\blacktriangleright\\hspace{0.8em}$}}',
	 '\\renewcommand{\\theequation}{\\roman{equation}}'
	].

latex_biblio(X) :-
	X = '@book{benthem:mlcl,
  author =        {Johan van Benthem},
  title =         {Modal Logic and Classical Logic},
  publisher =     {Bibliopolis},
  year =          {1983},
}

@book{benthem:open,
  author =        {Johan van Benthem},
  title =         {Modal Logic for Open Minds},
  publisher =     {CSLI Publications},
  year =          {2010},
}

@book{fitting:god,
  author =        {Melvin Fitting},
  title =         {Types, Tableaus, and Gödel\'s God},
  publisher =     {Springer},
  year =          {2002}
}

@book{sobel:theism,
  author =        {Jordan Howard Sobel},
  title =         {Logic and Theism},
  year =          {2004},
  publisher =     {Cambridge University Press},
  address =       {Cambridge}
}

@incollection{sobel:1987:goedel,
  author =        {Jordan Howard Sobel},
  title =         {Gödel\'s Ontological Proof},
  year =          {1987},
  booktitle =     {On Being and Saying: Essays for Richard Cartwright},
  editor  =       {Judith Jarvis Thomson},
  publisher  =    {MIT Press},	  
  address =       {Cambridge, MA}
}
  
@inproceedings{benzmueller:etal:2014:goedel,
  author    = {Christoph Benzmüller and Bruno {Woltzenlogel Paleo}},
  title     = {Automating {G}ödel\'s Ontological Proof of God\'s Existence
               with Higher-order Automated Theorem Provers},
  booktitle = {ECAI 2014},
  series    = {FAIA},
  volume    = {263},	   
  pages     = {93--98},
  year      = {2014},
  publisher = {IOS Press},
}

@article{benzmueller:etal:2017:assisted,
  author    = {Christoph Benzmüller and
               Leon Weber and
               Bruno {Woltzenlogel Paleo}},
  title     = {Computer-Assisted Analysis of the {A}nderson-{H}ájek
                  Ontological Controversy},
  journal   = {Logica Universalis},
  volume    = {11},
  number    = {1},
  pages     = {139--151},
  year      = {2017},
}

@article{kirchner:etal:2019:metaphysics,
  author = {Daniel Kirchner and Christoph Benzmüller and Edward N. Zalta},
  year = {2019},
  title = {Computer Science and Metaphysics: A Cross-Fertilization},
  note = {Preprint \\url{http://doi.org/10.13140/RG.2.2.25229.18403}}
}

@article{dls,
  author =        {Patrick Doherty and Witold {\\L}ukaszewicz and
	           Andrzej Sza{\\l}as},
  journal =       {JAR},
  number =        {3},
  pages =         {297--338},
  title =         {Computing Circumscription Revisited: A Reduction
                   Algorithm},
  volume =        {18},
  year =          {1997},
}

@inproceedings{dls:snc,
  author =        {Patrick Doherty and Witold {\\L}ukaszewicz and
	           Andrzej Sza{\\l}as},
  booktitle =     {IJCAI-01},
  pages =         {145--151},
  publisher =     {Morgan Kaufmann},
  title =         {Computing Strongest Necessary and Weakest Sufficient
                   Conditions of First-Order Formulas},
  year =          {2001},
}

@book{soqe:book,
  author =        {Dov M. Gabbay and Renate A. Schmidt and Andrzej Sza{\\l}as},
  title =         {Second-Order Quantifier Elimination:
                   Foundations, Computational Aspects and Applications},
  publisher =     {College Publications},
  year =          {2008},
}

@article{lin:snc,
  author =        {Fangzhen Lin},
  journal =       {Artificial Intelligence},
  pages =         {143--159},
  title =         {On Strongest Necessary and Weakest Sufficient
                   Conditions},
  volume =        {128},
  year =          {2001},
}

@article{cw:2012:projcirc,
  author =        {Christoph Wernhard},
  journal =       {Journal of Symbolic Computation},
  pages =         {1089--1108},
  title =         {Projection and Scope-Determined Circumscription},
  volume =        {47},
  year =          {2012},
}

@inproceedings{cw:2016:pie,
  author    = {Christoph Wernhard},
  title     = {The {PIE} system for Proving, Interpolating
               and Eliminating},
  booktitle = {PAAR 2016},
  year      = {2016},
  volume = {1635},
  pages = {125--138},
  series = {CEUR Workshop Proceedings},
  publisher = {CEUR-WS.org}
}'
.

  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ppl_set_source.
:- mac_install.
