%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: tom_constraints.pl,v 1.3 1994/05/31 20:03:48 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\chapter
[Die Datei {\tt tom\_constraints}]
{Die Datei {\Huge \tt tom\_constraints}}

This file contains the particular predicates for the approach using
constraints and constraint handling.

\PL*/
write_matrix(OutStream):-
	is_option('Tom:special_path_term',Value),
	( Value = off ->
	    true
	; concat_atom([Value,'.pl'],FullFileName),
	  compile(FullFileName)
	),
	!,
	( constraint(_AnyType, AFormula, matrix),
	  ( Value = off ->
	      NormalizedFormula = AFormula
	  ; Call_Normal_Formula =.. [Value, AFormula, NormalizedFormula],
	    call(Call_Normal_Formula)
	  ),
	  write_one_clause(OutStream,NormalizedFormula),
	  fail
	; true
	).

write_extras(OutStream):-
	check_options(OutStream),
	writeclause(OutStream,# begin(constraint_theory)),
	!,
	( setof(Type,B ^ A ^ constraint(Type,A,B),ListOfTypes)
	; true
	),
	( member((Functor / Arity),ListOfTypes),
	  ( is_option('Tom:theory_optimization') ->
	      add_driver_clause(OutStream,Functor,Arity)
	  ; true
	  ),
	  constraint((Functor / Arity),AxiomClause,Sort),
	  ( Sort = transitive ->
	      optimize_transitive(AxiomClause,OptimizedAxiom)
	  ; Sort = interaction ->
	      optimize_interaction(AxiomClause,OptimizedAxiom)
	  ; is_option('Tom:theory_optimization') ->
	      optimize_axiom(AxiomClause,OptimizedAxiom)
	  ; OptimizedAxiom = AxiomClause
	  ),
	  writeclause(OutStream, OptimizedAxiom),
	  fail
	; true
	),
	( is_option('Tom:theory_optimization') ->
	    writeclause(OutStream,
	           {check_theory_depth(Depth,NewDepth):-  
	                  ( (Depth =< 0) ->
			     setval(depthq,1),
			     fail
			  ; NewDepth is Depth-1)})
	; true
	),
	writeclause(OutStream,# end(constraint_theory)),
	is_option('Tom:special_unification',Value),
	( Value = off ->
	    true
	; write_descriptors(Value)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate write_one_clause/2 (+OutStream, +Clause).

This predicate organises the output of one clause |Clause| to the
output stream |OutStream|. We distinguish two cases here:
\begin{enumerate}
\item The usual case is an implication. As we are
dealing with some kind of Prolog notation and we want to write
implications, we write the conclusions at first followed by the
premises and finally the constraints. 
However, before we can print out the literals, we reorder the formula.
Any of the lists of literals (|Conclusions|, |Premises|, |Constraints|) may
be empty and the separators for formatting if printed depending on these lists.

We were not able to use the built-in predicate |writeclause| because it doesn't
know the operator |//:|.
\end{enumerate}
\PL*/
write_one_clause(Stream,(Conclusion :- Premise)):-
	!,
	reorder((Conclusion :- Premise),Premises,Conclusions,Constraints),
	write_literals(Stream,Conclusions,";"),
	( Premises = [] -> true ; printf(Stream," :- ",[])),
	write_literals(Stream,Premises,","),
	( Constraints = [] -> true ; printf(Stream,"  //: ",[])),
	write_literals(Stream,Constraints,","),
	printf(Stream,".\n",[]).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{enumerate}
\addtocounter{enumi}{1}
\item The second clause of this predicate is treating the ordinary formulae. They
are translated into a Prolog format and printed using |writeclause|. We can
be sure that if the formula is not an implication that there won't be any
constraint predicates because they are introduced in an implication
or a conjunction only. However, implications are treated by the first clause
of the predicate and a conjunct with an auxiliary predicate has been put in
the constraint theory part.
\end{enumerate}

\PL*/

write_one_clause(Stream,Formula):-
	build_clause(Formula,PrologClause),
	writeclause(Stream,PrologClause).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reorder/4 (+Formula, -Premises, -Conclusions, -Constraints).

This predicate examines the formula tree and collects the literals
into the three lists |Premises|, |Conclusions|, and |Constraints|.

The formula |Formula| is assumed to be an implication. At first, the premise
is treated and the conclusion afterwards.

\PL*/
reorder((Conclusion :- Premise),Premises,Conclusions,Constraints):-
	reorder(Premise,[],[],Premises,NewConstraints,p),
	reorder(Conclusion,[],NewConstraints,Conclusions,Constraints,c).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reorder/6 (+Formula, +InLiterals, +InConstraints,
	              -OutLiterals, -OutConstraints, +Indicator).

This predicates reorders formulae which are not implications. Due to the
construction of the algorithm, they are basically literals, conjunctions,
or disjunctions. The last two cases are covered by the first clause.
In case of disjunction and conjunction, we simply proceed on the disjuncts
and conjuncts respectively.

If we have a negated formula, this formula must be a literal (we are working
with a negation normal form). Auxiliary predicates cannot be negated, hence,
we simply chuck it in the list of literals.

In case we are dealing with a literal, it goes either into the list of literals
or into the list of constraints. If it goes into the list of constraints, we
have to take care of whether we are dealing with a premise or a conclusion
of an implication. Depending on that fact, we negate the literal or we don't.
For determining the fact just mentioned, we have introduced the additional
argument |Indicator| which can only take the values |p| and |c|, for premise
and conclusion respectively.

\PL*/
reorder((P1 , P2),InLiterals,InConstraints,
	             OutLiterals,OutConstraints,Indicator):-
	!,
	reorder(P1,InLiterals,InConstraints,
	            NewLiterals,NewConstraints,Indicator),
	reorder(P2,NewLiterals,NewConstraints,
	            OutLiterals,OutConstraints,Indicator).

reorder((P1 ; P2),InLiterals,InConstraints,
	             OutLiterals,OutConstraints,Indicator):-
	!,
	reorder(P1,InLiterals,InConstraints,
	            NewLiterals,NewConstraints,Indicator),
	reorder(P2,NewLiterals,NewConstraints,
	            OutLiterals,OutConstraints,Indicator).

reorder(-(Formula), InLiterals, InConstraints, [- Formula | InLiterals],
	  InConstraints,_):-!.

reorder(Formula,InLiterals,InConstraints,
	             OutLiterals,OutConstraints,Indicator):-
	Formula =.. [Functor | _Arguments],
	( name(Functor,[36, 82, 109, 111, 100 | _]) ->
	    ( Indicator = 'p' ->
		OutConstraints = [ Formula | InConstraints ]
	    ; OutConstraints = [ - Formula | InConstraints ]
	    ),
	    OutLiterals = InLiterals
	; OutConstraints = InConstraints,
	  OutLiterals = [ Formula | InLiterals]
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate write_literals/3 (+Stream, +ListOfLiterals, +Separator).

This predicate prints the literals from |ListOfLiterals| to |Stream|,
separating two of those by |Separator|. The predicate |printf| is used
for printing --- quick and dirty (well, kind of).

\PL*/

write_literals(_Stream,[],_Separator).

write_literals(Stream,[H | T],Separator):-
	printf(Stream,"%vDQMw ",[H]),
	( T = [] ->
	  true
	; printf(Stream," %w ",[Separator]),
	  write_literals(Stream,T,Separator)
	).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate optimize_axiom/2 (+InAxiom, -OutAxiom).

The optimization of any axiom basically means to append another argument to
each literal. This new argument expresses the depth of the theory inferences.
In order to do this, we analyse the structure of the clause to be
optimized. On the literal level, we add another argument to the
predicate symbol.

This predicate simply generates the additional argument and the following
predicate |optimize_axiom/3| is called.
\PL*/

optimize_axiom((Head :- Body),(OptHead :- OptBody)):-
	!,
	optimize_axiom(Head,OptHead,Depth),
	optimize_axiom(Body,OptBody,Depth).

optimize_axiom(Literal, OptLiteral):-
	Literal =.. [ Functor | Arguments ],
	append(Arguments, [_Depth], NewArguments),
	OptLiteral =.. [ Functor | NewArguments ].

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate optimize_axiom/3 (+InAxiom, -OutAxiom, +Depth).

Here, we get down to the literal level and increase the arity of the
predicate symbol by one.

\PL*/

optimize_axiom((L1 ; L2),(OL1 ; OL2),Depth):-
	!,
	optimize_axiom(L1, OL1, Depth),
	optimize_axiom(L2, OL2, Depth).

optimize_axiom((L1 , L2),(OL1 , OL2), Depth):-
	!,
	optimize_axiom(L1, OL1, Depth),
	optimize_axiom(L2, OL2, Depth).

optimize_axiom( -(Literal), -(OptLiteral), Depth):-
	!,
	optimize_axiom(Literal, OptLiteral, Depth).

optimize_axiom(Literal, OptLiteral, Depth):-
	Literal =.. [ Functor | Arguments ],
	append(Arguments, [Depth], NewArguments),
	OptLiteral =.. [ Functor | NewArguments ].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate optimize_transitive/2 (+InAxiom, -OutAxiom).

The axiom for transitivity has to be optimised in a special way. Most
other axioms are not self recursive, while the transitivity is. Hence,
we have to control the depth of inferences in the theory. This has been done
by including another predicate, into the optimised
axiom. This additional predicate is called |check_theory_depth|.
The Dollar sign has to be used because it will be stripped off
from the filter to follow (|constraints.pl|).

\PL*/

optimize_transitive((H :- C1,C2),
	            (OHead :- { check_theory_depth(X,Y),
		                nonvar(Argument),
		                Unification},
			      OC1, OC2)):-
	H =.. [HFunctor | [HArguments]],
	OHead =.. [HFunctor, Argument, X],
	C1 =.. C1List,
	append(C1List, [Y], NewC1),  
	OC1 =.. NewC1,
	C2 =.. C2List,
	append(C2List, [Y], NewC2),
	OC2 =.. NewC2,
	is_option('Tom:special_unification', Unificator),
	( Unificator = off ->
	    Unification = (Argument =  HArguments)
	; Unification =.. [Unificator, Argument, HArguments]
	).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate optimize_interaction/2 (+InAxiom, -OutAxiom).

The axiom for interaction has to be optimised in a special way. We want to
ensure that the A1-unification is carried out on the first arguments only.

\PL*/

optimize_interaction((H :- T),
	            (OHead :- { nonvar(HA),
	                        Unification},
	                      OT)):-
	H =.. [HF, HA],
	OHead =.. [HF, HA, Depth],
	T =.. [TF,  TA],
	OT =.. [TF, TA, Depth],
	is_option('Tom:special_unification', Unificator),
	( Unificator = off ->
	    Unification = (HA = TA)
	; Unification =.. [Unificator, HA, TA]
	).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_driver_clause/3 (+OutStream, +Functor, + Arity).

We add the driver clause for each different predicate symbol. This clause
simply adds another argument to the predicate which unifies with a counter
containing the depth.
\PL*/

add_driver_clause(OutStream, Functor, Arity):-
	functor(Pred1, Functor, Arity),
	Pred1 =.. List,
	append(List, [Val], NewList),
	Pred2 =.. NewList,
	writeclause(OutStream,(Pred1 :- ({getval(current_depth,Val)},
	                                 Pred2))).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate check_options/1 (+Stream).

This predicate simply checks the option |'Tom:theory_optimization'| and
acts accordingly to a simple strategy. If the flag is set to |off| and
there are ``dangerous'' axioms like transitivity, we set the flag to |on|
and proceed. Conversely, if the flag is set to |on| and there are no
self recursive clauses such as transitivity, we set the flag to |off| and
proceed. In all other cases, we simply go on. Thus, this predicate is merely
a case analysis. The messages about flag setting go to the respective streams
as Prolog comments.

\PL*/

check_options(Stream):-
	( ( \+ is_option('Tom:theory_optimization') ,
	     constraint((_AnyPredicate / _AnyArity),_AnyClause,transitive ) ->
	     set_option('Tom:theory_optimization' = on),
	     writeln(Stream,"
% I suggest to optimize the theory - changed the flag to on.")
	  )
	; ( is_option('Tom:theory_optimization') ,
	     \+ constraint((_APredicate / _AnArity) ,_AClause, transitive) ->
	       set_option('Tom:theory_optimization' = off),
	       writeln(Stream,"
% I don't believe it is necessary to optimize the theory - changed the flag to off.")
	  )
	; true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate write_descriptors/1(+Unificator).


\PL*/

write_descriptors(Unificator):-
	open('tom_c_descriptors.pl',write,DStream),
	writeclause(DStream, :- module('tom_c_descriptors')),
	nl(DStream),
	writeclause(DStream, :- compile(library(capri))),
	writeclause(DStream, :- lib(matrix)),
	nl(DStream),
	concat_atom([Unificator,'.pl'],FullName),
	writeclause(DStream,force_option('ProCom::post_link',[[FullName]])),
	writeclause(DStream,force_option('ProCom::path',['path.pl'])),
	writeclause(DStream,force_option(equality,[off])),
	writeclause(DStream,force_option(remove_unreached_clauses,[off])),
	writeclause(DStream,force_option(find_all_connections,[on])),
	writeclause(DStream,force_option(connect_weak_unifyable,[off])),
	writeclause(DStream,force_option(reductions,[[]])),
	writeclause(DStream,force_option(search,[iterative_deepening(1, 1, 1)])),
	nl(DStream),
	writeclause(DStream,(make_pred(A, B, C) :-
	( A =.. [D, E],
	functor(E, F, G),
	functor(H, F, G),
	(
	    D = (--)
	->
	    I = (++)
	;
	    (
		D = (++)
	    ->
		I = (--)
	    )
	),
	C =.. [I, H],
	B =.. [D, H]))),
	nl(DStream),
	writeclause(DStream, look_for_entry(A, B, C, D) :-
	(make_pred(A, B, C),
	'Contrapositive'(C,_, D))),
	nl(DStream),
	Unification1 =.. [Unificator, K, M],
	writeclause(DStream, descriptor((proof(reduction(Index3,K -> L)),
	   template(K, goal),
	   call(make_pred(K,M,L)),
	   template(L, path(Index3)),
	   constructor(Unification1)))),
	nl(DStream),
	Unification2 =.. [Unificator, O, Q],
	writeclause(DStream, descriptor((proof(connection(Index4, O -> P)),
	   template(O, goal),
	   call(look_for_entry(O,Q,P,Index4)),
	   template(P, extension(Index4)),
	   constructor(Unification2)))),
	close(DStream),
	set_option(prover = procom(tom_c_descriptors)).


/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */


