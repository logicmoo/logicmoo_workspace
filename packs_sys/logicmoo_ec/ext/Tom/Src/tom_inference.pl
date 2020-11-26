%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 20/02/95   File: tom_inference.pl             %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 20/02/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Zoltan Rigo                                                      %%
%%                                                                           %%
%% Usage:   prolog tom_inference.pl                                          %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\chapter
[Die Datei {\tt tom\_inference}]
{Die Datei {\Huge \tt tom\_inference}}

This file provides the predicates for the approach that generates inference
rules from certain modal axioms. The first two preciates are the ones
called in |tom.pl|.

\PL*/

write_matrix(OutStream):-
	is_option('Tom:special_path_term',NormalPathPredicate),
	( NormalPathPredicate = off ->
	    true
	; concat_atom([NormalPathPredicate,'.pl'],FullFileName),
	  compile(FullFileName)
	),
	( setof(Types,A ^ constraint(Types,A,matrix),ListOfTypes)
	; writeln(OutStream,"
% The matrix is empty. If I were you, I'd doublecheck what I did.")
        ),
	( member((Functor / Arity),ListOfTypes),
	  constraint((Functor / Arity),AxiomClause,matrix),
	  ( NormalPathPredicate = off ->
	      FinalClause = AxiomClause
	  ; CallPredicate =.. [NormalPathPredicate,AxiomClause,FinalClause],
	    call(CallPredicate)
	  ),
	  writeclause(OutStream,FinalClause),
	  fail
	; true
	).


write_extras(InputFileNameAtom):-
	concat_atom([InputFileNameAtom,'_descriptors'],DescriptorFile),
	open(DescriptorFile,write,DescriptorStream),
	write_general_part(DescriptorStream,DescriptorFile),
	write_query_specific_part(DescriptorStream),
	set_option(prover = procom(DescriptorFile)),
	close(DescriptorStream),
	( setof(AType, A ^ B ^ constraint(AType,A,B), ListOfATypes),
	  set_option('ProCom:extra_procedures' = ListOfATypes )
	; true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate write_general_part/2 (+Stream, +DescriptorFileName).

This predicate writes the general part of a descriptor set to |Stream| which
represents a descriptor file. Because of the \ProCom{} convention, we have
to give the file name in the file itself, therefore, we need the additional
argument.
\PL*/

write_general_part(Stream, DescriptorFileName):-
	writeclause(Stream, :- module(DescriptorFileName)),
	nl(Stream),
	writeclause(Stream, :- compile(library(capri))),
	writeclause(Stream, :- lib(literal)),
	writeclause(Stream, :- lib(matrix)),
	nl(Stream),
	set_options(Stream),
	nl(Stream),
	writeclause(Stream, make_pred(A, B, C) :-
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
	B =.. [D, H])),

	writeclause(Stream, look_for_entry(A, B, C, D) :-
	(make_pred(A, B, C),
	'Contrapositive'(C,_, D))),
	nl(Stream),
	is_option('Tom:special_unification',Unification),
	( Unification = off ->
	    Unifier1 = true
	; Unifier1 =.. [Unification, AAA, CCC]
	),
	writeclause(Stream, descriptor((proof(reduction(Index,AAA -> BDD)),
	   template(AAA, goal),
	   call(make_pred(AAA, CCC, BDD)),
	   template(BDD, path(Index)),
	   constructor(Unifier1)))),
	nl(Stream),
	( Unification = off ->
	    Unifier2 = true
	; Unifier2 =.. [Unification, BBB, DDD]
	),
	writeclause(Stream, descriptor((proof(connection(ADD, BBB -> CDD)),
	   template(BBB, goal),
	   call(look_for_entry(BBB, DDD, CDD, ADD)),
	   constructor(Unifier2),
	   template(CDD, extension(A))))),
	nl(Stream).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate write_query_specific_part/1 (+Stream).

This predicate writes the query specific part to |Stream| which represents a
descriptor file.
\PL*/

write_query_specific_part(Stream):-
	( constraint(_Type, Clause, Sort),
	  write_descriptor(Sort, Clause, Stream),
	  fail
	; true).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate write_descriptor/3 (+Sort, +Clause, +Stream).

This predicate produces the descriptor for |Clause| according to its |Sort|
to |Stream| which represents the descriptor file.
\PL*/

write_descriptor(interaction, (Head :- Body), Stream):-
	functor(Head,HFunctor,Arity),
	functor(NewHead, HFunctor,Arity),
	arg(1, Head, HArgument),
	arg(1, NewHead, NewArgument),
	is_option('Tom:special_unification',Unification),
	( Unification = off ->
	    Unifier = true
	; Unifier =.. [Unification, HArgument, NewArgument]
	),
	nl(Stream),
	writeclause(Stream, descriptor((proof(interaction(Head :- Body)),
	   template(-- (NewHead), goal),
	   constructor((nonvar(NewArgument),
	                Unifier)),
	   template(-- (Body), residue)))).

write_descriptor(transitive, (Head :- (Part1, Part2)), Stream):-
	functor(Head,HFunctor,Arity),
	functor(NewHead, HFunctor,Arity),
	arg(1, Head, HArgument),
	arg(1, NewHead, NewArgument),
	is_option('Tom:special_unification',Unification),
	( Unification = off ->
	    Unifier = true
	; Unifier =.. [Unification, HArgument, NewArgument]
	),
	nl(Stream),
	writeclause(Stream, descriptor((proof(transitive(Head:-(Part1,Part2))),
	   template(-- (NewHead), goal),
	   constructor((nonvar(NewArgument),
	                Unifier)),
	   template(-- (Part1), residue),
	   template(-- (Part2), residue)))).
	
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate set_options/1 (+DescriptorFile).

We compel the descriptors to set the options. Of course, the unification
file is derived from the option |'Tom:special_unification'|. All other options
are found by trial and error. If you don't like them, change them.

They are written to the descriptor file as the user might want to re-use
the descriptor file.

\PL*/

set_options(Stream):-
	is_option('Tom:special_unification',Unification),
	( Unification = off ->
	    true
	; concat_atom([Unification,'.pl'],Unificator),
	  writeclause(Stream,force_option('ProCom::post_link',[[Unificator]]))
	),
	writeclause(Stream,force_option('ProCom::path',['path.pl'])),
	writeclause(Stream,force_option(equality,[off])),
	writeclause(Stream,force_option(remove_unreached_clauses,[off])),
	writeclause(Stream,force_option(find_all_connections,[on])),
	writeclause(Stream,force_option(connect_weak_unifiable,[off])),
	writeclause(Stream,force_option(reductions,[[]])),
	writeclause(Stream,force_option(search,[iterative_deepening(1, 1, 1)])).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */






