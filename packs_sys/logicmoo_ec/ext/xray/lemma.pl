%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 24/04/95   File: lemma.pl                     %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 04/04/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemma.pl                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(dynamic_lemma/3).
:- dynamic(static_lemma/3).

:- dynamic(lemma_handling_flag/0).

:- dynamic(lemma_mode_parameter/1).
:- dynamic(lemma_format_parameter/1).
:- dynamic(lemma_type_parameter/1).

%%% ----------------------------------------------------------------------
%%% LEMMA CONFIGURATION

lemma_configuration :-
	nl,write('LEMMA CONFIGURATION:'),nl,nl,
	
	write("Lemma handling"),
	write(' = '),
	(lemma_handling_flag ->
	    write(on),
	    nl,nl,
	
	    write(lemma_format),
	    write(' = '),
	    (lemma_format_parameter(P), write(P),write(' '),fail ; write('.')),
	    nl,
	    
	    write(lemma_mode),
	    write(' = '),
	    (lemma_mode_parameter(M),   write(M),write(' '),fail ; write('.')),
	    nl,
	    
	    write(lemma_type),
	    write(' = '),
	    (lemma_type_parameter(T),   write(T),write(' '),fail ; write('.')),
	    nl;
	%true ->
	    write(off)),
	nl.

%%% Lemma handling is turned on by lemma_handling,
%%% off by no_lemma_handling.

lemma_handling :-                          % enable lemma handling
        retract(lemma_handling_flag),
        fail.
lemma_handling :-
        assert(lemma_handling_flag).

no_lemma_handling :-                       % disable lemma handling
        retract(lemma_handling_flag),
        fail.
no_lemma_handling.

%%% Lemma mode indicates the usage of dynamic, static
%%% or dystatic modes of lemmas
%%% depending on lemma_mode_parameter(dynamic),lemma_mode_parameter(static), 
%%% and lemma_mode_parameter(dystatic)

lemma_mode(dynamic) :-                          % enable DYNAMIC lemmas
        retract(lemma_mode_parameter(_)),
        fail.
lemma_mode(dynamic) :-
        assert(lemma_mode_parameter(dynamic)).

lemma_mode(dystatic) :-                         % enable DYSTATIC  lemmas
        retract(lemma_mode_parameter(_)),
        fail.
lemma_mode(dystatic) :-
        assert(lemma_mode_parameter(dystatic)).

lemma_mode(static) :-                           % enable STATIC  lemmas
        retract(lemma_mode_parameter(_)),
        fail.
lemma_mode(static) :-
        assert(lemma_mode_parameter(static)).

%%% Dynamic lemma handling is alternatively turned on by dynamic_lemmas,
%%% off by no_dynamic_lemmas.

dynamic_lemmas :- lemma_mode(dynamic).                       % enable  DYNAMIC lemma handling

add_dynamic_lemmas :-                                        % add     DYNAMIC lemma handling
        retract(lemma_mode_parameter(dynamic)),
        fail.
add_dynamic_lemmas :-
        assert(lemma_mode_parameter(dynamic)).

no_dynamic_lemmas :-                                         % disable DYNAMIC lemma handling
        retract(lemma_mode_parameter(dynamic)),
        fail.
no_dynamic_lemmas.

%%% Static lemma handling is alternatively turned on by static_lemmas,
%%% off by no_static_lemmas.

static_lemmas :- lemma_mode(static).                        % enable  STATIC lemma handling

add_static_lemmas :-                                        % add     STATIC lemma handling
        retract(lemma_mode_parameter(static)),
        fail.
add_static_lemmas :-
        assert(lemma_mode_parameter(static)).

no_static_lemmas :-                                         % disable STATIC lemma handling
        retract(lemma_mode_parameter(static)),
        fail.
no_static_lemmas.

%%% Dystatic lemma handling is alternatively turned on by dystatic_lemmas,
%%% off by no_dystatic_lemmas.
%%% dystatic lemmas are static lemmas stemming from dynamic ones

dystatic_lemmas :- lemma_mode(dystatic).                    % enable  DYSTATIC lemma handling

add_dystatic_lemmas :-                                      % add     DYSTATIC lemma handling
        retract(lemma_mode_parameter(dystatic)),
        fail.
add_dystatic_lemmas :-
        assert(lemma_mode_parameter(dystatic)).

no_dystatic_lemmas :-                                       % disable DYSTATIC lemma handling
        retract(lemma_mode_parameter(dystatic)),
        fail.
no_dystatic_lemmas.

%%% some macros for easier lemma configuration.

lemma_flag :- lemma_mode_parameter(X).
dynamic_lemma_flag  :- lemma_mode_parameter(dynamic).
static_lemma_flag   :- lemma_mode_parameter(static).
dystatic_lemma_flag :- lemma_mode_parameter(dystatic).

no_lemmas :- no_dynamic_lemmas,no_static_lemmas,no_dystatic_lemmas.

%%% Lemma type indicates the location of the lemmatization predicates
%%%   DELTA lemmas are attached to rules stemming from default rules
%%%                ( Head =.. [gamma|-] or infer_by(default(_)) )
%%%   OMEGA lemmas are attached to rules stemming from classical rules
%%%                ( infer_by(extension(_) )
%%% depends on lemma_type_parameter(delta),lemma_type_parameter(omega), 
%%% and lemma_type_parameter(all)

lemma_type(delta) :-                          % enable DELTA lemmas
        retract(lemma_type_parameter(_)),
        fail.
lemma_type(delta) :-
        assert(lemma_type_parameter(delta)).

lemma_type(omega) :-                          % enable OMEGA  lemmas
        retract(lemma_type_parameter(_)),
        fail.
lemma_type(omega) :-
        assert(lemma_type_parameter(omega)).

lemma_type(all) :-                            % enable ALL  lemmas
        retract(lemma_type_parameter(_)),
        fail.
lemma_type(all) :-
        assert(lemma_type_parameter(delta)),
        assert(lemma_type_parameter(omega)).

%%% Delta lemma handling is alternatively added  by delta_lemmas,
%%% disabled by no_delta_lemmas.

add_delta_lemmas :-                                        % add     DELTA lemma handling
        retract(lemma_type_parameter(delta)),
        fail.
add_delta_lemmas :-
        assert(lemma_type_parameter(delta)).

no_delta_lemmas :-                                         % disable DELTA lemma handling
        retract(lemma_type_parameter(delta)),
        fail.
no_delta_lemmas.

%%% Omega lemma handling is alternatively added  by omega_lemmas,
%%% disabled by no_omega_lemmas.

add_omega_lemmas :-                                        % add     OMEGA lemma handling
        retract(lemma_type_parameter(omega)),
        fail.
add_omega_lemmas :-
        assert(lemma_type_parameter(omega)).

no_omega_lemmas :-                                         % disable OMEGA lemma handling
        retract(lemma_type_parameter(omega)),
        fail.
no_omega_lemmas.

%%% Lemma format indicates the usage of unit or disjunctive lemmas
%%% depending on lemma_format_parameter(unit) and lemma_format_parameter(disj)

lemma_format(unit) :-                          % enable UNIT lemmas
        retract(lemma_format_parameter(_)),
        fail.
lemma_format(unit) :-
        assert(lemma_format_parameter(unit)).

lemma_format(disj) :-                          % enable DISJUNCTIVE  lemmas
        retract(lemma_format_parameter(_)),
        fail.
lemma_format(disj) :-
        assert(lemma_format_parameter(disj)).

%%% SETTINGS for LEMMA HANDLING
%%%

:- compile(lemma_config).

%%% ----------------------------------------------------------------------
%%% LEMMA GENERATION during COMPILATION

%%% Add extra arguments to each goal so that information
%%% on what inferences were made in the proof can be printed
%%% at the end.

add_lemmatization((Head :- Body),(Head1 :- Body1)) :-
	lemma_handling_flag,
	!,
        Head =.. L,
        append(L,[ProofOut,ProofEnd],L1),
        Head1 =.. L1,
        add_lemmatization_args(Body,Proof,ProofEnd,Body2,Lemma),
        (Lemma = true ->
	    Body1 = Body2,
	    ProofOut = Proof;
	 add_lemmatization_p(Head :- Body) ->
	    Lemmatization =.. [lemmatize,Lemma,Proof,ProofOut,ProofEnd],
	    conjoin(Body2,Lemmatization,Body1),
	    verbose("Lemmatization ":Lemma);
        %true ->
             Body1 = Body2,
             ProofOut = Proof).
add_lemmatization((Head :- Body),(Head :- Body)).

add_lemmatization_p(Head :- Body) :-
	lemma_flag,
	!,
	(functor(Head,query,_) -> fail;
         functor(Head,alpha,_) -> fail;
	 functor(Head,gamma,_) -> lemma_type_parameter(delta);
	 true ->                  lemma_type_parameter(omega)).
         
add_lemmatization_args(Body,Proof,ProofEnd,Body1,Lemma) :-
        Body = (A , B) ->
                add_lemmatization_args(A,Proof,Proof1,A1,L1),
                add_lemmatization_args(B,Proof1,ProofEnd,B1,L2),
                conjoin(A1,B1,Body1),
                conjoin(L1,L2,Lemma);
        Body = (A ; B) ->
                add_lemmatization_args(A,Proof,ProofEnd,A1,L1),
                add_lemmatization_args(B,Proof,ProofEnd,B1,L2),
                disjoin(A1,B1,Body1),
                conjoin(L1,L2,Lemma);
        Body = infer_by(X) ->
	        add_lemmatization_inference(X,Proof,ProofEnd,Record,Lemma),
		conjoin(Body,Record,Body1);
        Body =.. [search,Goal|L] ->
                add_lemmatization_args(Goal,Proof,ProofEnd,Goal1),
                Body1 =.. [search,Goal1|L],
                Lemma = true;
        Body = fail ->
                Body1 = Body,
                Lemma = true;
        builtin(Body) ->
                Proof = ProofEnd,
                Body1 = Body,
                Lemma = true;
        %true ->
                Body =.. L,
                append(L,[Proof,ProofEnd],L1),
                Body1 =.. L1,
                Lemma = true.

add_lemmatization_inference(Inference,Proof,ProofEnd,Record,Lemma) :-
	Inference = reduction(_) ->
	    /* ancestor test */
	    Lemma = true,
            (lemma_type_parameter(omega) ->
		Record = (Proof = [Inference|ProofEnd]);
	    %true ->
		Proof = ProofEnd,
		Record = true);
        Inference = extension(_) ->
            /* omega rule */
	    Proof = ProofEnd,
	    Record = true,
	    (lemma_type_parameter(omega) ->
		Lemma = Inference;
	    %true ->
		Lemma = true);
	Inference = default(_) ->
	    /* delta rule */
	    Lemma = Inference,
	    ((static_lemma_flag;dystatic_lemma_flag) ->
		Record = (Proof = [Inference|ProofEnd]);
	    %true ->
		Proof = ProofEnd,
		Record =true);
	Inference = static_lemma(_) ->
	    /* static lemma test (implicit (dy)static_lemma_flag)*/
	    Record = (Proof = [Inference|ProofEnd]),
	    Lemma = true;
	Inference = dynamic_lemma(_) ->
	    /* dynamic lemma test */
	    Lemma = true,
	    ((static_lemma_flag;dystatic_lemma_flag) ->
		Record = (Proof = [Inference|ProofEnd]);
	    %true ->
		Proof = ProofEnd,
		Record =true);
	%true ->
            /* unit. etc */
	    Proof = ProofEnd,
	    Record = true,
	    Lemma = true.


lemma_tests_p(P,N) :-
	lemma_handling_flag, (dynamic_lemma_test_p(P,N) ; static_lemma_test_p(P,N)).

dynamic_lemma_test_p(P,N) :-
	(dynamic_lemma_flag;dystatic_lemma_flag),
	!,
	(P == query -> fail;
         P == alpha -> fail;
	 P == gamma -> lemma_type_parameter(delta);
	 true       -> lemma_type_parameter(omega)).

static_lemma_test_p(P,N) :-
	(static_lemma_flag;dystatic_lemma_flag),
	!,
	(P == query -> fail;
         P == alpha -> fail;
	 P == gamma -> lemma_type_parameter(delta);
	 true       -> lemma_type_parameter(omega)).

lemma_tests(P,N,Result) :-
	lemma_handling_flag,
	lemma_tests_p(P,N),     % for avoiding problems with query/0
	!,
	N3 is N - 3,            % N - 3 due to 3 ancestor-lists
	functor(Head3,P,N3),
	Head3 =.. [P|Args1],
	append(Args1,[_,_,_],Args),
	Head =.. [P|Args],
	
	(dynamic_lemma_test_p(P,N) ->
	    dynamic_lemma_test(Head,Head3,DynamicLemmaBody);
	%true ->
	    DynamicLemmaBody=true),

	(static_lemma_test_p(P,N) ->
	    static_lemma_test(Head,Head3,StaticLemmaBody);
	%true ->
	    StaticLemmaBody=true),

	conjoin(DynamicLemmaBody,StaticLemmaBody,Result).
lemma_tests(_,_,true).

dynamic_lemma_test(Head,Head3,Test) :-
	lemma_format_parameter(unit) ->
	     ((static_lemma_flag;dystatic_lemma_flag) ->
		 Body = (infer_by(dynamic_lemma(Head3:Assumptions)),
		         dynamic_lemma(Head3,false,Assumptions)) ;
	     %true ->
        	 Body = (infer_by(dynamic_lemma(Head3)),
		         dynamic_lemma(Head3,false,[]))),
	     Test = (Head :- Body,!); /* REQUIRES FULLY INSTANTIIATED SUBGOALS */
	lemma_format_parameter(disj) ->
	%true              ->
	        not_yet_implemented.
		          
static_lemma_test(Head,Head3,Test) :-
	lemma_format_parameter(unit) ->
        	Body = (infer_by(static_lemma(Head3:Assumptions)),
		        static_lemma(Head3,false,Assumptions),
			justification(Assumptions)),
		Test = (Head :- Body);
	lemma_format_parameter(disj) ->
	%true              ->
	        not_yet_implemented.

%%% ----------------------------------------------------------------------
%%% LEMMA UTILIZATION during RUN-TIME

lemmatize(default(N:(Gamma :- _)),Proof,ProofOut,ProofEnd) :-
	verbose("Using static lemma code: ":lemmatize),
        !,
        remove_reductions(Proof,ProofOut),
        default_assumptions(ProofOut,ProofEnd,Ass),
        lemmatize_dynamically(gamma(N,Gamma),false,Ass).
%        lemmatize_statically(gamma(N,Gamma),false,Ass).
lemmatize(extension(_N:Goal),Proof,ProofOut,ProofEnd) :-
	verbose("Using static lemma code: ":lemmatize),
        !,
        skim_reductions(Goal,Proof,ProofOut,ProofEnd,Ancs),
        default_assumptions(ProofOut,ProofEnd,Ass),
        lemmatize_dynamically(Goal,Ancs,Ass).
%        lemmatize_statically(Goal,Ancs,Ass).
lemmatize(_Lemmatization,_Proof,_ProofOut,_ProofEnd) :-
        error_in_lemmatize.

lemmatize_statically(Goal,Ancestors,Assumptions) :-
        static_lemma(Goal,Ancestors,Assumptions),            % UNIFY, ==, ... ???
        !.
lemmatize_statically(Goal,Ancestors,Assumptions) :-
        assert(static_lemma(Goal,Ancestors,Assumptions)),
        verbose(static_lemma(Goal,Ancestors,Assumptions)).

lemmatize_dynamically(Goal,Ancestors,_) :-
        dynamic_lemma(Goal,Ancestors,_),                        % UNIFY, ==, ... ???
        !.
%lemmatize_dynamically(Goal,Ancestors,[]) :-               <=== only if cases below
%        !,
%        assert(dynamic_lemma(Goal,Ancestors,[])),
%        lemmatize_statically(Goal,Ancestors,[]),
%        verbose(dynamic_lemma(Goal,Ancestors)).
lemmatize_dynamically(Goal,Ancestors,Assumptions) :-
        (assert(dynamic_lemma(Goal,Ancestors,Assumptions)),
         verbose(activated:(dynamic_lemma(Goal,Ancestors,[Assumptions])));
         retract(dynamic_lemma(Goal,Ancestors,Assumptions)),
         verbose(deactivated:(dynamic_lemma(Goal,Ancestors,[Assumptions]))),
%         lemmatize_statically(Goal,Ancestors,Assumptions), <=== only if cases below
         !,
         fail).

% No check for ProofEnd, since default(...) should be contained
% Because only invoked in case of default inference

remove_reductions([default(X)|Proof],[default(X)|Proof]) :- 
        !.
remove_reductions([static_lemma(X)|Proof],[static_lemma(X)|ProofOut]) :- 
        !,
        remove_reductions(Proof,ProofOut).
remove_reductions([dynamic_lemma(X)|Proof],[dynamic_lemma(X)|ProofOut]) :- 
	/* ==> when using static lemmas <== */
        !,
        remove_reductions(Proof,ProofOut).
remove_reductions(Proof,ProofOut) :-
        Proof = [_|RestProof],
        remove_reductions(RestProof,ProofOut).

skim_reductions(_Goal,Proof,Proof,ProofEnd,false) :-
        Proof == ProofEnd,
        !.
skim_reductions(_Goal,Proof,Proof,_ProofEnd,false) :-
        Proof = [default(_)|_],
        !.
skim_reductions(Goal,[reduction(Anc)|Proof],Proof1,ProofEnd,Ancs) :-
        Goal == Anc,
        skim_reductions(Goal,Proof,Proof1,ProofEnd,Ancs),
        !.
skim_reductions(Goal,[reduction(Anc)|Proof],[reduction(Anc)|Proof1],ProofEnd,Ancs1) :-
        !,
        skim_reductions(Goal,Proof,Proof1,ProofEnd,Ancs),
        disjoin1(Anc,Ancs,Ancs1).
skim_reductions(Goal,[static_lemma(X)|Proof],[static_lemma(X)|Proof1],ProofEnd,Ancs) :-
        skim_reductions(Goal,Proof,Proof1,ProofEnd,Ancs).
skim_reductions(Goal,[dynamic_lemma(X)|Proof],[dynamic_lemma(X)|Proof1],ProofEnd,Ancs) :-
	/* ==> when using static lemmas <== */
        skim_reductions(Goal,Proof,Proof1,ProofEnd,Ancs).

%%% ----------------------------------------------------------------------
%%% default_assumptions/3 
%%%         extracts all neccessary default assunptions from a proof
%%%

default_assumptions(Proof,ProofEnd,[]) :-
        Proof == ProofEnd,
        !.
default_assumptions([default(_N:(_ :- _ : Just))|Proof],ProofEnd,Assumptions) :-
        !,
        default_assumptions(Proof,ProofEnd,Justs),
        combine_clauses(Just,Justs,Assumptions).
default_assumptions([static_lemma(_  : Just)|Proof],ProofEnd,Assumptions) :-
        !,
        default_assumptions(Proof,ProofEnd,Justs),
        combine_clauses(Just,Justs,Assumptions).
default_assumptions([dynamic_lemma(_ : Just)|Proof],ProofEnd,Assumptions) :-
	/* ==> when using static lemmas <== */
        !,
        default_assumptions(Proof,ProofEnd,Justs),
        combine_clauses(Just,Justs,Assumptions).
default_assumptions([_|Proof],ProofEnd,Wff) :-
        default_assumptions(Proof,ProofEnd,Wff).

%%% A is supposed to be a literal
%%% disjoin2 removes A from B; results in C

disjoin1(A,B,C) :-
        disjoin2(A,B,C1),
        disjoin(A,C1,C).

disjoin2(A,(B ; C),D) :-
        !,
        disjoin2(A,B,B1),
        disjoin2(A,C,C1),
        disjoin(B1,C1,D).
disjoin2(A,B,false) :-
        A == B,
        !.
disjoin2(_,B,B).


%%% ----------------------------------------------------------------------
%%% SOME helpful IO-Routines

write_lemmas(File) :-   
        concatenate(File,'.lem',LFile),
        open(LFile,write,LStream),
        !,
        (static_lemma(X,Y,Z),
         write_clauses(LStream,static_lemma(X,Y,Z)),
         fail;
         close(LStream)).

show_lemmas :-  
        show_dynamic_lemmas,fail;
        nl,show_static_lemmas.

show_dynamic_lemmas :- 
        (dynamic_lemma(X,Y,Z),
         write(dynamic_lemma(X,Y,Z)),
         nl,
         fail;
         true).
show_static_lemmas :-
        (static_lemma(X,Y,Z),
         write(static_lemma(X,Y,Z)),
         nl,
         fail;
         true).

remove_lemmas :-
        remove_static_lemmas,
        remove_dynamic_lemmas.
remove_dynamic_lemmas :-
        retract_all(dynamic_lemma(_,_,_)).
remove_static_lemmas :-
        retract_all(static_lemma(_,_,_)).
