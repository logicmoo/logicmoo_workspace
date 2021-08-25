%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 13/07/96   File: lemmaflex.pl
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 11/07/96 Created                                                          %%
%% 13/07/96 added compilation
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemmaflex.pl                                              %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lemma_runtime_procedures(Result) :-
	lemmatize_procedure(Lemmatization0),

	lemmatize_dynamically_procedure(DynamicLemmatization),
	lemmatize_statically_procedure(StaticLemmatization),
	conjoin(DynamicLemmatization,StaticLemmatization,Lemmatization1),

	conjoin(Lemmatization0,Lemmatization1,Lemmatization),

	default_assumptions_procedure(DefaultHandler),

	conjoin(Lemmatization,DefaultHandler,Result).

lemmatize_procedure(Result) :-
	lemma_handling_flag,
	!,

	/* DEFAULTHANDLER */
        ((static_lemma_flag;dystatic_lemma_flag)        % compile-flag 
	       ->
	          DefaultHandler   = default_assumptions(ProofOut,ProofEnd,Ass);
	 %true ->
		  DefaultHandler   = true,
		  Ass              = []
	),

	/* DELTA LEMMATIZE */
	(lemma_type_parameter(delta) ->
	    HeadD = lemmatize(default(N:(Gamma :- _)),Proof,ProofOut,ProofEnd),
	    /* REDUCTIONHANDLER */
	    (lemma_type_parameter(omega) ->
		ReductionHandlerD = remove_reductions(Proof,ProofOut);
	    %true ->
		ReductionHandlerD = true,
		ProofOut=Proof),
	    conjoin(ReductionHandlerD,DefaultHandler,BodyD1),
	    /* LEMMAHANDLER   */
	    lemma_handler(gamma(N,Gamma),false,Ass,LemmaHandlerD),
	    conjoin(BodyD1,LemmaHandlerD,BodyD),
	    RuleD = (HeadD :- !,BodyD);
	%true ->
	    RuleD = true),

	/* OMEGA LEMMATIZE */
	(lemma_type_parameter(omega) ->
	    HeadO             = lemmatize(extension(_:Goal),Proof,ProofOut,ProofEnd),
	    /* REDUCTIONHANDLER */
	    ReductionHandlerO = skim_reductions(Goal,Proof,ProofOut,ProofEnd,Ancs),
	    conjoin(ReductionHandlerO,DefaultHandler,BodyO1),
	    /* LEMMAHANDLER   */
	    lemma_handler(Goal,Ancs,Ass,LemmaHandler1),
	    (lemma_format_parameter(unit) ->
		LemmaHandlerO = (Ancs=false -> LemmaHandler1 ; verbose(skipping:Goal:Ancs));
	    %true ->
		LemmaHandlerO = LemmaHandler1),
	    conjoin(BodyO1,LemmaHandlerO,BodyO),
	    RuleO = (HeadO :- !,BodyO);
	%true ->
	    RuleO = true),

	/* ignore LEMMATIZE */
        HeadI = lemmatize(Inference,Proof,Proof,ProofEnd),
	BodyI = verbose("'lemmatize/4: Ignoring inference'":Inference),
	RuleI = (HeadI :- BodyI),

	/* RESULTING CODE */
        conjoin(RuleD,RuleO,Result1),
        conjoin(Result1,RuleI,Result).
lemmatize_procedure(Result) :-
	/* no_lemma_handling */
	Result = (lemmatize(_,_,_,_)).

lemma_handler(Goal,Ancs,Ass,LemmaHandler) :-
	((dynamic_lemma_flag;dystatic_lemma_flag) ->
	    DynamicLemmaHandler = lemmatize_dynamically(Goal,Ancs,Ass);
        %true ->
	    DynamicLemmaHandler = true),
	(static_lemma_flag ->
	    StaticLemmaHandler = lemmatize_statically(Goal,Ancs,Ass);
        %true ->
	    StaticLemmaHandler = true),
	conjoin(DynamicLemmaHandler,StaticLemmaHandler,LemmaHandler).

lemmatize_dynamically_procedure(Result) :-
	lemma_handling_flag,
	lemma_mode_parameter(Mode),
	(Mode = dynamic ; Mode = dystatic),
	!,

	Head0 = lemmatize_dynamically(Goal,Ancestors,_),
	Body0 = dynamic_lemma(Goal,Ancestors,_),           % UNIFY ?!
	Rule0 = (Head0 :- Body0, !),
	
	(dystatic_lemma_flag ->

	    Head1  = lemmatize_dynamically(Goal,Ancestors,[]),
	    Body1A = assert(dynamic_lemma(Goal,Ancestors,[])),
	    Body1S = lemmatize_statically(Goal,Ancestors,[]),
	    Rule1  = (Head1 :- !, Body1A, Body1S),

	    Body2S = lemmatize_statically(Goal,Ancestors,Assumptions);

	%true ->
	    Rule1  = true,
	    Body2S = true),

	conjoin(Rule0,Rule1,Rule01),

	Head2  = lemmatize_dynamically(Goal,Ancestors,Assumptions),
	Body2A =  assert(dynamic_lemma(Goal,Ancestors,Assumptions)),
	Body2R = retract(dynamic_lemma(Goal,Ancestors,Assumptions)),
	conjoin(Body2R,Body2S,Body2RS),
	Rule2  = (Head2 :- Body2A ; (Body2RS, ! , fail)),

	conjoin(Rule01,Rule2,Result).
lemmatize_dynamically_procedure(Rule) :-
	Head = lemmatize_dynamically(_,_,_),
	Body = (write(lemmatize_dynamically*not_in_charge),nl,trace,fail),
	Rule = (Head :- Body).

lemmatize_statically_procedure(Result) :-
	lemma_handling_flag,
	lemma_mode_parameter(Mode),
	(Mode = static ; Mode = dystatic),
	!,

	Head0  = lemmatize_statically(Goal,Ancestors,Assumptions),
	Body0  = static_lemma(Goal,Ancestors,Assumptions),           % UNIFY ?!
	Rule0  = (Head0 :- Body0, !),
	
	Head1  = lemmatize_statically(Goal,Ancestors,Assumptions),
	Body1A = assert(static_lemma(Goal,Ancestors,Assumptions)),
	Rule1  = (Head1 :- Body1A),

	Result = (Rule0,Rule1).
lemmatize_statically_procedure(Rule) :-
	Head = lemmatize_statically(_,_,_),
	Body = (write(lemmatize_statically*not_in_charge),nl,trace,fail),
	Rule = (Head :- Body).

default_assumptions_procedure(Result) :-
	lemma_handling_flag,
	lemma_mode_parameter(Mode),
	(Mode = static ; Mode = dystatic),
	!,
	
	Head0 = default_assumptions(Proof,ProofEnd,[]),
	Body0 = (Proof == ProofEnd),
	Rule0 = (Head0 :- Body0, !),

	BodyR = default_assumptions(Proof,ProofEnd,Justs),

	Body123 = (BodyR,combine_clauses(Just,Justs,Assumptions)),

	Head1 = default_assumptions([default(_:(_ :- _: Just))|Proof],ProofEnd,Assumptions),
	Head2 = default_assumptions([static_lemma(_   : Just) |Proof],ProofEnd,Assumptions),
	Head3 = default_assumptions([dynamic_lemma(_  : Just) |Proof],ProofEnd,Assumptions),

	Rule1 = (Head1 :- !, Body123),
	conjoin(Rule0,Rule1,Rule01),

	Rule2  = (Head2 :- !, Body123),
	Rule3  = (Head3 :- !, Body123),
	Rule23 = (Rule2,Rule3),
	conjoin(Rule01,Rule23,Rule0123),

	Head4 = default_assumptions([_|Proof],ProofEnd,Justs),
	Rule4 = (Head4 :- BodyR),

	conjoin(Rule0123,Rule4,Result).
default_assumptions_procedure(Rule) :- 
	Head = default_assumptions(_,_,_),
	Body = (write(default_assumptions*not_in_charge),nl,trace,fail),
	Rule = (Head :- Body).

%%% Compilation of run-time procedures for lemma handling
%%%

compile_lemma_handling(Name) :-
	lemma_runtime_procedures(LemmaProcs),
	write_lem(Name,LemmaProcs),
	compile_lem(Name).

read_lem(Name,Wff) :-
	concatenate(Name,'.lem',LFile),
	read_clauses(LFile,Wff).	
write_lem(File,LemmaProcs) :-
	concatenate(File,'.lem',LemmaFile),
	open(LemmaFile,write,LemmaStream),
        write_clauses(LemmaStream,LemmaProcs),
        close(LemmaStream),
	!.
compile_lem(File) :-	
	concatenate(File,'.lem',KBFile),
	compile(KBFile).

