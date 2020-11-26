%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 15/03/98   File: defaults.pl
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 18/03/95 Created                                                          %%
%% 14/07/96 added configuration utilities
%% 15/03/98 added pttp config and optional proof printing
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog defaults.pl                                               %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(base).
:- ensure_loaded(pttp).
:- ensure_loaded(builtin).
:- ensure_loaded(model).
:- ensure_loaded(io).
:- ensure_loaded(db).
:- ensure_loaded(herbrand).
:- ensure_loaded(hooks).
:- ensure_loaded(lemma).
:- ensure_loaded(xray_config).

%%% ----------------------------------------------------------------------
%%% XRay COMPILATION (patches pttp)

%%% Negation normal form/Defaults to Prolog clause translation.
%%% Include a literal in the body of each clause to
%%% indicate the number of the formula the clause came from.

clauses((A , B),L,WffNum) :-
        !,
        clauses(A,L1,WffNum),
        WffNum2 is WffNum + 1,
        clauses(B,L2,WffNum2),
        conjoin(L1,L2,L).
clauses( (Gamma :- Alpha : Beta) , L , WffNum ) :-
	!,
	clauses((Gamma :- gamma(WffNum,Gamma)),L1,WffNum),
	clauses((alpha(WffNum,Alpha) :- Alpha),L2,WffNum),
	conjoin(L1,L2,L3),
	conjoin(Gamma,Beta,C0),                             % ConDL-specific
	cnf(C0,C1),
	make_matrix(C1,C2),
	matrix_reduction(C2,Justification),
	(delta_ordering(compatibility>admissibility) ->
	    conjoin(justification(Justification),
	            alpha(WffNum,Alpha),
		    Body);
	%true ->
	    conjoin(alpha(WffNum,Alpha),
		    justification(Justification),
	            Body)),
	(compile_proof_printing ->
	    Record = infer_by(default(WffNum:(Gamma :- Alpha : Justification)));
	%true ->
	    Record = true),
	conjoin(Record,Body,Body1),
	DRule= (gamma(WffNum,Gamma) :- Body1),
	conjoin(DRule,L3,L).

clauses(A,L,WffNum) :-
        head_literals(A,Lits),
        clauses(A,Lits,L,WffNum).

clauses(A,[Lit|Lits],L,WffNum) :-
        body_for_head_literal(Lit,A,Body1),
        ((compile_proof_printing,Body1 = true) ->
                Record = infer_by(unit(WffNum:Lit));
         compile_proof_printing ->
                Record = infer_by(extension(WffNum:Lit));
        %true ->
		Record = true),
	conjoin(Record,Body1,Body),
        clauses(A,Lits,L1,WffNum),
        conjoin((Lit :- Body),L1,L).
clauses(_,[],true,_).

%%% This patches the original predicate
%%%
%%%

add_ancestor((Head :- Body),(Head1 :- Body1)) :-
        functor(Head,query,_) ->
                Head1 = Head,
                add_ancestor_args(Body,[[],[],[]],Body1);
	functor(Head,gamma,_) ->                      
                Head =.. L,                             
                append(L,[_,_,Defaults],L1),                             
                Head1 =.. L1,                                   
                add_ancestor_args(Body,[[],[],NewDefaults],Body2),
		conjoin((NewDefaults = [Head|Defaults]),Body2,Body1); 
	functor(Head,alpha,_) ->                      
                Head =.. L,                             
                append(L,[_,_,Defaults],L1),                             
                Head1 =.. L1,                                   
                add_ancestor_args(Body,[[],[],Defaults],Body1); 
        %true ->
                Head =.. L,
                append(L,[PosAncestors,NegAncestors,Defaults],L1),
                Head1 =.. L1,
                add_ancestor_args(Body,[NewPosAncestors,NewNegAncestors,Defaults],Body2),
                (Body == Body2 ->
                        Body1 = Body2;
                negative_literal(Head) ->
                        NewPosAncestors = PosAncestors,
                        conjoin((NewNegAncestors = [Head|NegAncestors]),Body2,Body1);
                %true ->
                        NewNegAncestors = NegAncestors,
                        conjoin((NewPosAncestors = [Head|PosAncestors]),Body2,Body1)).


ancestor_tests(P,N,Result) :-
        P == query ->
                Result = true;
	P == gamma ->
		Head = gamma(DefaultID,DefaultConseq,_,_,Defaults),
		Default = gamma(DefaultID,DefaultConseq),
		Result = (Head :- identical_member(Default,Defaults), !, fail);
	P == alpha ->
		Result = true;          % ??? <== Please, VERIFY !
        %true ->
                negated_functor(P,NotP),
                N3 is N - 3,            % N - 3 due to 3 ancestor-lists
                functor(Head1,P,N3),
                Head1 =.. [P|Args1],
                Head2 =.. [NotP|Args1],
                append(Args1,[PosAncestors,NegAncestors,_],Args),
                Head =.. [P|Args],
                (negative_functor(P) ->
                        C1Ancestors = NegAncestors, 
			C2Ancestors = PosAncestors;
                %true ->
                        C1Ancestors = PosAncestors, 
			C2Ancestors = NegAncestors),
                C1 = (Head :- identical_member(Head1,C1Ancestors), !, fail),
                count_inferences_pred(IncNcalls),
                (N3 = 0 ->              % special case for propositional calculus
                        conjoin((identical_member(Head2,C2Ancestors) , !),IncNcalls,V);
                %true ->
                        conjoin(unifiable_member(Head2,C2Ancestors),IncNcalls,V)),
                (compile_proof_printing ->
                        conjoin(V,infer_by(reduction(Head2)),V1);
                %true ->
                        V1 = V),
                C2 = (Head :- V1),
                conjoin(C1,C2,Result).


procedures_with_tests([[P,N]|Preds],Clauses,Procs) :-
        procedure(P,N,Clauses,Proc0),

        ancestor_tests(P,N,TestsA),
	lemma_tests(P,N,TestsL),
	conjoin(TestsA,TestsL,Tests),

	phook_tests(P,N,Tests,Proc0,ProcP),

        procedures_with_tests(Preds,Clauses,ProcsPs),
        conjoin(ProcP,ProcsPs,Procs).
procedures_with_tests([],_Clauses,true).


%%% Consistency checking.
%%%
%%% Add extra arguments to each goal so that information
%%% on what assumptions were made in the proof can be checked
%%% at each step/the end.
%%%
%%% I suppose Wff has to be replaced by cmm(Model,ModelStructure) ...
%%% [all this is a quick copy of add_proof_recording ...]

add_consistency_checking((Head :- Body),(Head1 :- Body1)) :-
        functor(Head,query,_) ->
                Head1 = Head,
		conjoin(model_initialization(MM0),Body,Body2),
		add_consistency_checking_args(Body2,MM0,MMOut,Body1);
        %true ->
		Head =.. L,
		append(L,[MMIn,MMOut],L1),
		Head1 =.. L1,
		add_consistency_checking_args(Body,MMIn,MMOut,Body1).

add_consistency_checking_args(Body,MMIn,MMOut,Body1) :-
        Body = (A , B) ->
                add_consistency_checking_args(A,MMIn,MMIn1,A1),
                add_consistency_checking_args(B,MMIn1,MMOut,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_consistency_checking_args(A,MMIn,MMOut,A1),
                add_consistency_checking_args(B,MMIn,MMOut,B1),
                disjoin(A1,B1,Body1);
        Body =.. [search,Goal|L] ->
                add_consistency_checking_args(Goal,MMIn,MMOut,Goal1), % ???
                Body1 =.. [search,Goal1|L];
        Body = justification(X) ->
		Body1 = compatible(X,MMIn,MMOut);
        Body = fail ->
                Body1 = Body;
        builtin(Body) ->
                MMIn = MMOut,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[MMIn,MMOut],L1),
                Body1 =.. L1.

add_model_structure(WffI,Q,C,WffO) :-
	WffI = (A , B) ->
                add_model_structure(A,Q,C,A1),
                add_model_structure(B,Q,C,B1),
		conjoin(A1,B1,WffO);
        WffI = (A ; B) ->
                add_model_structure(A,Q,C,A1),
                add_model_structure(B,Q,C,B1),
		disjoin(A1,B1,WffO);
        WffI = (A :- B) ->
                add_model_structure(B,Q,C,B1),
		WffO = (A :- B1);
	WffI = model_initialization(Var) ->
	        combine_clauses(Q,C,Matrix),
		WffO = model_initialization(Matrix,Var);
        %true ->
	        WffO = WffI.

classical_clauses(WffI,WffO) :-
        WffI = (A , B) ->
                classical_clauses(A,A1),
                classical_clauses(B,B1),
		conjoin(A1,B1,WffO);
        WffI = (A ; B) ->
                classical_clauses(A,A1),
                classical_clauses(B,B1),
		disjoin(A1,B1,WffO);
        WffI = (A :- B) ->                        % ??? (special case query elim. TS Apr04)
	        WffO = true;
        builtin(WffI) ->
                WffO = true;
        %true ->
	        WffI = WffO.

query_clause(WffI,WffO) :-
        WffI = (A , B) ->
	        (query_clause(A,WffO);
                 query_clause(B,WffO));
        WffI = (A ; B) ->
                (query_clause(A,WffO);
                 query_clause(B,WffO));
        WffI = (A :- B) ->
	        (A = query ->
                      classical_clauses(B,WffO);
		 %true ->
                      fail);
        %true ->
	        fail.

query(M) :-                             % call query with depth bound M
	(compile_complete_search, compile_proof_printing , lemma_handling_flag) ->
	        query(M,_N,_LemProof,_LemProofEnd,_Proof,_ProofEnd);
        (compile_complete_search, (compile_proof_printing ; lemma_handling_flag)) -> 
                query(M,_N,_Proof,_ProofEnd);
        compile_complete_search ->
                query(M,_N).

query :-                                % unbounded search of query
        (compile_complete_search ->
	    query(1000000);
	%true ->
	    ((compile_proof_printing , lemma_handling_flag) ->
	         query(_LemProof,_LemProofEnd,_Proof,_ProofEnd);
	     (compile_proof_printing ; lemma_handling_flag) -> 
		 query(Proof,_ProofEnd);
             %true ->
		 query)).
	    


%%% ----------------------------------------------------------------------
%%% XRay: THE ACTUAL COMPILATION PROCEDURES
%%%

xray(Name) :-
	read_kb(Name,KB),
	dpttp(Name,KB).
	

dpttp(Name,X) :-
        time(dpttp1(X,Y:Z),'Compilation Phase I'),
	time(dpttp2(Name,Y:Z),'Compilation Phase II'),
	time(dpttp3(Name),'Compilation Phase III').

dpttp(X) :-
	Name = 'temp',
	dpttp(Name,X).

dpttp1(X,Y:C) :-
        nl,
        write('XRay input formulas:'),
        apply_to_conjuncts(X,write_clause,_),
        nl,

	constants(X,H),
	(H = [] ->
	    nl,write('Empty Herbrand universe.'),nl;
	 %true ->
	    nl,write('Herbrand universe':H),nl),

	classical_clauses(X,C0),
	cnf(C0,C1),
	make_matrix(C1,C2),
	instantiation(C2,H,C3),
	matrix_reduction(C3,C),

        (verbose_flag ->
	     nl,
	     write('Classical output formulas:'),
	     apply_to_list(C,write_clause,_),
	     nl;
	%true ->
	     true),

	dpttp1(X,C,Y:C).

dpttp1(X,C,Y:C) :- 
	query_clause(X,Q0),
	variables(Q0,Vars),
	(Vars=[] ->
	         cnf(Q0,Q1),
		 make_matrix(Q1,Q2),
		 matrix_reduction(Q2,Q),

		 XQ=X;
	%true ->
		 Q = [],

		 apply_to_conjuncts(X,add_answer_preds,XQ)),

        (verbose_flag ->
	     nl,
	     write('Query formula:'),
	     apply_to_conjuncts(Q0,write_clause,_),
	     nl,
	     write('      compiled:'),
	     apply_to_list(Q,write_clause,_),
	     nl,nl;
	%true ->
	     true),

	dpttp1(XQ,Q,C,Y:C).

dpttp1(X,Q,C,Y:C) :- 
        clauses(X,XC,1),

	constants(X,H),
	(H = [] ->
	    XH=true,
	    X0=XC;
	 %true ->
	    herbrand_preds(H,XH),
	    apply_to_conjuncts(XC,add_herbrand_preds,X0)),

        apply_to_conjuncts(X0,add_count_inferences,X1),
        apply_to_conjuncts(X1,add_ancestor,X2),
        predicates(X2,Preds0),
        reverse(Preds0,Preds),
	procedures_with_tests(Preds,X2,X3),
	/* all contrapositives available */
        apply_to_conjuncts(X3,add_sound_unification,X4),
	apply_to_conjuncts(X4,add_consistency_checking,X5),
        (compile_complete_search ->
	    apply_to_conjuncts(X5,add_complete_search,X6);
	%true ->
	    X5=X6),
	apply_to_conjuncts(X6,add_lemmatization,XL), /* relies on 'infer_by */
	(compile_proof_printing ->
                apply_to_conjuncts(XL,add_proof_recording,X7);
	%true ->
                X7 = XL),
	add_model_structure(X7,Q,C,X8),

	apply_to_conjuncts(X8,add_body_hooks,XD),

	apply_to_conjuncts(X,prolog_clause,XP),
	conjoin(XP,XD,XR),

	conjoin(XH,XR,Y),

        (verbose_flag -> 
	     nl,
	     write('XRay output formulas:'),
	     apply_to_conjuncts(Y,write_clause,_),
	     nl;
	%true ->
	     true),
	!.

dpttp1(X) :-
	dpttp1(X,_).

dpttp2(Name,Y:Z) :-
        nl,
        write('XRay writing compiled clauses ... '),
        write_ckb(Name,Y),
	write_cmm(Name,Z),
	write('done.'),
	!.

dpttp2(Y:Z) :-
	Name = 'temp',
	dpttp2(Name,Y:Z).

dpttp3(Name) :-
	nl,
        write('XRay compiling clauses ... '),
        compile_ckb(Name),
	write('done.'),
        nl,
        write('XRay compiling query ... '),
        compile_query(Name),
	write('done.'),
        nl,
        !.
dpttp3 :-
	Name = 'temp',
	dpttp3(Name).

