% ===================================================================
% COMPLIER OPTIONS
% ===================================================================

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).



:- dynamic(count_inferences_pred/1).
:- dynamic(trace_search_progress_pred/1).
:- dynamic(compile_proof_printing/0).
:- dynamic(ncalls/1).
:- dynamic(dynamic_lemma/3).
:- dynamic(static_lemma/3).
:- dynamic(lemma_handling_flag/0).
:- dynamic(lemma_mode_parameter/1).
:- dynamic(lemma_format_parameter/1).
:- dynamic(lemma_type_parameter/1).
:- dynamic(body_hooks_flag/0).
:- dynamic(pred_hooks_flag/0).
:- dynamic(delta_ordering/1).
:- dynamic(verbose_flag/0).
:- dynamic(compile_complete_search/0).
	

count_inferences_pred(flag(ncalls,NCalls,NCalls+1)).


trace_search :-                         % enables search progress reports
        retract(trace_search_progress_pred(_)),
        fail.
trace_search :-
        assert(trace_search_progress_pred(write_search_progress)).

dont_trace_search :-                    % disables search progress reports
        retract(trace_search_progress_pred(_)),
        fail.
dont_trace_search :-
        assert(trace_search_progress_pred(nop)).



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
	    



xtry:-
	sigmaCache(_ ,nnf(X,Flags,Id), _, _, _, _, _, _, _),
	format('~q.~n',[X]),fail.


mkm:-
	tell(mkm),
	show_relations,
	told.

get_relations(KB,Ctx):-
	retractall(fact_relation(_)),
	retractall(rule_relation(_,_)),
	fail.
	
/*
get_relations(KB,Ctx):-
	sigmaCache(R, _, Axiom, Vars, KB, Ctx, Tracking, User, Status),
	get_relation(R),
	Axiom = subrelation(W,_),
	get_relation(W),
	fail.
*/

	


get_relations(KB,Ctx):-
	sigmaCache(R,Cons, Ante, _, Logic, KB, Ctx, Proof),
	get_relation(R),
	get_r_relation(R,Logic),
	fail.

get_relations(KB,Ctx):-listing(rule_relation),listing(fact_relation).

:-dynamic(rule_relation/2).
:-dynamic(fact_relation/1).

get_relation(R):-var(R),!,fail.

get_relation(R):-fact_relation(R),!.
get_relation(R):-rule_relation(R,_),!.
get_relation(R):-assert(fact_relation(R)).

get_r_relation(R,_):-var(R),!,fail.
get_r_relation(R,Logic):-
	rule_relation(R,Logic),!.
get_r_relation(R,Logic):-
	retractall(fact_relation(R)),
	asserta(rule_relation(R,Logic)),!.
	



mk_length(R,A,P):-
	getArity(R,A),!,
	length(L,A),
	numbervars(L,'$VAR',0,_),
	P=..[R|L].

getArity(R,A):-
	sigmaCache(valence, surface, valence(R,A), Vars, KBName, Context, Tracking, User, Status).
getArity(R,A):-
	sigmaCache(R, surface, P, Vars, KBName, Context, Tracking, User, Status),
	functor(P,R,A).
getArity(R,A):-
	sigmaCache(R, GAF, Vars, KBName, Context, Tracking, User),
	functor(GAF,holds,HF),
	A is HF-1,!.
	
getArity(R,2):-!.

show_relations(R):-
	mk_length(R,A,P),!,
	show_relations(R,A,P).

show_relations(R,A,P):-	shown_rel(R,A),!.
show_relations(R,A,P):-	
	assert(shown_rel(R,A)),
	format(
'% ~q/~w
mapping_nt(~q,undefined).

',[R,A,P]),!.

/*
	
        time(dpttp1(X,Y:Z),'Compilation Phase I'),
	time(dpttp2(Name,Y:Z),'Compilation Phase II'),
	time(dpttp3(Name),'Compilation Phase III').
*/

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
	     true),dpttp1(X,C,Y:C).

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
	     true), dpttp1(XQ,Q,C,Y:C).

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


%%% ----------------------------------------------------------------------
%%% PTTP CONFIGURATION

%%% complete search facilities are turned (during compile-time)
%%%  on by do_compile_complete_search,
%%% off by dont_compile_complete_search.
%%%
%%% do_compile_complete_search or dont_compile_complete_search *must* be
%%% executed before the problem is compiled.

do_compile_complete_search :-
        retract(compile_complete_search),
        fail.
do_compile_complete_search :-
        assert(compile_complete_search).

dont_compile_complete_search :-
        retract(compile_complete_search),
        fail.
dont_compile_complete_search.


%%% Proof printing is (better) turned (during compile-time)
%%%  on by   do_compile_proof_printing (print_proof),
%%% off by dont_compile_proof_printing (dont_print_proof).
%%%
%%% do_compile_proof_printing or dont_compile_proof_printing *must* be
%%% executed before the problem is compiled.

do_compile_proof_printing   :- print_proof.
dont_compile_proof_printing :- dont_print_proof.


%%% Proof printing is turned on by print_proof,
%%% off by dont_print_proof.
%%%
%%% print_proof or dont_print_proof should be
%%% executed before the problem is compiled.

print_proof :-                          % enable proof printing
        retract(compile_proof_printing),
        fail.
print_proof :-
        assert(compile_proof_printing).

dont_print_proof :-                     % disable proof printing
        retract(compile_proof_printing),
        fail.
dont_print_proof.


%%% Inference counting is turned
%%%  on by   do_compile_count_inferences (count_inferences),
%%% off by dont_compile_count_inferences (dont_count_inferences).
%%%
%%% Inferences are counted by retracting the current count
%%% and asserting the incremented count, so inference counting
%%% is very slow.

do_compile_count_inferences   :- count_inferences.
dont_compile_count_inferences :- dont_count_inferences.


pttp_configuration :-
	nl,format('PTTP CONFIGURATION:'),nl,
	(count_inferences_pred(true) ->
	    format('PTTP counts no inferences.');
	    format('PTTP counts inferences!')),
        (trace_search_progress_pred(nop) ->
	    format('PTTP does not trace search progress.');
	    format('PTTP traces search progress!')),
	(compile_proof_printing ->
	    format('PTTP compiles proof printing!');
	    format('PTTP does not compile proof printing.')),
	(compile_complete_search ->
	    format('PTTP compiles complete search!');
	    format('PTTP does not compile complete search.')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 24/04/95   File: lemma.pll                     %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 04/04/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemma.pll                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% ----------------------------------------------------------------------
%%% LEMMA CONFIGURATION

lemma_configuration :-
	nl,write('LEMMA CONFIGURATION:'),nl,nl,
	
	write('Lemma handling'),
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


% --- special purpose predicates



%%% ----------------------------------------------------------------------
%%% XRay CONFIGURATION

xray_configuration :-
	nl,write('XRay CONFIGURATION:'),nl,nl,
	
	write(delta_ordering),
	write(' = '),
	(delta_ordering(O), write(O),write(' '),fail ; write('.')),
	nl,
	
	write(verbose_mode),
	write(' = '),
	(verbose_flag, write('on') ; write('off')),
	nl.

%%% delta_ordering stears the order of admissibility 
%%% and compatibility checking in delta rules
%%% at compile-time

switch_delta_ordering :-
	delta_ordering(compatibility>admissibility),
	admissibility_first.
switch_delta_ordering :-
	delta_ordering(admissibility>compatibility),
	compatibility_first.

compatibility_first :-                      % check compatibility first
        retract(delta_ordering(_)),
        fail.
compatibility_first :-
        assert(delta_ordering(compatibility>admissibility)).

admissibility_first :-                      % check admissibility first
        retract(delta_ordering(_)),
        fail.
admissibility_first :-
        assert(delta_ordering(admissibility>compatibility)).

%%% Verbose proof printing is turned on by verbose_mode,
%%% off by dont_verbose_mode.
%%% works during compile- and run-time

verbose_mode :-                          % enable proof printing
        retract(verbose_flag),
        fail.
verbose_mode :-
        assert(verbose_flag).

no_verbose_mode :-                     % disable proof printing
        retract(verbose_flag),
        fail.
no_verbose_mode.


configuration :-
	hook_configuration,
	lemma_configuration,
	pttp_configuration,
	xray_configuration.




prolog_clause((Head :- Body),(Head :- Body)) :-
	functor(Head,Pred,_),
	builtin_predicate(Pred),!.
	
prolog_clause(Fact,(Fact:-true)) :-
	functor(Fact,Pred,_),
	Pred \=( ':-'),builtin_predicate(Pred),!.
prolog_clause(_,true).




%%% ----------------------------------------------------------------------
%%% HOOKS CONFIGURATION

hook_configuration :-
	nl,write('HOOK CONFIGURATION:'),nl,nl,
	
	write('body hook handling     '),
	write(' = '),
	(body_hooks_flag -> write(on) ; write(off)),
	nl,

	write('predicate hook handling'),
	write(' = '),
	(pred_hooks_flag -> write(on) ; write(off)),
	nl.

%%% Hook handling is turned off by no_hook_handling

no_hook_handling :- no_body_hooks, no_pred_hooks.

%%% Body hooks are turned on by body_hooks.
%%% off by no_body_hooks.

body_hooks :-                          % enable body hooks
        retract(body_hooks_flag),
        fail.
body_hooks :-
        assert(body_hooks_flag).

no_body_hooks :-                       % disable body hooks
        retract(body_hooks_flag),
        fail.
no_body_hooks.

%%% Predicate hooks are turned on by pred_hooks.
%%% off by no_pred_hooks.

pred_hooks :-                          % enable predicate hooks
        retract(pred_hooks_flag),
        fail.
pred_hooks :-
        assert(pred_hooks_flag).

no_pred_hooks :-                       % disable predicate hooks
        retract(pred_hooks_flag),
        fail.
no_pred_hooks.

%%% SETTINGS for HOOKS HANDLING
%%%


%%% pttp is the PTTP compiler top-level predicate.

pttp(X) :-
        time(pttp1(X),'Compilation').

pttp1(X) :-
        nl,
        write('PTTP input formulas:'),
        apply_to_conjuncts(X,write_clause,_),
        nl,
        clauses(X,X0,1),
        apply_to_conjuncts(X0,add_count_inferences,X1),
        apply_to_conjuncts(X1,add_ancestor,X2),
        predicates(X2,Preds0),
        reverse(Preds0,Preds),
        procedures_with_ancestor_tests(Preds,X2,X3),
        apply_to_conjuncts(X3,add_sound_unification,X4),
        apply_to_conjuncts(X4,add_complete_search,X5),
        (compile_proof_printing ->
                apply_to_conjuncts(X5,add_proof_recording,Y);
        %true ->
                Y = X5),
        nl,
        write('PTTP output formulas:'),
        apply_to_conjuncts(Y,write_clause,_),
        nl,
        nl,

        File = 'temp.plrolog',                     % Quintus Prolog on Sun
%       File = 'darwin:>stickel>pttp>temp.plrolog',% change file atom_codes for other systems

        open(File,write,OutFile),
        write_clause_to_file(Y,OutFile),
        close(OutFile),
        compile(File),
        nl,
        !.

write_clause_to_file((A,B),OutFile) :-
	write_clause_to_file(A,OutFile),
	write_clause_to_file(B,OutFile),
        !.
write_clause_to_file(A,OutFile) :-
	nl(OutFile),
	write(OutFile,A),
	write(OutFile,.),
	!.


query(M) :-                             % call query with depth bound M
        compile_proof_printing -> 
                query(M,_N,_Proof,_ProofEnd);
        %true ->
                query(M,_N).

query :-                                % unbounded search of query
        query(1000000).

negated_functor(F,NotF) :-
        atom_codes(F,L),
        atom_codes(not_,L1),
        (append(L1,L2,L) ->
                true;
        %true ->
                append(L1,L,L2)),
        atom_codes(NotF,L2).

negated_literal(Lit,NotLit) :-
        Lit =.. [F1|L1],
        negated_functor(F1,F2),
        (var(NotLit) ->
                NotLit =.. [F2|L1];
        %true ->
                NotLit =.. [F2|L2],
                L1 == L2).

negative_functor(F) :-atom_concat( 'not_', _, F),!.
negative_literal(Lit) :-
        functor(Lit,F,_),
        negative_functor(F).


write_clause(A) :-
        nl,
        write(A),
        write(.).

write_clause(A,_) :-                    % 2-ary predicate can be used as
        write_clause(A).                % argument of apply_to_conjuncts

%%% Inference counting is turned on by count_inferences,
%%% off by dont_count_inferences.
%%%
%%% Inferences are counted by retracting the current count
%%% and asserting the incremented count, so inference counting
%%% is very slow.




add_count_inferences((Head :- Body),(Head :- Body1)) :-
        functor(Head,query,_) ->
                Body1 = Body;
        %true ->
                count_inferences_pred(P),
                conjoin(P,Body,Body1).

%%% Search tracing is turned on by trace_search,
%%% off by dont_trace_search.

write_search_progress(Level) :-
        flag(ncalls,N,N),
        (N > 0 -> write(N) , write(' inferences so far.') ; true),
        nl,
        write('Begin cost '),
        write(Level),
        write(' search...  ').

%%% A query can be timed by time(query).

time(X) :-
        time(X,'Execution').

time(X,Type) :-
        flag(ncalls,_,0),

        statistics(runtime,[T1,_]),     % Quintus Prolog on Sun
%        T1 is get-internal-run-time,  % Common KIF time function

        call(X),

        statistics(runtime,[T2,_]),     % Quintus Prolog on Sun
        Secs is (T2 - T1) / 1000.0,     % Quintus measures runtime in milliseconds
%        T2 is get-internal-run-time,  % Common KIF time function
%        Secs is (T2 - T1) / 977.0,      % internal-time-units-per-second on Darwin

        nl,
        write(Type),
        write(' time: '),
        flag(ncalls,N,N),
        (N > 0 -> write(N) , write(' inferences in ') ; true),
        write(Secs),
        write(' seconds, including printing'),
        nl.



%%% ----------------------------------------------------------------------
%%% BODY HOOKS

add_body_hooks((Head :- Body),(Head :- Body2)) :-
	body_hooks_flag,
	!,
	(bhook1_p(Head :- Body) ->
	    conjoin((bhook1(Head :- Body)),Body,Body1);
        %true ->
	    Body1=Body),
	(bhook2_p(Head :- Body) ->
	    conjoin(Body1,(bhook2(Head :- Body)),Body2);
        %true ->
	    Body2=Body1).
add_body_hooks((Head :- Body),(Head :- Body)).

%%% COMPILE-TIME conditions for body hook insertion

bhook1_p(Head :- Body) :-
	true.
bhook2_p(Head :- Body) :-
	true.

%%% RUN-TIME predicates for body hooks

bhook1(Head :- Body) :-
	(Head = _) ->
	    Head =.. [P|_],
	    nl,write(b1:(P)),nl,nl;
        %true ->
	     true.
bhook2(Head :- Body) :-
	(Head = _) ->
	    Head =.. [P|_],
	    nl,write(b2:(P)),nl,nl;
        %true ->
	     true.

%%% ----------------------------------------------------------------------
%%% BODY HOOKS

%%% COMPILE-TIME conditions for PREDICATE hook insertion

phook_tests(P,N,TestsA,Proc,ProcP) :-
	pred_hooks_flag,
	!,
	
	phook1_tests(P,N,Tests1),
	conjoin(Tests1,TestsA,Tests1A),
	
	phook2_tests(P,N,Tests2),
	conjoin(Tests1A,Tests2,Tests1A2),

	phook3_tests(P,N,Tests3),
	conjoin(Proc,Tests3,Proc3),

	conjoin(Tests1A2,Proc3,ProcP).
phook_tests(_,_,TestsA,Proc,ProcP) :-
	conjoin(TestsA,Proc,ProcP).

phook1_p(P,N) :-
	true.
phook2_p(P,N) :-
	true.
phook3_p(P,N) :-
	true.

%%% COMPILE-TIME predicates PREDICATE hook insertion

phook1_tests(P,N,Result) :-
	phook1_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p1:P),nl,fail),
	Result = (Head :- Body).
phook1_tests(_,_,true).

phook2_tests(P,N,Result) :-
	phook2_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p2:P),nl,fail),
	Result = (Head :- Body).
phook2_tests(_,_,true).

phook3_tests(P,N,Result) :-
	phook3_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p3:P),nl,fail),
	Result = (Head :- Body).
phook3_tests(_,_,true).

head(P,N,Head) :-
	P == query ->
                Head = query;
	%true ->
		functor(Head,P,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date:  4/04/95   File: parser.pll                    %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%%  4/04/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog parser.pll                                                 %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% reads the knowledge base from the file 'Name.kb'

read_kb(Name,Wff) :-
	concat_atom(Name,'.kb',KBFile),
	read_clauses(KBFile,Wff).	

read_ckb(Name,Wff) :-
	concat_atom(Name,'.ckb',CKBFile),
	read_clauses(CKBFile,Wff).	

read_que(Name,Wff) :-
	concat_atom(Name,'.que',QFile),
	read_clauses(QFile,Wff).	

read_clauses(File,Wff) :-
	open(File,read,Stream),
	read_wff_loop(Stream,Wff),
	close(Stream).

read_wff_loop(Stream,Wff) :-
	read(Stream,Wff1),
	(Wff1 == end_of_file ->
	           Wff = true;
	 %true               ->
		   read_wff_loop(Stream,Wff2),
		   conjoin(Wff1,Wff2,Wff)).

read_matrix(File,Wff) :-
	open(File,read,Stream),
	read_matrix_loop(Stream,Wff),
	close(Stream).

read_matrix_loop(Stream,Matrix) :-
	read(Stream,Elem),
	(Elem == end_of_file ->
	           Matrix = [];
	%true                ->
		   read_matrix_loop(Stream,L),
		   Matrix = [Elem|L]).

% writes a compiled knowledge base consisting of contrapositives only
% to the file 'Name.ckb'

write_ckb(File,KB) :-
	concat_atom(File,'.ckb',KBFile),
	open(KBFile,write,KBStream),
	concat_atom(File,'.que',QFile),
	open(QFile,write,QStream),
        write_contrapositives(streams(KBStream,QStream),KB),
        close(KBStream),
        close(QStream),
	get_file_info(KBFile,size,KBFileSize),
	get_file_info(QFile,size,QFileSize),
	nl,nl,
	write(KBFile),write(' written '),write(KBFileSize),format(' bytes'),
	write(QFile), write(' written '),write(QFileSize), format(' bytes'),
	!.

write_cmm(File,Matrix) :-
	concat_atom(File,'.cmm',MFile),
	open(MFile,write,MStream),
        write_matrix(MStream,Matrix),
        close(MStream),
	!.

write_query(File,Query) :-
	concat_atom(File,'.que',QFile),
	open(QFile,write,QStream),
        write_query_only(QStream,Query),
        close(QStream),
	!.

write_query_only(Stream,(A,B)) :-
	!,
        write_query_only(Stream,A),
        write_query_only(Stream,B).
write_query_only(Stream,(A:-B)) :-
	functor(A,query,_) ->
		write_clauses(Stream,(A:-B));
	%true ->
		true.


write_contrapositives(Streams,(A,B)) :-
	!,
        write_contrapositives(Streams,A),
        write_contrapositives(Streams,B).
write_contrapositives(streams(KBStream,QStream),(A:-B)) :-
	functor(A,query,_) ->
		write_clauses(QStream,(A:-B));
	%true ->
		write_clauses(KBStream,(A:-B)).	


write_clauses(Stream,(A,B)) :-
        write_clauses(Stream,A),
        write_clauses(Stream,B),
        !.
write_clauses(Stream,A) :-
        write(Stream,A),
        write(Stream,.),
        nl(Stream),
        !.
write_clauses(A) :-
	File = 'temp.pll',
	open(File,write,Stream),
	write_clauses(Stream,A),
	close(Stream).


write_matrix(Stream,[]) :-
	nl(Stream),
        !.
write_matrix(Stream,[E|L]) :-
        write(Stream,E),
        write(Stream,.),
        nl(Stream),
	write_matrix(Stream,L),
        !.
write_matrix(L) :-
	File = 'temp.pll',
	open(File,write,Stream),
	write_matrix(Stream,L),
	close(Stream).


compile_ckb(File) :-	
	concat_atom(File,'.ckb',KBFile),
	compile(KBFile).

compile_query(File) :-	
	concat_atom(File,'.que',QFile),
	compile(QFile).

ask(Name,Query) :-
	(variables(Query,[]) ->
	         classical_clauses(Query,Q0),
		 cnf(Q0,Q1),
		 make_matrix(Q1,Q2),
		 matrix_reduction(Q2,Q);
	%true ->
		 Q = []),

	concat_atom(Name,'.cmm',MFile),
	read_matrix(MFile,Matrix),
	
	dpttp1((query:-Query),Q,Matrix,Query1:Matrix),

	nl,
        write('XRay writing query ... '),
        write_query(Name,Query1),
	write('done.'),

	nl,
        write('XRay compiling query ... '),
        compile_query(Name),
	write('done.'),
        nl,
        !.

tell(Name,Wff) :-	
	read_kb(Name,KB),
	conjoin(Wff,KB,NewKB),
	dpttp(Name,NewKB).

write_proved(Proof,ProofEnd) :-
        write('proved'),
	verbose_flag ->
	        write(' by:'),
		write_proof(Proof,ProofEnd);
	%true->
		length(Proof,X),
		write(' qed ;-) ':X).

write_proof(Proof,ProofEnd) :-
        Proof == ProofEnd,
        !.
write_proof([X|Y],ProofEnd) :-
	nl,
        write(' '),
        write(X),
        write_proof(Y,ProofEnd).

%%% SETTINGS for LEMMA HANDLING
%%%
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
	    verbose('Lemmatization ':Lemma);
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
	verbose('Using static lemma code: ':lemmatize),
        !,
        remove_reductions(Proof,ProofOut),
        default_assumptions(ProofOut,ProofEnd,Ass),
        lemmatize_dynamically(gamma(N,Gamma),false,Ass).
%        lemmatize_statically(gamma(N,Gamma),false,Ass).
lemmatize(extension(_N:Goal),Proof,ProofOut,ProofEnd) :-
	verbose('Using static lemma code: ':lemmatize),
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
        concat_atom(File,'.lem',LFile),
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
        retractAllProlog(dynamic_lemma(_,_,_)).
remove_static_lemmas :-
        retractAllProlog(static_lemma(_,_,_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 13/07/96   File: lemma:config.pll              %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 13/07/96 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemma:config.pll                                           %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_lemmatization_p(Head :- Body) :-
	lemma_flag,
	!,
	(functor(Head,query,_) -> fail;
         functor(Head,alpha,_) -> fail;
	 functor(Head,gamma,_) -> lemma_type_parameter(delta);
	 true ->                  lemma_type_parameter(omega)).

dynamic_lemma_test_p(P,N) :-
	dynamic_lemma_flag,
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
	 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 13/07/96   File: lemmaflex.pll
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 11/07/96 Created                                                          %%
%% 13/07/96 added compilation
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemmaflex.pll                                              %%
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
	BodyI = verbose('lemmatize/4: Ignoring inference':Inference),
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
	(Mode = (dynamic) ; Mode = dystatic),
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
	concat_atom(Name,'.lem',LFile),
	read_clauses(LFile,Wff).	
write_lem(File,LemmaProcs) :-
	concat_atom(File,'.lem',LemmaFile),
	open(LemmaFile,write,LemmaStream),
        write_clauses(LemmaStream,LemmaProcs),
        close(LemmaStream),
	!.
compile_lem(File) :-	
	concat_atom(File,'.lem',KBFile),
	compile(KBFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 28/06/96   File: model.pll
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 26/06/95 Created                                                          %%
%% 25/06/96 moved compatible from defaults.pll
%% 28/06/96 updated cnf,make_matrix, make_clause
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog model.pll                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%% ----------------------------------------------------------------------
%%% model_initialization/2
%%%     generates an initial model

model_initialization(Matrix,cmm(Model,NewMatrix)) :-
	model_generation(matrix([],Matrix),NewMatrix,Model),
%	display_cmm('INITIALIZATION',cmm(Model,NewMatrix)),
	!.

%%% Runtime predicate for consistency checking
%%%

compatible(MatrixJ,cmm(Model,ModelMatrix),cmm(NewModel,NewModelMatrix)) :-
	satisfy_matrix(MatrixJ,Model,NewModel) ->
	        append_matrix(MatrixJ,ModelMatrix,NewModelMatrix),
%		display_cmm('CHECKING - satisfy':MatrixJ,cmm(NewModel,NewModelMatrix)),
		verbose('+ satisfiability check');
	adjoin_matrix(MatrixJ,ModelMatrix,ModelMatrix1) ->
%	        display_cmm('CHECKING - generation I':MatrixJ,cmm(Model,ModelMatrix)),
		model_generation(ModelMatrix1,NewModelMatrix,NewModel);
%		display_cmm('CHECKING - generation II':MatrixJ,cmm(NewModel,NewModelMatrix));
	%true ->
%		display_cmm('CHECKING - failure':MatrixJ,cmm(Model,ModelMatrix)),
		verbose('- compatible *failure*'),
		fail.

%%% ----------------------------------------------------------------------
%%% satisfy_matrix/3
%%%     checks whether a given model, ModelI, satisfies
%%%     a formula in matrix form, Matrix, or whether the model
%%%     can be extended to a model, ModelO
%%%

satisfy_matrix([],Model,Model).
satisfy_matrix([Clause],ModelI,ModelO) :-
	!,
	satisfy_clause(Clause,ModelI,ModelO).
satisfy_matrix([C1,C2],ModelI,ModelO) :-
	!,
	satisfy_clause(C1,ModelI,Model1),
	satisfy_clause(C2,Model1,ModelO).
satisfy_matrix([C1,C2,C3|M],ModelI,ModelO) :-
	satisfy_clause(C1,ModelI,Model1),
	satisfy_clause(C2,Model1,Model2),
	satisfy_clause(C3,Model2,Model3),
	satisfy_matrix(M,Model3,ModelO).

satisfy_clause([],_,_) :- 
	!,
	fail.
satisfy_clause([L],ModelI,ModelO) :-
	!,
	satisfy_literal(L,ModelI,ModelO).
satisfy_clause([L1,L2],ModelI,ModelO) :-
	satisfy_literal(L1,ModelI,ModelO);
	satisfy_literal(L2,ModelI,ModelO).
satisfy_clause([L1,L2,L3|C],ModelI,ModelO) :-
	satisfy_literal(L1,ModelI,ModelO);
	satisfy_literal(L2,ModelI,ModelO);
	satisfy_literal(L3,ModelI,ModelO);
	satisfy_clause(C,ModelI,ModelO).

satisfy_literal(L,ModelI,ModelO) :-
        identical_member(L,ModelI) ->
	       ModelO = ModelI;
	%true  ->
	       negated_literal(L,NegL),
	       \+ identical_member(NegL,ModelI),
	       ModelO = [L|ModelI].


%%% ----------------------------------------------------------------------
%%% model_generation/2

model_generation(matrix(Units,Matrix),matrix(Units2,M1),Model) :-
	unit_extraction(Matrix,M1,Units1),
%	format(unit_extraction),display_matrix(Matrix),display_matrix(M1),display_units(Units1),
	dp(M1,Model1),
	append(Units,Units1,Units2),
/*	union(Units,Units1,Units2), append allows for multiple occurrences */
	append(Units2,Model1,Model),
	verbose('* model generation').

dp([],[]) :- !.
dp(M,_) :-
	member([],M),
	!,
	fail.
dp(M,NewMod) :-
	M=[[L|_]|_],
	split(M,L,M1,M2),
	unit_extraction(M1,M3,U3),
	unit_extraction(M2,M4,U4),
	(dp(M3,Mod),
         KK = [L|U3];
	 dp(M4,Mod),
         negated_literal(L,K),
	 KK = [K|U4]),
        append(KK,Mod,NewMod).


split([],_,[],[]).
split([C|M],L,M3,M4) :-
	split(M,L,M1,M2),
	negated_literal(L,NegL),
	(select(L,C,RestC) ->
	      M3=M1,
	      M4=[RestC|M2];
	select(NegL,C,RestC) ->
	      M3=[RestC|M1],
	      M4=M2;
        %true ->
	      M3=[C|M1],
	      M4=[C|M2]).

%%% ----------------------------------------------------------------------
%%% cnf/2

cnf(NNF,CNF) :-
	NNF = (F1,F2) ->
	    cnf(F1,CNF1),
	    cnf(F2,CNF2),
	    conjoin(CNF1,CNF2,CNF);
        NNF = (F1;(F2,F3)) ->
	    cnf((F1;F2),CNF1),
	    cnf((F1;F3),CNF2),
	    conjoin(CNF1,CNF2,CNF);
        NNF = ((F1,F2);F3) ->
	    cnf((F1;F3),CNF1),
	    cnf((F2;F3),CNF2),
	    conjoin(CNF1,CNF2,CNF);
	NNF = (F1;F2) ->
	    cnf(F1,CNF1),
	    cnf(F2,CNF2),
            (cnf_p(CNF1,CNF2,F1,F2) ->
	           disjoin(CNF1,CNF2,CNF);
	    %true ->
		   disjoin(CNF1,CNF2,CNF12),
	           cnf(CNF12,CNF));
	%true ->
	    NNF=CNF.

cnf_p(CNF1,CNF2,F1,F2) :-
	(F1=(_,_);F2=(_,_)) ->
	         fail;
		 F1=CNF1,F2=CNF2.

make_matrix(Wff,Matrix) :-
	Wff = (A,B) ->
	      make_matrix(A,MA),
	      make_matrix(B,MB),
	      append(MA,MB,Matrix);
	Wff = true ->
	      Matrix = [];
	Wff = false ->
	      Matrix = [[]];
	%true ->
	      make_clause(Wff,Clause),
	      Matrix=[Clause].

make_clause(Wff,Clause) :-
	Wff = (A;B) ->
	      make_clause(A,CA),
	      make_clause(B,CB),
	      append(CA,CB,Clause);
	%true ->
	      Clause=[Wff].

%%% ----------------------------------------------------------------------
%%% combine_clauses\3
%%%       merges two CNFs into one while simplifying the resulting CNF
%%%       CARE: C1 is supposed to be smaller than C2

combine_clauses(C,[],C) :-
	!,
	verbose('  trivial combination').
combine_clauses([[L]],C1,[[L]|C2]) :-
	!,
	verbose('  1 unit combination'),
	simplify(L,C1,C2).	
combine_clauses([[L1],[L2]],C1,[[L1],[L2]|C3]) :-
	!,
	verbose('  2 unit combinations'),
	simplify(L1,C1,C2),
	simplify(L2,C2,C3).
combine_clauses(C1,C2,C) :-	
	verbose('  general combination'),
	append(C1,C2,C3),
	unit_reduction(C3,C4),
	subs_reduction(C4,C).

%%% ----------------------------------------------------------------------
%%% adjoin/3
%%%
/* adjoin guarantees         that the new information
          is satisfied by the current unit clauses
   adjoin does not guarantee that the new information 
                                  together with the current matrix
          is satisfied by the current unit clauses
   So you better watch for new unit clauses stemming from reducing the
   new matrix

   Nonetheless: whenever a unit is added to the unit list, this literal
   has been removed (by simplify) from the matrix.
*/
      
adjoin_matrix([[L]],matrix(Units,_),_) :-
	verbose('  1 unit adjunction *failure*'),
	negated_literal(L,NegL),
	identical_member(NegL,Units),
	!,fail.
adjoin_matrix([[L]],matrix(Units,Matrix1),matrix([L|Units],Matrix2)) :-
	!,
	verbose('  1 unit adjunction'),
	simplify(L,Matrix1,Matrix2).
adjoin_matrix([[L1],[L2]],matrix(Units,_),_) :-
						% supposition not(negated_literal(L1,L2))
	verbose('  2 unit adjunction *failure*'),
	(negated_literal(L1,NegL) ; negated_literal(L2,NegL)),
	identical_member(NegL,Units),
	!,fail.
adjoin_matrix([[L1],[L2]],matrix(Units,Matrix1),matrix([L1,L2|Units],Matrix3)) :-
						% supposition not(negated_literal(L1,L2))
	!,
	verbose('  2 unit adjunctions'),
	simplify(L1,Matrix1,Matrix2),
	simplify(L2,Matrix2,Matrix3).
adjoin_matrix(Matrix1,matrix(Units,Matrix2),matrix(Units,Matrix3)) :-
	satisfy_matrix(Matrix1,Units,_) ->
	       	verbose('  full adjunction'),
		append(Matrix1,Matrix2,Matrix3);
	% true ->
		verbose('  weak satisfaction *failure*'),
		fail.


append_matrix([[L]],matrix(Units,Matrix),matrix(Units,[[L]|Matrix])) :-
						% allows for multiple occurrences
	verbose('  1 unit appendage'),
	!.
append_matrix([[L1],[L2]],matrix(Units,Matrix),matrix(Units,[[L1],[L2]|Matrix])) :-
						% allows for multiple occurrences
	verbose('  2 unit appendage'),
	!.
append_matrix(Matrix1,matrix(Units,Matrix2),matrix(Units,Matrix3)) :-
	verbose('  full appendage'),
	append(Matrix1,Matrix2,Matrix3).

%%% ----------------------------------------------------------------------
%%% matrix_reduction/2
%%%      bunch of matrix reductions
%%%

matrix_reduction(C1,C) :-
	taut_reduction(C1,C2),
	mult_reduction(C2,C3),
	unit_reduction(C3,C4),
	subs_reduction(C4,C).

%%% ----------------------------------------------------------------------
%%% unit_reduction/2
%%%      unit reduction
%%%

unit_reduction(M,[[L]|M1]) :-
	member([L],M),
	!,
	simplify(L,M,M2),
	unit_reduction(M2,M1).
unit_reduction(M,M).

%%% unit_extraction/3 
%%%        is a special purpose reduction for model-finding
%%%          2nd arg gives matrix without unit clauses
%%%          3rd arg gives literals in unit clauses

unit_extraction(M,M1,[L|R]) :-
	member([L],M),
	!,
	simplify(L,M,M2),
	unit_extraction(M2,M1,R).
unit_extraction(M,M,[]).


simplify(_L, [], [] ) .
simplify( L, [C|Cs] , NewCs ) :-
	member(L,C),
	!,
	simplify(L,Cs,NewCs).
simplify( L, [C|Cs], [NewC|NewCs] ) :-
	negated_literal(L,NegL),
	select(NegL,C,NewC),
	!,
	simplify(L,Cs,NewCs).
simplify( L, [C|Cs], [C|NewCs]    ) :-
	simplify(L,Cs,NewCs).

%%% ----------------------------------------------------------------------
%%% subs_reduction/2
%%%      subs reduction
%%%

subs_reduction([],[]).
subs_reduction([C1|M1],M) :-
	subs_reduction(M1,M2),
	(subsumes(C1,M2,M3) ->
	    M = [C1|M3];
	%true ->
	    M = M2).

subsumes(_,[],[]).
subsumes(C1,[C2|_],_) :-
	subset(C2,C1),
	!,
	fail.
subsumes(C,[C1|M1],M) :-
	subsumes(C,M1,M2),
	!,
	(subset(C,C1) ->
	        M=M2;
	%true ->
		M=[C1|M2]).

%%% ----------------------------------------------------------------------
%%% taut_reduction/2
%%%      taut reduction
%%%

taut_reduction([],[]) :- !.
taut_reduction([C|M1],M2) :-
	taut_reduction(M1,M3),
	(taut_clause(C) ->
	    M2 = M3;
	%true ->
	    M2 = [C|M3]
	).

taut_clause(C) :-
	member(L,C),
	negated_literal(L,K),
	member(K,C).
	
%%% ----------------------------------------------------------------------
%%% mult_reduction/2
%%%     mult reduction
%%%

mult_reduction([],[]).
mult_reduction([C|M1],[NewC|M3]) :-
	mult_reduction(M1,M3),
	remove_dups(C,NewC).

remove_dups([],[]).
remove_dups([L|RestC],NewC) :-
	remove_dups(RestC,NewRestC),
	(member(L,NewRestC) ->
	    NewC = NewRestC;
	%true ->
	    NewC = [L|NewRestC]
	).





%%% Sound unification.
%%%
%%% `add_sound_unification' transforms a clause so that its
%%% head has no repeated variables.  Unifying a goal with
%%% the clause head can then be done soundly without the occurs
%%% check.  The rest of the unification can then be done in
%%% the body of the transformed clause, using the sound `unify'
%%% predicate.
%%%
%%% For example,
%%%    p(X,Y,f(X,Y)) :- true.
%%% is transformed into
%%%    p(X,Y,f(X1,Y1)) :- unify(X,X1), unify(Y,Y1).

add_sound_unification((Head :- Body),(Head1 :- Body1)) :-
        linearize(Head,Head1,[],_,true,Matches),
        conjoin(Matches,Body,Body1).

linearize(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
        nonvar(TermIn) ->
                functor(TermIn,F,N),
                functor(TermOut,F,N),
                linearize_args(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,1,N);
        identical_member(TermIn,VarsIn) ->
                VarsOut = VarsIn,
                conjoin(MatchesIn,unify(TermIn,TermOut),MatchesOut);
        %true ->
                TermOut = TermIn,
                VarsOut = [TermIn|VarsIn],
                MatchesOut = MatchesIn.

linearize_args(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
        I > N ->
                VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        %true ->
                arg(I,TermIn,ArgI),
                linearize(ArgI,NewArgI,VarsIn,Vars1,MatchesIn,Matches1),
                arg(I,TermOut,NewArgI),
                I1 is I + 1,
                linearize_args(TermIn,TermOut,Vars1,VarsOut,Matches1,MatchesOut,I1,N).

%%% Sound unification algorithm with occurs check that is called
%%% by code resulting from the `add_sound_unification' transformation.
%%% This should be coded in a lower-level language for efficiency.

unify(X,Y) :- unify_with_occurs_check(X,Y).


%%% Depth-first iterative-deepening search.
%%%
%%% `add_complete_search' adds arguments DepthIn and DepthOut
%%% to each PTTP literal to control bounded depth-first
%%% search.  When a literal is called,
%%% DepthIn is the current depth bound.  When
%%% the literal exits, DepthOut is the new number
%%% of levels remaining after the solution of
%%% the literal (DepthIn - DepthOut is the number
%%% of levels used in the solution of the goal.)
%%%
%%% For clauses with empty bodies or bodies
%%% composed only of builtin functions,
%%% DepthIn = DepthOut.
%%%
%%% For other clauses, the depth bound is
%%% compared to the cost of the body.  If the
%%% depth bound is exceeded, the clause fails.
%%% Otherwise the depth bound is reduced by
%%% the cost of the body.
%%%
%%% p :- q , r.
%%% is transformed into
%%% p(DepthIn,DepthOut) :-
%%%     DepthIn >= 2, Depth1 is DepthIn - 2,
%%%     q(Depth1,Depth2),
%%%     r(Depth2,DepthOut).
%%%
%%% p :- q ; r.
%%% is transformed into
%%% p(DepthIn,DepthOut) :-
%%%     DepthIn >= 1, Depth1 is DepthIn - 1,
%%%     (q(Depth1,DepthOut) ; r(Depth1,DepthOut)).

add_complete_search((Head :- Body),(Head1 :- Body1)) :-
        Head =.. L,
        append(L,[DepthIn,DepthOut],L1),
        Head1 =.. L1,
        (functor(Head,query,_) ->
                add_complete_search_args(Body,DepthIn,DepthOut,Body1);
        nonzero_search_cost(Body,Cost) ->
                add_complete_search_args(Body,Depth1,DepthOut,Body2),
                conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),Body2,Body1);
        %true ->
                add_complete_search_args(Body,DepthIn,DepthOut,Body1)).

add_complete_search_args(Body,DepthIn,DepthOut,Body1) :-
        Body = (A , B) ->
                add_complete_search_args(A,DepthIn,Depth1,A1),
                add_complete_search_args(B,Depth1,DepthOut,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                search_cost(A,CostA),
                search_cost(B,CostB),
                (CostA < CostB ->
                        add_complete_search_args(A,DepthIn,DepthOut,A1),
                        add_complete_search_args(B,Depth1,DepthOut,B2),
                        Cost is CostB - CostA,
                        conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),B2,B1);
                CostA > CostB ->
                        add_complete_search_args(A,Depth1,DepthOut,A2),
                        add_complete_search_args(B,DepthIn,DepthOut,B1),
                        Cost is CostA - CostB,
                        conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),A2,A1);
                %true ->
                        add_complete_search_args(A,DepthIn,DepthOut,A1),
                        add_complete_search_args(B,DepthIn,DepthOut,B1)),
                disjoin(A1,B1,Body1);
        Body = search(Goal,Max,Min,Inc) ->
                PrevInc is Min + 1,
                add_complete_search_args(Goal,DepthIn1,DepthOut1,Goal1),
                DepthIn = DepthOut,
                Body1 = search(Goal1,Max,Min,Inc,PrevInc,DepthIn1,DepthOut1);
        Body = search(Goal,Max,Min) ->
                add_complete_search_args(search(Goal,Max,Min,1),DepthIn,DepthOut,Body1);
        Body = search(Goal,Max) ->
                add_complete_search_args(search(Goal,Max,0),DepthIn,DepthOut,Body1);
        Body = search(Goal) ->
                add_complete_search_args(search(Goal,1000000),DepthIn,DepthOut,Body1);
        functor(Body,search_cost,_) ->
                DepthIn = DepthOut,
                Body1 = true;
        builtin(Body) ->
                DepthIn = DepthOut,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[DepthIn,DepthOut],L1),
                Body1 =.. L1.

nonzero_search_cost(Body,Cost) :-
        search_cost(Body,Cost),
        Cost > 0.

%%% Search cost is computed by counting literals in the body.
%%% It can be given explicitly instead by including a number, as in
%%%   p :- search_cost(3).     (ordinarily, cost would be 0)
%%%   p :- search_cost(1),q,r. (ordinarily, cost would be 2)
%%%   p :- search_cost(0),s.   (ordinarily, cost would be 1)
%%%
%%% Propositional goals are not counted into the search cost so
%%% that fully propositional problems can be solved without
%%% deepening when iterative-deepening search is used.

search_cost(Body,N) :-
        Body = search_cost(M) ->
                N = M;
        Body = (A , B) ->
                (A = search_cost(M) ->  % if first conjunct is search_cost(M),
                        N = M;          % search cost of the entire conjunction is M
                %true ->
                        search_cost(A,N1),
                        search_cost(B,N2),
                        N is N1 + N2);
        Body = (A ; B) ->
                search_cost(A,N1),
                search_cost(B,N2),
                min(N1,N2,N);
        builtin(Body) ->
                N = 0;
        functor(Body,_,2) ->  % zero-cost 2-ary (0-ary plus ancestor lists) predicates
                N = 0;        % heuristic for propositional problems
        %true ->
                N = 1.

%%% Depth-first iterative-deepening search can be
%%% specified for a goal by wrapping it in a call
%%% on the search predicate:
%%%    search(Goal,Max,Min,Inc)
%%% Max is the maximum depth to search (defaults to a big number),
%%% Min is the minimum depth to search (defaults to 0),
%%% Inc is the amount to increment the bound each time (defaults to 1).
%%%
%%% Depth-first iterative deepening search can be
%%% specified inside the PTTP formula by compiling
%%%   query :- search(p(b,a,c),Max,Min,Inc)
%%% and executing
%%%   query.
%%% or directly by the user by compiling
%%%   query :- p(b,a,c))
%%% and executing
%%%   search(query,Max,Min,Inc).
%%%
%%% The search(Goal,Max,Min,Inc) predicate adds
%%% DepthIn and DepthOut arguments to its goal argument.

search(Goal,Max,Min,Inc) :-
        PrevInc is Min + 1,
        add_complete_search_args(Goal,DepthIn,DepthOut,Goal1),
        (compile_proof_printing ->
                add_proof_recording_args(Goal1,_Proof,_ProofEnd,Goal2);
        %true ->
                Goal2 = Goal1),
        !,
        search(Goal2,Max,Min,Inc,PrevInc,DepthIn,DepthOut).

search(Goal,Max,Min) :-
        search(Goal,Max,Min,1).

search(Goal,Max) :-
        search(Goal,Max,0).

search(Goal) :-
        search(Goal,1000000).

%%% Actual search driver predicate.
%%% Note that depth-bounded execution of Goal is enabled by
%%% the fact that the DepthIn and DepthOut arguments of
%%% search are also the DepthIn and DepthOut arguments of Goal.

search(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn,_DepthOut) :-
        Min > Max,
        !,
        fail.
search(Goal,_Max,Min,_Inc,PrevInc,DepthIn,DepthOut) :-
        trace_search_progress_pred(P1),
        L1 =.. [P1,Min],
        call(L1),
        DepthIn = Min,
        call(Goal),
        DepthOut < PrevInc.   % fail if this solution was found in previous search
search(Goal,Max,Min,Inc,_PrevInc,DepthIn,DepthOut) :-
        Min1 is Min + Inc,
        search(Goal,Max,Min1,Inc,Inc,DepthIn,DepthOut).

%%% Complete inference.
%%%
%%% Model elimination reduction operation and
%%% identical ancestor goal pruning.
%%%
%%% Two arguments are added to each literal, one
%%% for all the positive ancestors, one for all
%%% the negative ancestors.
%%%
%%% Unifiable membership is checked in the list 
%%% of opposite polarity to the goal
%%% for performing the reduction operation.
%%%
%%% Identity membership is checked in the list
%%% of same polarity as the goal
%%% for performing the ancestor goal pruning operation.
%%% This is not necessary for soundness or completeness,
%%% but is often effective at substantially reducing the
%%% number of inferences.
%%%
%%% The current head goal is added to the front
%%% of the appropriate ancestor list during the
%%% call on subgoals in bodies of nonunit clauses.

add_ancestor((Head :- Body),(Head1 :- Body1)) :-
        functor(Head,query,_) ->
                Head1 = Head,
                add_ancestor_args(Body,[[],[]],Body1);
        %true ->
                Head =.. L,
                append(L,[PosAncestors,NegAncestors],L1),
                Head1 =.. L1,
                add_ancestor_args(Body,[NewPosAncestors,NewNegAncestors],Body2),
                (Body == Body2 ->
                        Body1 = Body2;
                negative_literal(Head) ->
                        NewPosAncestors = PosAncestors,
                        conjoin((NewNegAncestors = [Head|NegAncestors]),Body2,Body1);
                %true ->
                        NewNegAncestors = NegAncestors,
                        conjoin((NewPosAncestors = [Head|PosAncestors]),Body2,Body1)).

add_ancestor_args(Body,AncestorLists,Body1) :-
        Body = (A , B) ->
                add_ancestor_args(A,AncestorLists,A1),
                add_ancestor_args(B,AncestorLists,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_ancestor_args(A,AncestorLists,A1),
                add_ancestor_args(B,AncestorLists,B1),
                disjoin(A1,B1,Body1);
        Body =.. [search,Goal|L] ->
                add_ancestor_args(Goal,AncestorLists,Goal1),
                Body1 =.. [search,Goal1|L];
        builtin(Body) ->
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,AncestorLists,L1),
                Body1 =.. L1.

ancestor_tests(P,N,Result) :-
        P == query ->
                Result = true;
        %true ->
                negated_functor(P,NotP),
                N2 is N - 2,            % N - 2 due to two ancestor-list arguments
                functor(Head1,P,N2),
                Head1 =.. [P|Args1],
                Head2 =.. [NotP|Args1],
                append(Args1,[PosAncestors,NegAncestors],Args),
                Head =.. [P|Args],
                (negative_functor(P) ->
                        C1Ancestors = NegAncestors, C2Ancestors = PosAncestors;
                %true ->
                        C1Ancestors = PosAncestors, C2Ancestors = NegAncestors),
                C1 = (Head :- identical_member(Head1,C1Ancestors), !, fail),
                count_inferences_pred(IncNcalls),
                (N2 = 0 ->              % special case for propositional calculus
                        conjoin((identical_member(Head2,C2Ancestors) , !),IncNcalls,V);
                %true ->
                        conjoin(unifiable_member(Head2,C2Ancestors),IncNcalls,V)),
                (compile_proof_printing ->
                        conjoin(V,infer_by(red),V1);
                %true ->
                        V1 = V),
                C2 = (Head :- V1),
                conjoin(C1,C2,Result).

procedures_with_ancestor_tests([[P,N]|Preds],Clauses,Procs) :-
        procedure(P,N,Clauses,Proc1),
        ancestor_tests(P,N,Tests),
        conjoin(Tests,Proc1,Proc),
        procedures_with_ancestor_tests(Preds,Clauses,Procs2),
        conjoin(Proc,Procs2,Procs).
procedures_with_ancestor_tests([],_Clauses,true).


%%% Proof Printing.
%%%
%%% Add extra arguments to each goal so that information
%%% on what inferences were made in the proof can be printed
%%% at the end.

add_proof_recording((Head :- Body),(Head1 :- Body1)) :-
        Head =.. L,
        append(L,[Proof,ProofEnd],L1),
        Head1 =.. L1,
        add_proof_recording_args(Body,Proof,ProofEnd,Body2),
        (functor(Head,query,_) ->
                conjoin(Body2,write_proved(Proof,ProofEnd),Body1);
        %true ->
                Body1 = Body2).

add_proof_recording_args(Body,Proof,ProofEnd,Body1) :-
        Body = (A , B) ->
                add_proof_recording_args(A,Proof,Proof1,A1),
                add_proof_recording_args(B,Proof1,ProofEnd,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_proof_recording_args(A,Proof,ProofEnd,A1),
                add_proof_recording_args(B,Proof,ProofEnd,B1),
                disjoin(A1,B1,Body1);
        Body =.. [search,Goal|L] ->
                add_proof_recording_args(Goal,Proof,ProofEnd,Goal1),
                Body1 =.. [search,Goal1|L];
        Body = infer_by(X) ->
                Body1 = (Proof = [X|ProofEnd]);
        Body = fail ->
                Body1 = Body;
        builtin(Body) ->
                Proof = ProofEnd,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[Proof,ProofEnd],L1),
                Body1 =.. L1.

write_proved(Proof,ProofEnd) :-
        write('proved by'),
        write_proof(Proof,ProofEnd).

write_proof(Proof,ProofEnd) :-
        Proof == ProofEnd,
        !.
write_proof([X|Y],ProofEnd) :-
        write(' '),
        write(X),
        write_proof(Y,ProofEnd).

/*
%%% Negation normal form to Prolog clause translation.
%%% Include a literal in the body of each clause to
%%% indicate the number of the formula the clause came from.

clauses((A , B),L,WffNum) :-
        !,
        clauses(A,L1,WffNum),
        WffNum2 is WffNum + 1,
        clauses(B,L2,WffNum2),
        conjoin(L1,L2,L).
clauses(A,L,WffNum) :-
        head_literals(A,Lits),
        clauses(A,Lits,L,WffNum).

clauses(A,[Lit|Lits],L,WffNum) :-
        body_for_head_literal(Lit,A,Body1),
        (compile_proof_printing ->
                conjoin(infer_by(WffNum),Body1,Body);
        %true ->
                Body = Body1),
        clauses(A,Lits,L1,WffNum),
        conjoin((Lit :- Body),L1,L).
clauses(_,[],true,_).

*/
head_literals(Wff,L) :-
        Wff = (A :- B) ->               % contrapositives are not formed for A :- ... inputs
                head_literals(A,L);
        Wff = (A , B) ->
                head_literals(A,L1),
                head_literals(B,L2),
                union(L1,L2,L);
        Wff = (A ; B) ->
                head_literals(A,L1),
                head_literals(B,L2),
                union(L1,L2,L);
        %true ->
                L = [Wff].

body_for_head_literal(Head,Wff,Body) :-
        Wff = (A :- B) ->
                body_for_head_literal(Head,A,A1),
                conjoin(A1,B,Body);
        Wff = (A , B) ->
                body_for_head_literal(Head,A,A1),
                body_for_head_literal(Head,B,B1),
                disjoin(A1,B1,Body);
        Wff = (A ; B) ->
                body_for_head_literal(Head,A,A1),
                body_for_head_literal(Head,B,B1),
                conjoin(A1,B1,Body);
        Wff == Head ->
                Body = true;
        negated_literal(Wff,Head) ->
                Body = false;
        %true ->
                negated_literal(Wff,Body).

%%% predicates returns a list of the predicates appearing in a formula.

predicates(Wff,L) :-
        Wff = (A :- B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        functor(Wff,search,_) ->        % list predicates in first argument of search
                arg(1,Wff,X),
                predicates(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                functor(Wff,F,N),
                L = [[F,N]].

%%% procedure returns a conjunction of the clauses
%%% with head predicate P/N.

procedure(P,N,Clauses,Proc) :-
        Clauses = (A , B) ->
                procedure(P,N,A,ProcA),
                procedure(P,N,B,ProcB),
                conjoin(ProcA,ProcB,Proc);
        (Clauses = (A :- B) , functor(A,P,N)) ->
                Proc = Clauses;
        %true ->
                Proc = true.

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


bhook1_p(Head :- Body) :-
	Body = (nl,write(_),nl,fail) ->
	     /* eliminates predicate hooks */
	     false;
	functor(Head,query,_) ->
	     false;
	%true ->
	     true.
bhook1(Head :- Body) :-
	functor(Head,gamma,_)  ->
	     Head =.. [Pred,Arg1,Arg2|_],
	     nl,write(trial:gamma(Arg1,Arg2)),nl;
        functor(Head,alpha,_)  ->
	     Head =.. [Pred,Arg1,Arg2|_],
	     nl,write(trial:alpha(Arg1,Arg2)),nl;
        Head = _ ->
	     Head =.. [Pred|_],
	     nl,write(trial:(Pred)),nl;
        %true ->
	     true.

bhook2_p(Head :- Body) :-
	bhook1_p(Head :- Body).
bhook2(Head :- Body) :-
	functor(Head,gamma,_)  ->
	     Head =.. [Pred,Arg1,Arg2|_],
	     nl,write(success:gamma(Arg1,Arg2)),nl;
        functor(Head,alpha,_)  ->
	     Head =.. [Pred,Arg1,Arg2|_],
	     nl,write(success:alpha(Arg1,Arg2)),nl;
        Head = _ ->
	     Head =.. [Pred|_],
	     nl,write(success:(Pred)),nl;
        %true ->
	     true.

phook1_p(P,N) :-
	P == query ->
                false;
	%true ->
		true.
phook2_p(P,N) :-
	phook1_p(P,N).
phook3_p(P,N) :-
	phook1_p(P,N).


phook1_tests(P,N,Result) :-
	phook1_p(P,N),
	!,
	head3(P,N,Head,Head3),
	(P = gamma ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(enter*predicate:gamma(Arg1,Arg2)),nl,fail);
	 P = alpha ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(enter*predicate:alpha(Arg1,Arg2)),nl,fail);
	%true ->
	    Body=(nl,write(enter*predicate:Head3),nl,fail)),
	Result = (Head :- Body).
phook1_tests(_,_,true).

phook2_tests(P,N,Result) :-
	phook2_p(P,N),
	!,
	head3(P,N,Head,Head3),
	(P = gamma ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(end_of_tests*predicate:gamma(Arg1,Arg2)),nl,fail);
	 P = alpha ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(end_of_tests*predicate:alpha(Arg1,Arg2)),nl,fail);
	%true ->
	    Body=(nl,write(end_of_tests*predicate:Head3),nl,fail)),
	Result = (Head :- Body).
phook2_tests(_,_,true).

phook3_tests(P,N,Result) :-
	phook3_p(P,N),
	!,
	head3(P,N,Head,Head3),
	(P = gamma ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(failure*predicate:gamma(Arg1,Arg2)),nl,fail);
	 P = alpha ->
	    Head =.. [Pred,Arg1,Arg2|_],
	    Body=(nl,write(failure*predicate:alpha(Arg1,Arg2)),nl,fail);
	%true ->
	    Body=(nl,write(failure*predicate:Head3),nl,fail)),
	Result = (Head :- Body).
phook3_tests(_,_,true).

head3(P,N,Head,Head3) :-
	P == query ->
                Head = query;
	%true ->
		N3 is N - 3,
                functor(Head3,P,N3),
                Head3 =.. [P|Args3],
                append(Args3,[_,_,_],Args),
                Head =.. [P|Args].
add_herbrand_preds((Head :- Body),(Head :- Body1)) :-
	herbrandize_variables(Body,[],BodyVars,false,_),
	herbrandize_variables(Head,BodyVars,_,true,Matches),
        conjoin(Matches,Body,Body1).
	

herbrandize_variables(Term,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
        builtin(Term) ->
	        VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        %true ->
	        nonvar(Term) ->
                       functor(Term,_,N),
		       herbrandize_args(Term,VarsIn,VarsOut,MatchesIn,MatchesOut,1,N);
	        identical_member(Term,VarsIn) ->
	               VarsOut = VarsIn,
		       MatchesOut = MatchesIn;
	        %true ->
		       VarsOut = [Term|VarsIn],
		       conjoin(MatchesIn,herbrand(Term),MatchesOut).

herbrandize_args(Term,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
        I > N ->
                VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        %true ->
                arg(I,Term,Arg),
                herbrandize_variables(Arg,VarsIn,Vars1,MatchesIn,Matches1),
                I1 is I + 1,
                herbrandize_args(Term,Vars1,VarsOut,Matches1,MatchesOut,I1,N).

herbrand_universe(U) :-
	setof(X,herbrand(X),U).

herbrand_preds([],true).
herbrand_preds([C|Cs],Wff) :-
	herbrand_preds(Cs,Wffs),
	conjoin((herbrand(C):-true),Wffs,Wff).

add_answer_preds((query :- Query),(query :- (Query,nl,nl,write(answer:Vars),nl))) :-
	!,
	variables(Query,Vars).
add_answer_preds(R,R).

%%% constants returns a list of the constants appearing in a formula.

constants(Wff,L) :-
        Wff = (A :- B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A : B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        functor(Wff,search,_) ->        % list constants in first argument of search
                arg(1,Wff,X),
                constants(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                functor(Wff,_,N),
                (N > 0 ->
		   constantize_args(Wff,[],L,1,N);
		 %true ->
                   L = []).

constantize_args(Term,FnsIn,FnsOut,I,N) :-
	var(Term) ->
		FnsOut = FnsIn;
	atom(Term) ->
	        FnsOut = [Term|FnsIn];
        I > N ->
                FnsOut = FnsIn;
        %true ->
                arg(I,Term,ArgI),
		(var(ArgI) ->
		        Fns1 = [];
		%true ->
		        functor(ArgI,_,NI),
                        constantize_args(ArgI,FnsIn,Fns1,1,NI)),
                I1 is I + 1,
                constantize_args(Term,Fns1,FnsOut,I1,N).

%%% variables returns a list of the variables appearing in a formula.

variables(Wff,L) :-
        Wff = (A :- B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A : B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        functor(Wff,search,_) ->        % list variables in first argument of search
                arg(1,Wff,X),
                variables(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                functor(Wff,_,N),
                (N > 0 ->
		   variablize_args(Wff,[],L,1,N);
		 %true ->
                   L = []).

variablize_args(Term,FnsIn,FnsOut,I,N) :-
	atom(Term) ->
		FnsOut = FnsIn;
	var(Term) ->
	        FnsOut = [Term|FnsIn];
        I > N ->
                FnsOut = FnsIn;
        %true ->
                arg(I,Term,ArgI),
		(var(ArgI) ->
		        union([ArgI],FnsIn,Fns1);
		%true ->
		        functor(ArgI,_,NI),
                        variablize_args(ArgI,FnsIn,Fns1,1,NI)),
                I1 is I + 1,
                variablize_args(Term,Fns1,FnsOut,I1,N).


variablize_clause(Clause,Vars) :-
	ClauseTerm =.. [clause|Clause],
	variables(ClauseTerm,Vars).

instance([],_,Clause).
instance(Vars,Cons,Clause) :-
	select(V,Vars,V1s),
	member(V,Cons),
	instance(V1s,Cons,Clause).

skolem([],Term,Term).
skolem([Var|Vars],Term,SkTerm) :-
	skolem(Vars,Term,Term1),
	SkTerm=(Var^Term1).

instances(Clause,Terms,Instances) :-
	variablize_clause(Clause,Vars),
	skolem(Vars,instance(Vars,Terms,Clause),DoIt),
	setof(Clause,DoIt,Instances).

instantiation([],_,[]) :-
	!.
instantiation(Matrix,[],Matrix) :-
	!.
instantiation([Clause|Matrix],Terms,MatrixInstances) :-
	instances(Clause,Terms,Instances),
	instantiation(Matrix,Terms,IMatrix),
	combine_clauses(Instances,IMatrix,MatrixInstances).




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



%%% verbose predicate, chatting if verbose_mode is turned on
verbose(X) :-
	verbose_flag ->
	        write(X),nl;
	%true->
		true.

%%% PRINT CONFIGURATION
%%%


%:- set_flag(print_depth,1000).
%:- set_flag(variable_names,off).

%:- dont_compile_count_inferences.

:- do_compile_proof_printing.            % default is to compile proof printing
:- do_compile_complete_search.            % default is to compile complete search
:- admissibility_first.                     % default is to check admissibility first
:- verbose_mode.                         % default is to print proof
:- body_hooks.
:- no_body_hooks.                      % default is no body hooks
:- pred_hooks.
:- no_pred_hooks.                      % default is no predicate hooks
:- print_proof.                         % default is to print proof
:- dont_trace_search.                        % default is to trace searching

:- lemma_handling.                         % default is LEMMA HANDLING
:- lemma_mode(dynamic).                    % default is to use DYNAMIC lemmas only
:- lemma_type(delta).                      % default is to use DELTA lemmas only
:- lemma_type(all).
:- lemma_format(unit).                     % default is to use UNIT lemmas only


:-add_dystatic_lemmas.

%:- no_lemma_handling,lemma_configuration.   % indicated by lemma_handling_flag


:-configuration.

