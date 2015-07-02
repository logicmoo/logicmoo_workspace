:- module(check_wrong_dynamic, []).

:- use_module(library(check), []).
:- use_module(library(prolog_codewalk)).
:- use_module(library(clambda)).
:- use_module(library(compact_pi_list)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_pi)).
:- use_module(library(database_fact)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(auditable_predicate)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(audit/audit)).
:- use_module(library(audit/audit_codewalk)).

:- multifile
    prolog:message//1,
    hide_wrong_dynamic/1,
    hide_var_dynamic/1.

hide_var_dynamic(check:list_strings/1).
hide_var_dynamic(check_non_mutually_exclusive:collect_non_mutually_exclusive/2).
hide_var_dynamic(check_non_mutually_exclusive:mutually_exclusive/3).
hide_var_dynamic(check_trivial_fails:cu_caller_hook/7).
hide_var_dynamic(implemented_in:implemented_in/3).
hide_var_dynamic(ref_scenarios:unfold_goal/2).
hide_var_dynamic(check_unused:mark_caller/1).
hide_var_dynamic(check_unused:unmarked/3).
hide_var_dynamic(check_dupcode:duptype_elem/6).
hide_var_dynamic(foreign_generator:bind_type_names/4).
hide_var_dynamic(filtered_backtrace:no_backtrace_entry/1).
hide_var_dynamic(check_unused:mark_to_head/2).
hide_var_dynamic(check_unused:current_arc/3).
hide_var_dynamic(ontrace:match_clause/5).
hide_var_dynamic(foreign_props:type_desc/2).
hide_var_dynamic(audit:prepare_results/3).
hide_var_dynamic(check_unused:current_edge/3).
hide_var_dynamic(commited_retract:commited_retract/1).
hide_var_dynamic(ntabling:tabling/2).
hide_var_dynamic(abstract_interpreter:match_head_body/3).

:- dynamic
    wrong_dynamic_db/4,
    var_dynamic_db/2.

hide_wrong_dynamic(user:prolog_trace_interception/4).

cleanup_dynamic_db :-
    retractall(wrong_dynamic_db(_, _, _, _)),
    retractall(var_dynamic_db(_, _)).

audit:check(wrong_dynamic, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_wrong_dynamic(from_chk(FileChk), OptionL, Result).

check_wrong_dynamic(FromChk, OptionL0, Pairs) :-
    select_option(module(M), OptionL0, OptionL1, M),
    merge_options(OptionL1,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_)],
		  OptionL),
    prolog_walk_code([source(false),
		      on_trace(collect_wrong_dynamic(M, FromChk))|OptionL]),
    findall(CRef, ( current_static_as_dynamic(_, _, _, _, clause(CRef), _),
		    retractall(wrong_dynamic_db(clause(CRef), _, _, _))
		  ; retract(var_dynamic_db(clause(CRef), _))
		  ), Clauses),
    ( Clauses==[]
    ->true
    ; prolog_walk_code([clauses(Clauses),
			on_trace(collect_wrong_dynamic(M))|OptionL])
    ),
    decl_walk_code(collect_wrong_dynamic(M, FromChk), M),
    collect_result(M:_, FromChk, Pairs),
    cleanup_dynamic_db.

collect_result(Ref, FromChk, Pairs) :-
    findall(Type-(as_dynamic(DType)-((Loc/PI)-(MLoc/MPI))),
	    ( current_static_as_dynamic(Type, DType, Loc, PI, From, MPI),
	      from_location(From, MLoc)), Pairs, Pairs1),
    findall(warning-(dynamic_as_static-(Loc-PI)),
	    current_dynamic_as_static(Ref, FromChk, Loc, PI), Pairs1, Pairs2),
    findall(warning-(var_as_dynamic-(PI-(Loc/CI))),
	    ( retract(var_dynamic_db(From, PI)),
	      check:predicate_indicator(From, CI, []),
	      from_location(From, Loc)), Pairs2, []).

current_static_as_dynamic(Type, DType, Loc, PI, MFrom, MPI) :-
    wrong_dynamic_db(MFrom, TypeDB, PI, MPI),
    memberchk(TypeDB,[def,retract]),
    PI = M:F/A,
    functor(H,F,A),
    Ref = M:H,
    \+ predicate_property(Ref, dynamic),
    \+ predicate_property(Ref, volatile),
    ( predicate_property(Ref, number_of_clauses(N)),
      N > 0 ->
      Type = error,
      DType = static,
      predicate_location(Ref, Loc)
    ; Type = warning,
      DType  = unknown,
      once(property_location(PI, _, Loc))      
    ).

:- meta_predicate current_dynamic_as_static(?, 1, -, ?).
current_dynamic_as_static(Ref, FromChk, Loc, PI) :-
    Ref = M:H,
    PI = M:F/A,
    ( var(H) ->
      current_defined_predicate(PI),
      functor(H, F, A)
    ; functor(H, F, A),
      current_defined_predicate(PI)
    ),
    auditable_predicate(Ref),
    predicate_property(Ref, dynamic),
    property_from(PI, dynamic, From),
    call(FromChk, From),
    %% if multifile, would be modified externally
    \+ predicate_property(Ref, multifile),
    \+ ( wrong_dynamic_db(_, Type, PI, _),
	 memberchk(Type,[def,retract])
       ),
    from_location(From, Loc).

prolog:message(acheck(wrong_dynamic, Type-List)) -->
    wrong_dynamic_message(Type, List).

as_dynamic(DType, Loc/PI-MLocPIs) -->
    ['\t'|Loc], ['~w ~q modified by'-[DType, PI], nl],
    maplist_dcg(show_locpi, MLocPIs).

show_locpi(Loc/PI) --> ['\t\t'|Loc], check:predicate(PI), [nl].

show_locci(Loc/CI) --> ['\t\t'|Loc], CI, [nl].

dynamic_as_static(Loc-PIs) -->
    {compact_pi_list(PIs, CPIs)},
    ['\t'|Loc], ['predicates ~w'-[CPIs], nl].

wrong_dynamic_message(as_dynamic(DType), LocPIs) -->
    ['Predicates are ~w, but never declared dynamic and modified:'-DType, nl],
    maplist_dcg(as_dynamic(DType), LocPIs).
wrong_dynamic_message(dynamic_as_static, LocPIs) -->
    ['Predicates declared dynamic, but never modified:', nl],
    maplist_dcg(dynamic_as_static, LocPIs).
wrong_dynamic_message(var_as_dynamic, PILocCIs) -->
    ['Predicates called with a variable in a module-sensitive argument:', nl],
    maplist_dcg(var_as_dynamic, PILocCIs).

var_as_dynamic(PI-LocCIs) -->
    ['\t~w called with a variable in'-[PI], nl],
    maplist_dcg(show_locci, LocCIs).

prolog:message(acheck(wrong_dynamic)) -->
    ['--------------------------', nl,
     'Wrong Dynamic Declarations', nl,
     '--------------------------', nl,
     'The predicates below present inconsistencies between its', nl,
     'usage and the dynamic declarations. Could be that they are', nl,
     'being used as dynamic without a proper declaration, being', nl,
     'declared as dynamic but never asserted, retracted, or using', nl,
     'a variable argument in a database predicate, making it', nl,
     'difficult to analyze.', nl, nl].

:- public collect_wrong_dynamic/5.
:- meta_predicate collect_wrong_dynamic(?,1,+,+,+).
collect_wrong_dynamic(M, FromChk, MGoal, Caller, From) :-
    call(FromChk, From),
    collect_wrong_dynamic(M, MGoal, Caller, From).

collect_wrong_dynamic(M, MGoal, Caller, From) :-
    record_location_meta(MGoal, M, From, \T^G^M^_^F^database_fact_ort(T,G,M,F),
			 record_location_wd(Caller)),
    fail.
collect_wrong_dynamic(_, _, _, _). % avoid side effects

record_location_wd(Caller, M:Fact, _, Type, IM:Goal, _, From) :-
    MGoal = IM:Goal,
    Def = dynamic(Type, _, MGoal),
    normalize_pi(MGoal, MPI),
    ( atom(M),
      callable(Fact)
    ->functor(Fact, F, A),
      record_location(Fact, M, Def, From),
      \+ hide_wrong_dynamic(M:F/A),
      assertz(wrong_dynamic_db(From, Type, M:F/A, MPI))
    ; \+ database_fact(Caller) ->
      normalize_pi(Caller, CM:PI),
      \+ hide_var_dynamic(CM:PI),
      assertz(var_dynamic_db(From, MPI))
    ; true
    ).
