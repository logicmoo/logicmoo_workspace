:- module(audit_codewalk, [audit_walk_code/4,
			   audit_wcsetup/4,
			   decl_walk_code/2,
			   record_issues/1]).

:- use_module(library(extra_location)).
:- use_module(library(option_utils)).
:- use_module(library(prolog_codewalk)).

:- thread_local
    issues/1.

:- meta_predicate
    decl_walk_code(3,-),
    audit_walk_code(+,3,-,-).

audit_walk_code(OptionL0, Tracer, M, FromChk) :-
    audit_wcsetup(OptionL0, OptionL1, M, FromChk),
    select_option(source(S), OptionL1, OptionL, false),
    optimized_walk_code(S, [on_trace(Tracer)|OptionL]),
    decl_walk_code(Tracer, M).

optimized_walk_code(false, OptionL) :-
    prolog_walk_code([source(false)|OptionL]).
optimized_walk_code(true, OptionL) :-
    prolog_walk_code([source(false)|OptionL]),
    findall(CRef, retract(issues(CRef)), Clauses),
    ( Clauses==[]
    ->true
    ; prolog_walk_code([clauses(Clauses)|OptionL])
    ).

audit_wcsetup(OptionL0, OptionL, M, FromChk) :-
    option_fromchk(OptionL0, OptionL1, FromChk),
    select_option(module(M), OptionL1, OptionL2, M),
    merge_options(OptionL2,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_),
		   module_class([user, system, library])
		  ], OptionL).

decl_walk_code(Tracer, M) :-
    forall(loc_declaration(Head, M, goal, From),
	   ignore(call(Tracer, M:Head, _:'<declaration>', From))).

record_issues(CRef) :-
    assertz(issues(CRef)).
