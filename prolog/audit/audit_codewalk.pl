:- module(audit_codewalk, [audit_walk_code/4,
			   decl_walk_code/2]).

:- use_module(library(extra_location)).
:- use_module(library(option_utils)).
:- use_module(library(prolog_codewalk)).

:- meta_predicate decl_walk_code(3,-).

:- meta_predicate audit_walk_code(+,3,-,-).
audit_walk_code(OptionL0, Tracer, M, FromChk) :-
    option_fromchk(OptionL0, OptionL1, FromChk),
    select_option(module(M), OptionL1, OptionL2, M),
    merge_options(OptionL2,
		  [source(false),
		   infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_),
		   module_class([user, system, library]),
		   on_trace(Tracer)
		  ], OptionL),
    prolog_walk_code(OptionL),
    decl_walk_code(Tracer, M).

decl_walk_code(Tracer, M) :-
    forall(extra_location(Head, M, goal, From),
	   ignore(call(Tracer, M:Head, _:'<declaration>', From))).
