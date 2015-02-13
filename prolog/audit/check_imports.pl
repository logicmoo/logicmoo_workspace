:- module(check_imports, []).

:- use_module(library(clambda)).
:- use_module(library(maplist_dcg)).
:- use_module(library(implementation_module)).
:- use_module(library(record_locations)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(normalize_head)).
:- use_module(library(audit/audit)).

:- multifile
    prolog:message//1.

prolog:message(acheck(imports)) -->
    ['--------------',nl,
     'Unused Imports',nl,
     '--------------',nl,
     'The predicates or modules below has been imported, however they', nl,
     'are never used in the importing module, or they do not implement', nl,
     'new clauses for multifile predicates.  Note that modules that', nl,
     'export operators, or that do not export any predicate are not', nl,
     'reported.', nl,
     'You can silent the warnings by declaring use_module/2 with an',nl,
     'empty import list. If they have desirable side effects and still', nl,
     'needs to be imported, you can refactorize you program so that', nl,
     'such side effects are not required anymore.', nl, nl].
prolog:message(acheck(imports, c(Class, Type, Name)-LocElemL)) -->
    ['~w ~w have unused ~w:'-[Class, Name, Type], nl],
    maplist_dcg(unused_import(Type), LocElemL).

unused_import(Type, Loc/Elem) -->
    Loc,
    ['unused ~w ~w'-[Type, Elem], nl].

:- dynamic
    used_import/1,
    used_usemod/2.

audit:check(imports, Ref, Result, OptionL0 ) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_imports(Ref, FileChk, OptionL, Result).

:- meta_predicate check_imports(?, ?, +, -).
check_imports(Ref, FileChk, OptionL0, Pairs) :-
    normalize_head(Ref, M:H),
    merge_options(OptionL0,
		  [source(false),
		   infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_:H),
		   module_class([user]),
		   on_trace(collect_imports(M, FileChk))
		  ], OptionL),
    cleanup_imports,
    prolog_walk_code(OptionL),
    forall(extra_location(Head, CM, goal, From),
	   ignore(collect_imports(M, FileChk, CM:Head, _, From))),
    collect_imports(M, Pairs, Tail),
    collect_usemods(M, Tail, []),
    cleanup_imports.

collect_imports(M, Pairs, Tail) :-
    findall(warning-(c(use_module, import, U)-(Loc/(F/A))),
	    ( clause(extra_location(Head, M, import(U), From), _, CRef),
	      M \= user,
	      \+ memberchk(Head, [term_expansion(_,_),
				  term_expansion(_,_,_,_),
				  goal_expansion(_,_),
				  goal_expansion(_,_,_,_)
				 ]),
	      \+ used_import(CRef),
	      \+ extra_location(Head, M, goal, _),
	      module_property(M, class(Class)),
	      memberchk(Class, [user]),
	      functor(Head, F, A),
	      from_location(From, Loc)
	    ),
	    Pairs, Tail).

collect_usemods(M, Pairs, Tail) :-
    findall(warning-(c(module, use_module, M)-(Loc/U)),
	    [M,U,Loc] +\
	   ( extra_location(U, M, use_module, From),
	     M \= user,
	     module_property(M, class(Class)),
	     memberchk(Class, [user]),
	     from_to_file(From, File),
	     \+ findall(I, source_file_property(File, included_in(I, _)),
			[_, _|_]),
	     absolute_file_name(U, UFile, [file_type(prolog), access(exist),
					   file_errors(fail)]),
	     current_module(UM, UFile),
	     module_property(UM, exports(EL)),
	     EL \= [],
	     \+ ( module_property(UM, exported_operators(OL)),
		  OL \= []
		),
	     \+ ( member(F/A, EL),
		  functor(Head, F, A),
		  implementation_module(UM:Head, IM),
		  used_usemod(M, IM)
		),
	     from_location(From, Loc)
	   ), Pairs, Tail).

collect_imports(M, FileChk, MGoal, Caller, From) :-
    from_to_file(From, File),
    call(FileChk, File),
    record_location_meta(MGoal, M, From, all_call_refs, mark_import),
    ( nonvar(Caller),
      Caller = MCaller:_,
      MGoal  = CM:_,
      CM \= MCaller,
      \+ used_usemod(CM, MCaller)
    ->assertz(used_usemod(CM, MCaller))
    ; true
    ).

mark_import(M:Head, CM, _, _, _, _) :-
    nonvar(M),
    callable(Head),
    mark_import(Head, M, CM).

mark_import(Head, M, CM) :-
    forall(( clause(extra_location(Head, CM, import(_), _), _, CRef),
	     \+ used_import(CRef)),
	   assertz(used_import(CRef))),
    ( M \= CM,
      \+used_usemod(CM, M)
    ->assertz(used_usemod(CM, M))
    ; true
    ).

cleanup_imports :-
    retractall(used_import(_)),
    retractall(used_usemod(_, _)).
