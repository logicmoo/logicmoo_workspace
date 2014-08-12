:- module(check_unused, []).

/*
  
  This analyzer is based on the Mark-and-Sweep Algorithm:
  http://www.brpreiss.com/books/opus5/html/page424.html

  Also uses the Tarjan's scc algorithm to show the output
  grouped by scc and hierarchically.

*/

:- use_module(library(prolog_codewalk)).
:- use_module(library(auditable_predicate)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(database_fact)).
:- use_module(library(is_entry_point)).
:- use_module(library(record_locations)).
:- use_module(library(location_utils)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(referenced_by)).

:- multifile
    prolog:message//1,
    audit:check/4.

:- dynamic marked/3, calls_to/4.

check_pred_file(PI, FileChk) :-
    property_from(PI, _, From),
    from_to_file(From, File),
    call(FileChk, File),
    !.

:- public collect_unused/5.
:- meta_predicate collect_unused(?,1,+,+,+).
collect_unused(M, FileChk, MGoal, Caller, From) :-
    from_to_file(From, File),
    call(FileChk, File),
    record_location_meta(MGoal, M, From, cu_callee_hook,
			 cu_caller_hook(Caller)).

audit:check(unused, Ref, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_unused(Ref, FileChk, OptionL, Result).

:- meta_predicate check_unused(?, ?, +, -).
check_unused(Ref0, FileChk, OptionL0, Pairs) :-
    normalize_head(Ref0, M:H),
    merge_options(OptionL0,
		  [source(false),
		   infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_:H),
		   on_trace(collect_unused(M, FileChk))
		  ], OptionL),
    prolog_walk_code(OptionL),
    mark(Ref),
    sweep(Ref, FileChk, Pairs),
    cleanup_unused.

cleanup_unused :-
    retractall(calls_to(_, _, _, _)),
    retractall(marked(_, _, _)),
    cleanup_locations(_, _, dynamic(_, _, _), _).

:- meta_predicate is_entry_caller(+,+,+).
is_entry_caller('<initialization>', _, _) :- !.
is_entry_caller(F, A, M) :- entry_caller(F, A, M, _).

entry_caller(F, A, M, H) :-
    functor(H, F, A),
    ( hide_unused(M:F/A) -> true
    ; is_entry_point(M:H)
    ).

entry_point(M:H, CM:CF/CA) :-
    calls_to(CF, CA, CM, M:F/A),
    is_entry_caller(CF, CA, CM),
    functor(H, F, A).

mark(Ref) :-
    forall(entry_point(Ref, PI),
	   mark_rec(PI)).

mark_rec(M:F/A) :-
    ( \+ marked(F, A, M) ->
      assertz(marked(F, A, M)),
      forall(calls_to(F, A, M, Callee), mark_rec(Callee))
    ; true
    ).

% Tarjan's scc algorithm:
:- use_module(library(audit/sccs)).

sweep(Ref, FileChk, Pairs) :-
    findall(PI, unmarked(Ref, FileChk, PI), Nodes),
    findall(arc(X, Y), ( member(X, Nodes),
			 X = M:F/A,
			 calls_to(F, A, M, Y),
			 memberchk(Y, Nodes)),
	    Arcs),
    nodes_arcs_sccs(Nodes, Arcs, SU),
    sort(SU, SL),
    % memberchk(arc(Node1, Node2), Arcs),
    maplist(adjacency(SL, dependent_scc_el), SL, AL),
    maplist(add_adj_key, AL, Pairs).

/*
sweep(Ref, Pairs) :-
    findall(warning-(Loc-(PI/D)), ( unmarked(Ref, PI),
				    property_location(PI, D, Loc)), Pairs).
*/

add_warning_key(L, warning-L).

audit:prepare_results(unused, Pairs, Results) :-
    maplist(add_adj_key, AL, Pairs),
    partial_dependency_tree(dependent_scc_al, AL, DL),
    add_location_info(add_location_al(Pairs), DL, PIR),
    maplist(add_warning_key, PIR, Results).

add_location_al(AL, PI, L) :-
    member(warning-(PILocDL/_), AL),
    member(PI/LocDL, PILocDL),
    findall(Loc-((PI)/D),
	    member(Loc/D, LocDL),
	    U),
    sort(U, L).

prop_loc_desc(PI, PI/LocDL) :-
    ( nonvar(LocDL)		% allow reversibility
    ->true
    ; findall(Loc/D, property_location(PI, D, Loc), LocDU),
      sort(LocDU, LocDL)
    ).

add_adj_key(PIL-L, warning-(PILocDL/L)) :-
    maplist(prop_loc_desc, PIL, PILocDL).

:- meta_predicate adjacency(+, 2, +, -).
adjacency(SL, Dependent, S, S-L) :-
    findall(S2, ( member(S2, SL),
		  S2 \= S,
		  call(Dependent, S, S2)
		),
	    U),
    sort(U, L).

:- public dependent_scc_el/2.

dependent_scc_el(SCC1, SCC2) :-
    ( member(Node1, SCC1) *->
      member(Node2, SCC2),
      dependent(Node1, Node2)
    ; true
    ),
    !.

dependent(M:F/A, Node2) :-
    calls_to(F, A, M, Node2).

dependent_scc_al(SCC1-AL, SCC2-_) :-
    ( SCC1 \= []
    ->memberchk(SCC2, AL)
    ; true
    ).

root_node(Dependent, SL, S0, S) :-
    call(Dependent, S0, S),
    \+ ( member(S2, SL),
	 S2 \= S,
	 call(Dependent, S2, S)
       ).

:- meta_predicate partial_dependency_tree(2,+,-).

partial_dependency_tree(Dependent, SL, L) :-
    partial_dependency_tree(Dependent, []-[], []-L, SL, []).

partial_dependency_tree(Dependent, S-A, S-L, SL0, SL) :-
    partition(root_node(Dependent, SL0, S-A), SL0, RL, SL1),
    maplist_dcg(partial_dependency_tree(Dependent), RL, L, SL1, SL).

:- meta_predicate add_location_info(2, +, -).
add_location_info(LI, L, R) :- add_location_info_(L, LI, R).

add_location_info_(L, LI, R) :-
    is_list(L),
    !,
    maplist(add_location_info(LI), L, U),
    sort(U, R).
add_location_info_(A-B, LI, C-D) :-
    add_location_info_(A, LI, C),
    add_location_info_(B, LI, D).
add_location_info_(M:PI, LI, L) :-
    call(LI, M:PI, L).

unmarked(Ref, FileChk, MPI) :-
    Ref = M:H,
    MPI = M:F/A,
    ( current_defined_predicate(MPI),
      functor(H, F, A),
      auditable_predicate(Ref)
    ; declaration_location(H, M, dynamic(def, _, _), _),
      functor(H, F, A)
    ),
    \+ entry_caller(F, A, M, H),
    \+ marked(F, A, M),
    check_pred_file(MPI, FileChk).

prolog:message(acheck(unused)) -->
    ['-----------------',nl,
     'Unused Predicates',nl,
     '-----------------',nl,
     'The predicates below has been implemented, however they are', nl,
     'never referenced in the code nor exported.  Probably are', nl,
     'dead-code, part of an incomplete implementation, or called', nl,
     'indirectly by some meta predicate without or with incorrect', nl,
     'meta_predicate declaration.  In any case this represents a', nl,
     'bad design and must be fixed, either completing the program,',nl,
     'or exporting/removing the unreferenced predicates.', nl, nl].
prolog:message(acheck(unused, PIDL-PIL)) -->
    message_unused_scc(PIDL, ['*', ' ']),
    maplist_dcg(maplist_dcg(message_unused_rec([' ', ' ', ' ', ' '])), PIL).

message_unused_one([], Level, Level) --> [].
message_unused_one([PID|PIDL], Level0, Level) -->
    message_unused(Level0, PID),
    { Level0 = [_, _|Level1],
      Level  = [' ', ' '|Level1]
    },
    maplist_dcg(message_unused(Level), PIDL).

message_unused_scc([PIDL|PIDLL], Level0) -->
    message_unused_one(PIDL, Level0, Level),
    maplist_dcg(maplist_dcg(message_unused([' ', ' '|Level])), PIDLL).

message_unused_rec(Level, PIDL-PIL) -->
    message_unused_scc(PIDL, Level),
    maplist_dcg(message_unused_rec([' ', ' '|Level]), PIL).

message_unused(Level, Loc-(PI/D)) -->
    Level,
    Loc,
    ['unused ~w: ~w'-[D, PI], nl].

% Hook to hide unused messages:
:- multifile hide_unused/1.

hide_unused(_:'$exported_op'/3).
hide_unused(predopts_analysis:attr_unify_hook/2).
hide_unused(shlib:loading/1).
hide_unused(pce_global:'pce catcher'/2).
hide_unused(_:term_expansion/2).
hide_unused(_:goal_expansion/2).
hide_unused(_:term_expansion/4).
hide_unused(_:goal_expansion/4).
hide_unused(plunit:_).
hide_unused(ciao:_).
hide_unused(user:thread_message_hook/3).
hide_unused(user:prolog_trace_interception/4).
hide_unused(M:F/A) :-
    unused_mo_clpfd(M),
    unused_pi_clpfd(F, A).

unused_mo_clpfd(clpfd_original).
unused_mo_clpfd(clpfd_relation).
unused_mo_clpfd(clpfd_gcc_occurred).
unused_mo_clpfd(clpfd_gcc_num).
unused_mo_clpfd(clpfd_gcc_vs).
unused_mo_clpfd(clpfd_gcc_aux).
unused_mo_clpfd(clpfd_aux).

unused_pi_clpfd(attribute_goals, 3).
unused_pi_clpfd(attr_unify_hook, 2).

prop_t(use).
prop_t(def).

:- multifile cu_callee_hook/3.

cu_callee_hook(use,  Goal, Goal).
cu_callee_hook(Prop, Goal, Fact) :-
    prop_t(Prop),
    database_fact(Prop, Goal, Fact).

cu_caller_hook(Caller, MGoal, dynamic(use, _, _), _From) :-
    normalize_pi(MGoal, MPI),
    ground(MPI),
    CPI = M:F/A,
    normalize_pi(Caller, CPI),
    (calls_to(F, A, M, MPI) -> true ; assertz(calls_to(F, A, M, MPI))).
cu_caller_hook(_Caller, MGoal, dynamic(def, CM, Goal), From) :-
    normalize_head(MGoal, M:Head),
    nonvar(M),
    callable(Head),
    record_location(Head, M, dynamic(def, CM, Goal), From).
