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
:- use_module(library(option_utils)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(referenced_by)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(implementation_module)).
:- use_module(library(audit/audit)).

:- multifile
    prolog:message//1.

:- dynamic marked/3, calls_to/2, arc/2, scc1/2, node_scc/2.

check_pred_file(Ref, FileChk) :-
    property_from(Ref, _, From),
    from_to_file(From, File),
    call(FileChk, File),
    !.

:- public collect_unused/5.
:- meta_predicate collect_unused(?,1,+,+,+).
collect_unused(M, FileChk, MGoal, Caller, From) :-
    from_to_file(From, File),
    call(FileChk, File),
    record_location_meta(MGoal, M, From, cu_callee_hook, cu_caller_hook(Caller)).

audit:check(unused, Ref, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_unused(Ref, FileChk, OptionL, Result).

:- meta_predicate check_unused(?, ?, +, -).
check_unused(Ref, FileChk, OptionL0, Pairs) :-
    normalize_head(Ref, M:H),
    merge_options(OptionL0,
		  [source(false),
		   infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_:H),
		   module_class([user,system,library]),
		   on_trace(collect_unused(M, FileChk))
		  ], OptionL),
    prolog_walk_code(OptionL),
    mark(Ref),
    sweep(Ref, FileChk, Pairs),
    cleanup_unused.

cleanup_unused :-
    retractall(calls_to(_, _)),
    retractall(marked(_, _, _)),
    retractall(arc(_, _)),
    retractall(scc1(_, _)),
    retractall(node_scc(_, _)).

is_entry_caller('<initialization>') :- !.
is_entry_caller(Ref) :- !,
    nth_clause(M:H, _, Ref),
    entry_caller(M, H).

entry_caller(M, H) :-
    ( is_entry_point(H, M) -> true
    ; extra_location(H, M, goal, _)
    ).

entry_point(M:H, Caller) :-
    calls_to(Caller, M:H),
    is_entry_caller(Caller).

mark(Ref) :-
    forall(entry_point(Ref, Caller), put_mark(Caller)).

mark_rec(M:H) :-
    ( H == '<initialization>'
    ->forall(calls_to(H, Callee), mark_rec(Callee))
    ; predicate_property(M:H, interpreted)
    ->mark_caller(M:H)
    ; true
    ).

resolve_meta_goal(H, M, G) :-
    ( predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(M:H, Meta, M:G)
    ; G = H
    ).

put_mark(H, M) :-
    retractall(marked(H, M, _)),
    assertz(marked(H, M, _)),
    forall(( nth_clause(M:H, _, CRef),
	     calls_to(CRef, Callee)
	   ),
	   mark_rec(Callee)).

match_all(G) :-
    functor(G, F, A),
    functor(H, F, A),
    G =@= H.

mark_caller(M:H) :-
    marked(H, M, I),
    var(I),
    !.
mark_caller(M:H) :-
    match_all(H),
    !,
    put_mark(H, M).
mark_caller(M:H) :-
    resolve_meta_goal(H, M, G),
    forall(( clause(M:G, _, CRef),
	     clause_property(CRef, file(_)) % Static clauses only
	   ), put_mark(CRef)),		    % Widening
    put_narrow_mark(M:G).

put_narrow_mark(M:G) :-
    ( functor(G, F, A),
      functor(P, F, A),
      ( marked(P, M, 0 ),
	subsumes_term(P, G)
      ->true
      ; forall(( clause(marked(P, M, 0 ), true, CRef),
		 subsumes_term(G, P)
	       ),
	       erase(CRef)),	    % Cleaning up
	assertz(marked(G, M, 0 ))   % Preserve narrow findings
      )
    ).

%% Generalization step, we lose precision but avoid loops --EMM
mark_to_head('<initialization>', _:'<initialization>'-0) :- !.
mark_to_head(CRef, M:H-I) :-
    clause(M:H, _, CRef),
    nth_clause(M:H, I, CRef).

put_mark(CRef) :-
    mark_to_head(CRef, M:H-I),
    ( \+ marked(H, M, I)
    ->assertz(marked(H, M, I)),
      forall(calls_to(CRef, Callee), mark_rec(Callee))
    ; true
    ).

% Tarjan's scc algorithm:
:- use_module(library(scc)).

current_arc(Nodes, X, Y) :-
    ( PI = M:F/A,
      member(X, Nodes),
      ( X = PI
      ->functor(H, F, A),
	clause(M:H, _, CRef)
      ; X = PI/I,
	I > 0
      ->functor(H, F, A),
	nth_clause(M:H, I, CRef)
      ),
      calls_to(CRef, M2:H2),
      functor(H2, F2, A2),
      PI2 = M2:F2/A2,
      ( Y = PI2,
	memberchk(Y, Nodes)
      ; predicate_property(M2:H2, interpreted),
	( clause(M2:H2, _, YRef),
	  nth_clause(_, I2, YRef),
	  Y = PI2/I2
	; %% extra_location(H2, M2, dynamic(use, _, _), _),
	  Y = PI2/0
	),
	memberchk(Y, Nodes)
      )
    ).

% Note: although is not nice, we are using dynamic predicates to cache partial
% results for performance reasons (arc/2, node_scc/2, scc1/2), otherwise the
% analysis will take 20 times more --EMM
%
sweep(Ref, FileChk, Pairs) :-
    findall(Node, unmarked(Ref, FileChk, Node), UNodes),
    sort(UNodes, Nodes),
    forall(current_arc(Nodes, X, Y), assertz(arc(X, Y))),
    findall(arc(X, Y), arc(X, Y), Arcs),
    nodes_arcs_sccs(Nodes, Arcs, SU),
    sort(SU, SL),
    table_sccs(SL),
    findall(I1-I2,
	    ( node_scc(Node1, I1),
	      ( arc(Node1, Node2),
		node_scc(Node2, I2),
		I2 \= I1
	      *->true
	      ; I2 = []
	      )
	    ), IU),
    sort(IU, IL),
    maplist(ipairs_scc, IL, DU),
    sort(DU, DL),
    maplist(add_adj_key, DL, Pairs).

ipairs_scc(I-[], SCC-[]) :- !,
    scc1(I, SCC).
ipairs_scc(I1-I2, SCC1-SCC2) :-
    scc1(I1, SCC1),
    scc1(I2, SCC2).

table_sccs(SCCL) :-
    forall(nth1(Idx, SCCL, SCC),
	   ( assertz(scc1(Idx, SCC)),
	     forall(member(Node, SCC),
		    assertz(node_scc(Node, Idx)))
	   )).

/*
sweep(Ref, Pairs) :-
    findall(warning-(Loc-(PI/D)), ( unmarked(Ref, PI),
				    property_location(PI, D, Loc)), Pairs).
*/

add_warning_key(L, warning-L).

audit:prepare_results(unused, Pairs, Results) :-
    maplist(add_adj_key, DU, Pairs),
    sort(DU, DL),
    group_pairs_by_key(DL, AL),
    partial_dependency_tree(dependent_scc_al, AL, PL),
    add_location_info(add_location_al(Pairs), PL, PIR),
    maplist(add_warning_key, PIR, Results).

add_location_al(AL, PI, L) :-
    member(warning-(PILocDL/_), AL),
    member(PI/LocDL, PILocDL),
    findall(Loc-(PI/D), member(Loc/D, LocDL), U),
    sort(U, L).

prop_loc_desc(PI, PI/LocDL) :-
    ( nonvar(LocDL)		% allow reversibility
    ->true
    ; findall(Loc/D, property_location(PI, D, Loc), LocDU),
      sort(LocDU, LocDL)
    ).

add_adj_key(HL-L, warning-(PILocDL/L)) :-
    maplist(prop_loc_desc, HL, PILocDL). 

/*
:- meta_predicate adjacency(+, 2, +, -).
adjacency(SL, Dependent, S, S-L) :-
    findall(D, ( member(D, SL),
		 D \= S,
		 call(Dependent, S, D)
	       ),
	    L).
*/

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
add_location_info_(H/I, LI, L) :-
    call(LI, H/I, L).

unmarked(Ref, FileChk, Node) :-
    Ref = M:H,
    MPI = M:F/A,
    ( current_defined_predicate(MPI),
      functor(H, F, A),
      auditable_predicate(Ref),
      \+ entry_caller(M, H),
      ( \+ marked(H, M, _)
      ->Node = MPI
      ; ( clause(M:H, _, CRef),
	  clause_property(CRef, file(_)), % Static clauses only
	  nth_clause(M:H, I, CRef),
	  \+ marked(H, M, I),
	  Node = MPI/I
	; extra_location(H, M, dynamic(def, _, _), _),
	  \+ marked(H, M, 0 ),
	  Node = MPI/0
	)
      )
    ; extra_location(H, M, dynamic(def, _, _), _),
      functor(H, F, A),
      \+ current_defined_predicate(MPI),
      auditable_predicate(Ref),
      \+ entry_caller(M, H),
      \+ marked(H, M, 0 ),
      Node = MPI/0
    ),
    \+ hide_unused(H, M),
    check_pred_file(Ref, FileChk).
    
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
:- multifile hide_unused/2.

hide_unused('$exported_op'(_,_,_), _).
hide_unused(attr_unify_hook(_, _), predopts_analysis).
hide_unused(loading(_), shlib).
hide_unused('pce catcher'(_, _), pce_global).
hide_unused(attribute_goals(_, _, _), M) :- unused_mo_clpfd(M).
hide_unused(attr_unify_hook(_, _),    M) :- unused_mo_clpfd(M).
hide_unused(_, plunit).
hide_unused(_, ciao).

unused_mo_clpfd(clpfd_original).
unused_mo_clpfd(clpfd_relation).
unused_mo_clpfd(clpfd_gcc_occurred).
unused_mo_clpfd(clpfd_gcc_num).
unused_mo_clpfd(clpfd_gcc_vs).
unused_mo_clpfd(clpfd_gcc_aux).
unused_mo_clpfd(clpfd_aux).

prop_t(use).
prop_t(def).

cu_callee_hook(lit,  Goal, _,  CM, CM:Goal).
cu_callee_hook(Prop, Goal, IM, CM, CM:Fact) :-
    prop_t(Prop),
    database_fact(Prop, IM:Goal, Fact).

caller_ptr('<initialization>', _, '<initialization>') :- !.
caller_ptr(_, clause(Ptr), Ptr).

cu_caller_hook(Caller, M:Head, CM, Type, Goal, _, From) :-
    nonvar(M),
    callable(Head),
    ( Type \= lit
    ->record_location(Head, M, dynamic(Type, CM, Goal), From)
    ; true
    ),
    record_calls_to(Type, Caller, Head, M, From).

record_calls_to(Type, Caller, Head, M, From) :-
    ( memberchk(Type, [use, lit])
    ->caller_ptr(Caller, From, Ptr),
      ( \+ calls_to(Ptr, M:Head)
      ->assertz(calls_to(Ptr, M:Head))
      ; true
      )
    ; true
    ).
