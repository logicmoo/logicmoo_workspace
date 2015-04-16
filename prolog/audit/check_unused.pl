:- module(check_unused, []).

/*
  
  This analyzer is based on the Mark-and-Sweep Algorithm:
  http://www.brpreiss.com/books/opus5/html/page424.html

*/

:- use_module(library(auditable_predicate)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(apply)).
:- use_module(library(clambda)).
:- use_module(library(commited_retract)).
:- use_module(library(is_entry_point)).
:- use_module(library(extra_location)).
:- use_module(library(location_utils)).
:- use_module(library(maplist_dcg)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(audit/audit)).
:- use_module(library(audit/audit_codewalk)).

:- multifile
    prolog:message//1.

:- dynamic marked/3, calls_to/2, edge/2, node/3.

check_pred_file(Ref, FromChk) :-
    property_from(Ref, _, From),
    call(FromChk, From),
    !.

:- public collect_unused/5.
:- meta_predicate collect_unused(?,1,+,+,+).
collect_unused(M, FromChk, MGoal, Caller, From) :-
    call(FromChk, From),
    record_location_meta(MGoal, M, From, all_call_refs, cu_caller_hook(Caller)).

audit:check(unused, Result, OptionL) :-
    check_unused(OptionL, Result).

check_unused(OptionL, Pairs) :-
    audit_walk_code(OptionL, collect_unused(M, FromChk), M, FromChk),
    mark(M),
    sweep(M, FromChk, Pairs),
    cleanup_unused.

cleanup_unused :-
    retractall(calls_to(_, _)),
    retractall(marked(_, _, _)),
    retractall(edge(_, _)).

is_entry_caller('<initialization>') :- !.
is_entry_caller(Ref) :- !,
    nth_clause(M:H, _, Ref),
    entry_caller(M, H).

entry_caller(M, H) :-
    ( is_entry_point(H, M) -> true
    ; loc_declaration(H, M, goal, _)
    ).

entry_point(M, Caller) :-
    calls_to(Caller, M:_),
    is_entry_caller(Caller).

mark(M) :-
    forall(entry_point(M, Caller), put_mark(Caller)).

mark_rec(M:H) :-
    ( H == '<initialization>'
    ->forall(calls_to(H, Callee), mark_rec(Callee))
    ; predicate_property(M:H, interpreted)
    ->mark_caller(M:H)
    ; true
    ).

resolve_meta_goal(H, M, G) :-
    ( predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(M:H, Meta, G)
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

current_edge(Nodes, X, Y) :-
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
% results for performance reasons (edge/2), otherwise the analysis will take 20
% times more --EMM
%
sweep(M, FromChk, Pairs) :-
    findall(Node, unmarked(M, FromChk, Node), UNodes),
    sort(UNodes, Nodes),
    forall(current_edge(Nodes, X, Y), assertz(edge(X, Y))),
    findall(warning-edge(X, Y), edge(X, Y), Pairs, Tail),
    findall(warning-Row,
	    [Nodes, Row] +\ 
	    ( member(Node, Nodes),
	      findall(Loc/D, property_location(Node, D, Loc), LocDU),
	      sort(LocDU, LocDL),
	      findall(Caller, ( edge(Caller, Node),
				Caller \= Node
			      ), CallerL),
	      findall(Node, edge(Node, Node), LoopL),
	      length(CallerL, CallerN),
	      length(LoopL, LoopN),
	      findall(Callee, edge(Node, Callee), CalleeL),
	      length(CalleeL, NCallee),
	      Row = node(sort_by(CallerN, LoopN, NCallee), LocDL, Node)),
	    Tail).

% Due to the nature of this algorithm, its 'declarative' equivalent is by far
% more difficult to understand, maintain and much slower, instead it is
% implemented using dynamic facts.
audit:prepare_results(unused, Pairs, Results) :-
    maplist(\ (warning-Value)^Value^true, Pairs, Values),
    sort(Values, Sorted),
    partition(\ node(_, _, _)^true, Sorted, Nodes, Edges),
    retractall(node(_, _, _)),
    retractall(edge(_, _)),
    maplist(assertz, Nodes),
    maplist(assertz, Edges),
    compact_results(Compact),
    maplist(\ Result^(warning-Result)^true, Compact, Results).

compact_results(Results) :-
    findall(Result, compact_result(_, Result), Results).

compact_result(Node, node(SortBy, LocDL, Node, EdgeL)-Results) :-
    commited_retract(node(SortBy, LocDL, Node)),
    findall(Edge, ( clause(edge(Node, Edge), _, Ref),
		    \+ node(_, _, Edge),
		    erase(Ref)
		  ), EdgeL),	% Edges to already reported nodes
    findall(Result, ( commited_retract(edge(Node, Edge)),
		      compact_result(Edge, Result)
		    ), Results).

/*
sweep(Ref, Pairs) :-
    findall(warning-(Loc-(PI/D)), ( unmarked(Ref, PI),
				    property_location(PI, D, Loc)), Pairs).
*/

unmarked(M, FromChk, Node) :-
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
	; loc_dynamic(H, M, dynamic(def, _, _), _),
	  \+ marked(H, M, 0 ),
	  Node = MPI/0
	)
      )
    ; loc_dynamic(H, M, dynamic(def, _, _), _),
      functor(H, F, A),
      \+ current_defined_predicate(MPI),
      auditable_predicate(Ref),
      \+ entry_caller(M, H),
      \+ marked(H, M, 0 ),
      Node = MPI/0
    ),
    \+ hide_unused(H, M),
    check_pred_file(Ref, FromChk).
    
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
prolog:message(acheck(unused, Node-EdgeLL)) -->
    message_unused_node(Node, ['*', ' ']),
    maplist_dcg(maplist_dcg(message_unused_rec([' ', ' ', ' ', ' '])), EdgeLL).

message_unused_node(node(sort_by(N, L, _), LocDL, PI, _ARL), Level) -->
    { R is N + L,
      unused_type(R, T)
    },
    /* Uncomment to help debugging:
    ( { Level = ['*'|_],
	N \= 0
      }
    ->( {ARL \= []}
      ->['In ~w ~w, called from ~w: calls to unused ~w already reported'-[T, PI, L, ARL], nl]
      ; ['In ~w ~w: called from ~w'-[T, PI, L], nl]
      )
    ; []
    ),
    */
    maplist_dcg(message_unused(T, Level, PI), LocDL).

message_unused_rec(Level, Node-EdgeL) -->
    message_unused_node(Node, Level),
    maplist_dcg(message_unused_rec([' ', ' '|Level]), EdgeL).

message_unused(T, Level, PI, Loc/D) -->
    Level,
    Loc,
    ['~w ~w: ~w'-[T, D, PI], nl].

unused_type(0, 'unreferenced') :- !.
unused_type(_, 'unreachable' ).

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
