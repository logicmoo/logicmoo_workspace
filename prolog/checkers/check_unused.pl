/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(check_unused, []).

/*
  
  This analyzer is based on the Mark-and-Sweep Algorithm:
  http://www.brpreiss.com/books/opus5/html/page424.html

*/

:- use_module(checkers(checker)).
:- use_module(library(apply)).
:- use_module(library(clambda)).
:- use_module(library(commited_retract)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(checkable_predicate)).
:- use_module(library(implementation_module)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(extra_codewalk)).
:- use_module(library(extra_location)).
:- use_module(library(is_entry_point)).
:- use_module(library(location_utils)).
:- use_module(library(ungroup_keys_values)).

:- multifile
    prolog:message//1.

:- dynamic
    calls_to_initialization/2,
    calls_to_assertion/4,
    calls_to_declaration/2,
    calls_to_clause/3,
    calls_to_predid/4,
    marked_assertion/2,
    marked_predid/2,
    marked_clause/1,
    marked_initialization/0,
    marked_declaration/0,
    edge/5.

:- public collect_unused/4.
collect_unused(M, MGoal, Caller, From) :-
    record_location_meta(MGoal, M, From, all_call_refs, cu_caller_hook(Caller)).

checker:check(unused, Result, OptionL) :-
    check_unused(OptionL, Result).

check_unused(OptionL, Pairs) :-
    extra_walk_code([source(false), on_etrace(collect_unused(M))|OptionL], M, FromChk),
    mark(M),
    sweep(M, FromChk, Pairs),
    cleanup_unused.

cleanup_unused :-
    retractall(calls_to_initialization(_, _)),
    retractall(calls_to_assertion(_, _, _, _)),
    retractall(calls_to_declaration(_, _)),
    retractall(calls_to_clause(_, _, _)),
    retractall(calls_to_predid(_, _, _, _)),
    retractall(marked_clause(_)),
    retractall(marked_assertion(_, _)),
    retractall(marked_predid(_, _)),
    retractall(marked_initialization),
    retractall(marked_declaration),
    retractall(edge(_, _, _, _, _)).

marked('<assertion>'(M:H)) :- marked_assertion(H, M).
marked(M:H)                :- marked_predid(H, M).
marked(clause(Ref))        :- marked_clause(Ref).
marked('<initialization>') :- marked_initialization.
marked('<declaration>')    :- marked_declaration.

record_marked('<assertion>'(M:H)) :- assertz(marked_assertion(H, M)).
record_marked(M:H)                :- assertz(marked_predid(H, M)).
record_marked(clause(Ref))        :- assertz(marked_clause(Ref)).
record_marked('<initialization>') :- assertz(marked_initialization).
record_marked('<declaration>'   ) :- assertz(marked_declaration).

is_entry_caller('<assertion>'(M:H)) :- entry_caller(M, H).
is_entry_caller('<initialization>').
is_entry_caller('<declaration>'   ).
is_entry_caller(M:H) :- entry_caller(M, H).
is_entry_caller(clause(Ref)) :-
    match_head_clause(M:H, Ref), % Unify head
    entry_caller(M, H).

entry_caller(M, H) :-
    ( is_entry_point(H, M) -> true
    ; loc_declaration(H, M, goal, _)
    ).

entry_point(M, Caller) :-
    calls_to(Caller, M, _),
    is_entry_caller(Caller).

mark(M) :-
    forall(entry_point(M, Caller), put_mark(Caller)).

resolve_meta_goal(H, M, G) :-
    ( ( predicate_property(M:H, meta_predicate(Meta))
				% don't use inferred_meta_predicate(M:H, Meta)
				% since actually it is not being used by the
				% compiler and would lead to incorrect results
      )
    ->qualify_meta_goal(M:H, Meta, G)
    ; G = H
    ).

% mark_to_head(clause(CRef), M:H-I) :- !,
%     nth_clause(M:H, I, CRef).
% mark_to_head(Ref, Ref).

is_marked(CRef) :-
    copy_term(CRef, Term),
    marked(Term),
    subsumes_term(Term, CRef).

put_mark(CRef) :-
    % mark_to_head(CRef, Head),
    % writeln(user_error, put_mark(Head)),
    ( \+ is_marked(CRef)
    ->record_marked(CRef),
      forall(calls_to(CRef, CM, Callee),
	     mark_rec(Callee, CM))
    ; true
    ).

mark_rec(H, M) :-
    resolve_meta_goal(H, M, G),
    forall(gen_lit_marks(G, M, CRef), % Widening
	   put_mark(CRef)).

% Generalization step, we lose precision but avoid loops --EMM
%
% The order of this clauses matters, because we record as marked from the most
% specific to the most generic one !!!
%
% The logic is: a call to a predicate will potentially use:
%
% (1) all the assertions
% (2) the clauses that match, and
% (3) the dynamic calls that match
%
gen_lit_marks(G, M, '<assertion>'(M:P)) :-
    functor(G, F, A),
    functor(P, F, A).	       % Build a fresh head without undesirable bindings
gen_lit_marks(G, M, clause(Clause)) :-
    match_head_clause(M:G, Clause),
    clause_property(Clause, file(_)).	 % Static clauses only
gen_lit_marks(G, M, M:P) :- copy_term(G, P). % copy term to avoid undesirable bindings

gen_marks(Ref, Ref).
gen_marks('<assertion>'(M:H), clause(Clause)) :-
    match_head_clause(M:H, Clause),
    clause_property(Clause, file(_)).

not_marked(Ref) :-
    \+ ( gen_marks(Ref, Mark),
	 marked(Mark)
       ).

not_marked(H, M) :-
    \+ ( gen_lit_marks(H, M, Mark),
	 marked(Mark)
       ).

match_head_clause(MH, Clause) :-
    catch(clause(MH, _, Clause), _, fail).

current_edge(X, Y) :-
    PI = M:F/A,
    ( X = PI
    ->functor(H, F, A),
      ( CRef = M:H
      ; match_head_clause(M:H, Clause),
	CRef = clause(Clause)
      )
    ; X = PI/I,
      I > 0
    ->functor(H, F, A),
      nth_clause(M:H, I, Clause),
      CRef = clause(Clause)
    ; X = PI/(-1)
    ->functor(H, F, A),
      CRef = '<assertion>'(M:H)
    ; X = PI/0
    ->functor(H, F, A),
      CRef = M:H
    ),
    calls_to(CRef, M2, H2),
    functor(H2, F2, A2),
    PI2 = M2:F2/A2,
    ( Y = PI2
    ; ( match_head_clause(M2:H2, YRef),
	nth_clause(_, I2, YRef),
	Y = PI2/I2
      ; %% extra_location(H2, M2, dynamic(use, _, _), _),
	Y = PI2/0
      )
    ).

% Note: although is not nice, we are using dynamic predicates to cache partial
% results for performance reasons (edge/2), otherwise the analysis will take 20
% times more --EMM
%
sweep(M, FromChk, Pairs) :-
    findall(node(Node, D, From), unmarked(M, FromChk, Node, D, From), UNodes),
    sort(UNodes, Nodes),
    findall(node(X, DX, LX)-NodeY,
	    ( member(node(X, DX, FX), Nodes),
	      from_location(FX, LX),
	      ( NodeY=node(Y, DY, LY),
	        current_edge(X, Y),
		memberchk(node(Y, DY, FY), Nodes),
		\+ hide_unused_from(Y, FY)
	      *->
		from_location(FY, LY)
	      ; NodeY=[]
	      )
	    ), EdgeU),
    sort(EdgeU, EdgeL),
    group_pairs_by_key(EdgeL, AdjL),
    maplist(add_sort_by(EdgeL), AdjL, AdjSG),
    ungroup_keys_values(AdjSG, AdjSL),
    ungroup_keys_values([warning-AdjSL], Pairs).

add_sort_by(EdgeL, Node-CalleeL, sort_by(InclN, LoopN, CalleeN)/Node-CalleeL) :-
    findall(_, member(_-Node, EdgeL), CallerL),
    ( partition(\=(Node), CallerL, InclL, LoopL),
      length(InclL, InclN),
      length(LoopL, LoopN)
    ),
    length(CalleeL, CalleeN).

% Due to the nature of this algorithm, its 'declarative' equivalent is by far
% more difficult to understand, maintain and much slower, instead it is
% implemented using dynamic facts.
checker:prepare_results(unused, Pairs, Results) :-
    maplist(\ (warning-Value)^Value^true, Pairs, Values),
    sort(Values, Sorted),
    maplist(assert_edge, Sorted),
    compact_results(Compact),
    maplist(\ Result^(warning-Result)^true, Compact, Results).

assert_edge(SortBy/node(X, D, L)-Node) :-
    assert(edge(SortBy, X, D, L, Node)).

compact_results(Results) :-
    findall(Result, compact_result(_, Result), Results).

compact_result(node(X, D, L), node(SortBy, L, D, X)-ResultL) :-
    repeat,
      ( edge(SortBy, X, D, L, _)
      ->true
      ; !,
	fail
      ),
      findall(Result,
	      ( commited_retract(edge(_, X, D, L, Edge)),
		Edge \= node(X, D, L), % loop
		compact_result(Edge, Result)
	      ), ResultU),
      sort(ResultU, ResultL).

/*
sweep(Ref, Pairs) :-
    findall(warning-(Loc-(PI/D)), ( unmarked(Ref, PI),
				    property_location(PI, D, Loc)), Pairs).
*/

semantic_head(H, M, 0, Dynamic, M:H, From) :-
    Dynamic = dynamic(def, _, _),
    loc_dynamic(H, M, Dynamic, From).
semantic_head(H, M, -1, assertion(S, T), '<assertion>'(M:H), From) :-
    assrt_lib:head_prop_asr(H, CM, S, T, _, _, From, _),
    implementation_module(CM:H, M).

unmarked(M, FromChk, Node, D, From) :-
    Ref = M:H,
    MPI = M:F/A,
    ( current_defined_predicate(MPI),
      functor(H, F, A),
      checkable_predicate(Ref),
      \+ entry_caller(M, H),
      ( not_marked(H, M)
      ->Node = MPI,
	property_from(Ref, D, From),
	check_pred_file(Ref, FromChk, From)
      ; ( match_head_clause(M:H, CRef),
	  clause_property(CRef, file(_)), % Static clauses only
	  From = clause(CRef),
	  not_marked(From),
	  check_pred_file(Ref, FromChk, From),
	  nth_clause(M:H, I, CRef),
	  D = clause(I)
	; semantic_head(H, M, I, D, Mark, From),
	  not_marked(Mark),
	  check_pred_file(Ref, FromChk, From)
	),
	Node = MPI/I
      )
    ; semantic_head(H, M, I, D, Mark, From),
      not_marked(Mark),
      functor(H, F, A),
      check_pred_file(Ref, FromChk, From),
      \+ current_defined_predicate(MPI),
      checkable_predicate(Ref),
      \+ entry_caller(M, H),
      Node = MPI/I
    ).

check_pred_file(Ref, FromChk, From) :-
    \+ hide_unused_from(Ref, From),
    call(FromChk, From), !.

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
    foldl(foldl(message_unused_rec([' ', ' ', ' ', ' '])), EdgeLL).

message_unused_node(node(sort_by(N, L, _), F, D, PI), Level) -->
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
    message_unused(T, Level, PI, F/D).

message_unused_rec(Level, Node-EdgeL) -->
    message_unused_node(Node, Level),
    foldl(message_unused_rec([' ', ' '|Level]), EdgeL).

message_unused(T, Level, PI, Loc/D) -->
    Level,
    Loc,
    ['~w ~w: ~w'-[T, D, PI], nl].

unused_type(0, 'unreferenced') :- !.
unused_type(_, 'unreachable' ).

% Hook to hide unused messages:
:- multifile
    hide_unused/2,
    hide_unused_from/2.

hide_unused('$exported_op'(_, _, _), _).
hide_unused(attr_unify_hook(_, _), predopts_analysis).
hide_unused(loading(_), shlib).
hide_unused('pce catcher'(_, _), pce_global).
hide_unused(attribute_goals(_, _, _), M) :- unused_mo_clpfd(M).
hide_unused(attr_unify_hook(_, _),    M) :- unused_mo_clpfd(M).
hide_unused(_, plunit).
hide_unused(_, ciao).
hide_unused(Call, _) :-
    functor(Call, Name, _),
    atom_concat('__aux_wrapper_', _, Name).
hide_unused(Call, _) :-
    current_predicate(apply_macros:maplist_expansion/1),
    apply_macros:maplist_expansion(Call).

hide_unused_from( M:F/A,    _) :- functor(H, F, A), hide_unused(H, M).
hide_unused_from((M:F/A)/_, _) :- functor(H, F, A), hide_unused(H, M).

unused_mo_clpfd(clpfd_original).
unused_mo_clpfd(clpfd_relation).
unused_mo_clpfd(clpfd_gcc_occurred).
unused_mo_clpfd(clpfd_gcc_num).
unused_mo_clpfd(clpfd_gcc_vs).
unused_mo_clpfd(clpfd_gcc_aux).
unused_mo_clpfd(clpfd_aux).

caller_ptr('<initialization>', _, '<initialization>') :- !.
caller_ptr('<assertion>'(AH),  _, '<assertion>'(AH) ) :- !.
caller_ptr('<declaration>',    _, '<declaration>'   ) :- !.
caller_ptr(_,        clause(Ptr), clause(Ptr)       ) :- !.
caller_ptr(M:H,                _, M:H).

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
      ( \+ calls_to(Ptr, M, Head)
      ->record_calls_to(Ptr, M, Head)
      ; true
      )
    ; true
    ).

calls_to('<initialization>',   M, Head) :- calls_to_initialization(   Head, M).
calls_to('<assertion>'(AM:AH), M, Head) :- calls_to_assertion(AH, AM, Head, M).
calls_to('<declaration>',      M, Head) :- calls_to_declaration(      Head, M).
calls_to(clause(Ref),          M, Head) :- calls_to_clause(Ref,       Head, M).
calls_to(CM:CH,                M, Head) :- calls_to_predid(CH, CM,    Head, M).

record_calls_to('<initialization>',   M, Head) :- assertz(calls_to_initialization(   Head, M)).
record_calls_to('<assertion>'(AM:AH), M, Head) :- assertz(calls_to_assertion(AH, AM, Head, M)).
record_calls_to('<declaration>',      M, Head) :- assertz(calls_to_declaration(      Head, M)).
record_calls_to(clause(Ref),          M, Head) :- assertz(calls_to_clause(Ref,       Head, M)).
record_calls_to(CM:CH,                M, Head) :- assertz(calls_to_predid(CH, CM,    Head, M)).
