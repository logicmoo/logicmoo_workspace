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

:- module(argument_chains,
	  [gen_argument_chains/2,
	   argument_chain/2,
	   unlinked_arg/4,
	   arg_id/6,
	   lead_to_root/1,
	   linked_arg/2]).

:- use_module(library(extra_codewalk)).
:- use_module(library(implementation_module)).

:- dynamic
    clause_db/1,
    unlinked_arg/4,
    linked_arg/2,
    arg_id/6,
    counter/1.

counter(1).

count(Curr) :-
    retract(counter(Curr)),
    succ(Curr, Next),
    assertz(counter(Next)).

gen_argument_chains(AIL, OptionL0 ) :-
    retractall(clause_db(_)),
    retractall(arg_id(_, _, _, _, _, _)),
    retractall(linked_arg(_, _)),
    retractall(unlinked_arg(_, _, _, _)),
    forall(member(AI, AIL),
	   record_linked(AI, 0 )),
    merge_options(OptionL0, [source(false)], OptionL),
    check_argument_fixpoint(0, OptionL).

record_linked(IM:F/A-Pos, Stage) :-
    functor(H, F, A),
    record_linked(H, IM, _, Pos, Stage, 0).

check_argument_fixpoint(Stage, OptionL) :-
    succ(Stage, NStage),
    findall(P, ( arg_id(H, M, Idx, Pos, Stage, _),
		 functor(H, F, A),
		 ( nonvar(Idx)
		 ->P = M:F/A-Idx/Pos
		 ; P = M:F/A-Pos
		 )
	       ), L),
    length(L, N),
    print_message(information, format("Stage ~w: Checking ~w argument positions", [NStage, N])),
    extra_walk_code([source(false), on_etrace(propagate_argument_1(Stage, NStage))|OptionL]),
    print_message(information, format("Stage ~w: Collecting unlinked arguments", [NStage])),
    findall(Clause, retract(clause_db(Clause)), ClauseU),
    sort(ClauseU, ClauseL),
    extra_walk_code([source(false),
		     clauses(ClauseL),
		     on_etrace(propagate_argument_2(Stage, NStage))|OptionL]),
    ( \+ arg_id(_, _, _, _, NStage, _)
    ->true
    ; check_argument_fixpoint(NStage, OptionL)
    ).

propagate_argument_1(Stage, NStage, MGoal, MCaller, From) :-
    propagate_argument(argument_cond_1(Id), record_callee_1(Id), Stage, NStage, MGoal, MCaller, From).

argument_cond_1(Id, Goal, M, Pos, Stage, _, _) :-
    arg_id(Goal, M, _, Pos, Stage, Id),
    \+ ( arg_id(Goal, M, _, Pos, PStage, _),
	 PStage < Stage
       ).

record_callee_1(Id, _, _, _, Ref, Id) :- assertz(clause_db(Ref)).

propagate_argument_2(Stage, NStage, MGoal, MCaller, From) :-
    propagate_argument(argument_cond_2, record_callee_2, Stage, NStage, MGoal, MCaller, From).

argument_cond_2(Goal, M, Pos, _, NStage, CM:H-Idx/CPos) :-
    \+ arg_id(Goal, M, _, Pos, _, _),
    arg_id(H, CM, Idx, CPos, NStage, _).

record_callee_2(Goal, M, Pos, _, Id) :-
    functor(Goal, F, A),
    functor(H,    F, A),
    record_unlinked(H, M, Pos, Id).

record_unlinked(H, M, Pos, Id) :-
    ( unlinked_arg(H, M, Pos, Id)
    ->true
    ; count(Id),
      ( unlinked_arg(H, M, Pos, Id) -> gtrace ; true ),
      assertz(unlinked_arg(H, M, Pos, Id))
    ).

record_linked(H, M, Idx, Pos, Stage, Id) :-
    ( arg_id(H, M, Idx, Pos, _, Ref)
    ->true
    ; ( retract(unlinked_arg(H, M, Pos, Ref))
      ->true
      ; count(Ref)
      ),
      assertz(arg_id(H, M, Idx, Pos, Stage, Ref))
    ),
    ( linked_arg(Id, Ref)
    ->true
    ; assertz(linked_arg(Id, Ref))
    ).

:- meta_predicate propagate_argument(6,5,?,?,?,?,?).
propagate_argument(GoalCondition, RecordCallee, Stage, NStage, MGoal, MCaller, From) :-
    MGoal = _:Goal,
    compound(Goal),
    implementation_module(MGoal, IM),
    MCaller = CM:Caller,
    compound(Caller),
    functor(Caller, F, A),
    functor(H, F, A),
    From = clause(CRef),
    nth_clause(_, Idx, CRef),
    arg(Pos, Goal, Arg),
    \+ ( nonvar(Arg),
	 predicate_property(MGoal, meta_predicate(Meta)),
         arg(Pos, Meta, 0 )
       ),
    call(GoalCondition, Goal, IM, Pos, Stage, NStage, CM:H-Idx/CPos),
    arg(CPos, Caller, CArg),
    \+ ( arg_id(H, CM, Idx, CPos, PStage, _),
	 PStage < NStage
       ),
    ( term_variables(CArg, CVL),
      term_variables(Arg, VL),
      member(C, CVL),
      member(V, VL),
      C==V
    ->call(RecordCallee, Goal, IM, Pos, CRef, Id),
      record_linked(H, CM, Idx, CPos, NStage, Id)
    ),
    fail.

argument_chain(M:F/A-Idx/Pos, Chain) :-
    functor(H, F, A),
    arg_id(H, M, Idx, Pos, _, Id),
    argument_chain_rec(Id, Chain).

argument_chain_rec(Id, [M:F/A-Idx/Pos|Chain]) :-
    arg_id(H, M, Idx, Pos, _, Id), !,
    functor(H, F, A),
    linked_arg(Ref, Id),
    argument_chain_rec(Ref, Chain).
argument_chain_rec(_, []).

lead_to_root(Chain) :-
    lead_to_root([], Chain).

lead_to_root(Chain0, Chain) :-
    linked_arg(0, Id),
    lead_to_root(Id, Chain0, Chain).

lead_to_root(Id, Chain, [Id|Chain]).
lead_to_root(Id, Chain0, Chain) :-
    linked_arg(Id, Id2),
    \+ memberchk(Id2, [Id|Chain0 ]),
    lead_to_root(Id2, [Id|Chain0 ], Chain).
