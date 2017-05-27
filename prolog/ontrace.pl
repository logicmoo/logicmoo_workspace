/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(ontrace, [ontrace/3,
                    clause_pc_location/3,
		    cleanup_trace/1,
                    call_inoutex/3]).

:- use_module(library(apply)).
:- use_module(library(edinburgh)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(tabling)).
:- use_module(library(prolog_clause), []).
:- use_module(library(prolog_codewalk), []).
:- use_module(library(prolog_source)).
:- use_module(library(clambda)).

:- meta_predicate ontrace(0,6,+).

ontrace(Goal, OnTrace, OptionL) :-
    State=state(_, _, _),       % Allow destructive assignment
    call_inoutex(Goal,
        setup_trace(State, OnTrace, OptionL),
        cleanup_trace(State)).

:- meta_predicate call_inoutex(0,0,0).
call_inoutex(Goal, OnIn, OnOut) :-
    catch(call_inout(Goal, OnIn, OnOut),
          E,  (OnOut, throw(E))).

call_inout(Goal, OnIn, OnOut) :-
    (OnIn;OnOut,fail),
    prolog_current_choice(C0),
    Goal,
    prolog_current_choice(C1),
    (OnOut;OnIn,fail),
    (C0==C1 -> ! ;true).

:- public true_1/1.
true_1(_).

%!  setup_trace(!State, :OnTrace, +OptL) is det.
%
setup_trace(State, M:OnTrace, OptL) :-
    select_option(goal(ValidGoal), OptL,  OptL1, ontrace:true_1),
    select_option(file(ValidFile), OptL1, OptL2, ontrace:true_1),
    %% redo port have weird bugs, ignoring it for now:
    select_option(ports(PortList), OptL2, _,
                  [call, exit, fail, unify, exception]),
    asserta((user:prolog_trace_interception(Port, Frame, PC, Action) :-
                 ( trace_port(Port, Frame, PC, M:OnTrace, M:ValidGoal,
                              M:ValidFile, Action)
                 ->true
                 ; Action = continue
                 )), Ref),
    foldl(port_mask, PortList, 0, Mask),
    '$visible'(Visible, Mask),
    '$leash'(Leash, Mask),
    nb_setarg(1, State, Visible),
    nb_setarg(2, State, Leash),
    nb_setarg(3, State, Ref),
    trace.

%!  cleanup_state(+State) is det.
%
cleanup_trace(state(Visible, Leash, Ref)) :-
    nodebug,
    '$visible'(_, Visible),
    '$leash'(_, Leash),
    erase(Ref),
    !.
cleanup_trace(State) :-
    print_message(error, format('Failed when saving tracer data', [State])),
    fail.

port_mask(Port, Mask0, Mask) :- '$syspreds':port_name(Port, Bit),
    Mask is Mask0\/Bit.

user_defined_module(M) :-
    M \= ontrace,
    module_property(M, class(user)).

:- public trace_port/7.
:- meta_predicate trace_port(+,+,+,5,1,1,-).
trace_port(Port, Frame, PC, OnTrace, ValidGoal, ValidFile, Action) :-
    do_trace_port(Port, Frame, PC, OnTrace, ValidGoal, ValidFile, Action).

do_trace_port(Port, Frame, PC, OnTrace, ValidGoal, ValidFile, Action) :-
    prolog_frame_attribute(Frame,  goal, M:H), % M:H to skip local predicates
    \+ \+ call(ValidGoal, M:H),
    ignore(( Port = (exit),
             prolog_frame_attribute(Frame, clause, ExCl),
             % Trace exit at clause level:
             check_and_call(exitcl, Frame, PC, OnTrace, ValidGoal, ValidFile,
                            _, [], Frame, ExCl, clause(ExCl))
           )),
    find_parents(Port, Frame, ParentL, RFrame, Cl, SubLoc),
    check_and_call(Port, Frame, PC, OnTrace, ValidGoal, ValidFile, Action,
                   ParentL, RFrame, Cl, SubLoc), !.
do_trace_port(_, _, _, _, _, _, continue).

check_and_call(Port, Frame, PC, OnTrace, ValidGoal, ValidFile, Action,
               ParentL, RFrame, Cl, SubLoc) :-
    prolog_frame_attribute(RFrame, goal, CM:CH),
    ( ( clause_property(Cl, file(File))
      ; module_property(CM, file(File))
      )
    -> \+ \+ call(ValidFile, File)
    ; true
    ),
    \+ \+ call(ValidGoal, CM:CH),
    \+ \+ ( member(F, [Frame|ParentL]),
            prolog_frame_attribute(F, goal, PM:_),
            user_defined_module(PM)
          ),
    call(OnTrace, Port, Frame, PC, ParentL, SubLoc, Action).

find_parents(Port, Frame, ParentL, RFrame, Cl, Loc) :-
    ( member(Port, [unify, redo(_)])
    ->ParentL = [],
      prolog_frame_attribute(Frame, clause, Cl),
      RFrame = Frame,
      Loc = clause(Cl)
    ; find_parent_with_pc(Frame, PC, [], ParentL),
      [Parent|_] = ParentL,
      prolog_frame_attribute(Parent, clause, Cl),
      RFrame = Parent,
      Loc = clause_pc(Cl, PC)
    ).

find_parent_with_pc(Frame, PC, List0, List) :-
    prolog_frame_attribute(Frame, parent, Parent),
    ( prolog_frame_attribute(Frame, pc, PC)
    ->List = [Parent|List0 ]
    ; find_parent_with_pc(Parent, PC, [Parent|List0 ], List)
    ).

:- multifile
    prolog:message_location//1.

:- table
    clause_pc_location/3.

clause_pc_location(Clause, PC, Loc) :-
    ( '$clause_term_position'(Clause, PC, List)
    ->clause_subloc(Clause, List, Loc)
    ; Loc = clause(Clause)
    ).

prolog:message_location(clause_pc(Clause, PC)) -->
    {clause_pc_location(Clause, PC, Loc)},
    '$messages':swi_location(Loc).

%!  clause_subloc(+ClauseRef, +List, -SubLoc) is det.
%
clause_subloc(Cl, List, SubLoc) :-
    ( clause_property(Cl, file(File)),
      clause_property(Cl, line_count(Line)),
      clause_property(Cl, module(Module))
    ->file_line_module_subloc(Cl, List, File, Line, Module, SubLoc)
    ; SubLoc = clause(Cl)
    ).

file_line_module_subloc(Cl, List, File, Line, Module, SubLoc) :-
    ( read_term_at_line(File, Line, Module, Term, TermPos)
    % Usage of term positions has priority
    ->( prolog_clause:ci_expand(Term, ClauseL, Module, TermPos, ClausePos),
        match_clause(Cl, ClauseL, Module, List2, List),
        nonvar(ClausePos)
      ->( foldl(find_subgoal, List2, ClausePos, SubPos), % Expensive
          nonvar(SubPos)
        ->true
        ; SubPos = ClausePos
        )
      ; SubPos = TermPos
      ),
      SubLoc = file_term_position(File, SubPos)
    ; SubLoc = file(File, Line, -1, _)
    ).

read_term_at_line(File, Line, Module, Clause, TermPos) :-
    setup_call_cleanup(
        '$push_input_context'(trace_info),
        read_term_at_line_2(File, Line, Module, Clause, TermPos),
        '$pop_input_context').

read_term_at_line_2(File, Line, Module, Clause, TermPos) :-
    catch(open(File, read, In), _, fail),
    set_stream(In, newline(detect)),
    call_cleanup(
        read_source_term_at_location(
            In, Clause,
            [ line(Line),
              module(Module),
              subterm_positions(TermPos)
            ]),
        close(In)).

list_pos(term_position(_, _, _, _, PosL), PosL).
list_pos(list_position(_, _, PosL, _), PosL).
list_pos(parentheses_term_position(_, _, Pos1), Pos) :-
    nonvar(Pos1),
    list_pos(Pos1, Pos).

find_subgoal(A, TermPos, Pos) :-
    list_pos(TermPos, PosL),
    is_list(PosL),
    nth1(A, PosL, Pos),
    nonvar(Pos), !.

match_clause(Ref, ClauseL, Module, List, Tail) :-
    % format(user_error, '~N~w',[match_clause(Ref, ClauseL, Module, List, Tail)]),
    ( is_list(ClauseL),
      clause(Head, Body, Ref),
      nth1(Pos, ClauseL, Clause),
      % format(user_error, '~N~w',[normalize_cl(Clause, Module, Module, NClause)]),
      normalize_cl(Clause, Module, Module, NClause),
      NClause =@= (Head :- Body)
    ->List = [Pos|Tail]
    ; List = Tail
    ).

normalize_cl(M:Clause, _, CM, NClause) :- !,
    normalize_cl(Clause, M, CM, NClause).
normalize_cl((Head :- Body), M, CM, (MHead :- NBody)) :- !,
    strip_mod(Head, M, MHead),
    strip_mod(Body, CM, MBody),
    ( MBody = M:Body
    ->NBody = Body
    ; NBody = MBody
    ).
normalize_cl(Head, M, CM, NClause) :-
    normalize_cl((Head :- true), M, CM, NClause).

strip_mod(M:Term, _, MTerm) :-
    strip_mod(Term, M, MTerm).
strip_mod(Term, M, M:Term).
