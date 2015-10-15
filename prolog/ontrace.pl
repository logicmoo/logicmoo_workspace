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

:- module(ontrace, [ontrace/3,
		    call_inoutex/3]).

:- use_module(library(apply)).
:- use_module(library(edinburgh)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(prolog_clause), []).
:- use_module(library(prolog_codewalk), []).
:- use_module(library(prolog_source)).
:- use_module(xlibrary(clambda)).

:- meta_predicate ontrace(0,6,+).

ontrace(Goal, OnTrace, OptionL) :-
    State=state(_, _, _),	% Allow destructive assignment
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

% Kludge to get an anonymous true/1 predicate:
:- meta_predicate true_1(1,-).
true_1(G, G).

%% setup_trace(!State, :OnTrace, +OptL) is det.
setup_trace(State, M:OnTrace, OptL) :-
    true_1(\_^true, True1),
    select_option(goal(ValidGoal), OptL,  OptL1, True1),
    select_option(file(ValidFile), OptL1, OptL2, True1),
    %% redo port have weird bugs, ignoring it for now:
    select_option(ports(PortList), OptL2, _,
		  [call, exit, fail, unify, exception]),
    asserta((user:prolog_trace_interception(Port, Frame, PC, Action)
	    :- ignore(trace_port(Port, Frame, PC, M:OnTrace, M:ValidGoal,
				 M:ValidFile, Action))),
	    Ref),
    foldl(port_mask, PortList, 0, Mask),
    '$visible'(Visible, Mask),
    '$leash'(Leash, Mask),
    nb_setarg(1, State, Visible),
    nb_setarg(2, State, Leash),
    nb_setarg(3, State, Ref),
    trace.

%% cleanup_state(+State) is det.
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
    find_parents(Port, Frame, ParentL, RFrame, Cl, SubLoc),
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
    !,
    call(OnTrace, Port, Frame, PC, ParentL, SubLoc, Action).
do_trace_port(_, _, _, _, _, _, continue).

find_parents(Port, Frame, ParentL, RFrame, Cl, Loc) :-
    ( % Due to a bug in SWI-Prolog, we can not rely on redo(PC) +
      % $clause_term_position(Cl,PC,List), in any case, the coverage of builtins
      % is not big deal, compared to the coverage of custom predicates --EMM
      member(Port, [redo(_PC), unify /*, exit*/])
    % TODO: if exit placed here, then it is marked in the clause, else in the
    % literal, perhaps would be good to have exit_clause and exit_lit
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

:- dynamic
    clause_location_cache/3.
:- volatile
    clause_location_cache/3.

clause_pc_location(Clause, PC, Loc) :-
    clause_location_cache(Clause, PC, Loc), !.
clause_pc_location(Clause, PC, Loc) :-
    ( '$clause_term_position'(Clause, PC, List)
    ->clause_subloc(Clause, List, Loc)
    ; Loc = clause(Clause)
    ),
    assertz(clause_location_cache(Clause, PC, Loc)).

prolog:message_location(clause_pc(Clause, PC)) -->
    {clause_pc_location(Clause, PC, Loc)},
    prolog:message_location(Loc).

%% clause_subloc(+ClauseRef, +List, -SubLoc) is det.
%
clause_subloc(Cl, List, SubLoc) :-
    ( clause_property(Cl, file(File)),
      clause_property(Cl, line_count(Line)),
      clause_property(Cl, module(Module))
    ->( read_term_at_line(File, Line, Module, Term, TermPos)
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
      )
    ; SubLoc = clause(Cl)
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
