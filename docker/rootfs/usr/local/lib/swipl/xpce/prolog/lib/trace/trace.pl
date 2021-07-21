/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  2001-2016, University of Amsterdam
                              VU University Amsterdam
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

:- module(pce_prolog_tracer,
          [ prolog_show_frame/2         % +Frame, +Options
          ]).
:- use_module(library(pce)).
:- use_module(library(prolog_clause)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- consult([ clause,
             util,
             source,
             gui,
             stack
           ]).

:- initialization
   visible(+cut_call).


                 /*******************************
                 *            INTERCEPT         *
                 *******************************/

%!  with_access_user(:Goal) is det.
%
%   Run Goal with set_prolog_flag(access_level,user)

:- meta_predicate with_access_user(0).
:- '$hide'(with_access_user/1).  % Just hide entry and leave children tracable

with_access_user(G) :-
    notrace(( current_prolog_flag(access_level, Was),
              set_prolog_flag(access_level, user))),
    setup_call_cleanup(
        true,
        G,
        notrace(set_prolog_flag(access_level, Was))).


:- thread_local
    finished_frame/1,
    last_action/1,
    show_unify_as/2.

user:prolog_trace_interception(Port, Frame, CHP, Action) :-
    with_access_user(prolog_trace_interception_gui(Port, Frame, CHP, Action)).

prolog_trace_interception_gui(Port, Frame, CHP, Action) :-
    current_prolog_flag(gui_tracer, true),
    (   notrace(intercept(Port, Frame, CHP, GuiAction)),
        map_action(GuiAction, Frame, Action)
    ->  true
    ;   print_message(warning,
                      guitracer(intercept_failed(Port, Frame,
                                                 CHP, Action))),
        Action = continue
    ).

:- initialization
    prolog_unlisten(frame_finished, retract_frame),
    prolog_listen(frame_finished, retract_frame).

retract_frame(Frame) :-
    retractall(finished_frame(Frame)).

%!  map_action(+GuiAction, +Frame, -Action) is det.
%
%   Map the abstract action of the gui-tracer into actions for the
%   low-level tracer.  Runs in the debugged thread.
%
%   @tbd    The argument frame is not used.  Delete?

map_action(creep, _, continue) :-
    traceall.
map_action(skip, _Frame, skip) :-
    trace.
map_action(into, _, continue) :-
    visible(+unify),
    traceall.
map_action(leap, _, continue) :-
    prolog_skip_level(_, very_deep),
    notrace.
map_action(retry, _, retry(Frame)) :-
    traceall,
    get_tracer(selected_frame, Frame).
map_action(fail, _, fail) :-
    traceall.
map_action(nodebug, _, nodebug).
map_action(abort, _, abort).
map_action(halt, _, continue) :-
    halt.
map_action(finish, _, continue) :-
    get_tracer(selected_frame, Frame),
    asserta(finished_frame(Frame)),
    trace,
    prolog_skip_frame(Frame).

%!  traceall is det.
%
%   Go into non-skipping trace mode.

traceall :-
    prolog_skip_level(_, very_deep),
    trace.

%!  intercept(+Port, +Frame, +Choice, -Action) is semidet.
%
%   Toplevel of the tracer interception.  Runs in debugged thread.

intercept(Port, Frame, CHP, Action) :-
    with_access_user(intercept_(Port, Frame, CHP, Action)).

intercept_(Port, Frame, CHP, Action) :-
    prolog_frame_attribute(Frame, predicate_indicator, PI),
    debug(gtrace(intercept),
          '*** do_intercept ~w, ~w, ~w: ~q ...', [Port, Frame, CHP, PI]),
    visible(-unify),
    (   do_intercept(Port, Frame, CHP, Action0)
    ->  true
    ;   debug(gtrace(intercept), 'Intercept failed; creeping', []),
        Action0 = creep
    ),
    fix_action(Port, Action0, Action),
    debug(gtrace(intercept), '*** ---> Action = ~w', [Action]),
    send_if_tracer(report(status, '%s ...', Action)),
    retractall(last_action(_)),
    asserta(last_action(Action)).

fix_action(fail, skip,   creep) :- !.
fix_action(exit, skip,   creep) :- !.
fix_action(_,    Action, Action).

%!  do_intercept(+Port, +Frame, +Choice, -Action) is det.
%
%   Actual core of the tracer intercepting code. Runs in the
%   debugged thread.

do_intercept(Port, Frame, CHP, Action) :-
    with_access_user(do_intercept_(Port, Frame, CHP, Action)).

do_intercept_(call, Frame, CHP, Action) :-
    (   \+ hide_children_frame(Frame),
        (   last_action(retry)
        ;   prolog_frame_attribute(Frame, top, true),
            debug(gtrace(intercept), 'Toplevel frame', [])
        ;   prolog_frame_attribute(Frame, parent, Parent),
            (   prolog_frame_attribute(Parent, hidden, true)
            ;   prolog_frame_attribute(Parent, goal, ParentGoal),
                predicate_property(ParentGoal, nodebug)
            )
        )
    ->  Action = into,
        asserta(show_unify_as(Frame, call))
    ;   show(Frame, CHP, 1, call),
        action(Action)
    ).
do_intercept_(exit, Frame, CHP, Action) :-
    (   \+ hide_children_frame(Frame),
        \+(( prolog_frame_attribute(Frame, skipped, true),
             \+ finished_frame(Frame),
             prolog_skip_level(L,L),
             L \== very_deep,
             prolog_frame_attribute(Frame, level, FL),
             FL >= L
          ))
    ->  show(Frame, CHP, 0, exit),
        action(Action)
    ;   last_action(leap)
    ->  Action = leap
    ;   Action = creep
    ).
do_intercept_(fail, Frame, CHP, Action) :-
    show(Frame, CHP, 1, fail),
    action(Action).
do_intercept_(exception(Except), Frame, CHP, Action) :-
    (   prolog_frame_attribute(Frame, goal, Goal),
        predicate_property(Goal, interpreted)
    ->  Up = 0
    ;   Up = 1                      % foreign, undefined, ...
    ),
    show(Frame, CHP, Up, exception(Except)),
    action(Action).
do_intercept_(redo(_), Frame, CHP, Action) :-
    (   hide_children_frame(Frame)
    ;   prolog_skip_level(redo_in_skip, redo_in_skip)
    ),                                     % inside black box or skipped goal
    !,
    show(Frame, CHP, 1, redo),
    action(Action).
do_intercept_(redo(0), Frame, _CHP, into) :-   % next clause
    !,
    asserta(show_unify_as(Frame, redo)).
do_intercept_(redo(_PC), _Frame, _CHP, creep).  % internal branch
do_intercept_(unify, Frame, CHP, Action) :-
    (   show_unify_as(Frame, How)
    ;   How = unify
    ),
    !,
    retractall(show_unify_as(_, _)),
    debug(gtrace(port), 'Show unify port as ~w', [How]),
    show(Frame, CHP, 0, unify, How),
    prolog_frame_attribute(Frame, goal, Goal),
    predicate_name(user:Goal, Pred),
    send_tracer(report(status, '%s: %s', How?label_name, Pred)),
    action(Action).
do_intercept_(cut_call(PC), Frame, CHP, Action) :-
    prolog_frame_attribute(Frame, goal, Goal),
    predicate_name(user:Goal, Pred),
    send_tracer(report(status, 'Cut in: %s', Pred)),
    prolog_show_frame(Frame,
                      [ pc(PC),
                        choice(CHP),
                        port(call),
                        style(call),
                        stack,
                        source,
                        bindings
                      ]),
    action(Action).
do_intercept_(cut_exit(PC), Frame, CHP, Action) :-
    prolog_show_frame(Frame,
                      [ pc(PC),
                        choice(CHP),
                        port(exit),
                        style(call),
                        stack,
                        source,
                        bindings
                      ]),
    action(Action).

%!  hide_children_frame(+Frame) is semidet.
%
%   True if Frame runs a goal for which we must hide the children.

hide_children_frame(Frame) :-
    prolog_frame_attribute(Frame, goal, Goal),
    (   predicate_property(Goal, nodebug)
    ->  true
    ;   predicate_property(Goal, foreign)
    ).


%!  show(+StartFrame, +Choice, +Up, +Port) is semidet.
%
%   Show current location from StartFrame.  Must be called in the
%   context of the debugged thread.
%
%   @param Up       Skip bottom Up frames.  Use to show call port
%                   in the parent frame.

show(StartFrame, CHP, Up, exception(Except)) :-
    !,
    show(StartFrame, CHP, Up, exception, exception),
    message_to_string(Except, Message, 200),
    send_tracer(report(warning, 'Exception: %s', Message)).
show(StartFrame, CHP, Up, Port) :-
    show(StartFrame, CHP, Up, Port, Port),
    prolog_frame_attribute(StartFrame, goal, Goal),
    predicate_name(user:Goal, Pred),
    (   Port == redo,
        prolog_frame_attribute(StartFrame, skipped, true)
    ->  send_tracer(report(status, '%s: %s (skipped)', Port?label_name, Pred))
    ;   send_tracer(report(status, '%s: %s', Port?label_name, Pred))
    ).
show(StartFrame, CHP, Up, Port, Style) :-
    find_frame(Up, StartFrame, Port, PC, Frame),
    send_tracer(trapped_location(StartFrame, Frame, Port)),
    prolog_show_frame(StartFrame,
                      [ port(Port),
                        choice(CHP),
                        stack
                      ]),
    prolog_show_frame(Frame,
                      [ pc(PC),
                        port(Port),
                        style(Style),
                        source,     % may fail
                        bindings
                      ]).

message_to_string(Except, Message, MaxLength) :-
    catch(message_to_string(Except, Message0), _, fail),
    string_length(Message0, Len),
    (   Len > MaxLength
    ->  sub_string(Message0, 0, MaxLength, _, Base),
        string_concat(Base, ' ...', Message)
    ;   Message = Message0
    ).
message_to_string(_, Message, _) :-
    Message = "<exception too large>".

%!  find_frame(+Up, +StartFrame, +Port, -PC, -Frame) is det.
%
%   Find the parent frame Up levels above StartFrame. Must be called
%   in the context of the debugged thread.   We  stop going up if we
%   find a frame that wants to hide   its  children. This happens if
%   nodebug code calls user-code. In that case we prefer to show the
%   user code over showing the internals of the nodebug code.
%
%   @param PC       PC in parent frame
%   @param Frame    Parent frame

find_frame(N, Start, _, PC, Frame) :-
    N > 0,
    debug(gtrace(frame), 'Frame = ~w', [Start]),
    prolog_frame_attribute(Start, pc, PC0),
    prolog_frame_attribute(Start, parent, Frame0),
    \+ hide_children_frame(Frame0),
    !,
    debug(gtrace(frame), 'parent = ~w', [Frame0]),
    NN is N - 1,
    find_frame2(NN, Frame0, PC0, Frame, PC).
find_frame(_, Frame, Port, Port, Frame).

find_frame2(0, F, PC, F, PC).
find_frame2(N, F0, _, F, PC) :-
    prolog_frame_attribute(F0, parent, F1),
    prolog_frame_attribute(F0, pc, PC1),
    NN is N - 1,
    find_frame2(NN, F1, PC1, F, PC).


                 /*******************************
                 *         SHOW LOCATION        *
                 *******************************/

%!  attribute(+Attributes, ?Att) is semidet.
%!  attribute(+Attributes, ?Att, +Default) is semidet.
%
%   Attribute parsing
%
%   @bug    Merge with option library.

attribute(Attributes, Att) :-
    memberchk(Att, Attributes),
    !.

attribute(Attributes, Att, _) :-
    memberchk(Att, Attributes),
    !.
attribute(_, Att, Def) :-
    arg(1, Att, Def).

%!  tracer_gui(+Attributes, -GUI) is det.
%
%   Find the tracer GUI object.

tracer_gui(Attributes, GUI) :-
    attribute(Attributes, gui(GUI)),
    !,
    debug(gtrace(gui), 'GUI = ~p (given)', [GUI]).
tracer_gui(_, GUI) :-
    thread_self_id(Thread),
    prolog_tracer(Thread, GUI),
    debug(gtrace(gui), 'GUI = ~p (from thread ~p)', [GUI, Thread]).

%!  prolog_show_frame(+Frame, +Attributes) is semidet.
%
%   Show given Prolog Frame in GUI-tracer, updating information as
%   provided by Attributes.  Defined attributes:
%
%           * pc(PC)
%           Location.  This is one of an integer (Program Counter),
%           a port-name or choice(CHP).
%           * choice(CHP)
%           * port(Port)
%           * style(Style)
%           Style to use for editor fragment indicating location
%           * source
%           Update source window
%           * bindings
%           Update variable bindings window
%           * stack
%           Update stack window
%           * gui(Object)
%           Gui to address

prolog_show_frame(Frame, Attributes) :-
    debug(gtrace(frame), 'prolog_show_frame(~p, ~p)', [Frame, Attributes]),
    show_stack(Frame, Attributes),
    show_bindings(Frame, Attributes),
    (   show_source(Frame, Attributes)
    ->  true
    ;   debug(gtrace(source),
              'show_source(~p,~p) failed', [Frame, Attributes]),
        fail
    ),
    (   setting(auto_raise, true)
    ->  tracer_gui(Attributes, GUI),
        send_tracer(GUI, expose)
    ;   true
    ).


%!  show_source(+Frame, +Attributes) is semidet.
%
%   Update the current location in the source window. If called from
%   the GUI, the attribute gui(GUI) must be   given to relate to the
%   proper thread.

show_source(Frame, Attributes) :-
    attribute(Attributes, source),
    !,
    tracer_gui(Attributes, GUI),
    debug(gtrace(source), 'source for #~w: ', [Frame]),
    (   attribute(Attributes, pc(PC)),
        attribute(Attributes, port(Port), call),
        attribute(Attributes, style(Style), Port),
        debug(gtrace(source),
              'Show source, PC = ~w, Port = ~w', [PC, Port]),
        (   clause_position(PC),
            prolog_frame_attribute(GUI, Frame, clause, ClauseRef),
            debug(gtrace(source),
                  'ClauseRef = ~w, PC = ~w', [ClauseRef, PC]),
            ClauseRef \== 0
        ->  subgoal_position(GUI, ClauseRef, PC, File, CharA, CharZ),
            debug(gtrace(source),
                  '~p.', [show_range(File, CharA, CharZ, Style)]),
            send_tracer(GUI, show_range(File, CharA, CharZ, Style)),
            (   clause_property(ClauseRef, erased)
            ->  send_tracer(GUI,
                            report(warning,
                                   'Running erased clause; \c
                                       source location may be incorrect'))
            ;   true
            )
        ;   prolog_frame_attribute(GUI, Frame, goal, Goal),
            qualify(Goal, QGoal),
            \+ predicate_property(QGoal, foreign),
            (   clause(QGoal, _Body, ClauseRef)
            ->  subgoal_position(GUI, ClauseRef, unify, File, CharA, CharZ),
                send_tracer(GUI, show_range(File, CharA, CharZ, Style))
            ;   functor(Goal, Functor, Arity),
                functor(GoalTemplate, Functor, Arity),
                qualify(GoalTemplate, QGoalTemplate),
                clause(QGoalTemplate, _TBody, ClauseRef)
            ->  subgoal_position(GUI, ClauseRef, unify, File, CharA, CharZ),
                send_tracer(GUI, show_range(File, CharA, CharZ, Style))
            ;   find_source(QGoal, File, Line),
                debug(gtrace(source), 'At ~w:~d', [File, Line]),
                send_tracer(GUI, show_line(File, Line, Style))
            )
        )
    ->  true
    ;   fail
    ).
show_source(_, _).

qualify(Goal, Goal) :-
    functor(Goal, :, 2),
    !.
qualify(Goal, user:Goal).

%!  clause_position(+PC) is semidet.
%
%   True if the position can be related to a clause.

clause_position(PC) :- integer(PC), !.
clause_position(exit).
clause_position(unify).
clause_position(choice(_)).

%!  subgoal_position(+GUI, +Clause, +PortOrPC,
%!                   -File, -CharA, -CharZ) is semidet.
%
%   Character  range  CharA..CharZ  in  File   is  the  location  to
%   highlight for the given clause at the given location.

subgoal_position(_, ClauseRef, unify, File, CharA, CharZ) :-
    !,
    pce_clause_info(ClauseRef, File, TPos, _),
    head_pos(ClauseRef, TPos, PosTerm),
    nonvar(PosTerm),
    arg(1, PosTerm, CharA),
    arg(2, PosTerm, CharZ).
subgoal_position(GUI, ClauseRef, choice(CHP), File, CharA, CharZ) :-
    !,
    (   prolog_choice_attribute(GUI, CHP, type, jump),
        prolog_choice_attribute(GUI, CHP, pc, To)
    ->  debug(gtrace(position), 'Term-position: choice-jump to ~w', [To]),
        subgoal_position(GUI, ClauseRef, To, File, CharA, CharZ)
    ;   clause_end(ClauseRef, File, CharA, CharZ)
    ).
subgoal_position(_, ClauseRef, Port, File, CharA, CharZ) :-
    end_port(Port),
    !,
    clause_end(ClauseRef, File, CharA, CharZ).
subgoal_position(_, ClauseRef, PC, File, CharA, CharZ) :-
    pce_clause_info(ClauseRef, File, TPos, _),
    (   '$clause_term_position'(ClauseRef, PC, List)
    ->  debug(gtrace(position), 'Term-position: for ref=~w at PC=~w: ~w',
              [ClauseRef, PC, List]),
        (   find_subgoal(List, TPos, PosTerm)
        ->  true
        ;   PosTerm = TPos,
            send_tracer(report(warning,
                               'Clause source-info could not be parsed')),
            fail
        ),
        nonvar(PosTerm),
        arg(1, PosTerm, CharA),
        arg(2, PosTerm, CharZ)
    ;   send_tracer(report(warning,
                           'No clause-term-position for ref=%s at PC=%s',
                           ClauseRef, PC)),
        fail
    ).

end_port(exit).
end_port(fail).
end_port(exception).

clause_end(ClauseRef, File, CharA, CharZ) :-
    pce_clause_info(ClauseRef, File, TPos, _),
    nonvar(TPos),
    arg(2, TPos, CharA),
    CharZ is CharA + 1.

head_pos(Ref, Pos, HPos) :-
    clause_property(Ref, fact),
    !,
    HPos = Pos.
head_pos(_, term_position(_, _, _, _, [HPos,_]), HPos).

%       warning, ((a,b),c)) --> compiled to (a, (b, c))!!!  We try to correct
%       that in clause.pl.  This is work in progress.

find_subgoal(_, Pos, Pos) :-
    var(Pos),
    !.
find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
    nth1(A, PosL, Pos),
    !,
    find_subgoal(T, Pos, SPos).
find_subgoal([1|T], brace_term_position(_,_,Pos), SPos) :-
    !,
    find_subgoal(T, Pos, SPos).
find_subgoal(List, parentheses_term_position(_,_,Pos), SPos) :-
    !,
    find_subgoal(List, Pos, SPos).
find_subgoal(_, Pos, Pos).


                 /*******************************
                 *             ACTION           *
                 *******************************/

%!  action(-Action) is det.
%
%   Wait for the user to perform some   action. We are called in the
%   context of the debugged thread. If we are in the main thread, we
%   use classical XPCE <-confirm. Otherwise  we   hang  waiting on a
%   message queue. While waiting, we must  be prepared to call goals
%   on behalf of in_debug_thread/2 started by   the  debugger gui to
%   get additional information  on  the   state  of  our (debugging)
%   thread.
%
%   @tbd    Synchronise with send_pce/1 and in_debug_thread/2.

action(Action) :- with_access_user(action_(Action)).

action_(Action) :-
    pce_thread(Pce),
    thread_self_id(Pce),
    !,
    get_tracer(action, Action0),
    debug(gtrace(action), 'Got action ~w', [Action0]),
    action(Action0, Action).
action_(Action) :-
    send_tracer(prepare_action),
    repeat,
    debug(gtrace(action), ' ---> action: wait', []),
    (   thread_self_id(Me),
        thread_debug_queue(Me, Queue),
        repeat,
        catch(thread_get_message(Queue, '$trace'(Result, Id)),
              E, wait_error(E))
    ->  true
    ;   debug('thread_get_message() failed; retrying ...'),
        fail
    ),
    debug(gtrace(action), ' ---> action: result = ~p', [Result]),
    (   Result = call(Goal, GVars, Caller)
    ->  run_in_debug_thread(Goal, GVars, Caller, Id),
        fail
    ;   Result = action(Action)
    ->  !
    ;   assertion(fail)
    ).

%!  wait_error(+ErrorTerm)
%
%   thread_get_message/1 can only fail due   to  signals throwing an
%   exception. For example,  if  the  traced   goal  is  guarded  by
%   call_with_time_limit/2. Here we  print  the   message  and  keep
%   waiting. Note that this causes the  signal   to  be lost for the
%   application.
%
%   @tbd    Allow passing the error to the application
%   @tbd    Deal with similar signals in other part of the tracing
%           code.

wait_error(E) :-
    message_to_string(E, Message),
    format(user_error, 'Error while waiting for for user: ~w~n\c
                           Retrying~n', [Message]),
    fail.


run_in_debug_thread(Goal, GVars, Caller, Id) :-
    (   catch(Goal, Error, true)
    ->  (   var(Error)
        ->  Result = true(GVars)
        ;   Result = error(Error)
        )
    ;   Result = false
    ),
    debug(gtrace(thread), ' ---> run_in_debug_thread: send ~p', [Result]),
    thread_debug_queue(Caller, Queue),
    thread_send_message(Queue, '$trace'(Result, Id)).


action(break, Action) :-
    !,
    break,
    format(user_error, 'Continuing the debug session~n', []),
    action(Action).
action(Action, Action).


                 /*******************************
                 *            STACK             *
                 *******************************/

%!  show_stack(+Frame, +Attributes) is det.
%
%   Show call- and choicepoint stack. Run in the context of the GUI.

show_stack(Frame, Attributes) :-
    attribute(Attributes, stack),
    !,
    tracer_gui(Attributes, GUI),
    debug(gtrace(stack), 'stack ...', []),
    in_debug_thread(GUI,
                    notrace(stack_info(Frame,
                               CallFrames, ChoiceFrames,
                               Attributes))),
    send_tracer(GUI, show_stack(CallFrames, ChoiceFrames)).
show_stack(_, _).

%!  stack_info(+Frame, -CallFrames, -ChoiceFrames, +Attributes) is det.
%
%   Find the callstack and choicepoints that must be made visible in
%   the stack window. Must  run  in   the  context  of  the debugged
%   thread.

stack_info(Frame, CallFrames, ChoiceFrames, Attributes) :-
    attribute(Attributes, port(Port), call),
    attribute(Attributes, pc(PC), Port),
    attribute(Attributes, choice(CHP), Frame),
    setting(stack_depth, Depth),
    setting(choice_depth, MaxChoice),
    stack_frames(Depth, Frame, PC, CallFrames),
    debug(gtrace(stack), 'Stack frames: ~w', [CallFrames]),
    level_range(CallFrames, Range),
    debug(gtrace(stack), 'Levels ~w, CHP = ~w', [Range, CHP]),
    choice_frames(MaxChoice, CHP, Range, [], ChoiceFrames),
    debug(gtrace(stack), 'Choicepoints: ~p', [ChoiceFrames]).


stack_frames(0, _, _, []) :- !.
stack_frames(Depth, F, PC, Frames) :-
    (   prolog_frame_attribute(F, hidden, true)
    ->  RestFrames = Frames,
        ND is Depth
    ;   Frames = [frame(F, PC)|RestFrames],
        ND is Depth - 1
    ),
    (   prolog_frame_attribute(F, parent, Parent),
        (   prolog_frame_attribute(F, pc, PCParent)
        ->  true
        ;   PCParent = foreign
        )
    ->  stack_frames(ND, Parent, PCParent, RestFrames)
    ;   RestFrames = []
    ).

%!  choice_frames(+Max, +CHP, +MinMaxLevel, -Frames) is det.
%
%   Frames is a list of frames that hold choice-points.
%
%   @param Max is the maximum number of choicepoints returned
%   @param CHP is the initial choicepoint
%   @param MinMaxLevel is a pair holding the depth-range we
%   consider. Currently, MaxLevel is ignored (see in_range/2).

choice_frames(_, none, _, _, []) :- !.
choice_frames(Max, CHP, Range, Seen, [frame(Frame, choice(CH))|Frames]) :-
    Max > 0,
    earlier_choice(CHP, CH),
    visible_choice(CH, Frame),
    \+ memberchk(Frame, Seen),
    prolog_frame_attribute(Frame, level, Flev),
    in_range(Flev, Range),
    !,
    NMax is Max - 1,
    (   prolog_choice_attribute(CH, parent, Prev)
    ->  choice_frames(NMax, Prev, Range, [Frame|Seen], Frames)
    ;   Frames = []
    ).
choice_frames(_, _, _, _, []).

%!  earlier_choice(+Here, -Visible) is nondet.
%
%   Visible is an older choicepoint  than   Here.  Older choices are
%   returned on backtracking.

earlier_choice(CHP, CHP).
earlier_choice(CHP, Next) :-
    prolog_choice_attribute(CHP, parent, Parent),
    earlier_choice(Parent, Next).

%!  ancestor_frame(+Frame, ?Ancestor) is nondet.
%
%   True when Ancestor is an ancestor of frame. Starts with Frame.

ancestor_frame(Frame, Frame).
ancestor_frame(Frame, Ancestor) :-
    prolog_frame_attribute(Frame, parent, Parent),
    ancestor_frame(Parent, Ancestor).

%!  visible_choice(+CHP, -Frame) is semidet.
%
%   A visible choice is a choice-point that realises a real choice
%   and is created by a visible frame.

visible_choice(CHP, Frame) :-
    prolog_choice_attribute(CHP, type, Type),
    real_choice_type(Type),
    prolog_choice_attribute(CHP, frame, Frame0),
    ancestor_frame(Frame0, Frame),
    prolog_frame_attribute(Frame, hidden, false),
    !,
    debug(gtrace(stack), 'Choice ~w of type ~w running frame ~w',
          [CHP, Type, Frame]).

real_choice_type(clause).
real_choice_type(foreign).
real_choice_type(jump).


level_range(Frames, H-L) :-
    Frames = [F0|_],
    last(Frames, FT),
    flevel(F0, L),
    flevel(FT, H).

flevel(frame(Frame, _), L) :-
    prolog_frame_attribute(Frame, level, L),
    debug(gtrace(stack), 'Frame ~d at level ~d', [Frame, L]).

in_range(Level, Low-_High) :-
    Level >= Low.
%       between(Low, High, Level).

%!  show_stack_location(+GUI, +Frame, +PC)
%
%   Highlight Frame in the stack-view.

show_stack_location(GUI, Frame, PC) :-
    get_tracer(GUI, member(stack), StackBrowser),
    send(StackBrowser, selection, Frame, PC).


                 /*******************************
                 *             BINDINGS         *
                 *******************************/

%!  show_bindings(+Frame, +Attributes) is det.
%
%   Show argument bindings.

show_bindings(Frame, Attributes) :-
    attribute(Attributes, bindings),
    !,
    tracer_gui(Attributes, GUI),
    debug(gtrace(bindings), 'bindings ... ', []),
    get_tracer(GUI, member(bindings), Browser),
    (   attribute(Attributes, pc(PC))
    ->  true
    ;   PC = @default
    ),
    debug(gtrace(bindings), '(Frame=~p, PC=~p) ', [Frame, PC]),
    show_stack_location(GUI, Frame, PC),
    send(Browser, clear),
    send(Browser, prolog_frame, Frame),
    (   \+ show_args_pc(PC),
        prolog_frame_attribute(GUI, Frame, clause, ClauseRef)
    ->  send(Browser, label, 'Bindings'),
        debug(gtrace(bindings), '(clause ~w) ', [ClauseRef]),
        catch(pce_clause_info(ClauseRef, _, _, VarNames), E,
              (print_message(error, E), fail)),
        in_debug_thread(GUI, frame_bindings(Frame, VarNames, Bindings)),
        debug(gtrace(bindings), '(bindings ~p) ', [Bindings]),
        send(Browser, bindings, Bindings),
        debug(gtrace(bindings), '(ok) ', [])
    ;   debug(gtrace(bindings), '(arguments) ', []),
        send(Browser, label, 'Arguments'),
        show_arguments(GUI, Frame, Attributes)
    ).
show_bindings(_, _).

%!  show_args_pc(+Port) is semidet.
%
%   If we are at Port, we must simple show the arguments.

show_args_pc(call).
show_args_pc(fail).
show_args_pc(exception).
show_args_pc(foreign).

show_arguments(GUI, Frame, _Attributes) :-
    get_tracer(GUI, member(bindings), Browser),
    in_debug_thread(GUI, frame_arguments(Frame, Args)),
    debug(gtrace(bindings), 'Frame arguments = ~w', [Args]),
    send(Browser, bindings, Args).

%!  frame_arguments(+Frame, -Args)
%
%   Return arguments of the frame  as [[I:I]=Value, ...], compatible
%   with the normal binding list. Must   run  in context of debugged
%   thread.

frame_arguments(Frame, Args) :-
    prolog_frame_attribute(Frame, goal, Goal),
    (   Goal = _:Head
    ->  functor(Head, _, Arity)
    ;   functor(Goal, _, Arity)
    ),
    frame_arguments(1, Arity, Frame, Args).

frame_arguments(I, Arity, Frame, [[I:I]=Value|T]) :-
    I =< Arity,
    !,
    prolog_frame_attribute(Frame, argument(I), Value),
    NI is I + 1,
    frame_arguments(NI, Arity, Frame, T).
frame_arguments(_, _, _, []).


%!  frame_bindings(+Frame, +VarNames, -Bindings) is det.
%
%   Get the variable bindings for Frame. Must run the the context of
%   the debugged thread.

frame_bindings(Frame, VarNames, Bindings) :-
    functor(VarNames, _, Arity),
    frame_bindings(0, Arity, Frame, VarNames, B0),
    (   setting(cluster_variables, true)
    ->  cluster_bindings(B0, Bindings)
    ;   Bindings = B0
    ).

frame_bindings(Arity, Arity, _, _, []) :- !.
frame_bindings(N, Arity, Frame, VarNames, [(Name:I)=Value|T]) :-
    I is N + 1,
    arg(I, VarNames, Name),
    Name \== '_',
    !,
    prolog_frame_attribute(Frame, argument(I), Value),
    frame_bindings(I, Arity, Frame, VarNames, T).
frame_bindings(N, Arity, Frame, VarNames, T) :-
    I is N + 1,
    frame_bindings(I, Arity, Frame, VarNames, T).

cluster_bindings([], []).
cluster_bindings([Name=Value|BR], [[Name|Names]=Value|CR]) :-
    clustered_binding(BR, BT, Value, Names),
    cluster_bindings(BT, CR).

clustered_binding([], [], _, []).
clustered_binding([Name=Val|BR], BT, Value, [Name|NT]) :-
    Val == Value,
    !,
    clustered_binding(BR, BT, Value, NT).
clustered_binding([B|BR], [B|BT], Value, C) :-
    clustered_binding(BR, BT, Value, C).


:- create_prolog_flag(gui_tracer, true, []).
