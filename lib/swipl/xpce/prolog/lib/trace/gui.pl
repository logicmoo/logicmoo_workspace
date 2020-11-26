/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog/projects/xpce/
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

:- module(prolog_gui,
          [ prolog_tracer/2,            % +Thread, -GUI
            send_tracer/2,              % +Thread, :Goal
            send_if_tracer/2,           % +Thread, :Goal
            get_tracer/3,               % +Thread, :Goal, -Result
            send_tracer/1,              % :Goal
            send_if_tracer/1,           % :Goal
            get_tracer/2,               % :Goal, -Result
            in_debug_thread/2,          % +ObjOrThread, :Goal
            thread_debug_queue/2,       % +Thread, -Queue
            prolog_frame_attribute/4,   % +GUI, +Frame, +Attr, -Value
            prolog_choice_attribute/4   % +GUI, +Choice, +Attr, -Value
          ]).
:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(library(pce_util)).
:- use_module(library(persistent_frame)).
:- use_module(library(debug)).
:- use_module(library(threadutil)).
:- use_module(library(pprint)).
:- use_module(trace).
:- use_module(clause).
:- use_module(util).
:- use_module(source).
:- consult([ settings,
             stack,
             viewterm
           ]).
:- require([ make/0,
	     acyclic_term/1,
	     current_predicate/1,
	     message_queue_create/1,
	     message_queue_destroy/1,
	     notrace/1,
	     pce_image_directory/1,
	     portray_text/1,
	     prolog_ide/1,
	     thread_self/1,
	     atomic_list_concat/2,
	     file_directory_name/2,
	     pce_help_file/2,
	     predicate_name/2,
	     prolog_listen/2,
	     prolog_unlisten/2,
	     string_codes/2,
	     term_attvars/2,
	     thread_get_message/2,
	     thread_property/2,
	     thread_send_message/2,
	     thread_signal/2,
	     unify_with_occurs_check/2,
	     with_mutex/2,
	     '$factorize_term'/3,
	     '$get_predicate_attribute'/3,
	     absolute_file_name/3,
	     atomic_list_concat/3,
	     file_name_extension/3,
	     maplist/3,
	     numbervars/4,
             start_emacs/0
	   ]).

:- multifile
    user:message_hook/3.

:- meta_predicate
    in_debug_thread(+, 0),
    send_pce(0),
    send_pce_async(0).

register_directories :-
    (   member(SpyBase, ['icons/nospy', library('trace/icons/nospy')]),
        absolute_file_name(SpyBase,
                           [ extensions([xpm]), access(read)],
                           SpyIcon)
    ->  file_directory_name(SpyIcon, Icons),
        pce_image_directory(Icons)
    ),
    (   member(HlpBase, ['pltracer', library('trace/pltracer')]),
        absolute_file_name(HlpBase,
                           [ extensions([hlp]), access(read)],
                           HlpFile)
    ->  pce_help_file(pltracer, HlpFile)
    ).

:- register_directories.

version('2.0').

                 /*******************************
                 *            RESOURCES         *
                 *******************************/

resource(debug, image,  image('debug.xpm')).
resource(Name,  image,  image(XPM)) :-
    button(_, _, XPM, _),
    file_name_extension(Name, xpm, XPM).

                 /*******************************
                 *           TOPLEVEL           *
                 *******************************/

:- dynamic
    gui/3.                          % +Thread, +BreakLevel, -Gui

%!  prolog_tracer(+Thread, -Ref) is det.
%!  prolog_tracer(+Thread, -Ref, +Create) is semidet.
%
%   Get the Prolog debugger window for Thread.

prolog_tracer(Thread, Ref) :-
    prolog_tracer(Thread, Ref, true).

% (*) (Windows) we must start Emacs first because Emacs tries to setup a
% DDE   service   on   Windows,   but    send_pce/1   eventually   calls
% thread_get_message/2, which causes the  main  thread   to  block  on a
% condition variable using SleepConditionVariableCS(),   which  does not
% process Windows messages and sleeps for  15 seconds before continuing.
% In contrast, start_emacs/0 uses  in_pce_thread_sync/1   which  uses  a
% normal message loop to wait for xpce  to   do  its  job. Note that the
% debugger cannot use in_pce_thread_sync/1 because  it requires a method
% to wait that allows for callbacks.

prolog_tracer(Thread, Ref, Create) :-
    break_level(Level),
    (   gui(Thread, Level, Ref)
    ->  true
    ;   Create == true
    ->  debug(gtrace(gui),
              'New GUI for thread ~p, break level ~p', [Thread, Level]),
        start_emacs,		% see (*)
        send_pce(send(new(Ref, prolog_debugger(Level, Thread)), open))
    ).

%!  break_level(-Level) is det.
%
%   Current break-level. Level is left unbound   if the caller is an
%   engine. In that case we  use  the   break  level  of  the client
%   thread.

break_level(Level) :-
    current_prolog_flag(break_level, Level),
    !.
break_level(_Level) :-
    thread_self(Me),
    thread_property(Me, engine(true)),
    !.
break_level(-1).                                % non-interactive thread.


%!  send_tracer(+ThreadOrGUI, +Term) is semidet.
%!  send_if_tracer(+Thread, +Term) is semidet.
%!  get_tracer(+Thread, +Term, -Reply) is semidet.
%
%   Send messages to the XPCE tracer window.
%
%   @param Thread: calling thread.

send_tracer(Term) :-
    notrace(send_tracer_(Term)).

send_tracer_(Term) :-
    thread_self_id(Thread),
    send_tracer(Thread, Term).

send_tracer(GUI, Term) :-
    object(GUI),
    !,
    send_pce(send(GUI, Term)).
send_tracer(Thread, Term) :-
    prolog_tracer(Thread, Ref),
    send_pce(send(Ref, Term)).

send_if_tracer(Term) :-
    thread_self_id(Thread),
    send_if_tracer(Thread, Term).
send_if_tracer(Thread, Term) :-
    (   prolog_tracer(Thread, Ref, false)
    ->  send_pce(send(Ref, Term))
    ;   true
    ).

get_tracer(Term, Result) :-
    thread_self_id(Thread),
    get_tracer(Thread, Term, Result).

get_tracer(GUI, Term, Result) :-
    object(GUI),
    !,
    get(GUI, Term, Result).
get_tracer(Thread, Term, Result) :-
    prolog_tracer(Thread, Ref),
    get(Ref, Term, Result).


                 /*******************************
                 *    THREAD SYNCHRONISATION    *
                 *******************************/

%!  thread_debug_queue(+Thread, -Queue) is det.
%
%   Queue is the debugging queue for Thread.  We do not use the main
%   queue to avoid interference with user-messages.

:- dynamic
    thread_debug_queue_store/2.

thread_debug_queue(Thread, Queue) :-
    with_mutex(debug_msg_queue,
               thread_debug_queue_locked(Thread, Queue)).

thread_debug_queue_locked(Thread, Queue) :-
    (   thread_debug_queue_store(Thread, Q)
    ->  Queue = Q
    ;   message_queue_create(Q),
        assert(thread_debug_queue_store(Thread, Q)),
        Queue = Q
    ).

:- initialization
    (   current_prolog_flag(threads, true)
    ->  prolog_unlisten(thread_exit, thread_finished),
        prolog_listen(thread_exit, thread_finished)
    ;   true
    ).

thread_finished(TID) :-
    destroy_thread_debug_gui(TID),
    forall(retract(thread_debug_queue_store(TID, Queue)),
           message_queue_destroy(Queue)).

msg_id(Id) :-
    with_mutex(debug_msg_id,
               msg_id_locked(Id)).

:- dynamic
    trace_msg_id/1.

msg_id_locked(Id) :-
    (   retract(trace_msg_id(Id0))
    ->  NId is Id0+1
    ;   NId = 1
    ),
    assert(trace_msg_id(NId)),
    Id = NId.


%!  send_pce(:Goal)
%
%   Run Goal in XPCE thread. Wait  for completion. In the meanwhile,
%   allow the XPCE thread to call in_debug_thread/1.

send_pce(Goal) :-
    thread_self_id(Me),
    pce_thread(Me),
    !,
    Goal.
send_pce(Goal) :-
    thread_self_id(Self),
    term_variables(Goal, GVars),
    msg_id(Id),
    in_pce_thread(run_pce(Goal, GVars, Self, Id)),
    thread_debug_queue(Self, Queue),
    repeat,
    thread_get_message(Queue, '$trace'(Result, Id2)),
    debug(gtrace(thread),
          ' ---> ~w: send_pce: result = ~p', [Id2, Result]),
    (   Result = call(CallBack, CBVars, Caller)
    ->  run_pce(CallBack, CBVars, Caller, Id2),
        fail
    ;   assertion(Id == Id2),
        (   Result = true(BGVars)
        ->  !, BGVars = GVars
        ;   Result == false
        ->  fail
        ;   Result = error(E)
        ->  throw(E)
        ;   assertion(false)
        )
    ).

run_pce(Goal, Vars, Caller, Id) :-
    debug(gtrace(thread), '~w: running ~p for thread ~p',
          [Id, Goal, Caller]),
    (   catch(Goal, Error, true)
    ->  (   var(Error)
        ->  Result = true(Vars)
        ;   Result = error(Error)
        )
    ;   Result = false
    ),
    debug(gtrace(thread), '~w: ok, returning ~p', [Id, Result]),
    thread_debug_queue(Caller, Queue),
    thread_send_message(Queue, '$trace'(Result, Id)).

%!  in_debug_thread(+Thread, :Goal) is semidet.
%!  in_debug_thread(+Object, :Goal) is semidet.
%
%   Run Goal in the thread  being   debugged.  The first argument is
%   either an XPCE object that is part  of the debugger window, or a
%   thread identifier.

in_debug_thread(Object, Goal) :-
    object(Object),
    !,
    get(Object, frame, Frame),
    get(Frame, thread, Thread),
    in_debug_thread(Thread, Goal).
in_debug_thread(Thread, Goal) :-
    thread_self_id(Thread),
    !,
    Goal,
    !.
in_debug_thread(Thread, Goal) :-
    thread_self_id(Self),
    msg_id(Id),
    debug(gtrace(thread), 'Call [Thread ~p] ~p', [Thread, Goal]),
    term_variables(Goal, GVars),
    thread_debug_queue(Thread, Queue),
    thread_send_message(Queue, '$trace'(call(Goal, GVars, Self), Id)),
    thread_debug_queue(Self, MyQueue),
    thread_get_message(MyQueue, '$trace'(Result, Id)),
    debug(gtrace(thread),
          ' ---> in_debug_thread: result = ~p', [Result]),
    (   Result = error(E)
    ->  throw(E)
    ;   Result = true(BGVars)
    ->  GVars = BGVars
    ).


%!  send_pce_async(:Goal) is det.
%
%   Send to the debug thread asynchronously.

send_pce_async(Goal) :-
    thread_self_id(Me),
    pce_thread(Me),
    !,
    Goal.
send_pce_async(Goal) :-
    in_pce_thread(Goal).


%!  prolog_frame_attribute(+GUI, +Frame, +Attribute, -Value) is det.
%!  prolog_frame_attribute(+Thread, +Frame, +Attribute, -Value) is det.
%
%   As prolog_frame_attribute/3, but calling in the thread debugged
%   by GUI.

prolog_frame_attribute(Thread, Frame, Attribute, Value) :-
    in_debug_thread(Thread,
                    prolog_frame_attribute(Frame, Attribute, Value)).

prolog_choice_attribute(GUI, Choice, Attribute, Value) :-
    in_debug_thread(GUI,
                    prolog_choice_attribute(Choice, Attribute, Value)).



                 /*******************************
                 *     DEBUGGER APPLICATION     *
                 *******************************/

:- pce_global(@prolog_gui, new(prolog_gui)).

:- pce_begin_class(prolog_gui, application,
                   "Toplevel driver for the Prolog GUI").

initialise(App) :->
    send_super(App, initialise, 'Prolog Debugger GUI'),
    send(App, kind, service).       % Do not debug in this

:- pce_end_class.


                 /*******************************
                 *         DEBUGGER FRAME       *
                 *******************************/

:- pce_begin_class(prolog_debugger, persistent_frame,
                   "Toplevel driver for the debugger").

variable(source,        any,            both, "Source view").
variable(break_level,   int,            get,  "Break-level I'm associated to").
variable(thread,        'int|name*',    get,  "Associated thread").
variable(trap_frame,    int*,           get,  "Last trapped frame").
variable(trap_port,     name*,          get,  "Last trapped port").
variable(current_frame, int*,           both, "The most recent frame").
variable(quitted,       bool := @off,   both, "Asked to quit").
variable(mode,          name := created,get,  "Current mode").

running_in_pce_thread :-
    pce_thread(Pce), thread_self_id(Pce).

initialise(F, Level:int, Thread:'int|name') :->
    assertion(running_in_pce_thread),
    send(F, slot, break_level, Level),
    send(F, slot, thread, Thread),
    send_super(F, initialise, 'SWI-Prolog debugger',
               application := @prolog_gui),
    send(F, icon, resource(debug)),
    send(F, done_message, message(F, quit)),
    send(F, append, new(MBD, dialog)),
    send(MBD, gap, size(0, 2)),
    send(MBD, pen, 0),
    send(MBD, append, new(menu_bar)),
    send(MBD, name, menu_bar_dialog),
    send(MBD, resize_message, message(MBD, layout, @arg2)),
    send(F, fill_menu_bar),
    send(new(D, prolog_button_dialog), below, MBD),
    send(D, name, buttons),

    new(V, prolog_bindings_view),
    send(V, label, 'Bindings'),
    send(V, name, bindings),
    send(new(S, prolog_stack_view), right, V),
    send(V, below, D),
    send(new(Src, prolog_source_view), below, V),
    send(F, source, Src),
    send(new(RD, report_dialog), below, Src),
    send(RD, warning_delay, 0),
    send(S, label, 'Call Stack'),
    send(S, name, stack),
    ignore(send(F, frame_finished, 0)),     % FR_WATCHED issue
    asserta(gui(Thread, Level, F)).

unlink(F) :->
    retractall(gui(_, _, F)),
    clear_clause_info_cache,        % safety first
    send_super(F, unlink).

quit(F) :->
    "User initiated quit"::
    (   (   get(F, mode, thread_finished)
        ;   get(F, mode, query_finished)
        ;   get(F, mode, aborted)
        ;   get(F, mode, replied)
        )
    ->  send(F, destroy)
    ;   get(F, tracer_quitted, Action),
        (   Action == cancel
        ->  true
        ;   send(F, return, Action)
        )
    ).

label(F, Label:char_array) :->
    "Set label, indicating associated thread"::
    get(F, thread, Thread),
    (   Thread == main
    ->  send_super(F, label, Label)
    ;   send_super(F, label, string('[Thread %s] %s', Thread, Label))
    ).

clear_stack_window(F) :->
    "Clear the stack window"::
    get(F, member, stack, StackView),
    send(StackView, clear).

clear(F, Content:[bool]) :->
    "Deactivate all views"::
    ignore(send(F, send_hyper, fragment, free)),
    get(F, member, stack, StackView),
    send(StackView, clear),
    get(F, member, bindings, BindingView),
    send(BindingView, clear, Content).


fill_menu_bar(F) :->
    get(F, member, menu_bar_dialog, MBD),
    get(MBD, member, menu_bar, MB),
    send(MB, append, new(Tool, popup(tool))),
    send(MB, append, new(Edit, popup(edit))),
    send(MB, append, new(View, popup(view))),
    send(MB, append, new(Comp, popup(compile))),
    send(MB, append, new(Help, popup(help)), right),
    send_list(Tool, append,
              [ menu_item(settings,
                          message(F, settings),
                          end_group := @on),
                menu_item(clear_source_cache,
                          message(@prolog, clear_clause_info_cache),
                          end_group := @on),
                menu_item(quit,
                          message(F, quit))
              ]),
    send_list(Edit, append,
              [ menu_item(breakpoints,
                          message(F, breakpoints)),
                menu_item(exceptions,
                          message(F, exceptions),
                          end_group := @on),
                menu_item(toggle_edit_mode,
                          message(F, edit),
                          end_group := @on),
                menu_item(copy_goal,
                          message(F, copy_goal))
              ]),
    send_list(View, append,
              [ menu_item(threads,
                          message(F, show_threads)),
                new(PT, menu_item(portray_code_lists,
                                  message(F, portray_text)))
              ]),
    send_list(Comp, append,
              [ menu_item(make,
                          message(F, make),
                          end_group := @on)
              ]),
    send_list(Help, append,
              [ menu_item(about,
                          message(F, about)),
                menu_item(help_on_debugger,
                          message(F, help),
                          end_group := @on),
                menu_item(prolog_manual,
                          message(@prolog, prolog_help)),
                menu_item('XPCE manual',
                          message(@prolog, manpce))
              ]),
    send(View, show_current, @on),
    send(View, multiple_selection, @on),
    send(PT, condition, message(F, update_portray_text, PT)).

settings(_F) :->
    "Edit the preferences"::
    trace_settings.


about(_) :->
    "Display aout message"::
    version(Version),
    send(@display, inform,
         'SWI-Prolog debugger version %s\n\c
              By Jan Wielemaker',
         Version).

help(_) :->
    "Show window with help-text"::
    send(@helper, give_help, pltracer, main).

show_frame(GUI, Frame:int, PC:prolog) :->
    "Show the variables of this frame"::
    (   get(GUI, trap_frame, Frame)         % the initial trapped port
    ->  get(GUI, trap_port, Style)
    ;   PC = choice(_)                      % A choice-point
    ->  Style = choice
    ;   Style = frame                       % Somewhere up the stack
    ),
    prolog_show_frame(Frame, [ gui(GUI), pc(PC),
                               source, bindings,
                               style(Style)
                             ]).

show_stack(GUI, CallFrames:prolog, ChoiceFrames:prolog) :->
    "Show the stack and choicepoints"::
    get(GUI, member, stack, StackWindow),
    send(StackWindow, clear),
    display_stack(StackWindow, CallFrames, ChoiceFrames).

show_threads(_GUI) :->
    "Open Thread monitor"::
    prolog_ide(thread_monitor).

portray_text(GUI) :->
    "Toggle portray of text"::
    portraying_text(Old),
    negate(Old, New),
    portray_text(New),
    send(GUI, refresh_bindings),
    send(GUI, report, status, 'Portray code-list as text: %s', New).

negate(true, false).
negate(false, true).

update_portray_text(_GUI, MI:menu_item) :->
    "Update selected of portray text item"::
    portraying_text(Bool),
    send(MI, selected, Bool).

portraying_text(Bool) :-
    current_predicate(portray_text:do_portray_text/1),
    !,
    portray_text:do_portray_text(Bool).
portraying_text(false).

trapped_location(GUI, StartFrame:int, Frame:int, Port:name) :->
    "The last trapped location"::
    send(GUI, slot, trap_frame, Frame),
    send(GUI, slot, trap_port, Port),
    send(GUI, current_frame, StartFrame).

refresh_bindings(GUI) :->
    "Refresch the binding view after changing parameters"::
    (   get(GUI, member, bindings, Bindings),
        get(Bindings, prolog_frame, Frame),
        Frame \== @nil
    ->  prolog_show_frame(Frame, [ gui(GUI),
                                   bindings
                                 ])
    ;   true
    ).


                 /*******************************
                 *            EVENT             *
                 *******************************/

source_typed(Frame, Typed:event_id) :->
    "Forward a typing event to the button-dialog"::
    get(Frame, member, buttons, Dialog),
    send(Dialog, typed, Typed).


                 /*******************************
                 *           ACTIONS            *
                 *******************************/

window_pos_for_button(F, ButtonName:name, Pos:point) :<-
    "Return position for transient window reacting on Button"::
    get(F, member, buttons, Dialog),
    get(Dialog, button, ButtonName, Button),
    get(Button, display_position, ButtonPos),
    get(ButtonPos, plus, point(0, 25), Pos).

prepare_action(Frame) :->
    "Prepare for reading an action"::
    send(Frame, open),              % make sure
    get(Frame, display, Display),
    send(Display, busy_cursor, @nil),
    send(Display, synchronise),
    send(Frame, mode, wait_user).

action(Frame, Action:name) :<-
    "Wait for the user to return an action"::
    send(Frame, prepare_action),
    get(Frame, confirm, Action),
    (   get(Frame, quitted, @on)
    ->  send(Frame, destroy)
    ;   true
    ).

return(Frame, Result:any) :->
    "Return user action"::
    (   get(Frame, mode, wait_user)
    ->  get(Frame, thread, Thread),
        send(Frame, mode, replied),
        (   pce_thread(Thread)
        ->  send_super(Frame, return, Result)
        ;   (   get(Frame, quitted, @on)
            ->  send(Frame, destroy)
            ;   true
            ),
            debug(gtrace(thread),
                  ' ---> frame for thread = ~p: result = ~p',
                  [Thread, Result]),
            thread_debug_queue(Thread, Queue),
            thread_send_message(Queue, '$trace'(action(Result), action))
        )
    ;   get(Frame, quitted, @on)
    ->  send(Frame, destroy)
    ;   send(Frame, report, warning, 'Not waiting')
    ).

%!  tracer_quitted(+Thread, -Action) is semidet.
%
%   Ask the user what to  do  after   a  user-initiated  quit of the
%   debugger.

tracer_quitted(Frame, Action) :<-
    "Confirm user requested quit"::
    get(Frame, thread, Thread),
    (   Thread == main
    ->  Label = 'Tracer quitted'
    ;   Label = string('[Thread %s] Tracer quitted', Thread)
    ),
    new(D, dialog(Label)),
    send(D, application, @prolog_gui),
    send(D, append,
         button(continue_without_debugging,
                message(D, return, nodebug))),
    send(D, append,
         button(abort,
                message(D, return, abort))),
    (   Thread == main
    ->  send(D, append,
             button(exit_prolog,
                    message(D, return, halt)))
    ;   true
    ),
    send(D, append,
         button(cancel,
                message(D, return, cancel))),
    send(D, transient_for, Frame),
    send(D, modal, transient),
    get(D, confirm_centered, Frame?area?center, Action),
    send(D, destroy),
    (   Action == cancel
    ->  true
    ;   send(Frame, quitted, @on)
    ).


selected_frame(F, Frame:int) :<-
    "Prolog frame selected by user in stack window"::
    get(F, member, stack, Browser),
    get(Browser, selection, Frame).


                 /*******************************
                 *  ACTIONS OF THE SOURCE VIEW  *
                 *******************************/

:- pce_group(actions).

edit(F) :->
    "(Toggle) Edit-mode of source-window"::
    send(F?source, edit).

breakpoints(_F) :->
    "Edit spy/break/trace-points"::
    prolog_ide(open_debug_status).

exceptions(_F) :->
    "Edit exceptions"::
    prolog_ide(open_exceptions).

make(_) :->
    "Run Prolog make"::
    (   object(@emacs)
    ->  send(@emacs, save_some_buffers)
    ;   true
    ),
    make.

goal(F, Goal:prolog) :<-
    "Return qualitied term for selected frame"::
    get(F, selected_frame, Frame),
    prolog_frame_attribute(F, Frame, goal, Goal0),
    (   Goal0 = _:_
    ->  Goal = Goal0
    ;   Goal = user:Goal0
    ).

nostop_or_spy(F) :->
    "Clear spy-point"::
    (   send(F?source, delete_selected_stop)
    ->  true
    ;   (   get(F, current_frame, Frame)
        ;   get(F, selected_frame, Frame)
        ),
        Frame \== @nil,
        prolog_frame_attribute(F, Frame, goal, Goal0),
        (   Goal0 = _:_
        ->  Goal = Goal0
        ;   Goal = user:Goal0
        ),
        '$get_predicate_attribute'(Goal, spy, 1)
    ->  nospy(Goal)
    ;   send(F, report, warning,
             'No selected break or current spy-point')
    ).

browse(_F) :->
    "Provides overview for edit/spy/break"::
    prolog_ide(open_navigator).

stop_at(F) :->
    "Set stop at caret"::
    get(F, source, SourceWindow),
    send(SourceWindow, stop_at).

up(F) :->
    "Select child frame"::
    get(F, member, stack, Stack),
    send(Stack, up).

down(F) :->
    "Select parent frame"::
    get(F, member, stack, Stack),
    send(Stack, down).

details(F) :->
    "Show (variable) details"::
    get(F, member, bindings, Bindings),
    send(Bindings, details).

nodebug(F) :->
    "User hit n(odebug)"::
    (   setting(auto_close, true)
    ->  send(F, quitted, @on)
    ;   true
    ),
    send(F, return, nodebug).

abort(F) :->
    "User hit a(bort)"::
    (   setting(auto_close, true)
    ->  send(F, quitted, @on)
    ;   true
    ),
    send(F, return, abort).

interrupt(F) :->
    "User hit t (interrupt, trace)"::
    get(F, thread, Thread),
    thread_signal(Thread, trace).

query(F) :->
    "Enter and run a query"::
    send(F, check_console),
    send(F, report, status,
         'Started toplevel in console.  Type Control-D to resume debugging'),
    send(F, synchronise),
    in_debug_thread(F, prolog),
    send(F, report, status,
         'Toplevel has terminated; resuming debugger').

check_console(F) :->
    "See whether the debugged thread has a console"::
    (   in_debug_thread(F, thread_has_console)
    ->  true
    ;   send(@display, inform,
             'The debugged thread is not attached to a console.\n\c
                  Cannot run an interactive session in the debuggee.'),
        fail
    ).

interactor(F) :->
    "Open a new interactor"::
    send(F, warn_windows_thread),
    prolog_ide(open_interactor).

warn_windows_thread(_F) :->
    "Warn to run in a separate thread"::
    (   current_prolog_flag(windows, true),
        pce_thread(main)
    ->  send(@display, inform,
             'Opening a new interactor from the debugger requires\n\c
                  for the tools to run in a separate thread.  Please set\n\c
                  the flag "xpce_threaded" to "true" in your Prolog startup\n\c
                  file and restart Prolog'),
        fail
    ;   true
    ).


copy_goal(F) :->
    "Copy the current goal into the copy-buffer"::
    get(F, selected_frame, Frame),
    (   Frame \== @nil
    ->  true
    ;   send(F, report, warning, 'No current frame'),
        fail
    ),
    prolog_frame_attribute(F, Frame, goal, Goal),
    prolog_frame_attribute(F, Frame, predicate_indicator, PI),
    (   numbervars(Goal, 0, _, [attvar(skip)]),
        format(string(Text), '~q', [Goal]),
        send(@display, copy, Text),
        fail
    ;   true
    ),
    format(atom(PIA), '~q', [PI]),
    send(F, report, inform, 'Copied goal (%s) to clipboard', PIA).

:- pce_group(delegate).

file(F, File:'name|emacs_buffer*') :->
    "Attach to indicated file"::
    send(F?source, source, File).

show_range(F, File:'name|text_buffer', From:int, To:int, Style:name) :->
    "Show indicated region using Style"::
    send(F?source, show_range, File, From, To, Style).

show_line(F, File:'name|text_buffer', Line:int, Style:name) :->
    "Show numbered line"::
    send(F?source, show_line, File, Line, Style).

listing(F, Module:name, Predicate:name, Arity:int) :->
    "List the specified predicate"::
    send(F?source, listing, Module, Predicate, Arity).

/* NOTE: lazy creation of this message interferes with call_cleanup/2 used
   by findall/3 from XPCE's lazy  method   binder.  Therefore  we make a
   dummy call to this method in ->initialise.
*/

frame_finished(F, Frame:int) :->
    "This frame was terminated; remove it"::
    get(F, member, stack, StackView),
    send(StackView, frame_finished, Frame),
    (   get(F, member, bindings, Bindings),
        get(Bindings, prolog_frame, Frame)
    ->  send(Bindings, background,
             ?(Bindings, class_variable_value, background_inactive)),
        send(Bindings, slot, prolog_frame, @nil),
        ignore(send(F, send_hyper, fragment, free))
    ;   true
    ),
    (   get(F, current_frame, Frame)
    ->  send(F, current_frame, @nil)
    ;   true
    ).

aborted(F) :->
    "User has aborted the query"::
    send(F, clear, @off),
    send(F, mode, aborted),
    send(F, report, status, 'Execution aborted').

thread_finished(F, Status:prolog) :->
    "Thread I'm associated with finished"::
    send(F, clear),
    send(F, mode, thread_finished),
    format(string(String), '~q', Status),
    send(F, report, status, 'Thread finished: %s', String).

query_finished(F, Message:char_array) :->
    "Toplevel query finished"::
    send(F, clear),
    send(F, mode, query_finished),
    send(F, report, status, Message).

mode(F, Mode:name) :->
    "Switch modes"::
    send(F, slot, mode, Mode),
    (   get(F, member, buttons, D)
    ->  (   Mode == wait_user
        ->  send(D, running, @off)
        ;   send(D, running, @on)
        )
    ;   true
    ).

:- pce_end_class(prolog_debugger).

                 /*******************************
                 *            BUTTONS           *
                 *******************************/

:- pce_begin_class(prolog_button_dialog, dialog,
                   "Dialog holding the function buttons").

%       button(Action, Keys, Image, Balloon)
%
%       If action is +Action, send message Action to the frame.  Otherwise
%       return Action to the caller.

button(into,           "i",   'into.xpm',            'Show unification').
button(creep,          "\n ", 'creep.xpm',           'Step').
button(skip,           "s",   'skip.xpm',            'Skip over this goal').
button(finish,         "f",   'finish.xpm',          'Finish selected goal').
button(gap,            -,     -,                     -).
button(retry,          "r",   'retry.xpm',           'Retry selected goal').
button(gap,            -,     -,                     -).
button(+nodebug,       "n",   'nodebug.xpm',         'Continue without debugging').
button(+abort,         "a",   'abort.xpm',           'Abort to the Prolog toplevel').
button(+interrupt,     "t",   'interrupt.xpm',       'Interrupt (trace)').
button(+query,         "b",   'break.xpm',           'Enter a query (in debugged thread)').
button(+interactor,    "B",   'interactor.xpm',      'Enter a query (in new thread)').
button(fail,           "F",   'fail.xpm',            'Force query to fail').
button(gap,            -,     -,                     -).
button(+up,            "u",   'up.xpm',              'Select parent frame').
button(+down,          "d",   'down.xpm',            'Select child frame').
button(gap,            -,     -,                     -).
button(+browse,        "",    '16x16/butterfly.xpm', 'Browse program structure').
button(gap,            -,     -,                     -).
button(leap,           "l",   'leap.xpm',            'Continue to spy- or breakpoint').
button(+breakpoints,   "+",   'spy.xpm',             'Edit spy- and breakpoints').
button(+stop_at,       "!",   'stop.xpm',            'Set Stop at caret').
button(+nostop_or_spy, "-",   'nostopspy.xpm',       'Delete break- or spy-point').
button(gap,            -,     -,                     -).
button(+details,       "v",   'details.xpm',         'Show (variable) details').
button(+edit,          "e",   'edit.xpm',            'Toggle read-only/edit-mode').


tag_balloon(Balloon0, Keys, Balloon) :-
    maplist(key_name, Keys, Names),
    atomic_list_concat(Names, ', ', Tag),
    atomic_list_concat([Balloon0, ' (', Tag, ')'], Balloon).

key_name(10, return) :- !.
key_name(32,  space) :- !.
key_name(C, A) :-
    char_code(A, C).

initialise(D) :->
    send_super(D, initialise),
    send(D, pen, 0),
    send(D, gap, size(0,0)),
    get(D, frame, Frame),
    send(D, append, new(TB, tool_bar(Frame))),
    (   button(Action, KeyString, Image, Balloon0),
        file_name_extension(Resource, _, Image),
        string_codes(KeyString, Keys),
        (   Action == gap
        ->  send(TB, append, gap)
        ;   tag_balloon(Balloon0, Keys, Balloon),
            make_message(Action, Name, D, Message),
            send(TB, append,
                 new(B, tool_button(Message,
                                    resource(Resource),
                                    Balloon,
                                    name := Name))),
            chain_list(KL, Keys),
            send(B, attribute, keys, KL)
        ),
        fail
    ;   true
    ).

make_message(+Action, Action, D, message(D?frame, Action)) :- !.
make_message(Action,  Action, D, message(D, return, Action)).

typed(D, Id:event_id, Delegate:[bool]) :->
    "Handle typing"::
    (   get(D, find, @default,
            and(message(@arg1, has_get_method, keys),
                message(@arg1?keys, member, Id)),
            Button)
    ->  send(Button, execute)
    ;   Delegate == @on
    ->  send_super(D, typed, Id, Delegate)
    ).

event(D, Ev:event) :->
    (   send(Ev, is_a, keyboard)
    ->  send(D, typed, Ev)
    ;   send_super(D, event, Ev)
    ).

button(D, Name:name, Button:button) :<-
    "Find button from its name"::
    get(D, member, tool_bar, TB),
    get(TB, member, Name, Button).

running(D, Running:bool) :->
    "Make some buttons (in)active"::
    get(Running, negate, NotRunning),
    forall(running_button(Name),
           (   get(D, button, Name, Button)
           ->  send(Button, active, NotRunning)
           ;   format('No button ~w~n', [Name])
           )),
    (   get(D, button, interrupt, Interrupt)
    ->  send(Interrupt, active, Running)
    ;   true
    ).

running_button(into).
running_button(creep).
running_button(skip).
running_button(retry).
running_button(finish).
running_button(nodebug).
running_button(abort).

:- pce_end_class(prolog_button_dialog).


                 /*******************************
                 *           VARIABLES          *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We use a view with some tweaks   to  display the bindings. Originally we
used a browser, but a view has  two   advantages.  First  of all, we can
write directly to it by opening it as   a stream and second the user can
use search and selection on the view to   analyse it or export text from
it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(prolog_bindings_view, view,
                   "Overview of bindings of the current frame").

class_variable(font,    font,   normal, "Font for bindings").
class_variable(size,    size,   size(40,11), "Initial size").

variable(prolog_frame, int*, both, "Frame who's variables we are showing").

class_variable(background_active,   colour, white).
class_variable(background_inactive, colour, grey80).

:- pce_global(@prolog_binding_recogniser,
              make_prolog_binding_recogniser).
:- pce_global(@prolog_binding_popup,
              make_prolog_binding_popup).

make_prolog_binding_recogniser(G) :-
    new(View, @event?window),
    new(Index, ?(@event?receiver, index, @event)),
    new(C1, click_gesture(left, '', single,
                          message(View, on_click, Index))),
    new(C2, click_gesture(left, '', double,
                          message(View, details))),
    new(C3, popup_gesture(@prolog_binding_popup)),
    send(@prolog_binding_popup, update_message,
         message(View, on_click, Index)),
    new(G, handler_group(C1, C2, C3)).

make_prolog_binding_popup(P) :-
    new(P, popup),
    send_list(P, append,
              [ menu_item(details, message(@arg1?window, details)),
                menu_item(copy,    message(@arg1?window, details,
                                           @default, copy))
              ]).

initialise(B) :->
    send_super(B, initialise),
    send(B?text_buffer, undo_buffer_size, 0),
    get(B, font, Font),
    get(Font, ex, Ex),
    Tab is 15 * Ex,
    send(B, wrap, none),
    send(B?image, tab_stops, vector(Tab)),
    send(B?image, recogniser, @prolog_binding_recogniser),
    send(B, editable, @off),
    send(B, style, constraint, style(colour := blue)),
    send(B?text_cursor, displayed, @off),
    send(B, ver_stretch, 0).

clear(B, Content:[bool]) :->
    send(B, prolog_frame, @nil),
    (   Content == @off
    ->  send(B, background, ?(B, class_variable_value, background_inactive))
    ;   send_super(B, clear)
    ).

scroll_to_end(B) :->
    "Scroll the last line to the bottom"::
    get(B, editor, E),
    get(E, size, size(_,Lines)),
    send(E, scroll_to, @default, Lines).

details(B, Fragment:[prolog_frame_var_fragment], Action:[{view,copy}]) :->
    "View details of the binding"::
    get(B, prolog_frame, Frame),
    (   Frame \== @nil
    ->  true
    ;   send(B, report, warning, 'No current frame'),
        fail
    ),
    (   Fragment == @default
    ->  (   get(B, selected_fragment, Frag),
            Frag \== @nil
        ->  true
        ;   send(B, report, warning, 'No selected variable'),
            fail
        )
    ;   Frag = Fragment
    ),
    (   get(Frag, var_name, VarName)
    ->  get(Frag, value, Value),
        prolog_frame_attribute(B, Frame, level, Level),
        prolog_frame_attribute(B, Frame, goal, Goal),
        predicate_name(Goal, PredName),
        (   integer(VarName)
        ->  VarType = 'Argument'
        ;   VarType = 'Variable'
        ),
        format(string(Label), '~w ~w of frame at level ~d running ~w',
               [ VarType, VarName, Level, PredName ]),
        debug(gtrace(bindings), 'Action ~w on ~w', [Action, Value]),
        (   Action == copy
        ->  (   numbervars(Value, 0, _, [attvar(skip)]),
                format(string(Text), '~q', [Value]),
                send(@display, copy, Text),
                fail
            ;   send(B, report, status, Label)
            )
        ;   view_term(Value,
                      [ comment(Label),
                        source_object(Frag),
                        expose(true)
                      ])
        )
    ;   send(B, report, warning, 'Not a variable value')
    ).

on_click(B, Index:int) :->
    "Select fragment clicked"::
    get(B, text_buffer, TB),
    send(B, selection, Index, Index),           % do not move
    (   get(TB, find_fragment, message(@arg1, overlap, Index), Frag)
    ->  send(B, selected_fragment, Frag)
    ;   send(B, selected_fragment, @nil)
    ).

% Bindings is a list of Vars = Value,   where Vars is a list of variable
% identifiers that take the form Name:ArgN,   were  Name is the variable
% name (atom) and ArgN is the location in the frame.

bindings(B, Bindings:prolog) :->
    "Display complete list of bindings"::
    (   term_attvars(Bindings, [])
    ->  Plain = Bindings,
        Constraints = []
    ;   copy_term(Bindings, Plain, Constraints)
    ),
    bind_vars(Plain),
    cycles(Plain, Template, Cycles, Plain),
    send(B, background, ?(B, class_variable_value, background_active)),
    pce_open(B, write, Fd),
    (   forall(member(Vars=Value, Template),
               send(B, append_binding, Vars, value(Value), Fd)),
        forall(member(C, Constraints),
               send(B, append_extra, C, Fd, constraint)),
        forall(member(C, Cycles),
               send(B, append_extra, C, Fd, cycle)),
        fail
    ;   true
    ),
    close(Fd),
    send(B, scroll_to_end).

bind_vars([]).
bind_vars([Vars=Value|T]) :-
    (   var(Value)
    ->  Vars = [Name:_|_],
        Value = '$VAR'(Name)
    ;   true
    ),
    bind_vars(T).

cycles(Term, Template, Cycles, _) :-
    acyclic_term(Term),
    !,
    Template = Term,
    Cycles = [].
cycles(Term, Template, Cycles, Bindings) :-
    '$factorize_term'(Term, Template, Factors),
    bind_non_cycles(Factors, Cycles),
    name_cycle_vars(Cycles, 1, Bindings).

bind_non_cycles([], []).
bind_non_cycles([V=Term|T], L) :-
    unify_with_occurs_check(V, Term),
    !,
    bind_non_cycles(T, L).
bind_non_cycles([H|T0], [H|T]) :-
    bind_non_cycles(T0, T).


name_cycle_vars([], _, _).
name_cycle_vars([H|T], I, Bindings) :-
    H = (Var=_Value),
    (   member(Vars=VarsValue, Bindings),
        VarsValue == Var,
        Vars = [Name:_|_]
    ->  I2 = I
    ;   atom_concat('_S', I, Name),
        I2 is I + 1
    ),
    Var = '$VAR'(Name),
    name_cycle_vars(T, I2, Bindings).


append_binding(B, Names0:prolog, ValueTerm:prolog, Fd:prolog) :->
    "Add a binding to the browser"::
    ValueTerm = value(Value0),      % protect :=, ?, etc.
    (   Value0 = '$VAR'(Name), Names0 = [Name:_]
    ->  (   setting(show_unbound, false)
        ->  true
        ;   format(Fd, '~w\t= _~n', [Name])
        )
    ;   (   Value0 = '$VAR'(_), Names0 = [_,_|_]
        ->  append(Names, [VarN:_], Names0),
            Value = '$VAR'(VarN)
        ;   Names = Names0,
            Value = Value0
        ),
        get(B, text_buffer, TB),
        get(TB, size, S0),
        (   Names = VarName:ArgN
        ->  format(Fd, '~w', [VarName])
        ;   Names = [VarName:ArgN|_],
            write_varnames(Fd, Names)
        ),
        current_prolog_flag(answer_write_options, Options),
        format(Fd, '\t= ~W~n', [Value, Options]),
        flush_output(Fd),
        get(TB, size, S1),
        new(_, prolog_frame_var_fragment(TB, S0, S1, VarName, ArgN))
    ).

write_varnames(Fd, [N:_]) :-
    !,
    format(Fd, '~w', N).
write_varnames(Fd, [N:_|T]) :-
    format(Fd, '~w = ', N),
    write_varnames(Fd, T).

append_extra(B, Constraint:prolog, Fd:prolog, Comment:name) :->
    "Display current constraints"::
    get(B, text_buffer, TB),
    current_prolog_flag(answer_write_options, Options),
    get(TB, size, S0),
    format(Fd, '(~w)\t~W~n', [Comment, Constraint, Options]),
    flush_output(Fd),
    get(TB, size, S1),
    new(_, prolog_frame_constraint_fragment(TB, S0, S1)).

:- pce_end_class(prolog_bindings_view).


:- pce_begin_class(prolog_frame_var_fragment, fragment,
                   "Represent a variable in a frame").

variable(var_name, name, get, "Name of displayed variable").
variable(argn,     int,  get, "Slot in frame").

initialise(F, TB:text_buffer, From:int, To:int, Name:name, ArgN:int) :->
    Len is To-From,
    send_super(F, initialise, TB, From, Len, frame),
    send(F, slot, var_name, Name),
    send(F, slot, argn, ArgN).

%       Issue: this copies really big values

value(F, Value:prolog) :<-
    "Get current value of the variable"::
    get(F, text_buffer, TB),
    get(TB?editors, head, Editor),
    get(Editor, window, View),
    get(View, prolog_frame, Frame), Frame \== @nil,
    get(F, argn, ArgN),
    prolog_frame_attribute(F, Frame, argument(ArgN), Value).

:- pce_end_class(prolog_frame_var_fragment).


:- pce_begin_class(prolog_frame_constraint_fragment, fragment,
                   "Represent a contraint on a frame").

initialise(F, TB:text_buffer, From:int, To:int) :->
    Len is To-From,
    send_super(F, initialise, TB, From, Len, constraint).

var_name(_F, _Name:name) :<-
    "Cannot show details"::
    fail.

:- pce_end_class(prolog_frame_constraint_fragment).


                 /*******************************
                 *             EVENTS           *
                 *******************************/

:- initialization
    prolog_unlisten(frame_finished, frame_finished),
    prolog_listen(frame_finished, frame_finished).

frame_finished(Frame) :-
    thread_self_id(Thread),
    gui(Thread, _, Gui),            % has a gui
    send_pce_async(send(Gui, frame_finished(Frame))).

destroy_thread_debug_gui(Thread) :-
    (   gui(Thread, _, Gui)
    ->  thread_property(Thread, status(Status)),
        send_pce_async(send(Gui, thread_finished(Status)))
    ;   true
    ).

user:message_hook('$aborted', _, _Lines) :-
    aborted,
    fail.
user:message_hook(query(YesNo), _, _Lines) :-
    query_finished(YesNo),
    fail.
user:message_hook(break(end, Level)) :-
    thread_self_id(Thread),
    gui(Thread, Level, Gui),
    send_pce_async(send(Gui, destroy)),
    fail.

aborted :-
    thread_self_id(Thread),
    gui(Thread, Level, Gui),
    (   Level \== 0
    ->  Message = destroy
    ;   Message = aborted
    ),
    send_pce_async(send(Gui, Message)).

query_finished(YesNo) :-
    finished(YesNo, Message),
    thread_self_id(Thread),
    break_level(Level),
    gui(Thread, Level, Gui),
    send_pce_async(send(Gui, query_finished(Message))).

finished(no, 'Query failed').
finished(yes, 'Query succeeded').
finished(done, 'User ended query').
finished(yes(_), 'Query succeeded with result').
finished(more(_), 'Query succeeded non-deterministically with result').
