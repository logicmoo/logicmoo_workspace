/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2001-2018, University of Amsterdam
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

:- module(prolog_source_view,
          [ current_source_buffer/2     % +File, -Buffer
          ]).
:- use_module(library(pce)).
:- use_module(library(debug)).
:- use_module(library(pce_emacs)).
:- use_module(util).
:- use_module(library(prolog_breakpoints)).
:- use_module(library(emacs_extend)).
:- use_module(library(pce_template)).
:- use_module(emacs_debug_modes).
:- require([ maplist/3,
	     merge_options/3,
	     setup_call_cleanup/3
	   ]).

:- multifile
    port_style/2.


                 /*******************************
                 *             STYLES           *
                 *******************************/

resource(call,   image, image('call.xpm')).
resource(exit,   image, image('exit.xpm')).
resource(redo,   image, image('redo.xpm')).
resource(fail,   image, image('fail.xpm')).
resource(except, image, image('except.xpm')).
resource(ndet,   image, image('ndet.xpm')).
resource(stack,  image, image('stack.xpm')).
resource(stop,   image, image('stop.xpm')).

style(Port, Style) :-
    def_style(Port, DefAttrs),
    (   port_style(Port, PrefAttrs)
    ->  merge_options(PrefAttrs, DefAttrs, Attrs)
    ;   Attrs = DefAttrs
    ),
    make_style(Attrs, Style).

make_style(Attributes, ObjTerm) :-
    maplist(att_assign, Attributes, Args),
    ObjTerm =.. [style|Args].

att_assign(Term, Name := Value) :-
    Term =.. [Name, Value].


def_style(call,         [background(green),     icon(resource(call))]).
def_style(break,        [background(cyan)]).
def_style(exit,         [background(green),     icon(resource(exit))]).
def_style(redo,         [background(yellow),    icon(resource(redo))]).
def_style(fail,         [background('#ff8080'), icon(resource(fail))]).
def_style(exception,    [background(magenta),   icon(resource(except))]).
def_style(unify,        [background(sky_blue)]).
def_style(choice,       [background(yellow),    icon(resource(ndet))]).
def_style(frame,        [background('#d6dc5e'), icon(resource(stack))]).
def_style(breakpoint,   [icon(resource(stop))]).


% If you define an alternative mode as a subclass of the Prolog mode
% and you use this mode in the debugger, you must create a mode
% <X>_debug.  The minimal example for this mode is below.  The
% predicate buffer/2 below associates the debug mode with the
% source view.

:- emacs_begin_mode(prolog_debug, prolog,
                    "Submode for the debugger",
                    [], []).
:- use_class_template(prolog_debug_methods).
:- emacs_end_mode.

:- initialization
   declare_emacs_mode(prolog_debug, []).


                 /*******************************
                 *          SOURCE VIEW         *
                 *******************************/

:- pce_begin_class(prolog_source_view, emacs_view,
                   "Prolog GUI source viewer").

class_variable(size,    size,   size(80,20), "Default size in characters").

variable(source,        'name|emacs_buffer*', get, "Currently shown source").

initialise(V) :->
    send(V, send_super, initialise),
    send(V, mode, prolog_debug),
    send(V, margin_width, 22),
    forall(style(Name, Style), send(V, style, Name, Style)),
    send(V, editable, @off),
    send(V, update_label).

lost_text_buffer(V) :->
    "The textbuffer has been destroyed, replace by a new one"::
    new(Scratch, emacs_buffer(@nil, '*scratch*')),
    send(V, text_buffer, Scratch).

update_label(V) :->
    "Create label from <-editable and <-source"::
    get(V, source, Source),
    (   atom(Source)
    ->  Label0 = Source
    ;   Source == @nil
    ->  Label0 = '<no source>'
    ;   get(Source, attribute, comment, Label0)
    ->  true
    ;   get(Source, name, Label0)
    ),
    (   get(V, editable, @on)
    ->  send(V, label, string('[edit] %s', Label0))
    ;   send(V, label, Label0)
    ).

:- pce_group(event).

post_event(V, Ev:event) :->
    (   send(Ev, is_a, keyboard),
        get(V, editable, @off),
        get(V, focus_function, @nil),
        get(V, frame, Tracer),
        send(Tracer, has_send_method, source_typed),
        send(Tracer, source_typed, Ev)
    ->  true
    ;   send_super(V, post_event, Ev)
    ).

:- pce_group(edit).

edit(V, Val:[bool]) :->
    "Toggle read-only mode"::
    (   Val == @default
    ->  get(V?editable, negate, NewVal)
    ;   NewVal = Val
    ),
    send(V, editable, NewVal),
    send(V, update_label).

:- pce_group(stop).

stop_at(V) :->
    "Set stop-point at location"::
    (   get(V, source_file, File)
    ->  get(V, caret, Caret),
        get(V, line_number, Line),
        set_breakpoint(File, Line, Caret, _)
    ;   send(V, report, error, 'No source'),
        fail
    ).

delete_selected_stop(V) :->
    "Deleted selected stop or stop overlapping <-caret"::
    (   get(V, selected_fragment, F),
        send(F, instance_of, break_fragment)
    ->  send(F, remove)
    ;   get(V, caret, Caret),
        get(V?text_buffer, find_fragment,
            and(message(@arg1, instance_of, break_fragment),
                message(@arg1, overlap_caret, Caret)), F)
    ->  send(F, remove)
    ).

:- pce_group(source).

:- pce_global(@gui_last_change_check, new(date)).

not_recently_checked :-
    new(D, date),
    get(D, difference, @gui_last_change_check, Secs),
    (   Secs < 5
    ->  free(D),
        fail
    ;   send(@gui_last_change_check, copy, D)
    ).

source(V, Source:'name|emacs_buffer*') :->
    "Attach to indicated file"::
    debug(gtrace(source), 'Attaching source ~p ...', [Source]),
    (   get(V, source, Source)
    ->  send(V, check_modified)
    ;   (   Source == @nil
        ->  send(V, text_buffer, emacs_buffer(@nil, '<no source>'))
        ;   send(Source, instance_of, emacs_buffer)
        ->  send(Source, margin_width, 22),
            send(V, text_buffer, Source)
        ;   absolute_file_name(Source, Canonical),
            buffer(Canonical, B),
            send(V, text_buffer, B),
            send(V, check_modified)
        ),
        send(V, slot, source, Source),
        send(V, update_label),
        send(V?editor, auto_colourise_buffer)
    ),
    debug(gtrace(source), 'ok', []).

source_file(V, File:name) :<-
    "Currently shown sourcefile"::
    get(V, source, Source),
    atom(Source),
    canonical_source_file(Source, File).

check_modified(V) :->
    "Check for possibly modified file"::
    (   not_recently_checked
    ->  get(V, text_buffer, TB),
        send(TB, check_modified_file, V?frame, @off)
    ;   true
    ).

:- pce_group(show).

show_range(V, File:'name|emacs_buffer', From:int, To:int, Style:name) :->
    "Show indicated region using Style"::
    send(V, source, File),
    send(V, caret, To),
    new(F, fragment(V, From, To-From, Style)),
    ignore(send(V?frame, send_hyper, fragment, free)),
    new(_, trace_hyper(V?frame, F, fragment, tracer)),
    send(V, normalise, From, To).

show_line(V, File:'name|emacs_buffer', Line:int, Style:name) :->
    "Show numbered line"::
    debug(gtrace(source), 'Show ~w:~w, style = ~w', [File, Line, Style]),
    send(V, source, File),
    get(V, text_buffer, TB),
    get(TB, scan, 0, line, Line - 1, start, SOL),
    get(TB, scan, SOL, line, 0, end, EOL),
    debug(gtrace(source), 'Char range ~w ... ~w', [SOL, EOL]),
    send(V, show_range, File, SOL, EOL, Style).

listing(V, Module:name, Predicate:name, Arity:int) :->
    "List the specified predicate"::
    functor(Head, Predicate, Arity),
    send(V, source, @nil),
    get(V, text_buffer, TB),
    setup_call_cleanup(open(TB, write, Fd),
                       with_output_to(Fd, listing(Module:Head)),
                       close(Fd)).

:- pce_end_class.

                 /*******************************
                 *      BUFFER MANAGEMENT       *
                 *******************************/

current_source_buffer(Buffer, Buffer) :-
    object(Buffer),
    send(Buffer, instance_of, emacs_buffer),
    !.
current_source_buffer(File, Buffer) :-
    get(@emacs, file_buffer, File, Buffer).

buffer(File, Buffer) :-
    new(Buffer, emacs_buffer(File)),
    get(Buffer, mode, Mode),
    (   sub_atom(Mode, _, _, 0, '_debug')
    ->  true
    ;   atom_concat(Mode, '_debug', DebugMode),
        get(@pce, convert, Mode, emacs_mode, _), % force loading the code
        get(@pce, convert, DebugMode, emacs_mode, _)
    ->  send(Buffer, mode, DebugMode)
    ;   send(Buffer, mode, prolog_debug)
    ),
    mark_special(File, Buffer).

mark_special(_, Buffer) :-
    get(Buffer, attribute, debugger_marks_done, @on),
    !.
mark_special(File, Buffer) :-
    canonical_source_file(File, Source),
    send(Buffer, attribute, debugger_marks_done, @on),
    send(Buffer, margin_width, 22),
    mark_stop_points(Buffer, Source).

mark_stop_points(_, Source) :-
    forall(breakpoint_property(Id, file(Source)),
           mark_breakpoint(Id, set)).

:- pce_global(@prolog_debugger, new(object)).

%!  mark_breakpoint(+Id, +SetDel) is det.
%
%   Mark stop-points using a breakpoint fragment.  SetDel is one of
%   =set= or =delete=

mark_breakpoint(Id, set) :-
    break_fragment(Id, _),
    !.
mark_breakpoint(Id, set) :-
    breakpoint_property(Id, file(File)),
    current_source_buffer(File, Buffer),
    new(F, break_fragment(Buffer, Id)),
    assertz(break_fragment(Id, F)),
    new(_, hyper(@prolog_debugger, F, break, debugger)).

mark_breakpoint(Id, delete) :-
    forall(break_fragment(Id, Fragment),
           free(Fragment)).

:- multifile user:message_hook/3.

user:message_hook(breakpoint(SetDel, Id), _, _) :-
    catch(mark_breakpoint(Id, SetDel), _, fail),
    fail.


                 /*******************************
                 *            TRACE HYPER       *
                 *******************************/

:- pce_begin_class(trace_hyper, hyper).

unlink_from(H) :->
    get(H, to, Fragment),
    free(Fragment),
    free(H).

:- pce_end_class.

:- pce_begin_class(break_fragment, fragment,
                   "Visualise a break-point").

variable(breakpoint_id, int, both, "Identifier for the break-point").

:- dynamic
    break_fragment/2.               % ?Id, ?Fragment

initialise(F, TB:text_buffer, Id:int) :->
    "Indicate the location of a break-point"::
    (   breakpoint_property(Id, character_range(Start, Len))
    ->  send_super(F, initialise, TB, Start, Len, breakpoint)
    ;   breakpoint_property(Id, line_count(Line))
    ->  Skip is Line - 1,
        get(TB, scan, 0, line, Skip, start, Start),
        get(TB, scan, Start, line, 0, end, End),
        Len is End-Start,
        send_super(F, initialise, TB, Start, Len, breakpoint)
    ;   send_super(F, initialise, TB, 0, 0) % No source, what to do?
    ),
    send(F, slot, breakpoint_id, Id).

unlink(F) :->
    retractall(break_fragment(_,F)),
    send_super(F, unlink).

remove(F) :->
    "Remove the associated break-point"::
    break_fragment(Id, F),
    delete_breakpoint(Id).

overlap_caret(F, Caret:int) :->
    "True if F overlaps with position"::
    (   send(F, overlap, Caret)
    ->  true
    ;   get(F, end, E),
        get(F, start, S),
        Caret >= S, Caret =< E+1
    ).

:- pce_end_class.
