/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cwi.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2020, University of Amsterdam
                              CWI, Amsterdam
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

:- module(prolog_predicate, []).
:- use_module(library(pce)).
:- use_module(pce_arm).
:- use_module(library(persistent_frame)).
:- use_module(library(tabbed_window)).
:- use_module(library(tabular)).
:- require([ atomic_list_concat/2,
             term_to_atom/2,
             auto_call/1
           ]).

:- if(exists_source(library(pldoc/man_index))).
:- autoload(library(pldoc/man_index), [man_object_property/2]).
:- endif.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class prolog_predicate represents the identity of a Prolog predicate. It
is used with predicate_item  for   locating  predicates and encapsulates
access to various parts of the development environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(prolog_predicate, object,
                   "Represent a Prolog predicate").

variable(module,        name*,   get, "Module of the predicate").
variable(name,          name,    get, "Name of predicate").
variable(arity,         ['0..'], get, "Arity of the predicate").

initialise(P, Term:prolog) :->
    "Create from [Module]:Name/Arity"::
    (   Term = Module:Name/Arity
    ->  true
    ;   Term = Name/Arity
    ->  Module = @nil
    ;   Term = Module:Head,
        callable(Head)
    ->  functor(Head, Name, Arity)
    ;   callable(Term)
    ->  functor(Term, Name, Arity),
        Module = @nil
    ),
    (   var(Arity)
    ->  Arity = @default
    ;   true
    ),
    (   var(Module)
    ->  Module = @nil
    ;   true
    ),
    send(P, slot, module, Module),
    send(P, slot, name, Name),
    send(P, slot, arity, Arity).

convert(_, From:name, P:prolog_predicate) :<-
    "Convert textual and Prolog term"::
    catch(term_to_atom(From, Term), _, fail),
    (   (   Term = _:_/_
        ;   Term = _/_
        )
    ->  new(P, prolog_predicate(Term))
    ;   Term = Module:Head,
        callable(Head)
    ->  functor(Head, Name, Arity),
        new(P, prolog_predicate(Module:Name/Arity))
    ;   callable(Term)
    ->  functor(Term, Name, Arity),
        new(P, prolog_predicate(Name/Arity))
    ).

print_name(P, PN:name) :<-
    "Return as [Module:]Name/Arity"::
    get(P, name, Name),
    get(P, arity, Arity),
    get(P, module, Module),
    (   Module \== @nil,
        Arity \== @default
    ->  functor(Head, Name, Arity), % fully qualified
        (   user:prolog_predicate_name(Module:Head, PN)
        ->  true
        ;   \+ hidden_module(Module, Head)
        ->  atomic_list_concat([Module, :, Name, /, Arity], PN)
        ;   atomic_list_concat([Name, /, Arity], PN)
        )
    ;   (   Arity == @default
        ->  End = ['/_']
        ;   End = [/, Arity]
        )
    ->  (   Module == @nil
        ->  atomic_list_concat([Name|End], PN)
        ;   atomic_list_concat([Module, :, Name|End], PN)
        )
    ).

hidden_module(system, _).
hidden_module(user, _).
hidden_module(M, H) :-
    predicate_property(system:H, imported_from(M)).

head(P, Qualify:[bool], Head:prolog) :<-
    "Get a head-term"::
    get(P, module, Module),
    get(P, name, Name),
    get(P, arity, Arity),
    Arity \== @default,
    functor(Head0, Name, Arity),
    qualify(Qualify, Module, Head0, Head).

qualify(Qualify, Module, Head0, Head) :-
    (   (   Qualify == @off
        ;   Qualify == @default,
            Module == @nil
        )
    ->  Head = Head0
    ;   Module \== @nil
    ->  Head = Module:Head0
    ;   Head = user:Head0
    ).

pi(P, Qualify:[bool], PI:prolog) :<-
    "Get a predicate indicator"::
    get(P, module, Module),
    get(P, name, Name),
    get(P, arity, Arity),
    (   Arity == @default
    ->  PI0 = Name/_
    ;   PI0 = Name/Arity
    ),
    qualify(Qualify, Module, PI0, PI).


%       <-source:
%
%       Get the source-location for this predicate. If not available and
%       the autoload argument is not @off, try to autoload the predicate
%       and try again.
%
%       TBD: Deal with multiple solutions

source(P, Autoload:[bool], Loc:source_location) :<-
    "Return source-location from Prolog DB"::
    get(P, head, Head0),
    (   Head0 = _:_
    ->  Head = Head0
    ;   Head = _:Head0
    ),
    (   predicate_property(Head, file(File))
    ->  true
    ;   Autoload \== @off,
        send(P, autoload),
        predicate_property(Head, file(File))
    ),
    (   predicate_property(Head, line_count(Line))
    ->  new(Loc, source_location(File, Line))
    ;   new(Loc, source_location(File))
    ).


edit(P) :->
    "Edit the predicate"::
    get(P, head, @on, Head),
    auto_call(edit(Head)).


autoload(P, Module:[name]) :->
    "Autoload the definition"::
    get(P, head, @off, Term),
    (   Module == @default
    ->  '$define_predicate'(Term)
    ;   '$define_predicate'(Module:Term)
    ).

has_property(P, Prop:prolog) :->
    "Test predicate property"::
    get(P, head, Head),
    predicate_property(Head, Prop).

help(P) :->
    "Activate the help-system"::
    get(P, head, @off, Head),
    functor(Head, Name, Arity),
    (   help(Name/Arity)
    ->  true
    ;   send(P, report, warning, 'Cannot find help for %s/%d', Name, Arity)
    ).

has_help(P) :->
    "See if there is help around"::
    get(P, summary, _).

summary(P, Summary:string) :<-
    get(P, name, Name),
    get(P, arity, Arity),
    (   man_predicate_summary(Name/Arity, Summary0),
        new(Summary, string('%s', Summary0))
    ->  true
    ;   (   get(P, module, M),
            M \== @nil
        ->  true
        ;   M = _
        ),
        summary(M:Name/Arity, Summary)
    ).

:- if(current_predicate(man_object_property/2)).
man_predicate_summary(PI, Summary) :-
    man_object_property(PI, summary(Summary)).
:- elif(current_predicate(predicate/5)).
man_predicate_summary(Name/Arity, Summary) :-
    predicate(Name, Arity, Summary, _, _).
:- else.
man_predicate_summary(_, _) :-
    fail.
:- endif.

:- multifile
    prolog:predicate_summary/2.

summary(PI, Summary) :-
    prolog:predicate_summary(PI, Summary).

info(P) :->
    "Open information sheet on predicate"::
    (   get(P, head, Head),
        predicate_property(Head, imported_from(M2))
    ->  get(P, pi, @off, PI),
        send(prolog_predicate_frame(prolog_predicate(M2:PI)), open)
    ;   send(prolog_predicate_frame(P), open)
    ).

:- pce_end_class(prolog_predicate).


:- pce_begin_class(prolog_predicate_frame, persistent_frame,
                   "Provide information about a predicate").

variable(predicate, prolog_predicate, get, "Current predicate").

initialise(F, P:prolog_predicate) :->
    "Create from a predicate"::
    send_super(F, initialise, string('Info for %s', P?print_name)),
    send(F, slot, predicate, P),
    send(F, append, new(tabbed_window)),
    send(F, add_general_info),
    send(F, add_documentation),
    send(F, add_callers).

add_general_info(F) :->
    "Show general info on the predicate"::
    get(F, predicate, P),
    get(F, member, tabbed_window, TW),
    send(TW, append, prolog_predicate_info_window(P)).

add_documentation(_F) :->
    "Show documentation about the predicate"::
    true.

add_callers(_F) :->
    "Add window holding callers to the predicate"::
    true.

:- pce_end_class(prolog_predicate_frame).


:- pce_begin_class(prolog_predicate_info_window, window,
                   "Show table with general properties of predicate").
:- use_class_template(arm).

variable(tabular,   tabular,          get, "Displayed table").
variable(predicate, prolog_predicate, get, "Displayed predicate").

initialise(W, P:prolog_predicate) :->
    "Create info sheet for P"::
    send_super(W, initialise),
    send(W, name, properties),
    send(W, pen, 0),
    send(W, scrollbars, vertical),
    send(W, display, new(T, tabular)),
    send(T, rules, all),
    send(T, cell_spacing, -1),
    send(W, slot, tabular, T),
    send(W, predicate, P).

resize(W) :->
    send_super(W, resize),
    get(W?visible, width, Width),
    send(W?tabular, table_width, Width-3).

clear(W) :->
    send(W?tabular, clear).

predicate(W, P:prolog_predicate) :->
    send(W, slot, predicate, P),
    send(W, update).

update(W) :->
    get(W, predicate, P),
    send(W, clear),
    get(P, pi, PI),
    (   PI = _:_
    ->  QPI = PI
    ;   QPI = _:PI
    ),
    forall(setof(Prop, pi_property(QPI, Prop), Props),
           send(W, properties, QPI, Props)).

pi_property(M:Name/Arity, Prop) :-
    integer(Arity),
    functor(Head, Name, Arity),
    current_predicate(M:Name/Arity),
    \+ predicate_property(M:Head, imported_from(_)),
    predicate_property(M:Head, Prop).
pi_property(M:Name/_, Prop) :-
    current_predicate(M:Name, Head),
    \+ predicate_property(M:Head, imported_from(_)),
    predicate_property(M:Head, Prop).

properties(W, QPI:prolog, Props:prolog) :->
    "Append property sheet or a specific definition"::
    get(W, tabular, T),
    format(atom(AQPI), '~q', [QPI]),
    BG = (background := khaki1),
    send(T, append, AQPI, halign := center, colspan := 2, BG),
    send(T, next_row),
    partition(atom, Props, Atomic, Valued),
    (   select(file(File), Valued, Valued1),
        select(line_count(Line), Valued1, Valued2)
    ->  send(T, append, 'Source:', bold, right),
        send(T, append, source_location_text(source_location(File,Line))),
        send(T, next_row)
    ;   Valued2 = Valued
    ),
    delete(Atomic, visible, Atomic1),
    (   memberchk(meta_predicate(_), Valued2)
    ->  delete(Atomic1, transparent, Atomic2)
    ;   Atomic2 = Atomic1
    ),
    forall(member(P, Valued2), send(W, property, P)),
    atomic_list_concat(Atomic2, ', ', AtomicText),
    send(T, append, 'Flags:', bold, right),
    send(T, append, AtomicText),
    send(T, next_row).

property(W, Prop:prolog) :->
    "Append a property"::
    get(W, tabular, T),
    (   Prop =.. [Name,Value]
    ->  send(T, append, string('%s:', Name?label_name), bold, right),
        format(atom(AValue), '~q', [Value]),
        send(T, append, AValue)
    ;   send(T, append, Prop?label_name, colspan := 2)
    ),
    send(T, next_row).

:- pce_end_class(prolog_predicate_info_window).


:- pce_begin_class(source_location_text, text,
                   "Indicate a source location").

variable(location, source_location, get, "Represented location").

initialise(T, Loc:source_location) :->
    "Create from source location"::
    send_super(T, initialise, Loc?print_name),
    send(T, slot, location, Loc).

:- pce_global(@source_location_text_recogniser,
              new(handler_group(@arm_recogniser,
                                click_gesture(left, '', single,
                                              message(@receiver, edit))))).

event(T, Ev:event) :->
    (   send_super(T, event, Ev)
    ->  true
    ;   send(@source_location_text_recogniser, event, Ev)
    ).


arm(TF, Val:bool) :->
    "Preview activiity"::
    (   Val == @on
    ->  send(TF, underline, @on)
    ;   send(TF, underline, @off)
    ).

edit(T) :->
    get(T, location, Loc),
    send(@emacs, goto_source_location, Loc, tab).

:- pce_end_class(source_location_text).
