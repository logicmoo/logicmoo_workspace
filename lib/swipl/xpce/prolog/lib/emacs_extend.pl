/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(emacs_extend,
          [ declare_emacs_mode/2,
            declare_emacs_mode/3
          ]).
:- use_module(library(pce)).
:- require([ atomic_list_concat/2,
             send_list/3
           ]).

                /********************************
                *         DECLARE MODES         *
                ********************************/

:- dynamic                              % ensure it is restored over a state
    emacs_mode_name/1.

fix_mode_name_type :-
    get(@pce, convert, mode_name, type, Type),
    send(Type, name_reference, mode_name_type),
    send(Type, kind, name_of),
    send(Type, slot, context, new(Ctx, chain)),
    send_list(Ctx, append,
              [ fundamental
              , prolog
              , shell
              ]),
    forall(emacs_mode_name(Mode), send(Ctx, append, Mode)),
    send(Ctx, sort, unique := @on).

:- initialization fix_mode_name_type.

%!  declare_emacs_mode(+ModeName, +FileSpec).
%
%   Specifies that PceEmacs mode `ModeName' may be defined by
%   (auto)loading `FileSpec'.

declare_emacs_mode(Mode, File) :-
    get(string('emacs_%s_mode', Mode), value, EmacsModeClass),
    (   File == []
    ->  true
    ;   pce_autoload(EmacsModeClass, File)
    ),
    (   \+ special_mode(Mode)
    ->  get(@mode_name_type, context, Ctx),
        send(Ctx, add, Mode),
        send(Ctx, sort, unique := @on),
        assert(emacs_mode_name(Mode))
    ;   true
    ).

special_mode(shell).
special_mode(gdb).
special_mode(annotate).


%!  declare_emacs_mode(+ModeName, +FileSpec, +ListOfPatterns)
%
%   Sames as declare_emacs_mode/2.  `ListOfPatterns' is a list of
%   regular expressions that will automatically start this mode.

declare_emacs_mode(Mode, File, Extensions) :-
    declare_emacs_mode(Mode, File),
    declare_file_patterns(Extensions, Mode, @emacs_mode_list).

declare_file_patterns([], _, _).
declare_file_patterns([Ext|Rest], Mode, Sheet) :-
    send(Sheet, value, regex(Ext), Mode),
    declare_file_patterns(Rest, Mode, Sheet).

%       :- emacs_begin_mode(+Mode, +Super, +Summary, +Bindings, +Syntax).
%
%       Binding:
%
%               Selector = [key(Key)]
%                          [+ button(Button)]
%                          [+ button(Button, Function)]         (pullright)
%
%       Syntax:
%
%               Char [=+] Category(Args)

emacs_expansion((:- emacs_begin_mode(Mode, Super, Summary, Bindings, Syntax)),
                [(:- pce_begin_class(PceMode, PceSuper, Summary)),
                 (:- pce_class_directive(emacs_extend:emacs_mode_bindings(Mode,
                                                             Module,
                                                             Bindings,
                                                             Syntax)))
                ]) :-
    emacs_mode_class(Mode, PceMode),
    emacs_mode_class(Super, PceSuper),
    prolog_load_context(module, Module).
emacs_expansion((:- emacs_extend_mode(Mode, Bindings)),
                [(:- pce_extend_class(PceMode)),
                 (:- pce_class_directive(emacs_extend:emacs_mode_bindings(Mode,
                                                             Module,
                                                             Bindings,
                                                             [])))
                ]) :-
    emacs_mode_class(Mode, PceMode),
    prolog_load_context(module, Module).
emacs_expansion((:- emacs_end_mode), (:- pce_end_class)).

%!  emacs_mode_bindings(+Mode, +Module, +Bindings, +Syntax)

:- public emacs_mode_bindings/4. % called from code expanded by emacs_expansion/2.

emacs_mode_bindings(Mode, Module, Bindings, Syntax) :-
    emacs_mode_class(Mode, PceClass),
    get(@pce, convert, PceClass, class, ClassObject),
    get(ClassObject, super_class, SuperClass),
    get(SuperClass, name, SuperName),
    emacs_mode_class(SuperMode, SuperName),
    new(KB, emacs_key_binding(Mode, SuperMode)),
    new(MM, emacs_mode_menu(Mode, SuperMode)),
    (   get(@syntax_tables, member, Mode, ST)
    ->  true
    ;   new(ST, syntax_table(Mode, SuperMode))
    ),
    make_bindings(Bindings, Module, KB, MM),
    send(KB, apply_preferences),
    make_syntax(Syntax, ST).

make_bindings([], _, _, _).
make_bindings([Selector = Term|Rest], Module, KB, MM) :-
    bind(Term, Selector, Module, KB, MM),
    make_bindings(Rest, Module, KB, MM).

make_syntax([], _).
make_syntax([S|Rest], ST) :-
    syntax(S, ST),
    make_syntax(Rest, ST).

bind(key(Key), Selector, _, KB, _) :-
    send(KB, function, Key, Selector).
bind(-button(Button), Selector, _, _, MM) :-
    send(MM, delete, Button, Selector).
bind(button(Button), Selector, _, _, MM) :-
    send(MM, append, Button, Selector).
bind(button(Button, Func), Selector, Module, _, MM) :-
    send(MM, Module:append(Button, emacs_argument_item(Selector, Func))).
bind(A+B, Selector, Module, KB, MM) :-
    bind(A, Selector, Module, KB, MM),
    bind(B, Selector, Module, KB, MM).

syntax(Char = Term, ST) :-
    Term =.. TermArgs,
    Msg =.. [syntax, Char | TermArgs],
    send(ST, Msg).
syntax(Char + Term, ST) :-
    Term =.. TermArgs,
    Msg =.. [add_syntax, Char | TermArgs],
    send(ST, Msg).
syntax(paragraph_end(End), ST) :-
    (   is_list(End)
    ->  atomic_list_concat(End, '|', Regex)
    ;   Regex = End
    ),
    send(ST, paragraph_end, Regex).
syntax(sentence_end(End), ST) :-
    (   is_list(End)
    ->  atomic_list_concat(End, '|', Regex)
    ;   Regex = End
    ),
    send(ST, sentence_end, Regex).
syntax(quasi_quotation(Start, End), ST) :-
    send(ST, quasi_quotation_start, Start),
    send(ST, quasi_quotation_end, End).
syntax(prolog, ST) :-
    send(ST, prolog, @on).


                 /*******************************
                 *             UTIL             *
                 *******************************/

%!  emacs_mode_class(?ModeName, ?ClassName)
%
%   Convert between plain PceEmacs modename and the mode class.

emacs_mode_class(@default, emacs_mode) :- !.
emacs_mode_class([], emacs_mode) :- !.
emacs_mode_class(ModeName, ClassName) :-
    atom(ModeName),
    !,
    atomic_list_concat([emacs_, ModeName, '_mode'], ClassName).
emacs_mode_class(ModeName, ClassName) :-
    atom_concat(emacs_, M0, ClassName),
    atom_concat(ModeName, '_mode', M0),
    !.
emacs_mode_class(@default, emacs_mode).


                 /*******************************
                 *         REGISTRATION         *
                 *******************************/

:- multifile
    user:pce_pre_expansion_hook/2.
:- dynamic
    user:pce_pre_expansion_hook/2.

user:pce_pre_expansion_hook(In, Out) :-
    emacs_expansion(In, Out).
