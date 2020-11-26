/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2012, University of Amsterdam
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

:- module(pce_realise,
          [ pce_register_class/1,       % +ClassName
            pce_extended_class/1,       % +ClassName
            pce_realise_class/1,        % +ClassName
            pce_begin_class_definition/4,% +ClassName, +Meta, +Super, +Summary
            pce_prolog_class/1,         % ?ClassName
            pce_prolog_class/2          % ?ClassName, ?SuperName
          ]).

:- use_module(pce_boot(pce_principal)).
:- use_module(pce_boot(pce_global)).
:- require([ ignore/1
           , pce_error/1
           , call/3
           , last/2
           ]).

:- pce_global(@class, new(var(class, class, @nil))).

                 /*******************************
                 *            REGISTER          *
                 *******************************/

pce_register_class(ClassName) :-
    check_loaded_class(ClassName).


                 /*******************************
                 *       EXTENDING CLASSES      *
                 *******************************/

pce_extended_class(ClassName) :-
    get(@classes, member, ClassName, Class),
    !,
    attach_class_attributes(ClassName),
    send(Class, clear_cache),
    resolve_method_message(Msg),
    send(Class, resolve_method_message, Msg).
pce_extended_class(_).


%!  pce_begin_class_definition(+ClassName, +Meta, +Super, +Summary)
%
%   Dummy to allow cross-referencing and other tools to locate
%   class-definitions after term-expansion.

pce_begin_class_definition(_,_,_,_).


                 /*******************************
                 *            RELOAD            *
                 *******************************/

%!  check_loaded_class(+ClassName)
%
%   If the class is already defined, we are dealing with redefinition
%   and have to take action immediately.

check_loaded_class(ClassName) :-
    get(@classes, member, ClassName, _),
    !,
    pce_realise_class(ClassName).
check_loaded_class(_).


                 /*******************************
                 *        REALISE-CLASS         *
                 *******************************/

%!  pce_realise_class(+ClassName)
%   Creates `ClassName' from the compiled representation.

pce_realise_class(ClassName) :-
    pce_class(ClassName, MetaClassName, SuperName, _, _, _),
    MetaClassName \== (-),
    create_class(ClassName, MetaClassName, SuperName, Class),
    !,
    resolve_method_message(Msg),
    send(Class, resolve_method_message, Msg),
    attach_class_attributes(ClassName),
    (   cache_table(TableName),
        get(Class, slot, TableName, Table),
        get(Table, size, Size),
        Size > 0
    ->  delete_prolog_methods(Class)
    ;   true
    ),
    ignore(get(Class, send_method, in_event_area, _)). % HACK!

cache_table(send_table).
cache_table(get_table).
cache_table(send_methods).
cache_table(get_methods).

attach_class_attributes(ClassName) :-
    get(@classes, member, ClassName, Class),
    pce_class(ClassName, _, _,
              Variables,
              ClassVariables,
              Directives),
    attach_variables(Variables, Class),
    attach_class_variables(ClassVariables, Class),
    run_directives(Directives, Class),
    fail ; true.


%!  pce_prolog_class(?ClassName:atom) is nondet.
%!  pce_prolog_class(?ClassName:atom, ?SuperName:atom) is nondet.
%
%   Is true if ClassName refers to a class defined in Prolog with
%   the given superclass.

pce_prolog_class(ClassName) :-
    pce_prolog_class(ClassName, _SuperName).
pce_prolog_class(ClassName, SuperName) :-
    pce_class(ClassName, _MetaClassName, SuperName,
              _Variables,
              _ClassVariables,
              _Directives),
    SuperName \== '-'.              % extended class


%!  create_class(+ClassName, +MetaClassName, +SuperName, -Class)
%
%   Creates class `ClassName' below `SuperName'.  Succeeds
%   (for redefinition) if the class already existed with the
%   same super-class.

create_class(ClassName, MetaClassName, Super, Class) :-
    get(@classes, member, ClassName, Class),
    send(Class, instance_of, class),
    !,
    get(Class, super_class, SuperClass),
    (   (   Super == @nil,
            SuperClass == @nil
        ;   SuperClass \== @nil,
            get(SuperClass, name, Super)
        )
    ->  true
    ;   pce_error(superclass_not_changed(ClassName))
    ),
    (   (   MetaClassName == @default
        ;   get(Class, class, MetaClass),
            get(MetaClass, name, MetaClassName)
        )
    ->  true
    ;   pce_error(metaclass_not_changed(ClassName))
    ),
    send(Class, clear_cache).
create_class(ClassName, MetaClassName, SuperName, Class) :-
    (   get(@pce, convert, SuperName, class, Super)
    ->  true
    ;   pce_error(superclass_not_exist(SuperName, ClassName))
    ),
    (   MetaClassName == @default
    ->  get(Super, sub_class, ClassName, Class)
    ;   Term =.. [MetaClassName, ClassName, Super],
        new(Class, Term)
    ).


%!  attach_variables(+VariableList, +Class)
%
%   Attach the instance variables.  Error checking is done by the
%   XPCE kernel.

attach_variables([], _).
attach_variables([V|T], Class) :-
    catch(send(Class, instance_variable, V), E,
          pce_error(E)),
    attach_variables(T, Class).

%!  attach_class_variables(+ClassVarList, +Class)
%
%   Attach the class variables

attach_class_variables([], _).
attach_class_variables([R|T], Class) :-
    attach_class_variable(Class, R),
    attach_class_variables(T, Class).

attach_class_variable(Class, M:class_variable(Name, Def, Type, Summary)) :-
    !,
    classvar_default(Def, PceDef),
    new(_, M:class_variable(Class, Name, PceDef, Type, Summary)).
attach_class_variable(Class, ClassVar) :-
    attach_class_variable(Class, user:ClassVar).

%!  classvar_default(+DefaultSpec, -Default) is det.
%
%   Allow environment specific defaults. If   DefaultSpec is a list,
%   it may hold terms   windows(WindowsDefault),  'X'(X11Default) or
%   apple(AppleDefault). The system is considered   =apple=  if @pce
%   has  window_system  =X=  and    <-operating_system  matches  the
%   substring  =darwin=.  If  apple(AppleDefault)    is  not  found,
%   'X'(X11Default) is tried.

classvar_default(List, Default) :-
    is_list(List),
    !,
    (   get(@pce, window_system, 'X'),
        get(@pce, operating_system, OS),
        sub_atom(OS, _, _, _, darwin),
        memberchk(apple(AppleDefault), List)
    ->  Default = AppleDefault
    ;   get(@pce, window_system, WS),
        Term =.. [WS,Default],
        memberchk(Term, List)
    ->  true
    ;   List = [H|_],
        compound(H),
        H =.. [_, Default]
    ->  true
    ;   throw(error(type_error(class_variable_default, List), _))
    ).
classvar_default(Default, Default).


run_directives([], _).
run_directives(Directives, Class) :-
    send(@class, assign, Class),
    run_directives(Directives).

run_directives([]).
run_directives([H|T]) :-
    H,
    run_directives(T).

delete_prolog_methods(Class) :-
    get(Class, name, ClassName),
    (   pce_lazy_send_method(Selector, ClassName, _Binder1),
        send(Class, delete_send_method, Selector),
        fail
    ;   pce_lazy_get_method(Selector, ClassName, _Binder2),
        send(Class, delete_get_method, Selector),
        fail
    ;   true
    ).


                 /*******************************
                 *    LAZY METHOD RESOLUTION    *
                 *******************************/

%!  resolve_method_message(-Message)
%
%   Create the @pce_resolve_method_message that is called by XPCE
%   whenever there is a method to be resolved.

resolve_method_message(X) :-
    X = @pce_resolve_method_message,
    (   object(X)
    ->  true
    ;   new(X, message(@prolog, call, '_bind_lazy', @arg1, @arg2, @arg3))
    ).

pce_ifhostproperty(prolog(swi),
                   (:- '$hide'('_bind_lazy'/3))).

pce_ifhostproperty(prolog(swi),
('_bind_lazy'(Type, ClassName, Selector) :-
%       format('bind_lazy(~p, ~p, ~p)~n', [Type, ClassName, Selector]),
        notrace(do_bind_lazy(Type, ClassName, Selector))),
('_bind_lazy'(Type, ClassName, Selector) :-
        do_bind_lazy(Type, ClassName, Selector))).

do_bind_lazy(send, ClassName, @default) :-
    !,
    get(@pce, convert, ClassName, class, Class),
    (   send_binder(Selector, ClassName, Binder),
        \+ send(Class, bound_send_method, Selector),
        call_binder(ClassName, Selector, Binder),
        fail ; true
    ).
do_bind_lazy(send, ClassName, Selector) :-
    send_binder(Selector, ClassName, Binder),
    call_binder(ClassName, Selector, Binder).
do_bind_lazy(get, ClassName, @default) :-
    !,
    get(@pce, convert, ClassName, class, Class),
    (   get_binder(Selector, ClassName, Binder),
        \+ send(Class, bound_get_method, Selector),
        call_binder(ClassName, Selector, Binder),
        fail ; true
    ).
do_bind_lazy(get, ClassName, Selector) :-
    get_binder(Selector, ClassName, Binder),
    call_binder(ClassName, Selector, Binder).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This deals with possible redefined methods.  We distinguish two types of
`legal' method redefinition: using pce_extend_class/1  and redefining an
implementation inherited from a template.

Other cases are reported by checkpce/0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

send_binder(Sel, Class, Binder) :-
    bagof(B, pce_lazy_send_method(Sel, Class, B), Binders),
    last(Binders, Binder).
get_binder(Sel, Class, Binder) :-
    bagof(B, pce_lazy_get_method(Sel, Class, B), Binders),
    last(Binders, Binder).

call_binder(ClassName, Selector, Binder) :-
    build_in_binder(Binder, ClassName, Selector),
    !.
call_binder(ClassName, Selector, Binder) :-
    call(Binder, ClassName, Selector).

build_in_binder(bind_send(Id, T, D, L, G), C, S) :-
    !,
    pce_bind_send(Id, T, D, L, G, C, S).
build_in_binder(bind_send(Id, T, D, L), C, S) :-
    !,
    pce_bind_send(Id, T, D, L, @default, C, S).
build_in_binder(bind_send(Id, T, D), C, S) :-
    !,
    pce_bind_send(Id, T, D, @default, @default, C, S).
build_in_binder(bind_send(Id, T), C, S) :-
    !,
    pce_bind_send(Id, T, @default, @default, @default, C, S).

build_in_binder(bind_get(Id, R, T, D, L, G), C, S) :-
    !,
    pce_bind_get(Id, R, T, D, L, G, C, S).
build_in_binder(bind_get(Id, R, T, D, L), C, S) :-
    !,
    pce_bind_get(Id, R, T, D, L, @default, C, S).
build_in_binder(bind_get(Id, R, T, D), C, S) :-
    !,
    pce_bind_get(Id, R, T, D, @default, @default, C, S).
build_in_binder(bind_get(Id, R, T), C, S) :-
    !,
    pce_bind_get(Id, R, T, @default, @default, @default, C, S).


pce_bind_send(Id, Types, Doc, Loc, Group, ClassName, Selector) :-
    get(@pce, convert, ClassName, class, Class),
    pce_method_implementation(Id, Message),
    send(Class, send_method,
         send_method(Selector, Types, Message, Doc, Loc, Group)).

pce_bind_get(Id, RType, Types, Doc, Loc, Group, ClassName, Selector) :-
    get(@pce, convert, ClassName, class, Class),
    pce_method_implementation(Id, Message),
    send(Class, get_method,
         get_method(Selector, RType, Types, Message, Doc, Loc, Group)).

