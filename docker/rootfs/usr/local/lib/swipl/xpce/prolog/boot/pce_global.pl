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

:- module(pce_global,
        [ pce_global/2                            % Ref x Goal
        ]).

:- meta_predicate
      pce_global(+, :).

:- use_module(pce_boot(pce_principal)).

:- require([strip_module/3, gensym/2, append/3]).

:- dynamic
    'pce global goal'/3,                      % Ref, Module, Goal
    'pce catcher'/2.                          % Module, Goal


                /********************************
                *            USER PART          *
                ********************************/

%!  pce_global(+Object, :Goal) is det.
%
%   Register Goal to be a goal that creates @Reference

pce_global(@Ref, MGoal) :-
    strip_module(MGoal, Module, Goal),
    global(Ref, Module, Goal).

global(Ref, Module, Goal) :-
    var(Ref),
    !,
    retractall('pce catcher'(Module, Goal)),
    asserta('pce catcher'(Module, Goal)).
global(Ref, Module, Goal) :-                      % just reconsult
    'pce global goal'(Ref, Module, Goal),
    !,
    (   Goal = new(_)
    ->  true
    ;   reload_global(@Ref)
    ).
global(Ref, Module, Goal) :-
    'pce global goal'(Ref, Module, _),       % definition changed
    !,
    reload_global(@Ref),
    retractall('pce global goal'(Ref, Module, _)),
    asserta('pce global goal'(Ref, Module, Goal)).
global(Ref, _M1, new(Term)) :-                    % same definition
    'pce global goal'(Ref, _M2, new(Term)),
    !.
global(Ref, M1, G1) :-
    'pce global goal'(Ref, M2, G2),
    !,
    print_message(warning, object_already_defined(Ref, M2)),
    retractall('pce global goal'(Ref, M2, G2)),
    asserta('pce global goal'(Ref, M1, G1)).
global(Ref, Module, Goal) :-
    asserta('pce global goal'(Ref, Module, Goal)).

reload_global(Ref) :-
    object(Ref),
    !,
    (   get(Ref, references, 0)
    ->  free(Ref)
    ;   Ref = @Name,
        gensym(Name, NewName),
        send(Ref, name_reference, NewName),
        print_message(informational, renamed_reference(Name, NewName))
    ).
reload_global(_).


                /********************************
                *            SYSTEM             *
                ********************************/

register_handler :-
    send(@pce?exception_handlers,
         append(attribute(undefined_assoc,
                          message(@prolog, call, trap_ref, @arg1)))).

:- initialization
    register_handler.

trap_ref(Ref) :-
    'pce global goal'(Ref, Module, Goal),
    !,
    (   Goal = new(Term)
    ->  (   new(@Ref, Module:Term)
        ->  true
        ;   print_message(error, create_failed(Term)),
            trace,
            fail
        )
    ;   Goal =.. List,
        append(List, [@Ref], GoalList),
        GoalTerm =.. GoalList,
        (   Module:GoalTerm
        ->  true
        ;   print_message(error, make_global_failed(Module:GoalTerm)),
            trace,
            fail
        )
    ).
trap_ref(Ref) :-
    'pce catcher'(Module, Goal),
    call(Module:Goal, Ref).
