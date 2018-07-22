/*  Part of Run-Time Checker for Assertions

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(rtchecks,
          [(rtchecked)/1,

           op(1150, fx, rtchecked)
          ]).

:- reexport(library(compound_expand)).
:- use_module(library(rtcprops), []).
:- use_module(library(implementation_module)).
:- use_module(system:library(rtchecks_rt)).

rtchecked(PlList) :-
    throw(error(context_error(nodirective, rtcheck(PlList)), _)).

:- multifile
    prolog:rename_predicate/2.

wrappers(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
wrappers((A,B)) -->
    !,
    wrappers(A),
    wrappers(B).
wrappers(Name//Arity) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      Arity1 is Arity+2
    },
    wrappers(Name/Arity1).
wrappers(Name/Arity) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      functor(Head, Name, Arity),
      atom_concat(Name, ' rtchecked', WrapName),
      Head =.. [Name|Args],
      WrappedHead =.. [WrapName|Args],
      prolog_load_context(module, Module),
      ( implementation_module(Module:Head, Module),
        \+ predicate_property(Module:Head, multifile),
        \+ predicate_property(Module:Head, dynamic)
      ->Head2 = Head,
        Goal2 = WrappedHead,
        Name2 = Name,
        Level = head
      ; Head2 = WrappedHead,
        Goal2 = Head,
        Name2 = WrapName,
        Level = body
      )
    },
    ( { predicate_property(Module:Head, exported),
        Level = body
      }
    ->[(:- export(Name2/Arity))]
    ; []
    ),
    [ '$rtchecked'(Head, Level),
      (:- module_transparent Name2/Arity),
      (Head2 :-
           start_rtcheck(Module:Head, Goal2)
      )
    ].

%!  prolog:rename_predicate(:Head1, :Head) is semidet.
%
%   Hook into term_expansion for  post   processing  renaming of the
%   generated predicate.

prolog:rename_predicate(M:Head1, M:Head) :-
    '$defined_predicate'(M:'$rtchecked'(_, _)),
    call(M:'$rtchecked'(Head1, head)),
    !,
    rename_term(Head1, Head).

rename_term(Compound1, Compound) :-
    compound(Compound1),
    !,
    compound_name_arguments(Compound1, Name, Args),
    atom_concat(Name, ' rtchecked', WrapName),
    compound_name_arguments(Compound, WrapName, Args).
rename_term(Name, WrapName) :-
    atom_concat(Name, ' rtchecked', WrapName).

term_expansion((:- rtchecked(Preds)),
               [ (:- discontiguous('$rtchecked'/2)),
                 (:- public '$rtchecked'/2)
                 | Clauses
               ]) :-
    phrase(wrappers(Preds), Clauses).

goal_expansion(Goal1, Pos, '$with_ploc'(Goal, From),
               term_position(0, 0, 0, 0, [Pos, _])) :-
    prolog_load_context(module, M),
    implementation_module(M:Goal1, IM),
    '$defined_predicate'(IM:'$rtchecked'(_, _)),
    call(IM:'$rtchecked'(Goal1, body)),
    rename_term(Goal1, Goal),
    source_location(File, Line),
    ( nonvar(Pos)
    ->From = file_term_position(File, Pos)
    ; From = file(File, Line, -1, _)
    ).

:- multifile
    sandbox:safe_directive/1.

%!  sandbox:safe_directive(+Directive) is semidet.
%
%   Allow rtchecks directives that affect locally defined predicates.

sandbox:safe_directive(Dir) :-
    ground(Dir),
    local_rtchecks_dir(Dir).

local_rtchecks_dir(rtchecked(Preds)) :-
    local_preds(Preds).

local_preds((A,B)) :-
    !,
    local_preds(A),
    local_preds(B).

local_preds(Name/Arity) :-
    atom(Name), integer(Arity).
local_preds(Name//Arity) :-
    atom(Name), integer(Arity).
