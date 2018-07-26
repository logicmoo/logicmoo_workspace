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
:- use_module(library(implementation_module)).
:- use_module(library(rtcprops), []).
:- use_module(library(ctrtchecks)).
:- use_module(system:library(rtchecks_rt)).
:- use_module(library(assrt_lib)).

rtchecked(PlList) :-
    throw(error(context_error(nodirective, rtchecked(PlList)), _)).

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
      prolog_load_context(module, Module)
    },
    ['$rtchecked'(Head, Level)],
    ( { implementation_module(Module:Head, Module),
        \+ predicate_property(Module:Head, multifile),
        \+ '$get_predicate_attribute'(Module:Head, (discontiguous), 1),
        \+ predicate_property(Module:Head, dynamic)
      }
    ->{Level = head},
      % module_transparent is required to be able to execute WrapperHead in the
      % context where Head was called from, otherwise the runtime-checks will be
      % incorrectly skipped for imported predicates if rtchecks_level is exports
      [(:- module_transparent Name/Arity),
       (Head :- start_rtcheck(Module:Head, WrappedHead))],
      ( {'$get_predicate_attribute'(Module:Head, (transparent), 1)}
      ->[(:- module_transparent WrapName/Arity)]
      ; []
      )
    ; {Level = body}
    ).

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

generate_rtchecks(Preds) :-
    phrase(( ( { '$current_source_module'(CM),
                 '$defined_predicate'(CM:'$rtchecked'(_, _))
               }
             ->[]
             ; [(:- discontiguous('$rtchecked'/2)),
                (:- public '$rtchecked'/2)]
             ),
             wrappers(Preds)
           ), Clauses),
    % We use compile_aux_clauses to make Clauses immediately available:
    compile_aux_clauses(Clauses).

term_expansion((:- rtchecked(Preds)), []) :-
    generate_rtchecks(Preds).

term_expansion(assrt_lib:asr_head_prop(_, M, Pred, Status, Type, _, _), _) :-
    \+ memberchk(Status, [check, debug]),
    Type \= (prop),
    \+ prop_asr(Pred, M, _, (prop), _, _, _),
    is_valid_status_type(Status, Type),
    \+ ( '$current_source_module'(CM),
         '$defined_predicate'(CM:'$rtchecked'(_, _)),
         CM:'$rtchecked'(Pred, _)
       ),
    functor(Pred, F, A),
    generate_rtchecks(F/A),
    fail.

rtcheck_lit_pos(P, term_position(F,T,F,T,[_,P,_])) :-
    nonvar(P),
    arg(1, P, F),
    arg(2, P, T).

:- dynamic expanding/0.
:- volatile expanding/0.

goal_expansion(Goal1, Pos1, rtcheck_lit(Level, Goal, From), Pos) :-
    \+ expanding,
    % prolog_load_context(module, M),
    '$current_source_module'(M),
    implementation_module(M:Goal1, IM),
    '$defined_predicate'(IM:'$rtchecked'(_, _)),
    call(IM:'$rtchecked'(Goal1, Level)),
    source_location(File, Line),
    setup_call_cleanup(assertz(expanding),
                       expand_goal(Goal1, Pos1, Goal, Pos2),
                       retractall(expanding)),
    ( rtcheck_lit_pos(Pos2, Pos)
    ->From = file_term_position(File, Pos1)
    ; From = file(File, Line, -1, _)
    ).

:- multifile
    prolog_clause:unify_goal/5.

prolog_clause:unify_goal(G, rtcheck_lit(_, G, _), _, P1, P) :- rtcheck_lit_pos(P1, P).

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
