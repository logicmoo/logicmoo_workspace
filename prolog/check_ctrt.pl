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

:- module(check_ctrt, 
	  [checkif_modl/5,
           is_prop_check/5,
           collect_assertions/4,
	   current_assertion/4]).

:- use_module(library(apply)).
:- use_module(library(assrt_lib)).
:- use_module(library(clambda)).
:- use_module(library(rtcprops)).

:- meta_predicate checkif_modl(?, ?, 0, ?, 0).
checkif_modl(M, M,    _,    _, Goal) :- !, call(Goal).
checkif_modl(_, _, GMod, Goal, Goal) :- call(GMod).

is_prop_check(Step, Level, T, Part, Asr) :-
    part_time(Part, T),
    asr_aprop(Asr, type, Type,   _),
    asr_aprop(Asr, stat, Status, _),
    assrt_op(Part, Step, Level, Type),
    is_valid_status_type(Status, Type), !.

part_time(call, _).
part_time(succ, _).
part_time(glob, rt).

%!  assrt_op(+Part, +Step, +Level, +Type).

assrt_op(call, step1, _,       entry).
assrt_op(call, step1, exports, calls).
assrt_op(call, step1, exports, pred).
assrt_op(call, step1, exports, prop).
assrt_op(call, step2, inner,   calls).
assrt_op(call, step2, inner,   pred).
assrt_op(call, step2, inner,   prop).
assrt_op(succ, step1, _,       exit).
assrt_op(succ, step1, exports, success).
assrt_op(succ, step1, exports, pred).
assrt_op(succ, step1, exports, prop).
assrt_op(succ, step2, inner,   success).
assrt_op(succ, step2, inner,   pred).
assrt_op(succ, step2, inner,   prop).
assrt_op(glob, step1, exports, comp).
assrt_op(glob, step1, exports, pred).
assrt_op(glob, step1, exports, prop).
assrt_op(glob, step2, inner,   comp).
assrt_op(glob, step2, inner,   pred).
assrt_op(glob, step2, inner,   prop).

is_valid_status_type(true, entry) :- current_prolog_flag(rtchecks_entry, yes).
is_valid_status_type(Status, Type) :-
        rtcheck_assr_type(Type),
        rtcheck_assr_status(Status).

rtcheck_assr_status(true)  :- current_prolog_flag(rtchecks_true,  yes).
rtcheck_assr_status(trust) :- current_prolog_flag(rtchecks_trust, yes).
rtcheck_assr_status(debug) :- current_prolog_flag(rtchecks_debug, yes).
rtcheck_assr_status(trace) :- current_prolog_flag(rtchecks_trace, yes).
rtcheck_assr_status(check) :- current_prolog_flag(rtchecks_check, yes).

rtcheck_assr_type(calls).
rtcheck_assr_type(pred).
rtcheck_assr_type(prop).
rtcheck_assr_type(comp).
rtcheck_assr_type(exit) :- current_prolog_flag(rtchecks_exit, yes).
rtcheck_assr_type(success).

/*
  Combination of status and rtcheck indicators, to control the compile
  and run-time checking:

  ===========+============++===========+==========
  Status     | + Command  || ctchecked | rtchecked
  ===========+============++===========+==========
  true/trust | -          || no        | no
  true/trust | rtcheck    || no        | yes
  check      | no_rtcheck || yes       | no
  check      | -          || yes       | yes
  ===========+============++===========+==========
  Note: This is weird to preserve ciao-compatibility
*/

black_list_pred(_=_).

assertion_is_valid(rt, Status, Type, Asr) :-
    ( \+ prop_asr(glob, rtcheck(_), _, Asr)
    ->is_valid_status_type(Status, Type),
      \+ prop_asr(glob, no_rtcheck(_), _, Asr)
    ; true % Force run-time checking
    ).
assertion_is_valid(ct, _, _, _).

current_assertion(T, Pred, M, Asr) :-
    \+ ( T = rt,
         prop_asr(Pred, M, _, prop, _, _, _)
       ),
    prop_asr(Pred, M, Status, Type, _, _, Asr),
    assertion_is_valid(T, Status, Type, Asr),
    ( current_prolog_flag(rtchecks_level, inner)
    ->true
    ; current_prolog_flag(rtchecks_level, exports),
      predicate_property(M:Pred, export)
    ->true
    ),
    \+ black_list_pred(Pred).

collect_assertions(T, Pred, M, AsrL) :-
    copy_term_nat(Pred, Head), % copy to avoid duplication of atributes
    findall(Head-Asr, current_assertion(T, Head, M, Asr), Pairs),
    maplist([Pred] +\ (Pred-T)^T^true, Pairs, AsrL).
