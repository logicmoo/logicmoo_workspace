/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(assrt_meta, []).

:- use_module(library(assertions)).
:- use_module(library(assertions_op)).
:- use_module(library(location_utils)).
:- use_module(library(implementation_module)).
:- use_module(library(tabling)).

:- create_prolog_flag(assrt_meta_pred, none, [type(atom)]).

:- meta_predicate
    with_amp(0, +, +).

:- table
    am_head_prop_idx/5.

meta_has_mode_info(Meta) :-
    arg(_, Meta, Spec),
    memberchk(Spec, [+,-]),
    !.

with_amp(Goal, OldFlag, NewFlag) :-
    setup_call_cleanup(set_prolog_flag(assrt_meta_pred, NewFlag),
                       Goal,
                       set_prolog_flag(assrt_meta_pred, OldFlag)).

am_head_prop_idx(Head, M, Meta, From) :-
    current_prolog_flag(assrt_meta_pred, Flag),
    Flag \= none,
    copy_term_nat(Head, Term),
    with_amp(am_head_prop_idx(Flag, Term, M, Meta, From), Flag, none),
    Head = Term.

am_head_prop_idx(Flag, Head, M, Meta, From) :-
    var(Meta), !,
    Pred = M:Head,
    ( var(Head)
    ->module_property(M, class(user)),
      current_predicate(M:F/A),
      functor(Head, F, A)
    ; functor(Head, F, A),
      module_property(M, class(user)),
      current_predicate(M:F/A) % Narrow answer set for M
    ),
    \+ predicate_property(Pred, imported_from(_)),
    % if something can not be debugged, can not be rtchecked either:
    \+ predicate_property(Pred, nodebug),
    '$predicate_property'(meta_predicate(Meta), Pred),
    % predicate_property(Pred, meta_predicate(Meta)),
    meta_has_mode_info(Meta),
    ( Flag = all
    ->
      \+ ( asr_head_prop(Asr, CM, Head, check, _, _, _),
           implementation_module(CM:Head, M),
           asr_glob(Asr, CM, no_meta_modes(_), _)
         )
    ; Flag = specific
    ->once(( asr_head_prop(Asr, CM, Head, check, _, _, _),
             implementation_module(CM:Head, M),
             asr_glob(Asr, CM, meta_modes(_), _)
           ))
    ),
    ( property_from(M:Pred, meta_predicate, From),
      From \= []
    ->true
    ; predicate_from(Pred, From)
    ),
    assertion(nonvar(From)).
am_head_prop_idx(_, _, _, _, _).

assrt_lib:asr_head_prop(am_asr(M, H, S, F), M, H, check, (comp), [], F) :-
    am_head_prop_idx(H, M, S, F).
assrt_lib:asr_glob(am_asr(M, H, S, F), assrt_meta,
                   rtcheck_goal(_, [am_asr2(M, H, S, F)]), F) :-
    am_head_prop_idx(H, M, S, F).

assrt_lib:asr_aprop(am_asr2(M, H, _, From), head,   M:H, From).
assrt_lib:asr_aprop(am_asr2(_, _, _, From), stat, check, From).
assrt_lib:asr_aprop(am_asr2(_, _, _, From), type,  pred, From).
assrt_lib:asr_aprop(am_asr2(M, H, Meta, From), Type,  Prop, From) :-
    (nonvar(Type) -> memberchk(Type, [call, succ]) ; true),
    assrt_lib:current_normalized_assertion(pred Meta, M, _, M:H, _,
                                           _, _, CaL, SuL, _, _, _, _),
    member(Type-PropL, [call-CaL, succ-SuL]),
    member(Prop-_, PropL).
