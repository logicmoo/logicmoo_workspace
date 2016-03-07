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
:- use_module(library(rtchecks_basic)).
:- use_module(library(rtchecks_gen)).
:- use_module(library(location_utils)).

:- create_prolog_flag(assrt_meta_pred, none, [type(atom)]).

% Extends assertion_db/12 to get assertions from meta predicate declarations.

assrt_lib:asr_glob(am_idx(M, Head), assrt_meta, rtc_stub(RTChecks, Goal)) :-
    am_head_prop_idx(Head, M, Status, Spec, Pos),
    Pred = M:Head,
    normalize_assertion_head(Spec, M, _, Pred, Comp, Call, Succ, Glob, _),
    current_prolog_flag(rtchecks_namefmt, NameFmt),
    get_pretty_names(NameFmt, n(Head, Comp, Call, Succ, Glob), [], TName),
    TName = n(HeadName, CompName, CallName, SuccName, GlobName),
    AssrL = [assr(Head, Status, (pred), Comp, Call, Succ, Glob, Pos, HeadName, CompName, CallName, SuccName, GlobName)],
    generate_rtchecks(AssrL, M, RTChecksL, G, G, Goal),
    lists_to_lits(RTChecksL, RTChecks).

assrt_lib:head_prop_asr(Head, M, Status, (comp), "", [], Pos, am_idx(M, Head)) :-
    am_head_prop_idx(Head, M, Status, _, Pos).

am_head_prop_idx(Head, M, Status, Spec, Pos) :-
    current_prolog_flag(assrt_meta_pred, Status),
    Status \= none,
    Pred = M:Head,
    ( var(Head)
    ->current_predicate(M:F/A),
      functor(Head, F, A)
    ; functor(Head, F, A),
      current_predicate(M:F/A) % Narrow answer set for M
    ),
    \+ predicate_property(Pred, imported_from(_)),
    % if something can not be debugged, can not be rtchecked either
    \+ predicate_property(Pred, nodebug),
    '$predicate_property'(meta_predicate(Spec), Pred),
    % predicate_property(Pred, meta_predicate(Spec)),
    ( property_from(M:Spec, meta_predicate, Pos)
    ->true
    ; predicate_from(Pred, Pos)
    ),
    assertion(nonvar(Pos)).

:- true prop rtc_stub/3.
:- meta_predicate rtc_stub(0,0,?).
:- public rtc_stub/3.

rtc_stub(Goal, RTChecks, Goal) :-
    call(RTChecks).
