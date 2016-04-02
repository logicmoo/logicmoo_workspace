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

:- module(called_from, [called_from/1,
			called_from/2,
			called_from/5,
			collect_called_from/5,
			collect_called_from/6,
			current_called_from/5,
			current_used_from/6,
			used_predicates/2,
			used_predicates/3
		       ]).

:- use_module(library(implementation_module)).
:- use_module(library(assrt_lib)).
:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(extra_codewalk)).
:- use_module(library(extra_location)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).

:- multifile
    prolog:message//1.

:- dynamic called_from_db/5.

prolog:message(acheck(called_from(MsgLoc, Args))) -->
	MsgLoc,
	['~w called from ~w'-Args].

called_from(Ref) :-
	called_from(Ref, _).

called_from(Ref, Caller) :-
    ( called_from(Ref, _CM, Caller, [], Sorted),
      maplist(print_call_point, Sorted),
      fail
    ; cleanup_loc_dynamic(_, _, dynamic(_, _, _), _),
      retractall(called_from_db(_, _, _, _, _))
    ).

called_from(Ref, CM, Caller, OptionL, Pairs) :-
    normalize_head(Ref, M:H),
    collect_called_from(H, M, CM, Caller, OptionL, Pairs).

collect_called_from(H, M, CM, Caller, OptionL, Sorted) :-
    collect_called_from(H, M, CM, Caller, OptionL),
    findall(Loc-[M:F/A, CPI], ( current_called_from(H, M, CM, From, C),
				functor(H, F, A),
				normalize_pi(C, CPI),
				from_location(From, Loc)
			      ), Pairs),
    keysort(Pairs, Sorted).

collect_called_from(Ref, M, CM, Caller, OptionL0) :-
    cleanup_loc_dynamic(_, _, dynamic(_, _, _), _),
    retractall(called_from_db(_, _, _, _, _)),
    merge_options([source(true),
		   infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_:Ref),
		   module_class([user, system, library]),
		   on_etrace(collect_call_point(M, CM, Caller))],
		  OptionL0, OptionL),
    extra_walk_code(OptionL, M, _).

current_called_from(H, M, CM, From, Caller) :-
    current_used_from([retract, query], H, M, CM, From, Caller).

current_used_from(DynTypes, H, M, CM, From, Caller) :-
    ( called_from_db(H, M, CM, Caller, From)
    ; loc_dynamic(H, M, dynamic(Type, CM, Caller), From),
      memberchk(Type, DynTypes)
    ; loc_declaration(H, CM, goal, From),
      implementation_module(CM:H, M)
    ; asr_head_prop(_, CM, H, _, _, _, From),
      implementation_module(CM:H, M),
      Caller = '<assertion>'(M:H)
    ).

:- public collect_call_point/6.
collect_call_point(IM, M, Caller, MGoal, Caller, From) :-
    ignore(record_location_dynamic(MGoal, IM, From)),
    MGoal = M:Goal,
    implementation_module(MGoal, IM),
    update_fact_from(called_from_db(Goal, IM, M, Caller), From).

print_call_point(L-A) :-
    print_message(information, acheck(called_from(L, A))).

% used_predicates(+Module, +Context, -PIL) is det
%
% Unifies PIL with a list of predicates implemented in the module Module,
% actually being used in the context Context.  Note that this would be different
% than the imported predicates.
%
used_predicates(Module, Context, PIL) :-
    collect_called_from(_, Module, Context, _, [source(false)]),
    findall(F/A,
	    ( current_called_from(H, Module, Context, _, _),
	      functor(H, F, A)
	    ), PIU),
    sort(PIU, PIL).

used_predicates(Module, Groups) :-
    collect_called_from(_, Module, _, _, [source(false)]),
    findall(Context-(F/A),
	    ( current_called_from(H, Module, Context, _, _),
	      functor(H, F, A)
	    ), Pairs),
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Groups).
