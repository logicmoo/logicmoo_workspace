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

:- module(check_undefined, []).

% A wrapper from library(check)
:- use_module(assertions(assrt_lib)).
:- use_module(checkers(checker)).
:- use_module(library(prolog_codewalk)).
:- use_module(xlibrary(infer_alias)).
:- use_module(xlibrary(normalize_pi)).
:- use_module(xtools(extra_codewalk)).
:- use_module(xtools(location_utils)).
:- use_module(xtools(referenced_by)).

:- multifile
    prolog:message//1.

:- dynamic
    undef/3.

checker:check(undefined, Results, OptionL) :-
    check_undefined(OptionL, Results).

check_undefined(OptionL0, Pairs) :-
    extra_wcsetup([trace_reference(-), undefined(trace)|OptionL0],
		  OptionL, FromChk),
    ignore(option(module(M), OptionL)),
    prolog_walk_code([on_trace(collect_undef(M, FromChk))|OptionL]),
    decl_walk_code(extra_undef(M, FromChk), M),
    found_undef_assr(M, FromChk),
    findall(warning-(PIAL-(Loc/['~w'-[CI]])),
	    ( retract(undef(PI, CI, From)),
	      find_alternatives(PI, AL),
	      PIAL=PI/AL,
	      from_location(From, Loc)
	    ), Pairs).

extra_undef(M, FromChk, M:Head, Caller, From) :-
    functor(Head, F, A),
    \+ current_predicate(M:F/A),
    collect_undef(M, FromChk, M:Head, Caller, From).

hide_undef(M:H) :- hide_undef(H, M).

find_alternatives(M:F/A, AL) :-
    functor(H, F, A),
    findall(AA, ( current_predicate(AM:F/A),
		  AM \= M,
		  \+ predicate_property(AM:H, imported_from(_)),
		  ( module_property(AM, file(AF))
		  ->( library_alias(AF, AA)
		    ->true
		    ; AA = AF
		    )
		  ; AA=AM
		  )
		), AU),
    sort(AU, AL).

% Hook to hide undef messages:
:- multifile hide_undef/2.
hide_undef(assertion_head(_,_,_,_,_,_,_), assrt_lib).

found_undef(To, Caller, From) :-
    normalize_pi(To, PI),
    normalize_pi(Caller, CI),
    ( hide_undef(To) -> true
    ; undef(PI, CI, From) -> true
    ; assertz(undef(PI, CI, From))
    ).

found_undef_assr(M, FromChk) :-
    forall(( assertion_head_body_loc(Head, M, _, _, _, _, _, _, From),
	     functor(Head, F, A),
	     \+ current_predicate(M:F/A),
	     call(FromChk, From)),
	   found_undef(M:Head, assrt_lib:assertion_head/7, From)).

:- public collect_undef/5.
:- meta_predicate collect_undef(?,1,+,+,+).
collect_undef(M, FromChk, MCall, Caller, From) :-
    call(FromChk, From),
    M:_ = MCall,
    found_undef(MCall, Caller, From),
    fail. % prevent unexpected unification

prolog:message(acheck(undefined)) -->
    ['--------------------',nl,
     'Undefined Predicates',nl,
     '--------------------',nl],
    prolog:message(check(undefined_predicates)).
prolog:message(acheck(undefined, PIAL-LocCIList)) -->
    { PIAL = PI/AL
    ->true
    ; PI = PIAL,
      AL = []
    },
    [ '~w undefined, '-[PI]],
    show_alternatives(AL),
    [ 'referenced by', nl ],
    referenced_by(LocCIList).

show_alternatives([]) --> !.
show_alternatives(AL) --> ['but modules ~w have definitions for it, '-[AL]].
