/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.

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

:- module(rtchecks_utils,
          [handle_rtcheck/1, call_rtc/1, save_rtchecks/1, load_rtchecks/1,
           assrchk_error/1]).

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(plprops)).
:- use_module(library(prolog_codewalk),  []). % for message_location
:- use_module(library(filtered_backtrace)).
:- use_module(library(intercept)).

:- multifile
    prolog:message_location//1.

filtered_backtrace:no_backtrace_clause_hook(_, rtchecks_utils).
filtered_backtrace:no_backtrace_clause_hook(_, rtchecks_tracer).
filtered_backtrace:no_backtrace_clause_hook(_, rtchecks_rt).
filtered_backtrace:no_backtrace_clause_hook(_, intercept).
filtered_backtrace:no_backtrace_clause_hook(_, nativeprops).
filtered_backtrace:no_backtrace_clause_hook(_, send_check).
filtered_backtrace:no_backtrace_clause_hook(_, plprops).
filtered_backtrace:no_backtrace_clause_hook(_, context_values).
filtered_backtrace:no_backtrace_clause_hook('$rat_trap'(_, _, _, _, _), _).

tracertc :-
    filtered_backtrace(100).

:- doc(author, "Edison Mera").

:- doc(module, "This module contains some useful predicates to
        facilitate work with run-time checks.").

:- doc(handle_rtcheck/1, "Predicate that processes a rtcheck exception.").

:- prop location_t/1 + type.
location_t(Loc) :-
    ( clause('$messages':swi_location(Term, _, _), _)
    ; clause(prolog:message_location(Term, _, _), _)
    ),
    nonvar(Term),
    Term = Loc.

:- prop assrchk_error/1 + type #
        "Specifies the format of an assertion check error.".

assrchk_error(assrchk(Level, error(Type, _Pred, PropValues, ALoc))) :-
    rtcheck_level(Level),
    rtcheck_type(Type),
    keylist(PropValues),
    location_t(ALoc).

:- prop rtcheck_level/1 + type.

rtcheck_level(asr).
rtcheck_level(ppt(Caller, Loc)) :-
    term(Caller),
    location_t(Loc).

:- prop rtcheck_type/1 + type # "Specifies the type of run-time errors.".

rtcheck_type(comp).
rtcheck_type(pp_check).
rtcheck_type(success).
rtcheck_type(compat).
rtcheck_type(compatpos).
rtcheck_type(calls).

:- pred handle_rtcheck/1 : assrchk_error.

handle_rtcheck(RTCheck) :-
    \+ ( copy_term_nat(RTCheck, Term),
         numbervars(Term, 0, _),
         print_message(error, Term),
         fail
       ).

:- multifile
        prolog:error_message_signal//1,
        prolog:error_message//1,
        prolog:message//1.

prolog:error_message_signal(RTCheck) -->
    prolog:message(RTCheck).

prolog:error_message(unintercepted_signal(Signal)) -->
        ( prolog:error_message_signal(Signal) -> []
        ; ['unintercepted signal: ~p'-[Signal]]
        ).

prolog:message(acheck(checks, RTChecks)) -->
    foldl(prolog:message, RTChecks).

assr_level_message(asr) --> [].
assr_level_message(ppt(Caller, Loc)) -->
    '$messages':swi_location(Loc),
    ['At program point in ~q:'-[Caller], nl].

prolog:message(assrchk(Level, Error)) -->
    assr_level_message(Level),
    assr_error_message(Error).

assr_error_message(error(Type, Pred, PropValues, ALoc)) -->
    '$messages':swi_location(ALoc),
    ['Assertion failure for ~q.'-[Pred], nl],
    ['    In *~w*, unsatisfied properties: '-[Type], nl],
    foldl(prop_values, PropValues).

prop_values(From/Prop-Values) -->
    ['        '],
    '$messages':swi_location(From),
    ['~q'-[Prop]],
    ( {Values = []}
    ->['.']
    ; [', because: ~q.'-[Values]]
    ),
    [nl].

:- meta_predicate call_rtc(0).

:- pred call_rtc/1 : callable # "This predicate calls a goal and if an
        rtcheck signal is intercepted, an error message is shown and
        the execution continues. Alternatively, it is re-raised as an
        exception depending on the flag rtchecks_abort_on_error
        value.".

call_rtc(Goal) :-
        Error = assrchk(_, _),
        ( current_prolog_flag(rtchecks_abort_on_error, yes)
	->intercept(Goal, Error, throw(Error)) % rethrow signal as exception
        ; intercept(Goal, Error, (handle_rtcheck(Error), tracertc))
        ).

:- dynamic rtcheck_db/1.

:- meta_predicate save_rtchecks(0).

:- pred save_rtchecks/1 : callable # "Asserts in rtcheck_db/1 all the
        run-time check exceptions thrown by the goal.".

save_rtchecks(Goal) :-
        RTError = assrchk(_, _),
        intercept(Goal, RTError, assertz(rtcheck_db(RTError))).

:- pred load_rtchecks/1 => list(assrchk_error) # "retract the
        rtcheck_db/1 facts and return them in a list.".

load_rtchecks(RTChecks) :-
        findall(RTCheck, retract(rtcheck_db(RTCheck)), RTChecks).
