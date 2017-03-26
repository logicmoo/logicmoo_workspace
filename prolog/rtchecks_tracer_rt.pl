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

:- module(rtchecks_tracer_rt, [rtcheck_call/1]).

:- use_module(library(ontrace)).
:- use_module(library(rtchecks_rt)).

:- meta_predicate
    rtcheck_call(0).

rtcheck_call(Call) :-
    rtcheck_pause(rtcheck_goal(Call, rtcheck_start)).

:- meta_predicate rtcheck_start(0).
rtcheck_start(Call) :-
    call_inoutex(Call, trace, nodebug).

:- meta_predicate rtcheck_pause(0).
rtcheck_pause(Call) :-
    call_inoutex(Call, nodebug, trace).
