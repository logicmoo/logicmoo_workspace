/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, VU University Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(http_watchdog, []).
:- use_module(library(http/http_exception), []).
:- autoload(library(broadcast), [broadcast/1]).
:- autoload(library(aggregate), [aggregate_all/3]).

:- multifile http:map_exception_to_http_status_hook/4.

http:map_exception_to_http_status_hook(error(resource_error(Which),_), _, _, _) :-
    outof(Which),
    fail.

:- dynamic
    outof/2.

outof(Which) :-
    get_time(Now),
    Del is Now - 3600,
    forall(( outof(Which, Then),
             Then < Del
           ),
           retractall(outof(Which, Then))),
    aggregate_all(count, outof(Which, _), Count),
    Count > 3,
    !,
    broadcast(http(watchdog(Which))),
    halt(1).
outof(Which) :-
    get_time(Now),
    asserta(outof(Which, Now)).

