/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (C): 2020, SWI-Prolog Solutions b.v.

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

:- module(fastly,
          [ purge_location/1            % +Location
          ]).
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).

:- use_module(parms).

/** <module> Purge pages on our CDN
*/

:- multifile
    http_open:map_method/2.

http_open:map_method(purge, 'PURGE').

%!  purge_location(+Location) is det.
%
%   Send a purge request for a Fastly URL

purge_location(Location) :-
    compound(Location),
    !,
    http_absolute_location(Location, Path, []),
    purge_location(Path).
purge_location(Location) :-
    server(cdn, Server),
    format(string(URL), 'https://~w~w', [Server, Location]),
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(purge)
                  ]),
        json_read_dict(In, Reply),
        close(In)),
    (   Reply.get(status) == "ok"
    ->  true
    ;   print_message(warning, fastly(purge(Reply)))
    ).

