/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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

:- module(markdown,
	  [ markdown_dom/2		% +MarkDown, -DOM
	  ]).
:- use_module(library(sgml)).
:- use_module(library(process)).
:- use_module(library(error)).

/** <module> Parse markdown documents into a DOM
*/

:- create_prolog_flag(markdown_program, markdown, []).

%%	markdown_dom(+Input, -DOM) is det.
%
%	Process markdown input into an HTML  DOM structure compatible to
%	load_structure/3     and     html//1     as      provided     by
%	library(http/html_write).
%
%	@param	Input is either a term stream(+Stream) or the name of a
%		file.

markdown_dom(stream(Stream), DOM) :- !,
	must_be(stream, Stream),
	current_prolog_flag(markdown_program, Prog),
	process_create(path(Prog), [],
		       [ stdin(pipe(In)),
			 stdout(pipe(Out)),
			 process(PID)
		       ]),
	thread_create(( copy_stream_data(Stream, In),
			close(In)
		      ), _, [detached(true)]),
	load_structure(Out, DOM, [dialect(xml)]),
	process_wait(PID, _).
markdown_dom(File, DOM) :-
	current_prolog_flag(markdown_program, Prog),
	process_create(path(Prog), [file(File)],
		       [ stdout(pipe(Out)),
			 process(PID)
		       ]),
	load_structure(Out, DOM, [dialect(xml)]),
	process_wait(PID, _).

