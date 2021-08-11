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

:- module(markitup,
	  [ markitup//1
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(option)).
:- use_module(markdown).
:- use_module(wiki).

/** <module> Wrapper for markItUp ajax markup editor

@see http://markitup.jaysalvat.com/home/
*/

:- http_handler(root('markitup/preview/markdown'), preview_markdown, []).
:- http_handler(root('markitup/preview/pldoc'),    preview_pldoc,    []).

:- html_resource(js('markitup/jquery.markitup.js'),
		 [ requires([ jquery
			    ])
		 ]).
:- html_resource(js('markitup/sets/markdown/set.js'),
		 [ requires([ js('markitup/jquery.markitup.js'),
			      js('markitup/skins/markitup/style.css'),
			      js('markitup/sets/markdown/style.css')
			    ])
		 ]).
:- html_resource(markdown,
		 [ virtual(true),
		   requires([ js('markitup/sets/markdown/set.js')
			    ])
		 ]).
:- html_resource(js('markitup/sets/pldoc/set.js'),
		 [ requires([ js('markitup/jquery.markitup.js'),
			      js('markitup/skins/markitup/style.css'),
			      js('markitup/sets/pldoc/style.css')
			    ])
		 ]).
:- html_resource(pldoc,
		 [ virtual(true),
		   requires([ js('markitup/sets/pldoc/set.js')
			    ])
		 ]).

%%	markitup(Options)// is det.
%
%	Insert a =textarea= with markItUp support.

markitup(Options) -->
	{ option(markup(Language), Options, markdown),
	  option(id(Id), Options, markdown),
	  option(name(Name), Options, Id),
	  option(cols(Cols), Options, 80),
	  option(rows(Rows), Options, 20),
	  option(value(Content), Options, []),
	  option(preview(Preview), Options, false)
	},
	html_requires(Language),
	html(textarea([id(Id), name(Name), cols(Cols), rows(Rows)], Content)),
	js_script({|javascript(Id,Language,Preview)||
		   $(document).ready(function() {
		      $("#"+Id).markItUp(eval(Language+"_settings"));
		      if ( eval(Preview) ) {
			$('a[title="Preview"]').trigger("mouseup");
		      }
		    });
		  |}).


%%	preview_markdown(+Request)
%
%	Handle preview requests from markItUp.  The data is send using
%	a POST request, where the =data= field contains the content of
%	the textarea.

preview_markdown(Request) :-
	http_parameters(Request,
			[ data(Data, [optional(true), default('')])
			]),
	debug(markitup(preview), 'Preview:~n~w~n', [Data]),
	open_atom_stream(Data, In),
	markdown_dom(stream(In), DOM),
	phrase(html(DOM), Tokens),
	format('Content-type: text/html; charset=UTF-8\n\n'),
	print_html(Tokens).

%%	preview_pldoc(+Request)
%
%	Handle preview requests from markItUp.  The data is send using
%	a POST request, where the =data= field contains the content of
%	the textarea.

preview_pldoc(Request) :-
	http_parameters(Request,
			[ data(Data, [optional(true), default('')])
			]),
	debug(markitup(preview), 'Preview:~n~w~n', [Data]),
	atom_codes(Data, Codes),
	wiki_file_codes_to_dom(Codes, '/', DOM), % FIXME: What file to pass?
	phrase(page(plain, [], [\html_requires(pldoc)|DOM]), Tokens),
	format('Content-type: text/html; charset=UTF-8\n\n'),
	print_html(Tokens).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

open_atom_stream(Atom, Stream) :-
	atom_to_memory_file(Atom, MF),
	open_memory_file(MF, read, Stream,
			 [ free_on_close(true)
			 ]).
