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

:- module(pack_info,
	  [ update_pack_metadata/0,
	    update_pack_metadata_in_background/0,
	    pack_file_hierarchy//1,		% +Pack
	    pack_readme//1,			% +Pack
	    pack_file_details/3,		% +Pack, +File, +Options
	    clean_pack_info/1,			% +Pack
	    pack_archive/3			% ?Pack, ?Hash, ?Archive
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_html),
	      [ doc_for_file/2			% other imports conflict
	      ]).				% with doc_wiki
:- use_module(library(pldoc/doc_htmlsrc)).
:- use_module(library(prolog_xref)).
:- use_module(pack_analyzer).
:- use_module(pack_mirror).
:- use_module(pack).
:- use_module(wiki).


/** <module> Visual (web) components that show info about packs
*/

		 /*******************************
		 *	   COLLECT INFO		*
		 *******************************/

:- dynamic
	pack_archive/3,			% ?Pack, ?Hash, ?Archive
	pack_file/4,			% ?Pack, ?File, ?Info, ?XrefID
	xreffed_pack/2.

%%	update_pack_metadata is det.
%%	update_pack_metadata_in_background is det.
%
%	Destroy     and     recompute     all       pack      meta-data.
%	update_pack_metadata_in_background/0 runs update_pack_metadata/0
%	in a detached thread.

update_pack_metadata :-
	setup_call_cleanup(
	    ( open('log/pack-warnings.log', write, ErrorOut),
	      asserta((user:thread_message_hook(_Term, Kind, Lines) :-
		        (   must_print(Kind)
			->  print_message_lines(ErrorOut, kind(Kind), Lines)
			;   true
			)))
	    ),
	    ( clean_pack_metadata,
	      mirror_packs,
	      xref_packs
	    ),
	    close(ErrorOut)).

must_print(warning).
must_print(error).

clean_pack_metadata :-
	retractall(pack_archive(_,_,_)),
	forall(retract(pack_file(_,_,_,XrefID)),
	       (   xref_current_source(XrefID)
	       ->  xref_clean(XrefID)
	       ;   true
	       )),
	retractall(xreffed_pack(_,_)).

update_pack_metadata_in_background :-
	thread_create(update_pack_metadata, _,
		      [ detached(true),
			alias(update_pack_metadata)
		      ]).

%%	mirror_packs
%
%	Mirror the latest versions of all known packs

mirror_packs :-
	forall(pack(Pack), mirror_pack(Pack)).

%%	mirror_pack(+Pack)
%
%	Process a pack, collecting  the   relevant  information into the
%	(local) Prolog database. Automatically reprocesses   the pack if
%	the pack has been upgraded.

mirror_pack(Pack) :-
	pack_mirror(Pack, ArchiveFile, Hash),
	absolute_file_name(ArchiveFile, ArchivePath),
	(   pack_archive(Pack, Hash, ArchivePath)
	->  true
	;   clean_pack_info(Pack),
	    pack_members(ArchivePath, Members),
	    maplist(assert_file_info(Pack, ArchivePath), Members),
	    assertz(pack_archive(Pack, Hash, ArchivePath))
	), !.
mirror_pack(Pack) :-
	print_message(warning, pack(mirror_failed(Pack))).

assert_file_info(Pack, ArchivePath, file(File, Size)) :-
	(   pack_prolog_entry(File)
	->  directory_file_path(ArchivePath, File, XrefID),
	    assertz(pack_file(Pack, File, file(Size), XrefID))
	;   assertz(pack_file(Pack, File, file(Size), -))
	).
assert_file_info(Pack, _, link(File, Target)) :-
	assertz(pack_file(Pack, File, link(Target), -)).

%%	clean_pack_info(+Pack)
%
%	Remove the collected info for Pack

clean_pack_info(Pack) :-
	retractall(pack_archive(Pack,_,_)),
	forall(retract(pack_file(Pack, _, _, XrefID)),
	       (   XrefID == (-)
	       ->  true
	       ;   xref_clean(XrefID)
	       )).

%%	xref_packs
%
%	Cross-reference all mirrored packs

xref_packs :-
	forall(pack_archive(Pack, _Hash, Archive),
	       ( debug(pack(xref), 'Cross-referencing pack ~w', [Pack]),
		 ensure_xref_pack(Archive))).

ensure_xref_pack(Pack) :-
	xreffed_pack(Pack, _), !.
ensure_xref_pack(Pack) :-
	xref_pack(Pack),
	get_time(Time),
	asserta(xreffed_pack(Pack, Time)).


		 /*******************************
		 *	     VISUALS		*
		 *******************************/

%%	pack_file_hierarchy(+Pack)// is det.
%
%	Create a =ul= for all files that   appear  in the pack. Maybe we
%	should consider a tree-styled nested =ul=?

pack_file_hierarchy(Pack) -->
	html(h2(class(wiki), 'Contents of pack "~w"'-[Pack])),
	{ mirror_pack(Pack),
	  pack_archive(Pack, _Hash, Archive),
	  ensure_xref_pack(Archive),
	  findall(File, pack_file(Pack, File, _Size, _XrefID), Files),
	  files_to_tree(Files, Trees)
	},
	pack_size(Pack),
	html_requires(css('ul_tree.css')),
	html(div(class('pack-files'),
		 ul(class(tree),
		    \dir_nodes(Pack, Trees)))).

pack_size(Pack) -->
	{ aggregate_all(
	      sum(Size)-count,
	      pack_file(Pack, _Name, file(Size), _XrefID),
	      Total-Count)
	},
	html(p([ 'Pack contains ', \n('~D', Count), ' files holding a total of ',
		 b(\n(human, Total)), ' bytes.'
	       ])).

dir_nodes(_, []) --> [].
dir_nodes(Pack, [H|T]) --> dir_node(H, Pack), dir_nodes(Pack, T).

dir_node(leaf(File), Pack) --> !,
	html(li(class(file), \pack_file_link(Pack, File))).
dir_node(tree(Dir, SubTrees), Pack) -->
	html(li(class(dir),
		[ span(class(dir), Dir),
		  ul(class(dir),
		     \dir_nodes(Pack, SubTrees))
		])).

pack_file_link(Pack, File) -->
	{ file_base_name(File, Label),
	  http_link_to_id(pack_file_details, [], HREF0),
	  atomic_list_concat([HREF0, Pack, File], /, HREF)
	},
	html(a(href(HREF), Label)),
	file_hierarchy_info(Pack, File).

file_hierarchy_info(Pack, File) -->
	{ pack_file(Pack, File, file(Size), XrefID)
	}, !,
	html(span(class('file-tree-info'),
		 [ '(', \n(human, Size), ' bytes',
		   \prolog_file_info(Pack, File, XrefID),
		   ')'
		 ])).
file_hierarchy_info(_,_) --> [].

prolog_file_info(_, _, -) --> !.
prolog_file_info(_Pack, File, XrefID) -->
	module_info(File, XrefID).

module_info(File, XrefID) -->
	{ xref_module(XrefID, Module), !,
	  file_base_name(File, Base),
	  file_name_extension(Clean, _, Base)
	},
	(   {Module == Clean}
	->  []
	;   html(span(class('module-mismatch'), Module))
	).
module_info(_, _) -->
	html([', ', span(class(warning), 'not a module')]).


%%	files_to_tree(+Files:list(atom), -Tree) is det.
%
%	Creates a tree from a list of file names.  A tree is a term
%
%	  * tree(Dir, SubTrees)
%	  * leaf(File)

files_to_tree(Files, Tree) :-
	map_list_to_pairs(path_of, Files, Pairs),
	keysort(Pairs, Sorted),
	make_tree(Sorted, Tree).

path_of(File, Segments) :-
	atomic_list_concat(Segments, /, File).

make_tree([], []).
make_tree([H|T], [Node|More]) :-
	first_path(H, HS, Dir),
	(   HS = []-File
	->  Node = leaf(File),
	    Rest = T
	;   Node = tree(Dir, SubTrees),
	    same_first_path(T, Dir, TS, Rest),
	    make_tree([HS|TS], SubTrees)
	),
	make_tree(Rest, More).

first_path([Dir|Sub]-File, Sub-File, Dir).

same_first_path([], _, [], []) :- !.
same_first_path([H|T], Dir, [HS|TS], Rest) :-
	first_path(H, HS, Dir), !,
	same_first_path(T, Dir, TS, Rest).
same_first_path(Rest, _, [], Rest).


%%	n(+Format, +Value)//
%
%	HTML component to emit a number.

n(Fmt, Value) -->
	{ number_html(Fmt, Value, HTML) },
	html(HTML).

number_html(human, Value, HTML) :-
	integer(Value), !,
	human_count(Value, HTML).
number_html(Fmt, Value, HTML) :-
	number(Value), !,
	HTML = Fmt-[Value].
number_html(_, Value, '~p'-[Value]).


human_count(Number, HTML) :-
	Number < 1024, !,
	HTML = '~d'-[Number].
human_count(Number, HTML) :-
	Number < 1024*1024, !,
	KB is Number/1024,
	digits(KB, N),
	HTML = '~*fK'-[N, KB].
human_count(Number, HTML) :-
	Number < 1024*1024*1024, !,
	MB is Number/(1024*1024),
	digits(MB, N),
	HTML = '~*fM'-[N, MB].
human_count(Number, HTML) :-
	TB is Number/(1024*1024*1024),
	digits(TB, N),
	HTML = '~*fG'-[N, TB].

digits(Count, N) :-
	(   Count < 100
	->  N = 1
	;   N = 0
	).

%%	pack_readme(+Pack)//
%
%	Insert readme information if provided.

pack_readme(Pack) -->
	{ pack_readme_file(Pack, File, Size) },
	pack_readme(Pack, File, Size).

pack_readme(_Pack, File, Size) -->
	{ MaxSize = 50000,
	  Size > MaxSize
	}, !,
	html(p(class(warning),
	       'Readme file ~w too large (~D bytes; maximum size is ~D)'-
	       [File, Size, MaxSize])).
pack_readme(Pack, File, _) -->
	{ pack_archive(Pack, _, Archive),
	  format(atom(FileURL), '~w/~w', [Archive, File]),
	  setup_call_cleanup(
	      pack_open_entry(Archive, File, Stream),
	      read_stream_to_codes(Stream, String),
	      close(Stream)),
	  setup_call_cleanup(
	      b_setval(pldoc_file, FileURL),
	      wiki_codes_to_dom(String, [], DOM),
	      nb_delete(pldoc_file))
	},
	html(DOM).

pack_readme_file(Pack, Readme, Size) :-
	mirror_pack(Pack),
	pack_file(Pack, Readme, file(Size), -),
	downcase_atom(Readme, Key),
	readme_file(Key).

readme_file(readme).
readme_file('readme.txt').
readme_file('readme.md').

%%	pack_file_details(+Pack, +File, +Options) is det.
%
%	Reply with an web-page with details on File in Pack.  Options:
%
%	  * show(+Show)
%	  One of =doc=, =src=, =raw=
%	  * public_only(+Bool)
%
%	@tbd	Is rendering files without checking them a good idea?

pack_file_details(Pack, _File, _Options) :-
	mirror_pack(Pack),
	pack_archive(Pack, _Hash, Archive),
	ensure_xref_pack(Archive),
	fail.
pack_file_details(Pack, File, Options) :-
	pack_file(Pack, File, file(_Size), XrefID),
	XrefID \== (-),
	option(show(Show), Options, doc),
	(   Show == doc
	->  !,
	    format(atom(Title), 'Pack ~w -- ~w', [Pack, File]),
	    doc_for_file(XrefID,
			 [ title(Title),
			   edit(false)
			 ])
	;   Show == src
	->  !,
	    pack_archive(Pack, _Hash, Archive),
	    directory_file_path(Archive, File, Path),
	    format('Content-type: text/html~n~n'),
	    source_to_html(Path, stream(current_output), [])
	).
pack_file_details(Pack, File, _Options) :-
	pack_file(Pack, File, file(Size), -),
	file_base_name(File, Base),
	downcase_atom(Base, BaseLwr),
	wiki_file(BaseLwr), !,
	format(atom(Title), 'Pack ~w -- ~w', [Pack, File]),
	reply_html_page(
	    pack(text, Title),
	    title(Title),
	    \pack_readme(Pack, File, Size)).
pack_file_details(Pack, File, _Options) :-
	pack_file(Pack, File, file(_Size), -),
	pack_archive(Pack, _Hash, Archive),
	file_mime_type(File, MimeType),
	format('Content-type: ~w~n~n', [MimeType]),
	setup_call_cleanup(
	    pack_open_entry(Archive, File, Stream),
	    copy_stream_data(Stream, current_output),
	    close(Stream)).

wiki_file(readme).
wiki_file(todo).
wiki_file(Name) :- file_name_extension(_, md, Name).
wiki_file(Name) :- file_name_extension(_, txt, Name).

:- multifile
	plweb:page_title//1.

plweb:page_title(pack(_Type, Title)) -->
	html(Title).
