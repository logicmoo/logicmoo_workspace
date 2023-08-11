/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2014, University of Amsterdam
			      VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(pldoc_index,
	  [ doc_for_dir/2,              % +Dir, +Options
	    dir_index//2,               % +Dir, +Options, //
	    object_summaries//3,        % +Objs, +Section, +Options, //
	    file_index_header//2,       % +File, +Options, //
	    doc_links//2,               % +Directory, +Options, //
	    doc_file_href/2,            % +File, -HREF
	    places_menu//1,             % +Dir, //
	    source_directory/1          % ?Directory
	  ]).
:- use_module(doc_process).
:- use_module(doc_html).
:- use_module(doc_wiki).
:- use_module(doc_search).
:- use_module(doc_util).

:- if(exists_source(library(doc_http))).
:- use_module(library(http/http_dispatch)).
:- use_module(library(doc_http), []).
:- endif.

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(readutil)).
:- use_module(library(url)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- include(hooks).

/** <module> Create indexes
*/

:- predicate_options(dir_index//2, 2,
		     [ directory(atom),
		       edit(boolean),
		       files(list),
		       members(list),
		       qualify(boolean),
		       title(atom),
		       if(oneof([true,loaded])),
		       recursive(boolean),
		       secref_style(oneof([number, title, number_title])),
		       pass_to(doc_links/4, 2)
		     ]).
:- predicate_options(doc_links//2, 2,
		     [ files(list),
		       pass_to(pldoc_search:search_form/3, 1)
		     ]).
:- predicate_options(file_index_header//2, 2,
		     [ directory(any),
		       files(list),
		       qualify(boolean),
		       secref_style(oneof([number, title, number_title])),
		       pass_to(pldoc_html:edit_button/4, 2),
		       pass_to(pldoc_html:source_button/4, 2)
		     ]).
:- predicate_options(object_summaries//3, 3,
		     [ edit(boolean),
		       files(list),
		       module(atom),
		       public(list),
		       qualify(boolean),
		       secref_style(oneof([number, title, number_title]))
		     ]).
:- predicate_options(doc_for_dir/2, 2, [pass_to(dir_index/4, 2)]).

%!  doc_for_dir(+Dir, +Options) is det.
%
%   Write summary index for all files  in   Dir  to  Out. The result
%   consists of the =README= file  (if   any),  a  table holding with
%   links to objects and summary  sentences   and  finaly the =TODO=
%   file (if any).

doc_for_dir(DirSpec, Options) :-
    absolute_file_name(DirSpec,
		       [ file_type(directory),
			 access(read)
		       ],
		       Dir),
    (   option(title(Title), Options)
    ->  true
    ;   file_base_name(Dir, Title)
    ),
    doc_write_page(
	pldoc(dir_index),
	title(Title),
	\dir_index(Dir, Options),
	Options).

:- html_meta doc_write_page(+, html, html, +).

doc_write_page(Style, Head, Body, Options) :-
    option(files(_), Options),
    !,
    phrase(page(Style, Head, Body), HTML),
    print_html(HTML).
doc_write_page(Style, Head, Body, _) :-
    reply_html_page(Style, Head, Body).


%!  dir_index(+Dir, +Options)//
%
%   Create an index for all Prolog files appearing in Dir or in
%   any directory contained in Dir.  Options:
%
%     * members(+Members)
%     Documented members.  See doc_files.pl
%     * title(+Title)
%     Title to use for the index page

dir_index(Dir, Options) -->
    { dir_source_files(Dir, Files0, Options),
      sort(Files0, Files),
      maplist(ensure_doc_objects, Files),
      directory_file_path(Dir, 'index.html', File),
      b_setval(pldoc_file, File)    % for predref
    },
    html([ \doc_resources(Options),
	   \doc_links(Dir, Options),
	   \dir_header(Dir, Options),
	   \subdir_links(Dir, Options),
	   h2(class([wiki,plfiles]), 'Prolog files'),
	   table(class(summary),
		 \file_indices(Files, [directory(Dir)|Options])),
	   \dir_footer(Dir, Options)
	 ]).

%!  dir_source_files(+Dir, -Files, +Options) is det
%
%   Create a list of source-files to be documented as part of Dir.

dir_source_files(_, Files, Options) :-
    option(members(Members), Options),
    !,
    findall(F, member(file(F,_Doc), Members), Files).
dir_source_files(Dir, Files, Options) :-
    directory_source_files(Dir, Files, Options).

%!  subdir_links(+Dir, +Options)// is det.
%
%   Create links to subdirectories

subdir_links(Dir, Options) -->
    { option(members(Members), Options),
      findall(SubDir, member(directory(SubDir, _, _, _), Members), SubDirs),
      SubDirs \== []
    },
    html([ h2(class([wiki,subdirs]), 'Sub directories'),
	   table(class(subdirs),
		 \subdir_link_rows(SubDirs, Dir))
	 ]).
subdir_links(_, _) --> [].

subdir_link_rows([], _) --> [].
subdir_link_rows([H|T], Dir) -->
    subdir_link_row(H, Dir),
    subdir_link_rows(T, Dir).

subdir_link_row(Dir, From) -->
    { directory_file_path(Dir, 'index.html', Index),
      relative_file_name(Index, From, Link),
      file_base_name(Dir, Base)
    },
    html(tr(td(a([class(subdir), href(Link)], ['[dir] ', Base])))).

%!  dir_header(+Dir, +Options)// is det.
%
%   Create header for directory.  Options:
%
%     * readme(File)
%     Include File as introduction to the directory header.

dir_header(Dir, Options) -->
    wiki_file(Dir, readme, Options),
    !.
dir_header(Dir, Options) -->
    { (   option(title(Title), Options)
      ->  true
      ;   file_base_name(Dir, Title)
      )
    },
    html(h1(class=dir, Title)).

%!  dir_footer(+Dir, +Options)// is det.
%
%   Create footer for directory. The footer contains the =TODO= file
%   if provided.  Options:
%
%     * todo(File)
%     Include File as TODO file in the footer.

dir_footer(Dir, Options) -->
    wiki_file(Dir, todo, Options),
    !.
dir_footer(_, _) -->
    [].

%!  wiki_file(+Dir, +Type, +Options)// is semidet.
%
%   Include text from a Wiki text-file.

wiki_file(Dir, Type, Options) -->
    { (   Opt =.. [Type,WikiFile],
	  option(Opt, Options)
      ->  true
      ;   directory_files(Dir, Files),
	  member(File, Files),
	  wiki_file_type(Type, Pattern),
	  downcase_atom(File, Pattern),
	  directory_file_path(Dir, File, WikiFile)
      ),
      access_file(WikiFile, read),
      !,
      read_file_to_codes(WikiFile, String, []),
      wiki_codes_to_dom(String, [], DOM)
    },
    pldoc_html:html(DOM).

%!  wiki_file_type(+Category, -File) is nondet.
%
%   Declare file pattern names that are included for README and TODO
%   for a directory. Files are matched case-insensitively.

wiki_file_type(readme, 'readme').
wiki_file_type(readme, 'readme.md').
wiki_file_type(readme, 'readme.txt').
wiki_file_type(todo,   'todo').
wiki_file_type(todo,   'todo.md').
wiki_file_type(todo,   'todo.txt').

%!  file_indices(+Files, +Options)// is det.
%
%   Provide a file-by-file index of the   contents of each member of
%   Files.

file_indices([], _) -->
    [].
file_indices([H|T], Options) -->
    file_index(H, Options),
    file_indices(T, Options).

%!  file_index(+File, +Options)// is det.
%
%   Create an index for File.

file_index(File, Options) -->
    { doc_summaries(File, Objs0),
      module_info(File, ModuleOptions, Options),
      doc_hide_private(Objs0, Objs1, ModuleOptions),
      sort(Objs1, Objs)
    },
    html([ \file_index_header(File, Options)
	 | \object_summaries(Objs, File, ModuleOptions)
	 ]).

doc_summaries(File, Objects) :-
    xref_current_source(FileSpec),
    xref_option(FileSpec, comments(collect)),
    !,
    Pos = File:0,
    findall(doc(Obj,Pos,Summary),
	    xref_doc_summary(Obj, Pos, Summary), Objects).
doc_summaries(File, Objects) :-
    Pos = File:_Line,
    findall(doc(Obj,Pos,Summary),
	    doc_comment(Obj, Pos, Summary, _), Objects).

xref_doc_summary(M:Name/Arity, File:_, Summary) :-
    xref_comment(File, Head, Summary, _Comment),
    xref_module(File, Module),
    strip_module(Module:Head, M, Plain),
    functor(Plain, Name, Arity).

%!  file_index_header(+File, +Options)// is det.
%
%   Create an entry in a summary-table for File.

file_index_header(File, Options) -->
    prolog:doc_file_index_header(File, Options),
    !.
file_index_header(File, Options) -->
    { (   option(directory(Dir), Options),
	  directory_file_path(Dir, Label, File)
      ->  true
      ;   file_base_name(File, Label)
      ),
      doc_file_href(File, HREF, Options)
    },
    html(tr(th([colspan(3), class(file)],
	       [ span(style('float:left'), a(href(HREF), Label)),
		 \file_module_title(File),
		 span(style('float:right'),
		      [ \source_button(File, Options),
			\edit_button(File, Options)
		      ])
	       ]))).

file_module_title(File) -->
    { (   module_property(M, file(File))
      ;   xref_module(File, M)
      ),
      doc_comment(M:module(Title), _, _, _)
    },
    !,
    html([&(nbsp), ' -- ', Title]).
file_module_title(_) -->
    [].


%!  doc_file_href(+File, -HREF, +Options) is det.
%
%   HREF is reference to documentation of File.

doc_file_href(File, HREF, Options) :-
    option(directory(Dir), Options),
    atom_concat(Dir, Local0, File),
    atom_concat(/, Local, Local0),
    !,
    (   option(files(Map), Options),        % generating files
	memberchk(file(File, DocFile), Map)
    ->  file_base_name(DocFile, HREF)
    ;   HREF = Local
    ).
doc_file_href(File, HREF, _) :-
    doc_file_href(File, HREF).



%!  doc_file_href(+Path, -HREF) is det.
%
%   Create a /doc HREF from Path.  There   are  some nasty things we
%   should take care of.
%
%           * Windows paths may start with =|L:|= (mapped to =|/L:|=)
%           * Paths may contain spaces and other weird stuff

doc_file_href(File0, HREF) :-
    insert_alias(File0, File),
    ensure_slash_start(File, SlashFile),
    http_location([path(SlashFile)], Escaped),
    http_location_by_id(pldoc_doc, DocRoot),
    atom_concat(DocRoot, Escaped, HREF).


%!  ensure_slash_start(+File0, -File) is det.
%
%   Ensure  File  starts  with  a  /.    This  maps  C:/foobar  into
%   /C:/foobar, so our paths start with /doc/ again ...

ensure_slash_start(File, File) :-
    sub_atom(File, 0, _, _, /),
    !.
ensure_slash_start(File0, File) :-
    atom_concat(/, File0, File).


%!  object_summaries(+Objects, +Section, +Options)// is det.
%
%   Create entries in a summary table for Objects.

object_summaries(Objects, Section, Options) -->
    { tag_pub_priv(Objects, Tagged, Options),
      keysort(Tagged, Ordered)
    },
    obj_summaries(Ordered, Section, Options).

obj_summaries([], _, _) -->
    [].
obj_summaries([_Tag-H|T], Section, Options) -->
    object_summary(H, Section, Options),
    obj_summaries(T, Section, Options).

tag_pub_priv([], [], _).
tag_pub_priv([H|T0], [Tag-H|T], Options) :-
    (   private(H, Options)
    ->  Tag = z_private
    ;   Tag = a_public
    ),
    tag_pub_priv(T0, T, Options).


%!  object_summary(+Object, +Section, +Options)// is det
%
%   Create a summary for Object.  Summary consists of a link to
%   the Object and a summary text as a table-row.
%
%   @tbd    Hacky interface.  Do we demand Summary to be in Wiki?

object_summary(q(_Q,Obj), Section, Options) -->
    { nonvar(Obj) },
    !,
    object_summary(Obj, Section, Options).
object_summary(doc(Obj, _Pos, _Summary), wiki, Options) -->
    !,
    html(tr(class(wiki),
	    [ td(colspan(3), \object_ref(Obj, Options))
	    ])).
object_summary(doc(Obj, _Pos, Summary), _Section, Options) -->
    !,
    (   { string_codes(Summary, Codes),
	  wiki_codes_to_dom(Codes, [], DOM0),
	  strip_leading_par(DOM0, DOM),
	  (   private(Obj, Options)
	  ->  Class = private               % private definition
	  ;   Class = public                % public definition
	  )
	}
    ->  html(tr(class(Class),
		[ td(\object_ref(Obj, Options)),
		  td(class(summary), DOM),
		  td([align(right)],
		     span(style('white-space: nowrap'),
			  [ \object_source_button(Obj, Options),
			    \object_edit_button(Obj, Options)
			  ]))
		]))
    ;   []
    ).
object_summary(Obj, Section, Options) -->
    { prolog:doc_object_summary(Obj, _Cat, Section, Summary)
    },
    !,
    object_summary(doc(Obj, _, Summary), Section, Options).
object_summary(_, _, _) -->
    [].


		 /*******************************
		 *          NAVIGATION          *
		 *******************************/

%!  doc_links(+Directory, +Options)// is det.
%
%   Provide overview links and search facilities.

doc_links(_Directory, Options) -->
    { option(files(_), Options), !
    }.
doc_links(Directory, Options) -->
    prolog:doc_links(Directory, Options),
    !,
    { option(html_resources(Resoures), Options, pldoc) },
    html_requires(Resoures).
doc_links(Directory, Options) -->
    {   (   Directory == ''
	->  working_directory(Dir, Dir)
	;   Dir = Directory
	),
	option(html_resources(Resoures), Options, pldoc)
    },
    html([ \html_requires(Resoures),
	   div(class(navhdr),
	       [ div(class(jump),
		      div([ \places_menu(Dir),
			    \plversion
			  ])),
		 div(class(search), \search_form(Options)),
		 br(clear(right))
	       ])
	 ]).


%!  version// is det.
%
%   Prolog version

plversion -->
    { current_prolog_flag(version_data, swi(Major, Minor, Patch, _))
    },
    !,
    html(a([ class(prolog_version),
	     href('http://www.swi-prolog.org')
	   ],
	   [' SWI-Prolog ', Major, '.', Minor, '.', Patch])).

plversion -->
    { current_prolog_flag(version_data, yap(Major, Minor, Patch, _))
    },
    html(a([ class(prolog_version),
	     href('http://www.dcc.fc.up.pt/~vsc')
	   ],
	   [' YAP ', Major, '.', Minor, '.', Patch])).


%!  places_menu(Current)// is det
%
%   Create a =select= menu with entries for all loaded directories

places_menu(Dir) -->
    prolog:doc_places_menu(Dir),
    !.
places_menu(Dir) -->
    { findall(D, source_directory(D), List),
      sort(List, Dirs)
    },
    html(form([ action(location_by_id(go_place))
	      ],
	      [ input([type(submit), value('Go')]),
		select(name(place),
		       \packs_source_dirs(Dirs, Dir))
	      ])).

packs_source_dirs(Dirs, Dir) -->
    packs_link,
    source_dirs(Dirs, Dir).

source_dirs([], _) -->
    [].
source_dirs([H|T], WD) -->
    { (   H == WD
      ->  Attrs = [selected]
      ;   Attrs = []
      ),
      format(string(IndexFile), '~w/index.html', [H]),
      doc_file_href(IndexFile, HREF),
      format(string(Call), 'document.location=\'~w\';', [HREF])
    },
    html(option([onClick(Call)|Attrs], H)),
    source_dirs(T, WD).

packs_link -->
    { pack_property(_,_),
      !,
      http_link_to_id(pldoc_pack, [], HREF),
      format(atom(Call), 'document.location=\'~w\';', [HREF])
    },
    html(option([ class(packs),
		  onClick(Call),
		  value(':packs:')
		],
		'List extension packs')).
packs_link -->
    [].

%!  source_directory(+Dir) is semidet.
%!  source_directory(-Dir) is nondet.
%
%   True if Dir is a directory  from   which  we  have loaded Prolog
%   sources.

source_directory(Dir) :-
    (   ground(Dir)
    ->  '$time_source_file'(File, _Time1, _System1),
	file_directory_name(File, Dir), !
    ;   '$time_source_file'(File, _Time2, _System2),
	file_directory_name(File, Dir)
    ).
