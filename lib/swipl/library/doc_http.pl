/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2017, University of Amsterdam
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

:- module(pldoc_http,
          [ doc_enable/1,               % +Boolean
            doc_server/1,               % ?Port
            doc_server/2,               % ?Port, +Options
            doc_browser/0,
            doc_browser/1               % +What
          ]).
:- use_module(library(pldoc)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/mimetype)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_hook)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(www_browser)).
:- use_module(pldoc(doc_process)).
:- use_module(pldoc(doc_htmlsrc)).
:- use_module(pldoc(doc_html)).
:- use_module(pldoc(doc_index)).
:- use_module(pldoc(doc_search)).
:- use_module(pldoc(doc_man)).
:- use_module(pldoc(doc_wiki)).
:- use_module(pldoc(doc_util)).
:- use_module(pldoc(doc_access)).
:- use_module(pldoc(doc_pack)).
:- use_module(pldoc(man_index)).

/** <module> Documentation server

The module library(pldoc/http) provides an   embedded HTTP documentation
server that allows for browsing the   documentation  of all files loaded
_after_ library(pldoc) has been loaded.
*/

:- dynamic
    doc_server_port/1,
    doc_enabled/0.

http:location(pldoc, root(pldoc), []).
http:location(pldoc_man, pldoc(refman), []).
http:location(pldoc_pkg, pldoc(package), []).
http:location(pldoc_resource, Path, []) :-
    http_location_by_id(pldoc_resource, Path).

%!  doc_enable(+Boolean)
%
%   Actually activate the PlDoc server. Merely   loading the server does
%   not do so to avoid incidental loading   in a user HTTP server making
%   the documentation available.

doc_enable(true) :-
    (   doc_enabled
    ->  true
    ;   assertz(doc_enabled)
    ).
doc_enable(false) :-
    retractall(doc_enabled).

%!  doc_server(?Port) is det.
%!  doc_server(?Port, +Options) is det.
%
%   Start a documentation server in the  current Prolog process. The
%   server is started in a separate   thread.  Options are handed to
%   http_server/2.  In  addition,   the    following   options   are
%   recognised:
%
%           * allow(HostOrIP)
%           Allow connections from HostOrIP.  If HostOrIP is an atom
%           it is matched to the hostname.  It if starts with a .,
%           suffix match is done, matching the domain.  Finally it
%           can be a term ip(A,B,C,D). See tcp_host_to_address/2 for
%           details.
%
%           * deny(HostOrIP)
%           See allow(HostOrIP).
%
%           * edit(Bool)
%           Allow editing from localhost connections? Default:
%           =true=.
%
%   The predicate doc_server/1 is defined as below, which provides a
%   good default for development.
%
%   ==
%   doc_server(Port) :-
%           doc_server(Port,
%                      [ allow(localhost)
%                      ]).
%   ==
%
%   @see    doc_browser/1

doc_server(Port) :-
    doc_server(Port,
               [ allow(localhost),
                 allow(ip(127,0,0,1)) % Windows ip-->host often fails
               ]).

doc_server(Port, _) :-
    doc_enable(true),
    catch(doc_current_server(Port), _, fail),
    !.
doc_server(Port, Options) :-
    doc_enable(true),
    prepare_editor,
    host_access_options(Options, ServerOptions),
    http_absolute_location(pldoc('.'), Entry, []),
    merge_options(ServerOptions,
                  [ port(Port),
                    entry_page(Entry)
                  ], HTTPOptions),
    http_server(http_dispatch, HTTPOptions),
    assertz(doc_server_port(Port)).

%!  doc_current_server(-Port) is det.
%
%   TCP/IP port of the documentation server.   Fails if no server is
%   running. Note that in the current   infrastructure we can easily
%   be embedded into another  Prolog  HTTP   server.  If  we are not
%   started from doc_server/2, we  return  the   port  of  a running
%   HTTP server.
%
%   @tbd    Trap destruction of the server.
%   @error  existence_error(http_server, pldoc)

doc_current_server(Port) :-
    (   doc_server_port(P)
    ->  Port = P
    ;   http_current_server(_:_, P)
    ->  Port = P
    ;   existence_error(http_server, pldoc)
    ).

%!  doc_browser is det.
%!  doc_browser(+What) is semidet.
%
%   Open user's default browser on the documentation server.

doc_browser :-
    doc_browser([]).
doc_browser(Spec) :-
    catch(doc_current_server(Port),
          error(existence_error(http_server, pldoc), _),
          doc_server(Port)),
    browser_url(Spec, Request),
    format(string(URL), 'http://localhost:~w~w', [Port, Request]),
    www_open_url(URL).

browser_url([], Root) :-
    !,
    http_location_by_id(pldoc_root, Root).
browser_url(Name, URL) :-
    atom(Name),
    !,
    browser_url(Name/_, URL).
browser_url(Name//Arity, URL) :-
    must_be(atom, Name),
    integer(Arity),
    !,
    PredArity is Arity+2,
    browser_url(Name/PredArity, URL).
browser_url(Name/Arity, URL) :-
    !,
    must_be(atom, Name),
    (   man_object_property(Name/Arity, summary(_))
    ->  format(string(S), '~q/~w', [Name, Arity]),
        http_link_to_id(pldoc_man, [predicate=S], URL)
    ;   browser_url(_:Name/Arity, URL)
    ).
browser_url(Spec, URL) :-
    !,
    Spec = M:Name/Arity,
    doc_comment(Spec, _Pos, _Summary, _Comment),
    !,
    (   var(M)
    ->  format(string(S), '~q/~w', [Name, Arity])
    ;   format(string(S), '~q:~q/~w', [M, Name, Arity])
    ),
    http_link_to_id(pldoc_object, [object=S], URL).

%!  prepare_editor
%
%   Start XPCE as edit requests comming from the document server can
%   only be handled if XPCE is running.

prepare_editor :-
    current_prolog_flag(editor, pce_emacs),
    !,
    start_emacs.
prepare_editor.


                 /*******************************
                 *          USER REPLIES        *
                 *******************************/

:- http_handler(pldoc(.),          pldoc_root,
                [ prefix,
                  authentication(pldoc(read)),
                  condition(doc_enabled)
                ]).
:- http_handler(pldoc('index.html'), pldoc_index,   []).
:- http_handler(pldoc(file),       pldoc_file,     []).
:- http_handler(pldoc(place),      go_place,       []).
:- http_handler(pldoc(edit),       pldoc_edit,
                [authentication(pldoc(edit))]).
:- http_handler(pldoc(doc),        pldoc_doc,      [prefix]).
:- http_handler(pldoc(man),        pldoc_man,      []).
:- http_handler(pldoc(doc_for),    pldoc_object,   [id(pldoc_doc_for)]).
:- http_handler(pldoc(search),     pldoc_search,   []).
:- http_handler(pldoc('res/'),     pldoc_resource, [prefix]).


%!  pldoc_root(+Request)
%
%   Reply using the index-page  of   the  Prolog  working directory.
%   There are various options for the   start directory. For example
%   we could also use the file or   directory of the file that would
%   be edited using edit/0.

pldoc_root(Request) :-
    http_parameters(Request,
                    [ empty(Empty, [ oneof([true,false]),
                                     default(false)
                                   ])
                    ]),
    pldoc_root(Request, Empty).

pldoc_root(Request, false) :-
    http_location_by_id(pldoc_root, Root),
    memberchk(path(Path), Request),
    Root \== Path,
    !,
    existence_error(http_location, Path).
pldoc_root(_Request, false) :-
    working_directory(Dir0, Dir0),
    allowed_directory(Dir0),
    !,
    ensure_slash_end(Dir0, Dir1),
    doc_file_href(Dir1, Ref0),
    atom_concat(Ref0, 'index.html', Index),
    throw(http_reply(see_other(Index))).
pldoc_root(Request, _) :-
    pldoc_index(Request).


%!  pldoc_index(+Request)
%
%   HTTP handle for /index.html, providing an overall overview
%   of the available documentation.

pldoc_index(_Request) :-
    reply_html_page(pldoc(index),
                    title('SWI-Prolog documentation'),
                    [ \doc_links('', []),
                       h1('SWI-Prolog documentation'),
                      \man_overview([])
                    ]).


%!  pldoc_file(+Request)
%
%   Hander for /file?file=File, providing documentation for File.

pldoc_file(Request) :-
    http_parameters(Request,
                    [ file(File, [])
                    ]),
    (   source_file(File)
    ->  true
    ;   throw(http_reply(forbidden(File)))
    ),
    doc_for_file(File, []).

%!  pldoc_edit(+Request)
%
%   HTTP handler that starts the user's   default editor on the host
%   running the server. This  handler  can   only  accessed  if  the
%   browser connection originates from  =localhost=.   The  call can
%   edit files using the =file=  attribute   or  a predicate if both
%   =name= and =arity= is given and optionally =module=.

pldoc_edit(Request) :-
    http:authenticate(pldoc(edit), Request, _),
    http_parameters(Request,
                    [ file(File,
                           [ optional(true),
                             description('Name of the file to edit')
                           ]),
                      line(Line,
                           [ optional(true),
                             integer,
                             description('Line in the file')
                           ]),
                      name(Name,
                           [ optional(true),
                             description('Name of a Prolog predicate to edit')
                           ]),
                      arity(Arity,
                            [ integer,
                              optional(true),
                              description('Arity of a Prolog predicate to edit')
                            ]),
                      module(Module,
                             [ optional(true),
                               description('Name of a Prolog module to search for predicate')
                             ])
                    ]),
    (   atom(File)
    ->  allowed_file(File)
    ;   true
    ),
    (   atom(File), integer(Line)
    ->  Edit = file(File, line(Line))
    ;   atom(File)
    ->  Edit = file(File)
    ;   atom(Name), integer(Arity)
    ->  (   atom(Module)
        ->  Edit = (Module:Name/Arity)
        ;   Edit = (Name/Arity)
        )
    ),
    edit(Edit),
    format('Content-type: text/plain~n~n'),
    format('Started ~q~n', [edit(Edit)]).
pldoc_edit(_Request) :-
    http_location_by_id(pldoc_edit, Location),
    throw(http_reply(forbidden(Location))).


%!  go_place(+Request)
%
%   HTTP handler to handle the places menu.

go_place(Request) :-
    http_parameters(Request,
                    [ place(Place, [])
                    ]),
    places(Place).

places(':packs:') :-
    !,
    http_link_to_id(pldoc_pack, [], HREF),
    throw(http_reply(moved(HREF))).
places(Dir0) :-
    expand_alias(Dir0, Dir),
    (   allowed_directory(Dir)
    ->  format(string(IndexFile), '~w/index.html', [Dir]),
        doc_file_href(IndexFile, HREF),
        throw(http_reply(moved(HREF)))
    ;   throw(http_reply(forbidden(Dir)))
    ).


%!  allowed_directory(+Dir) is semidet.
%
%   True if we are allowed to produce and index for Dir.

allowed_directory(Dir) :-
    source_directory(Dir),
    !.
allowed_directory(Dir) :-
    working_directory(CWD, CWD),
    same_file(CWD, Dir).
allowed_directory(Dir) :-
    prolog:doc_directory(Dir).


%!  allowed_file(+File) is semidet.
%
%   True if we are allowed to serve   File.  Currently means we have
%   predicates loaded from File or the directory must be allowed.

allowed_file(File) :-
    source_file(_, File),
    !.
allowed_file(File) :-
    absolute_file_name(File, Canonical),
    file_directory_name(Canonical, Dir),
    allowed_directory(Dir).


%!  pldoc_resource(+Request)
%
%   Handler for /res/File, serving CSS, JS and image files.

pldoc_resource(Request) :-
    http_location_by_id(pldoc_resource, ResRoot),
    memberchk(path(Path), Request),
    atom_concat(ResRoot, File, Path),
    file(File, Local),
    http_reply_file(pldoc(Local), [], Request).

file('pldoc.css',     'pldoc.css').
file('pllisting.css', 'pllisting.css').
file('pldoc.js',      'pldoc.js').
file('edit.png',      'edit.png').
file('editpred.png',  'editpred.png').
file('up.gif',        'up.gif').
file('source.png',    'source.png').
file('public.png',    'public.png').
file('private.png',   'private.png').
file('reload.png',    'reload.png').
file('favicon.ico',   'favicon.ico').
file('h1-bg.png',     'h1-bg.png').
file('h2-bg.png',     'h2-bg.png').
file('pub-bg.png',    'pub-bg.png').
file('priv-bg.png',   'priv-bg.png').
file('multi-bg.png',  'multi-bg.png').


%!  pldoc_doc(+Request)
%
%   Handler for /doc/Path
%
%   Reply documentation of a file. Path is  the absolute path of the
%   file for which to return the  documentation. Extension is either
%   none, the Prolog extension or the HTML extension.
%
%   Note that we reply  with  pldoc.css   if  the  file  basename is
%   pldoc.css to allow for a relative link from any directory.

pldoc_doc(Request) :-
    memberchk(path(ReqPath), Request),
    http_location_by_id(pldoc_doc, Me),
    atom_concat(Me, AbsFile0, ReqPath),
    (   sub_atom(ReqPath, _, _, 0, /)
    ->  atom_concat(ReqPath, 'index.html', File),
        throw(http_reply(moved(File)))
    ;   clean_path(AbsFile0, AbsFile1),
        expand_alias(AbsFile1, AbsFile),
        is_absolute_file_name(AbsFile)
    ->  documentation(AbsFile, Request)
    ).

documentation(Path, Request) :-
    file_base_name(Path, Base),
    file(_, Base),                         % serve pldoc.css, etc.
    !,
    http_reply_file(pldoc(Base), [], Request).
documentation(Path, Request) :-
    file_name_extension(_, Ext, Path),
    autolink_extension(Ext, image),
    http_reply_file(Path, [unsafe(true)], Request).
documentation(Path, Request) :-
    Index = '/index.html',
    sub_atom(Path, _, _, 0, Index),
    atom_concat(Dir, Index, Path),
    exists_directory(Dir),                 % Directory index
    !,
    (   allowed_directory(Dir)
    ->  edit_options(Request, EditOptions),
        doc_for_dir(Dir, EditOptions)
    ;   throw(http_reply(forbidden(Dir)))
    ).
documentation(File, Request) :-
    wiki_file(File, WikiFile),
    !,
    (   allowed_file(WikiFile)
    ->  true
    ;   throw(http_reply(forbidden(File)))
    ),
    edit_options(Request, Options),
    doc_for_wiki_file(WikiFile, Options).
documentation(Path, Request) :-
    pl_file(Path, File),
    !,
    (   allowed_file(File)
    ->  true
    ;   throw(http_reply(forbidden(File)))
    ),
    doc_reply_file(File, Request).
documentation(Path, _) :-
    throw(http_reply(not_found(Path))).

:- public
    doc_reply_file/2.

doc_reply_file(File, Request) :-
    http_parameters(Request,
                    [ public_only(Public),
                      reload(Reload),
                      show(Show),
                      format_comments(FormatComments)
                    ],
                    [ attribute_declarations(param)
                    ]),
    (   exists_file(File)
    ->  true
    ;   throw(http_reply(not_found(File)))
    ),
    (   Reload == true,
        source_file(File)
    ->  load_files(File, [if(changed), imports([])])
    ;   true
    ),
    edit_options(Request, EditOptions),
    (   Show == src
    ->  format('Content-type: text/html~n~n', []),
        source_to_html(File, stream(current_output),
                       [ skin(src_skin(Request, Show, FormatComments)),
                         format_comments(FormatComments)
                       ])
    ;   Show == raw
    ->  http_reply_file(File,
                        [ unsafe(true), % is already validated
                          mime_type(text/plain)
                        ], Request)
    ;   doc_for_file(File,
                     [ public_only(Public),
                       source_link(true)
                     | EditOptions
                     ])
    ).


:- public src_skin/5.                   % called through source_to_html/3.

src_skin(Request, _Show, FormatComments, header, Out) :-
    memberchk(request_uri(ReqURI), Request),
    negate(FormatComments, AltFormatComments),
    replace_parameters(ReqURI, [show(raw)], RawLink),
    replace_parameters(ReqURI, [format_comments(AltFormatComments)], CmtLink),
    phrase(html(div(class(src_formats),
                    [ 'View source with ',
                      a(href(CmtLink), \alt_view(AltFormatComments)),
                      ' or as ',
                      a(href(RawLink), raw)
                    ])), Tokens),
    print_html(Out, Tokens).

alt_view(true) -->
    html('formatted comments').
alt_view(false) -->
    html('raw comments').

negate(true, false).
negate(false, true).

replace_parameters(ReqURI, Extra, URI) :-
    uri_components(ReqURI, C0),
    uri_data(search, C0, Search0),
    (   var(Search0)
    ->  uri_query_components(Search, Extra)
    ;   uri_query_components(Search0, Form0),
        merge_options(Extra, Form0, Form),
        uri_query_components(Search, Form)
    ),
    uri_data(search, C0, Search, C),
    uri_components(URI, C).


%!  edit_options(+Request, -Options) is det.
%
%   Return edit(true) in Options  if  the   connection  is  from the
%   localhost.

edit_options(Request, [edit(true)]) :-
    catch(http:authenticate(pldoc(edit), Request, _), _, fail),
    !.
edit_options(_, []).


%!  pl_file(+File, -PlFile) is semidet.

pl_file(File, PlFile) :-
    file_name_extension(Base, html, File),
    !,
    absolute_file_name(Base,
                       PlFile,
                       [ file_errors(fail),
                         file_type(prolog),
                         access(read)
                       ]).
pl_file(File, File).

%!  wiki_file(+File, -TxtFile) is semidet.
%
%   True if TxtFile is an existing file  that must be served as wiki
%   file.

wiki_file(File, TxtFile) :-
    file_name_extension(_, Ext, File),
    wiki_file_extension(Ext),
    !,
    TxtFile = File.
wiki_file(File, TxtFile) :-
    file_base_name(File, Base),
    autolink_file(Base, wiki),
    !,
    TxtFile = File.
wiki_file(File, TxtFile) :-
    file_name_extension(Base, html, File),
    wiki_file_extension(Ext),
    file_name_extension(Base, Ext, TxtFile),
    access_file(TxtFile, read).

wiki_file_extension(md).
wiki_file_extension(txt).


%!  clean_path(+AfterDoc, -AbsPath)
%
%   Restore the path, Notably deals Windows issues

clean_path(Path0, Path) :-
    current_prolog_flag(windows, true),
    sub_atom(Path0, 2, _, _, :),
    !,
    sub_atom(Path0, 1, _, 0, Path).
clean_path(Path, Path).


%!  pldoc_man(+Request)
%
%   Handler for /man, offering one of the parameters:
%
%       * predicate=PI
%       providing documentation from the manual on the predicate PI.
%       * function=PI
%       providing documentation from the manual on the function PI.
%       * 'CAPI'=F
%       providing documentation from the manual on the C-function F.

pldoc_man(Request) :-
    http_parameters(Request,
                    [ predicate(PI, [optional(true)]),
                      function(Fun, [optional(true)]),
                      'CAPI'(F,     [optional(true)]),
                      section(Sec,  [optional(true)])
                    ]),
    (   ground(PI)
    ->  atom_pi(PI, Obj)
    ;   ground(Fun)
    ->  atomic_list_concat([Name,ArityAtom], /, Fun),
        atom_number(ArityAtom, Arity),
        Obj = f(Name/Arity)
    ;   ground(F)
    ->  Obj = c(F)
    ;   ground(Sec)
    ->  atom_concat('sec:', Sec, SecID),
        Obj = section(SecID)
    ),
    man_title(Obj, Title),
    reply_html_page(
        pldoc(object(Obj)),
        title(Title),
        \man_page(Obj, [])).

man_title(f(Obj), Title) :-
    !,
    format(atom(Title), 'SWI-Prolog -- function ~w', [Obj]).
man_title(c(Obj), Title) :-
    !,
    format(atom(Title), 'SWI-Prolog -- API-function ~w', [Obj]).
man_title(section(_Id), Title) :-
    !,
    format(atom(Title), 'SWI-Prolog -- Manual', []).
man_title(Obj, Title) :-
    format(atom(Title), 'SWI-Prolog -- ~w', [Obj]).

%!  pldoc_object(+Request)
%
%   Handler for /doc_for?object=Term, Provide  documentation for the
%   given term.

pldoc_object(Request) :-
    http_parameters(Request,
                    [ object(Atom, []),
                      header(Header, [default(true)])
                    ]),
    (   catch(atom_to_term(Atom, Obj, _), error(_,_), fail)
    ->  true
    ;   atom_to_object(Atom, Obj)
    ),
    (   prolog:doc_object_title(Obj, Title)
    ->  true
    ;   Title = Atom
    ),
    edit_options(Request, EditOptions),
    reply_html_page(
        pldoc(object(Obj)),
        title(Title),
        \object_page(Obj, [header(Header)|EditOptions])).


%!  pldoc_search(+Request)
%
%   Search the collected PlDoc comments and Prolog manual.

pldoc_search(Request) :-
    http_parameters(Request,
                    [ for(For,
                          [ optional(true),
                            description('String to search for')
                          ]),
                      page(Page,
                           [ integer,
                             default(1),
                             description('Page of search results to view')
                           ]),
                      in(In,
                         [ oneof([all,app,noapp,man,lib,pack,wiki]),
                           default(all),
                           description('Search everying, application only or manual only')
                         ]),
                      match(Match,
                            [ oneof([name,summary]),
                              default(summary),
                              description('Match only the name or also the summary')
                            ]),
                      resultFormat(Format,
                                   [ oneof(long,summary),
                                     default(summary),
                                     description('Return full documentation or summary-lines')
                                   ])
                    ]),
    edit_options(Request, EditOptions),
    format(string(Title), 'Prolog search -- ~w', [For]),
    reply_html_page(pldoc(search(For)),
                    title(Title),
                    \search_reply(For,
                                  [ resultFormat(Format),
                                    search_in(In),
                                    search_match(Match),
                                    page(Page)
                                  | EditOptions
                                  ])).


                 /*******************************
                 *     HTTP PARAMETER TYPES     *
                 *******************************/

:- public
    param/2.                        % used in pack documentation server

param(public_only,
      [ boolean,
        default(true),
        description('If true, hide private predicates')
      ]).
param(reload,
      [ boolean,
        default(false),
        description('Reload the file and its documentation')
      ]).
param(show,
      [ oneof([doc,src,raw]),
        default(doc),
        description('How to show the file')
      ]).
param(format_comments,
      [ boolean,
        default(true),
        description('If true, use PlDoc for rendering structured comments')
      ]).
