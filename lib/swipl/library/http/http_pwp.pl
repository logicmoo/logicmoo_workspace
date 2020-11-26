/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2011, VU University, Amsterdam
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

:- module(http_pwp,
          [ reply_pwp_page/3,           % :File, +Options, +Request
            pwp_handler/2               % +Options, +Request
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pwp)).

:- predicate_options(pwp_handler/2, 1,
                     [ cache(boolean),
                       hide_extensions(list(atom)),
                       index_hook(callable),
                       mime_type(any),
                       path_alias(atom),
                       unsafe(boolean),
                       view(boolean)
                     ]).
:- predicate_options(reply_pwp_page/3, 2,
                     [ dtd(any),
                       mime_type(any),
                       pwp_module(boolean),
                       unsafe(boolean)
                     ]).

/** <module> Serve PWP pages through the HTTP server

This  module  provides  convience  predicates  to  include  PWP  (Prolog
Well-formed Pages) in a Prolog  web-server.   It  provides the following
predicates:

    * pwp_handler/2
    This is a complete web-server aimed at serving static pages, some
    of which include PWP.  This API is intended to allow for programming
    the web-server from a hierarchy of pwp files, prolog files and static
    web-pages.

    * reply_pwp_page/3
    Return a single PWP page that is executed in the context of the calling
    module.  This API is intended for individual pages that include so much
    text that generating from Prolog is undesirable.

@tbd    Support elements in the HTML header that allow controlling the
        page, such as setting the CGI-header, authorization, etc.
@tbd    Allow external styling.  Pass through reply_html_page/2?  Allow
        filtering the DOM before/after PWP?
*/

%!  pwp_handler(+Options, +Request)
%
%   Handle PWP files. This predicate is   defined to create a simple
%   HTTP server from a hierarchy of PWP,   HTML and other files. The
%   interface      is      kept      compatible        with      the
%   library(http/http_dispatch). In the typical  usage scenario, one
%   needs to define an http location and  a file-search path that is
%   used as the root of the server.  E.g., the following declarations
%   create a self-contained web-server for files in =|/web/pwp/|=.
%
%       ==
%       user:file_search_path(pwp, '/web/pwp').
%
%       :- http_handler(root(.), pwp_handler([path_alias(pwp)]), [prefix]).
%       ==
%
%   Options include:
%
%       * path_alias(+Alias)
%       Search for PWP files as Alias(Path).  See absolute_file_name/3.
%       * index(+Index)
%       Name of the directory index (pwp) file.  This option may
%       appear multiple times.  If no such option is provided,
%       pwp_handler/2 looks for =|index.pwp|=.
%       * view(+Boolean)
%       If =true= (default is =false=), allow for ?view=source to serve
%       PWP file as source.
%       * index_hook(:Hook)
%       If a directory has no index-file, pwp_handler/2 calls
%       Hook(PhysicalDir, Options, Request).  If this semidet
%       predicate succeeds, the request is considered handled.
%       * hide_extensions(+List)
%       Hide files of the given extensions.  The default is to
%       hide .pl files.
%       * dtd(?DTD)
%       DTD to parse the input file with. If unbound, the generated
%       DTD is returned
%
%   @see reply_pwp_page/3
%   @error permission_error(index, http_location, Location) is
%   raised if the handler resolves to a directory that has no
%   index.

:- meta_predicate
    pwp_handler(:, +).

pwp_handler(QOptions, Request) :-
    meta_options(is_meta, QOptions, Options),
    (   memberchk(path_info(Spec), Request)
    ->  true
    ;   Spec = '.'
    ),
    (   option(path_alias(Alias), Options)
    ->  Term =.. [Alias,Spec]
    ;   Term = Spec
    ),
    http_safe_file(Term, Options),
    (   absolute_file_name(Term, Path,
                           [ file_type(directory),
                             access(read),
                             file_errors(fail)
                           ])
    ->  ensure_slash(Path, Dir),
        (   (   member(index(Index), Options)
            *-> true
            ;   Index = 'index.pwp'
            ),
            atom_concat(Dir, Index, File),
            access_file(File, read)
        ->  true
        ;   option(index_hook(Hook), Options),
            call(Hook, Path, Options, Request)
        ->  true
        ;   memberchk(path(Location), Request),
            permission_error(index, http_location, Location)
        )
    ;   absolute_file_name(Term, File,
                           [ access(read)
                           ])
    ),
    server_file(File, Request, Options).

is_meta(index_hook).

server_file(File, _, _) :-              % index-hook did the work
    var(File),
    !.
server_file(File, Request, Options) :-
    file_name_extension(_, pwp, File),
    !,
    (   option(view(true), Options),
        memberchk(search(Query), Request),
        memberchk(view=source, Query)
    ->  http_reply_file(File, [ mime_type(text/plain),
                                unsafe(true)
                              ], Request)
    ;   merge_options(Options,
                      [ pwp_module(true)
                      ], Opts),
        reply_pwp_page(File, [unsafe(true)|Opts], Request)
    ).
server_file(File, Request, Options) :-
    option(hide_extensions(Exts), Options, [pl]),
    file_name_extension(_, Ext, File),
    (   memberchk(Ext, Exts)
    ->  memberchk(path(Location), Request),
        permission_error(read, http_location, Location)
    ;   http_reply_file(File, [unsafe(true)|Options], Request)
    ).


ensure_slash(Path, Dir) :-
    (   sub_atom(Path, _, _, 0, /)
    ->  Dir = Path
    ;   atom_concat(Path, /, Dir)
    ).


%!  reply_pwp_page(:File, +Options, +Request)
%
%   Reply  a  PWP  file.  This  interface   is  provided  to  server
%   individual locations from PWP files.  Using   a  PWP file rather
%   than generating the page from Prolog   may  be desirable because
%   the page contains a lot of text (which is cumbersome to generate
%   from Prolog) or because the  maintainer   is  not  familiar with
%   Prolog.
%
%   Options supported are:
%
%       * mime_type(+Type)
%       Serve the file using the given mime-type.  Default is
%       text/html.
%       * unsafe(+Boolean)
%       Passed to http_safe_file/2 to check for unsafe paths.
%       * pwp_module(+Boolean)
%       If =true=, (default =false=), process the PWP file in
%       a module constructed from its canonical absolute path.
%       Otherwise, the PWP file is processed in the calling
%       module.
%
%   Initial context:
%
%       * SCRIPT_NAME
%       Virtual path of the script.
%       * SCRIPT_DIRECTORY
%       Physical directory where the script lives
%       * QUERY
%       Var=Value list representing the query-parameters
%       * REMOTE_USER
%       If access has been authenticated, this is the authenticated
%       user.
%       * REQUEST_METHOD
%       One of =get=, =post=, =put= or =head=
%       * CONTENT_TYPE
%       Content-type provided with HTTP POST and PUT requests
%       * CONTENT_LENGTH
%       Content-length provided with HTTP POST and PUT requests
%
%   While processing the script, the file-search-path pwp includes
%   the current location of the script.  I.e., the following will
%   find myprolog in the same directory as where the PWP file
%   resides.
%
%       ==
%       pwp:ask="ensure_loaded(pwp(myprolog))"
%       ==
%
%   @tbd complete the initial context, as far as possible from CGI
%        variables.  See http://hoohoo.ncsa.illinois.edu/docs/cgi/env.html
%   @see pwp_handler/2.

:- meta_predicate
    reply_pwp_page(:, +, +).

reply_pwp_page(M:File, Options, Request) :-
    http_safe_file(File, Options),
    absolute_file_name(File, Path,
                       [ access(read)
                       ]),
    memberchk(method(Method), Request),
    file_directory_name(Path, Dir),
    (   option(dtd(DTD), Options)
    ->  SGMLOptions = [dtd(DTD)]
    ;   SGMLOptions = []
    ),
    load_structure(Path, Contents, [dialect(xml)|SGMLOptions]),
    findall(C, pwp_context(Request, C), Context),
    (   option(pwp_module(true), Options)
    ->  PWP_M = Path
    ;   PWP_M = M
    ),
    setup_call_cleanup(asserta(script_dir(Dir), Ref),
                       pwp_xml(PWP_M:Contents, Transformed,
                               [ 'REQUEST_METHOD' = Method,
                                 'SCRIPT_DIRECTORY' = Dir
                               | Context
                               ]),
                       erase(Ref)),
    copy_http_equiv(Transformed),
    default_mime_type(Request, DefType),
    option(mime_type(Type), Options, DefType),
    format('Content-type: ~w\r\n\r\n', [Type]),
    (   Type = text/html
    ->  html_write(current_output, Transformed, [])
    ;   xml_write(current_output, Transformed, [])
    ).


%!  copy_http_equiv(+XMLDOM) is det.
%
%   Copy =|http-equiv|= elements  from  the   document  to  the  CGI
%   header.

copy_http_equiv(Contents) :-
    memberchk(element(html, _, HtmlElement), Contents),
    memberchk(element(head, _, HeadElement), HtmlElement),
    !,
    forall(http_equiv(HeadElement, HttpEquiv, HttpEquivValue),
           format('~w: ~w\r\n', [HttpEquiv, HttpEquivValue])).
copy_http_equiv(_).

http_equiv(Head, Name, Value) :-
    member(element(meta, MetaAttributes, []), Head),
    memberchk('http-equiv'=Name, MetaAttributes),
    memberchk(content=Value, MetaAttributes).


%!  default_mime_type(+Request, +DefType) is det.
%
%   Extract the preferred content-type from the Request.  This is
%   part of the PWP reply-format negotiation.
%
%   See http://www.w3.org/TR/xhtml-media-types/#media-types

default_mime_type(Request, DefType) :-
    XHTML = application/'xhml+xml',
    memberchk(accept(Accept), Request),
    memberchk(media(Type, _, _, _), Accept),
    Type == XHTML,
    !,
    DefType = XHTML.
default_mime_type(_, text/html).

%!  pwp_context(+Request, -Context) is nondet.
%
%   Provide some environment variables similar to CGI scripts.

pwp_context(Request, 'REMOTE_USER' = User) :-
    memberchk(user(User), Request).
pwp_context(Request, 'QUERY' = Query) :-
    memberchk(search(Query), Request).
pwp_context(Request, 'SCRIPT_NAME' = Path) :-
    memberchk(path(Path), Request).
pwp_context(Request, 'CONTENT_TYPE' = ContentType) :-
    memberchk(content_type(ContentType), Request).
pwp_context(Request, 'CONTENT_LENGTH' = Length) :-
    memberchk(content_length(Length), Request).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.
:- thread_local script_dir/1.

user:file_search_path(pwp, ScriptDir) :-
    script_dir(ScriptDir).


