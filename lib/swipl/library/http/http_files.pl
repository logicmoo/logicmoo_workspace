/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2020, VU University Amsterdam
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

:- module(http_files,
          [ http_reply_from_files/3     % +Dir, +Options, +Request
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dirindex)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- predicate_options(http_reply_from_files/3, 2, [indexes(list(atom))]).

/** <module> Serve plain files from a hierarchy

Although the SWI-Prolog Web Server is   intended to serve documents that
are computed dynamically, serving plain   files  is sometimes necessary.
This small module combines the   functionality  of http_reply_file/3 and
http_reply_dirindex/3 to act as a simple   web-server. Such a server can
be created using the following code  sample,   which  starts a server at
port 8080 that serves files from the  current directory ('.'). Note that
the handler needs a `prefix` option to   specify that it must handle all
paths that begin with the registed location of the handler.

```
:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).

:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).

:- initialization(http_server([port(8080)]), main).
```

@see    pwp_handler/2 provides similar facilities, where .pwp files
        can be used to add dynamic behaviour.
*/


%!  http_reply_from_files(+Dir, +Options, +Request)
%
%   HTTP handler that serves files  from   the  directory  Dir. This
%   handler uses http_reply_file/3 to  reply   plain  files.  If the
%   request resolves to a directory, it uses the option =indexes= to
%   locate an index file (see   below) or uses http_reply_dirindex/3
%   to create a listing of the directory.
%
%   Options:
%
%     * indexes(+List)
%     List of files tried to find an index for a directory.  The
%     default is `['index.html']`.
%
%   Note that this handler must be tagged as a =prefix= handler (see
%   http_handler/3 and module introduction). This  also implies that
%   it is possible to  override  more   specific  locations  in  the
%   hierarchy using http_handler/3 with a longer path-specifier.
%
%   @param  Dir is either a directory or an path-specification as
%           used by absolute_file_name/3.  This option provides
%           great flexibility in (re-)locating the physical files
%           and allows merging the files of multiple physical
%           locations into one web-hierarchy by using multiple
%           user:file_search_path/2 clauses that define the same
%           alias.
%   @see    The hookable predicate file_mime_type/2 is used to
%           determine the ``Content-type`` from the file name.

http_reply_from_files(Dir, Options, Request) :-
    (   memberchk(path_info(PathInfo), Request)
    ->  true
    ;   PathInfo = ''
    ),
    http_safe_file(PathInfo, []),
    locate_file(Dir, PathInfo, Result, ResultType, Options),
    !,
    reply(ResultType, Result, Request).

reply(file, Path, Request) :-
    http_reply_file(Path, [unsafe(true)], Request).
reply(index, Path, Request) :-
    http_reply_dirindex(Path, [unsafe(true)], Request).
reply(redirect, _, Request) :-
    memberchk(path(Path), Request),
    atom_concat(Path, /, NewLocation),
    http_redirect(moved_temporary, NewLocation, Request).

locate_file(Dir, PathInfo, Result, ResultType, Options) :-
    absolute_file_name(Dir, DirPath,
                       [ file_type(directory),
                         access(read),
                         solutions(all)
                       ]),
    directory_file_path(DirPath, PathInfo, Path),
    (   exists_file(Path)
    ->  ResultType = file,
        Result = Path
    ;   exists_directory(Path),
        (   sub_atom(Path, _, _, 0, /)
        ->  (   option(indexes(Indexes), Options, ['index.html']),
                member(Index, Indexes),
                directory_file_path(Path, Index, IndexFile),
                exists_file(IndexFile)
            ->  Result = IndexFile,
                ResultType = file
            ;   Result = Path,
                ResultType = index
            )
        ;   ResultType = redirect
        )
    ).
