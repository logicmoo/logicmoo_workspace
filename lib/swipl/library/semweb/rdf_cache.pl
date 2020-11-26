/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2020, VU University Amsterdam
                              CWI, Amsterdam
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

:- module(rdf_cache,
          [ rdf_set_cache_options/1,    % +Options
            rdf_cache_file/3            % +URL, +RW, -File
          ]).
:- autoload(library(error),[must_be/2,domain_error/2]).
:- autoload(library(filesex),[make_directory_path/1]).

/** <module> Cache RDF triples

The library library(semweb/rdf_cache) defines the   caching strategy for
triples sources. When using large RDF   sources, caching triples greatly
speedup loading RDF documents. The cache  library implements two caching
strategies that are controlled by rdf_set_cache_options/1.

*|Local caching|* This approach  applies  to   files  only.  Triples are
cached in a sub-directory of  the   directory  holding  the source. This
directory is called =|.cache|= (=|_cache|=  on   Windows).  If the cache
option =create_local_directory= is =true=, a  cache directory is created
if posible.

*|Global caching|* This approach applies  to   all  sources,  except for
unnamed streams. Triples are cached in   directory  defined by the cache
option =global_directory=.

When loading an RDF file, the system   scans  the configured cache files
unless cache(false) is specified as option   to rdf_load/2 or caching is
disabled. If caching is enabled but no cache exists, the system will try
to create a cache file. First it will try to do this locally. On failure
it will try to configured global cache.
*/

:- dynamic
    cache_option/1.

set_setfault_options :-
    assert(cache_option(enabled(true))),
    (   current_prolog_flag(windows, true)
    ->  assert(cache_option(local_directory('_cache')))
    ;   assert(cache_option(local_directory('.cache')))
    ).

:- set_setfault_options.                % _only_ when loading!

%!  rdf_set_cache_options(+Options)
%
%   Change the cache policy.  Provided options are:
%
%     * enabled(Boolean)
%     If =true=, caching is enabled.
%
%     * local_directory(Name).
%     Plain name of local directory.  Default =|.cache|=
%     (=|_cache|= on Windows).
%
%     * create_local_directory(Bool)
%     If =true=, try to create local cache directories
%
%     * global_directory(Dir)
%     Writeable directory for storing cached parsed files.
%
%     * create_global_directory(Bool)
%     If =true=, try to create the global cache directory.

rdf_set_cache_options([]) :- !.
rdf_set_cache_options([H|T]) :-
    !,
    rdf_set_cache_options(H),
    rdf_set_cache_options(T).
rdf_set_cache_options(Opt) :-
    functor(Opt, Name, Arity),
    arg(1, Opt, Value),
    (   cache_option(Name, Type)
    ->  must_be(Type, Value)
    ;   domain_error(cache_option, Opt)
    ),
    functor(Gen, Name, Arity),
    retractall(cache_option(Gen)),
    expand_option(Opt, EOpt),
    assert(cache_option(EOpt)).

cache_option(enabled,                 boolean).
cache_option(local_directory,         atom).
cache_option(create_local_directory,  boolean).
cache_option(global_directory,        atom).
cache_option(create_global_directory, boolean).

expand_option(global_directory(Local), global_directory(Global)) :-
    !,
    absolute_file_name(Local, Global).
expand_option(Opt, Opt).


%!  rdf_cache_file(+URL, +ReadWrite, -File) is semidet.
%
%   File is the cache file  for  URL.   If  ReadWrite  is =read=, it
%   returns the name of an existing file.  If =write= it returns
%   where a new cache file can be overwritten or created.

rdf_cache_file(_URL, _, _File) :-
    cache_option(enabled(false)),
    !,
    fail.
rdf_cache_file(URL, read, File) :-
    !,
    (   atom_concat('file://', Path, URL),
        cache_option(local_directory(Local)),
        file_directory_name(Path, Dir),
        local_cache_file(URL, LocalFile),
        atomic_list_concat([Dir, Local, LocalFile], /, File)
    ;   cache_option(global_directory(Dir)),
        url_cache_file(URL, Dir, trp, read, File)
    ),
    access_file(File, read),
    !.
rdf_cache_file(URL, write, File) :-
    !,
    (   atom_concat('file://', Path, URL),
        cache_option(local_directory(Local)),
        file_directory_name(Path, Dir),
        (   cache_option(create_local_directory(true))
        ->  RWDir = write
        ;   RWDir = read
        ),
        ensure_dir(Dir, Local, RWDir, CacheDir),
        local_cache_file(URL, LocalFile),
        atomic_list_concat([CacheDir, LocalFile], /, File)
    ;   cache_option(global_directory(Dir)),
        ensure_global_cache(Dir),
        url_cache_file(URL, Dir, trp, write, File)
    ),
    access_file(File, write),
    !.


ensure_global_cache(Dir) :-
    exists_directory(Dir),
    !.
ensure_global_cache(Dir) :-
    cache_option(create_global_directory(true)),
    make_directory_path(Dir),
    print_message(informational, rdf(cache_created(Dir))).


                 /*******************************
                 *         LOCAL CACHE          *
                 *******************************/

%!  local_cache_file(+FileURL, -File) is det.
%
%   Return the name of the cache file   for FileURL. The name is the
%   plain filename with the .trp extension.  As   the  URL is a file
%   URL, it is guaranteed  to  be   a  valid  filename.  Assumes the
%   hosting OS can handle  multiple   exensions  (=|.x.y|=)  though.
%   These days thats even true on Windows.

local_cache_file(URL, File) :-
    file_base_name(URL, Name),
    file_name_extension(Name, trp, File).


                 /*******************************
                 *         GLOBAL CACHE         *
                 *******************************/

%!  url_cache_file(+URL, +Dir, +Ext, +RW, -Path) is semidet.
%
%   Determine location of cache-file for the   given  URL in Dir. If
%   Ext is provided, the  returned  Path   is  ensured  to  have the
%   specified extension.
%
%   @param RW       If =read=, no directories are created and the call
%                   fails if URL is not in the cache.

url_cache_file(URL, Dir, Ext, RW, Path) :-
    term_hash(URL, Hash0),
    Hash is Hash0 + 100000,         % make sure > 4 characters
    format(string(Hex), '~16r', [Hash]),
    sub_atom(Hex, _, 2, 0, L1),
    ensure_dir(Dir, L1, RW, Dir1),
    sub_atom(Hex, _, 2, 2, L2),
    ensure_dir(Dir1, L2, RW, Dir2),
    url_to_file(URL, File),
    ensure_ext(File, Ext, FileExt),
    atomic_list_concat([Dir2, /, FileExt], Path).

ensure_dir(D0, Sub, RW, Dir) :-
    atomic_list_concat([D0, /, Sub], Dir),
    (   exists_directory(Dir)
    ->  true
    ;   RW == write
    ->  catch(make_directory(Dir), _, fail)
    ).

ensure_ext(File, '', File) :- !.
ensure_ext(File, Ext, File) :-
    file_name_extension(_, Ext, File),
    !.
ensure_ext(File, Ext, FileExt) :-
    file_name_extension(File, Ext, FileExt).

%!  url_to_file(+URL, -File)
%
%   Convert a URL in something that fits  in a file, i.e. avoiding /
%   and :. We  simply  replace  these  by   -.  We  could  also  use
%   www_form_encode/2, but confusion when to replace  as well as the
%   fact that we loose the '.' (extension)   makes this a less ideal
%   choice.  We could also consider base64 encoding of the name.

url_to_file(URL, File) :-
    atom_codes(URL, Codes),
    phrase(safe_file_name(Codes), FileCodes),
    atom_codes(File, FileCodes).

safe_file_name([]) -->
    [].
safe_file_name([H|T]) -->
    replace(H),
    !,
    safe_file_name(T).
safe_file_name([H|T]) -->
    [H],
    safe_file_name(T).

%!  replace(+Code)//
%
%   Replace a character  code  that  cannot   safely  be  put  in  a
%   filename. Should we use %XX?

replace(0'/)  --> "-".                  % directory separator
replace(0'\\) --> "-".                  % not allowed in Windows filename
replace(0':)  --> "-".                  % idem
replace(0'?)  --> "-".                  % idem
replace(0'*)  --> "-".                  % idem


                 /*******************************
                 *             MESSAGES         *
                 *******************************/

:- multifile prolog:message/3.

prolog:message(rdf(cache_created(Dir))) -->
    [ 'Created RDF cache directory ~w'-[Dir] ].
