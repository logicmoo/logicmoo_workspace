/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2020, University of Amsterdam
                              VU University Amsterdam
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

:- module(files_ex,
          [ set_time_file/3,            % +File, -OldTimes, +NewTimes
            link_file/3,                % +OldPath, +NewPath, +Type
            chmod/2,                    % +File, +Mode
            relative_file_name/3,       % ?AbsPath, +RelTo, ?RelPath
            directory_file_path/3,      % +Dir, +File, -Path
            directory_member/3,		% +Dir, -Member, +Options
            copy_file/2,                % +From, +To
            make_directory_path/1,      % +Directory
            copy_directory/2,           % +Source, +Destination
            delete_directory_and_contents/1, % +Dir
            delete_directory_contents/1 % +Dir
          ]).
:- autoload(library(apply),[maplist/2,maplist/3,foldl/4]).
:- autoload(library(error),
	    [permission_error/3,must_be/2,domain_error/2]).
:- autoload(library(lists),[member/2]).
:- autoload(library(nb_set),[empty_nb_set/1,add_nb_set/3]).


/** <module> Extended operations on files

This module provides additional operations on   files.  This covers both
more  obscure  and  possible  non-portable    low-level  operations  and
high-level utilities.

Using these Prolog primitives is typically   to  be preferred over using
operating system primitives through shell/1  or process_create/3 because
(1) there are no potential file  name   quoting  issues, (2) there is no
dependency  on  operating   system   commands    and   (3)   using   the
implementations from this library is usually faster.
*/

:- predicate_options(directory_member/3, 3,
                     [ recursive(boolean),
                       follow_links(boolean),
                       file_type(atom),
                       extensions(list(atom)),
                       file_errors(oneof([fail,warning,error])),
                       access(oneof([read,write,execute])),
                       matches(text),
                       exclude(text),
                       exclude_directory(text),
                       hidden(boolean)
                     ]).


:- use_foreign_library(foreign(files), install_files).

%!  set_time_file(+File, -OldTimes, +NewTimes) is det.
%
%   Query and set POSIX time attributes of a file. Both OldTimes and
%   NewTimes are lists of  option-terms.   Times  are represented in
%   SWI-Prolog's standard floating point numbers.   New times may be
%   specified as =now= to indicate the current time. Defined options
%   are:
%
%       * access(Time)
%       Describes the time of last access   of  the file. This value
%       can be read and written.
%
%       * modified(Time)
%       Describes the time  the  contents  of   the  file  was  last
%       modified. This value can be read and written.
%
%       * changed(Time)
%       Describes the time the file-structure  itself was changed by
%       adding (link()) or removing (unlink()) names.
%
%   Below  are  some  example  queries.   The  first  retrieves  the
%   access-time, while the second sets the last-modified time to the
%   current time.
%
%       ==
%       ?- set_time_file(foo, [access(Access)], []).
%       ?- set_time_file(foo, [], [modified(now)]).
%       ==

%!  link_file(+OldPath, +NewPath, +Type) is det.
%
%   Create a link in  the  filesystem   from  NewPath  to  OldPath. Type
%   defines the type of link and is one of =hard= or =symbolic=.
%
%   With some limitations, these functions also   work on Windows. First
%   of all, the underlying filesystem must  support links. This requires
%   NTFS. Second, symbolic links are only supported in Vista and later.
%
%   @error  domain_error(link_type, Type) if the requested link-type
%           is unknown or not supported on the target OS.

%!  relative_file_name(+Path:atom, +RelToFile:atom, -RelPath:atom) is det.
%!  relative_file_name(-Path:atom, +RelToFile:atom, +RelPath:atom) is det.
%
%   True when RelPath is Path, relative to the _file_ RelToFile. Path and
%   RelTo are first handed to absolute_file_name/2, which makes the
%   absolute *and* canonical. Below are two examples:
%
%   ```
%   ?- relative_file_name('/home/janw/nice',
%                         '/home/janw/deep/dir/file', Path).
%   Path = '../../nice'.
%
%   ?- relative_file_name(Path, '/home/janw/deep/dir/file', '../../nice').
%   Path = '/home/janw/nice'.
%   ```
%
%   Add a terminating `/` to get a path relative to a _directory_, e.g.
%
%       ?- relative_file_name('/home/janw/deep/dir/file', './', Path).
%       Path = 'deep/dir/file'.
%
%   @param  All paths must be in canonical POSIX notation, i.e.,
%           using / to separate segments in the path.  See
%           prolog_to_os_filename/2.
%   @bug    It would probably have been cleaner to use a directory
%	    as second argument.  We can not do such dynamically as this
%	    predicate is defined as a _syntactical_ operation, which
%	    implies it may be used for non-existing paths and URLs.

relative_file_name(Path, RelTo, RelPath) :- % +,+,-
    nonvar(Path),
    !,
    absolute_file_name(Path, AbsPath),
    absolute_file_name(RelTo, AbsRelTo),
    atomic_list_concat(PL, /, AbsPath),
    atomic_list_concat(RL, /, AbsRelTo),
    delete_common_prefix(PL, RL, PL1, PL2),
    to_dot_dot(PL2, DotDot, PL1),
    atomic_list_concat(DotDot, /, RelPath).
relative_file_name(Path, RelTo, RelPath) :-
    (   is_absolute_file_name(RelPath)
    ->  Path = RelPath
    ;   file_directory_name(RelTo, RelToDir),
        directory_file_path(RelToDir, RelPath, Path0),
        absolute_file_name(Path0, Path)
    ).

delete_common_prefix([H|T01], [H|T02], T1, T2) :-
    !,
    delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
    to_dot_dot(T0, T, Tail).


%!  directory_file_path(+Directory, +File, -Path) is det.
%!  directory_file_path(?Directory, ?File, +Path) is det.
%
%   True when Path is the full path-name   for  File in Dir. This is
%   comparable to atom_concat(Directory, File, Path), but it ensures
%   there is exactly one / between the two parts.  Notes:
%
%     * In mode (+,+,-), if File is given and absolute, Path
%     is unified to File.
%     * Mode (-,-,+) uses file_directory_name/2 and file_base_name/2

directory_file_path(Dir, File, Path) :-
    nonvar(Dir), nonvar(File),
    !,
    (   (   is_absolute_file_name(File)
        ;   Dir == '.'
        )
    ->  Path = File
    ;   sub_atom(Dir, _, _, 0, /)
    ->  atom_concat(Dir, File, Path)
    ;   atomic_list_concat([Dir, /, File], Path)
    ).
directory_file_path(Dir, File, Path) :-
    nonvar(Path),
    !,
    (   nonvar(Dir)
    ->  (   Dir == '.',
            \+ is_absolute_file_name(Path)
        ->  File = Path
        ;   sub_atom(Dir, _, _, 0, /)
        ->  atom_concat(Dir, File, Path)
        ;   atom_concat(Dir, /, TheDir)
        ->  atom_concat(TheDir, File, Path)
        )
    ;   nonvar(File)
    ->  atom_concat(Dir0, File, Path),
        strip_trailing_slash(Dir0, Dir)
    ;   file_directory_name(Path, Dir),
        file_base_name(Path, File)
    ).
directory_file_path(_, _, _) :-
    throw(error(instantiation_error(_), _)).

strip_trailing_slash(Dir0, Dir) :-
    (   atom_concat(D, /, Dir0),
        D \== ''
    ->  Dir = D
    ;   Dir = Dir0
    ).


%!  directory_member(+Directory, -Member, +Options) is nondet.
%
%   True when Member is a path inside Directory.  Options defined are:
%
%     - recursive(+Boolean)
%       If `true` (default `false`), recurse into subdirectories
%     - follow_links(+Boolean)
%       If `true` (default), follow symbolic links.
%     - file_type(+Type)
%       See absolute_file_name/3.
%     - extensions(+List)
%       Only return entries whose extension appears in List.
%     - file_errors(+Errors)
%       How to handle errors.  One of `fail`, `warning` or `error`.
%       Default is `warning`.  Errors notably happen if a directory is
%       unreadable or a link points nowhere.
%     - access(+Access)
%       Only return entries with Access
%     - matches(+GlobPattern)
%       Only return files that match GlobPattern.
%     - exclude(+GlobPattern)
%       Exclude files matching GlobPattern.
%     - exclude_directory(+GlobPattern)
%       Do not recurse into directories matching GlobPattern.
%     - hidden(+Boolean)
%       If `true` (default), also return _hidden_ files.
%
%   This predicate is safe against cycles   introduced by symbolic links
%   to directories.
%
%   The idea for a non-deterministic file   search  predicate comes from
%   Nicos Angelopoulos.

directory_member(Directory, Member, Options) :-
    dict_create(Dict, options, Options),
    (   Dict.get(recursive) == true,
        \+ Dict.get(follow_links) == false
    ->  empty_nb_set(Visited),
        DictOptions = Dict.put(visited, Visited)
    ;   DictOptions = Dict
    ),
    directory_member_dict(Directory, Member, DictOptions).

directory_member_dict(Directory, Member, Dict) :-
    directory_files(Directory, Files, Dict),
    member(Entry, Files),
    \+ special(Entry),
    directory_file_path(Directory, Entry, AbsEntry),
    filter_link(AbsEntry, Dict),
    (   exists_directory(AbsEntry)
    ->  (   filter_dir_member(AbsEntry, Entry, Dict),
            Member = AbsEntry
        ;   filter_directory(Entry, Dict),
            Dict.get(recursive) == true,
            \+ hidden_file(Entry, Dict),
            no_link_cycle(AbsEntry, Dict),
            directory_member_dict(AbsEntry, Member, Dict)
        )
    ;   filter_dir_member(AbsEntry, Entry, Dict),
        Member = AbsEntry
    ).

directory_files(Directory, Files, Dict) :-
    Errors = Dict.get(file_errors),
    !,
    errors_directory_files(Errors, Directory, Files).
directory_files(Directory, Files, _Dict) :-
    errors_directory_files(warning, Directory, Files).

errors_directory_files(fail, Directory, Files) :-
    catch(directory_files(Directory, Files), _, fail).
errors_directory_files(warning, Directory, Files) :-
    catch(directory_files(Directory, Files), E,
          (   print_message(warning, E),
              fail)).
errors_directory_files(error, Directory, Files) :-
    directory_files(Directory, Files).


filter_link(File, Dict) :-
    \+ ( Dict.get(follow_links) == false,
         read_link(File, _, _)
       ).

no_link_cycle(Directory, Dict) :-
    Visited = Dict.get(visited),
    !,
    absolute_file_name(Directory, Canonical,
                       [ file_type(directory)
                       ]),
    add_nb_set(Canonical, Visited, true).
no_link_cycle(_, _).

hidden_file(Entry, Dict) :-
    false == Dict.get(hidden),
    sub_atom(Entry, 0, _, _, '.').

%!  filter_dir_member(+Absolute, +BaseName, +Options)
%
%   True when the given file satisfies the filter expressions.

filter_dir_member(_AbsEntry, Entry, Dict) :-
    Exclude = Dict.get(exclude),
    wildcard_match(Exclude, Entry),
    !, fail.
filter_dir_member(_AbsEntry, Entry, Dict) :-
    Include = Dict.get(matches),
    \+ wildcard_match(Include, Entry),
    !, fail.
filter_dir_member(AbsEntry, _Entry, Dict) :-
    Type = Dict.get(file_type),
    \+ matches_type(Type, AbsEntry),
    !, fail.
filter_dir_member(_AbsEntry, Entry, Dict) :-
    ExtList = Dict.get(extensions),
    file_name_extension(_, Ext, Entry),
    \+ memberchk(Ext, ExtList),
    !, fail.
filter_dir_member(AbsEntry, _Entry, Dict) :-
    Access = Dict.get(access),
    \+ access_file(AbsEntry, Access),
    !, fail.
filter_dir_member(_AbsEntry, Entry, Dict) :-
    hidden_file(Entry, Dict),
    !, fail.
filter_dir_member(_, _, _).

matches_type(directory, Entry) :-
    !,
    exists_directory(Entry).
matches_type(Type, Entry) :-
    \+ exists_directory(Entry),
    user:prolog_file_type(Ext, Type),
    file_name_extension(_, Ext, Entry).


%!  filter_directory(+Entry, +Dict) is semidet.
%
%   Implement the exclude_directory(+GlobPattern) option.

filter_directory(Entry, Dict) :-
    Exclude = Dict.get(exclude_directory),
    wildcard_match(Exclude, Entry),
    !, fail.
filter_directory(_, _).


%!  copy_file(+From, +To) is det.
%
%   Copy a file into a new file or  directory. The data is copied as
%   binary data.

copy_file(From, To) :-
    destination_file(To, From, Dest),
    setup_call_cleanup(
        open(Dest, write, Out, [type(binary)]),
        copy_from(From, Out),
        close(Out)).

copy_from(File, Stream) :-
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        copy_stream_data(In, Stream),
        close(In)).

destination_file(Dir, File, Dest) :-
    exists_directory(Dir),
    !,
    file_base_name(File, Base),
    directory_file_path(Dir, Base, Dest).
destination_file(Dest, _, Dest).


%!  make_directory_path(+Dir) is det.
%
%   Create Dir and all required  components   (like  mkdir  -p). Can
%   raise various file-specific exceptions.

make_directory_path(Dir) :-
    make_directory_path_2(Dir),
    !.
make_directory_path(Dir) :-
    permission_error(create, directory, Dir).

make_directory_path_2(Dir) :-
    exists_directory(Dir),
    !.
make_directory_path_2(Dir) :-
    atom_concat(RealDir, '/', Dir),
    RealDir \== '',
    !,
    make_directory_path_2(RealDir).
make_directory_path_2(Dir) :-
    Dir \== (/),
    !,
    file_directory_name(Dir, Parent),
    make_directory_path_2(Parent),
    E = error(existence_error(directory, _), _),
    catch(make_directory(Dir), E,
          (   exists_directory(Dir)
          ->  true
          ;   throw(E)
          )).

%!  copy_directory(+From, +To) is det.
%
%   Copy the contents of the directory  From to To (recursively). If
%   To is the name of an existing  directory, the _contents_ of From
%   are copied into To. I.e., no  subdirectory using the basename of
%   From is created.

copy_directory(From, To) :-
    (   exists_directory(To)
    ->  true
    ;   make_directory(To)
    ),
    directory_files(From, Entries),
    maplist(copy_directory_content(From, To), Entries).

copy_directory_content(_From, _To, Special) :-
    special(Special),
    !.
copy_directory_content(From, To, Entry) :-
    directory_file_path(From, Entry, Source),
    directory_file_path(To, Entry, Dest),
    (   exists_directory(Source)
    ->  copy_directory(Source, Dest)
    ;   copy_file(Source, Dest)
    ).

special(.).
special(..).

%!  delete_directory_and_contents(+Dir) is det.
%
%   Recursively remove the directory Dir and its contents. If Dir is
%   a symbolic link or symbolic links   inside  Dir are encountered,
%   the links are removed rather than their content. Use with care!

delete_directory_and_contents(Dir) :-
    read_link(Dir, _, _),
    !,
    delete_file(Dir).
delete_directory_and_contents(Dir) :-
    directory_files(Dir, Files),
    maplist(delete_directory_contents(Dir), Files),
    E = error(existence_error(directory, _), _),
    catch(delete_directory(Dir), E,
          (   \+ exists_directory(Dir)
          ->  true
          ;   throw(E)
          )).

delete_directory_contents(_, Entry) :-
    special(Entry),
    !.
delete_directory_contents(Dir, Entry) :-
    directory_file_path(Dir, Entry, Delete),
    (   exists_directory(Delete)
    ->  delete_directory_and_contents(Delete)
    ;   E = error(existence_error(file, _), _),
        catch(delete_file(Delete), E,
              (   \+ exists_file(Delete)
              ->  true
              ;   throw(E)))
    ).

%!  delete_directory_contents(+Dir) is det.
%
%   Remove all content from  directory   Dir,  without  removing Dir
%   itself. Similar to delete_directory_and_contents/2,  if symbolic
%   links are encountered in Dir, the  links are removed rather than
%   their content.

delete_directory_contents(Dir) :-
    directory_files(Dir, Files),
    maplist(delete_directory_contents(Dir), Files).


%!  chmod(+File, +Spec) is det.
%
%   Set the mode of the target file. Spec  is one of `+Mode`, `-Mode` or
%   a plain `Mode`, which adds new   permissions, revokes permissions or
%   sets the exact permissions. `Mode`  itself   is  an integer, a POSIX
%   mode name or a list of POSIX   mode names. Defines names are `suid`,
%   `sgid`, `svtx` and  all names  defined  by  the  regular  expression
%   =|[ugo]*[rwx]*|=. Specifying none of "ugo" is the same as specifying
%   all of them. For example, to make   a  file executable for the owner
%   (user) and group, we can use:
%
%     ```
%     ?- chmod(myfile, +ugx).
%     ```

chmod(File, +Spec) :-
    must_be(ground, Spec),
    !,
    mode_bits(Spec, Bits),
    file_mode_(File, Mode0),
    Mode is Mode0 \/ Bits,
    chmod_(File, Mode).
chmod(File, -Spec) :-
    must_be(ground, Spec),
    !,
    mode_bits(Spec, Bits),
    file_mode_(File, Mode0),
    Mode is Mode0 /\ \Bits,
    chmod_(File, Mode).
chmod(File, Spec) :-
    must_be(ground, Spec),
    !,
    mode_bits(Spec, Bits),
    chmod_(File, Bits).

mode_bits(Spec, Spec) :-
    integer(Spec),
    !.
mode_bits(Name, Bits) :-
    atom(Name),
    !,
    (   file_mode(Name, Bits)
    ->  true
    ;   domain_error(posix_file_mode, Name)
    ).
mode_bits(Spec, Bits) :-
    must_be(list(atom), Spec),
    phrase(mode_bits(0, Bits), Spec).

mode_bits(Bits0, Bits) -->
    [Spec], !,
    (   { file_mode(Spec, B), Bits1 is Bits0\/B }
    ->  mode_bits(Bits1, Bits)
    ;   { domain_error(posix_file_mode, Spec) }
    ).
mode_bits(Bits, Bits) -->
    [].

file_mode(suid, 0o4000).
file_mode(sgid, 0o2000).
file_mode(svtx, 0o1000).
file_mode(Name, Bits) :-
    atom_chars(Name, Chars),
    phrase(who_mask(0, WMask0), Chars, Rest),
    (   WMask0 =:= 0
    ->  WMask = 0o0777
    ;   WMask = WMask0
    ),
    maplist(mode_char, Rest, MBits),
    foldl(or, MBits, 0, Mask),
    Bits is Mask /\ WMask.

who_mask(M0, M) -->
    [C],
    { who_mask(C,M1), !,
      M2 is M0\/M1
    },
    who_mask(M2,M).
who_mask(M, M) -->
    [].

who_mask(o, 0o0007).
who_mask(g, 0o0070).
who_mask(u, 0o0700).

mode_char(r, 0o0444).
mode_char(w, 0o0222).
mode_char(x, 0o0111).

or(B1, B2, B) :-
    B is B1\/B2.

