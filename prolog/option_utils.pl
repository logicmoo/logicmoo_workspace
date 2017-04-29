/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(option_utils, [select_option_default/3,
                         option_allchk/3,
                         option_dirchk/3,
                         option_allchk/4,
                         option_fromchk/3,
                         source_extension/2,
                         call_2/3,
			 check_alias/3,
                         check_dir_file/3,
                         check_pred/2,
                         check_module/2,
                         from_chk/2,
                         from_chk/3]).

:- reexport(library(module_files)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(from_utils)).
:- use_module(library(implemented_in)).

select_option_default(Holder-Default, OptionL0, OptionL) :-
    select_option(Holder, OptionL0, OptionL, Default).

:- public check_alias/4. % This should be exported since it may be used
                         % indirectly in other places

check_alias(Alias, File, OptionL1) :-
    ( select_option(extensions(EL), OptionL1, OptionL2)
    ->OptionL = [extensions([''|EL])|OptionL2]
    ; OptionL = OptionL1
    ),
    absolute_file_name(Alias, Pattern, [solutions(all)|OptionL]),
    expand_file_name(Pattern, FileL),
    member(File, FileL),
    access_file(File, exist). % exist checked at the end to avoid premature fail

check_dir_file(Dir, File, _) :-
    nonvar(Dir),
    nonvar(File), !,
    directory_file_path(Dir, _, File).
check_dir_file(Dir, File, OptionL) :-
    % here, we need all the files, even if the option specifies only loaded
    % files, otherwise included files without clauses will be ignored
    % directory_source_files(Dir, FileL, [recursive(true), if(false)]),
    absolute_file_name(Dir, AbsDir, [file_type(directory), access(read)]),
    directory_files(AbsDir, Files),
    phrase(src_files(Files, AbsDir, [recursive(true)|OptionL]), SrcFiles),
    member(File, SrcFiles).

source_extension(Type, Ext) :-
    user:prolog_file_type(Ext, Type),
    ( Type \= prolog
    ->true
    ; \+ user:prolog_file_type(Ext, qlf)
    ).

% Based on predicate with same name in prolog_source.pl:

src_files([], _, _) -->
    [].
src_files([H|T], Dir, Options) -->
    { file_name_extension(_, Ext, H),
      ( option(extensions(ExtL), Options)
      ->memberchk(Ext, ExtL)
      ; option(file_type(Type), Options, prolog)
      ->once(source_extension(Type, Ext))
      ),
      directory_file_path(Dir, H, File0),
      absolute_file_name(File0, File,
                         [ file_errors(fail)
                         | Options
                         ])
    },
    !,
    [File],
    src_files(T, Dir, Options).
src_files([H|T], Dir, Options) -->
    { \+ special(H),
      option(recursive(true), Options),
      directory_file_path(Dir, H, SubDir),
      exists_directory(SubDir),
      !,
      catch(directory_files(SubDir, Files), _, fail)
    },
    !,
    src_files(Files, SubDir, Options),
    src_files(T, Dir, Options).
src_files([_|T], Dir, Options) -->
    src_files(T, Dir, Options).

special(.).
special(..).

option_files(File, FileGen0-OptionL0, FileGen-OptionL1) :-
    foldl(select_option_default,
	  [files(Files)-Files,
	   file( AFile)-AFile,
	   file_type(Type)-prolog
	  ], OptionL0, OptionL1),
    OptionL=[file_type(Type)|OptionL1],
    ( nonvar(Files)
    ->( nonvar(AFile)
      ->flatten([AFile|Files], FileL)
      ; AFile = File,
        flatten(Files, FileL)
      ),
      FileGen0 = ( member(Alias, FileL),
                   check_alias(Alias, File, OptionL),
                   FileGen
                 )
    ; nonvar(AFile)
    ->FileGen0 = ( check_alias(AFile, File, OptionL),
                   FileGen
                 )
    ; AFile = File,
      FileGen0 = FileGen
    ).

option_exclude_files(File, FG-OptionL0, FG-OptionL1) :-
    foldl(select_option_default,
	  [exclude_files(ExFileL)-[],
	   file_type(Type)-prolog
	  ], OptionL0, OptionL1),
    OptionL=[file_type(Type)|OptionL1],
    ( ExFileL = []
    ->true
    ; freeze(File, \+ ( member(ExAlias, ExFileL),
                        check_alias(ExAlias, File, OptionL)
                      ))
    ).

option_exclude_fdirs(File, FG-OptionL0, FG-OptionL1) :-
    foldl(select_option_default,
	  [exclude_dirs(ExDirL)-[],
	   file_type(Type)-directory
	  ], OptionL0, OptionL1),
    foldl(select_option_default,
	  [extensions(E)-E
	  ], OptionL1, OptionL2),
    OptionL=[file_type(Type)|OptionL2],
    ( ExDirL = []
    ->true
    ; freeze(File, \+ ( member(ExAlias, ExDirL),
                        check_alias(ExAlias, ExDir, OptionL),
                        check_dir_file(ExDir, File, OptionL1)
                      ))
    ).

option_fdirs(File, FileGen0-OptionL0, FileGen-OptionL1) :-
    foldl(select_option_default,
	 [dirs(Dirs)-Dirs,
	  dir( ADir)-ADir,
	  file_type(Type)-directory
	 ], OptionL0, OptionL1),
    foldl(select_option_default,
	  [extensions(E)-E
	  ], OptionL1, OptionL2),
    OptionL=[file_type(Type)|OptionL2],
    ( nonvar(Dirs)
    ->( nonvar(ADir)
      ->flatten([ADir|Dirs], DirL)
      ; ADir = Dir,
        flatten(Dirs, DirL)
      ),
      FileGen0 = ( member(Alias, DirL),
                   check_alias(Alias, Dir, OptionL),
                   check_dir_file(Dir, File, OptionL1),
                   FileGen
                 )
    ; nonvar(ADir)
    ->FileGen0 = ( check_alias(ADir, Dir, OptionL),
                   check_dir_file(Dir, File, OptionL1),
                   FileGen
                 )
    ; FileGen0 = FileGen
    ).

option_exclude_dirs(Dir, DG-OptionL0, DG-OptionL1) :-
    foldl(select_option_default,
	 [exclude_dirs(ExDirL)-[],
	  file_type(Type)-directory
	 ], OptionL0, OptionL1),
    foldl(select_option_default,
	  [extensions(E)-E
	  ], OptionL1, OptionL2),
    OptionL=[file_type(Type)|OptionL2],
    ( ExDirL = []
    ->true
    ; freeze(Dir, \+ ( member(ExAlias, ExDirL),
                       check_alias(ExAlias, Dir, OptionL)
                     ))
    ).

option_dirs(Dir, DirGen0-OptionL0, DirGen-OptionL1) :-
    foldl(select_option_default,
	  [dirs(Dirs)-Dirs,
	   dir( ADir)-ADir,
	   file_type(Type)-directory
	  ], OptionL0, OptionL1),
    foldl(select_option_default,
	  [extensions(E)-E
	  ], OptionL1, OptionL2),
    OptionL=[file_type(Type)|OptionL2],
    ( nonvar(Dirs)
    ->( nonvar(ADir)
      ->flatten([ADir|Dirs], DirL)
      ; ADir = Dir,
        flatten(Dirs, DirL)
      ),
      DirGen0 = ( member(Alias, DirL),
                  check_alias(Alias, Dir, OptionL),
                  DirGen
                )
    ; nonvar(ADir)
    ->DirGen0 = ( check_alias(ADir, Dir, OptionL),
                  DirGen
                )
    ; ADir = Dir,
      DirGen0 = DirGen
    ).

check_pred(Head, File) :-
    implemented_in(Head, From, _),
    from_to_file(From, File).

option_preds(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(preds(HeadL), OptionL0, OptionL, HeadL),
    ( is_list(HeadL)
    ->FileGen0 = ( member(Head, HeadL),
                   check_pred(Head, File),
                   FileGen
                 )
    ; nonvar(HeadL)
    ->FileGen0 = ( check_pred(HeadL, File),
                   FileGen
                 )
    ; FileGen0 = FileGen
    ).

check_module(M, File) :-
    module_files(M, FileL),
    member(File, FileL).

option_module(M, File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(module(M), OptionL0, OptionL1, M),
    select_option(if(Loaded), OptionL1, OptionL, false),
    ( nonvar(M)
    ->FileGen0 = ( module_file(M, File),
                   FileGen
                 )
    ; Loaded = false
    ->FileGen0 = ( FileGen,
                   ignore(module_file(M, File))
                 )
    ; FileGen0 = ( FileGen,
                   once(module_file(M, File))
                 )
    ).

option_modules(M, File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(modules(ML), OptionL0, OptionL, []),
    ( ML \= [],
      var(M)
    ->FileGen0 = ( member(M, ML),
                   check_module(M, File),
                   FileGen
                 )
    ; FileGen0 = FileGen
    ).

option_mod_prop(M, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(module_property(Prop), OptionL0, OptionL, []),
    ( Prop \= []
    ->FileGen0 = ( module_property(M, Prop),
                   FileGen
                 )
    ; FileGen0 = FileGen
    ).

option_allchk(M, File) -->
    option_exclude_files(File),
    option_exclude_fdirs(File),
    option_mod_prop(M),
    option_module(M, File),
    option_modules(M, File),
    option_files(File),
    option_preds(File),
    option_fdirs(File).

option_filechk(File) -->
    option_exclude_files(File),
    option_exclude_fdirs(File),
    option_files(File),
    option_preds(File),
    option_fdirs(File).

option_dirchk_(Dir) -->
    option_exclude_dirs(Dir),
    option_dirs(Dir).

:- meta_predicate call_2(0,?,?).
call_2(Goal, File, File) :- call(Goal).

:- meta_predicate from_chk(1,?).
from_chk(FileChk, From) :-
    ( nonvar(From)
    ->from_to_file(From, File),
      call(FileChk, File)
    ; true
    ).

:- meta_predicate from_chk(0,?,?).
from_chk(Goal, File, From) :-
    ( nonvar(From)
    ->from_to_file(From, File),
      call(Goal)
    ; true
    ).

option_allchk(OptionL0, OptionL, option_utils:call_2(FileGen, File)) :-
    option_filechk(File, FileGen-OptionL0, true-OptionL).

option_fromchk(OptionL0, OptionL, option_utils:from_chk(FileGen, File)) :-
    option_filechk(File, FileGen-OptionL0, true-OptionL).

option_dirchk(OptionL0, OptionL, option_utils:call_2(DirGen, Dir)) :-
    option_dirchk_(Dir, DirGen-OptionL0, true-OptionL).
