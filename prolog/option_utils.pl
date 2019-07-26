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
                         option_fromchk/5,
                         source_extension/2,
                         call_2/3,
                         check_dir_file/3,
                         check_files/3,
                         check_pred/2,
                         check_module/2,
                         from_chk/2,
                         from_chk/3]).

:- reexport(library(module_files)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(from_utils)).
:- use_module(library(implemented_in)).

:- multifile user:prolog_file_type/2.
:- dynamic   user:prolog_file_type/2.

% TBD: This module requires optimization

select_option_default(Holder-Default, Options1, Options) :-
    select_option(Holder, Options1, Options, Default).

curr_alias_file(AliasL, File, Options1) :-
    member(Alias, AliasL),
    ( select_option(extensions(EL), Options1, Options2)
    ->Options = [extensions([''|EL])|Options2]
    ; Options = Options1
    ),
    absolute_file_name(Alias, Pattern, [solutions(all)|Options]),
    expand_file_name(Pattern, FileL),
    member(File, FileL).

alias_files(AliasL, FileL, Options) :-
    findall(File, curr_alias_file(AliasL, File, Options), FileL).

:- public check_files/3. % This should be exported since it may be used
                         % indirectly in other places
check_files(FileL, File, Loaded) :-
    member(File, FileL),
    check_file(Loaded, File).

check_file(false, File) :- access_file(File, exist).  % exist checked at the end to avoid premature fail
check_file(true,  _).

check_dir_file(Dir, File, _) :-
    nonvar(Dir),
    nonvar(File), !,
    directory_file_path(Dir, _, File).
check_dir_file(Dir, File, Options) :-
    % here, we need all the files, even if the option specifies only loaded
    % files, otherwise included files without clauses will be ignored
    % directory_source_files(Dir, FileL, [recursive(true), if(false)]),
    option(if(Loaded), Options, false),
    ( Loaded = false
    ->absolute_file_name(Dir, AbsDir, [file_type(directory), access(read)]),
      directory_files(AbsDir, Files),
      phrase(src_files(Files, AbsDir, [recursive(true)|Options]), SrcFiles),
      member(File, SrcFiles)
    ; absolute_file_name(Dir, AbsDir, [file_type(directory), solutions(all)]),
      distinct(File, module_file(_, File)),
      directory_file_path(AbsDir, _, File)
    ).

source_extension(Type, Ext) :-
    user:prolog_file_type(Ext, Type),
    ( Type \= prolog
    ->true
    ; \+ user:prolog_file_type(Ext, qlf)
    ).

% Based on predicate with same name in prolog_source.pl:

src_files([], _, _) --> [].
src_files([H|T], Dir, Options1) -->
    { \+ special(H),
      file_name_extension(_, Ext, H),
      select_option(extensions(ExtL), Options1, Options, []),
      ( ExtL \= []
      ->memberchk(Ext, ExtL)
      ; option(file_type(Type), Options, prolog)
      ->once(source_extension(Type, Ext))
      ),
      directory_file_path(Dir, H, File1),
      absolute_file_name(File1, File,
                         [ file_errors(fail)
                         | Options
                         ])
    },
    !,
    [File],
    src_files(T, Dir, Options1).
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

option_files(File, FileGen1-Options1, FileGen-Options2) :-
    foldl(select_option_default,
	  [files(Files)-Files,
	   file( AFile)-AFile,
	   file_type(Type)-prolog,
           loaded(Loaded)-false
	  ], Options1, Options2),
    Options=[file_type(Type)|Options2],
    ( nonvar(Files)
    ->( nonvar(AFile)
      ->flatten([AFile|Files], AliasL)
      ; AFile = File,
        flatten(Files, AliasL)
      ),
      alias_files(AliasL, FileL, Options),
      FileGen1 = ( check_files(FileL, File, Loaded),
                   FileGen
                 )
    ; nonvar(AFile)
    ->alias_files([AFile], FileL, Options),
      FileGen1 = ( check_files(FileL, File, Loaded),
                   FileGen
                 )
    ; AFile = File,
      FileGen1 = FileGen
    ).

option_exclude_files(File, FG-Options1, FG-Options2) :-
    foldl(select_option_default,
	  [exclude_files(ExFileL)-[],
	   file_type(Type)-prolog,
           loaded(Loaded)-false
	  ], Options1, Options2),
    Options=[file_type(Type)|Options2],
    ( ExFileL = []
    ->true
    ; alias_files(ExFileL, FileL, Options),
      freeze(File, \+ check_files(FileL, File, Loaded))
    ).

option_exclude_fdirs(File, FG-Options1, FG-Options2) :-
    foldl(select_option_default,
	  [exclude_dirs(ExDirL)-[],
	   file_type(Type)-directory,
           loaded(Loaded)-false
	  ], Options1, Options2),
    foldl(select_option_default,
	  [extensions(E)-E
	  ], Options2, Options3),
    Options=[file_type(Type)|Options3],
    ( ExDirL = []
    ->true
    ; alias_files(ExDirL, DirL, Options),
      freeze(File, \+ ( check_files(DirL, ExDir, Loaded),
                        check_dir_file(ExDir, File, Options2)
                      ))
    ).

option_fdirs(File, FileGen1-Options1, FileGen-Options2) :-
    foldl(select_option_default,
	 [dirs(Dirs)-Dirs,
	  dir( ADir)-ADir,
	  file_type(Type)-directory,
          loaded(Loaded)-false
	 ], Options1, Options2),
    foldl(select_option_default,
	  [extensions(E)-E
	  ], Options2, Options3),
    Options=[file_type(Type)|Options3],
    ( nonvar(Dirs)
    ->( nonvar(ADir)
      ->flatten([ADir|Dirs], ADirL)
      ; ADir = Dir,
        flatten(Dirs, ADirL)
      ),
      alias_files(ADirL, DirL, Options),
      FileGen1 = ( check_files(DirL, Dir, Loaded),
                   check_dir_file(Dir, File, Options2),
                   FileGen
                 )
    ; nonvar(ADir)
    ->alias_files([ADir], DirL, Options),
      FileGen1 = ( check_files(DirL, Dir, Loaded),
                   check_dir_file(Dir, File, Options2),
                   FileGen
                 )
    ; FileGen1 = FileGen
    ).

option_exclude_dirs(Dir, DG-Options1, DG-Options2) :-
    foldl(select_option_default,
	 [exclude_dirs(ExDirL)-[],
	  file_type(Type)-directory,
          loaded(Loaded)-false
	 ], Options1, Options2),
    foldl(select_option_default,
	  [extensions(E)-E
	  ], Options2, Options3),
    Options=[file_type(Type)|Options3],
    ( ExDirL = []
    ->true
    ; alias_files(ExDirL, DirL, Options),
      freeze(Dir, \+ check_files(DirL, Dir, Loaded))
    ).

option_dirs(Dir, DirGen1-Options1, DirGen-Options2) :-
    foldl(select_option_default,
	  [dirs(Dirs)-Dirs,
	   dir( ADir)-ADir,
	   file_type(Type)-directory,
           loaded(Loaded)-false
	  ], Options1, Options2),
    foldl(select_option_default,
	  [extensions(E)-E
	  ], Options2, Options3),
    Options=[file_type(Type)|Options3],
    ( nonvar(Dirs)
    ->( nonvar(ADir)
      ->flatten([ADir|Dirs], ADirL)
      ; ADir = Dir,
        flatten(Dirs, ADirL)
      ),
      alias_files(ADirL, DirL, Options),
      DirGen1 = ( check_files(DirL, Dir, Loaded),
                  DirGen
                )
    ; nonvar(ADir)
    ->alias_files([ADir], DirL, Options),
      DirGen1 = ( check_files(DirL, Dir, Loaded),
                  DirGen
                )
    ; ADir = Dir,
      DirGen1 = DirGen
    ).

check_pred(Head, File) :-
    implemented_in(Head, From, _),
    from_to_file(From, File).

option_preds(File, FileGen1-Options1, FileGen-Options) :-
    select_option(preds(HeadL), Options1, Options, HeadL),
    ( is_list(HeadL)
    ->FileGen1 = ( member(Head, HeadL),
                   check_pred(Head, File),
                   FileGen
                 )
    ; nonvar(HeadL)
    ->FileGen1 = ( check_pred(HeadL, File),
                   FileGen
                 )
    ; FileGen1 = FileGen
    ).

check_module(M, File) :-
    distinct(File, module_file(M, File)).

option_module(M, File, FileGen1-Options1, FileGen-Options) :-
    select_option(module(M), Options1, Options, M),
    option(if(Loaded), Options, false),
    ( nonvar(M)
    ->FileGen1 = ( module_file(M, File),
                   FileGen
                 )
    ; Loaded = false
    ->FileGen1 = ( FileGen,
                   ignore(module_file(M, File))
                 )
    ; FileGen1 = ( FileGen,
                   MFG
                 ),
      freeze(FileGen,
             ( FileGen = true
             ->MFG = module_file(M, File)
             ; MFG = once(module_file(M, File))
             ))
    ).

option_modules(M, File, FileGen1-Options1, FileGen-Options) :-
    select_option(modules(ML), Options1, Options, []),
    ( ML \= [],
      var(M)
    ->FileGen1 = ( member(M, ML),
                   check_module(M, File),
                   FileGen
                 )
    ; FileGen1 = FileGen
    ).

option_mod_prop(M, FileGen1-Options1, FileGen-Options) :-
    select_option(module_property(Prop), Options1, Options, []),
    ( Prop \= []
    ->FileGen1 = ( module_property(M, Prop),
                   FileGen
                 )
    ; FileGen1 = FileGen
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

option_allchk(Options1, Options, option_utils:call_2(FileGen, File)) :-
    option_filechk(File, FileGen-Options1, true-Options).

option_fromchk(Options1, Options, option_utils:from_chk(FileGen, File)) :-
    option_filechk(File, FileGen-Options1, true-Options).

option_fromchk(M, File, Options1, Options, option_utils:from_chk(FileMGen, File)) :-
    option_allchk(M, File, FileMGen-Options1, true-Options).

option_dirchk(Options1, Options, option_utils:call_2(DirGen, Dir)) :-
    option_dirchk_(Dir, DirGen-Options1, true-Options).
