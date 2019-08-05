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
                         option_module_files/2,
                         option_files/2,
                         option_dirs/2,
                         source_extension/2,
                         check_dir_file/3,
                         check_pred/2,
                         check_module/2
                        ]).

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

curr_alias_file(AliasL, EL, Loaded, File, Options1) :-
    member(Alias, AliasL),
    ( EL = (-)
    ->Options = Options1
    ; Options = [extensions([''|EL])|Options1]
    ),
    absolute_file_name(Alias, Pattern, [solutions(all)|Options]),
    expand_file_name(Pattern, FileL),
    member(File, FileL),
    check_file(Loaded, File).

alias_files(AliasL, EL, Loaded, FileL, Options) :-
    findall(File, curr_alias_file(AliasL, EL, Loaded, File, Options), FileL).

check_file(true, File) :- access_file(File, exist).  % exist checked at the end to avoid premature fail
check_file(loaded,  _).

:- table module_file_/2 as subsumptive.

module_file_(M, F) :- module_file(M, F).

check_module(Module, File) :-
    distinct(File, module_file_(Module, File)).

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

option_files(File, EL, Loaded, Options1, Options2) :-
    foldl(select_option_default,
	  [files(Files)-Files,
	   file( AFile)-AFile
	  ], Options1, Options2),
    merge_options(Options2, [file_type(prolog)], Options),
    ( ( nonvar(Files)
      ->( nonvar(AFile)
        ->flatten([AFile|Files], AliasL)
        ; AFile = File,
          flatten(Files, AliasL)
        )
      ; nonvar(AFile)
      ->AliasL = [AFile]
      )
    ->alias_files(AliasL, EL, Loaded, FileL, Options),
      member(File, FileL)
    ; AFile = File
    ).

option_exclude_files(EL, Loaded, FileL, Options1, Options2) :-
    foldl(select_option_default,
	  [exclude_files(ExFileL)-[]
	  ], Options1, Options2),
    merge_options(Options2, [file_type(prolog)], Options),
    alias_files(ExFileL, EL, Loaded, FileL, Options).

option_exclude_fdirs(ExDirL, Options1, Options2) :-
    foldl(select_option_default,
	  [exclude_dirs(ExADirL)-[]
	  ], Options1, Options2),
    merge_options([file_type(directory)], Options2, Options),
    alias_files(ExADirL, -, true, ExDirL, Options).

option_fdirs(File, Loaded, Options1, Options2) :-
    foldl(select_option_default,
	 [dirs(Dirs)-Dirs,
	  dir( ADir)-ADir
	 ], Options1, Options2),
    merge_options([file_type(directory)], Options2, Options),
    ( ( nonvar(Dirs)
      ->( nonvar(ADir)
        ->flatten([ADir|Dirs], ADirL)
        ; ADir = Dir,
          flatten(Dirs, ADirL)
        )
      ; nonvar(ADir)
      ->ADirL = [ADir]
      )
    ->alias_files(ADirL, -, true, DirL, Options),
      ( Loaded = true
      ->Params = source(Options2)
      ; findall(F, module_file_(_, F), LFileU),
        sort(LFileU, LFileL),
        Params = loaded(LFileL)
      ),
      member(Dir, DirL),
      check_dir_file(Params, Dir, File)
    ; true
    ).

% here, we need all the files, even if the option specifies only loaded
% files, otherwise included files without clauses will be ignored
% directory_source_files(Dir, FileL, [recursive(true), if(false)]),

check_dir_file(source(Options), Dir, File) :-
    directory_files(Dir, Files),
    phrase(src_files(Files, Dir, [recursive(true)|Options]), FileU),
    sort(FileU, FileL),
    member(File, FileL).

check_dir_file(loaded(FileL), Dir, File) :-
    % Note: here we can not use distinct(File, module_file(_, File)) because
    % that will be too slow, instead we findall and deduplicate via sort/2
    member(File, FileL),
    directory_file_path(Dir, _, File).

option_exclude_dirs(EL, DirL, Options1, Options2) :-
    foldl(select_option_default,
	 [exclude_dirs(ExDirL)-[]
	 ], Options1, Options2),
    merge_options([file_type(directory)], Options2, Options),
    alias_files(ExDirL, EL, true, DirL, Options).

option_dirs(EL, Dir, Options1, Options2) :-
    foldl(select_option_default,
	  [dirs(Dirs)-Dirs,
	   dir( ADir)-ADir
	  ], Options1, Options2),
    merge_options([file_type(directory)], Options2, Options),
    ( nonvar(Dirs)
    ->( nonvar(ADir)
      ->flatten([ADir|Dirs], ADirL)
      ; ADir = Dir,
        flatten(Dirs, ADirL)
      )
    ; nonvar(ADir)
    ->ADirL = [ADir]
    ),
    alias_files(ADirL, EL, true, DirL, Options),
    member(Dir, DirL).

check_pred(Head, File) :-
    implemented_in(Head, From, _),
    from_to_file(From, File).

option_preds(File, Options1, Options) :-
    select_option(preds(HeadL), Options1, Options, HeadL),
    ( is_list(HeadL)
    ->member(Head, HeadL),
      check_pred(Head, File)
    ; nonvar(HeadL)
    ->check_pred(HeadL, File)
    ; true
    ).

option_file(M, File) -->
    foldl(select_option_default,
          [module_files(MFileD)-(-)
          ]),
    {MFileD \= (-)},
    !,
    { get_dict(M, MFileD, FileD),
      get_dict(File, FileD, _)
    }.
option_file(M, File) -->
    foldl(select_option_default,
          [if(Loaded)-loaded,
           module_property(Prop)-[],
           module(M)-M,
           modules(ML)-[],
           extensions(EL)-(-)
          ]),
    option_exclude_files(EL, Loaded, ExFileL),
    option_exclude_fdirs(ExDirL),
    option_fdirs(File, Loaded),
    option_preds(File),
    option_files(File, EL, Loaded),
    { \+ member(File, ExFileL),
      \+ ( member(ExDir, ExDirL),
           directory_file_path(ExDir, _, File)
         ),
      ( ML \= []
      ->member(M, ML)
      ; true
      ),
      ( nonvar(M)
      ->module_file_(M, File)
      ; Loaded = true
      ->ignore(( module_file_(M, File)
               ; M = (-)
               ))
      ; var(File)
      ->module_file_(M, File)
      ; once(module_file_(M, File))
      ),
      ( Prop \= []
      ->module_property(M, Prop)
      ; true
      )
    }.

option_dir(Dir) -->
    foldl(select_option_default,
          [extensions(EL)-(-)
          ]),
    option_exclude_dirs(EL, ExDirL),
    option_dirs(EL, Dir),
    {\+ member(Dir, ExDirL)}.

to_nv(Name, Name=_).

pair_nv(M-FileL, M=FileD) :-
    list_dict(FileL, file, FileD).

option_module_files(Options, MFileD) :-
    option(module_files(MFileD), Options),
    !.
option_module_files(Options, MFileD) :-
    findall(M-File, option_file(M, File, Options, _), MFileU),
    keysort(MFileU, MFileS),
    group_pairs_by_key(MFileS, MFileL),
    maplist(pair_nv, MFileL, MFileNV),
    dict_create(MFileD, mfile, MFileNV).

list_dict(ElemU, Key, ElemD) :-
    sort(ElemU, ElemL),
    maplist(to_nv, ElemL, ElemKVL),
    dict_create(ElemD, Key, ElemKVL).

option_files(Options, FileD) :-
    findall(File, option_file(_, File, Options, _), FileL),
    list_dict(FileL, file, FileD).

option_dirs(Options, DirD) :-
    findall(Dir, option_dir(Dir, Options, _), DirU),
    list_dict(DirU, dir, DirD).
