/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(option_utils, [select_option_default/3,
			 option_allchk/3,
			 option_dirchk/3,
			 option_allchk/4,
			 option_fromchk/3,
			 call_2/3,
			 check_alias/3,
			 check_dir_file/2,
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

check_alias(Type, Alias, File) :-
    absolute_file_name(Alias, Pattern, [file_type(Type), solutions(all)]),
    expand_file_name(Pattern, FileL),
    member(File, FileL),
    access_file(File, exist). % exist checked at the end to avoid premature fail

check_dir_file(Dir, File) :-
    nonvar(Dir),
    nonvar(File), !,
    directory_file_path(Dir, _, File).
check_dir_file(Dir, File) :-
    % here, we need all the files, even if the option specifies only loaded
    % files, otherwise included files without clauses will be ignored
    directory_source_files(Dir, FileL, [recursive(true), if(false)]),
    member(File, FileL).

option_files(File, FileGen0-OptionL0, FileGen-OptionL) :-
    foldl(select_option_default,
		[files(Files)-Files,
		 file( AFile)-AFile
		], OptionL0, OptionL),
    ( nonvar(Files)
    ->( nonvar(AFile)
      ->flatten([AFile|Files], FileL)
      ; AFile = File,
	flatten(Files, FileL)
      ),
      FileGen0 = ( member(Alias, FileL),
		   check_alias(prolog, Alias, File),
		   FileGen
		 )
    ; nonvar(AFile)
    ->FileGen0 = ( check_alias(prolog, AFile, File),
		   FileGen
		 )
    ; AFile = File,
      FileGen0 = FileGen
    ).

option_exclude_files(File, FG-OptionL0, FG-OptionL) :-
    foldl(select_option_default,
		[exclude_files(ExFileL)-[]
		], OptionL0, OptionL),
    ( ExFileL = []
    ->true
    ; freeze(File, \+ ( member(ExAlias, ExFileL),
			check_alias(prolog, ExAlias, File)
		      ))
    ).

option_exclude_fdirs(File, FG-OptionL0, FG-OptionL) :-
    foldl(select_option_default,
		[exclude_dirs(ExDirL)-[]
		], OptionL0, OptionL),
    ( ExDirL = []
    ->true
    ; freeze(File, \+ ( member(ExAlias, ExDirL),
			check_alias(directory, ExAlias, ExDir),
			check_dir_file(ExDir, File)
		      ))
    ).

option_fdirs(File, FileGen0-OptionL0, FileGen-OptionL) :-
    foldl(select_option_default,
		[dirs(Dirs)-Dirs,
		 dir( ADir)-ADir
		],
		OptionL0, OptionL),
    ( nonvar(Dirs)
    ->( nonvar(ADir)
      ->flatten([ADir|Dirs], DirL)
      ; ADir = Dir,
	flatten(Dirs, DirL)
      ),
      FileGen0 = ( member(Alias, DirL),
		   check_alias(directory, Alias, Dir),
		   check_dir_file(Dir, File),
		   FileGen
		 )
    ; nonvar(ADir)
    ->FileGen0 = ( check_alias(directory, ADir, Dir),
		   check_dir_file(Dir, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_exclude_dirs(Dir, DG-OptionL0, DG-OptionL) :-
    foldl(select_option_default,
		[exclude_dirs(ExDirL)-[]
		], OptionL0, OptionL),
    ( ExDirL = []
    ->true
    ; freeze(Dir, \+ ( member(ExAlias, ExDirL),
		       check_alias(directory, ExAlias, Dir)
		     ))
    ).

option_dirs(Dir, DirGen0-OptionL0, DirGen-OptionL) :-
    foldl(select_option_default,
		[dirs(Dirs)-Dirs,
		 dir( ADir)-ADir
		],
		OptionL0, OptionL),
    ( nonvar(Dirs)
    ->( nonvar(ADir)
      ->flatten([ADir|Dirs], DirL)
      ; ADir = Dir,
	flatten(Dirs, DirL)
      ),
      DirGen0 = ( member(Alias, DirL),
		  check_alias(directory, Alias, Dir),
		  DirGen
		)
    ; nonvar(ADir)
    ->DirGen0 = ( check_alias(directory, ADir, Dir),
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
