:- module(option_utils, [option_allchk/3,
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

:- use_module(library(from_utils)).
:- use_module(library(implemented_in)).
:- reexport(library(module_files)).

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

option_alias(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(alias(Alias), OptionL0, OptionL, Alias),
    ( nonvar(Alias)
    ->FileGen0 = ( check_alias(prolog, Alias, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_aliases(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(aliases(AliasL), OptionL0, OptionL, AliasL),
    ( nonvar(AliasL)
    ->FileGen0 = ( member(Alias, AliasL),
		   check_alias(prolog, Alias, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_fdir(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(dir(Alias), OptionL0, OptionL, Alias),
    ( nonvar(Alias)
    ->FileGen0 = ( check_alias(directory, Alias, Dir),
		   check_dir_file(Dir, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_fdirs(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(dirs(AliasL), OptionL0, OptionL, []),
    ( AliasL \= []
    ->FileGen0 = ( member(Alias, AliasL),
		   check_alias(directory, Alias, Dir),
		   check_dir_file(Dir, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_dir(Dir, DirGen0-OptionL0, DirGen-OptionL) :-
    select_option(dir(Alias), OptionL0, OptionL, Alias),
    ( nonvar(Alias)
    ->DirGen0 = ( check_alias(directory, Alias, Dir),
		  DirGen
		)
    ; DirGen0 = DirGen
    ).

option_dirs(Dir, DirGen0-OptionL0, DirGen-OptionL) :-
    select_option(dirs(AliasL), OptionL0, OptionL, []),
    ( AliasL \= []
    ->DirGen0 = ( member(Alias, AliasL),
		  check_alias(directory, Alias, Dir),
		  DirGen
		)
    ; DirGen0 = DirGen
    ).

check_pred(Head, File) :-
    implemented_in(Head, From, _),
    from_to_file(From, File).
		   
option_pred(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(pred(Head), OptionL0, OptionL, []),
    ( Head \= []
    ->FileGen0 = ( check_pred(Head, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_preds(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(preds(HeadL), OptionL0, OptionL, []),
    ( Head \= []
    ->FileGen0 = ( member(Head, HeadL),
		   check_pred(Head, File),
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
    option_mod_prop(M),
    option_module(M, File),
    option_modules(M, File),
    option_filechk(File).

option_filechk(File) -->
    option_alias(File),
    option_aliases(File),
    option_pred(File),
    option_preds(File),
    option_fdir(File),
    option_fdirs(File).

option_dir_dirs(Dir) -->
    option_dir(Dir),
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

option_allchk(OptionL1, OptionL, option_utils:call_2(FileGen, File)) :-
    option_filechk(File, FileGen-OptionL1, true-OptionL).

option_fromchk(OptionL1, OptionL, option_utils:from_chk(FileGen, File)) :-
    option_filechk(File, FileGen-OptionL1, true-OptionL).

option_dirchk(OptionL1, OptionL, option_utils:call_2(DirGen, Dir)) :-
    option_dir_dirs(Dir, DirGen-OptionL1, true-OptionL).
