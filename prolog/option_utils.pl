:- module(option_utils, [option_allchk/3,
			 option_dirchk/3,
			 option_allchk/4,
			 call_2/3,
			 check_alias/3,
			 check_pred/2,
			 check_module/2]).

:- use_module(library(location_utils)).
:- use_module(library(module_files)).

check_alias(Type, Alias, File) :-
    absolute_file_name(Alias, Pattern, [file_type(Type),
					solutions(all)]),
    expand_file_name(Pattern, FileL),
    member(File, FileL).

option_alias(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(alias(Alias), OptionL0, OptionL, Alias),
    ( nonvar(Alias)
    ->FileGen0 = ( check_alias(directory, Alias, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_aliases(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(aliases(AliasL), OptionL0, OptionL, AliasL),
    ( nonvar(AliasL)
    ->FileGen0 = ( member(Alias, AliasL),
		   check_alias(directory, Alias, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_fdir(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(dir(Alias), OptionL0, OptionL, Alias),
    ( nonvar(Alias)
    ->FileGen0 = ( check_alias(directory, Alias, Dir),
		   directory_file_path(Dir, _, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_fdirs(File, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(dirs(AliasL), OptionL0, OptionL, []),
    ( AliasL \= []
    ->FileGen0 = ( member(Alias, AliasL),
		   check_alias(directory, Alias, Dir),
		   directory_file_path(Dir, _, File),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_dir(Dir, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(dir(Alias), OptionL0, OptionL, Alias),
    ( nonvar(Alias)
    ->FileGen0 = ( check_alias(directory, Alias, Dir),
		   FileGen
		 )
    ; FileGen0 = FileGen
    ).

option_dirs(Dir, FileGen0-OptionL0, FileGen-OptionL) :-
    select_option(dirs(AliasL), OptionL0, OptionL, []),
    ( AliasL \= []
    ->FileGen0 = ( member(Alias, AliasL),
		   check_alias(directory, Alias, Dir),
		   FileGen
		 )
    ; FileGen0 = FileGen
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
    select_option(module(M), OptionL0, OptionL, M),
    ( nonvar(M), M = (-)
    ->FileGen0 = FileGen
    ; FileGen0 = ( check_module(M, File),
		   FileGen
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
    ; var(M)
    ->FileGen0 = ( current_module(M),
		   FileGen
		 )		% TODO: allow unknown module
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

option_dir_dirs(File) -->
    option_dir(File),
    option_dirs(File).

:- meta_predicate call_2(0,?,?).
call_2(Goal, File, File) :- call(Goal).

option_allchk(OptionL1, OptionL, call_2(FileGen, File)) :-
    option_filechk(File, FileGen-OptionL1, true-OptionL).

option_dirchk(OptionL1, OptionL, call_2(FileGen, File)) :-
    option_dir_dirs(File, FileGen-OptionL1, true-OptionL).
