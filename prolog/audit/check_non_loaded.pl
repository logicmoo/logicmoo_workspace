:- module(check_non_loaded, []).

:- use_module(library(maplist_dcg)).
:- use_module(library(option_utils)).
:- use_module(library(audit/audit)).

:- multifile
    prolog:message//1.

audit:check(non_loaded, Results, OptionL) :-
    option_allchk(OptionL, _, FileChk0 ),
    ( FileChk0 = option_utils:call_2(true, _) ->
      FileChk = option_utils:call_2(( working_directory(Dir, Dir),
				      check_dir_file(Dir, File)), File)
    ; FileChk  = FileChk0
    ),
    check_non_loaded(FileChk, Results).

check_non_loaded(FileChk, Pairs) :-
    findall(Dir-Name,
	    ( call(FileChk, File),
	      directory_file_path(Dir, Name, File)
	    ), DirNameU),
    sort(DirNameU, DirName),
    group_pairs_by_key(DirName, DirNameG),
    findall(warning-(load_info(Dir, L, N)-Excluded),
	    ( member(Dir-NameL, DirNameG),
	      findall(Name, ( member(Name, NameL),
			      directory_file_path(Dir, Name, File),
			      \+ source_file_property(File, modified(_))
			    ), ExcludedL),
	      ExcludedL \= [],
	      length(NameL, N),
	      length(ExcludedL, ExN),
	      L is N - ExN,
	      member(Excluded, ExcludedL)
	    ), Pairs).

prolog:message(acheck(non_loaded)) -->
    ['----------',nl,
     'Non Loaded',nl,
     '----------',nl,
     'The following files are not being loaded, which', nl,
     'means you are not testing them statically', nl, nl].
prolog:message(acheck(non_loaded, load_info(Dir, L, N)-ExcludedL)) -->
    ['At directory ~w, loaded ~w/~w:'-[Dir, L, N],nl],
    maplist_dcg(non_loaded(Dir), ExcludedL).

non_loaded(Dir, Excluded) -->
    ['\t~w/~w:1: non loaded'-[Dir, Excluded], nl].
