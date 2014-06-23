:- module(check_non_loaded, [loaded_files/2,
			     existing_files/2,
			     remainder_files/2
			    ]).

:- use_module(library(included_files)).
:- use_module(library(maplist_dcg)).
:- use_module(library(location_utils)).

:- multifile
    prolog:message//1.

audit:check(non_loaded, _, Results, OptionL) :-
    option_dirchk(OptionL, in_dir(DirL)),
    findall(Result, check_non_loaded(DirL, Result), ResultL),
    append(ResultL, Results).

loaded_files(Dir, Loaded) :-
    directory_source_files(Dir, Used, [recursive(true)]),
    included_files(Used, LoadedL, [Used]),
    append(LoadedL, ULoaded),
    sort(ULoaded, Loaded).

existing_files(Dir, Exists) :-
    directory_source_files(Dir, UExists, [recursive(true), if(true)]),
    sort(UExists, Exists).
existing_files(r_true, []).

remainder_files(Dir, Remainder) :-
    loaded_files(Dir, Loaded),
    existing_files(Dir, Exists),
    ord_subtract(Exists, Loaded, Remainder).

check_non_loaded(DirL, Pairs) :-
    member(Dir, DirL),
    loaded_files(Dir, Loaded),
    existing_files(Dir, Exists),
    ord_subtract(Exists, Loaded, Remainder),
    ( Remainder \== []
    ->length(Exists, E),
      intersection(Exists, Loaded, Differ),
      length(Differ, L),
      maplist(add_warning_key(load_info(Dir, L, E)), Remainder, Pairs)
    ; Pairs = []
    ).

add_warning_key(I, F, warning-(I-F)).

prolog:message(acheck(non_loaded)) -->
    ['----------',nl,
     'Non Loaded',nl,
     '----------',nl,
     'The following files are not being loaded, which', nl,
     'means you are not testing them statically', nl, nl].
prolog:message(acheck(non_loaded, load_info(Dir,L,E)-MsgL)) -->
    ['At directory ~w, loaded ~w/~w:'-[Dir, L, E],nl],
    maplist_dcg(non_loaded, MsgL).

non_loaded(File) --> ['\t~w:1: non loaded'-[File],nl].
