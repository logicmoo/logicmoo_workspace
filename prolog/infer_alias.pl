:- module(infer_alias,
	  [infer_alias/3,
	   fastest_alias/2,
	   library_alias/2,
	   smallest_alias/2,
	   current_alias/2,
	   pretty_path/2
	  ]).

infer_alias(File, CAlias, OptionL) :-
    select_option(sort(SortL), OptionL, _, []),
    findall(SortTerm-Alias,
	    ( current_alias(File, Alias),
	      Alias =.. [AName, _],
	      maplist(sort_field(Alias, AName), SortL, SortTerm)
	    ), SA),
    sort(SA, [_-CAlias|_]).

sort_field(_, A, alias(L), N) :-
    ( nth0(N, L, A)
    ->true
    ; length(L, N)
    ).
sort_field(_, A,       sols, N) :-
    findall(A, user:file_search_path(A, _), L),
    length(L, N).
sort_field(Alias, _, size, N) :-
    term_to_atom(Alias, Atom),
    atom_length(Atom, N).

fastest_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([sols, size])]).

library_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([alias([library]), sols, size])]).

smallest_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([size, sols])]).

current_alias(File, Alias) :-
    user:file_search_path(A0, ADir),
    A0 \= autoload,
    absolute_file_name(ADir, Dir, [file_type(directory), solutions(all)]),
    directory_file_path(Dir, Base, File),
    file_name_extension(Name, _Ext, Base),
    pretty_path(Name, Path),
    Alias =.. [A0, Path].

pretty_path(Name0, Path/F) :-
    directory_file_path(Dir, F, Name0),
    Dir \= '.',
    !,
    pretty_path(Dir, Path).
pretty_path(Name, Name).
