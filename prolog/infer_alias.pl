:- module(infer_alias,
	  [fastest_alias/2,
	   smallest_alias/2,
	   current_alias/2,
	   pretty_path/2
	  ]).

fastest_alias(File, CAlias) :-
    findall(s(Size1,Size2)-Alias,
	    ( current_alias(File, Alias),
	      Alias =.. [A0, _],
	      findall(A0, user:file_search_path(A0, _), L),
	      length(L, Size1),
	      term_to_atom(Alias, A1),
	      atom_length(A1, Size2)
	    ), SA),
    sort(SA, [_-CAlias|_]).

smallest_alias(File, CAlias) :-
    findall(s(Size2,Size1)-Alias,
	    ( current_alias(File, Alias),
	      Alias =.. [A0, _],
	      findall(A0, user:file_search_path(A0, _), L),
	      length(L, Size1),
	      term_to_atom(Alias, A1),
	      atom_length(A1, Size2)
	    ), SA),
    sort(SA, [_-CAlias|_]).

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
