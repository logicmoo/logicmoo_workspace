%:-ensure_loaded(godis_search_paths).

:-ensure_loaded('$GODIS/prolog/godis/general/search_paths').

:-ensure_loaded(user:library(trindikit_search_paths)).

:- working_directory(Home, Home),
	assertz(user:file_search_path(home,Home)).
	
:- assertz(user:library_directory(home)).
:- assertz(user:library_directory(home('Resources'))).



