:- assertz(user:file_search_path(godis,'C:/MyCVS/godis/dist/prolog/godis')).


:- assertz(user:library_directory(godis('godis-aod'))).
:- assertz(user:library_directory(godis('general'))).
:- assertz(user:library_directory(godis('general/HelpLexicons'))).
:- assertz(user:library_directory(godis('domain-medical'))).

:-ensure_loaded(user:library(trindikit_search_paths)).
