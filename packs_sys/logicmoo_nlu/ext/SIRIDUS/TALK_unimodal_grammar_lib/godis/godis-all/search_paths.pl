:- assertz(user:file_search_path(godis,'@GODIS_SEARCHPATH@')).

:- assertz(user:library_directory(godis('godis-aod'))).
%:- assertz(user:library_directory(godis('godis-iod'))).
%:- assertz(user:library_directory(godis('godis-grounding'))).
%:- assertz(user:library_directory(godis('godis-basic'))).
:- assertz(user:library_directory(godis('general'))).
:- assertz(user:library_directory(godis('domain-vcr'))).
:- assertz(user:library_directory(godis('domain-tel'))).
:- assertz(user:library_directory(godis('domain-telvcr'))).

:-ensure_loaded(user:library(trindikit_search_paths)).
