:- ensure_loaded(user_module_base).
:- use_module(lps_server_UI).
:- if(exists_source(swish(lib/render))). % DMILES: Deal with no SWISH
:- use_rendering(lps_server_UI).
:- endif.
:- multifile user:file_search_path/2.
user:file_search_path(lps_swish, lps_engine_dir('../swish')).
:- absolute_file_name(lps_swish(extensions),ED), format("Extensions directory: ~a~n",[ED]), 
	(exists_directory(ED) -> writeln("Loading..."), ensure_loaded(lps_swish('extensions/extensions.pl')), writeln("...user extensions."); writeln("user extensions missing.")). % DMILES Cares
 % place after the above loading so we don't use it...:
:- lps_term_expander:ensure_loaded(term_expander).

