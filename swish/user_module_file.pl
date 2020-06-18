:- ensure_loaded(user_module_base).
:- use_module(lps_server_UI).
:- use_rendering(lps_server_UI).
:- (exists_directory(extensions) -> ensure_loaded('extensions/extensions.pl'), writeln("Loaded extensions."); true).
 % place after the above loading so we don't use it...:
:- ensure_loaded(term_expander).

