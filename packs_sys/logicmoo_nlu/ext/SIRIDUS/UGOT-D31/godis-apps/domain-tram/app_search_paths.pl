% GoDiS
:- ensure_loaded('$GODIS/search_paths_aod').
:- use_module(library(lists),[append/3]).

% APPLY CHANGES BELOW

% Application
set_app_search_paths:-
  absolute_file_name('app_search_paths.pl',Abs),
  atom_chars(Abs,AbsChars),
  append(PathChars,"/app_search_paths.pl",AbsChars),
  atom_chars(Path,PathChars),
  assert(user:file_search_path(app,Path)),
  assert(user:library_directory(app(''))),
  assert(user:library_directory(app('Resources'))).

:-set_app_search_paths.
