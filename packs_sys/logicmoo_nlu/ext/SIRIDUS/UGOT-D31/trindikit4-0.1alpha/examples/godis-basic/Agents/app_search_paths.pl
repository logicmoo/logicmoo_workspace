% Application specific search paths for godis-basic version bundled with 
% trindikit4
%
% David Hjelm

:- use_module(library(lists),[append/3]).

% Application
set_app_search_paths:-
  absolute_file_name('app_search_paths.pl',Abs),
  atom_chars(Abs,AbsChars),
  append(PathChars,"/app_search_paths.pl",AbsChars),
  atom_chars(Path,PathChars),
  assert(user:file_search_path(app,Path)),
  assert(user:library_directory(app(''))),
  assert(user:library_directory(app('../datatypes'))),
  assert(user:library_directory(app('../modules'))),
  assert(user:library_directory(app('../resource_interfaces'))),
  assert(user:library_directory(app('../resources'))),
  assert(user:library_directory(app('../TISdefs'))).

:-set_app_search_paths.
