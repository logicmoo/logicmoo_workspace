/*************************************************************************

         name: search_paths_common.pl (former trindikit_search_paths.pl)
      version: Sep 19, 2004 
  description: Asserts search paths as specified by the Trindikit file
               search_paths.pl. Relies on the environment variable TRINDIKIT
	       pointing to the Trindikit installation directory.
               Also asserts file_search_path for godis..
       author: David Hjelm
 
*************************************************************************/
:-use_module(library(system),[file_exists/1,environ/2]).
:-use_module(library(lists),[append/3]).

% asserts file search path for godis
% does not assert godis library directories
% does only work at when this file is consulted, will not overwrite
% existing godis file search path

% do not do anything if godis file search path is already asserted
assert_godis_file_search_path:-
   user:file_search_path(godis,Path),!.

assert_godis_file_search_path:-
        absolute_file_name('search_paths_common.pl',Abs),
        atom_chars(Abs,AbsStr),
        append(PathStr,"/search_paths_common.pl",AbsStr),
        atom_chars(Path,PathStr),
        assert(user:file_search_path(godis,Path)).

 
%first try new trindikit 4 (search paths already loaded)
assert_trindikit_paths:-
	current_module(tkit_properties),!.

% asserts file search path and library directories for trindikit
assert_trindikit_paths:-
	environ('TRINDIKIT',T),!,
        assert_trindikit_paths(T).

assert_trindikit_paths:-
        nl,
	write('ERROR: The environment variable TRINDIKIT is not set\n'),
	write('You must set TRINDIKIT to point at your Trindikit installation directory\n'),
	nl,
	halt.


%first try trindikit 4
assert_trindikit_paths(T):-
       % trindikit4, 
       atom_concat(T,'/core/prolog/search_paths.pl',T2),
       file_exists(T2),!,
       format('INFO: trindikit search paths found in ~a\n',[T2]),
       ensure_loaded(T2).

assert_trindikit_paths(T):-
       % trindikit4, 
       atom_concat(T,'/dist/prolog/core/search_paths.pl',T2),
       file_exists(T2),!,
       format('INFO: trindikit search paths found in ~a\n',[T2]),
       ensure_loaded(T2).

%then try trindikit 3 
assert_trindikit_paths(T):-
	atom_concat(T,'/dist/prolog/trindikit/search_paths.pl',T2),
        file_exists(T2),!,
       format('INFO: trindikit search paths found in ~a\n',[T2]),
        ensure_loaded(T2).

%then try trindikit 3/dist
assert_trindikit_paths(T):-
	atom_concat(T,'/prolog/trindikit/search_paths.pl',T2),
        file_exists(T2),!,
       format('INFO: trindikit search paths found in ~a\n',[T2]),
        ensure_loaded(T2).

assert_trindikit_paths(TrindikitRoot):-
	nl,
	write('ERROR: Can not find file search_paths.pl '),
	nl,
	write('Is really Trindikit installed at '),write(TrindikitRoot),
	write('?\n\n'),
	halt.

:-assert_trindikit_paths,assert_godis_file_search_path.
