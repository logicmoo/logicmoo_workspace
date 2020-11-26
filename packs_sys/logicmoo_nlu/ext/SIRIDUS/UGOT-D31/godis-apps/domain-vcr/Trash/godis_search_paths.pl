/*************************************************************************

         name: godis_search_paths.pl
      version: Jan 9, 2003; Nov 11, 2003
  description: Asserts search paths as specified by the GoDiS file
               search_paths.pl. Relies on the environment variable GODIS
	       pointing to the GODIS installation directory.
       author: David Hjelm, Staffan Larsson
 
*************************************************************************/


assert_godis_paths:-
	environ('GODIS',T),!,
	convert_path_if_windows_godis(T,T1),
	atom_concat(T1,'/prolog/godis/general/search_paths.pl',T2),
	on_exception( _,
		      consult(T2),
		      wrong_var_godis(T1,T2)
		    ).

assert_godis_paths:-
	no_var_godis.

no_var_godis:-nl,
	write('ERROR: The environment variable GODIS is not set\n'),
	write('You must set GODIS to point at your GoDiS installation directory\n'),
	nl,
	halt.

wrong_var_godis(GodisRoot,SearchPathFilePath):-
	nl,
	write('ERROR: Can not find file '),write(SearchPathFilePath),
	nl,
	write('Is GoDiS really installed at '),write(GodisRoot),
	write('?\n\n'),
	halt.

%WINDOWS
convert_path_if_windows_godis(Path,ConvertedPath):-
	prolog_flag(host_type,OS), 
	atom_concat('x86-win32',_,OS),!,
	atom_codes(Path,PathCodes),
	backslash2slash_godis(PathCodes,ConvertedPathCodes),
	atom_codes(ConvertedPath,ConvertedPathCodes).

%NOT WINDOWS
convert_path_if_windows_godis(Path,Path).

backslash2slash_godis([],[]).
backslash2slash_godis([92|As],[47|Bs]):-!,
	backslash2slash_godis(As,Bs).
backslash2slash_godis([A|As],[A|Bs]):-
	backslash2slash_godis(As,Bs).


:-use_module(library(system)),assert_godis_paths.