/*************************************************************************
  
         name: search_paths_aod.pl (former search_paths.pl in godis-aod)
      version: Sep 19, 2004 
  description: Asserts search paths and library directories for godis-aod
       author: David Hjelm
 
*************************************************************************/
:- ensure_loaded(search_paths_common).

:- assert(user:library_directory(godis('godis-aod'))).
:- assert(user:library_directory(godis('general'))).
