/*************************************************************************
  
         name: search_paths_all.pl (former search_paths.pl in godis-all)
      version: Sep 19, 2004 
  description: Asserts search paths and library directories for godis-all
       author: David Hjelm
 
*************************************************************************/
:- ensure_loaded(search_paths_common).

:- assert(user:library_directory(godis('godis-all'))).
:- assert(user:library_directory(godis('general'))).
