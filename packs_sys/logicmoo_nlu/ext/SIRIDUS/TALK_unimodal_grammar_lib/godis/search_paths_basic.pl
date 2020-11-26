/*************************************************************************
  
         name: search_paths_basic.pl (former search_paths.pl in godis-basic)
      version: Sep 19, 2004 
  description: Asserts search paths and library directories for godis-basic
       author: David Hjelm
 
*************************************************************************/
:- ensure_loaded(search_paths_common).

:- assert(user:library_directory(godis('godis-basic'))).
:- assert(user:library_directory(godis('general'))).
