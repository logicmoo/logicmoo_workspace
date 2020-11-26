/*************************************************************************
  
         name: search_paths_iod.pl (former search_paths.pl in godis-iod)
      version: Sep 19, 2004 
  description: Asserts search paths and library directories for godis-iod
       author: David Hjelm
 
*************************************************************************/
:- ensure_loaded(search_paths_common).

:- assert(user:library_directory(godis('godis-iod'))).
:- assert(user:library_directory(godis('general'))).
