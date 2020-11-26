/*************************************************************************
  
         name: search_paths_grounding.pl (former search_paths.pl in godis-grounding)
      version: Sep 19, 2004 
  description: Asserts search paths and library directories for godis-grounding
       author: David Hjelm
 
*************************************************************************/
:- ensure_loaded(search_paths_common).

:- assert(user:library_directory(godis('godis-grounding'))).
:- assert(user:library_directory(godis('general'))).
