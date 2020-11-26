/*************************************************************************

  name: start-ta-text.pl
  description: GoDiS Travel Agency starter, text
 
*************************************************************************/

:- ensure_loaded( app_search_paths ).
:- use_module( library(starter) ).
:- init( 'godis-ta-text' ).


/*========================================================================
   Run
========================================================================*/

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.
















