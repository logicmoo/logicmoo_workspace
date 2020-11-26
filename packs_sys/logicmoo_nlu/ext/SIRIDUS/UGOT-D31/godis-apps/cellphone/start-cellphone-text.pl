/*************************************************************************

  name: start-cellphone-text.pl
  description: GoDiS cellphone starter, text
 
*************************************************************************/

:- ensure_loaded( app_search_paths ).
:- use_module( library(starter) ).
:- init( 'godis-cellphone-text.pl' ).


/*========================================================================
   Run
========================================================================*/

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.





