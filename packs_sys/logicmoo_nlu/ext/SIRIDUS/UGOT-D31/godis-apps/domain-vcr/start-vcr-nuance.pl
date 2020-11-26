/*************************************************************************

  name: start-vcr-nuance.pl
  description: GoDiS VCR starter, nuance
 
*************************************************************************/

:- ensure_loaded( app_search_paths ).
:- use_module( library(starter) ).
:- init( 'godis-vcr-nuance.pl' ).


/*========================================================================
   Run
========================================================================*/

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.





