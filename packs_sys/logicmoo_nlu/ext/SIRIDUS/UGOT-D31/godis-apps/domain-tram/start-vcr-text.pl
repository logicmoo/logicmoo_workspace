/*************************************************************************

  name: start-vcr-text.pl
  description: GoDiS VCR starter, text
 
*************************************************************************/

:- ensure_loaded( app_search_paths ).
:- use_module( library(starter) ).
:- init( 'godis-vcr-text.pl' ).


/*========================================================================
   Run
========================================================================*/

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.





