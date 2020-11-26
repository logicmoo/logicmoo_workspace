/*************************************************************************

  name: start-vcr-dme.pl
  description: GoDiS VCR starter, text
 
*************************************************************************/

:- ensure_loaded( app_search_paths ).
:- use_module( library(starter) ).
:- init( 'godis-vcr-dme.pl' ).


/*========================================================================
   Run
========================================================================*/
run:-
	run(vcr-english).

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	connect_to_oaa.





