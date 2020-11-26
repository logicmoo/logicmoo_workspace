/*************************************************************************

  name: startagenda.pl
  description: Starts the AgendaTalk application
 
*************************************************************************/

:- ensure_loaded( app_search_paths ).

:- use_module( library(starter) ).

:- init( agendatalk_text ).




/*========================================================================
   Set flags
========================================================================*/

:- setflag(show_rules,yes).
:- setflag(show_state,all).

/*========================================================================
   Run
========================================================================*/

quiet:-
	setflag(show_rules,no),
	setflag(show_state,no).
verb:-
	setflag(show_rules,yes),
	setflag(show_state,all).

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.


run :- run(agenda-english).

% reload update rules
rur :-
	update:ensure_loaded(library(update_rules)).

rsr :-
	select:ensure_loaded(library(selection_rules)).

:- assert(hide_path('UNLIKELYPAHNAME')).
