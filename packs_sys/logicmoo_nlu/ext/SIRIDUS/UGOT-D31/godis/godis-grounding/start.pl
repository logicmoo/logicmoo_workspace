/*************************************************************************

   name: start.pl
  description: IBiS2 starter
 
*************************************************************************/

:- ensure_loaded( search_paths ).
:- use_module( library(starter) ).
:- init( 'godis-grounding' ).

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

auteng:-
	setflag(domain, autoroute),
	setflag(language, english).

traveng:-
	setflag(domain, travel),
	setflag(language, english).

cellsvensk:-
	setflag(domain, cellphone),
	setflag(language, svenska ).

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.


run :- run(travel-english).

% reload update rules
rur :-
	update:ensure_loaded(library(update_rules)).

rsr :-
	select:ensure_loaded(library(selection_rules)).

:- assert(hide_path('UNLIKELYPAHNAME')).