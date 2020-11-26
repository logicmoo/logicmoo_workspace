/*************************************************************************

  name: start_legoturtle.pl


  description: GoDiS-AOD starter

  Startar upp systemet.input_form( [sluta, rita], request(speak_pen_up) ).

*************************************************************************/

:- ensure_loaded( search_paths ).

:- use_module( library(starter) ).

:- init( 'godis-AOD-legoturtle' ).




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

run( Domain-_Language ):-
 	setflag(domain, Domain),
 	setflag(language, svenska),
	quiet,
 	start_trindikit.


run :- run(legoturtle-svenska).

% reload update rules
rur :-
	update:ensure_loaded(library(update_rules)).

rsr :-
	select:ensure_loaded(library(selection_rules)).

:- assert(hide_path('UNLIKELYPAHNAME')).






