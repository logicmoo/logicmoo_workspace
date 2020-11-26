/*************************************************************************

         name: start-lamps-text.pl
  description: GoDiS X10 starter, controlling lamps
         date: 2004-11-23
       author: Andreas Wallentin
 
*************************************************************************/

% Make sure you have set the GODIS environment variable!
:-ensure_loaded(app_search_paths).
% Load starter routines; don't change
:- use_module( library(starter) ).

% GODIS spec filename here; initialize system
:- init( 'godis-lamps-text' ).

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	quiet,
	start_trindikit.

run:- run(lamps-svenska).


spi(Module:Goal):-
        Goal=..[P|_],
        spy(Module:P,goal(Module:Goal)),!.

spi(Goal):-
        Goal=..[P|_],
        spy(P,goal(Goal)).

spy_rule(Rule):-
        spy(tis:apply_rule,goal(tis:apply_rule(_M:(Rule,_,_)))).


:- run.


% lägga till test punkt mptre
% etthundratjugoåtta punkt sexton punkt etthundrafemtionio punkt etthundrasextiosex kolon åttatusen
