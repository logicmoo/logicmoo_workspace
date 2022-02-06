% load.pl : Load Chat-80, for Quintus Prolog

/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

:- ensure_loaded(xgrun).	% XG runtimes
:- ensure_loaded(newg).		% clone + lex
:- ensure_loaded(clotab).	% attachment tables
:- ensure_loaded(newdict).	% syntactic dictionary
:- ensure_loaded(slots).	% fits arguments into predicates
:- ensure_loaded(scopes).	% quantification and scoping
:- ensure_loaded(templa).	% semantic dictionary
:- ensure_loaded(qplan).	% query planning
:- ensure_loaded(talkr).	% query evaluation
:- ensure_loaded(ndtabl).	% relation info.
:- ensure_loaded(readin).	% sentence input
:- ensure_loaded(ptree).	% print trees
:- ensure_loaded(aggreg).	% aggregation operators
:- ensure_loaded(world0).     	% data base
:- ensure_loaded(rivers).
:- ensure_loaded(cities).
:- ensure_loaded(countries).
:- ensure_loaded(contain).
:- ensure_loaded(borders).
:- ensure_loaded(newtop).	% top level



