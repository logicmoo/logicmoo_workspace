%% Misc utils for fuzzy algebras
%% 
%% Author: Angelos Charalambidis <acharal@users.sourceforge.net>
%% Created: 22 Feb 2008
%% 
%% Copyright (C) 2008 Angelos Charalambidis
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along
%% with this program; if not, write to the Free Software Foundation, Inc.,
%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


:- module( fuzzyutils, [tnorms/3, tnorm/3, tnorm/4,
			  is_fuzzy_degree/1,
			  less_fuzzy/2, equally_fuzzy/2,
			  check_less_fuzzy/2,
			  check_equally_fuzzy/2,
			  check_clause_degree/2,
			  get_max/2, sup_degree/2, inf_degree/2]).

:- user:use_algebra(Algebra), consult(Algebra).

tnorms( _, [], Deg) :- equally_fuzzy(Deg, 0.0).
tnorms( _, [Deg], Deg).
tnorms( Norm, [Deg1, Deg2], DegRes) :- tnorm( Norm, Deg1, Deg2, DegRes).
tnorms( Norm, [Deg, Deg2, Deg3 | RestDegrees], DegRes) :-
	tnorm( Norm, Deg, Deg2, DegRes1),
	tnorms( Norm, [DegRes1, Deg3 | RestDegrees], DegRes).

sup_degree([], Deg) :- equally_fuzzy(Deg, 0.0).
sup_degree([Deg], Deg) :- !.
sup_degree([Deg1 |DegRest], DegMax) :-
	sup_degree(DegRest, Deg2),
	(check_less_fuzzy(Deg2, Deg1) -> DegMax = Deg2; DegMax = Deg1).

inf_degree([], Deg) :- equally_fuzzy(Deg, 0.0).
inf_degree([Deg], Deg) :- !.
inf_degree([Deg1 |DegRest], DegMin) :-
	inf_degree(DegRest, Deg2),
	(check_less_fuzzy(Deg1, Deg2) -> DegMin = Deg2; DegMin = Deg1).
