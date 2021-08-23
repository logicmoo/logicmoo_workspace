%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(scopespec,
	  [sc_to_scsae/3,
	   sc_to_scsap/3,
	   sc_to_scse/2,
	   scse_to_scsp/2,
	   sc_duals/2,
	   scse_add_duals/2,
	   scsae_add_duals/2,
	   scp_functor/2]).

/*

There are various types to represent scopes internally.  All of them are
sorted lists whose member format depends on the type:

Type     Member Format

scs      symbol
scse     symbol-p_or_n
scsp     symbol-p_or_n_or_pn (no symbol present with both p and n)
scsae    symbol/arity-p_or_n
scsap    symbol/arity-p_or_n_or_pn (no symbol present with both p and n)

Externally (that is, on the user level) a scope specifier is a possibly
unsorted list whose members can have any of the above formats:

Type     Member Format

sc       all of scs. scse, scsae or scsap

*/

scp_functor(P/_-_, P) :- !.
scp_functor(P-_, P) :- !.
scp_functor(P/_, P) :- !.
scp_functor(P, P).

sc_duals(S, S1) :-
	sc_to_scse(S, S2),
	map_scd_1(S2, S3),
	scse_to_scsp(S3, S1).

scd_1(P-p, P-n) :- !.
scd_1(P-n, P-p).

map_scd_1([X|Xs], [X1|Xs1]) :-
	scd_1(X, X1),
	map_scd_1(Xs, Xs1).
map_scd_1([], []).


scsae_add_duals_to_sc(S, S1) :-
	findall(P, member(P/_-_, S), Ps),
	sort(Ps, S1).

scsae_add_duals(S, S1) :-
	scse_add_duals(S, S1).
	
scse_add_duals(S, S1) :-
	findall(P-POL, (member(P-_, S), (POL=p ; POL=n)), S2),
	sort(S2, S1).
	
sc_to_scse(S, S1) :-
	findall(P, (member(P1, S), mexps(P1, P)), S2),
	sort(S2, S1).

mexps(P-p, P-p) :- !.
mexps(P-n, P-n) :- !.	
mexps(P-pn, P1) :- !, (P1 = P-p ; P1 = P-n).
mexps(P/_-POL, Q) :- !,	mexps(P-POL, Q).
mexps(P/_, Q) :- !, mexps(P-pn, Q).
mexps(P, Q) :- mexps(P-pn, Q).

scse_to_scsp(S, S1) :-
	sort(S, S2),
	mcp(S2, S3),
	sort(S3, S1).

scsae_to_scsap(S, S1) :-
	scse_to_scsp(S, S1).

mcp([P1-n,P2-p,P3-pn|Xs], [P1-pn|Xs1]) :- P1==P2, P2==P3, !, mcp(Xs, Xs1).
mcp([P1-n,P2-p|Xs], [P1-pn|Xs1]) :- P1==P2, !, mcp(Xs, Xs1).
mcp([P1-n,P3-pn|Xs], [P1-pn|Xs1]) :- P1==P3, !, mcp(Xs, Xs1).
mcp([P2-p,P3-pn|Xs], [P2-pn|Xs1]) :- P2==P3, !, mcp(Xs, Xs1).
mcp([X|Xs], [X|Xs1]) :- mcp(Xs, Xs1).
mcp([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_to_scsae(S, F, S1) :-
	sc_to_scsae_1(S, F, S2),
	sort(S2, S1).

sc_to_scsae_1([X-pn|Ps], F, Ps1) :-
	!,
	sc_to_scsae_1([X-p,X-n|Ps], F, Ps1).
sc_to_scsae_1([P/N-Pol|Ps], F, [P/N-Pol|Ps1]) :-
	!,
	sc_to_scsae_1(Ps, F, Ps1).
sc_to_scsae_1([P-Pol|Ps], F, [P/N-Pol|Ps1]) :-
	!,
	mac_get_arity(P, F, N),
	sc_to_scsae_1(Ps, F, Ps1).
sc_to_scsae_1([P-Pol|Ps], F, [P/N-Pol|Ps1]) :-
	!,
	mac_get_arity(P, F, N),
	sc_to_scsae_1(Ps, F, Ps1).
sc_to_scsae_1([P|Ps], F, Ps1) :-
	sc_to_scsae_1([P-p, P-n|Ps], F, Ps1).
sc_to_scsae_1([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_to_scsap(S, F, S1) :-
	sc_to_scsae(S, F, S2),
	scsae_to_scsap(S2, S1).

