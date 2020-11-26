%% CLP(r) implementation of Lukasiewicz norms
%%
%% Authors: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%%          Angelos Charalambidis <acharal@users.sourceforge.net>
%% Created: 4-6-2007
%%
%% Copyright (C) 2007-2008 Stasinos Konstantopoulos
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

% The various fuzzy algebras ae implemented as modules that impose
% CHR constraints on tnorm/2
% This module implements lukasiewicz algebra


:- module( alg_lukasiewicz, [fmt_range/2,
			     is_fuzzy_degree/1,
			     less_fuzzy/2, equally_fuzzy/2,
			     check_less_fuzzy/2,
			     check_equally_fuzzy/2,
			     check_clause_degree/2,
			     get_max/2,
			     tnorm/3, tnorm/4] ).
:- use_module( library(clpr) ).

% If In happens to be instantiated and not a variable,
% an instantiation_error/2 is thrown. In such a case,
% abstract restrictions are meaningless

fmt_range( In, Out ) :-
	length( In, 1 ),
	!,
	catch( dump(In,[d],Out),
	       instantiation_error(dump([D],[d],_),1),
	       Out=[(d=D)] ).
fmt_range( In, Out ) :-
	length( In, 2 ),
	!,
	catch( dump( In, [x,d], Out ),
	       instantiation_error(dump([X,D],[x,d],_),1),
	       Out=[(x=X),(d=D)] ).
fmt_range( _, [] ).

is_fuzzy_degree( Deg ) :-
	{ Deg >= 0.0 },
	{ Deg =< 1.0 }.

%% less_fuzzy/2 ( ?Deg1, ?Deg2, -ExtraRestriction )

/*
less_fuzzy( x, Deg2, X, Deg2 ) :- !,
	{ X >= Deg2 }.
less_fuzzy( Deg1, x, Deg1, X ) :- !,
	{ Deg1 >= X }.
*/
less_fuzzy( Deg1, Deg2 ) :-
	{ Deg1 >= Deg2 }.

check_less_fuzzy( Deg1, Deg2 ) :-
	entailed( Deg1 >= Deg2 ).

check_clause_degree( [Deg], Value ) :-
	!,
	entailed( Deg =:= Value ).
check_clause_degree( [_,Deg], Value ) :-
	!,
	entailed( Deg =:= Value ).

equally_fuzzy( Deg1, Deg2 ) :-
	{ Deg1 =:= Deg2 }.

check_equally_fuzzy( Deg1, Deg2 ) :-
	entailed( Deg1 =:= Deg2 ).

get_max( Deg, Max ) :-
	sup( Deg, Max ).

tnorm( complement, X, Res ) :-
	{ Res =:= 1 - X }.

tnorm( conjunction, X, Y, Res ) :-
	{ Res =:= max(0.0, X + Y - 1.0) }.

tnorm( disjunction, X, Y, Res ) :-
	{ Res =:= min(1.0, X + Y) }.

tnorm( implication, X, Y, Res ) :-
	{ Res =:= min(1.0, 1.0 - X + Y) }.

tnorm( weakconjunction, X, Y, Res) :-
	{ Res =:= min(X, Y) }.

tnorm( weakdisjunction, X, Y, Res) :-
	{ Res =:= max(X, Y) }.


% modus ponens: ( (H <- B) and B ) -> H
% fuzzy degree of        X and Y   -> R
% is calculated according to algebra above,
% under the requirement that the implication holds at degree 1.0.

tnorm( mp, X, Y, Res ) :-
	{
	 X + Y >= 1.0,
	 Res =:= X + Y - 1
	}.
