%% fuzzy ALC reasoner
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 4-6-2007
%% Copyright (C) 2007 Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
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
% This module implements Goedel algebra


:- module( alg_goedel, [tnorm/3, tnorm/4] ).
:- use_module(library(chr)).

:- constraints tnorm/3.
:- constraints tnorm/4.


tnorm( conjunction, X, Y, Res ) :-
	float(X), float(Y),
	(X < Y -> Res is X; Res is Y).
tnorm( disjunction, X, Y, Res ) :-
	float(X), float(Y),
	(X > Y -> Res is X; Res is Y).
tnorm( implication, X, Y, Res ) :-
	float(X), float(Y),
	(X =< Y -> Res is 1.0; Res is Y).
tnorm( complement, X, Res ) :-
	float(X),
	(X == 0.0 -> Res is 1.0; Res is 0.0).

