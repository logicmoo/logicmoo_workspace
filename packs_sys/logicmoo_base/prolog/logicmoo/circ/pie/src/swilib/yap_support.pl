%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This program is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% This program is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(yap_support, [call_with_time_limit/3,
			call_with_inference_limit/3,
			atom_prefix/2,
			subtract/3,
			date/1]).

:- module_transparent(call_with_time_limit/2).
:- module_transparent(call_with_inference_limit/3).

:- use_module(library(timeout)).

call_with_time_limit(Goal, Timeout) :-
	Timeout1 is truncate(Timeout * 1000),
	time_out(once(Goal), Timeout1, success).

%%%%
%%%% Does not implement the full functionality of the SWI predicate
%%%%
call_with_inference_limit(Goal, Infs, Result) :-
	Timeout is truncate(Infs * 0.000079),
	time_out(once(Goal), Timeout, Result1),
	( Result1 = success ->
	  Result = (!)
	; Result = inference_limit_exceeded
	).

atom_prefix(Atom, Prefix) :-
	sub_atom(Atom, 0, _, _, Prefix).

subtract([], _, []) :- !.
subtract([E|T], D, R) :-
	memberchk(E, D), !,
	subtract(T, D, R).
subtract([H|T], D, [H|R]) :-
	subtract(T, D, R).

date(date(Y, M, D)) :-
	datime(datime(Y, M, D,_,_,_)).
