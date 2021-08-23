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

:- module(compile_term, [compile_term/2]).

compile_term(Clauses, Module) :-
	( Module:current_predicate(P/A),
	  functor(F,P,A), 
	  \+(Module:predicate_property(F, imported_from(_))),
	  Module:abolish(P/A),
	  fail
	; true
	),
	( member(Clause, Clauses),
	  ( Clause = (:- Declaration) ->
	    ( Declaration = set_prolog_flag(Flag, Value) ->
	      Declaration1 = set_prolog_flag(Module:Flag, Value)
	    ; Declaration1 = Declaration
	    ),
	    Module:call(Declaration1)
	  ; Module:assert(Clause)
	  ),
	  fail
	; true
	).
