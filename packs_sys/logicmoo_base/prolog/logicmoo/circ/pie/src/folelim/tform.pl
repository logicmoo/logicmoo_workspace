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

:- module(tform,
	  [so_to_t/2,
	   t_to_so/2,
	   so_to_tnnfpl/2,
	   tnnfpl_to_so/2,
	   tnnfpl_contains_predicate/2,
	   tnnfpl_contains_predicate/3,
	   tnnfpl_copy_bound_vars/2,
	   tnnfpl_free_prolog_vars/2,
	   ppt/1]).

:- use_module(logop_fol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Conversion to and from formula formats "t" and "tnnfpl" used by some
%%%% elimination methods.  "so" is the toyelim standard format (nf/nf.pl,
%%%% logop_fol.pl).
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ppt(F) :-
	t_to_so(F, F1),
	logform_gather_quantifiers(F1, F2),
	pp(F2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tnnfpl_to_so(F, F1) :-
	logform_clean_vars_to_symbols(F, F2),
	( term_variables(F2, FreeVars), FreeVars \= [] ->
	  copy_term(F2-FreeVars, F3-FreeVars1),
	  map_logform_gen_function(FreeVars1)
	; F3 = F2
	),
	t_to_so(F3, F1).

map_logform_gen_function([X|Xs]) :-
	logform_gen_function(X),
	map_logform_gen_function(Xs).
	map_logform_gen_function([]).

so_to_tnnfpl(F, F1) :-
	so_to_t(F, F2),
	logform_to_nnf(F2, F3),
	logform_clean_vars_to_prolog_vars(F3, F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

so_to_t(F, F1) :-
	logform_process_subforms(F, so_to_t_atom, F1).

so_to_t_atom(P/T, P/T) :-
	%% allow atoms that are already in t form:
	!.
so_to_t_atom(PT, P/T) :-
	logform_is_atom(PT),
	!,
	PT =.. [P|Ts],
	T =.. [t|Ts].
so_to_t_atom(F, F).

t_to_so(F, F1) :-
	copy_term(F, F2),
	logform_process_subforms(F2, t_to_so_atom, F1).

t_to_so_atom(P/T, PT) :-
	!,
	( var(P) -> logform_gen_predicate(P) ; true ),
	T =.. [t|Ts],
	PT =.. [P|Ts].
t_to_so_atom(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tnnfpl_contains_predicate(F, P) :-
	logform_enum_atoms(F, P1/_),
	P == P1,
	!.

tnnfpl_contains_predicate(F, P, Pol) :-
	logform_enum_atoms(F, P1/_, Pol),
	P == P1,
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Utilities for formulas with Prolog variables
%%%% - Each FOL variable must be a unique Prolog variable
%%%% - Works on SOL, H, T Formats
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% 
%%%% 
tnnfpl_copy_bound_vars(F, F1) :-
	logform_clean_vars_to_prolog_vars(F, F1).
	
tnnfpl_free_prolog_vars(F, Vs) :-
	logform_clean_vars_to_symbols(F, F1),
	term_variables(F1, Vs).


	
