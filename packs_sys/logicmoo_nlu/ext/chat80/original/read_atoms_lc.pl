
:- throw(dont_read_atoms).

read_atoms_lc( L) :-
  read_atoms( A),
  atoms_to_lower( A, L).

atoms_to_lower( [A|As], [L|Ls]) :-
  !,
  atom_string( A, S),
  Low is lowcase( S),
  atom_string( L0, Low),
  getnum( L0, L),
  atoms_to_lower( As, Ls).
atoms_to_lower( [], []).

/*
getnum( A, nb(A)) :-
  integer( A),
  !.
*/
getnum( A, A).
