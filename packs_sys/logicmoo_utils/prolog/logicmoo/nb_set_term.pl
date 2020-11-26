:- module(nb_set_term, [nb_set_has/2, nb_set_add/2, nb_set_add1/2, nb_set_rem/2, nb_set_rem1/2]).
:- set_module(class(library)).
/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/

%nb_set_has(F, List):- arg(N, List, E), E =@= F, !.
nb_set_has(Set, F):- functor(Set,_, A), 
  ((arg(N, Set, E), N < A, E=@=F) -> true;
   (arg(A,Set,T), ((T==[];var(T)) -> (!,fail) ; nb_set_has(T, F)))).

nb_set_add(Set, List):- is_list(List), !, maplist(nb_set_add1(Set),List).
nb_set_add(Set, E):- nb_set_add1(Set,E), !.

nb_set_add1(Set, F):- functor(Set,_, A), 
  ((arg(N, Set, E), N < A, E=@=F) -> true;
   (arg(A,Set,T), ((T==[];var(T)) -> nb_setarg(A, Set, [F]) ; nb_set_add1(T, F)))).

nb_set_rem(Set, List):- is_list(List), !, maplist(nb_set_rem1(Set),List).
nb_set_rem(Set, E):- nb_set_rem1(Set,E), !.

nb_set_rem1(Set, F):- functor(Set,_, A), 
  ((arg(N, Set, E), N < A, E=@=F) -> throw(cant_remove(arg(N, Set, E))) ;
   (arg(A,Set,T), ((T==[];var(T)) -> true ; nb_set_rem1(T, F)))).


:- fixup_exports.

