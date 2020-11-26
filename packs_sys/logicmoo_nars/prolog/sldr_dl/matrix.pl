:- use_module(library(clpfd)).

% Addition

const_add_const(X,Y,Z):-
  Z is X + Y,
  !.

vec_add_vec(X,Y,R):-
  maplist(const_add_const,X,Y,R),
  !.

mat_add_mat(X,Y,R):-
  maplist(vec_add_vec,X,Y,R),
  !.

% Multiplication

const_mult_const(X,Y,Z):-
  Z is X * Y,
  !.

const_mult_vec(C,V,R):-
  maplist(const_mult_const(C),V,R),
  !.

vec_mult_const(V,C,R):-
  maplist(const_mult_const(C),V,R),
  !.

vec_mult_vec(X,Y,R):-
  maplist(const_mult_const,X,Y,R),
  !.

mat_mult_const(M,C,R):-
  maplist(const_mult_vec(C),M,R),
  !.

mat_mult_vec(M,V,R):-
  maplist(vec_mult_vec(V),M,T),
  maplist(sumlist,T,R),
  !.

mat_mult_mat(X,Y,R):-
  transpose(Y,T),
  maplist(mat_mult_vec(T),X,R),
  !.

/*
mat_mult_mat(X,Y,R):-
  transpose(Y,T),
  maplist(mat_mult_vec(X),T,S),
  transpose(S,R),
  !.
*/
mapmat(F,M,R):-
  maplist(mapmatsub(F),M,R).
mapmatsub(F,V,R):-
  maplist(F,V,R),
  !.
