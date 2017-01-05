/** <module> matrix

This module performs matrix operations.
Impemented operations:
 - sum
 - difference
 - multiplication
 - Cholesky decomposition https://en.wikipedia.org/wiki/Cholesky_decomposition
 - determinant for positive semi-definite matrices (using Cholesky decomposition)
 - inversion for positive semi-definite matrices (using Cholesky decomposition)
 - inversion for lower triangular matrices

The library was developed for dealing with multivariate Gaussian distributions,
that's the reson for the focus on positive semi-definite matrices

@author Fabrizio Riguzzi
@license Artistic License 2.0
@copyright Fabrizio Riguzzi
*/


:- module(matrix,
    [matrix_multiply/3,
    matrix_sum/3,
    matrix_diff/3,
    matrix_mult_scal/3,
    matrix_div_scal/3,
    dot_product/3,
    cholesky_decomposition/2,
    matrix_inversion/2,
    matrix_inv_triang/2,
    determinant/2,
    list0/2
    ]).
:- use_module(library(clpfd), [transpose/2]).
%%  matrix_div_scal(+A,+V,-B) is det.
% divide matrix A by scalar V
%
matrix_div_scal(A,V,B):-
  maplist(maplist(div(V)),A,B).

div(A,B,C):-
  C is B/A.
%%  matrix_mult_scal(+A,+V,-B) is det.
% multiply matrix A by scalar V
%
matrix_mult_scal(A,V,B):-
  maplist(maplist(mult(V)),A,B).

mult(A,B,C):-
  C is A*B.
%%  determinant(+A,-D) is det.
% computes the determinant for a positive semi-definite matrix.
% Uses the Cholenski decomposition
% ==
% ?- determinant([[2,-1,0],[-1,2,-1],[0,-1,2]],D).
% D = 3.999999999999999.
% ==
determinant(A,Det):-
  cholesky_decomposition(A,L),
  get_diagonal(L,D),
  foldl(prod,D,1,DetL),
  Det is DetL*DetL.
 
prod(A,P0,P):- 
  P is P0*A.

%%  matrix_inversion(+M,-IM) is det.
% inversion of a positive semi-definite matrix. Uses the Cholenski 
% decomposition
% ==
% ?- matrix_inversion([[2,-1,0],[-1,2,-1],[0,-1,2]],L).
% L = [[0.7499999999999999, 0.5000000000000001, 0.2500000000000001], [0.5000000000000001, 1.0000000000000004, 0.5000000000000002], [0.2500000000000001, 0.5000000000000002, 0.7500000000000001]].
% ==
matrix_inversion(A,B):-
  cholesky_decomposition(A,L),
  matrix_inv_triang(L,LI),
  transpose(LI,LIT),
  matrix_multiply(LIT,LI,B).

%%  matrix_inv_triang(+M,-IM) is det.
% inversion of a lower triangular matrix
% code from
% http://www.mymathlib.com/c_source/matrices/linearsystems/unit_lower_triangular.c
% http://www.mcs.csueastbay.edu/~malek/TeX/Triangle.pdf
% code from
% ==
% ?- matrix_inv_triang([[2,0,0],[-1,2,0],[0,-1,2]],L).
% L = [[0.5, 0.0, 0.0], [0.25, 0.5, 0.0], [0.125, 0.25, 0.5]].
% ==
matrix_inv_triang(L1,L2):-
  get_diagonal(L1,D),
  maplist(inv,D,ID),
  diag(ID,IDM),
  matrix_multiply(IDM,L1,LL1),
  list_to_term(LL1,LT),
  length(LL1,N),
  matrix_inv_i(1,N,LT),
  term_to_list(LT,N,LL2),
  matrix_multiply(LL2,IDM,L2).


matrix_inv_i(N,N,_LT):-!.

matrix_inv_i(I,N,LT):-
  matrix_inv_j(0,I,N,LT),
  I1 is I+1,
  matrix_inv_i(I1,N,LT).

matrix_inv_j(I,I,_N,_LT):-!.

matrix_inv_j(J,I,N,LT):-
  get_v(I,J,N,LT,Vij),
  V_ij is -Vij,
  set_v(I,J,N,LT,V_ij),
  J1 is J+1,
  matrix_inv_k(J1,J,I,N,LT),
  matrix_inv_j(J1,I,N,LT).
  
matrix_inv_k(I,_J,I,_N,_LT):-!.

matrix_inv_k(K,J,I,N,LT):-
  get_v(I,K,N,LT,Vik),
  get_v(K,J,N,LT,Vkj),
  get_v(I,J,N,LT,Vij),
  NVij is Vij-Vik*Vkj,
  set_v(I,J,N,LT,NVij),
  K1 is K+1,
  matrix_inv_k(K1,J,I,N,LT).

 
diag(L,D):-
  length(L,N),
  NN is N*N,
  list0(NN,L0),
  DT =..[a|L0],
  N1 is N-1,
  numlist(0,N1,In),
  maplist(set_diag(DT,N),In,L),
  term_to_list(DT,N,D).

set_diag(D,N,I,V):-
  set_v(I,I,N,D,V).

inv(A,B):-
  B is 1.0/A.

get_diagonal(L,D):-
  length(L,N),
  list_to_term(L,LT),
  get_diag(0,N,LT,D).

get_diag(N,N,_L,[]):-!.

get_diag(N0,N,L,[H|R]):-
  get_v(N0,N0,N,L,H),
  N1 is N0+1,
  get_diag(N1,N,L,R).

list_to_term(L,LT):-
  append(L,LL),
  LT=..[a|LL].
%%  matrix_multiply(+X,+Y,-M) is det.
%
%   X(N*P),Y(P*M),M(N*M)
% ==
% ?- matrix_multiply([[1,2],[3,4],[5,6]], [[1,1,1],[1,1,1]],R).
% R = [[3, 3, 3], [7, 7, 7], [11, 11, 11]].
% ==
% code from http://stackoverflow.com/questions/34206275/matrix-multiplication-with-prolog
matrix_multiply(X,Y,M) :-
  matrix_mul(X,Y,M0),
  maplist(maplist(is),M,M0).

matrix_mul(X,Y,M) :-
    transpose(Y,T),
    maplist(row_multiply(T),X,M).

row_multiply(T,X,M) :-
    maplist(dot_product(X),T,M).

%%  dot_product(+X,+Y,-D) is det.
% computes the dot produce of two vectors
%
dot_product([X|Xs],[T|Ts],M) :-
    foldl(mul,Xs,Ts,X*T,M).
mul(X,T,M,M+X*T).

%% matrix_diff(+A,+B,-C) is det
matrix_diff(X,Y,S):-
  maplist(maplist(diff),X,Y,S).

diff(A,B,C):-
  C is A-B.
%% matrix_sum(+A,+B,-C) is det
% ==
% matrix_sum([[1,2],[3,4],[5,6]],[[1,2],[3,4],[5,6]],M).
% ==
matrix_sum(X,Y,S):-
  maplist(maplist(sum),X,Y,S).

sum(A,B,C):-
  C is A+B.

%% cholesky_decomposition(+A,-L) is det.
% computes the Cholesky decomposition of a positive semi-definite matrix
% code from https://rosettacode.org/wiki/Cholesky_decomposition#C
% ==
% cholesky_decomposition([[25, 15, -5], [15, 18,  0], [-5,  0, 11]],L).
% L = [[5.0, 0, 0], [3.0, 3.0, 0], [-1.0, 1.0, 3.0]].
% cholesky_decomposition([[18, 22,  54,  42],[22, 70,  86,  62],[ 54, 86, 174, 134],[ 42, 62, 134, 106]],L).
% L = [[4.242640687119285, 0, 0, 0], [5.185449728701349, 6.565905201197403, 0, 0], [12.727922061357857, 3.0460384954008553, 1.6497422479090704, 0], [9.899494936611667, 1.624553864213788, 1.8497110052313648, 1.3926212476456026]].
% ==
cholesky_decomposition(A,L):-
  append(A,AL),
  length(AL,NL),
  AM =..[a|AL],
  list0(NL,LL),
  LM=..[l|LL],
  length(A,N),
  cholesky_i(0,N,AM,LM),
  term_to_list(LM,N,L).

cholesky_i(N,N,_A,_L):-!.

cholesky_i(I,N,A,L):-
  cholesky_j(0,I,N,A,L),
  I1 is I+1,
  cholesky_i(I1,N,A,L).

cholesky_j(I,I,N,A,L):-!,
 cholesky_k(0,I,I,N,0,S,L),
  get_v(I,I,N,A,Aii),
  V is sqrt(Aii-S),
  set_v(I,I,N,L,V).


cholesky_j(J,I,N,A,L):-
  cholesky_k(0,J,I,N,0,S,L),
  get_v(I,J,N,A,Aij),
  get_v(J,J,N,L,Ljj),
  V is 1.0/Ljj*(Aij-S),
  set_v(I,J,N,L,V),
  J1 is J+1,
  cholesky_j(J1,I,N,A,L).

cholesky_k(J,J,_I,_N,S,S,_L):-!.

cholesky_k(K,J,I,N,S0,S,L):-
  get_v(I,K,N,L,Lik),
  get_v(J,K,N,L,Ljk),
  S1 is S0+Lik*Ljk,
  K1 is K+1,
  cholesky_k(K1,J,I,N,S1,S,L).

get_v(I,J,N,M,V):-
  Argij is I*N+J+1,
  arg(Argij,M,V).

set_v(I,J,N,M,V):-
  Argij is I*N+J+1,
  setarg(Argij,M,V).

 
term_to_list(T,N,L):-
  T=..[_|E],
  identify_rows(E,N,L).

identify_rows([],_N,[]):-!.

identify_rows(E,N,[R|L]):-
  length(R,N),
  append(R,Rest,E),
  identify_rows(Rest,N,L).

%% list0(+N,-L) is det
% returns a list of N zeros
list0(0,[]):-!.

list0(N,[0|T]):-
  N1 is N-1,
  list0(N1,T).
