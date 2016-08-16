:- module(matrix_multiply,
    [matrix_multiply/3,
    dot_product/3,
    cholesky_decomposition/2
    ]).
:- use_module(library(clpfd), [transpose/2]).

%%  matrix_multiply(+X,+Y,-M) is det.
%
%   X(N*P),Y(P*M),M(N*M)
%
matrix_multiply(X,Y,M) :-
    transpose(Y,T),
    maplist(row_multiply(T),X,M).

row_multiply(T,X,M) :-
    maplist(dot_product(X),T,M).

dot_product([X|Xs],[T|Ts],M) :-
    foldl(mul,Xs,Ts,X*T,M).
mul(X,T,M,M+X*T).

/*
?- [matrix_multiply].
?- matrix_multiply([[1,2],[3,4],[5,6]], [[1,1,1],[1,1,1]],R),maplist(maplist(is),C,R).
R = [[1*1+2*1, 1*1+2*1, 1*1+2*1], [3*1+4*1, 3*1+4*1, 3*1+4*1], [5*1+6*1, 5*1+6*1, 5*1+6*1]],
C = [[3, 3, 3], [7, 7, 7], [11, 11, 11]].
*/
% https://rosettacode.org/wiki/Cholesky_decomposition#C
/*
cholesky_decomposition([[25, 15, -5], [15, 18,  0], [-5,  0, 11]],L).
L = [[5.0, 0, 0], [3.0, 3.0, 0], [-1.0, 1.0, 3.0]].
cholesky_decomposition([[18, 22,  54,  42],[22, 70,  86,  62],[ 54, 86, 174, 134],[ 42, 62, 134, 106]],L).
L = [[4.242640687119285, 0, 0, 0], [5.185449728701349, 6.565905201197403, 0, 0], [12.727922061357857, 3.0460384954008553, 1.6497422479090704, 0], [9.899494936611667, 1.624553864213788, 1.8497110052313648, 1.3926212476456026]].
*/
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


list0(0,[]):-!.

list0(N,[0|T]):-
  N1 is N-1,
  list0(N1,T).
