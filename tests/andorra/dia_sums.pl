:- module(dia_sums, [dia_sums/0, member/3]).

:- use_module(library(andorra/andorra)).

:- determinate(dia_sums,true).

%:- determinate(member(A,B,C), ( instantiated(B,[1]),nonvar(A) ; C?\=[_|_]) )).
%:- determinate(member(A,B,C), ( nonvar(B),B=[H|_],nonvar(A),nonvar(H) ;
%                                nonvar(C), \+(C=[_|_]) )).
% Too weak: we need H?\=A

%:- determinate(member(A,B,C), ( nonvar(A),nonvar(B),cond((B=[H|_],nonvar(H),\+(test_unify(H,A)))) ;
%                                cond((nonvar(C), \+(test_unify(C,[_|_])))))).


%:- determinate(member(A,B,C), ( instantiated(B,[1]),nonvar(A) ; C?\=[_|_]) ).

:- determinate(member(A,B,C), ( nonvar(A), A ?\= term(B,[1])  ; nonvar(C), C?\=[_|_]) ).

%:- determinate(member(A,B,C), ( ground(A),ground(B) ; C?\=[_|_]) ).

%:- determinate(member(_A,_B,_C),true).

dia_sums :- 
        A=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19],
        member(B,A,C),
        member(D,C,E),
        F is 38-B-D,
        member(F,E,G),
        B<F,
        member(H,G,I),
        J is 38-B-H,
        member(J,I,K),
        B<J,
        F<J,
        member(L,K,M),
        member(N,M,O),
        P is 38-H-L-N,
        member(P,O,Q),
        R is 38-F-P,
        member(R,Q,S),
        B<R,
        member(T,S,U),
        V is 38-D-L-T,
        member(V,U,W),
        X is 38-J-V,
        member(X,W,Y),
        B<X,
        member(Z,Y,A1),
        B1 is 38-F-N-Z-X,
        member(B1,A1,C1),
        D1 is 38-J-T-Z-R,
        member(D1,C1,E1),
        F1 is 38-D-N-D1,
        member(F1,E1,G1),
        H1 is 38-R-F1,
        member(H1,G1,I1),
        B<H1,
        J1 is 38-X-H1,
        member(J1,I1,K1),
        J1 is 38-H-T-B1,
  	member(L1,K1,[]),
        L1 is 38-F1-B1-V,
        L1 is 38-B-L-Z-H1,
        L1 is 38-P-D1-J1, 
        write('The solution is: '),
        write([B,D,F,H,L,N,P,J,T,Z,D1,R,V,B1,L1,F1,X,J1,H1]),
        nl.

member(A,[A|B],B).
member(A,[B|C],[B|D]) :-
        A\==B,
        member(A,C,D).
