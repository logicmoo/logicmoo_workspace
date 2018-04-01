:- module(mqu, [mqu/1, queens/1]).

:- use_module(library(andorra/andorra)).

:- determinate(queens(_),true).
:- determinate(zero(A),nonvar(A)).
:- determinate(adddig(_,_,_),true).
%:- determinate(place(A,B), (nonvar(A), ( A==0 ; instantiated(B,[1]) ) ) ).
%:- determinate(place(A,B), (nonvar(A), ( A==0 ; nonvar(B),B=[X|_],nonvar(X) ))).
%:- determinate(place(A,B), ( nonvar(A),A=0 ; 
%	                     nonvar(A),A=s(_),nonvar(B),B=[X|_],nonvar(X) ) ).
%:- determinate(place(A,B), (nonvar(A),A==0;nonvar(A), nonvar(B), A ?= s(_), instantiated(B,[1]))).
:- determinate(place(A,B), ( A==0; A ?= s(_), instantiated(B,[1]))).
%% :- determinate(app(A,B,C), ( nonvar(A) ; B?\=C ; C?\=[_|_] ) ).
%:- determinate(app(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),\+(B=C) ; 
%	                     nonvar(C),\+C=[_|_] ) ).
:- determinate(app(A,B,C), ( nonvar(A) ; nonvar(B), nonvar(C), B?\=C ; nonvar(C), C?\=[_|_] ) ).
:- determinate(seed(A,B), ( nonvar(A) ; nonvar(B) ) ).
:- determinate(displ(A),nonvar(A)).
%% :- determinate(board(A,B,C,D,E,F,G), 
%% 	( nonvar(A) ; B?\=s(_); C?\=[] ; nonvar(D) ; F?\=G ) ).
%:- determinate(board(A,B,C,D,E,F,G), 
%	( nonvar(A) ; nonvar(B),\+B=s(_); nonvar(C),\+C=[]; nonvar(D) ; 
%	  nonvar(F),nonvar(G),\+F=G ) ).

:- determinate(board(A,B,C,D,_,F,G), 
 	( nonvar(A) ; nonvar(B), B?\=s(_); nonvar(C), C?\=[] ; nonvar(D) ; nonvar(F), nonvar(G), F?\=G ) ).
%:- determinate(atmost1(A), ( atomic(A) ; instantiated(A,[1]) ) ).
%:- determinate(atmost1(A), ( nonvar(A), ( A==[] ; A=[X|_],nonvar(X) ) ) ).
%:- determinate(atmost1(A), ( nonvar(A),A=[] ; nonvar(A),A=[X|_],nonvar(X) ) ).

:- determinate(atmost1(A), ( A==[] ;  instantiated(A,[1]) ) ).

%:- determinate(add(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),\+(B=C) ;
%	nonvar(B),\+B=[_|_] ; nonvar(C),\+C=[_|_] ) ).

:- determinate(add(A,B,C), ( nonvar(A) ; nonvar(B), nonvar(C), B?\=C ;
	nonvar(B), B?\=[_|_] ; nonvar(C), C?\=[_|_] )).
:- determinate(check(A),nonvar(A)).
:- determinate(rev(A,B,C), ( nonvar(A) ; nonvar(B), nonvar(C), B?\=C )).
:- determinate(new_(A,B), ( nonvar(A) ; nonvar(B) ) ).
:- determinate(queens(_,_,_,_),true).

mqu(A) :- queens(A,A,A,_).

queens(A) :-
        queens(A,A,A,B),
        nl,
        displ(B).

zero([]).
zero([0|A]) :-
        zero(A).

adddig([A|B],C,[[A]|D]) :-
        add(B,C,D).

place(0,A) :-
        zero(A).
place(s(A),[q|B]) :-
        place(A,B).
place(s(A),[0|B]) :-
        place(s(A),B).

app([],A,A).
app([A|B],C,[A|D]) :-
        app(B,C,D).

seed(0,[]).
seed(s(A),[[]|B]) :-
        seed(A,B).

displ([]).
displ([A|B]) :-
        write(A),
        nl,
        displ(B).

board(0,s(A),[],[],B,C,C) :-
        seed(s(A),B),
        seed(A,C).
board(s(A),B,C,[D|E],F,G,H) :-
        board(A,B,I,E,J,K,L),
        new_(B,D),
        app(D,I,C),
        add(D,J,F),
        adddig(D,K,G),
        rev(D,[],M),
        adddig(M,L,H).

atmost1([]).
atmost1([q|A]) :-
        zero(A).
atmost1([0|A]) :-
        atmost1(A).

add([],A,A).
add([A|B],[C|D],[[A|C]|E]) :-
        add(B,D,E).

check([]).
check([A|B]) :-
        atmost1(A),
        check(B).

rev([],A,A).
rev([A|B],C,D) :-
        rev(B,[A|C],D).

new_(0,[]).
new_(s(A),[_|C]) :-
        new_(A,C).

queens(A,B,C,D) :-
        board(B,C,E,F,G,H,I),
        place(A,E),
        check(F),
        check(G),
        check(H),
        check(I),
        D=F.
