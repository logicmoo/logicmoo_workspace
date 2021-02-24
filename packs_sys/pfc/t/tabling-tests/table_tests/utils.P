:- export n_letters/3,
	list_alpha/1,
	list_num/1,
	alist/1,
	numle/2,
	display_all/1,
	ziplist/3,
	glb/3,
	glb_list/3,
	same_elems/2,
	findall_s/3,
	list_diff/3,
	delete/3,
	for/3.

:- import member/2 from basics.

n_letters(_,0,[]).
n_letters(A,N,[A|L]) :- N > 0,N1 is N - 1,n_letters(A,N1,L).


%Data generators
list_alpha([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,a,b,c,d]).
list_num([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34]).
alist(X) :-  member(X,[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,a,b,c,d]).

numle(N,N).
numle(N,M):- N > 0,N1 is N -1,numle(N1,M).

for(A,B,A):- A =< B.
for(A,B,C):- A1 is A +1, A1 =< B, for(A1,B,C).

display_all(X):- call(X),write(X),nl,fail;true.


ziplist(A,[],A):- !.
ziplist([],A,A).
ziplist([H|T],[H1|T1],[H,H1|T2]):- ziplist(T,T1,T2).


glb(Term1,Term2,Term):- (var(Term1) -> Term = _TempVar
	;
	var(Term2) -> Term = _TempVar
	;
	Term1 =.. [Head1|Tail1],
	Term2 =.. [Head2|Tail2],
	(Head1 = Head2 ->
		glb_list(Tail1,Tail2,Tail),
		Term =.. [Head1|Tail]
	;
		Term = _TempVar
	)
	)-> true
	;
	Term = _TempVar.

glb_list([],[],[]).
glb_list([H|T],[H1|T1],[H2|T2]) :-
	glb(H,H1,H2),
	glb_list(T,T1,T2).


my_in([],_).
my_in([H|T],Y) :- member(H,Y),my_in(T,Y).

same_elems(X,Y) :- my_in(X,Y), my_in(Y,X). 

findall_s(X,Y,Z) :- findall(X,Y,L),same_elems(L,Z),!.


% do not attempt to delete variables !
delete(_,[],[]).
delete(Elem,[Head|Tail],List) :- delete(Elem,Tail,Tlist),
				(Elem = Head -> List = Tlist;
					    List = [Head|Tlist]).

list_diff([],X,X).
list_diff([Head|Tail],In,Out):- delete(Head,In,Temp),
	list_diff(Tail,Temp,Out).
