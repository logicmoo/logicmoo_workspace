
%_________________________
%
% RECURSIVE PATH ORDERING:
%_________________________

greater_rpo(S,T):-  
		S == T, !, fail.
greater_rpo(S,_):-  
		var(S), !, fail.
greater_rpo(S,T):-
		var(T), not_in(T,S), !, fail.
greater_rpo(_,T):-
		var(T), !.
greater_rpo(S,T):-
		S =.. [F|_],
	        T =.. [F|_],
		status_lexic(D,F),!,
		lexic(D,S,T),!.
greater_rpo(S,T):-  
		S =.. [F|Args1],
	        T =.. [F|Args2],!,
		greater_rpo_multi(Args1,Args2),!.
greater_rpo(S,T):-  
		S =.. [F|_], 
		T =.. [G|Args2], 
		greater_po(F,G),!,
		greater_rpo_multi([S],Args2),!.
greater_rpo(S,T):-
		S =.. [_|Args1],
		grequal_rpo_multi(Args1,[T]),!.


% compares terms whose (same) functor has lexicographical status

lexic(_,S,T):-
		S =.. [F|Args1],
	        T =.. [F|_],
		vmember( S1, Args1 ),
		greater_rpo(S1,T),!.
lexic(D,S,T):-
		S =.. [F|Args1],
	        T =.. [F|Args2],
		lexicographic(D,Args1,Args2),
		\+(( vmember(T1,Args2), greater_rpo(T1,S) )),!.

lexicographic(lr,[],_):-!,fail.
lexicographic(lr,[S1|_],[T1|_]):-
		greater_rpo(S1,T1),!.
lexicographic(lr,[S1|S],[T1|T]):-
		S1 == T1,
		lexicographic(lr,S,T),!.


lexicographic(rl,[S1],[T1]):-
		greater_rpo(S1,T1),!.
lexicographic(rl,[_|S],[_|T]):-
		lexicographic(rl,S,T),!.
lexicographic(rl,[S1|S],[T1|T]):-
		S == T,
		greater_rpo(S1,T1),!.

greater_rpo_multi( L1,L2 ):-
		equal_set(L1,L2),
		!,fail.
greater_rpo_multi( _,  []):-!.
greater_rpo_multi( L1, [T2|Rest] ):-
		vmember( T1,L1 ),
		greater_rpo( T1,T2 ),
		greater_rpo_multi( L1,Rest ),!.
greater_rpo_multi( L1,[T|Rest] ):-
		num( T, L1,   Num1),
		num( T, Rest, Num2),
		Num1 > Num2,
		greater_rpo_multi( L1,Rest ),!.

% partial order on function symbols 

greater_po( F1,F2 ):-
		po_order( L ),
		vmember( [F1,F2] , L ),!.

% The multiset L1 is >= than multiset L2.

grequal_rpo_multi(L1,L2):-
		equal_set( L1,L2 ),!.
grequal_rpo_multi(L1,L2):-
		greater_rpo_multi( L1,L2 ),!.

equal_set( [],[] ):-!.
equal_set( [T|Rest],L1 ):-
		delete1( T,L1,L2 ),
		equal_set( Rest,L2 ),!.

delete1(X,[Y|L],L):- X == Y,!.
delete1(X,[Y|L],[Y|L1]):- 
                delete1(X,L,L1),!.

%_______________________________________________________________________________

not_in(X,Y) :-
		var(Y),!,
		X \== Y.
not_in(_,Y) :-
		atom(Y),!.
not_in(X,[A1|ARGS]) :- !,
		not_in(X,A1),!,
		not_in(X,ARGS),!.
not_in(X,Y) :-
		Y=..[_|ARGY],
		not_in(X,ARGY).

% num(X,L,N): X appears N times in L.

num(_,[],0):-!.
num(X,[Y|Rest],N1):-
		X == Y,!,
		num(X,Rest,N2),
		N1 is N2 + 1,!.
num(X,[_|Rest],N):-
		num(X,Rest,N),!.

