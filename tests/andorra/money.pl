%% Cryptarithmetic puzzle  (? Ref. CLP paper, Proc. 4th ICLP)
%% version for andorra1.7 (i.e. after added parser and new built-ins)

:- module(money, [money/0, go/0]).

:- use_module(library(andorra/andorra)).

:- determinate(test, true).
:- determinate(go, true).
:- determinate(go2(_), true).
:- determinate(solve(_, _, _, _, _, _, _, _), true).
:- determinate(const(A, B, C, D, E),
               ( ground(E), ground(D), ground(B), ground(C)
               ; ground(D), ground(E), ground(A), ground(C)
               ; ground(A), ground(E), ground(D), ground(B)
               ; ground(B), ground(E), ground(A), ground(C)
               ; ground(C), ground(D), ground(A), ground(B)
               )).
:- determinate(generate(_, _, _, _, _, _, _, _), true).
:- determinate(difflist(A), nonvar(A) ).
:- determinate(not_member(_, B), nonvar(B)).
:- determinate(not_same(_, _), true).
:- determinate(digi(A), nonvar(A)).
:- determinate(carry(A,B,C), (nonvar(A), nonvar(B), nonvar(C))).
:- determinate(print_out(_, _, _, _, _, _, _, _), true).

money :-
    solve(A,B,C,D,E,F,G,H),
%	wakeup(_1,_3), to include in the result!!!
    print_out(A,B,C,D,E,F,G,H).

go:- solve(_S,_E,_N,_D,_M,_O,_R,_Y).

go2(fn(S,E,N,D,M,O,R,Y)):-
    solve(S,E,N,D,M,O,R,Y),
    print_out(S,E,N,D,M,O,R,Y).

solve(S,E,N,D,M,O,R,Y):- 
	M = 1,			% M must be 1
	carry(C1,C2,C3),
	generate(S,E,N,D,M,O,R,Y),
	S > 0,			% S can't be 0
	const(0,D,E,Y,C1),  	% D+E = Y+10*C1
	const(C1,N,R,E,C2),	% C1+N+R = E+10*C2
	const(C2,E,O,N,C3),	% C2+E+O = N+10*C3
	const(C3,S,M,O,M).	% C3+S+M = O+10*M

const(A,B,C,D,E):-		% if only A is unbound
	A is 10 * E + D - B - C,!.
const(A,B,C,D,E):-		% if only B is unbound
	B is 10 * E + D - A - C,!.
const(A,B,C,D,E):-		% if only C is unbound
	C is 10 * E + D - A - B,!.
const(A,B,C,D,E):-		% if only D is unbound
	D is C - 10 * E + A + B,!.
const(A,B,C,D,E):-		% if only E is unbound
	E is (C - D + A + B) / 10.

generate(S,E,N,D,M,O,R,Y):- 
	digi(S), 
	digi(E), 
	digi(N), 
	digi(D), 
	digi(M),
	digi(O), 
	digi(R), 
	digi(Y), 
	difflist([S,E,N,D,M,O,R,Y]).

difflist([]).
difflist([X|T]):- not_member(X,T), difflist(T).

not_member(_, []).
not_member(X, [Y|L]) :- not_same(X,Y), not_member( X, L ).

not_same(X,Y):- X\==Y.

digi(9).
digi(8).
digi(7).
digi(6).
digi(5).
digi(4).
digi(3).
digi(2).
digi(1).
digi(0).

carry(1,1,1).
carry(1,1,0).
carry(1,0,1).
carry(1,0,0).
carry(0,1,1).
carry(0,1,0).
carry(0,0,1).
carry(0,0,0).

print_out(S,E,N,D,M,O,R,Y):- 
	write('    SEND'), nl,
     	write(' +  MORE'), nl,
	write('-----------'), nl,
	write('   MONEY'), nl, nl,
	write('The solution is:'), nl, nl,
	write('   '), write(S),write(E),write(N),write(D), nl,
	write(' + '), write(M),write(O),write(R),write(E), nl,
	write('-----------'), nl,
	write('  '),
	write(M),write(O),write(N),write(E),write(Y), nl.
