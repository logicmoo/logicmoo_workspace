/* Find distinct non-zero digits such that the following equation holds:
        A        D        G
     ------  + ----- + ------  = 1
       B*C      E*F      H*I
*/
go:-
        cputime(X),
	Vars=[A,B,C,D,E,F,G,H,I],
	Vars in 1..9,
	[D1,D2,D3] in 1..81,
	D1 #= B*C,
	D2 #= E*F,
	D3 #= H*I,
	A*D2*D3 + D*D1*D3 + G*D1*D2 #= D1*D2*D3,
	% break the symmetry
	A*D2 #>= D*D1,
	D*D3 #>= G*D2,
	%redundant constraints
	3*A #>= D1,
	3*G #=< D2,
	alldifferent(Vars),
    mylabeling_ff(Vars),
        cputime(Y),
	T is Y-X,
	write(Vars), write(' found in '), write(T), write(' milliseconds'),nl.

mylabeling_ff(Vars):-
    abolish('$no_backtracks',1),
    assert('$no_backtracks'(0)),
    mylabeling_ff_1(Vars).

mylabeling_ff_1(Vars):-
    deleteff(V,Vars,Rest),!,
    myindomain(V),
    mylabeling_ff_1(Rest).
mylabeling_ff_1(_):-'$no_backtracks'(X),write(backtracks(X)),nl.

mylabeling(Vars):-
    abolish('$no_backtracks',1),
    assert('$no_backtracks'(0)),
    mylabeling_1(Vars).

mylabeling_1([V|Vs]):-
    myindomain(V),
    mylabeling_1(Vs).
mylabeling_1([]):-'$no_backtracks'(X),write(backtracks(X)),nl.

myindomain(V):-
    integer(V),!.
myindomain(V):-
    fd_dom(V,L),
    myindomain(V,L).

myindomain(V,[X]):-!,
    V=X.
myindomain(V,[X|Xs]):-V=X.
myindomain(V,[_|Xs]):-
    (retract('$no_backtracks'(No))->true;No=0),
    No1 is No+1,
    assert('$no_backtracks'(No1)),
    myindomain(V,Xs).

