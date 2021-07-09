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
       labeling_ff(Vars),
        cputime(Y),
	T is Y-X,
	write(Vars), write(' found in '), write(T), write(' milliseconds'),nl.
