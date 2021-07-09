%   File   : sudoku81.pl (in B-Prolog)
%   Author : Neng-Fa ZHOU
%   Date   : 1996
%   Purpose: solve a Japanese arithmetic puzzle (9*9)

top:-
    vars(Vars),
    labeling(Vars).
%    display_board(Vars).

    
go:-
    statistics(runtime,[Start|_]),
    top,
    statistics(runtime,[End|_]),
    T is End-Start,
    write('execution time is '),write(T), write(milliseconds),nl.

vars(Vars):-
    Vars=[A11,A12,A13,B11,B12,B13,C11,C12,C13,
	  A21,A22,A23,B21,B22,B23,C21,C22,C23,
	  A31,A32,A33,B31,B32,B33,C31,C32,C33,
	  D11,D12,D13,E11,E12,E13,F11,F12,F13,
	  D21,D22,D23,E21,E22,E23,F21,F22,F23,
	  D31,D32,D33,E31,E32,E33,F31,F32,F33,
	  G11,G12,G13,H11,H12,H13,I11,I12,I13,
	  G21,G22,G23,H21,H22,H23,I21,I22,I23,
	  G31,G32,G33,H31,H32,H33,I31,I32,I33],
    Vars in 1..9,
    A12=6,B11=2,B13=4,C12=5,
    A21=4,A22=7,B22=6,C22=8,C23=3,
    A33=5,
    B32=7,
    C31=1,
    D11=9,E11=1,E13=3,F13=2,
    D22=1,D23=2,F23=9,
    D31=6,E31=7,E33=9,F33=8,
    G13=6,H12=8,I11=7,
    G21=1,G22=4,H22=9,I22=2,I23=5,
    G32=8,H31=3,H33=5,I32=9,
    % block
    alldifferent([A11,A12,A13,A21,A22,A23,A31,A32,A33]),
    alldifferent([B11,B12,B13,B21,B22,B23,B31,B32,B33]),
    alldifferent([C11,C12,C13,C21,C22,C23,C31,C32,C33]),
    alldifferent([D11,D12,D13,D21,D22,D23,D31,D32,D33]),
    alldifferent([E11,E12,E13,E21,E22,E23,E31,E32,E33]),
    alldifferent([F11,F12,F13,F21,F22,F23,F31,F32,F33]),
    alldifferent([G11,G12,G13,G21,G22,G23,G31,G32,G33]),
    alldifferent([H11,H12,H13,H21,H22,H23,H31,H32,H33]),
    alldifferent([I11,I12,I13,I21,I22,I23,I31,I32,I33]),
    % horizontal
    alldifferent([A11,A12,A13,B11,B12,B13,C11,C12,C13]),
    alldifferent([A21,A22,A23,B21,B22,B23,C21,C22,C23]),
    alldifferent([A31,A32,A33,B31,B32,B33,C31,C32,C33]),
    alldifferent([D11,D12,D13,E11,E12,E13,F11,F12,F13]),
    alldifferent([D21,D22,D23,E21,E22,E23,F21,F22,F23]),
    alldifferent([D31,D32,D33,E31,E32,E33,F31,F32,F33]),
    alldifferent([G11,G12,G13,H11,H12,H13,I11,I12,I13]),
    alldifferent([G21,G22,G23,H21,H22,H23,I21,I22,I23]),
    alldifferent([G31,G32,G33,H31,H32,H33,I31,I32,I33]),
    % vertical
    alldifferent([A11,A21,A31,D11,D21,D31,G11,G21,G31]),
    alldifferent([A12,A22,A32,D12,D22,D32,G12,G22,G32]),
    alldifferent([A13,A23,A33,D13,D23,D33,G13,G23,G33]),
    alldifferent([B11,B21,B31,E11,E21,E31,H11,H21,H31]),
    alldifferent([B12,B22,B32,E12,E22,E32,H12,H22,H32]),
    alldifferent([B13,B23,B33,E13,E23,E33,H13,H23,H33]),
    alldifferent([C11,C21,C31,F11,F21,F31,I11,I21,I31]),
    alldifferent([C12,C22,C32,F12,F22,F32,I12,I22,I32]),
    alldifferent([C13,C23,C33,F13,F23,F33,I13,I23,I33]).
    

display_board([A11,A12,A13,B11,B12,B13,C11,C12,C13,
	       A21,A22,A23,B21,B22,B23,C21,C22,C23,
	       A31,A32,A33,B31,B32,B33,C31,C32,C33,
	       D11,D12,D13,E11,E12,E13,F11,F12,F13,
	       D21,D22,D23,E21,E22,E23,F21,F22,F23,
	       D31,D32,D33,E31,E32,E33,F31,F32,F33,
	       G11,G12,G13,H11,H12,H13,I11,I12,I13,
	       G21,G22,G23,H21,H22,H23,I21,I22,I23,
	       G31,G32,G33,H31,H32,H33,I31,I32,I33]):-
    write([A11,A12,A13,B11,B12,B13,C11,C12,C13]),nl,
    write([A21,A22,A23,B21,B22,B23,C21,C22,C23]),nl,
    write([A31,A32,A33,B31,B32,B33,C31,C32,C33]),nl,
    write([D11,D12,D13,E11,E12,E13,F11,F12,F13]),nl,
    write([D21,D22,D23,E21,E22,E23,F21,F22,F23]),nl,
    write([D31,D32,D33,E31,E32,E33,F31,F32,F33]),nl,
    write([G11,G12,G13,H11,H12,H13,I11,I12,I13]),nl,
    write([G21,G22,G23,H21,H22,H23,I21,I22,I23]),nl,
    write([G31,G32,G33,H31,H32,H33,I31,I32,I33]),nl.

    






