:- dynamic contrapos_recorded/1.
:- style_check(- singleton).
:- style_check(- discontiguous).

% dyn(diag(_7834,_7836)).
:- dynamic ex_not_diag/5.
:- dynamic ex_tru_diag/5.
:- dynamic prove_not_diag/4.
:- dynamic prove_tru_diag/4.

% dyn(val(_8398,_8400)).
:- dynamic ex_not_val/5.
:- dynamic ex_tru_val/5.
:- dynamic prove_not_val/4.
:- dynamic prove_tru_val/4.

% fact(gate(x1,xor)).
prove_tru_gate(x1, xor, _, _).
ex_tru_gate(x1, xor, ths(A, A, B, B), _, ans(C, C)).

% fact(gate(x2,xor)).
prove_tru_gate(x2, xor, _, _).
ex_tru_gate(x2, xor, ths(A, A, B, B), _, ans(C, C)).

% fact(gate(a1,and)).
prove_tru_gate(a1, and, _, _).
ex_tru_gate(a1, and, ths(A, A, B, B), _, ans(C, C)).

% fact(gate(a2,and)).
prove_tru_gate(a2, and, _, _).
ex_tru_gate(a2, and, ths(A, A, B, B), _, ans(C, C)).

% fact(gate(o1,or)).
prove_tru_gate(o1, or, _, _).
ex_tru_gate(o1, or, ths(A, A, B, B), _, ans(C, C)).

% fact(conn(in(1,f1),in(1,x1))).
prove_tru_conn(in(1, f1), in(1, x1), _, _).
ex_tru_conn(in(1, f1), in(1, x1), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(in(1,f1),in(1,a1))).
prove_tru_conn(in(1, f1), in(1, a1), _, _).
ex_tru_conn(in(1, f1), in(1, a1), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(in(2,f1),in(2,x1))).
prove_tru_conn(in(2, f1), in(2, x1), _, _).
ex_tru_conn(in(2, f1), in(2, x1), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(in(2,f1),in(2,a1))).
prove_tru_conn(in(2, f1), in(2, a1), _, _).
ex_tru_conn(in(2, f1), in(2, a1), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(in(3,f1),in(2,x2))).
prove_tru_conn(in(3, f1), in(2, x2), _, _).
ex_tru_conn(in(3, f1), in(2, x2), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(in(3,f1),in(1,a2))).
prove_tru_conn(in(3, f1), in(1, a2), _, _).
ex_tru_conn(in(3, f1), in(1, a2), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(out(1,x1),in(1,x2))).
prove_tru_conn(out(1, x1), in(1, x2), _, _).
ex_tru_conn(out(1, x1), in(1, x2), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(out(1,x1),in(2,a2))).
prove_tru_conn(out(1, x1), in(2, a2), _, _).
ex_tru_conn(out(1, x1), in(2, a2), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(out(1,a1),in(2,o1))).
prove_tru_conn(out(1, a1), in(2, o1), _, _).
ex_tru_conn(out(1, a1), in(2, o1), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(out(1,a2),in(1,o1))).
prove_tru_conn(out(1, a2), in(1, o1), _, _).
ex_tru_conn(out(1, a2), in(1, o1), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(out(1,x2),out(1,f1))).
prove_tru_conn(out(1, x2), out(1, f1), _, _).
ex_tru_conn(out(1, x2), out(1, f1), ths(A, A, B, B), _, ans(C, C)).

% fact(conn(out(1,o1),out(2,f1))).
prove_tru_conn(out(1, o1), out(2, f1), _, _).
ex_tru_conn(out(1, o1), out(2, f1), ths(A, A, B, B), _, ans(C, C)).

% fact(val(in(_17978,_17980),anything)).
prove_tru_val(in(_, _), anything, _, _).
ex_tru_val(in(_, _), anything, ths(A, A, B, B), _, ans(C, C)).

% default(ok(_18514)).
prove_tru_ok(A, B, _) :-
	member(ok(A), B).
ex_tru_ok(D, ths(A, A, B, B), _, ans(C, C)) :-
	member(ok(D), A).
ex_tru_ok(A, ths(B, [ok(A)|B], C, C), _, ans(D, D)) :-
	variable_free(ok(A)),
	\+ member(ok(A), B),
	\+ prove_not_ok(A, [ok(A)|B], anc([], [])).
ex_tru_ok(B, ths(A, A, C, [ok(B)|C]), _, ans(D, D)) :-
	\+ variable_free(ok(B)).

% fact(<-(val(out(1,_19442),_19448),(ok(_19442),gate(_19442,_19458),ttable(_19458,_19464,_19466,_19448),val(in(1,_19442),_19464),val(in(2,_19442),_19466)))).
prove_tru_val(out(1, A), C, B, anc(D, E)) :-
	prove_tru_ok(A,
		 B,
		 anc([val(out(1, A), C)|D], E)),
	prove_tru_gate(A,
		   F,
		   B,
		   anc([val(out(1, A), C)|D], E)),
	prove_tru_ttable(F,
		     G,
		     H,
		     C,
		     B,
		     anc([val(out(1, A), C)|D], E)),
	prove_tru_val(in(1, A),
		  G,
		  B,
		  anc([val(out(1, A), C)|D], E)),
	prove_tru_val(in(2, A),
		  H,
		  B,
		  anc([val(out(1, A), C)|D], E)).
ex_tru_val(out(1, A), D, ths(B, U, C, W), anc(E, F), ans(G, Y)) :-
	ex_tru_ok(A,
	      ths(B, H, C, I),
	      anc([val(out(1, A), D)|E], F),
	      ans(G, J)),
	ex_tru_gate(A,
		K,
		ths(H, L, I, M),
		anc([val(out(1, A), D)|E], F),
		ans(J, N)),
	ex_tru_ttable(K,
		  O,
		  S,
		  D,
		  ths(L, P, M, Q),
		  anc([val(out(1, A), D)|E], F),
		  ans(N, R)),
	ex_tru_val(in(1, A),
	       O,
	       ths(P, T, Q, V),
	       anc([val(out(1, A), D)|E], F),
	       ans(R, X)),
	ex_tru_val(in(2, A),
	       S,
	       ths(T, U, V, W),
	       anc([val(out(1, A), D)|E], F),
	       ans(X, Y)).
prove_not_ok(A, B, anc(C, D)) :-
	( prove_tru_gate(A,
		     E,
		     B,
		     anc(C, [ok(A)|D])),
	  prove_tru_ttable(E,
		       F,
		       G,
		       H,
		       B,
		       anc(C, [ok(A)|D])),
	  prove_tru_val(in(1, A),
		    F,
		    B,
		    anc(C, [ok(A)|D])),
	  prove_tru_val(in(2, A),
		    G,
		    B,
		    anc(C, [ok(A)|D]))
	),
	prove_not_val(out(1, A),
		      H,
		      B,
		      anc(C, [ok(A)|D])).
ex_not_ok(A, ths(B, U, C, W), anc(D, E), ans(F, Y)) :-
	( ex_tru_gate(A,
		  G,
		  ths(B, H, C, I),
		  anc(D, [ok(A)|E]),
		  ans(F, J)),
	  ex_tru_ttable(G,
		    K,
		    O,
		    S,
		    ths(H, L, I, M),
		    anc(D, [ok(A)|E]),
		    ans(J, N)),
	  ex_tru_val(in(1, A),
		 K,
		 ths(L, P, M, Q),
		 anc(D, [ok(A)|E]),
		 ans(N, R)),
	  ex_tru_val(in(2, A),
		 O,
		 ths(P, T, Q, V),
		 anc(D, [ok(A)|E]),
		 ans(R, X))
	),
	ex_not_val(out(1, A),
		   S,
		   ths(T, U, V, W),
		   anc(D, [ok(A)|E]),
		   ans(X, Y)).
prove_not_gate(D, A, B, anc(C, E)) :-
	( prove_tru_ttable(A,
		       F,
		       G,
		       H,
		       B,
		       anc(C, [gate(D, A)|E])),
	  prove_tru_val(in(1, D),
		    F,
		    B,
		    anc(C, [gate(D, A)|E])),
	  prove_tru_val(in(2, D),
		    G,
		    B,
		    anc(C, [gate(D, A)|E]))
	),
	prove_tru_ok(D, B, anc(C, [gate(D, A)|E])),
	prove_not_val(out(1, D),
		      H,
		      B,
		      anc(C, [gate(D, A)|E])).
ex_not_gate(E, A, ths(B, U, C, W), anc(D, F), ans(G, Y)) :-
	( ex_tru_ttable(A,
		    H,
		    L,
		    S,
		    ths(B, I, C, J),
		    anc(D, [gate(E, A)|F]),
		    ans(G, K)),
	  ex_tru_val(in(1, E),
		 H,
		 ths(I, M, J, N),
		 anc(D, [gate(E, A)|F]),
		 ans(K, O)),
	  ex_tru_val(in(2, E),
		 L,
		 ths(M, P, N, Q),
		 anc(D, [gate(E, A)|F]),
		 ans(O, R))
	),
	ex_tru_ok(E,
	      ths(P, T, Q, V),
	      anc(D, [gate(E, A)|F]),
	      ans(R, X)),
	ex_not_val(out(1, E),
		   S,
		   ths(T, U, V, W),
		   anc(D, [gate(E, A)|F]),
		   ans(X, Y)).
prove_not_ttable(D, A, E, F, B, anc(C, G)) :-
	( prove_tru_val(in(1, H),
		    A,
		    B,
		    anc(C,
			[ttable(D, A, E, F)|G])),
	  prove_tru_val(in(2, H),
		    E,
		    B,
		    anc(C,
			[ttable(D, A, E, F)|G]))
	),
	prove_tru_gate(H,
		   D,
		   B,
		   anc(C,
		       [ttable(D, A, E, F)|G])),
	prove_tru_ok(H,
		 B,
		 anc(C,
		     [ttable(D, A, E, F)|G])),
	prove_not_val(out(1, H),
		      F,
		      B,
		      anc(C,
			  [ttable(D, A, E, F)|G])).
ex_not_ttable(E, A, F, G, ths(B, U, C, W), anc(D, H), ans(I, Y)) :-
	( ex_tru_val(in(1, J),
		 A,
		 ths(B, K, C, L),
		 anc(D,
		     [ttable(E, A, F, G)|H]),
		 ans(I, M)),
	  ex_tru_val(in(2, J),
		 F,
		 ths(K, N, L, O),
		 anc(D,
		     [ttable(E, A, F, G)|H]),
		 ans(M, P))
	),
	ex_tru_gate(J,
		E,
		ths(N, Q, O, R),
		anc(D, [ttable(E, A, F, G)|H]),
		ans(P, S)),
	ex_tru_ok(J,
	      ths(Q, T, R, V),
	      anc(D, [ttable(E, A, F, G)|H]),
	      ans(S, X)),
	ex_not_val(out(1, J),
		   G,
		   ths(T, U, V, W),
		   anc(D,
		       [ttable(E, A, F, G)|H]),
		   ans(X, Y)).
prove_not_val(in(1, A), D, B, anc(C, E)) :-
	prove_tru_val(in(2, A),
		  F,
		  B,
		  anc(C, [val(in(1, A), D)|E])),
	prove_tru_ttable(G,
		     D,
		     F,
		     H,
		     B,
		     anc(C, [val(in(1, A), D)|E])),
	prove_tru_gate(A,
		   G,
		   B,
		   anc(C, [val(in(1, A), D)|E])),
	prove_tru_ok(A,
		 B,
		 anc(C, [val(in(1, A), D)|E])),
	prove_not_val(out(1, A),
		      H,
		      B,
		      anc(C, [val(in(1, A), D)|E])).
ex_not_val(in(1, A), E, ths(B, U, C, W), anc(D, F), ans(G, Y)) :-
	ex_tru_val(in(2, A),
	       H,
	       ths(B, I, C, J),
	       anc(D, [val(in(1, A), E)|F]),
	       ans(G, K)),
	ex_tru_ttable(L,
		  E,
		  H,
		  S,
		  ths(I, M, J, N),
		  anc(D, [val(in(1, A), E)|F]),
		  ans(K, O)),
	ex_tru_gate(A,
		L,
		ths(M, P, N, Q),
		anc(D, [val(in(1, A), E)|F]),
		ans(O, R)),
	ex_tru_ok(A,
	      ths(P, T, Q, V),
	      anc(D, [val(in(1, A), E)|F]),
	      ans(R, X)),
	ex_not_val(out(1, A),
		   S,
		   ths(T, U, V, W),
		   anc(D, [val(in(1, A), E)|F]),
		   ans(X, Y)).
prove_not_val(in(2, A), D, B, anc(C, E)) :-
	prove_tru_val(in(1, A),
		  F,
		  B,
		  anc(C, [val(in(2, A), D)|E])),
	prove_tru_ttable(G,
		     F,
		     D,
		     H,
		     B,
		     anc(C, [val(in(2, A), D)|E])),
	prove_tru_gate(A,
		   G,
		   B,
		   anc(C, [val(in(2, A), D)|E])),
	prove_tru_ok(A,
		 B,
		 anc(C, [val(in(2, A), D)|E])),
	prove_not_val(out(1, A),
		      H,
		      B,
		      anc(C, [val(in(2, A), D)|E])).
ex_not_val(in(2, A), E, ths(B, U, C, W), anc(D, F), ans(G, Y)) :-
	ex_tru_val(in(1, A),
	       H,
	       ths(B, I, C, J),
	       anc(D, [val(in(2, A), E)|F]),
	       ans(G, K)),
	ex_tru_ttable(L,
		  H,
		  E,
		  S,
		  ths(I, M, J, N),
		  anc(D, [val(in(2, A), E)|F]),
		  ans(K, O)),
	ex_tru_gate(A,
		L,
		ths(M, P, N, Q),
		anc(D, [val(in(2, A), E)|F]),
		ans(O, R)),
	ex_tru_ok(A,
	      ths(P, T, Q, V),
	      anc(D, [val(in(2, A), E)|F]),
	      ans(R, X)),
	ex_not_val(out(1, A),
		   S,
		   ths(T, U, V, W),
		   anc(D, [val(in(2, A), E)|F]),
		   ans(X, Y)).

% default(faulty(_5774)).
prove_tru_faulty(A, B, _) :-
	member(faulty(A), B).
ex_tru_faulty(D, ths(A, A, B, B), _, ans(C, C)) :-
	member(faulty(D), A).
ex_tru_faulty(A, ths(B, [faulty(A)|B], C, C), _, ans(D, D)) :-
	variable_free(faulty(A)),
	\+ member(faulty(A), B),
	\+ prove_not_faulty(A, [faulty(A)|B], anc([], [])).
ex_tru_faulty(B, ths(A, A, C, [faulty(B)|C]), _, ans(D, D)) :-
	\+ variable_free(faulty(B)).

% fact(<-(val(out(1,_6822),_6828),(faulty(_6822),gate(_6822,_6838),ttable(_6838,_6844,_6846,_6848),opp(_6848,_6828),val(in(1,_6822),_6844),val(in(2,_6822),_6846)))).
prove_tru_val(out(1, A), C, B, anc(D, E)) :-
	prove_tru_faulty(A,
		     B,
		     anc([val(out(1, A), C)|D], E)),
	prove_tru_gate(A,
		   F,
		   B,
		   anc([val(out(1, A), C)|D], E)),
	prove_tru_ttable(F,
		     H,
		     I,
		     G,
		     B,
		     anc([val(out(1, A), C)|D], E)),
	prove_tru_opp(G,
		  C,
		  B,
		  anc([val(out(1, A), C)|D], E)),
	prove_tru_val(in(1, A),
		  H,
		  B,
		  anc([val(out(1, A), C)|D], E)),
	prove_tru_val(in(2, A),
		  I,
		  B,
		  anc([val(out(1, A), C)|D], E)).
ex_tru_val(out(1, A), D, ths(B, Y, C, A1), anc(E, F), ans(G, C1)) :-
	ex_tru_faulty(A,
		  ths(B, H, C, I),
		  anc([val(out(1, A), D)|E], F),
		  ans(G, J)),
	ex_tru_gate(A,
		K,
		ths(H, L, I, M),
		anc([val(out(1, A), D)|E], F),
		ans(J, N)),
	ex_tru_ttable(K,
		  S,
		  W,
		  O,
		  ths(L, P, M, Q),
		  anc([val(out(1, A), D)|E], F),
		  ans(N, R)),
	ex_tru_opp(O,
	       D,
	       ths(P, T, Q, U),
	       anc([val(out(1, A), D)|E], F),
	       ans(R, V)),
	ex_tru_val(in(1, A),
	       S,
	       ths(T, X, U, Z),
	       anc([val(out(1, A), D)|E], F),
	       ans(V, B1)),
	ex_tru_val(in(2, A),
	       W,
	       ths(X, Y, Z, A1),
	       anc([val(out(1, A), D)|E], F),
	       ans(B1, C1)).
prove_not_faulty(A, B, anc(C, D)) :-
	( prove_tru_gate(A,
		     E,
		     B,
		     anc(C, [faulty(A)|D])),
	  prove_tru_ttable(E,
		       G,
		       H,
		       F,
		       B,
		       anc(C, [faulty(A)|D])),
	  prove_tru_opp(F,
		    I,
		    B,
		    anc(C, [faulty(A)|D])),
	  prove_tru_val(in(1, A),
		    G,
		    B,
		    anc(C, [faulty(A)|D])),
	  prove_tru_val(in(2, A),
		    H,
		    B,
		    anc(C, [faulty(A)|D]))
	),
	prove_not_val(out(1, A),
		      I,
		      B,
		      anc(C, [faulty(A)|D])).
ex_not_faulty(A, ths(B, Y, C, A1), anc(D, E), ans(F, C1)) :-
	( ex_tru_gate(A,
		  G,
		  ths(B, H, C, I),
		  anc(D, [faulty(A)|E]),
		  ans(F, J)),
	  ex_tru_ttable(G,
		    O,
		    S,
		    K,
		    ths(H, L, I, M),
		    anc(D, [faulty(A)|E]),
		    ans(J, N)),
	  ex_tru_opp(K,
		 W,
		 ths(L, P, M, Q),
		 anc(D, [faulty(A)|E]),
		 ans(N, R)),
	  ex_tru_val(in(1, A),
		 O,
		 ths(P, T, Q, U),
		 anc(D, [faulty(A)|E]),
		 ans(R, V)),
	  ex_tru_val(in(2, A),
		 S,
		 ths(T, X, U, Z),
		 anc(D, [faulty(A)|E]),
		 ans(V, B1))
	),
	ex_not_val(out(1, A),
		   W,
		   ths(X, Y, Z, A1),
		   anc(D, [faulty(A)|E]),
		   ans(B1, C1)).
prove_not_gate(D, A, B, anc(C, E)) :-
	( prove_tru_ttable(A,
		       G,
		       H,
		       F,
		       B,
		       anc(C, [gate(D, A)|E])),
	  prove_tru_opp(F,
		    I,
		    B,
		    anc(C, [gate(D, A)|E])),
	  prove_tru_val(in(1, D),
		    G,
		    B,
		    anc(C, [gate(D, A)|E])),
	  prove_tru_val(in(2, D),
		    H,
		    B,
		    anc(C, [gate(D, A)|E]))
	),
	prove_tru_faulty(D,
		     B,
		     anc(C, [gate(D, A)|E])),
	prove_not_val(out(1, D),
		      I,
		      B,
		      anc(C, [gate(D, A)|E])).
ex_not_gate(E, A, ths(B, Y, C, A1), anc(D, F), ans(G, C1)) :-
	( ex_tru_ttable(A,
		    L,
		    P,
		    H,
		    ths(B, I, C, J),
		    anc(D, [gate(E, A)|F]),
		    ans(G, K)),
	  ex_tru_opp(H,
		 W,
		 ths(I, M, J, N),
		 anc(D, [gate(E, A)|F]),
		 ans(K, O)),
	  ex_tru_val(in(1, E),
		 L,
		 ths(M, Q, N, R),
		 anc(D, [gate(E, A)|F]),
		 ans(O, S)),
	  ex_tru_val(in(2, E),
		 P,
		 ths(Q, T, R, U),
		 anc(D, [gate(E, A)|F]),
		 ans(S, V))
	),
	ex_tru_faulty(E,
		  ths(T, X, U, Z),
		  anc(D, [gate(E, A)|F]),
		  ans(V, B1)),
	ex_not_val(out(1, E),
		   W,
		   ths(X, Y, Z, A1),
		   anc(D, [gate(E, A)|F]),
		   ans(B1, C1)).
prove_not_ttable(D, E, F, A, B, anc(C, G)) :-
	( prove_tru_opp(A,
		    I,
		    B,
		    anc(C,
			[ttable(D, E, F, A)|G])),
	  prove_tru_val(in(1, H),
		    E,
		    B,
		    anc(C,
			[ttable(D, E, F, A)|G])),
	  prove_tru_val(in(2, H),
		    F,
		    B,
		    anc(C,
			[ttable(D, E, F, A)|G]))
	),
	prove_tru_gate(H,
		   D,
		   B,
		   anc(C,
		       [ttable(D, E, F, A)|G])),
	prove_tru_faulty(H,
		     B,
		     anc(C,
			 [ttable(D, E, F, A)|G])),
	prove_not_val(out(1, H),
		      I,
		      B,
		      anc(C,
			  [ttable(D, E, F, A)|G])).
ex_not_ttable(E, F, G, A, ths(B, Y, C, A1), anc(D, H), ans(I, C1)) :-
	( ex_tru_opp(A,
		 W,
		 ths(B, J, C, K),
		 anc(D,
		     [ttable(E, F, G, A)|H]),
		 ans(I, L)),
	  ex_tru_val(in(1, M),
		 F,
		 ths(J, N, K, O),
		 anc(D,
		     [ttable(E, F, G, A)|H]),
		 ans(L, P)),
	  ex_tru_val(in(2, M),
		 G,
		 ths(N, Q, O, R),
		 anc(D,
		     [ttable(E, F, G, A)|H]),
		 ans(P, S))
	),
	ex_tru_gate(M,
		E,
		ths(Q, T, R, U),
		anc(D, [ttable(E, F, G, A)|H]),
		ans(S, V)),
	ex_tru_faulty(M,
		  ths(T, X, U, Z),
		  anc(D,
		      [ttable(E, F, G, A)|H]),
		  ans(V, B1)),
	ex_not_val(out(1, M),
		   W,
		   ths(X, Y, Z, A1),
		   anc(D,
		       [ttable(E, F, G, A)|H]),
		   ans(B1, C1)).
prove_not_opp(C, D, A, anc(B, E)) :-
	( prove_tru_val(in(1, F),
		    G,
		    A,
		    anc(B, [opp(C, D)|E])),
	  prove_tru_val(in(2, F),
		    H,
		    A,
		    anc(B, [opp(C, D)|E]))
	),
	prove_tru_ttable(I,
		     G,
		     H,
		     C,
		     A,
		     anc(B, [opp(C, D)|E])),
	prove_tru_gate(F,
		   I,
		   A,
		   anc(B, [opp(C, D)|E])),
	prove_tru_faulty(F,
		     A,
		     anc(B, [opp(C, D)|E])),
	prove_not_val(out(1, F),
		      D,
		      A,
		      anc(B, [opp(C, D)|E])).
ex_not_opp(D, E, ths(A, Y, B, A1), anc(C, F), ans(G, C1)) :-
	( ex_tru_val(in(1, H),
		 L,
		 ths(A, I, B, J),
		 anc(C, [opp(D, E)|F]),
		 ans(G, K)),
	  ex_tru_val(in(2, H),
		 M,
		 ths(I, N, J, O),
		 anc(C, [opp(D, E)|F]),
		 ans(K, P))
	),
	ex_tru_ttable(Q,
		  L,
		  M,
		  D,
		  ths(N, R, O, S),
		  anc(C, [opp(D, E)|F]),
		  ans(P, T)),
	ex_tru_gate(H,
		Q,
		ths(R, U, S, V),
		anc(C, [opp(D, E)|F]),
		ans(T, W)),
	ex_tru_faulty(H,
		  ths(U, X, V, Z),
		  anc(C, [opp(D, E)|F]),
		  ans(W, B1)),
	ex_not_val(out(1, H),
		   E,
		   ths(X, Y, Z, A1),
		   anc(C, [opp(D, E)|F]),
		   ans(B1, C1)).
prove_not_val(in(1, A), D, B, anc(C, E)) :-
	prove_tru_val(in(2, A),
		  F,
		  B,
		  anc(C, [val(in(1, A), D)|E])),
	prove_tru_opp(G,
		  I,
		  B,
		  anc(C, [val(in(1, A), D)|E])),
	prove_tru_ttable(H,
		     D,
		     F,
		     G,
		     B,
		     anc(C, [val(in(1, A), D)|E])),
	prove_tru_gate(A,
		   H,
		   B,
		   anc(C, [val(in(1, A), D)|E])),
	prove_tru_faulty(A,
		     B,
		     anc(C, [val(in(1, A), D)|E])),
	prove_not_val(out(1, A),
		      I,
		      B,
		      anc(C, [val(in(1, A), D)|E])).
ex_not_val(in(1, A), E, ths(B, Y, C, A1), anc(D, F), ans(G, C1)) :-
	ex_tru_val(in(2, A),
	       K,
	       ths(B, H, C, I),
	       anc(D, [val(in(1, A), E)|F]),
	       ans(G, J)),
	ex_tru_opp(L,
	       W,
	       ths(H, M, I, N),
	       anc(D, [val(in(1, A), E)|F]),
	       ans(J, O)),
	ex_tru_ttable(P,
		  E,
		  K,
		  L,
		  ths(M, Q, N, R),
		  anc(D, [val(in(1, A), E)|F]),
		  ans(O, S)),
	ex_tru_gate(A,
		P,
		ths(Q, T, R, U),
		anc(D, [val(in(1, A), E)|F]),
		ans(S, V)),
	ex_tru_faulty(A,
		  ths(T, X, U, Z),
		  anc(D, [val(in(1, A), E)|F]),
		  ans(V, B1)),
	ex_not_val(out(1, A),
		   W,
		   ths(X, Y, Z, A1),
		   anc(D, [val(in(1, A), E)|F]),
		   ans(B1, C1)).
prove_not_val(in(2, A), D, B, anc(C, E)) :-
	prove_tru_val(in(1, A),
		  F,
		  B,
		  anc(C, [val(in(2, A), D)|E])),
	prove_tru_opp(G,
		  I,
		  B,
		  anc(C, [val(in(2, A), D)|E])),
	prove_tru_ttable(H,
		     F,
		     D,
		     G,
		     B,
		     anc(C, [val(in(2, A), D)|E])),
	prove_tru_gate(A,
		   H,
		   B,
		   anc(C, [val(in(2, A), D)|E])),
	prove_tru_faulty(A,
		     B,
		     anc(C, [val(in(2, A), D)|E])),
	prove_not_val(out(1, A),
		      I,
		      B,
		      anc(C, [val(in(2, A), D)|E])).
ex_not_val(in(2, A), E, ths(B, Y, C, A1), anc(D, F), ans(G, C1)) :-
	ex_tru_val(in(1, A),
	       K,
	       ths(B, H, C, I),
	       anc(D, [val(in(2, A), E)|F]),
	       ans(G, J)),
	ex_tru_opp(L,
	       W,
	       ths(H, M, I, N),
	       anc(D, [val(in(2, A), E)|F]),
	       ans(J, O)),
	ex_tru_ttable(P,
		  K,
		  E,
		  L,
		  ths(M, Q, N, R),
		  anc(D, [val(in(2, A), E)|F]),
		  ans(O, S)),
	ex_tru_gate(A,
		P,
		ths(Q, T, R, U),
		anc(D, [val(in(2, A), E)|F]),
		ans(S, V)),
	ex_tru_faulty(A,
		  ths(T, X, U, Z),
		  anc(D, [val(in(2, A), E)|F]),
		  ans(V, B1)),
	ex_not_val(out(1, A),
		   W,
		   ths(X, Y, Z, A1),
		   anc(D, [val(in(2, A), E)|F]),
		   ans(B1, C1)).

% fact(<-(n(ok(_27488)),faulty(_27488))).
prove_not_ok(A, B, anc(C, D)) :-
	prove_tru_faulty(A, B, anc(C, [ok(A)|D])).
ex_not_ok(A, B, anc(C, D), E) :-
	ex_tru_faulty(A, B, anc(C, [ok(A)|D]), E).
prove_not_faulty(A, B, anc(C, D)) :-
	prove_tru_ok(A, B, anc(C, [faulty(A)|D])).
ex_not_faulty(A, B, anc(C, D), E) :-
	ex_tru_ok(A, B, anc(C, [faulty(A)|D]), E).

% fact(opp(on,off)).
prove_tru_opp(on, off, _, _).
ex_tru_opp(on, off, ths(A, A, B, B), _, ans(C, C)).

% fact(opp(off,on)).
prove_tru_opp(off, on, _, _).
ex_tru_opp(off, on, ths(A, A, B, B), _, ans(C, C)).

% fact(ttable(and,on,on,on)).
prove_tru_ttable(and, on, on, on, _, _).
ex_tru_ttable(and, on, on, on, ths(A, A, B, B), _, ans(C, C)).

% fact(ttable(and,off,anything,off)).
prove_tru_ttable(and, off, anything, off, _, _).
ex_tru_ttable(and, off, anything, off, ths(A, A, B, B), _, ans(C, C)).

% fact(ttable(and,anything,off,off)).
prove_tru_ttable(and, anything, off, off, _, _).
ex_tru_ttable(and, anything, off, off, ths(A, A, B, B), _, ans(C, C)).

% fact(ttable(or,off,off,off)).
prove_tru_ttable(or, off, off, off, _, _).
ex_tru_ttable(or, off, off, off, ths(A, A, B, B), _, ans(C, C)).

% fact(ttable(or,on,anything,on)).
prove_tru_ttable(or, on, anything, on, _, _).
ex_tru_ttable(or, on, anything, on, ths(A, A, B, B), _, ans(C, C)).

% fact(ttable(or,anything,on,on)).
prove_tru_ttable(or, anything, on, on, _, _).
ex_tru_ttable(or, anything, on, on, ths(A, A, B, B), _, ans(C, C)).

% fact(ttable(xor,off,on,on)).
prove_tru_ttable(xor, off, on, on, _, _).
ex_tru_ttable(xor, off, on, on, ths(A, A, B, B), _, ans(C, C)).

% fact(ttable(xor,off,off,off)).
prove_tru_ttable(xor, off, off, off, _, _).
ex_tru_ttable(xor, off, off, off, ths(A, A, B, B), _, ans(C, C)).

% fact(<-(ttable(xor,on,_3386,_3388),opp(_3386,_3388))).
prove_tru_ttable(xor, on, A, B, C, anc(D, E)) :-
	prove_tru_opp(A,
		  B,
		  C,
		  anc([ttable(xor, on, A, B)|D], E)).
ex_tru_ttable(xor, on, A, B, C, anc(D, E), F) :-
	ex_tru_opp(A,
	       B,
	       C,
	       anc([ttable(xor, on, A, B)|D], E),
	       F).
prove_not_opp(A, B, C, anc(D, E)) :-
	prove_not_ttable((xor),
			 on,
			 A,
			 B,
			 C,
			 anc(D, [opp(A, B)|E])).
ex_not_opp(A, B, C, anc(D, E), F) :-
	ex_not_ttable((xor),
		      on,
		      A,
		      B,
		      C,
		      anc(D, [opp(A, B)|E]),
		      F).

% fact(<-(val(_5416,_5418),(ne(_5418,anything),conn(_5428,_5416),val(_5428,_5418)))).
prove_tru_val(C, A, B, anc(D, E)) :-
	prove_tru_ne(A,
		 anything,
		 B,
		 anc([val(C, A)|D], E)),
	prove_tru_conn(F,
		   C,
		   B,
		   anc([val(C, A)|D], E)),
	prove_tru_val(F,
		  A,
		  B,
		  anc([val(C, A)|D], E)).
ex_tru_val(D, A, ths(B, M, C, O), anc(E, F), ans(G, Q)) :-
	ex_tru_ne(A,
	      anything,
	      ths(B, H, C, I),
	      anc([val(D, A)|E], F),
	      ans(G, J)),
	ex_tru_conn(K,
		D,
		ths(H, L, I, N),
		anc([val(D, A)|E], F),
		ans(J, P)),
	ex_tru_val(K,
	       A,
	       ths(L, M, N, O),
	       anc([val(D, A)|E], F),
	       ans(P, Q)).
prove_not_ne(C, anything, A, anc(B, D)) :-
	( prove_tru_conn(E,
		     F,
		     A,
		     anc(B, [ne(C, anything)|D])),
	  prove_tru_val(E,
		    C,
		    A,
		    anc(B, [ne(C, anything)|D]))
	),
	prove_not_val(F,
		      C,
		      A,
		      anc(B, [ne(C, anything)|D])).
ex_not_ne(D, anything, ths(A, M, B, O), anc(C, E), ans(F, Q)) :-
	( ex_tru_conn(G,
		  K,
		  ths(A, H, B, I),
		  anc(C, [ne(D, anything)|E]),
		  ans(F, J)),
	  ex_tru_val(G,
		 D,
		 ths(H, L, I, N),
		 anc(C, [ne(D, anything)|E]),
		 ans(J, P))
	),
	ex_not_val(K,
		   D,
		   ths(L, M, N, O),
		   anc(C, [ne(D, anything)|E]),
		   ans(P, Q)).
prove_not_conn(A, D, B, anc(C, E)) :-
	prove_tru_val(A,
		  F,
		  B,
		  anc(C, [conn(A, D)|E])),
	prove_tru_ne(F,
		 anything,
		 B,
		 anc(C, [conn(A, D)|E])),
	prove_not_val(D,
		      F,
		      B,
		      anc(C, [conn(A, D)|E])).
ex_not_conn(A, E, ths(B, M, C, O), anc(D, F), ans(G, Q)) :-
	ex_tru_val(A,
	       H,
	       ths(B, I, C, J),
	       anc(D, [conn(A, E)|F]),
	       ans(G, K)),
	ex_tru_ne(H,
	      anything,
	      ths(I, L, J, N),
	      anc(D, [conn(A, E)|F]),
	      ans(K, P)),
	ex_not_val(E,
		   H,
		   ths(L, M, N, O),
		   anc(D, [conn(A, E)|F]),
		   ans(P, Q)).
prove_not_val(A, D, B, anc(C, E)) :-
	prove_tru_conn(A,
		   F,
		   B,
		   anc(C, [val(A, D)|E])),
	prove_tru_ne(D,
		 anything,
		 B,
		 anc(C, [val(A, D)|E])),
	prove_not_val(F,
		      D,
		      B,
		      anc(C, [val(A, D)|E])).
ex_not_val(A, E, ths(B, M, C, O), anc(D, F), ans(G, Q)) :-
	ex_tru_conn(A,
		K,
		ths(B, H, C, I),
		anc(D, [val(A, E)|F]),
		ans(G, J)),
	ex_tru_ne(E,
	      anything,
	      ths(H, L, I, N),
	      anc(D, [val(A, E)|F]),
	      ans(J, P)),
	ex_not_val(K,
		   E,
		   ths(L, M, N, O),
		   anc(D, [val(A, E)|F]),
		   ans(P, Q)).

% prolog(ne(_12240,_12242)).
ex_tru_ne(D, E, ths(A, A, B, B), _, ans(C, C)) :-
	ne(D, E).
prove_tru_ne(A, B, _, _) :-
	ne(A, B).

% assertz((ne(_12646,_12648):- \+_12646=_12648)).

% fact(<-(n(val(_12724,off)),val(_12724,on))).
prove_not_val(A, off, B, anc(C, D)) :-
	prove_tru_val(A, on, B, anc(C, [val(A, off)|D])).
ex_not_val(A, off, B, anc(C, D), E) :-
	ex_tru_val(A,
	       on,
	       B,
	       anc(C, [val(A, off)|D]),
	       E).
prove_not_val(A, on, B, anc(C, D)) :-
	prove_tru_val(A, off, B, anc(C, [val(A, on)|D])).
ex_not_val(A, on, B, anc(C, D), E) :-
	ex_tru_val(A,
	       off,
	       B,
	       anc(C, [val(A, on)|D]),
	       E).

% fact(<-(diag(_14550,_14552),(val(out(1,f1),_14550),val(out(2,f1),_14552)))).
prove_tru_diag(A, C, B, anc(D, E)) :-
	prove_tru_val(out(1, f1),
		  A,
		  B,
		  anc([diag(A, C)|D], E)),
	prove_tru_val(out(2, f1),
		  C,
		  B,
		  anc([diag(A, C)|D], E)).
ex_tru_diag(A, D, ths(B, I, C, K), anc(E, F), ans(G, M)) :-
	ex_tru_val(out(1, f1),
	       A,
	       ths(B, H, C, J),
	       anc([diag(A, D)|E], F),
	       ans(G, L)),
	ex_tru_val(out(2, f1),
	       D,
	       ths(H, I, J, K),
	       anc([diag(A, D)|E], F),
	       ans(L, M)).
prove_not_val(out(1, f1), C, A, anc(B, D)) :-
	prove_tru_val(out(2, f1),
		  E,
		  A,
		  anc(B, [val(out(1, f1), C)|D])),
	prove_not_diag(C,
		       E,
		       A,
		       anc(B, [val(out(1, f1), C)|D])).
ex_not_val(out(1, f1), D, ths(A, I, B, K), anc(C, E), ans(F, M)) :-
	ex_tru_val(out(2, f1),
	       G,
	       ths(A, H, B, J),
	       anc(C, [val(out(1, f1), D)|E]),
	       ans(F, L)),
	ex_not_diag(D,
		    G,
		    ths(H, I, J, K),
		    anc(C, [val(out(1, f1), D)|E]),
		    ans(L, M)).
prove_not_val(out(2, f1), C, A, anc(B, D)) :-
	prove_tru_val(out(1, f1),
		  E,
		  A,
		  anc(B, [val(out(2, f1), C)|D])),
	prove_not_diag(E,
		       C,
		       A,
		       anc(B, [val(out(2, f1), C)|D])).
ex_not_val(out(2, f1), D, ths(A, I, B, K), anc(C, E), ans(F, M)) :-
	ex_tru_val(out(1, f1),
	       G,
	       ths(A, H, B, J),
	       anc(C, [val(out(2, f1), D)|E]),
	       ans(F, L)),
	ex_not_diag(G,
		    D,
		    ths(H, I, J, K),
		    anc(C, [val(out(2, f1), D)|E]),
		    ans(L, M)).
