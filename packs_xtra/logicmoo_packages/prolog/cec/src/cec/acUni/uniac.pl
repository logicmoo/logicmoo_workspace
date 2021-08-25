/*
 *	file:		uniac.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the AC-unification algorithm.
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/* Version 1.2a  	17.10.86
   ------------
   Bemerkung: Einbau von Zeitmessungen: cont(ac_uniAC_Total,T) liefert die
	      gesamte von der AC-Unifikation verbrauchte Zeit.
	      cont(ac_dio_Total,T) liefert die gesamte Zeit, die das
	      Berechnen der Loesungsbasis und das finden der geeigneten 
	      Teilmenge braucht. (noch nicht eingeschaltet)
   Autor: Hubert Baumeister
 */
/* ac_uniAC(N,M)
   -------------
   N und M sind zwei Prologterme, die miteinander unifiziert werden, so dass sie
   bezueglich Assoziativitaet und Komutativitaet bestimmter Funktionssymbole 
   gleich sind.
   Das Programm prueft mit dem Praedikat 'ac_ist_AC(f)' ob f ein Assoziatives und
   Kommutatives Funktionssymbol ist.
   Fuer jedes AC-Symbol muss dann also ein Fakt
   (z.B.: ac_ist_AC(+).  ac_ist_AC(*).) vorhanden sein.
   Ueber Backtracking bekommt man alternative Unifikationen.
 */
 
:-compile(library(unify)).





ac_uniAC(_,N,M):-
	ac_uniAC(N,M).



%ac_uniAC(N,M):-
%	totalRuntime(Zeit1),
%	ac_uniAC_Start := Zeit1,
	
%	(
%	(ac_ist_C(_) ->
%		ac_uniAC1(N,M)
%	;	unify(N,M)
%	)
%; 
%	add_zeit(ac_uniAC_Start,ac_uniAC_Total),!,fail),

%	add_zeit(ac_uniAC_Start,ac_uniAC_Total)
%	.



ac_uniAC(N,M):-
	(ac_ist_C(_) ->
		ac_uniAC1(N,M)
	;	unify(N,M)
	).



add_zeit(Start,Total):-
	cont(Start,T1),
	totalRuntime(T2),
	Start := T2,
	Used is T2 - T1,
	(cont(Total,Alt);
	Alt = 0),
	Summe is Used + Alt,
	Total := Summe,
	!.


ac_uniAC1(N,M):-
	var(N),
	var(M),
	N=M.

ac_uniAC1(N,M):-
	var(N),
	!,
	\+ var(M),
	not ac_occurs(M,N),
	N=M.


ac_uniAC1(N,M):- 
	var(M),
	\+ var(N), 
	!,
	\+ ac_occurs(N,M),
	N=M.


ac_uniAC1(N,M):-
	\+ var(M),
	\+ var(N),
	M =.. [F|Margs],
	N =.. [F|Nargs],
	(ac_ist_AC(F)->
		ac_del_assoc(F,Nargs,Nargs1),
		ac_del_assoc(F,Margs,Margs1),
		!,
		ac_unify_AC(F,Margs1,Nargs1);
	(ac_ist_C(F) ->
		Margs=[M1,M2],
		Nargs=[N1,N2],
		!,
%		ac_unilist(MR,NR),
		(	ac_unilist([M1,M2],[N1,N2])
		;	ac_unilist([M1,M2],[N2,N1])
		)
	;	ac_unilist(Nargs,Margs)
	)).

ac_occurs(T,V):-
	var(T),
	!,
	T==V.
ac_occurs(T,V):-
	functor(T,_,N),
	subterm(_,N,T,TJ),
	ac_occurs(TJ,V),
	!.

ac_unilist([],[]).
ac_unilist([M|Margs],[N|Nargs]):-
	ac_uniAC1(M,N),
	ac_unilist(Margs,Nargs).

ac_unify_AC(F,[],[]).
ac_unify_AC(F,Margs,Nargs):-
	Margs \== [],
	Nargs \== [],
	ac_dio(F,Margs,Nargs,Tl1,Tl2),
	ac_unilist(Tl1,Tl2).

/* ac_dio(F,Margs,Nargs,Tl1,Tl2)
   --------------------------------------
   Tl1, Tl2 sind die Termlisten, die mit einer der Loesungen, der zu den Listen
   Margs und Nargs gehoerenden Diophantischen Gleichung verbunden sind,
   und die spaeter noch unifiziert werden muessen.
 */

%ac_dio(F,Margs,Nargs,Tl1,Tl2):-
%	ac_sort_term(Margs,Margs1),
%	ac_sort_term(Nargs,Nargs1),
%	ac_streiche_gleich(Margs1,Nargs1,Gek1,Gek2),
%        
%	ac_sort_Anzahl(Gek1,Gek12),
%	ac_sort_Anzahl(Gek2,Gek22),
%	
%	ac_koeffizienten(Gek12,K1),
%	ac_koeffizienten(Gek22,K2),
%
%	((ac_bekannte_loesung(K1,K2,M),!,append(Gek12,Gek22,Tl));
%	 (ac_bekannte_loesung(K2,K1,M),!,append(Gek22,Gek12,Tl));
%	 (ac_loesen(K1,K2,M),!,
%            assert(ac_bekannte_loesung(K1,K2,M)),append(Gek12,Gek22,Tl))),
%
%	ac_elem_k_groesser_2(Tl,M,C,V),
%	bagof(M2,ac_teil_menge(Tl,C,V,M2),Loesung),
%	ac_term(Tl,Tl2),
%	!,
%	ac_elem(L,Loesung),
%	ac_term_liste(F,L,Tl1).

ac_dio(F,Margs,Nargs,Tl1,Tl2):-
	ac_sort_term(Margs,Margs1),
	ac_sort_term(Nargs,Nargs1),
	ac_streiche_gleich(Margs1,Nargs1,Gek12,Gek22),
        
	ac_koeffizienten(Gek12,K1),
	ac_koeffizienten(Gek22,K2),

%	totalRuntime(T1),
%	ac_dio_Start := T1,

%	(
	ac_loesen(K1,K2,M),
	append(Gek12,Gek22,Tl),

	ac_elem_k_groesser_2(Tl,M,C,V),
	bagof(M2,ac_teil_menge(Tl,C,V,M2),Loesung)
%	;
%	add_zeit(ac_dio_Start,ac_dio_Total),
%	!,
%	fail)
	,

%	add_zeit(ac_dio_Start,ac_dio_Total),

	ac_term(Tl,Tl2),
	!,
	ac_elem(L,Loesung),
	ac_term_liste(F,L,Tl1).

/* ac_elem(E,M)
   ---------
   E ist Element von der Menge M, die als Liste dargestellt ist.
 */

ac_elem(E,[E|M]).
ac_elem(E,[E1|M]):-
	ac_elem(E,M),
	E1 \== E.

ac_del_assoc(F,[E|Zl],L):-
	not atom(E),
	not var(E),
	E =.. [F|Arg],!,
	ac_del_assoc(F,Arg,L1),
	ac_del_assoc(F,Zl,L2),
	append(L1,L2,L),
	!.
ac_del_assoc(F,[E|Zl],[E|T]):-
	ac_del_assoc(F,Zl,T),
	!.
ac_del_assoc(F,[],[]).

ac_term_liste(F,[Lh|Lt],Liste):-
	ac_init_args(Lh,Var,Argl),
	ac_term_liste1(Lt,Argl,Arg1l),
	ac_build_term(F,Arg1l,Liste),
	!.
ac_term_liste(_,[],[]).

ac_term_liste1([],Argl,Argl):-!.
ac_term_liste1([Lh|Lt],Argl,Arg1l):-
	ac_build_args_2(Lh,Var,Argl,Arg2l),
	ac_term_liste1(Lt,Arg2l,Arg1l).

ac_build_term(F,[],[]):-!.
ac_build_term(F,[Ah|At],[Lh|Lt]):-
	ac_rreduce(F,Ah,Lh),               %  HG
	ac_build_term(F,At,Lt).

ac_rreduce(_,[T],T):-!.
ac_rreduce(F,[T|Ts],FT):-
	ac_rreduce(F,Ts,FTs),
	FT=..[F,T,FTs],
	!.

ac_init_args([],Var,[]):-!.
ac_init_args([Lh|Lt],Var,[Argh|Argt]):-
	ac_build_args(Lh,Var,Argh),
	ac_init_args(Lt,Var,Argt).

ac_build_args(0,Var,[]):-!.
ac_build_args(N,Var,[Var|Arg]):-
	N1 is N - 1,
	ac_build_args(N1,Var,Arg).

ac_build_args_2([],Var,[],[]):-!.
ac_build_args_2([Lh|Lt],Var,[Argh|Argt],[Arg1h|Arg1t]):-
	ac_build2_args_2(Lh,Var,Argh,Arg1h),
	ac_build_args_2(Lt,Var,Argt,Arg1t).

ac_build2_args_2(0,Var,Arg,Arg):-!.
ac_build2_args_2(N,Var,Arg,Arg1):-
	N1 is N - 1,
	ac_build2_args_2(N1,Var,[Var|Arg],Arg1).

ac_term([],[]):-!.
ac_term([sym(T,_,_)|Tr],[T|T1r]):-
	ac_term(Tr,T1r).

/* ac_sort_term(L,SL)
 * L wird in Reihenfolge der Terme sortiert und zu jedem Element der
 * Liste wird vermerkt, wie oft es auftritt, und ob es ein Atom, Term
 * oder eine Variable ist.
 * Die Struktur, die diese Terme aufnimmt ist: sym(Term,Art,Anzahl).
 * Die Elemente von L werden in einen geordneten Baum eingefuegt und
 * SL ist dann der Inorder Durchlauf des Baumes
 */

ac_sort_term(L,SL):- 
	ac_ist_Baum(L,[],Baum), 
	ac_inorder(SL,Baum),
	!.

ac_ist_Baum([],Baum,Baum):-!.
ac_ist_Baum([E|R],Baum,Baum2):-
	ac_insert(E,Baum,Baum1),
	ac_ist_Baum(R,Baum1,Baum2).

ac_insert(E,[],[sym(E,Art,1),[],[]]):- ist_art(E,Art),!.
ac_insert(E,[sym(E1,Art,Anzahl),L,R],[sym(E,Art,Anzahl1),L,R]):-
	E == E1,
	Anzahl1 is Anzahl + 1,
	!.
ac_insert(E,[sym(X,Art,Anzahl),L,R],[sym(X,Art,Anzahl),L1,R1]):-
	E @< X -> (ac_insert(E,L,L1),R1 = R);
		  (ac_insert(E,R,R1),L1 = L).

ac_inorder([],[]):-!.
ac_inorder(SL,[W,R,L]):-
	ac_inorder(SL1,R),
	ac_inorder(SL2,L),
	append(SL1,[W],SL3),
	append(SL3,SL2,SL).

ist_art(E,atom):- atom(E),!.
ist_art(E,variable):- var(E),!.
ist_art(E,ac_term).

/* ac_streiche_gleich(T1,T2,L1,L2)
 * Die gleichen Elemente aus den Listen T1 und T2 werden geloescht.
 * Vorausgesetzt ist, das T1 und T2 sortiert sind
 * Das Ergebnis steht in L1 fuer T1 und in L2 fuer T2.
 */

ac_streiche_gleich([],L,[],L):-!.
ac_streiche_gleich(L,[],L,[]):-!.
ac_streiche_gleich([H1|T1],[H2|T2],L1,L2):-
	H1 == H2,
	ac_streiche_gleich(T1,T2,L1,L2),
	!.
ac_streiche_gleich([sym(T,Art,Anzahl1)|T1],[sym(T3,Art,Anzahl2)|T2],L1,L2):-
	T == T3,
	(Anzahl1 > Anzahl2 -> (L1 = [sym(T,Art,Anzahl3)|Lr1], L2 = Lr2,
			       Anzahl3 is Anzahl1 - Anzahl2);
			      (L2 = [sym(T,Art,Anzahl3)|Lr2], L1 = Lr1,
			       Anzahl3 is Anzahl2 - Anzahl1)),
	ac_streiche_gleich(T1,T2,Lr1,Lr2),
	!.
ac_streiche_gleich([H1|T1],[H2|T2],[H1|L1],L2):-
	H1 @< H2,
	ac_streiche_gleich(T1,[H2|T2],L1,L2),
	!.
ac_streiche_gleich([H1|T1],[H2|T2],L1,[H2|L2]):-
	ac_streiche_gleich([H1|T1],T2,L1,L2).

/* ac_koeffizienten(T,Anz_l)
 * Aus einer Liste mit Elementen der Form sym(Term,Art,Anzahl) wird
 * die Komponente Anzahl in der Liste Anz_l zurueckgegeben.
 */

ac_koeffizienten([sym(T,Art,Anz)|R],[Anz|R1]):-
	ac_koeffizienten(R,R1),
	!.
ac_koeffizienten([],[]).

/* ac_elem_k_groesser_2(Tl,M,C,V)
 * M enthaelt die Basisloesung der linearen ac_diophantischen Gleichung, die
 * durch die Koeffizienten in Tl gebildet wird.
 * C enthaelt die Menge von Loesungen, die in der Loesung an der Stelle von 
 * Atomen und Termen mindesten einen Koeffizienten mit eins hat, keine 
 * Koeffizienten groesser als eins und wenn mehrere Koeffizienten eins sind,
 * dann sind die Terme unifizierbar.
 * Die Loesungen aus V enthalten nur an der Stelle von Variablen Koeffizienten
 * ungleich null.
 */

ac_elem_k_groesser_2(Tl,[S|R],C,V):-
	ac_suche_k_gleich_1(Tl,S,Cl),
	ac_unifizierbar(Cl),
	((Cl = []) ->(V = [S|Vr], ac_elem_k_groesser_2(Tl,R,C,Vr));
		     (C = [S|Cr], ac_elem_k_groesser_2(Tl,R,Cr,V))),
	!.
ac_elem_k_groesser_2(Tl,[S|R],C,V):-
	ac_elem_k_groesser_2(Tl,R,C,V),
	!.
ac_elem_k_groesser_2(Tl,[],[],[]).

ac_suche_k_gleich_1([sym(_,variable,_)|T],[S|R],Cl):-
	ac_suche_k_gleich_1(T,R,Cl),
	!.
ac_suche_k_gleich_1([H|T],[0|R],Cl):-
	ac_suche_k_gleich_1(T,R,Cl),
	!.
ac_suche_k_gleich_1([H|T],[1|R],[H|Clt]):-
	ac_suche_k_gleich_1(T,R,Clt).
ac_suche_k_gleich_1([],[],[]).

ac_unifizierbar([sym(_,atom,_)]):-!.
ac_unifizierbar([sym(_,ac_term,_)|R]):-
	ac_unifizierbar2(R).
ac_unifizierbar([]).

ac_unifizierbar2([sym(_,ac_term,_)|R]):-
	ac_unifizierbar2(R),
	!.
ac_unifizierbar2([]).

ac_teil_menge(Tl,C,V,M):-
	ac_init_summe(Tl,S,Anzahl),
	((Anzahl = 0, !);
	 (C = [], !, fail);
	 ac_teilmenge_k_gleich_eins2(Tl,C,S,Anzahl,C1)),
	ac_teilmenge(Tl,C1,V,M).


/* ac_teilmenge_k_gleich_eins(Tl,C,M)
   -------------------------------
   Tl ist eine Liste mit Elementen der Form:
   sym(Term,Art,Anzahl) und C ist eine Menge von Teilmengen von M.
   C enthaelt die Teilmenge aus Loesungsbasis der Diophantischen Gleichung,
   bei denen mindestens ein Koeffizienten der
   Terme, die keine Variablen sind genau eins ist.
   M ist dann die Menge der Mengen aus Elementen von C, bei denen die Summe
   der Koeffizienten der Terme, die keine Variablen sind genau eins ist.
 */

ac_teilmenge_k_gleich_eins(Tl,C,M):-
	ac_init_summe(Tl,S,Anzahl),!,
	((Anzahl = 0, !);
	 (C = [], !, fail);
	 bagof(C1,ac_teilmenge_k_gleich_eins2(Tl,C,S,Anzahl,C1),M)).
ac_teilmenge_k_gleich_eins(_,_,[]).

ac_teilmenge_k_gleich_eins2(_,_,_,0,[]):-!.
ac_teilmenge_k_gleich_eins2(Tl,C,S,Anzahl,[Cvor|C1t]):-
	ac_c_vorschlag(C,Cvor,Crest),
	ac_okay(Tl,Cvor,S,Z,S1),
	Diff is Anzahl - Z,
	ac_teilmenge_k_gleich_eins2(Tl,Crest,S1,Diff,C1t).

ac_okay([sym(_,variable,_)|Tr],[Ch|Ct],[Sh|Sr],Z,[Sh|S1r]):- !,
	ac_okay(Tr,Ct,Sr,Z,S1r),
	!.
ac_okay([Th|Tr],[1|Ct],[0|Sr],Z,[1|S1r]):-
	ac_okay(Tr,Ct,Sr,Z1,S1r),
	Z is Z1 + 1,
	!.
ac_okay([Th|Tr],[0|Ct],[Sh|Sr],Z,[Sh|Sr1]):-
	ac_okay(Tr,Ct,Sr,Z,Sr1),
	!.
ac_okay([],[],[],0,[]).

/* ac_c_vorschlag(C,Cvor,Crest)
   -------------------------
   C ist eine Liste.
   Cvor ein Element aus der Liste und Crest der Rest der Liste nach Cvor.
 */

ac_c_vorschlag([Cvor|Crest],Cvor,Crest).
ac_c_vorschlag([Cvor|Crest],Cvor1,Crest1):-
	ac_c_vorschlag(Crest,Cvor1,Crest1).

/* ac_init_summe(Tl,S,Anzahl)
   -----------------------
   Tl ist ein Liste mit Elementen der Form: sym(Term,Art,Anzahl).
   S ist eine Liste von Zahlen, die an den Stellen eine 1 hat, an der 
   in Tl bei Art ein atom oder ein ac_term steht.
 */

ac_init_summe([sym(_,variable,_)|Tlt],[0|S],Anzahl):-
	ac_init_summe(Tlt,S,Anzahl),
	!.
ac_init_summe([Th|Tr],[0|S],Anzahl):-
	ac_init_summe(Tr,S,Anzahl1),
	Anzahl is Anzahl1 + 1,
	!.
ac_init_summe([],[],0).

/* ac_teilmenge(Tl,C,V,M).
   --------------------
   Tl ist ein Liste mit Elementen der Form: sym(Term,Art,Anzahl).
   C ist
   C ist die Menge der Mengen aus der Basisloesung, bei denen die Summe
   der Koeffizienten der Terme, die keine Variablen sind genau eins ist.
   V ist die Teilmenge der Basisloesung, deren Koeffizienten nur bei Termen
   die Variblen sind ungleich null sein koennen.
 */

ac_teilmenge(Tl,[],V,M):- !,
	ac_init_Maske(Tl,Maske,Anzahl),
	ac_teilmenge2(Tl,V,Maske,Anzahl,M).
ac_teilmenge(Tl,C,V,M):- !,
	ac_init_Bed(Tl,C,Maske,Anzahl,AnzVar),
	Diff is Anzahl - AnzVar,
	ac_teilmenge2(Tl,V,Maske,Diff,M1),
	append(C,M1,M).

ac_teilmenge2(_,V,_,0,M):- !,
	ac_teilmenge3(V,M).
ac_teilmenge2(Tl,V,Maske,Anzahl,[Vvor|M]):-
	ac_c_vorschlag(V,Vvor,Vrest),
	ac_bed(Tl,Vvor,Maske,Maske1,AnzVar),
	Diff is Anzahl - AnzVar,
	ac_teilmenge2(Tl,Vrest,Maske1,Diff,M).

ac_teilmenge3([_|_],[]).
ac_teilmenge3(V,[Vvor|M]):-
	ac_c_vorschlag(V,Vvor,Vrest),
	ac_teilmenge3(Vrest,M).
ac_teilmenge3([],[]).

ac_bed([sym(_,variable,_)|Tr],[N|Vr],[0|Mr],[1|M1r],Anzahl):-
	N > 0,!,
	ac_bed(Tr,Vr,Mr,M1r,Anzahl1),
	Anzahl is Anzahl1 + 1,
	!.
ac_bed([Th|Tr],[Vh|Vr],[Mh|Mr],[Mh|M1r],Anzahl):-
	ac_bed(Tr,Vr,Mr,M1r,Anzahl),
	!.
ac_bed([],[],[],[],0).

ac_init_Maske([sym(_,variable,_)|Tr],[0|Mr],Anzahl):-!,
	ac_init_Maske(Tr,Mr,Anzahl1),
	Anzahl is Anzahl1 + 1,
	!.
ac_init_Maske([Th|Tr],[0|Mr],Anzahl):-
	ac_init_Maske(Tr,Mr,Anzahl),
	!.
ac_init_Maske([],[],0).

ac_init_Bed(Tl,[Ch|Cr],Maske,Anzahl,AnzVar):-
	ac_init_Bed(Tl,Cr,Maske1,Anzahl,AnzVar1),
	ac_init_Bed2(Tl,Ch,Maske1,Maske,AnzVar2),
	AnzVar is AnzVar1 + AnzVar2,
	!.
ac_init_Bed(Tl,[],Maske,Anzahl,0):-
	ac_init_Maske(Tl,Maske,Anzahl).

ac_init_Bed2([sym(_,variable,_)|Tr],[N|Cr],[0|Mr],[1|M1r],AnzVar):-
	N > 0, !,
	ac_init_Bed2(Tr,Cr,Mr,M1r,AnzVar1),
	AnzVar is AnzVar1 + 1.
ac_init_Bed2([Th|Tr],[Ch|Cr],[Mh|Mr],[Mh|M1r],AnzVar):-
	ac_init_Bed2(Tr,Cr,Mr,M1r,AnzVar),
	!.
ac_init_Bed2([],[],[],[],0).

/* ac_loesen(A,B,M)  
 * M enthaelt die Basis Loesungen der lineare Diophantischen Gleichung, 
 * die durch die Koefizientenliste A und B gegeben ist.
 */
ac_loesen(A,B,Mat):-
	A \== [],
	B \== [],
	ac_ist_Maximum(A,MaxA,M,B,MaxB,N), /* Bestimme das Maximum der 
					   Koefizienten und die Anzahl der 
					   Elemente in A und B */
	MaxA1 is MaxA + 1,
	ac_ist_init_Maxy0(MaxA1,Maxy,N),/* Initialisiere die Max yi0 mit Max A */
	ac_ist_init_Eij_Dij(A,B,Eij,Dij), /* Initialisiere die Eij und Dij */
	bagof(S,ac_ist_loesung1(A,B,S,0,Maxy,MaxB,Eij,Dij),[M1h|M1t]),
	ac_ist_redundant(M1t,M2), /* entferne alle Loesungen die nicht minimal 
				  sind */
	ac_erzeuge(Sij,Eij,Dij,M,N,1),  /* Erzeuge die Loesungen, die auf 
					jeder Seite nur ein Element ungleich 
					null haben. */
	append(M2,Sij,Mat),
	!.
ac_loesen([],[],[]).

/* ac_ist_loesung1(A,B,S,Summe_Ai_Si,Maxy,MaxB,Ei,Di)
 * S ist die Loesung der linearen Diophantischen Gleichung mit den
 * Koeffizientenlisten A und B.
 * Summe_Ai_Si ist der Wert der Summe Ai Si, der bis jetzt gegebenen Si.
 * Maxy ist die Liste der Max yik der bis jetzt bekannten Sj 1<=j<=k
 * Eij ist die Matrix der lcm(ai,bj)/bj
 * Dij ist die Matrix der lcm(ai,bj)/ai
 */
ac_ist_loesung1([Ah|At],B,[Sk|S],Summe_Ai_Si,Maxy,MaxB,[Ei|Eij],[Di|Dij]):-
	ac_vorschlag(0,Sk,MaxB), /* Erzeuge ein Sk, dass die Bedingung a):
				 Si <= MaxB fuer alle i <= k erfuellt ist. */ 
	Neue_Summe_Ai_Si is Summe_Ai_Si + Ah * Sk,
	ac_maxyk_plus_1(Maxy,Sk,Ei,Di,Neu_Maxy),
	ac_summe(B,Neu_Maxy,Summe), /* Summe ist die Summe Bi Max yik */
	Neue_Summe_Ai_Si < Summe, /* Bedingung b) */
	ac_ist_loesung1(At,B,S,Neue_Summe_Ai_Si,Neu_Maxy,MaxB,Eij,Dij).
ac_ist_loesung1([],B,S,Summe_Ai_Si,Maxy,MaxB,Eij,Dij):-
	ac_ist_loesung2(B,S,Summe_Ai_Si,0,Maxy).
				      
ac_ist_loesung2([Bh|Bt],[Sl|S],Summe_Ai_Si,Summe_Bi_Si,[Maxymj|Maxy]):-
	M is Maxymj - 1,
	ac_vorschlag(0,Sl,M), /* Sl muss kleiner max ymj. Bed. c) */
	Neue_Summe_Bi_Si is Summe_Bi_Si + Bh * Sl,
	(Neue_Summe_Bi_Si > Summe_Ai_Si,!,fail; /* Bedingung d) */
	ac_ist_loesung2(Bt,S,Summe_Ai_Si,Neue_Summe_Bi_Si,Maxy)).
ac_ist_loesung2([],[],Summe_Ai_Si,Summe_Ai_Si,_).
	/* Summe ai si und Summe bi si muessen gleich sein */

/*---------------------------------------------------------------------------*/
/* ist_maximum(A,MaxA,M,B,MaxB,N)
   ------------------------------
   MaxA, MaxB sind die Maxima der Liste von Zahlen A und B, und M, N sind
   die Anzahl der Elemente in A und B.
 */

ac_ist_Maximum(A,MaxA,M,B,MaxB,N):-
	ac_ist_Max(A,MaxA,M),
	ac_ist_Max(B,MaxB,N),
	!.
ac_ist_Max([A],A,1):-!.
ac_ist_Max([Ah|At],MaxA,N):-
	ac_ist_Max(At,MaxA1,N1),
	(Ah > MaxA1 -> MaxA = Ah; 
		       MaxA = MaxA1),
	N is N1 + 1.

/*---------------------------------------------------------------------------*/
/* ac_ist_init_Maxy0(MaxA,L,N)
   ------------------------
   L ist eine Liste mit N Elementen der Form MaxA.
 */

ac_ist_init_Maxy0(MaxA,[MaxA],1):-!.
ac_ist_init_Maxy0(MaxA,[MaxA|Maxy],N):-
	N1 is N - 1,
	ac_ist_init_Maxy0(MaxA,Maxy,N1).

/*---------------------------------------------------------------------------*/
/* ac_erzeuge(Sij,Eij,Dij,M,N,I)
   --------------------------
   Eij und Dij sind Zahlenmatrizen. 1 <= i <= M , 1 <= j <= N.
   Sij ist eine Liste von N*M Listen mit N + M Elementen.
   Fuer die i*j.te Liste gilt: 
   An der i.ten Stelle steht Eij und an der M + j.ten Stelle Dij.
 */
ac_erzeuge(Sij,[Ei|Eij],[Di|Dij],M,N,I):-
	ac_erzeuge1(Sij1,Di,Ei,M,N,I,1),
	I1 is I + 1,
	ac_erzeuge(Sij2,Eij,Dij,M,N,I1),
	append(Sij1,Sij2,Sij),
	!.
ac_erzeuge([],[],[],_,_,_).

ac_erzeuge1([Sh|St],[E|Ei],[D|Di],M,N,I,J):-
	I2 is M - I + 1,
	J2 is N - J + 1,
	ac_build(M,I2,E,X),
	ac_build(N,J2,D,Y),
	append(X,Y,Sh),
	J1 is J + 1,
	ac_erzeuge1(St,Ei,Di,M,N,I,J1),
	!.
ac_erzeuge1([],[],[],_,_,_,_).

ac_build(0,_,_,[]):-!.
ac_build(M,M,D,[D|X]):-
	M1 is M - 1,
	ac_build(M1,M,D,X),
	!.
ac_build(M,I,D,[0|X]):-
	M1 is M - 1,
	ac_build(M1,I,D,X).

/*---------------------------------------------------------------------------*/
/* ac_ist_redundant(M,M1)
   -------------------
   M1 ist die Zahlenmatrix, die nur linear unabhaengige Vektoren aus M 
   enthaelt.
 */

ac_ist_redundant([M],[M]):-!.
ac_ist_redundant([Mh|Mt],[Mh|M]):-
	ac_loesche_groesser(Mh,Mt,M1),
	ac_ist_redundant(M1,M),
	!.
ac_ist_redundant([],[]).

ac_loesche_groesser(E,[Mh|Mt],M):-
	ac_kleiner(E,Mh),
	ac_loesche_groesser(E,Mt,M),
	!.
ac_loesche_groesser(E,[Mh|Mt],[Mh|M]):-
	ac_loesche_groesser(E,Mt,M),
	!.
ac_loesche_groesser(E,[],[]).

/* ac_kleiner(V1,V2)
   --------------
   V1 und V2 muesen gleichlange Zahlenvektoren sein.
   V1 ist komponentenweise kleiner als V2.
 */
ac_kleiner([Eh|Et],[Mh|Mt]):-
	Eh =< Mh,
	ac_kleiner(Et,Mt),
	!.
ac_kleiner([],[]).

/*---------------------------------------------------------------------------*/
/* ac_vorschlag(M,N,Max)
   ------------------
   N ist eine ganze Zahl zwischen einschliesslich M und Max.
 */
ac_vorschlag(M,M,Max):-M > Max, ! ,fail.
ac_vorschlag(M,M,Max).
ac_vorschlag(M,N1,Max):-N is M + 1,ac_vorschlag(N,N1,Max).

/*---------------------------------------------------------------------------*/
/* ac_summe(A,B,C)
   ------------
   A und B sind gleichlange Zahlenvektoren. C ist das Skalarprodukt von A und
   B.
 */
ac_summe([A],[B],C):-
	C is A * B,
	!.
ac_summe([Ah|A],[Bh|B],C):-
	ac_summe(A,B,C1),
	C is Ah * Bh + C1.

/* ac_ist_init_Eij_Dij(A,B,Eij,Dij)
   -----------------------------
   Eij und Dij sind Zahlenmatrizen und A, B Zahlenvektoren und es gilt:
   Eij = lcm(Ai,Bj)/Bi
   Dij = lcm(Ai,Bj)/Aj
 */

ac_ist_init_Eij_Dij([Ah|At],B,[Ei|Eij],[Di|Dij]):-
	ac_ist_init_Eij_Dij1(Ah,B,Ei,Di),
	ac_ist_init_Eij_Dij(At,B,Eij,Dij),
	!.
ac_ist_init_Eij_Dij([],B,[],[]).

ac_ist_init_Eij_Dij1(Ah,[Bh|Bt],[Eh|Et],[Dh|Dt]):-
	ac_lcm(Ah,Bh,KGV),
	integerDivision(KGV,Bh,Eh),
	integerDivision(KGV,Ah,Dh),
	ac_ist_init_Eij_Dij1(Ah,Bt,Et,Dt),
	!.
ac_ist_init_Eij_Dij1(_,[],[],[]).

ac_lcm(A,B,KGV):-
	ac_ggt(A,B,GGT),
	AB is A*B,	% hu!
	integerDivision(AB,GGT,KGV).

/* ac_ggt(A,B,C)
   ----------
   C = ac_ggt(A,B),
   A, B > 0.
 */

ac_ggt(A,A,A):- !.
ac_ggt(A,B,GGT):-
	A < B,!,
	B1 is B - A,
	ac_ggt(A,B1,GGT),
	!.
ac_ggt(A,B,GGT):-
	A1 is A - B,
	ac_ggt(A1,B,GGT),
	!.

/* ac_maxyk_plus_1(Maxy,S,Ei,Di,Neu_Maxy)
   -----------------------------------
   Maxy, Neu_Maxy, Ei, Di sind gleichlange Zahlenvektoren. S eine Zahl.
   fuer alle j <= |Maxy|:
     Neu_Maxy[i] := if S >= D[i] and E[i] <= Maxy[i] then E[i]
						     else Maxy[i]
 */

ac_maxyk_plus_1([],_,[],[],[]):-!.
ac_maxyk_plus_1([Maxyi|Maxy],Sk,[E|Ei],[D|Di],[E|Neu_Maxy]):-
	Sk >= D,
	E < Maxyi,
	ac_maxyk_plus_1(Maxy,Sk,Ei,Di,Neu_Maxy),!.
ac_maxyk_plus_1([Maxyi|Maxy],Sk,[E|Ei],[D|Di],[Maxyi|Neu_Maxy]):-
	ac_maxyk_plus_1(Maxy,Sk,Ei,Di,Neu_Maxy),!.

/* ac_sort_Anzahl(L,Sl)
   --------------------
   L ist eine Liste der Form: sym(Term,Art,Anzahl),
   Sl ist L nach 'Anzahl' sortiert, wobei gleiche Elemente nicht
   geloescht werden.
 */

ac_sort_Anzahl(L,SL):- ac_ist_tree(L,[],Baum), ac_inorder(SL,Baum),!.

ac_ist_tree([],Baum,Baum):-!.
ac_ist_tree([E|R],Baum,Baum2):-
	ac_insert_Anzahl(E,Baum,Baum1),
	ac_ist_tree(R,Baum1,Baum2).

ac_insert_Anzahl(E,[],[E,[],[]]):-!.
ac_insert_Anzahl(sym(T,A,Anz),[sym(T1,A1,Anz1),L,R],[sym(T1,A1,Anz1),L1,R1]):-
	Anz < Anz1 -> (ac_insert_Anzahl(sym(T,A,Anz),L,L1),R1 = R);
		      (ac_insert_Anzahl(sym(T,A,Anz),R,R1),L1 = L).

