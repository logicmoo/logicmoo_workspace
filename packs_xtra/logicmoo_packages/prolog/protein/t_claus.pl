%********************     t_claus.pl     ********************

% Praedikate zur Klauselverwaltung.

% Klauselstruktur: (ClausN, Type, Costs, LitL)
% Eine Eingabeklausel wird in die Claus-Struktur transformiert, wobei die
% LitL der Klauselnormalform der Klausel entspricht.
% ClausNr : Laufende Nummer der Klausel im Input-File
% Type    : Typ der Klausel bzw des Preds
% Costs   : Kosten der Klausel bzw des Preds. Entweder ein Zahlentupel oder die
%           Konstante default. 

%************************************************************

:- module_interface( t_claus).

:- export claus_del / 1,
	  claus_exists_query / 0,
	  claus_get / 3, 
	  claus_next / 1,
	  claus_init / 0,
	  claus_set / 2,
	  claus_struct / 5,
	  claus_test / 2.
        
:- begin_module( t_claus).

:- use_module( t_definitions).
:- use_module( t_utils).
:- use_module( t_literal).
:- use_module( t_fnc).

:- dynamic claus / 4,
           stat_sim / 2,
	   subsumed / 0.
	

%%% claus_struct( ClausN, Type, Costs, LitL, Claus)
%%% Definition der Klauselstruktur

claus_struct( ClausN, Type, Costs, LitL, (ClausN,Type,Costs,LitL)).


%%% claus_get( Flag, Claus, Value)
%%% Liefert ein Argument der Claus-Struktur

:- mode claus_get( ++, +, -).

claus_get( clausN, Claus, V) :- claus_struct( V, _, _, _, Claus).
claus_get( costs,  Claus, V) :- claus_struct( _, _, V, _, Claus).
claus_get( litL,   Claus, V) :- claus_struct( _, _, _, V, Claus).
claus_get( type,   Claus, V) :- claus_struct( _, V, _, _, Claus).

claus_get( group, Claus, V) :-
	claus_get( type, Claus, Type),
	def_get( group, Type, V).

claus_get( class, Claus, V) :-
	claus_get( type, Claus, Type),
	def_get( class, Type, V).


%%% claus_test( Flag, Clause)
%%% Test auf bestimmte Typklassen je nach Flag

:- mode claus_test( ++, +).

claus_test( nocontrapos, Claus) :- claus_test( q_or_r, Claus), !.
claus_test( nocontrapos, Claus) :- claus_test( theory, Claus), !.
claus_test( nocontrapos, Claus) :- claus_test( simplify, Claus), !.
claus_test( q_or_r,      Claus) :- claus_get( type,  Claus, query), !.
claus_test( q_or_r,      Claus) :- claus_get( type,  Claus, restart), !.
claus_test( theory,      Claus) :- claus_get( group, Claus, th_start), !.
claus_test( test,        Claus) :- claus_get( class, Claus, test), !.
claus_test( Type,        Claus) :- claus_get( type, Claus, Type), !.
claus_test( Group,       Claus) :- claus_get( group, Claus, Group), !.

claus_test( negative,    Claus) :-
	claus_get( litL, Claus, LitL),
	checklist( lit_test_neg, LitL), !.


%%% claus_exists_query
%%% Testet, ob in der aktuellen Partition eine Query enthalten ist.

claus_exists_query :- claus( _, query, _, _).


%%% claus_init
%%% Loeschen aller claus-Praedikate
 
claus_init :-
	retract_all( claus(_,_,_,_)),
	setval( clausN, 0),
	setval( reorder, 0),
	setval( simplify, 0),
	setval( taut, 0),
	retract_all( stat_sim( _, _)).


%%% claus_next( Claus)
%%% Liefert naechste Klausel

claus_next( Claus) :- 
	claus_struct( ClausN, Type, Costs, LitL, Claus),
	claus( ClausN, Type, Costs, LitL).


%%% claus_del( Claus)
%%% Retractet Klausel

claus_del( Claus) :-
	claus_struct( ClausN, Type, Costs, LitL, Claus),
	retract( claus( ClausN, Type, Costs, LitL)), !.


%%% claus_set( FlagL, Claus)
%%% - Normalisierung der Literale
%%% - evtl Ermittlung und Ueberpruefung von Klauselnummer und Typ
%%% - Ueberpruefung der Kostenangabe
%%% - Assertierung der Klausel als claus-Praedikat

:- mode claus_set( ++, +).

claus_set( [Reorder|_], InClaus) :-
	claus_struct( InClausN, InType, Costs, InLitL, InClaus),
	(InLitL = [InLit1L,InLit2L,InLit3L], 
	 length( InLit1L, _), length( InLit2L, _), length( InLit3L, _) ->
	    lit_norm( con, [Reorder], InLit1L, OutLit1L),
	    (InType = sim(rew,_,_) -> 
	       OutLit2L = InLit2L,
	       OutLit3L = InLit3L
            ;
	       lit_norm( dis, [Reorder], InLit2L, OutLit2L),
	       (InType = sim(_,dis,_) ->
	          lit_norm( dis, [Reorder], InLit3L, OutLit3L)
	       ;
	          lit_norm( con, [Reorder], InLit3L, OutLit3L))),
	    OutLitL = [OutLit1L,OutLit2L,OutLit3L]   
	; 
	    lit_norm( dis, [Reorder], InLitL, OutLitL)),
        (OutLitL == [true] -> true ;
	   (InClausN == default -> 
               incval( clausN),
	       getval( clausN, OutClausN) ;	   
           (InClausN == old ->
	       getval( clausN, OutClausN) 
           ; 
               InClausN = OutClausN)),
	   (InType == default -> 
	       lit_get_type( OutLitL, OutType) 
           ; 
	       InType = OutType),
	   (integer( OutClausN) -> true; 
	       def_message( error, clausN, OutClausN)),
	   (def_test( type, OutType) -> true; 
	       def_message( error, type, OutType)),
	   ((Costs = (Costs1,Costs2), integer(Costs1), integer(Costs2)) ;
            (Costs == default) -> true ; 
               def_message( error, costs, Costs)),
	   claus_struct( OutClausN, OutType, Costs, OutLitL, OutClaus), 
	   retract_all( subsumed),
	   l_subsumption_test( OutClaus),
	   (subsumed -> true ; 
	      assert( claus( OutClausN, OutType, Costs, OutLitL)))), !.


% ---------- Peters subsumption check ----------

%%% l_subsumption_test( Claus)
%%% Subsumption Test auf der claus-Struktur
%%% Testet, ob Claus andere Klausel subsumiert oder von anderer Klausel
%%% subsumiert wird. Jeweils mit gleicher Klauselnummer.

:- mode l_subsumption_test( +).

l_subsumption_test( Claus) :- claus_test( simplify, Claus).
l_subsumption_test( Claus) :- claus_test( theory, Claus).
l_subsumption_test( Claus) :- claus_test( restart, Claus).

l_subsumption_test( Claus) :-
	claus_get( litL, Claus, LitL),
	claus_get( clausN, Claus, ClausN),
	claus_get( type, Claus, Type),
	claus_get( clausN, ZwiClaus, ClausN),
	(Type == query -> claus_get( type, ZwiClaus, Type) ; true),
	claus_next( ZwiClaus),
	   (Type == query -> true ; \+ claus_test( nocontrapos, ZwiClaus)), 
	   claus_get( litL, ZwiClaus, ZwiLitL),
	   (l_subsume( LitL, ZwiLitL) -> claus_del( ZwiClaus) ;
           (l_subsume( ZwiLitL, LitL) -> assert( subsumed) ; true)),
        fail.

l_subsumption_test( _).


%%% l_subsume( C1, C2)
%%% Does litlist (clause) C1 subsume litlist (clause) C2?
%%% e.g. [p(X)] subsumes [p(f(X)),q(b)]
%%% clauses may share variables - they are renamed before.
%%% l_subsume does no instantiation

:- mode l_subsume( +, +).

l_subsume( C1, C2) :-
	length( C1, N1),
	length( C2, N2),
	N1 > N2, !, fail.

l_subsume( C1, C2) :-
	\+ \+ (
	copy_term( C2, C2C),
	term_variables( C2C, C2CVs),
	l_skolemize( C2CVs, 1),
	l_do_subsume( C1, C2C), !
	).


%%% l_skolemize( VarL, N)
%%% Instanziierung der Variablen

:- mode l_skolemize( +, ++).

l_skolemize( [], _).

l_skolemize( [sk(N)|VarR], N) :-
	N1 is N + 1,
	l_skolemize( VarR, N1).


%%% l_do_subsume( C1, C2)
%%% like l_subsume, but assumes that C2 is ground already

:- mode l_do_subsume( +, ++).

l_do_subsume( [], _).

% optional case:
l_do_subsume( [C1H|C1R], C2) :-
	\+ checklist( \==(C1H), C2), !, 
	l_do_subsume( C1R, C2).

l_do_subsume( [C1H|C1R], C2) :-
	member( C1H, C2), 
	l_do_subsume( C1R, C2).
	