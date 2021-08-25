%********************  t_translate_1.pl  ********************

% Erster Teil der Compilation: 
% Lesen des .tme-Files. Als Ergebnis stehen dem System
% - die Klauseln im claus-Praedikat,
% - der PrologCode in File t_header_prolog,
% - die Flags im protein_flag-Praedikat
% zur Verfuegung.

%************************************************************

:- module_interface( t_translate_1).

:- export g_translate_1 / 0.

:- begin_module( t_translate_1).

:- use_module( t_definitions).
:- use_module( t_utils).
:- use_module( t_literal).
:- use_module( t_claus).

:- use_module( t_fnc).

:- global_op( 1110, xfx, '<-').
:- global_op( 1110, xf, '<-').
:- global_op( 1040, xfy, '|').
:- global_op( 1040, xfy, '<->').
:- global_op( 1200, yfx, '#').


% ---------- Haupteinprungspunkt ----------

%%% g_translate_1
%%% Anstoss der Verarbeitung.

g_translate_1 :-
	l_init,
	u_get_stream( tme, S),
	   read( S, Term),
	   once( l_which( Term)), 
	fail.

g_translate_1 :- !.


% ---------- Initialisierung ----------

%%% l_init
%%% Oeffnen des Eingabefiles und Prologfiles und 
%%% Initialisierung des Streammanagements

l_init :-
	def_const( problem, Problem),
	def_const( header, HeaderF),
	concat_atoms( Problem, '.tme', TmeF), 	
	u_init_streams( tme),
	u_init_streams( prolog),
	u_open_file( tme, read, TmeF),
	u_open_file( prolog, write, HeaderF),
	u_get_stream( prolog, ProS),
	u_out( "%n%ntranslating %w.", [Problem]),
	printf( ProS, "%% ---------- Prolog Text ----------%n%n", []), !.


%%% l_which( Term)
%%% - Kontrollangaben verarbeiten
%%% - Prologcode eventuell ausfuehren und ablegen
%%% - Bei Eingabeklauseln Kosten berechnen, normalisieren und ablegen

:- mode l_which( +).

l_which( Term) :- def_stat( Term).

l_which( Term) :-
	def_const( mode, prolog), !,
	(Term = :-(Call) -> call( Call), ! ; true),
	u_writeclause( prolog, Term). 

l_which( InputClaus # (Cost,Plus)) :- !,
	(def_test( costs, (Cost,Plus)) -> true ;
	    def_message( error, costs)),
	def_const( mode, Mode),
	l_normalize( Mode, InputClaus, (Cost,Plus)).

l_which( InputClaus # Cost) :- !,
	(def_test( costs, (Cost,0)) -> true ;
	    def_message( error, costs)),
	def_const( mode, Mode),
	l_normalize( Mode, InputClaus, (Cost,0)).

l_which( InputClaus) :- !, 
	def_const( mode, Mode),
	l_normalize( Mode, InputClaus, default).


%%% l_normalize( Mode, InputClaus, Costs)
%%% Es werden folgende Umformungen vorgenommen:
%%% - Mode == theory:
%%%     P1,...,Pn -> false             zu [[P1,...,Pn],[],[]]
%%%     P1,...,Pn -> false | Guard     zu [[P1,...,Pn],[],[Guard]]
%%%     P1,...,Pn -> C1,...,Cm         zu [[P1,...,Pn],[C1,...,Cm],[]]
%%%     P1,...,Pn -> C1,...,Cm | Guard zu [[P1,...,Pn],[C1,...,Cm],[Guard]]
%%% - Mode == simplify:
%%%     C1,...,Cn -> E = F             zu [[C1,...,Cn],[E],[F]]
%%%     E = F                          zu [[],[E],[F]] 
%%%     C1,...,Cn -> E <-> F1;,...;,Fl zu [[C1,...,Cn],[E],[F1,...,Fl]]
%%%                                und [[C1,...,Cn],[not_E],[not_F1,...,not_Fl]]
%%%     E <-> F1;,...;,Fl              zu [[],[E],[F1,...,Fl]] 
%%%     C1,...,Cn -> E                 zu [[C1,...,Cn],[E],[]]
%%%                                   und [[C1,...,Cn],[not_E],[]]
%%%     E                              zu [[],[E],[]] und [[],[not_E],[]]
%%%                                   und  [[],[not_E],[not_F1,...,not_Fl]]
%%%     C1,...,Cn -> E -> F1;,...;,Fl  zu [[C1,...,Cn],[E],[F1,...,Fl]]
%%%                                und [[C1,...,Cn],[not_E],[not_F1,...,not_Fl]]
%%%     E -> F1;,...;,Fl               zu [[],[E],[F1,...,Fl]] 
%%%                                   und  [[],[not_E],[not_F1,...,not_Fl]]
%%% - Mode == plain:
%%%     H1,...,Hm :- B1,...,Bn         zu [H1,...,Hm,not_B1,...,not_Bn]
%%%     H1;...;Hm :- B1,...,Bn         zu [H1,...,Hm,not_B1,...,not_Bn]
%%%     H1,...,Hm <- B1,...,Bn         zu [H1,...,Hm,not_B1,...,not_Bn]
%%%     H1;...;Hm <- B1,...,Bn         zu [H1,...,Hm,not_B1,...,not_Bn]
%%%     L1,...,Ln                      zu [L1,...,Ln]
%%%     L1;...;Ln                      zu [L1,...,Ln]
%%%     ?- Q1,..., Qn                  zu [not_Q1,...,not_Qn]

:- mode l_normalize( ++, +, ++).

% Theorieinferenzregeln

% Prem -> Conc | Guard
l_normalize( theory, Prem -> ConcGuard, Costs) :-
	def_set_const( theory, yes),
	((ConcGuard = (Conc | Guard)) -> true ; ConcGuard = Conc),
	u_makelist_comma( Prem, PremL),
	((Conc == false ; Conc == fail) ->
	    ConcL = []
	;
	    u_makelist_comma( Conc, ConcL)),
	(Guard = true -> GuardL = []; GuardL = [Guard]),
	claus_struct( default, default, Costs, [PremL,ConcL,GuardL], Claus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus).  

% Simplifizierungsregeln

% [ Cond -> ] E = F
l_normalize( simplify(KZ), InputClaus, Costs) :-
	(InputClaus = (Cond -> E = F) ;
         InputClaus = (E = F)),
	((var(Cond) ; Cond == true) -> 
	    CondL = [],
	    NegCondL = []
        ; 
            u_makelist_comma( Cond, CondL),
	    maplist( lit_neg, CondL, NegCondL)),
	nonvar( E),
	u_makelist( E, [E]),
	u_makelist( F, [F]),
	def_set_const( simplify, yes),
	claus_struct( default, sim(rew,-,KZ), Costs, [NegCondL,[E],[F]], Claus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus).

% [ Cond -> ] Eq1 [ <-> Eq2 ]
l_normalize( simplify(KZ), InputClaus, Costs) :-
	(InputClaus = (Cond -> Eq1 <-> Eq2) ;
	 InputClaus = (Eq1 <-> Eq2) ;
	 InputClaus = (Cond -> Eq1) ;
	 InputClaus = Eq1),
	Eq1 \= (_ -> _), 
	((var(Cond) ; Cond = true) -> 
	    NegCondL = []
        ; 
	    u_makelist_comma( Cond, CondL),
	    maplist( lit_neg, CondL, NegCondL)),
	\+ u_makelist( Eq1, [_,_|_]),
	((var(Eq2) ; Eq2 == true) -> 
	    Eq2L = [], Type = sim(eq,con,KZ), NegType = sim(eq,dis,KZ) ;
        (Eq2 == false -> 
	    Eq2L = [], Type = sim(eq,dis,KZ), NegType = sim(eq,con,KZ) ;
	(u_makelist_semicolon( Eq2, Eq2L) ->
            Type = sim(eq,dis,KZ), NegType = sim(eq,con,KZ) ;
        (u_makelist_comma( Eq2, Eq2L) ->
            Type = sim(eq,con,KZ), NegType = sim(eq,dis,KZ))))),
	def_set_const( simplify, yes),
	lit_neg( Eq1, NegEq1),
	maplist( lit_neg, Eq2L, NegEq2L),
        claus_struct( default, Type, Costs, [NegCondL,[Eq1],Eq2L], Claus),
        claus_struct( old,NegType,Costs, [NegCondL,[NegEq1],NegEq2L], NegClaus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus),
	claus_set( [Reorder], NegClaus).

% [ Cond -> ] Im1 -> Im2
l_normalize( simplify(KZ), InputClaus, Costs) :-
	(InputClaus = (Cond -> Im1 -> Im2) ; 
         InputClaus = (Im1 -> Im2)), 
	((var(Cond) ; Cond = true) -> 
	    NegCondL = []
        ; 
	    u_makelist_comma( Cond, CondL),
	    maplist( lit_neg, CondL, NegCondL)),
	\+ u_makelist( Im1, [_,_|_]),
        (Im2 == true  -> Im2L = [], Type = sim(im,con,KZ) ;
        (Im2 == false -> Im2L = [], Type = sim(im,dis,KZ) ;
	(u_makelist_semicolon( Im2, Im2L) -> Type = sim(im,dis,KZ) ;
        (u_makelist_comma( Im2, Im2L)     -> Type = sim(im,con,KZ))))),
	def_set_const( simplify, yes),
        claus_struct( default, Type, Costs, [NegCondL,[Im1],Im2L], Claus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus).


% andere Klauseln
    
% Head :- Body, Head <- Body
l_normalize( plain, InputClaus, Costs) :-
	(InputClaus = (Head ':-' Body) ; 
         InputClaus = (Head '<-' Body)), 
	u_makelist( Head, HeadL),
	u_makelist_comma( Body, BodyL),
	maplist( lit_neg, BodyL, NegBodyL),
	append( HeadL, NegBodyL, LitL),
        claus_struct( default, default, Costs, LitL, Claus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus).

% Head <- 
l_normalize( plain, (Head '<-'), Costs) :- 
   	u_makelist( Head, LitL),
	claus_struct( default, default, Costs, LitL, Claus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus).

% :- Body 
l_normalize( plain, (':-' Body), Costs) :-
	u_makelist( Body, BodyL),
	maplist( lit_neg, BodyL, LitL),
	claus_struct( default, default, Costs, LitL, Claus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus).

% ?- Query
l_normalize( plain, ('?-' Query), Costs) :-
	u_makelist_comma( Query, QueryL),
	maplist( lit_neg, QueryL, LitL),
	claus_struct( default, query, Costs, [query|LitL], Claus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus).

% Sonst
l_normalize( plain, InputClaus, Costs) :-
	u_makelist( InputClaus, LitL),
	claus_struct( default, default, Costs, LitL, Claus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], Claus).

l_normalize( _, InputClaus, _) :- def_message( error, syntax, InputClaus).
