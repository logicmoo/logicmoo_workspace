% ********************  t_translate_2.pl  ********************

% Zweiter Teil der Compilation:
% - verschiedene Klauseltransformationen (Hyper)
% - statisches Simplifizieren
% - Generierung der Query- und Restart-Klauseln
% - Erstellen der Klauseln zum Anstoss des Restarts 
% - Dokumentieren der Klauseln und Flags im Tracefile

% ************************************************************

:- module_interface( t_translate_2).

:- export g_translate_2 / 0.

:- begin_module( t_translate_2).

:- use_module( t_definitions).
:- use_module( t_utils).
:- use_module( t_fnc).
:- use_module( t_literal).
:- use_module( t_claus).

:- lib( numbervars).

:- dynamic sim_keep_clause / 0.


% ---------- Haupteinprungspunkt ----------

%%% g_translate_2
%%% Anstoss der Verarbeitung

g_translate_2 :-
	l_init,
	l_hyper,
	def_test_afterwards, 
        statistics( runtime, _),
	l_simplify,
        statistics( runtime, [_,Time]),
	Seconds is Time * 0.001,
	setval( sim_time, Seconds), 
	l_query,
	l_restart,
	l_write_trc,
	u_out( "."), !.


%%% l_init
%%% Oeffnen des Tracefiles und Initialisierung des Streammanagements

l_init :- protein_flag( trace, off), !.

l_init :-
	def_const( titel, Titel),
	def_const( problem, Problem),
        concat_atoms( Problem, '.trc', TrcF), 	
	u_init_streams( trc),
	u_open_file( trc, write, TrcF),
	u_get_stream( trc, TrcS),
        printf( TrcS, "%w%n", [Titel]), !.
 

% ---------- Hyper-Transformation ----------

%%% l_hyper
%%% Transformation der Klauseln fuer den Hyperlink

l_hyper :- \+ protein_flag( calculus, hyper).

l_hyper :-
	def_set_const( theory, yes),
	claus_next( Claus),
	   claus_test( ext, Claus),
	   once( l_do_hyper( Claus)),
	fail.   

l_hyper :-
	claus_struct( default, query, default, [query,not_any], Query),
	claus_set( [off], Query),
	def_set_flag( calculus, rme), !.


%%% l_do_hyper( Claus)
%%% Transformation einer Klausel fuer den Hyperlink

l_do_hyper( Claus) :-
	claus_test( query, Claus), !,
	claus_del( Claus),
	claus_struct( ClausN, _, Costs, InLitL, Claus),
	subtract( InLitL, [query], OutLitL),
	claus_struct( ClausN, default, Costs, OutLitL, NegClaus),
	claus_set( [off], NegClaus).

l_do_hyper( Claus) :- !,
	claus_del( Claus),
	claus_struct( ClausN, _, Costs, InLitL, Claus),
	u_splitlist( def_test_prolog_lit, InLitL, ProL, ZwiL),
        u_splitlist( lit_test_neg, ZwiL, NegLitL, ConcL),
	maplist( lit_neg, NegLitL, ZwiPremL),
	append( [not_any|ProL], ZwiPremL, PremL),
	(Costs == default ->
	    length( ConcL, ThCosts),
	    OutCosts = (ThCosts,0)
	;
	    OutCosts = Costs),
	claus_struct( ClausN, default, OutCosts, [PremL,ConcL,[]], OutClaus),
	protein_flag( reorder, Reorder),
	claus_set( [Reorder], OutClaus).


% ---------- Verarbeitung der Simplifizierungsregeln ----------

%%% l_simplify
%%% Wendet die Simplifizierungsregeln auf die Eingabeklauseln an.
%%% +++ Was mit theory Inferenzregeln?

l_simplify :-
	once( def_const( simplify, yes)),
	once( protein_flag( sim_static, SimStatic)),
	once( protein_flag( sim_deletion, SimDeletion)),
	once( protein_flag( sim_focus, SimFocus)),
	once( protein_flag( reorder, Reorder)),
        SimStatic \== off,
	l_init_sim( SimL),
        claus_next( Claus),
	   (SimFocus == query -> claus_test( query, Claus) ; true),
	   \+ claus_test( simplify, Claus),
	   \+ claus_test( theory, Claus),
	   l_simplify_claus( [SimStatic,SimDeletion,SimFocus], [Reorder], 
                             SimL, Claus), 
	fail.

l_simplify :- !.


%%% l_init_sim( SimL)
%%% Initialisierung der Simplifizierung

:- mode l_init_sim( -).

l_init_sim( SimL) :-
	protein_flag( sim_static, SimStatic),
	(u_get_stream( trc, TrcS) ->
           printf( TrcS,
   "%n---------------- static simplification steps ---------------%n%n", [])
           ; true),
	(SimStatic == uncond -> CondL = [] ; true),
	findall( SimRule, (
	   claus_get( litL, SimRule, [CondL,_,_]),
	   claus_next( SimRule),
	   claus_test( simplify, SimRule)), 
           SimL), !.


%%% l_simplify_claus( SimFlagL, FlagL, SimL, InClaus)
%%% Simplifizierung der Klausel, Speicherung der simplifizierten Klauseln,
%%% evtl Loeschung der Orginalklausel

:- mode l_simplify_claus( ++, ++, +, +).

l_simplify_claus( [_,SimDeletion|_], FlagL, SimL, InClaus) :-
	retract_all( sim_keep_clause),
	l_do_simplify( SimL, SimL, [InClaus], OutClausL),
        (variant( [InClaus], OutClausL) -> true ;
	   (u_get_stream( trc, TrcS) -> nl( TrcS) ; true),
           incval( simplify),
	   checklist( claus_set( FlagL), OutClausL),
	   (((SimDeletion == complete, sim_keep_clause) ; 
             (SimDeletion == keep)) -> true ; 
              once( claus_del( InClaus)))), !.


%%% l_do_simplify( ComSimL, SimL, InClausL, OutClausL)
%%% Anwendung der Simplifikationsregeln

:- mode l_do_simplify( +, +, +, -).

l_do_simplify( _, [], ClausL, ClausL) :- !.
l_do_simplify( _, _, [], []) :- !.

l_do_simplify( ComSimL, SimL, [InClaus|InClausR], OutClausL) :-
	l_do_simplify( ComSimL, SimL, InClaus,  ZwiClausL),
	l_do_simplify( ComSimL, SimL, InClausR, OutClausR),
	append( ZwiClausL, OutClausR, OutClausL), !.

l_do_simplify( ComSimL, [SimRule|SimR], InClaus, OutClausL) :-
	findall( ZwiClaus, l_rewrite_claus( SimRule, InClaus, ZwiClaus),
                 ZwiClausL), !,
	l_do_simplify( ComSimL, SimR, [InClaus], Out1ClausL),
	(variant( [InClaus], ZwiClausL) -> OutClausL = Out1ClausL ;
	   l_trace_sim( SimRule, InClaus),
	   l_do_simplify( ComSimL, ComSimL, ZwiClausL, Out2ClausL),
	   (variant( [InClaus], Out1ClausL) -> OutClausL = Out2ClausL ;
           (variant( Out1ClausL, Out2ClausL) -> OutClausL = Out2ClausL ;
               append( Out1ClausL, Out2ClausL, OutClausL)))), !. 
	

%%% l_rewrite_claus( SimRule, InClaus, OutClaus)
%%% Anwendung der Simplifizierungsregel auf eine Klausel

:- mode l_rewrite_claus( +, +, -).

l_rewrite_claus( SimRule, InClaus, OutClaus) :-
	claus_get( litL, SimRule, [CondL,[SimLit],RewriteL]),
	claus_get( type, SimRule, Type),
	(Type = sim(rew,_,_) -> KZ = rew ; Type = sim(_,KZ,_)),
	claus_get( litL, InClaus, InLitL), !,
	u_rewrite_list( KZ, SimLit, RewriteL, CondL, InLitL, ZwiLitL, ZwiCondL),
	l_gen_simplified_claus( ZwiLitL, ZwiCondL, InClaus, OutClaus).


%%% l_gen_simplified_claus( LitL, CondL, InClaus, OutClaus)
%%% Generiert aus simplifizierter Literalliste und Bedingungsliste die
%%% simplifizierte Klausel.

:- mode l_gen_simplified_claus( +, +, +, -).

l_gen_simplified_claus( LitL, CondL, Claus, Claus) :-
	flatten( CondL, []),
	claus_get( litL, Claus, InLitL),
	u_flatten( LitL, ZwiLitL),
        variant( ZwiLitL, InLitL), !.

l_gen_simplified_claus( LitL, CondL, InClaus, OutClaus) :-
	append( LitL, CondL, ZwiLitL),
	u_flatten( ZwiLitL, ZwoLitL),
	protein_flag( reorder, Reorder),
	lit_norm( dis, [Reorder], ZwoLitL, OutLitL),
	claus_struct( ClausN,       _, Costs,       _,  InClaus),
	claus_struct( ClausN, default, Costs, OutLitL, OutClaus), !.


%%% l_trace_sim( SimRule, Claus)
%%% Trace der Simplifizierung

:- mode l_trace_sim( +, +).

l_trace_sim( SimRule, Claus) :-
	(u_get_stream( trc, TrcS) ->
	    claus_get( clausN, SimRule, SimN),
	    claus_get( clausN, Claus, ClausN),
	    printf( TrcS, "%w-%w ",[ClausN,SimN]) ; true),
	(sim_keep_clause -> true ;
	(claus_get( type, SimRule, sim(Ty,_,_)),
	 Ty \== im,
         claus_get( litL, SimRule, [[],_,_]) -> true ;
            assert( sim_keep_clause))), !.


% ---------- Verarbeitung der Query ----------

%%% l_query
%%% Stellt je nach Kalkuel Querys und Restarts bereit.
%%% Queries: Falls mindestens eine Query mit ?- angegeben wurde, werden nur 
%%%          diese verwendet. Wurde keine angegeben, werden alle rein negative
%%%          Klauseln zu Queries.

l_query :-
	l_get_query_flags( FlagL),
	claus_next( Claus),
	   \+ claus_test( theory, Claus),
	   \+ claus_test( simplify, Claus),
	   (claus_get( litL, Claus, [query|_]) -> 
	       once( l_do_query( FlagL, Claus)), fail; true),
	   (claus_test( negative, Claus) -> 
	       once( l_do_negative( FlagL, Claus)), fail; true),
	fail.

l_query :- protein_flag( dynamic, on), !.
l_query :- claus_exists_query, !.
l_query :- def_message( abort, no_query).


%%% l_get_query_flags( FlagL)
%%% Ermittelt Flags fuer folgende Auswertungen

:- mode l_get_query_flags( -).

l_get_query_flags( [Typ,Calculus,QueryReuse]) :-
	(claus_exists_query -> Typ = q; Typ = gnf),
	protein_flag( calculus, Calculus),
	protein_flag( query_reuse, QueryReuse), !.

	
% Entscheidungstabelle
%
%  Typ | QReuse | Cal | q-> | n->
% ---------------------------------------
%   q  |    n   |  me |  q  |  n
%   q  |    n   | -me |  q  |  r
%   q  |    y   |  me | qn  |  n
%   q  |    y   | -me | qr  |  r 
%  gnf |    n   |  me |  -  |  q
%  gnf |    n   | -me |  -  |  q
%  gnf |    y   |  me |  -  | qn 
%  gnf |    y   | -me |  -  | qr


%%% l_do_query( FlagL, Query)
%%% Verarbeitung einer Query

:- mode l_do_query( ++, +).	

% q->qn
l_do_query( [q,me,on|_], Query) :-
	claus_struct( ClausN,   query, Costs, [query|LitL], Query),     
	claus_struct( ClausN, default, Costs,         LitL, NegClaus),
	claus_set( [off], NegClaus).

% q->qr
l_do_query( [q,_,on|_], Query) :- 
	claus_struct( ClausN, Type, Costs,   [query|LitL], Query), 
	claus_struct( ClausN, Type, Costs, [restart|LitL], Restart),
	claus_set( [off], Restart).


%%% l_do_negative( FlagL, NegClaus)
%%% Verarbeitung einer rein negativen Klausel

:- mode l_do_negative( ++, +).

% n->r
l_do_negative( [q,rme|_], NegClaus) :- 
	claus_struct( ClausN,     _, Costs,          LitL, NegClaus),
        claus_struct( ClausN, query, Costs, [restart|LitL], Query),
	claus_del( NegClaus),
	claus_set( [off], Query).

% n->q
l_do_negative( [gnf,_,off|_], NegClaus) :- 
	claus_struct( ClausN,     _, Costs,         LitL, NegClaus),
	claus_struct( ClausN, query, Costs, [query|LitL], Query),
	claus_del( NegClaus),
	claus_set( [off], Query).

% n->qn
l_do_negative( [gnf,me|_], NegClaus) :- 
	claus_struct( ClausN,     _, Costs,         LitL, NegClaus),
        claus_struct( ClausN, query, Costs, [query|LitL], Query),
	claus_set( [off], Query).

% n->qr
l_do_negative( [gnf,rme|_], NegClaus) :-
	claus_struct( ClausN,     _, Costs,           LitL, NegClaus),
	claus_struct( ClausN, query, Costs,   [query|LitL], Query),
        claus_struct( ClausN, query, Costs, [restart|LitL], Restart),
	claus_del( NegClaus),
	claus_set( [off], Query),
	claus_set( [off], Restart).


% ---------- Restart-Klauseln ----------

%%% l_restart
%%% RME: Anstoss der Generierung der Restart-Clauses, mit denen ein Restart
%%% ausgeloest werden kann.

l_restart :-
	protein_flag( calculus, rme), 
	fnc_next( Fnc),
	   fnc_struct( F, A, Fnc),
	   lit_test_neg_fnc( F),
	   \+ def_test_special_lit( F, A),
	   functor( Lit, F, A),
	   claus_struct( 0, restart, default, [Lit,not_restart], Restart),
	   claus_set( [off], Restart),
	fail.

l_restart :- !.


%%% l_write_trc
%%% Schreibt Klauseln und Flags in den Tracefile

l_write_trc :- protein_flag( trace, off), !.

l_write_trc :- 
	u_get_stream( trc, TrcS),
	printf( TrcS,
      "%n--------------------    input clauses   --------------------%n%n", []),
        (bagof( _, (claus_next( Claus), 
                   claus_get( clausN, Claus, ClausN),
	           ClausN > 0,	
                   numbervars( Claus, 0, _),
                   l_write_claus( TrcS, Claus)), _); true),
        printf( TrcS, 
      "%n--------------------------- flags --------------------------%n%n", []),
        (bagof( _, (protein_flag( Flag, Value),
	           \+ def_flag( Flag, Value, _,   _, _),
		   \+ def_flag( Flag,     _, _, int, _),
	           printf( TrcS, "%w - %w%n", [Flag,Value])), _); true).
                 

%%% l_write_claus( S, Bez, Claus)
%%% Schreibt einzelne Klausel in die Datei

:- mode l_write_claus( ++, ++, +).

l_write_claus( S, Claus) :-
	claus_get( clausN, Claus, ClausN),
	claus_get( costs, Claus, Costs),
	claus_get( litL, Claus, LitL),
	claus_get( group, Claus, Group),
	claus_get( type, Claus, Type),
	l_trans_litL( Type, LitL, OutLitL),
	l_get_bez( Group, LitL, Bez),
	(Costs == default ->
	    printf( S, "%Dw No.%4d : %QDw%n", [Bez,ClausN,OutLitL])
        ;
            printf( S, "%Dw No.%4d : %QDw   costs: %QDw%n", 
	            [Bez,ClausN,OutLitL,Costs])), !.


%%% l_trans_litL( Type, InLitL, OutLitL)
%%% Literallistentransformation

:- mode l_trans_litL( ++, +, -).

l_trans_litL( sim(_,con,_), [L1,L2,[]], [L1,L2,true]) :- !.
l_trans_litL( sim(_,dis,_), [L1,L2,[]], [L1,L2,false]) :- !.
l_trans_litL( sim(_,con,_), [L1,L2,L3], [L1,L2,[L3]]) :- !.
l_trans_litL( _, LitL, LitL).


%%% l_get_bez( Group, LitL, Bez)
%%% Ermittlung der Klauselbezeichnung

:- mode l_get_bez( ++, +, -).

l_get_bez( th_start, _,      'Th-Rule ').
l_get_bez( simplify, _,      'Sim-Rule').
l_get_bez( ext, [query|_],   'Query   ').
l_get_bez( ext, [restart|_], 'Restart ').
l_get_bez( ext, _,           'Clause  ').






