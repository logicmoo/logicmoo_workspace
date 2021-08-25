%********************     protein.pl     ********************

% Hauptmodul
% Es enthaelt die Haupteinsprungspunkte :
% - protein/1 : Uebersetzen der Klauseln, Generierung des Prolog-Code und
%               Durchfuehrung des Beweises
% - translate/1 : Uebersetzung der Klauseln, Generierung des Prolog-Code
% - make_protein_exec/0 : Generierung eine Stand-alone-Version des Programms

%************************************************************

% ---------- Compileranweisungen ----------

% Unterdrueckung von "loading library" Meldungen
:- set_error_handler( 139, true/0).

% ----------

:- module_interface( protein).

:- global protein / 1,
	  translate / 1, 
	  make_protein_exec / 0.

:- begin_module( protein).

:- lib( lists).
:- lib( sorts).
:- lib( strings).

:- lib( apply_macros).
:- pragma( expand).

:- ensure_loaded( t_utils).
:- ensure_loaded( t_definitions).
:- ensure_loaded( t_fnc).
:- ensure_loaded( t_literal).
:- ensure_loaded( t_claus).
:- ensure_loaded( t_pred).
:- ensure_loaded( t_add).
:- ensure_loaded( t_translate_1).
:- ensure_loaded( t_translate_2).
:- ensure_loaded( t_translate_3).
:- ensure_loaded( t_translate_4).
:- ensure_loaded( t_translate_5).

:- use_module( t_utils).
:- use_module( t_definitions).
:- use_module( t_fnc).
:- use_module( t_literal).
:- use_module( t_claus).
:- use_module( t_pred).
:- use_module( t_translate_1).
:- use_module( t_translate_2).
:- use_module( t_translate_3).
:- use_module( t_translate_4).
:- use_module( t_translate_5).

:- set_interrupt_handler(14, u_abort/0).


% ---------- Einsprungspunkte ----------

%%% translate( Problem)
%%% Einstiegspunkt: Compilierung ohne Ausfuehrung 

:- mode translate( ++).

translate( InProblem) :-
	block( (l_init( InProblem, _),
	        l_translate,
	        u_tini),
               protein, fail).


%%% protein( Problem)
%%% Compilierung und Ausfuehrung

:- mode protein( ++).

protein( InProblem) :-
	block( (l_init( InProblem, Problem),
	        l_translate,
	        compile( Problem),
	        use_module( protein_proof),
	        proof,
	        l_mmr,
	        u_tini),
	       protein, fail).


% ---------- Initialisierung ----------

%%% l_init( InProblem, OutProblem)
%%% Initialisierung

:- mode l_init( ++, -).

l_init( InProblem, OutProblem) :-
        def_const( titel, Titel),
        printf( "%n%w", [Titel]),
        def_init_flags,
	u_init( InProblem, OutProblem),
	fnc_init, 
	claus_init,
	pred_init, !.


% ---------- Compilierung ----------	

%%% l_translate / l_translate_2to4 / l_translate_2to5 
%%% Haupteinsprungspunkt zum Start der Generierung des Prolog-Codes
%%% Falls die Listen mitgegeben werden, wird die Ein- und Ausgabe nicht ueber
%%% Dateien gesteuert.

l_translate :-
        statistics( runtime, _),
        g_translate_1, 
	protein_flag( timeout, N),
	(N \== 0 -> alarm( N); true),
	g_translate_2,
	g_translate_3,
	g_translate_4,
	g_translate_5,   
	def_const( problem, Problem),
        statistics( runtime, [_,Time]),
	Seconds is Time * 0.001,
	u_out( "%n%w.tme   translated in %w seconds%n", [Problem,Seconds]).

l_translate_2to4 :-
	g_translate_2,
	g_translate_3,
	g_translate_4.
	
l_translate_2to5 :-
        g_translate_2,
	g_translate_3,
	g_translate_4,
	g_translate_5, !.


% ---------- Berechnung minimaler Modelle ----------

%%% l_mmr
%%% Konsistenztest der potential candidates und entsprechende Ausgabe

% nichts zu tun
l_mmr :- protein_result( proved), !.
l_mmr :- \+ protein_flag( mmr, gcwa), !.
l_mmr :- protein_flag( ctest, now), !.
l_mmr :- protein_flag( ctest, store), !.  

l_mmr :- 
	\+ protein_ae( pc, _), !,
	u_out( "%n--- negation by failure to explain ---%n").
	
% Michas Hyper: Vorbereitung und Aufruf 
l_mmr :-
	protein_flag( ctest, end),
	use_module( t_hyper),
	findall( PC, protein_ae( pc, PC), PCL),
	l_tests( [end], PCL).

% PROTEIN: Vorbereitung und Aufruf
l_mmr :-
	protein_flag( ctest, protein),
	findall( PC, protein_ae( pc, PC), PCL),
	def_const( problem, Problem),
	concat_atoms( Problem, '-ct', CTProblem),
	def_set_const( problem, CTProblem),
	protein_flag( check_flags, FlagL),
	protein_flag( trace, Trace),
	protein_flag( search, Search),
	def_init_flags,
	def_set_flag( trace, Trace),
	def_set_flag( search, Search),
	checklist( def_set_flag, FlagL),
        def_set_flag( dynamic, on), 
	l_delete_q_n_r,
        u_out( "%ntranslating %w", [CTProblem]),
        pred_init,
      	l_translate_2to5,
	compile( CTProblem),
	use_module( protein_proof),
	def_set_flag( dynamic, off), 
	l_tests( [protein], PCL).


%%% l_delete_q_n_r
%%% Loescht alle assertierten Query und Restart Klauseln

l_delete_q_n_r :-
	claus_next( Claus), 
	   claus_test( q_or_r, Claus),
	   once( claus_del(Claus)),
        fail.

l_delete_q_n_r :- !.


%%% l_tests( FlagL, PCL)
%%% Ausfuehrung der Tests

:- mode l_tests( ++, +).

l_tests( [CTest|_], [PC|PCR]) :-
	   u_out( "\ntesting potential candidate %w...", [PC]),
           (l_do_test( [CTest], PC) ->
	      l_tests( [CTest|_], PCR)
           ; 
              u_out( " %n--- indefinite ---%n")).

l_tests( _, []) :- u_out( "--- negation by failure to explain ---%n"), !. 


%%% l_do_test( FlagL, PC)
%%% Ausfuehrung eines Tests

:- mode l_do_test( ++, +).

l_do_test( [end], PC) :-
	maplist( lit_neg, PC, NegPC), !,
	(protein_cc_prove( NegPC) ->
	    u_out( " proved%n")
	;
	    u_out( " failed%n"),
	    fail).

l_do_test( [protein], PC) :-
	protein_flag( calculus, Calculus),
	maplist( lit_enclose(list,_), PC, PCL),
	fnc_init,
	claus_init,
	pred_init, 
	maplist( claus_struct( 0, default, default), PCL, ClausL),
	checklist( claus_set( [off]), ClausL),
	l_translate_2to4,
	l_assert_or_retract( assert, [Calculus]),
	proof,
        l_assert_or_retract( retract, [Calculus]), !,
	protein_result( proved).
	

%%% l_assert_or_retract( Flag, FlagL)
%%% assertieren bzw retracten der noetigen Preds

:- mode l_assert_or_retract( ++, ++).

l_assert_or_retract( Flag, FlagL) :-
	pred_next( Pred),
	   l_do_assert_or_retract( Flag, FlagL, Pred),
	fail.

l_assert_or_retract( _, _).


%%% l_do_asssert_or_retract( Flag, FlagL, Pred)

l_do_assert_or_retract( _, _, Pred) :- pred_test( test, Pred), !.

l_do_assert_or_retract( _, [hyper|_], Pred) :- 
	pred_get( type, Pred, Type),
	Type \== th_start_1,
	Type \== th_start,
	Type \== th_fact, !.

l_do_assert_or_retract( Flag, _, Pred) :- 
	pred_get( litL, Pred, [Head]),
	h_assert_or_retract( Flag, Head), !.

l_do_assert_or_retract( Flag, _, Pred) :-
	pred_get( litL, Pred, [Head|BodyL]),
	u_makelist_comma( Body, BodyL),
	h_assert_or_retract( Flag, Head :- Body), !.


% ---------- Executable erstellen ----------

%%% make_protein_exec
%%% Nicht mehr ab Eclipse-Version 3.6.1
%%% Erstellung eines stand-alone-Programm
%%% Durch Aufruf "protein(Filename)" wird "protein(Filename)" ausgefuehrt 

make_protein_exec :-
        nodbgcomp,
	save( protein),
	l_read_arg( Filename),
	protein( Filename),
	halt.


%%% l_read_arg( Filename)

l_read_arg( Filename) :-
	argc( 2), !,
	argv( 1, Filestring),
	atom_string( Filename, Filestring).

l_read_arg( _) :-
	def_const( titel, Titel),
	writeln( stderr, Titel),
	nl( stderr),
	writeln( stderr, "Usage:"),
        writeln( stderr, "protein Filename (extension .tme assumed)"),
        halt.



