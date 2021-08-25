%********************  t_translate_5.pl  ********************

% Schreiben des generierten Code

%************************************************************

:- module_interface( t_translate_5).

:- export g_translate_5 / 0.

:- begin_module( t_translate_5).

:- use_module( t_definitions).
:- use_module( t_utils).
:- use_module( t_literal).
:- use_module( t_fnc).
:- use_module( t_claus).
:- use_module( t_pred).
:- [t_header_info].

	
%%% g_translate_5
%%% Schreibt den generierten Code

g_translate_5 :-
	def_const( problem, Problem),
	concat_atoms( Problem, '.pl', PlF),
	u_open_file( out, write, PlF), !,
	l_write_header,
       	l_write_control, 
	l_write_flags,
%	statistics( times, [BTime|_]),
        l_write_preds,
%	statistics( times, [ETime|_]),
%	Time is ETime - BTime,
%	u_out( "%w", [Time]),
	u_close_file( out),
	u_out( ".%n"),
	(protein_flag( trace, off) -> true ; 
            protein_flag( out_stream, S),
	    h_write_trans_stat( S),
	    u_close_file( trc)), !.


%%% l_write_header
%%% Uebertragt die gleichbleibenden Header in den Ausgabestream. Der Name
%%% des Headers wird ist als Konstante abgelegt. Er wird falls noetig um
%%% einen Pfad aus der 'path'-environment-variablen erg"anzt.
%%% Zur Zeit existieren folgende Header :
%%% t_header.pl         - Beweissteuerung
%%% t_header_weight.pl  - optional, iterative term deepening    
%%% t_header_info.pl    - optional, Beweisstatistiken
%%% t_header_tree.pl    - optional, Beweisbaumgenerierung und -ausgabe
%%% t_header_dynamic    - optional, Dynamiche Beweisbaumschreibung
%%% t_header_mmr.pl     - optional, MMR-Berechnungen
%%% t_header_utils.pl   - Utilities fuer die Beweissteuerung
%%% t_header_Problem.pl - Prologcode u.a. aus dem Inputfile 

l_write_header :-
	u_get_stream( out, S),
	def_const( titel, Titel),
	printf( S, "%%%w%n%n", [Titel]),
        u_copy_file( S, '$PROTEINHEADER/t_header.pl'),
	(protein_flag( mmr, off) ->
	    writeclause( S, h_mmr), nl( S)
        ;
	    u_copy_file( S, '$PROTEINHEADER/t_header_mmr.pl')),
	(protein_flag( search, id_term) -> 
	    u_copy_file( S, '$PROTEINHEADER/t_header_weight.pl') ; true),    
	protein_flag( trace, Trace), 
	(Trace == off ->
	    writeclause( S, h_init_stat),
	    writeclause( S, h_write_stat(_)), 
            writeclause( S, h_write_trc), nl( S)
	;
	    u_copy_file( S, '$PROTEINHEADER/t_header_info.pl')),    
        (Trace == internal ->
            u_copy_file( S, '$PROTEINHEADER/t_header_tree.pl')
	;
	    writeclause( S, h_write_tree(_)), nl( S)),
	(Trace = dynamic(_) ->
	    u_copy_file( S, '$PROTEINHEADER/t_header_dynamic.pl')
        ;
            writeclause( S, h_init_dynamic), nl( S)),
        u_copy_file( S, '$PROTEINHEADER/t_header_utils.pl'),
	def_const( header, HeaderF),
	u_copy_file( S, HeaderF),
	nl( S), !.
	    

%%% l_write_control
%%% Schreibt von Flags abhaengige Kontrollpraedikate 

l_write_control :-
	once( u_get_stream( out, S)),
        printf( S, "%n:- set_flag( variable_names, on).%n%n", []),
	printf( S, "%n%% ---------- Control Predicates ----------%n", []),
	l_write_start, 
        l_write_answer,  
	l_write_zargs, 
        l_write_consts, !. 


%%% l_write_start
%%% Steuert Beginn der Beweissuche -> Schreibt entsprechenden Code

l_write_start :-
	once( u_get_stream( out, S)),
        lit_add( query, Query),
	lit_name( ancs,    ([],[]), Query),
	lit_name( b_ancs,  ([],[]), Query),
	lit_name( aunts,   ([],[]), Query),
        lit_name( b_aunts, ([],[]), Query),
	(protein_flag( trace, dynamic(_)) ->
           lit_name( trace, 0, Query)
        ;
           lit_name( trace, Trace, Query)),
	printf( S, "%n%%%%%% h_start_proof( OutDepth, Trace)%n", []),
        (protein_flag( search, prolog) -> 
           printf( S, 
               "%%%%%% Starten des Beweises ohne iterative deepening.%n%n", []),
           u_writeclause( out, (h_start_proof(_, Trace) :- 
                            true, once(Query), true,
                            (protein_flag( mmr, wgcwa) ->
                               retract_all( protein_result( indefinite)); true),
			    (protein_result( _) -> true;
                               u_out( "+++ proved +++%n"),
                               assert( protein_result( proved)))))
        ;				 
           protein_flag( depth_increment, Inc),
	   lit_name( depth, QDepth, Query),
          printf(S,"%%%%%% Starten des Beweises mit iterative deepening,%n",[]),
           printf( S, "%%%%%% dabei Ermitteln der Beweistiefe.%n%n", []), 
           u_writeclause( out, (h_start_proof( OutDepth, Trace) :- 
	                   h_search( Query, QDepth, Inc, Inc, OutDepth)))), !. 
                               

%%% l_write_answer
%%% Steuert Answer-Berechnung -> Schreibt entsprechenden Code

l_write_answer :-
	once( u_get_stream( out, S)),
	printf( S, "%n%n%%%%%% protein_answer( Answer)%n", []),
        printf( S, "%%%%%% Speicherung der Antworten", []),
        printf( S, "durch Verzoegerung dieses Praedikates.%n%n", []),
        (protein_flag( definite_answers, on) ->
	    u_writeclause( out, (protein_answer( Answer) :-
	                     delayed_goals( DL),
	                     \+ member( protein_answer( _), DL),
	                     u_delay( true, protein_answer( Answer)))),
            nl( S),
            u_writeclause( out, (protein_answer( Answer) :-
	                     delayed_goals( DL),
	                     member( protein_answer( Answer), DL)))
        ;
	    u_writeclause( out, protein_answer( Answer) :- 
                            u_delay( true, protein_answer( Answer)))), !.	


%%% l_write_zargs
%%% Schreibt Zusatzargumente, die verwendet werden mit Position.
%%% Nur zur Information, wird nicht benutzt.

l_write_zargs :-
	\+ protein_flag( trace, off),
	once( u_get_stream( out, S)),
	printf(S,"%n%n%% ---------- Additional Arguments ----------%n%n", []),  
	def_get( use, ZArg, y),
	   def_get( pos, ZArg, Pos),
           writeclause( S, p_zarg( ZArg, Pos)),
        fail.

l_write_zargs :- !.
      

%%% l_write_consts
%%% Schreibt benoetigte Konstanten.

l_write_consts :-
	once( u_get_stream( out, S)),
	def_const( titel, Titel),
	def_const( problem, Problem),
	def_const( theory, Theory),
	def_const( simplify, Simplify),
	printf( S, "%n%n%% ---------- Constants ----------%n%n", []),
        writeclause( S, def_const( problem, Problem)),
        writeclause( S, def_const( titel, Titel)),
	writeclause( S, def_const( theory, Theory)),
	writeclause( S, def_const( simplify, Simplify)), !.


%%% l_write_flags
%%% Schreibt die Flags.

l_write_flags :-
	once( u_get_stream( out, S)),
	printf( S, "%n%n%% ---------- Flags ----------%n%n", []),
	protein_flag( Flag, Value),
           writeclause( S, protein_flag( Flag, Value)),
        fail.

l_write_flags :- !.
      

%%% l_write_preds
%%% Schreibt Preds nach Head, Kosten und Prioritaet sortiert

l_write_preds :-
	once( u_get_stream( out, S)),
	printf( S, "%n%n%% ---------- Predicates ----------%n%n", []),       
        protein_flag( sorting, Sorting),
	fnc_next( FA),
	   pred_get( fa, SuchPred, FA),
	   l_do_write_preds( Sorting, SuchPred).

l_write_preds :-
	once( u_get_stream( out, S)),
	printf( S, "%n:- set_flag( variable_names, check_singletons).%n",[]), !.


%%% l_do_write_preds( Sorting, SuchPred)
%%% Schreibt Preds 
%%% - erst Tests nach Kosten und Prioritaet sortiert
%%% - dann Extensionen nach Klauselnummer sortiert
%%% - schliesslich Nachfolgepraedikate nach Kosten und Prioritaet sortiert

:- mode l_do_write_preds( ++, +).

l_do_write_preds( costs, SuchPred) :-
	l_level( SuchPred, [cost,prio], [0,1]).

l_do_write_preds( input, SuchPred) :- 
	pred_get( class, SuchPred, test),
	l_level( SuchPred, [cost,prio], [0,1]).

l_do_write_preds( input, SuchPred) :-
	getval( clausN, N),
	def_set_const( clausN, N),
	pred_get( class, SuchPred, ext),
	l_level( SuchPred, [clausN], [0]).

l_do_write_preds( input, SuchPred) :- 
	pred_get( class, SuchPred, last),
	l_level( SuchPred, [cost,prio], [0,1]).


%%% l_level( SuchPred, LevelL, ValueL)
%%% Schreibt Preds, die Suchpred unifizieren aufsteigend sortiert nach Level

:- mode l_level( +, ++, ++).

l_level( SuchPred, [Level], [N]) :-
	pred_get( Level, SuchPred, N),
	pred_del( SuchPred),
	   l_do_write_pred( SuchPred),
	fail.

l_level( SuchPred, [Level,LevelH|LevelR], [N|NR]) :- 
	pred_get( Level, SuchPred, N),
	l_level( SuchPred, [LevelH|LevelR], NR).

l_level( SuchPred, [Level|LevelR], [N|NR]) :-
        \+ \+ pred_next( SuchPred),
	M is N + 1,
	l_level( SuchPred, [Level|LevelR], [M|NR]).

		 
%%% l_do_write_pred( Pred)
%%% Schreibt ein Pred.

:- mode l_do_write_pred( +).

% Fakten
l_do_write_pred( Pred) :-
	pred_get( litL, Pred, [Head]),
        u_writeclause( out, Head), !.

l_do_write_pred( Pred) :-
	pred_test( simplify, Pred),
	pred_get( litL, Pred, [Head|BodyL]),
	u_makelist_comma( Body, BodyL),
	u_writeclause( out, Head :- -?-> Body), !.

l_do_write_pred( Pred) :-
	pred_get( litL, Pred, [Head|BodyL]),
	u_makelist_comma( Body, BodyL),
	u_writeclause( out, Head :- Body), !.
