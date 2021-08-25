%********************     t_header.pl    ********************

% Praedikate zur Durchfuehrung bzw Start des eigentlichen Beweises.

%************************************************************

:- module_interface( protein_proof).

:- global proof / 0,
	  protein_result / 1.

:- export protein_ae / 2.

:- begin_module( protein_proof).

% Unterdrueckung von "indefinite delay" Meldungen
:- set_error_handler( 272, true/0).

:- dynamic protein_result / 1,
	   protein_ae / 2.

protein_mode( proof).


% ---------- Haupteinsprungpunkt ----------

%%% proof
%%% Haupteinsprungpunkt zum Start des eigentlichen Beweises

proof :-
	h_init,
	h_proof( Trace),
	h_output( Trace), !.


%%% h_init
%%% Initialisierungen, Ausgabe Hinweise an den Benutzer

h_init :- 
	def_const( problem, Problem),
	(protein_flag( calculus, me) ; protein_flag( delayed_rme, off) -> true;
            setval( block, 0)),
	protein_flag( mmr, MMR),
	protein_flag( ec_pruning, ECP), 
	((MMR == gcwa ; MMR == model ; ECP == on) -> 
	    setval( restart, 0),
	    ((protein_flag(ctest,protein) ; MMR == model ; ECP == on) -> true;
		compile( "$HYPERHOME/hyper", t_hyper),
		use_module( t_hyper),
		protein_cc_init( Problem))
        ; true),
	(protein_flag( search, id_inf) -> setval( id_inf, 0); true),
	h_init_stat,
	h_init_dynamic,
	u_out( "%nproving %w...%n", [Problem]).

 
%%% h_proof( Trace)
%%% Start des Beweises mit Statistiken

h_proof( Trace) :-
	protein_flag( search, Search),
	(Search == id_tree -> u_out( "tree depth ") ;
        (Search == id_inf  -> u_out( "inference maximum ") ;
	(Search == id_term -> u_out( "term depth "); true))),
        statistics( runtime, _),
	h_retract,
        (h_start_proof( DepthOut, Trace) ; true),
        statistics( runtime, [_,Time]),
	Seconds is Time * 0.001,
	setval( proof_time, Seconds),
	(protein_flag( search, prolog) -> true; setval( proof_depth, DepthOut)).


%%% h_retract
%%% Loeschen aller dynamischen EIntraege fuer die neue Runde

h_retract :-
 	retract_all( protein_result(_)),
	retract_all( protein_ae(_,_)).


%%% h_output( Trace)
%%% Ausgabe der Statistik, Antworten, Trace

:- mode h_output( +).

h_output( Trace) :-    
	protein_result( proved), !,
	protein_flag( out_stream, S),
	h_write_stat( S), 
        h_write_answers( S),
        h_write_more_answers,
	h_write_tree( Trace),
        h_write_trc.

h_output( _) :-
	protein_result( indefinite), !,
	protein_flag( out_stream, S),
	h_write_stat( S), 
	u_out( "--- indefinite ---%n").

h_output( _) :-
	protein_flag( mmr, MMR),
	(MMR == gcwa ; MMR == model),
	protein_flag( ctest, CTest),
	(CTest == end ; CTest == protein ; MMR == model), !,
	protein_flag( out_stream, S),
	h_write_stat( S),
	h_mmr.

h_output( _) :-
	protein_flag( mmr, gcwa), !,
	protein_flag( out_stream, S),
	h_write_stat( S),
        u_out( "--- negation by failure to explain ---%n"). 
       
h_output( _) :-
	protein_flag( out_stream, S),
	h_write_stat( S),
        u_out( "--- failure ---%n"). 
       
        
% ---------- iterative deepening ----------

%%% h_search( Goal, Tiefe in Query, Eingabetiefe, Inkrement, Ausgabetiefe) 
%%% Suchen des Beweisen mit iterative deepening
%%% Die globale Variable more_depth wird benutzt, um zu ermitteln, ob eine
%%% Erhoehung der Tiefe noch sinnvoll ist. 

:- mode h_search( +, ?, +, ++, -).

% Starten des Beweises mit Tiefe Depth
h_search( Goal, Depth, Depth, _, Depth) :-
	h_print_depth( Depth),
        Goal,  
	(protein_result( _) -> true ;
           u_out( "+++ proved +++%n"),
           assert( protein_result( proved))).

% Erhoehen der Tiefe um Inc und Restart des Beweises
h_search( Goal, QDepth, InDepth, Inc, OutDepth) :-
        getval( more_depth, More_depth),
        (More_depth == 0 ->
           InDepth = OutDepth,
	   u_out( "--- proof not possible ---%n"), !
	;
           ZwiDepth is InDepth + Inc,
	   h_retract,
	   h_search( Goal, QDepth, ZwiDepth, Inc, OutDepth)).


%%% h_print_depth( Depth)
%%% Schreibt aktuelle Tiefe und initialisiert neues Level

:- mode h_print_depth( ++).

h_print_depth( Depth) :-
	u_out( "%w,", [Depth]),
	setval( more_depth, 0).

  
%---------- Antworten ----------

%%% h_write_answers( S)
%%% Ausgabe gefundene Antworten

h_write_answers( S) :-
	delayed_goals( DL),
        (protein_flag( answer_set_handling, on) ->
	   setof( Answer, member( protein_answer( Answer), DL), AnswerL)
        ;
	   bagof( Answer, member( protein_answer( Answer), DL), AnswerL)),
	printf( S, "%nAnswers%n-------%n", []),
	h_do_write_answers( S, AnswerL), !.
       
h_write_answers( _) :- !.
        

%%% h_do_write_answers( S, AnswerL)
%%% Schreibt gefundene Antwort

:- mode h_do_write_answers( ++, +).

h_do_write_answers( S, [Answer]) :- writeln( S, Answer), !.

h_do_write_answers( S, [Answer|AnswerR]) :-
	write( S, Answer), 
        write( S, ' ; '),
	h_do_write_answers( S, AnswerR).


%%% h_write_more_answers
%%% stoesst gegebenenfalls das Suchen nach weiteren Beweisen an.

h_write_more_answers :-
        protein_flag( answers, more),
	protein_flag( search, Search),
	u_out( "%n   More? (;)"),
	u_on_backtrack( h_retract), 
	(Search == id_tree -> u_on_backtrack( u_out("tree_depth "));
        (Search == id_inf  -> u_on_backtrack( u_out("inference maximum "));
	(Search == id_term -> u_on_backtrack( u_out("term depth ")); true))),
        u_on_backtrack( u_out( ";%n")),
        \+ tyi( 59), !,
        u_out( "%n").  

h_write_more_answers :-
        protein_flag( answers, all),
	protein_flag( search, Search),
	u_out( "%n"),	
	u_on_backtrack( h_retract), 
	(Search == id_tree -> u_on_backtrack( u_out("tree_depth "));
        (Search == id_inf  -> u_on_backtrack( u_out("inference maximum "));
	(Search == id_term -> u_on_backtrack( u_out("term depth ")); true))),
        u_on_backtrack( u_out( "%n")), 
        fail, !.

h_write_more_answers :- protein_flag( answers, one), !.

% END t_header.pl END

