%********************  t_header_info.pl  ********************

% Praedikate fuer Beweisstatistiken

%************************************************************

%%% h_write_trans_stat( S)
%%% Ausgabe der Uebersetzungsstatistik

:- mode h_write_trans_stat( ++).

h_write_trans_stat( S) :-
	nl( S),
	(protein_flag( reorder, off) -> true;
           getval( reorder, Reorder),
	   printf( S, "   Static Reorderings:    %5d%n", [Reorder])),
        (def_const( simplify, yes) -> 
	   getval( simplify, Simplify),
	   getval( sim_time, SimTime),
	   printf( S, "   Static Simplifications:%5d%n", [Simplify]),
           FSimTime is SimTime * 1.0,
           printf( S, "   Simplification Time:     %5.1f sec%n", [FSimTime])
        ; true),
	getval( taut, Taut),
        (Taut > 0 ->
           printf( S, "   Tautology Deletions:   %5d%n", [Taut]); true), !.


%%% p_count( GlobaleVariablenname)
%%% Bei jedem Schritt des Beweises wird entsprechend des Typs des Schritts
%%% eine globale Variable erhoeht. (Backtrackingfaehig)

:- mode p_count( ++).
	
p_count( Type) :- incval( Type), u_on_backtrack( decval( Type)). 


%%% h_init_stat
%%% Initialisierung der globalen Variablen fuer die Statistik 

h_init_stat :-
	setval( ext, 0),
	setval( red, 0),
	setval( red_cut, 0),
	setval( restart, 0),
	setval( factor, 0),
	setval( factor_cut, 0),
	setval( theory, 0),
        setval( inf, 0), 
	setval( proof_depth, 0),
	setval( proof_time, 0.0),
	setval( sim, 0),
	setval( sim_fail, 0),
	setval( sim_time, 0).


%%% h_write_stat( S)
%%% Schreibt Statistiken

:- mode h_write_stat( ++).

h_write_stat( S)  :-
	getval( ext, Ext),
	getval( red, Red_Nocut),
	getval( red_cut, Red_Cut),
	getval( restart, Restart),
	getval( factor, Factor_Nocut),
	getval( factor_cut, Factor_Cut),
	getval( sim, Sim),
	getval( sim_fail, SimFail),
	getval( theory, Theory),
	getval( inf, Inf),
        getval( proof_time, Proof_Time),
        (protein_flag( search, id_tree) -> 
	    getval( proof_depth, Proof_Depth); true),
	Red is Red_Nocut + Red_Cut,
	Factor is Factor_Nocut + Factor_Cut,
	nl( S),
	    printf( S, "   Proof Time:              %5.1f sec%n", [Proof_Time]),
        (protein_flag( search, id_tree) ->
            printf( S, "   Proof Depth:           %5d%n", [Proof_Depth]); true),
	(protein_result( proved) ->
            printf( S, "   Extension Steps:       %5d%n", [Ext]),
           (protein_flag( calculus, rme) ->
            printf( S, "   Restart Steps:         %5d%n", [Restart]); true),
            printf( S, "   Reduction Steps:       %5d", [Red]), 
           ((protein_flag( reduction, cut) ; 
             protein_flag( reduction, both)) ->
	    printf( S, " (incl.%6d with cut)%n", [Red_Cut]); nl(S)),
	    printf( S, "   Factoring Steps:       %5d", [Factor]),
           ((protein_flag( factorisation, cut) ; 
             protein_flag( factorisation, both)) ->
	    printf( S, " (incl.%6d with cut)%n", [Factor_Cut]); nl(S)),
	   (def_const( simplify, yes) ->
	    printf( S, "   Simplification Steps:  %5d", [Sim]),
           (SimFail > 0 ->
            printf( S, " (add.%7d forced failures)%n", [SimFail]); nl(S));true),
	   (def_const( theory, yes) ->
	    printf( S, "   Theory Steps:          %5d%n", [Theory]);true)
        ; true),
	    printf( S, "   Inferences:          %7d%n", [Inf]).


% ---------- Beweisinformationen schreiben ----------

%%% h_write_trc
%%% Schreibt die Beweisinfos in die Datei [Input-File-Name].trc

h_write_trc :-
	u_open_trc( S),
	printf( S,
        "%n------------------------ statistics ------------------------%n", []),
        h_write_trans_stat( S),
	h_write_stat( S),
	printf( S, 
       "%n------------------------- answers --------------------------%n", []),
        h_write_answers( S),
        close( S).

% END t_header_info.pl END

