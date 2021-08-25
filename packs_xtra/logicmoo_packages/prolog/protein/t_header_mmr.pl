%********************   t_header_mmr.pl  ********************

% Praedikate fuer das Minimal Model Reasoning

%************************************************************

%%% p_mmr( Type)
%%% Berechnen der potential candidates

:- mode p_mmr( ++).

p_mmr( now) :-
	getval( block, BlockN),
	protein_ae( link, (BlockN,_,_)),
	delayed_goals( GoalL),
	maplist( l_get_lit, GoalL, PCL),
	l_test_pc( [PCL]),
	retract_all( protein_ae(_,_)).

p_mmr( now) :-
	\+ protein_ae( link, (BlockN,_,_)),
	setof( Leaf, BlockN^protein_ae(leaf,(BlockN,Leaf)), PC),
	l_test_pc( [PC]),
	retract_all( protein_ae(_,_)).
	
p_mmr( store) :-
	findall( _, h_find_cut, _),
	findall( PC, protein_ae(pc,PC), PCL),
	l_test_pc( PCL).

p_mmr( _).


%%% l_get_lit( Goal, Lit)
%%% Extrahiert gerestartetes Literal aus dem Aufruf

l_get_lit( Restart, Lit) :-
	Restart =.. [restart|ZArgL],
	once( member( (_,[Lit|_]), ZArgL)).

l_get_lit( _, []).


%%% l_test_pc( PCL)
%%% Konsisenztest eines Kandidaten

:- mode l_test_pc( +).

l_test_pc( []) :- !.
l_test_pc( [[]|PCR]) :- l_test_pc( PCR), !.

l_test_pc( [PC|PCR]) :-
	getval( block, BlockN),
	u_out( "Block%3d: testing potential candidate %w... ", [BlockN,PC]),
        maplist( p_neg_shortlit(_), PC, NegPC),
	(protein_cc_prove( NegPC) ->
	    u_out( "proved%n"),
	    l_test_pc( PCR)
        ;
	    u_out( "failed%n"),
	    (protein_result( indefinite) -> true;
               retract_all(protein_result(_)),
	       assert(protein_result( indefinite)))).


%%% h_mmr
%%% Ausgabe der potential candidates oder Modelle

h_mmr :-  
	protein_flag( mmr, gcwa), !, 
	h_trc,
	findall( _, h_find_cut, _),
	(protein_ae( pc, _) ->
	    u_out( "%nPotential Candidates for Abductive Explanations%n"),
	    u_out(   "-----------------------------------------------%n"),
	    findall( _, (protein_ae(pc,PC),u_out("%w%n",[PC])), _)
	;
	    u_out( "no potential candidates for abductive explanations")).

h_mmr :-
	protein_flag( mmr, model), !,
	findall( _, h_find_model, _),
	h_trc,
	(protein_ae( model, _) ->
	    u_out( "%nModels%n------%n"),
	    findall( _, (protein_ae(model,LitL), 
	                 maplist(p_neg_shortlit(_),LitL,NegLitL),
			 u_out( "%w%n", [NegLitL])), _)
        ;
	    u_out( "no model found%n")).


%%% h_find_cut
%%% Berechnet ein Potential Candidate
%%% 1. fuer jeden Block einen Baum mit Tiefe 1 aufbauen.
%%% 2. falls es einen Link von einem solchen Blockbaum zu einem anderen gibt,
%%%    wird der andere unter das Blatt gehaengt, das gerestarted wurde, bis 
%%%    man zu diesem Block kam.
%%% 3. alle moeglichen Cuts sind die AEs

:- mode h_find_cut( ++, +).

% Baum der Tiefe 1 der nicht unter einem anderen haengt
h_find_cut :-
	h_get_leafs_of_block( BlockN, LitL),
	\+ (protein_ae(link,(BlockN,_,BlockM)), BlockN \== BlockM),
	h_assert_sorted( pc, LitL),
	findall( _, h_find_cut( BlockN, LitL), _).

% Cuts mit darunterhaengenden Baeumen
h_find_cut( BlockN, LitLN) :-
	protein_ae( link, (BlockM,_,BlockN)),
	h_get_leafs_of_block( BlockM, LitLM),
	h_get_restarted_leaf( BlockN, BlockM, RestartLit),
	append( LitLNa, [RestartLit|LitLNb], LitLN),
	flatten( [LitLNa,LitLM,LitLNb], LitL),
	h_assert_sorted( pc, LitL),
	findall( _, h_find_cut( BlockN, LitL), _),
	findall( _, h_find_cut( BlockM, LitL), _).


%%% h_get_leafs_of_block( BlockN, LeafL)
%%% Ermittelt die Blaetter eines Blockes ohne die, unter denen ein 
%%% geschlossenes Tableaux haengt.
%%% Failt, falls keine vorhanden sind.

% :- mode h_get_leafs_of_block( ++, -).

h_get_leafs_of_block( BlockN, LeafL) :-
	setof( Leaf, (protein_ae(leaf,(BlockN,Leaf)),
	              \+ h_delete_leaf(BlockN,Leaf)), LeafL).


%%% h_get_restarted_leaf( BlockN, BlockM, Lit)
%%% Ermittelt ob die Bloecke durch Restarts miteinander verbunden sind und
%%% welches Literal als erstes gerestarted wurde.

:- mode h_get_restarted_leaf( ++, ++, -).

h_get_restarted_leaf( BlockN, BlockM, Lit) :-
	protein_ae( restart, (BlockN,Lit,BlockM)).

h_get_restarted_leaf( BlockN, BlockM, Lit) :-
	protein_ae( restart, (BlockN,Lit,BlockK)),
	h_get_restarted_leaf( BlockK, BlockM, _).


%%% h_find_model
%%% Berechnet Modell aus 
%%% 1. Rekonstruktion der Pfade aus den Leafs und den Restarts
%%% 2. Jeder Pfad, unter dessen Blatt kein saturiertes oder geschlossenes
%%%    Tableaux haengt ist ein Model

h_find_model :-
	protein_ae( leaf, (BlockN,Lit)),
	\+ protein_ae( restart, (_,_,BlockN)),
	h_extend_path( BlockN, [Lit]).


%%% h_extend_path( BlockN, LitL)
%%% Erweiterung des Pfades und Ausgabe als Modell, falls unter dem letzten 
%%% Blatt kein geschlossenes Tableaux haengt.

:- mode h_extend_path( ++, +).

h_extend_path( BlockN, [Lit1|LitR]) :-
	protein_ae( restart, (BlockN,Lit1,BlockM)),
	protein_ae( leaf, (BlockM,Lit2)),
	h_extend_path( BlockM, [Lit2,Lit1|LitR]).

h_extend_path( BlockN, [Lit|LitR]) :- 
	\+ protein_ae( restart, (BlockN,Lit,_)),
	\+ h_delete_leaf( BlockN, Lit),
	h_assert_sorted( model, [Lit|LitR]).
	
	
%%% h_delete_leaf( BlockN, Leaf)
%%% Loeschen von Leafs, unter denen ein geschlossenes Tableaux haengt.

:- mode h_delete_leaf( ++, +).

h_delete_leaf( BlockN, Lit) :- protein_ae( delete, (BlockN,Lit)).

h_delete_leaf( BlockN, Lit1) :-
	protein_ae( link, (BlockM,Lit1,BlockN)),
	findall( Lit2, protein_ae(leaf,(BlockM,Lit2)), LitL),
	checklist( h_delete_leaf(BlockM), LitL),
	assert( protein_ae( delete, (BlockN, Lit1))).


%%% h_assert_sorted( Flag, LitL)
%%% Test auf Doppelte und gegebenenfalls Speichern der AE

:- mode h_assert_sorted( ++, +).

h_assert_sorted( Flag, LitL) :- 
	sort( 0, <, LitL, SortedLitL),
	protein_ae( Flag, SortedLitL), !.

h_assert_sorted( Flag, LitL) :- 
	sort( 0, <, LitL, SortedLitL),
	assert( protein_ae( Flag, SortedLitL)).
	

%%% h_trc
%%% Ausgabe der aufgesammelten Informationen

h_trc :-
	protein_flag( trace, info), !,
	u_open_trc( S),
	findall( _, (protein_ae(leaf,(BlockN1,Lit1)),
		     writeln(S,leaf(BlockN1,Lit1))), _), nl( S),
	findall( _, (protein_ae(delete,(BlockN1,Lit1)),
		     writeln(S,delete(BlockN1,Lit1))), _), nl( S),
	findall( _, (protein_ae(restart,(BlockN2,Lit2,BlockM2)),
	             writeln(S,restart(BlockN2,Lit2,BlockM2))), _), nl( S),
	findall( _, (protein_ae(link,(BlockN3,Lit3,BlockM3)),
	             writeln(S,link(BlockN3,Lit3,BlockM3))), _), nl( S), 
	findall( _, (protein_ae(pc,PC1),writeln(S,pc(PC1))), _),
	close( S).

h_trc.

% END t_header_mmr.pl END

