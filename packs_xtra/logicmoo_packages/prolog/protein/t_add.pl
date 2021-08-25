%********************      t_add.pl      ********************

% Praedikate zur Erweiterung der Bodys 

%************************************************************

:- module_interface( t_add).

:- export add_get_flags / 1,
	  add_code / 3.
	 
:- begin_module( t_add).

:- use_module( t_definitions).
:- use_module( t_utils).
:- use_module( t_literal).
:- use_module( t_claus).
:- use_module( t_pred).

          
% ---------- Ergaenzt Code in den Bodys ----------

%%% add_get_flags( AddFlagL)
%%% Ermittlung Flags fuer folgende Berechnung

:- mode add_get_flags( -).

add_get_flags( [Search,Trace,Ancs,SidelitAncs,Factorisation,Regularity,
                DRME,MMR,CTest,HeadBehindProlog,ARME,ECP]) :-
	protein_flag( search, Search),
	protein_flag( trace, Trace),
	protein_flag( ancs, Ancs),
	protein_flag( th_sidelit_anc, SidelitAncs),
	protein_flag( factorisation, Factorisation),
	protein_flag( regularity, Regularity),
	protein_flag( delayed_rme, ZwiDRME),
	(protein_flag( calculus, me) -> DRME = off ; DRME = ZwiDRME),
	protein_flag( mmr, MMR),
	protein_flag( ctest, CTest),
	protein_flag( head_behind_prolog, HeadBehindProlog),
	(protein_flag( calculus, rme), protein_flag( ancestry_rme, on) ->
	    ARME = on ; ARME = off),
	(protein_flag( calculus, rme), protein_flag( ec_pruning, on) ->
            ECP = on ; ECP = off), !.


%%% add_code( FlagL, InPred, OutPred)
%%% Ergaenzt Code in den Bodyliteralen

:- mode add_code( ++, +, -).

add_code( FlagL, PredA, PredZ) :-
        pred_get( type, PredA, Type),
	def_get( group, Type, Group), !,
        l_ancestor(FlagL, Type, Group, PredA, PredB), !,
	l_aunts(   FlagL, Type, Group, PredB), !,
        l_reg(     FlagL, Type, Group, PredB), !,
	l_sort(    FlagL, Type, Group, PredB, PredC), !,
	l_block1(  FlagL, Type, Group, PredC, PredD), !,
	l_block2(  FlagL, Type, Group, PredD, PredE), !, 
	l_ecp(     FlagL, Type, Group, PredE, PredF), !,
	l_dynamic( FlagL, Type, Group, PredF, PredG), !,
        l_info(    FlagL, Type, Group, PredG, PredM), !,
        l_store(   FlagL, Type, Group, PredM, PredR), !,
        l_mmr(     FlagL, Type, Group, PredR, PredV), !,
	l_drme(    FlagL, Type, Group, PredV, PredW), !,
	l_search(  FlagL, Type, Group, PredW, PredX), !,
        l_trace(   FlagL, Type, Group, PredX, PredY), !,
	l_delete(  FlagL, Type, Group, PredY, PredZ), !.


%%% l_ancestor( FlagL, Type, Group, InPred, OutPred)
%%% Verbindet Ancestors in Head und Body.
%%% Theoriepraedikate werden immer als nicht disjunktiv angesehen.

:- mode l_ancestor( ++, ++, ++, +, -).

l_ancestor( [_,_,off|_], _, _, Pred, Pred).
l_ancestor( _, prolog, _, Pred, Pred).
l_ancestor( _, th_end, _, Pred, Pred).

l_ancestor( _, Type, Group, Pred, Pred) :- 
        Type \== reg,
	Group \== simplify, 
	pred_test( test, Pred).

l_ancestor( _, _, _, Pred, Pred) :- pred_get( body, Pred, []).

% Ancestors (und DJ) nur uebertragen
l_ancestor( [_,_,_,ThSideLitAncs|_], Type, _, Pred, Pred) :-
        (Type == reg ; Type == inc ; Type == th_prolog ;   
	 (Type == th_ext, ThSideLitAncs == off)), 
	pred_get( litL, Pred, LitL),
        lit_name( ancs, _, LitL),
	lit_name( dj, _, LitL).

% th_sidelit_ancs = on +++ 
l_ancestor( [_,_,AncsFlag,on|_], th_ext, _, Pred, Pred) :-	
	pred_get( theory, Pred, (LongThHead,_,ThCall,_,ConcL)),	
	lit_strip( LongThHead, ShortThHead),
	ShortThHead = theory( _, [Lit|_], _),
	lit_neg( Lit, NegLit),
	lit_get_ancs( [AncsFlag], LongThHead, NegLit, ExtAncs),   
	lit_name( ancs, Ancs, [LongThHead|ConcL]), Ancs = Ancs,
	lit_name( ancs, ExtAncs, ThCall).	
	
% Theoriepreds und th_sidelit_ancs = on: 
l_ancestor( [_,_,AncsFlag,on|_], _, _, Pred, Pred) :- 
	pred_get( theory, Pred, (Head,_,ThCall,_,ConcL)),
	lit_strip( Head, ShortHead),
	lit_get_ancs( [AncsFlag], Head, ShortHead, Ancs),
	lit_name( ancs, Ancs, ThCall),
	lit_strip( ThCall, theory( _, _, ThAncs)),
	lit_name( ancs, ThAncs, ConcL).

% Ancs = pos_dj oder both_dj: 
% - DJ Zusatzargument bestuecken
% - verhindern, dass positive Blaetter aus nicht disjunktiven Klauseln
%   in die Ancestorliste kommen
l_ancestor( [_,_,AncsFlag|_], _, _, InPred, OutPred) :-
	(AncsFlag == pos_dj ; AncsFlag == both_dj), !,
	pred_get( litL, InPred, [Head|BodyL]),
	(pred_test( disjunctive, InPred) ->
	    lit_name( dj, dj, BodyL)
        ;
            lit_name( dj, n,  BodyL)),
	lit_strip( Head, ShortHead),
        lit_get_ancs( [AncsFlag], Head, ShortHead, LongAncs),
	(lit_test_neg( Head) ->
	   lit_name( ancs, ShortAncs, Head),
	   lit_name( ancs, ZwiAncs, BodyL),
	   lit_name( dj, DJ, Head),
	   l_add_body( [(DJ == dj -> ZwiAncs=LongAncs; ZwiAncs=ShortAncs)], [],
                       InPred, OutPred)
        ; 
	   lit_name( ancs, LongAncs, BodyL),   
           InPred = OutPred).

l_ancestor( [_,_,AncsFlag|_], _, _, Pred, Pred) :-
	pred_get( litL, Pred, [Head|BodyL]),
	lit_strip( Head, ShortHead),
        lit_get_ancs( [AncsFlag], Head, ShortHead, Ancs),
        lit_name( ancs, Ancs, BodyL).


%%% l_aunts( FlagL, Type, Group, Pred)
%%% Setzt Aunts in Head und Body

:- mode l_aunts( ++, ++, ++, +).

l_aunts( [_,_,_,_,off|_], _, _).
l_aunts( _, prolog, _, _).
l_aunts( _, th_prolog, _, _).

l_aunts( _, Type, Group, Pred) :- 
	Type \== reg, 
	Group \== simplify,
        pred_test( test, Pred). 

l_aunts( _, _, _, Pred) :- pred_get( body, Pred, []).

% th_sidelit_ancs = on +++ 
l_aunts( [_,_,_,on|_], th_ext, _, Pred) :-	
	pred_get( theory, Pred, (LongThHead,_,_,_,ConcL)),	
	lit_strip( LongThHead, ShortThHead),
	ShortThHead = theory( _, [ShortLit|_], _),
	lit_add( ShortLit, LongLit),
	lit_name( aunts, Aunts, LongThHead),
	lit_set_aunts( [LongLit|ConcL], Aunts).	

% Aunts nur weiterreichen
l_aunts( _, Type, _, Pred) :-
	(Type == reg ; Type == inc),
	pred_get( litL, Pred, LitL),
	lit_name( aunts, _, LitL).

% Aunts erweitern
l_aunts( _, _, _, Pred) :-
	pred_get( litL, Pred, [Head|BodyL]),
	lit_name( aunts, Aunts, Head),
	lit_set_aunts( BodyL, Aunts). 


%%% l_block1( FlagL, Type, Group, InPred, OutPred)
%%% Blockinformationen: Blockvariable

:- mode l_block1( ++, ++, ++, +, -).

l_block1( [_,_,_,_,_,_,_,off,_,_,_,off|_], _, _, Pred, Pred).

l_block1( _, query, _, InPred, OutPred) :-
	pred_get( body, InPred, BodyL),
	lit_name( block, BlockN, BodyL),
	l_add_body( [incval(block),getval(block,BlockN)], [], InPred, OutPred).

l_block1( _, _, _, Pred, Pred) :-
	pred_get( litL, Pred, LitL),
	lit_name( block, _, LitL).


%%% l_block2( FlagL, Type, Group, InPred, OutPred)
%%% Blockinformationen: Ancestors und Aunts mit Block

:- mode l_block2( ++, ++, ++, +, -).

l_block2( [_,_,_,_,_,_,_,  off,_,_,_,off|_], _, _, Pred, Pred).
l_block2( [_,_,_,_,_,_,_,wgcwa,_,_,_,off|_], _, _, Pred, Pred).

l_block2( _, _, _, Pred, Pred) :-
	pred_get( litL, Pred, [Head|BodyL]),
	lit_name( block, BlockN, Head),
	lit_name( ancs, HAncs, Head),
	lit_name( b_ancs, HBAncs, Head),
        lit_name( aunts, HAunts, Head),
	lit_name( b_aunts, HBAunts, Head),
        checklist( l_block( ancs, b_ancs, BlockN, HAncs, HBAncs), BodyL), 
	checklist( l_block( aunts, b_aunts, BlockN, HAunts, HBAunts), BodyL).


%%% l_drme( FlagL, Type, Group, InPred, OutPred) 
%%% Delayed Restart Model Elimination: Wakevariable und delayen

:- mode l_drme( ++, ++, ++, +, -).

l_drme( [_,_,_,_,_,_,off|_], _, _, Pred, Pred).

% neue Wake-Variable und aufwecken
l_drme( [_,_,_,_,_,_,DRME|_], query, _, InPred, OutPred) :-
	pred_get( body, InPred, BodyL),
	lit_name( delay, Wake, BodyL),
	(DRME == cut -> CutL = [!] ; CutL = []),
	append( CutL, [p_call(Wake = awake)], CodeL),
	l_add_body( [], CodeL, InPred, OutPred).

% Restarts delayen
l_drme( _, restart, _, InPred, OutPred) :-
	pred_get( litL, InPred, [Head|BodyL]),
	lit_name( delay, Wake, Head),
	maplist( lit_enclose(restart,Wake), BodyL, OutBodyL), 
	pred_chg( body, InPred, OutBodyL, OutPred).

% Wakeargument weitergeben
l_drme( _, _, _, Pred, Pred) :-
	pred_get( litL, Pred, LitL),
	lit_name( delay, _, LitL).


%%% l_dynamic( FlagL, Type, Group, InPred, OutPred)
%%% Dynamische Baumschreibung

:- mode l_dynamic( ++, ++, ++, +, -).

% Blatt das nicht angezeigt wird
l_dynamic( [_,dynamic(_)|_], th_end, _, Pred, Pred).

% Test, nur uebertragen
l_dynamic( [_,dynamic(_)|_], Type, _, Pred, Pred) :-
	def_get( leaf, Type, u),
	pred_get( litL, Pred, LitL),
	lit_name( trace, _, LitL).

% Theorieschritte
l_dynamic( [_,dynamic(Speed)|_], Type, Group, InPred, OutPred) :-
        Group \== th_start,
        Type \== th_prolog,
	def_get( info, Type, (Blatt,Text,Shape,Color)), 
	pred_get( litL, InPred, [Head|BodyL]),
	lit_strip( Head, ShortHead),
	l_get_node( ShortHead, Node),
	lit_neg( Node, NegNode),
	append( _, [ThCall|BodyR], BodyL),
	nonvar( ThCall),
	functor( ThCall, theory, _),
	lit_name( trace, Father, [Head,ThCall]),
	lit_name( trace, Son, BodyR),
	CodeL=[p_dynamic(Blatt,Text,Shape,Color,Node,NegNode,Father,Son,Speed)],
	(Blatt == y ->
	    l_add_body( [], CodeL, InPred, OutPred)
        ; 
	    l_add_body( CodeL, [], InPred, OutPred)).

% Blatt
l_dynamic( [_,dynamic(Speed)|_], Type, _, InPred, OutPred) :-
	def_get( info, Type, (y,Text,Shape,Color)),
	pred_get( head, InPred, Head),
	lit_strip( Head, ShortHead),
	l_get_node( ShortHead, Node),
	lit_neg( Node, NegNode),
	lit_name( trace, Father, Head),
	l_add_body( [],[p_dynamic(y,Text,Shape,Color,_,NegNode,Father,_,Speed)],
                    InPred, OutPred).

% Knoten
l_dynamic( [_,dynamic(Speed)|_], Type, _, InPred, OutPred) :-
	def_get( info, Type, (n,Text,Shape,Color)),
	pred_get( litL, InPred, [Head|BodyL]),
	lit_strip( Head, ShortHead),
	l_get_node( ShortHead, Node),
	lit_neg( Node, NegNode),
	lit_name( trace, Father, Head),
	maplist( l_collect_trace, BodyL, ZwiTraceL),
	flatten( ZwiTraceL, TraceL),
	checklist( =(Son), TraceL),
	l_erase_in_body( InPred, (trace, 1), ZwiPred),
	l_add_body(  
                  [p_dynamic(n,Text,Shape,Color,Node,NegNode,Father,Son,Speed)],
                       [], ZwiPred, OutPred).

l_dynamic( _, _, _, Pred, Pred).


%%% l_ecp( FlagL, Type, Group, InPred, OutPred)
%%% EC-Pruning

:- mode l_ecp( ++, ++, ++, +, -).

l_ecp( [_,_,_,_,_,_,_,_,_,_,_,on|_], query, _, InPred, OutPred) :-
	pred_get( litL, InPred, [Head|BodyL]),
	lit_strip( Head, restart),
	lit_name( block, RBlockN, Head),
	lit_name( block, BlockN, BodyL),
	lit_enclose( block, RBlockN, RLit, BRLit),
	lit_name( b_ancs, (_,[BRLit|_]), Head),
	l_add_body( [], [p_call(protein_ae(link,(BlockN,RLit,RBlockN)))],
	            InPred, OutPred).

l_ecp( _, _, _, Pred, Pred).


%%% l_info( FlagL, Type, Group, InPred, OutPred)
%%% Info

:- mode l_info( ++, ++, ++, +, -).

l_info( [_,off|_], _, _, Pred, Pred).

% Restart Steps
l_info( _, restart, _, InPred, OutPred) :-
	l_add_body( [incval(inf)], [p_count(restart)], InPred,OutPred).

% Extension Steps ohne Queries
l_info( _, Type, _, InPred, OutPred) :-
	(Type == ext ; Type == fact),
	l_add_body( [incval(inf)], [p_count(ext)], InPred, OutPred). 

% Rewriting Steps
l_info( _, sim(rew,-), _, InPred, OutPred) :-
	l_add_body( [incval(inf)], [p_count(sim)], InPred, OutPred).

% Simplification Steps ausser Rewriting
l_info( _, _, simplify, InPred, OutPred) :-
	pred_get( body, InPred, BodyL),
	(append( ZwiBodyL, [fail], BodyL) ->
	    pred_chg( body, InPred, ZwiBodyL, ZwiPred),
	    l_add_body( [incval(inf)], [incval(sim_fail), fail], 
                        ZwiPred, OutPred)
        ;
	    l_add_body( [incval(inf)], [p_count(sim)], InPred, OutPred)).

% Theory Steps
l_info( _, _, th_start, InPred, OutPred) :-
	l_add_body( [incval(inf)], [p_count(theory)], InPred, OutPred).

% Tests
l_info( _, Type, Group, InPred, OutPred) :-
	(Group == red ; Group == factor),
	CodeL = [incval(inf), p_count(Type)],
	l_add_body( [], CodeL, InPred, OutPred).

l_info( _, _, _, Pred, Pred).


%%% l_mmr( FlagL, Type, Group, InPred, OutPred)
%%% General Closed World Assumption II

:- mode l_mmr( ++, ++, ++, +, -).

l_mmr( [_,_,_,_,_,_,_,wgcwa|_], query, _, InPred, OutPred) :-
	pred_get( body, InPred, BodyL),
	lit_name( block, BlockN, BodyL),
	l_add_body( [], [p_call((BlockN == 1 -> 
                                   assert(protein_result(indefinite)) ; true))],
	            InPred, OutPred).

% PCs berechnen und Konsistenztests ausfuehren
l_mmr( [_,_,_,_,_,_,_,gcwa,CTest|_], query, _, InPred, OutPred) :-
	(CTest == now ; CTest == store),
	l_add_body( [], [!,p_mmr(CTest)], InPred, OutPred).

l_mmr( _, _, _, Pred, Pred).	


%%% l_reg( FlagL, Type, Group, Pred)
%%% Setzt Regularity-Argument

:- mode l_reg( ++, ++, ++, +).

l_reg( [_,_,_,_,_,off|_], _, _, _).
l_reg( [_,_,_,_,_,nodelay,_,_,_,_,off|_], _, _, _).
l_reg( _, prolog, _, _).
l_reg( _, _, theory, _).

% Regularitaetstests
l_reg( _, _, reg, Pred) :-
	pred_get( litL, Pred, [Head|BodyL]),
	lit_name( reg, r, Head),
	lit_name( reg, n, BodyL).

% keine Regularitaetstests nach Restarts
l_reg( _, restart, _, Pred) :-
        pred_get( litL, Pred, LitL),
        lit_name( reg, n, LitL).

% keine Regularitaetstest fuer die erste Extension bei arme
l_reg( [_,_,_,_,_,_,_,_,_,_,on|_], query, _, Pred) :-
	pred_get( body, Pred, LitL),
	lit_add( none, LongNone),
	member( LongNone, LitL),
	lit_name( reg, n, LitL).

% keine Regularitaetstests fuer negative/positive Blaetter bei nur 
% positiven/negativen Ancestors
l_reg( [_,_,Ancs,_,_,delay|_], _, _, Pred) :-
	pred_get( litL, Pred, [Head|BodyL]),
	(Ancs == off ; 
	 (lit_test_neg( Head), Ancs == neg) ;
         (\+ lit_test_neg( Head), (Ancs == pos ; Ancs == pos_dj))), !,
	lit_name( reg, _, Head),
	lit_name( reg, r, BodyL).

% Regularitaetstests bei extendierten Literalen
l_reg( [_,_,_,_,_,delay|_], _, ext, Pred) :-
        pred_get( litL, Pred, [Head|BodyL]),
        lit_name( reg, n, Head),
        lit_name( reg, r, BodyL).

% Tests etc erst nach einem Regularitaetstest
l_reg( [_,_,_,_,_,delay|_], _, _, Pred) :-
	pred_get( head, Pred, Head),
	lit_name( reg, n, Head).

l_reg( _, _, _, _).


%%% l_search( FlagL, Type, Group, InPred, OutPred)
%%% Iterative Tree Deepening I und Iterative Term Deepening

:- mode l_search( ++, ++, ++, +, -).

l_search( [prolog|_], _, _, Pred, Pred).

l_search( [id_inf|_], _, _, InPred, OutPred) :-
	pred_get( costs, InPred, (Cost,Plus)),
	Cost > 0,
	pred_get( litL, InPred, LitL),
	lit_name( depth, Depth, LitL),
	l_add_body( [getval( id_inf, InInf),
	             p_call( plus(InInf,Cost,Inf1)),
		     p_call( plus(Inf1,Plus,Inf2)),
                     (Inf2 =< Depth ->
                         p_count( id_inf, Cost) 
                     ;
                         setval( more_depth,1), !, fail)],
	            [], InPred, OutPred).

l_search( [id_inf|_], _, _, InPred, OutPred) :-
	pred_get( costs, InPred, (0,Plus)), 
	Plus > 0,
        pred_get( litL, InPred, LitL),
	lit_name( depth, Depth, LitL),
	l_add_body( [getval( id_inf, InInf),
	             p_call( plus(InInf,Plus,ZwiInf)),
                     (ZwiInf =< Depth -> true ; 
                         setval( more_depth,1), !, fail)], 
	            [], InPred, OutPred).

l_search( [id_inf|_], _, _, Pred, Pred) :-
	pred_get( litL, Pred, LitL),
	lit_name( depth, _, LitL).

l_search( [Search|_], _, _, InPred, OutPred) :-
	pred_get( theory, InPred, (Head,_,ThCall,_,ConcL)),
	l_do_search( [Search], InPred, [Head,ThCall], ConcL, OutPred).    

l_search( [Search|_], _, _, InPred, OutPred) :-
	pred_get( litL, InPred, [Head|BodyL]),
	l_do_search( [Search], InPred, Head, BodyL, OutPred).

l_search( [id_term|_], inc, _, InPred, OutPred) :-
	protein_flag( depth_increment, Inc),
	protein_flag( out_stream, S),
	pred_get( litL, InPred, [Head|BodyL]),
	lit_name( depth, Weight, Head),
	lit_name( depth, NewWeight, BodyL),
	CodeL = [p_call( plus(Weight,Inc,NewWeight)), 
                 p_call( write(NewWeight)),
                 p_call( write(', ')), 
                 p_call( flush(S))],
	l_add_body( CodeL, [], InPred, OutPred).
	
l_search( [id_term|_], Type, th_start, InPred, OutPred) :-
	Type \== th_fact,
	pred_get( theory, InPred, (Head,TestL,ThCall,_,ConcL)),
	lit_name( depth, Depth, Head),
	lit_name( depth, Depth, ConcL),
	lit_strip( ConcL, ZwiLitL),
	u_splitlist( def_test_prolog_lit, ZwiLitL, _, WeightLitL),
	flatten( [TestL,[ThCall], 
                  [p_weight(WeightLitL,Weight),
                   p_call(Depth >= Weight)],ConcL],
                  BodyL), 
	pred_chg( body, InPred, BodyL, OutPred).
	
l_search( [id_term|_], ext, _, Pred, Pred) :-
	pred_get( litL, Pred, LitL),
	lit_name( depth, _, LitL).

l_search( _, _, _, Pred, Pred).


%%% l_do_search( FlagL, InPred, ToNameInLitL, ToNameOutLitL, OutPred)

l_do_search( [id_tree|_], Pred, ToNameIn, ToNameOut, Pred) :-	
	pred_get( costs, Pred, (0,0)), !,
	lit_name( depth, Depth, ToNameIn),
	lit_name( depth, Depth, ToNameOut).

l_do_search( [id_tree|_], InPred, ToNameIn, ToNameOut, OutPred) :-
	pred_get( costs, InPred, (0,Plus)), !, 
        lit_name( depth, Depth, ToNameIn),
	lit_name( depth, Depth, ToNameOut), 
	l_add_body( [(Depth >= Plus -> true ;
                        setval(more_depth,1), !, fail)], 
	            [], InPred, OutPred).
	
l_do_search( [id_tree|_], InPred, ToNameIn, ToNameOut, OutPred) :-  
	pred_get( costs, InPred, (Cost1,Plus)), !, 
	lit_name( depth, DepthIn, ToNameIn),
	lit_name( depth, DepthOut, ToNameOut),
	Cost2 is Cost1 + Plus,
	l_add_body( [(DepthIn >= Cost2 ->
                        plus(DepthOut,Cost1,DepthIn)
                     ;
                        setval( more_depth, 1), !, fail)],
	            [], InPred, OutPred).


%%% l_sort( FlagL, Type, Group, InPred, OutPred)
%%% Sortiert erstes Prologlit an erste Stelle des Bodies

:- mode l_sort( ++, ++, ++, +, -).

l_sort( [_,_,_,_,_,_,_,_,_,on|_], _, _, InPred, OutPred) :-
	pred_get( body, InPred, InBodyL),
	delete( ProLit, InBodyL, BodyR),
	lit_strip( ProLit, protein_prolog(_)),
	pred_chg( body, InPred, [ProLit|BodyR], OutPred).

l_sort( _, _, _, Pred, Pred).


%%% l_store( FlagL, Type, Group, InPred, OutPred)
%%% Speichern von Information fuer MMR und RME mit ECP

:- mode l_store( ++, ++, +, -).

% Fuer MMR und ECP
l_store( [_,_,_,_,_,_,_,wgcwa,_,_,_,off|_], _, _, Pred, Pred).
l_store( [_,_,_,_,_,_,_,  off,_,_,_,off|_], _, _, Pred, Pred).

% Reduktionlinks zwischen Blocks aufsammeln
l_store( _, _, red, InPred, OutPred) :-
	pred_get( head, InPred, Head),
	pred_get( litN, InPred, N),
	lit_name( block, BlockN, Head),
	lit_name( b_ancs, (PosBAnc,NegBAnc), Head),
	lit_strip( Head, ShortHead),
	lit_neg( ShortHead, Lit),
	lit_enclose( block, RBlockN, Lit, BLit),
	(lit_test_neg( Lit) ->
	    NTest = p_unifiable_n_count( BLit, NegBAnc, N)
        ;
	    NTest = p_unifiable_n_count( BLit, PosBAnc, N)),
	l_add_body( [], [p_call(once(NTest)),
                         p_call(
                            ((BlockN == RBlockN) -> true ;
                         p_assert_bck(protein_ae(link,(BlockN,Lit,RBlockN)))))],
                    InPred, OutPred). 

% Nur MMR
l_store( [_,_,_,_,_,_,_,wgcwa|_], _, _, Pred, Pred).
l_store( [_,_,_,_,_,_,_,  off|_], _, _, Pred, Pred).

% Restarts aufsammeln
l_store( _, query, _, InPred, OutPred) :-
	pred_get( litL, InPred, [Head|BodyL]),
	lit_strip( Head, restart), 
	lit_name( block, RBlockN, Head),
	lit_name( block, BlockN, BodyL),
	lit_enclose( block, RBlockN, RLit, BRLit),
	lit_name( b_ancs, (_,[BRLit|_]), Head),
	l_add_body([],[p_assert_bck(protein_ae(restart,(RBlockN,RLit,BlockN)))],
                    InPred, OutPred).	

% positive Blaetter aufsammeln
l_store( _, restart, _, InPred, OutPred) :-
	pred_get( head, InPred, Head),
	lit_name( block, BlockN, Head),
	lit_strip( Head, ShortHead),
	l_add_body( [], [p_assert_bck(protein_ae(leaf,(BlockN,ShortHead)))],
                    InPred, OutPred).

% Faktorisierungslinks zwischen Blocks aufsammeln
l_store( _, _, factor, InPred, OutPred) :-
	pred_get( head, InPred, Head),
	pred_get( litN, InPred, N),
	lit_name( block, BlockN, Head),
	lit_name( b_aunts, (PosBAnc,NegBAnc), Head),
	lit_strip( Head, Lit),
	lit_enclose( block, RBlockN, Lit, BLit),
	(lit_test_neg( Lit) ->
	    NTest = p_unifiable_n_count( BLit, NegBAnc, N)
        ;
	    NTest = p_unifiable_n_count( BLit, PosBAnc, N)),
	l_add_body( [], [p_call(once(NTest)),
	                 p_call(
                            ((BlockN == RBlockN) -> true ;
                         p_assert_bck(protein_ae(link,(BlockN,Lit,RBlockN)))))],
                    InPred, OutPred). 

l_store( _, _, _, Pred, Pred).


%%% l_trace( FlagL, Type, Group, InPred, OutPred)
%%% Trace

:- mode l_trace( ++, ++, ++, +, -).

l_trace( [_,Trace|_], _, _, Pred, Pred) :- Trace \== internal, !.

% Trace wird nur uebertragen
l_trace( _, Type, _, Pred, Pred) :- 
	def_get( leaf, Type, u), 
	pred_get( litL, Pred, [Head|BodyL]),        
	lit_name( trace, Trace, Head),
	maplist( l_collect_trace, BodyL, ZwiTraceL),
	flatten( ZwiTraceL, [Trace]).

% Ende der Theorieverarbeitung
l_trace( _, th_end, _, Pred, Pred) :- 
	pred_get( head, Pred, Head),
	lit_name( trace, th_end, Head).

% Blaetter
l_trace( _, Type, Group, InPred, OutPred) :-
	Group \== theory,
	def_get( leaf, Type, y),
	((Type == fact ; Type == prolog) ->
	    pred_get( clausN, InPred, ClausNOrSteps),
            pred_get( litN, InPred, LitN)
        ; 
	    pred_get( litN, InPred, ClausNOrSteps),
            LitN = 0),
	pred_get( head, InPred, Head),
	lit_strip( Head, ShortHead),
	l_get_node( ShortHead, Node),
        lit_name( trace, Trace, Head),
	l_add_body( [], [p_trace(Type,ClausNOrSteps,LitN,Node,[],Trace)], 
                    InPred, OutPred). 

% Ancestors
l_trace( _, Type, _, InPred, OutPred) :-
	pred_get( clausN, InPred, ClausN),
	pred_get( litN, InPred, LitN),
	pred_get( litL, InPred, [Head|BodyL]),
	lit_strip( Head, ShortHead),
	l_get_node( ShortHead, Node),
        lit_name( trace, Trace, Head),
        maplist( l_collect_trace, BodyL, ZwiSubTraceL),
	flatten( ZwiSubTraceL, SubTraceL),
	l_add_body( [], [p_trace(Type,ClausN,LitN,Node,SubTraceL,Trace)], 
                    InPred, OutPred). 


% ---------- Loeschen ueberfluessiger Literale ----------

%%% l_delete( FlagL, Type, Group, InPred, OutPred)
%%% Loeschen ueberfluessiger Literale

:- mode l_delete( ++, +, ++, +, -).

l_delete( [_,_,_,_,_,_,_,_,_,_,on|_], query, _, InPred, OutPred) :-
	def_const( zarg, A),
	l_erase_in_body( InPred, (none,A), OutPred).

l_delete( _, sim(_,con), simplify, InPred, OutPred) :-
	def_const( zarg, A),
	l_erase_in_body( InPred, (none,A), OutPred).

l_delete( _, _, _, Pred, Pred).


% ---------- Utility-Praedikate ----------

%%% l_get_node( Lit, Node)
%%% Ermittelt aus dem Theoriepraedikat den NegNode fuer den Trace

:- mode l_get_node( +, -).

l_get_node( protein_prolog( Node), Node).
l_get_node( theory(_,[Node|_],_), Node).
l_get_node( Node, Node).


%%% l_collect_trace( Lit, Trace)
%%% Sammelt TraceVariablen aus den Bodyliteralen

:- mode l_collect_trace( ?, -).

l_collect_trace( Var, []) :- var( Var), !.        
l_collect_trace( u_delay( _, Lit), [Trace]) :- lit_name( trace, Trace, Lit), !.
l_collect_trace( Lit, []) :- def_test_prolog_lit( Lit). 
l_collect_trace( Lit, [Trace]) :- lit_name( trace, Trace, Lit), !.


%%% l_block( ZArg, BlockedZArg, BlockN, HeadZArg, BlockedHeadZArg, LongLit)
%%% Erweitert Ancestors oder Aunts um Blocknummer

:- mode l_block( ++, ++, ?, ?, ?, ?).

l_block( ZArg, BZArg, BlockN, (HPos,HNeg), (HPosB,HNegB), LongLit) :-
	   lit_name( ZArg, (BPos,BNeg), LongLit),
           lit_name( BZArg, (BPosB,BNegB), LongLit),   
 	   (BPos \== HPos ->
	       copy_term( BPos, CBPos),
               append( PosLitL, [], CBPos), 
	       maplist( lit_enclose( block, BlockN), PosLitL, BPosLitL),
               append( BPosLitL, HPosB, BPosB) 
           ; 
               BPosB = HPosB), 
           (BNeg \== HNeg ->
               copy_term( BNeg, CBNeg),
               append( NegLitL, [], CBNeg), 
	       maplist( lit_enclose( block, BlockN), NegLitL, BNegLitL),
               append( BNegLitL, HNegB, BNegB) 
           ; 
               BNegB = HNegB).


%%% l_add_body( BeginL, EndL, InPred, OutPred)
%%% Haengt Code hinter dem alten Body an

:- mode l_add_body( +, +, +, -).

l_add_body( BeginL, EndL, InPred, OutPred) :-
	pred_get( body, InPred, BodyL),
	flatten( [BeginL,BodyL,EndL], OutBodyL),
	pred_chg( body, InPred, OutBodyL, OutPred).


%%% l_erase_in_body( InPred, Term, OutPred)
%%% Loescht Praed im Format (Fnc, Arity) aus Body

:- mode l_erase_in_body( +, +, -).

l_erase_in_body( InPred, Praed, OutPred) :-
	pred_get( body, InPred, InBodyL),
	maplist( l_do_erase( Praed), InBodyL, ZwiBodyL),
	flatten( ZwiBodyL, OutBodyL),
	pred_chg( body, InPred, OutBodyL, OutPred).


%%% l_do_erase( Term, InLit, OutLitL)

:- mode l_do_erase( ++, ?, -).

l_do_erase( _, Lit, [Lit]) :- var( Lit), !.
l_do_erase( (Fnc,Arity), Lit, []) :- functor( Lit, Fnc, Arity), !.
l_do_erase( _, Lit, [Lit]).

