% ********************  t_translate_3.pl  ********************

% Dritter Teil der Compilation:
% Generierung der Praedikate aus den Klauseln

% ************************************************************

:- module_interface( t_translate_3).

:- export g_translate_3 / 0.

:- begin_module( t_translate_3).

:- use_module( t_definitions).
:- use_module( t_utils).
:- use_module( t_literal).
:- use_module( t_fnc).
:- use_module( t_claus).
:- use_module( t_pred).
:- use_module( t_add).

:- lib( listut).


% ---------- Haupteinprungspunkt ----------

%%% g_translate_3
%%% Anstoss der Verarbeitung

g_translate_3 :-
	l_get_pred_flags( PFlagL),
	add_get_flags( AFlagL), 
	claus_next( Claus), 
	   l_gen_pred( PFlagL, Claus, Pred),
	      add_code( AFlagL, Pred, OutPred),
	      pred_set( OutPred),
        fail.

g_translate_3 :- u_out("."), !.


%%% l_get_pred_flags( FlagL)
%%% Ermittelt Flags fuer die Predgenerierung

:- mode l_get_pred_flags( -).

l_get_pred_flags( [Calculus,SelectionFunction,ThRegularity,Ancs,StrictRME,
                   AncestryRME,ThNoNewClauses,SimDynamic,MaxRewCond,Rewrite]) :-
	protein_flag( calculus, Calculus),
	protein_flag( selection_function, SelectionFunction),
	protein_flag( th_regularity, ThRegularity), 
	protein_flag( ancs, Ancs),
	protein_flag( strict_rme, StrictRME), 
	protein_flag( ancestry_rme, AncestryRME),
	protein_flag( th_nonewclauses, ThNoNewClauses),
	protein_flag( sim_dynamic, SimDynamic), 
	protein_flag( max_rew_cond, MaxRewCond), 
	(protein_flag( rewrite, flat) -> Rewrite = flat ; Rewrite = rew), !.


% ---------- Generierung der preds ----------

%%% l_gen_pred( FlagL, Claus, Pred)
%%% Umwandeln von Klauseln in MEHRERE Preds
%%% Typen : ext, fact, query, restart, th_fact, th_start, th_start_1

:- mode l_gen_pred( ++, +, -).

% Theorien: th_fact, th_start, th_start_1
l_gen_pred( FlagL, Claus, Pred) :-
	claus_test( theory, Claus),
	claus_struct( ClausN, Type, Costs, [PremL,ConcL,GuardL], Claus), !,
	u_nth( PremL, 1, LitN, Lit, PremR),
	\+ def_test_special_lit( Lit),
        once( l_th_gen( FlagL, Lit, LitN, PremR, ConcL, GuardL, 
                        ClausN, Type, Costs, Pred)).

% Simplifizierungsregeln: sim(_,_,_)
l_gen_pred( FlagL, Claus, Pred) :- 
	claus_test( simplify, Claus), !,
	claus_struct( ClausN, Type, Costs, [CondL,SimLit,RewriteL], Claus), !,
	l_sim_gen( FlagL, SimLit, CondL, RewriteL, ClausN, Type, Costs, Pred).  

% keine Kontrapositive: query, restart
l_gen_pred( _, Claus, Pred) :-
	claus_test( nocontrapos, Claus), !,
	claus_struct( ClausN, Type, Costs, [Head|BodyL], Claus),
	maplist( lit_neg, BodyL, NegBodyL), 
        pred_gen( Head, NegBodyL, ClausN, Type, Costs, 1, Pred), !.

% ME: Bilden aller Kontrapositive: fact, ext
l_gen_pred( [me|_], Claus, Pred) :-
	claus_struct( ClausN, Type, Costs, LitL, Claus),
	u_nth( LitL, 1, LitN, Head, BodyL),
	\+ def_test_special_lit( Head),
	maplist( lit_neg, BodyL, NegBodyL),
	pred_gen( Head, NegBodyL, ClausN, Type, Costs, LitN, Pred).

% RME: selection_function, on: das erste Positive Literal als Kopf: fact, ext
l_gen_pred( [rme,on|_], Claus, Pred) :-
	claus_struct( ClausN, Type, Costs, LitL, Claus),
	u_nth( LitL, 1, LitN, Head, BodyL),
	\+ lit_test_neg( Head),
	\+ def_test_special_lit( Head), !,
	maplist( lit_neg, BodyL, NegBodyL),
        pred_gen( Head, NegBodyL, ClausN, Type, Costs, LitN, Pred), !.

% RME: Kontrapositive von positiven Literalen: fact, ext
l_gen_pred( [rme,off|_], Claus, Pred) :- 
	claus_struct( ClausN, Type, Costs, LitL, Claus),
	u_nth( LitL, 1, LitN, Head, BodyL),
	\+ lit_test_neg( Head),
	\+ def_test_special_lit( Head),
	maplist( lit_neg, BodyL, NegBodyL),	
	pred_gen( Head, NegBodyL, ClausN, Type, Costs, LitN, Pred).


% ---------- Simplifizierungsregeln -----------

%%% l_sim_gen( FlagL, SimLit, CondL, RewriteL, ClausN, Type, Costs, Pred)
%%% Es werden folgende Umformungen vorgenommen:
%%% [C1,...,Cn],[K],[L1,...,Lm] zu
%%% sim_dis: not_K :- theory(Flag,[C1,...,Cn],ThAncs), L1,..., Lm.
%%%
%%% theory/3 wie unten

:- mode l_sim_gen( ++, +, +, +, ++, +, ++, -).

l_sim_gen( [_,_,_,_,_,_,_,off|_], _,_,_,_,_,_,_) :- !, fail.

% Rewrite-Regeln
l_sim_gen( [_,_,_,_,_,_,ThNoNewClauses,SimDynamic,MaxRewCond,Rewrite|_],
           [SimTerm], CondL, RewriteL, ClausN, sim(rew,-,KZ), Costs, Pred) :- !,
	length( CondL, N),
	N =< MaxRewCond,
	(ThNoNewClauses == on -> Flag = n ; Flag = y),
	(( KZ == c ;
	  (KZ == default, SimDynamic == cut) ;
          (KZ == m , CondL == []) ; 
	  (KZ == default, SimDynamic == mixed, CondL == [])) -> 
           CutL = [!] ; CutL = []),
	fnc_next( Fnc), 
           fnc_struct( F, A, Fnc),
	   \+ def_test_special_lit( F, A),
	   A > 0,
           functor( Head, F, A),
	   functor( OutHead, F, A),
	   maplist( lit_neg, CondL, NotCondL),
	   append( [p_rewrite( Rewrite, SimTerm, RewriteL, NotCondL, Head, 
                            [OutHead], OutCondL),
	            p_call( Head \== OutHead),
		    theory( Flag, OutCondL, _) | CutL],
	           [OutHead], 
                   BodyL),
	   pred_gen( Head, BodyL, ClausN, sim(rew,-), Costs, 0, Pred).

% disjunktive Replacement-Regeln
l_sim_gen( [_,_,_,_,_,_,ThNoNewClauses,SimDynamic|_], 
           [SimLit], CondL, RewriteL, ClausN, sim(Ty,dis,KZ), Costs, Pred) :- !,
	maplist( lit_neg, [SimLit|CondL], [NotSimLit|NotCondL]),
	maplist( lit_neg, RewriteL, NotRewriteL),
        (ThNoNewClauses == on -> Flag = n ; Flag = y),
	(( KZ == c ;
	  (KZ == default, SimDynamic == cut) ;
          (KZ == m , CondL == []) ; 
	  (KZ == default, SimDynamic == mixed, CondL == [])) -> 
           CutL = [!] ; CutL = []),
	(CondL == [] -> 
	    append( CutL, NotRewriteL, BodyL)
        ;
	    append( [theory(Flag,NotCondL,_)|CutL], NotRewriteL, BodyL)),
	pred_gen( NotSimLit, BodyL, ClausN, sim(Ty,dis), Costs, 0, Pred).

% konjunktive Replacement-Regeln
l_sim_gen( [_,_,_,_,_,_,ThNoNewClauses,SimDynamic|_],
	   [SimLit], CondL, [], ClausN, sim(Ty,con,KZ), Costs, Pred) :-
	( KZ == c ; 
         (KZ == default, SimDynamic == cut) ;
         (KZ == m, CondL == []) ;
         (KZ == default, SimDynamic == mixed, CondL == [])), !,
	maplist( lit_neg, [SimLit|CondL], [NotSimLit|NotCondL]),
        (ThNoNewClauses == on -> Flag = n ; Flag = y),
	(NotCondL == [] ->
	    BodyL = [!,fail]
        ;
	    BodyL = [theory(Flag,NotCondL,_), !, fail]),
	pred_gen( NotSimLit, BodyL, ClausN, sim(Ty,con), Costs, 0, Pred).

l_sim_gen( _, _, _, [], _, sim(_,con,_), _, _) :- !, fail.

l_sim_gen( [_,_,_,_,_,_,ThNoNewClauses,SimDynamic|_], 
           [SimLit], CondL, RewriteL, ClausN, sim(Ty,con,KZ), Costs, Pred) :- !,
	maplist( lit_neg, [SimLit|CondL], [NotSimLit|NotCondL]),
	maplist( lit_neg, RewriteL, NotRewriteL),
	u_makelist_semicolon( RewriteTerm, NotRewriteL),
        (ThNoNewClauses == on -> Flag = n ; Flag = y),
	(( KZ == c ;
	  (KZ == default, SimDynamic == cut) ;
          (KZ == m , CondL == []) ; 
	  (KZ == default, SimDynamic == mixed, CondL == [])) -> 
           CutL = [!] ; CutL = []),
	(NotCondL == [] ->
	    append( CutL, [none,RewriteTerm], BodyL)
        ;
	    append( [theory(Flag,NotCondL,_)|CutL], [none,RewriteTerm], BodyL)),
	pred_gen( NotSimLit, BodyL, ClausN, sim(Ty,con), Costs, 0, Pred),
	pred_get( body, Pred, LongBodyL),
	append( _, [LongNone,LongRewriteTerm], LongBodyL),
	u_makelist_semicolon( LongRewriteTerm, LongRewriteL),
	lit_name( [LongNone,LongRewriteL]).

l_sim_gen( _, _, _, _, _, _, _, _) :- !, fail.


% ---------- Theorie Preds ----------

%%% l_th_gen( FlagL, Lit, LitN, PremR, ConcL, GuardL, ClausN, Type, Costs, Pred)
%%% Es werden folgende Umformungen vorgenommen :
%%% [[P1,...Pn],[C1,...,Cm],[G]] zu  
%%% not_P1 :- theory( Flag, [P2,...,Pn], ThAncs),
%%%           G, not_C1,...,not_Cm.
%%% ...
%%% not_Pn :- theory( Flag, [P1,...,Pn-1], ThAncs),
%%%              G, not_C1,...,not_Cm.
%%%    
%%% theory(...) hat 3 Parameter:
%%% - newclauses Kennzeichen
%%% - restliche noch abzuarbeitende Praemissenliste
%%% - Ancestorlisten fuer positive und negative Ancestors
%%%   darin werden die Ancestors wie sie am Ende der Praemissenabarbeitung 
%%%   sind gespeichert

:- mode l_th_gen( ++, +, ++, +, +, +, ++, ++, ++, -).

% strict_rme: keine Schritte mit negativen Head-Literalen  
l_th_gen( [rme,_,_,_,on|_], Lit, _, _, _, _, _, _, _, _) :-
	\+ lit_test_neg( Lit), !, fail.

% non_strict_rme: keine Schritte mit negativen Head-Literalen falls kein 
%                 Theoriereduktionsschritt vorliegt
l_th_gen( [rme|_], Lit, _, _, ConcL, _, _, _, _, _) :-
	ConcL \== [], 
	\+ lit_test_neg( Lit), !, fail.

% Flag th_nonewclauses on, Reduktionsschritt ohne Guard
l_th_gen( [rme,_,_,_,_,_,on|_], Lit,LitN,PremR,[],[],ClausN,Type,Costs,Pred) :- 
	\+ lit_test_neg( Lit), !, 
	lit_neg( Lit, NotLit),
        pred_gen( NotLit, [theory(n,PremR,_)], ClausN, Type, Costs, LitN, Pred).

% Flag th_nonewclauses on, Reduktionsschritt mit Guard
l_th_gen( [rme,_,_,_,_,_,on|_], Lit, LitN, PremR, [], [Guard], 
	  ClausN, Type, Costs, Pred) :- 
	\+ lit_test_neg( Lit), !, 
	lit_neg( Lit, NotLit),
        pred_gen( NotLit, [theory(n,PremR,_),Guard], 
                  ClausN, Type, Costs, LitN, Pred).

% Reduktionsschritt ohne Guard
l_th_gen( _, Lit, LitN, PremR, [], [], ClausN, Type, Costs, Pred) :- !,
	lit_neg( Lit, NotLit),
	pred_gen( NotLit, [theory(y,PremR,_)], ClausN, Type, Costs, LitN, Pred).

% Reduktionsschritt mit Guard
l_th_gen( _, Lit, LitN, PremR, [], [Guard], ClausN, Type, Costs, Pred) :- !,
	lit_neg( Lit, NotLit),
	pred_gen( NotLit,[theory(y,PremR,_),Guard], 
                  ClausN, Type, Costs, LitN, Pred).

% Extentionsschritt ohne Guard mit Regularitaetstest
l_th_gen( FlagL, Lit, LitN, PremR, ConcL, [], ClausN, Type, Costs, Pred) :- !,
	maplist( lit_neg, ConcL, NotConcL),
	maplist( l_gen_threg(FlagL,PosAnc,NegAnc), NotConcL, RegL),
	flatten( [RegL,[theory(y,PremR,_)],NotConcL], BodyL),
	lit_neg( Lit, NotLit),
	pred_gen( NotLit, BodyL, ClausN, Type, Costs, LitN, Pred),
	pred_get( head, Pred, Head),
	lit_name( ancs, (PosAnc,NegAnc), Head).

% Extentionsschritt mit Guard mit Regularitaetstest
l_th_gen( FlagL, Lit, LitN, PremR, ConcL, GuardL, ClausN, Type,Costs, Pred):- !,
	maplist( lit_neg, ConcL, NotConcL),
	maplist( l_gen_threg(FlagL,PosAnc,NegAnc), NotConcL, RegL),
	flatten( [RegL,[theory(y,PremR,_)],GuardL,NotConcL], BodyL),
	lit_neg( Lit, NotLit),
	pred_gen( NotLit, BodyL, ClausN, Type, Costs, LitN, Pred),
	pred_get( head, Pred, Head),
	lit_name( ancs, (PosAnc,NegAnc), Head).


%%% l_gen_threg( FlagL, PosAnc, NegAnc, Lit, RegL)
%%% Durchfuehrung des Regularitaetstests

:- mode l_gen_threg( ++, ?, ?, +, -).

l_gen_threg( FlagL, PosAnc, NegAnc, Lit, RegL) :-
	(lit_test_neg(Lit) ->
	    Anc = NegAnc,
            PosNeg = neg
	;
	    Anc = PosAnc,
            PosNeg = pos),
	l_do_gen_threg( FlagL, Anc, PosNeg, Lit, RegL), !.


%%% l_do_gen_threg( FlagL, Anc, PosNeg, Lit, RegL)
%%% Ermittlung des Codes fuer den Regularitaetstest

:- mode l_do_gen_threg( ++, +, ?, +, -).

l_do_gen_threg( [_,_,off|_],      _,   _, _, []).
l_do_gen_threg( [_,_,_,   off|_], _,   _, _, []).
l_do_gen_threg( [_,_,_,   pos|_], _, neg, _, []).
l_do_gen_threg( [_,_,_,pos_dj|_], _, neg, _, []).
l_do_gen_threg( [_,_,_,   neg|_], _, pos, _, []).

% delayed version
l_do_gen_threg( [Calculus,_,delay,_,_,AncestryRME|_], Anc, PosNeg, Lit, 
	       [p_never_identical( Lit, Anc)]) :-
        (Calculus == me ; PosNeg == neg ; AncestryRME == on).

l_do_gen_threg( [_,_,delay|_], Anc, _, Lit, 
               [p_never_identical_upto_restart( Lit, Anc)]).

% not delayed version
l_do_gen_threg( [Calculus,_,nodelay,_,_,AncestryRME|_], Anc, PosNeg, Lit, 
	       [p_call(\+ p_identical(Lit,Anc))]) :-
        (Calculus == me ; PosNeg == neg ; AncestryRME == on).

l_do_gen_threg( [_,_,nodelay|_], Anc, _, Lit, 
               [p_call(\+ p_identical_upto_restart( Lit, Anc))]).


