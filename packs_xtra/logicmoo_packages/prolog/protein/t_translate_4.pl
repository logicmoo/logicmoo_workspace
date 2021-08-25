%********************    t_translate_4   ********************

% Vierter Teil der Compilation: Generierung zusaetzlicher Preds
% - Theorieextensionspreds zu allen Extensionsschritten generieren.
% - Preds zum Test fuer die Reduktion, Reduktion im Grundfall, Regularitaet
%   und Faktorisierung generieren
% - weitere zusaetzliche Preds
% - Zusaetzlicher notwendiger Prologcode wird generiert.

%************************************************************

:- module_interface( t_translate_4).

:- export g_translate_4 / 0.

:- begin_module( t_translate_4).

:- use_module( t_utils).
:- use_module( t_fnc).
:- use_module( t_definitions).
:- use_module( t_literal).
:- use_module( t_pred).
:- use_module( t_add).


%%% g_translate_4
%%% Anstoss der Verarbeitung

g_translate_4 :-
	l_gen_th_ext_preds,
	l_del_preds,
	l_gen_test_preds,
	l_gen_extra_preds,
	l_utility_code,
	u_out( "."), !.


% ---------- Theorieextensionen ----------

%%% l_gen_th_ext_preds
%%% Theorieextensionen generieren

l_gen_th_ext_preds :-
	once(
          def_const( theory, yes) ;
	  (def_const( simplify, yes), \+ protein_flag( sim_dynamic,off))),
	add_get_flags( AFlagL),
        pred_next( Pred),
           \+ pred_test( nocontrapos, Pred),
	   l_gen_th_ext_pred( Pred, ThPred),
	   add_code( AFlagL, ThPred, OutPred),
	   pred_set( OutPred),
	fail.

l_gen_th_ext_preds.


%%% l_gen_th_ext_pred( Pred, ThPred)
%%% Generiert zu allen normalen Schritten Theorieschritte der Form:
%%%    H :- B1, ..., Bn zu theory(y,[H|R],_) :- theory(y,[R],_), B1, ..., Bn.

:- mode l_gen_th_ext_pred( +, -).

l_gen_th_ext_pred( Pred, ThPred) :-
	pred_get( litL, Pred, [LongHead|BodyL]),
	pred_get( clausN, Pred, ClausN),
	pred_get( litN, Pred, LitN),
	pred_get( costs, Pred, (PredCost,PredPlus)),
	def_get( costs, th_ext, (ThExtCost,ThExtPlus)),
	lit_strip( LongHead, ShortHead),
	u_splitlist( def_test_prolog_lit, BodyL, _, LongBodyL),
	lit_strip( LongBodyL, ShortBodyL),
	(PredCost = 0 -> Cost = 0 ; Cost is PredCost + ThExtCost),
	Plus is PredPlus + ThExtPlus,
	pred_gen( theory(y,[ShortHead|Rest],ThAncs), 
	          [theory(y,Rest,ThAncs)|ShortBodyL], 
		  ClausN, th_ext, (Cost,Plus), LitN, ThPred).


% ---------- Loeschen unerwuenschter Preds ----------

%%% l_del_ext_preds
%%% Extensionsschritte loeschen

l_del_preds :-
	protein_flag( translate, plain_theory),
	pred_next( Pred),
	   pred_get( type, Pred, Type),
           Type \== query,
	   def_get( group, Type, ext),
	   pred_del( Pred),
        fail.

l_del_preds.


% ---------- Test Preds ----------

%%% l_gen_test_preds
%%% Tests generieren

l_gen_test_preds :-
	l_get_test_flags( TFlagL), 
	add_get_flags( AFlagL),
	fnc_next( Fnc),
	   fnc_struct( F, A, Fnc),
	   \+ def_test_special_lit( F, A),
	   functor( Lit, F, A),
	   lit_neg( Lit, NotLit),
	   (lit_test_neg_fnc( F) -> PosNeg = neg ; PosNeg = pos),
	   l_gen_test_pred( PosNeg, TFlagL, Lit, NotLit, Pred),
              add_code( AFlagL, Pred, OutPred),
	      pred_set( OutPred),
        fail.

l_gen_test_preds.


%%% l_get_test_flags( FlagL)
%%% Ermittlung der fuer die Tests relevanten Flags

:- mode l_get_test_flags( -).

l_get_test_flags( [Calculus,Regularity,Reduction,Factorisation,	          
                   Ancs,StrictRME,AncestryRME,Count]) :-
	protein_flag( calculus, Calculus),
	protein_flag( regularity, Regularity),
	protein_flag( reduction, Reduction),
	protein_flag( factorisation, Factorisation),
	protein_flag( ancs, Ancs),
	protein_flag( strict_rme, StrictRME),
	protein_flag( ancestry_rme, AncestryRME),
	protein_flag( count, Count), !.


%%% l_gen_test_pred( PosNeg, FlagL, Lit, NotLit, Pred).
%%% Generiert zu Funktor der Testpraedikate fuer die
%%% Reduktion, Reduktion im Grundfall, Regularitaet und Faktorisierung

:- mode l_gen_test_pred( ++, ++, +, +, -). 

l_gen_test_pred( neg, FlagL, Lit, _, Pred) :-
	l_test_reg( FlagL, Lit, NegAnc, (_,NegAnc), neg, Pred).

l_gen_test_pred( pos, FlagL, Lit, _, Pred) :-
        l_test_reg( FlagL, Lit, PosAnc, (PosAnc,_), pos, Pred).

l_gen_test_pred( neg, FlagL, Lit, NotLit, Pred) :-
        l_test_red( FlagL, Lit, NotLit, PosAnc, (PosAnc,_), neg, Pred).

l_gen_test_pred( pos, FlagL, Lit, NotLit, Pred) :-
	l_test_red( FlagL, Lit, NotLit, NegAnc, (_,NegAnc), pos, Pred).

l_gen_test_pred( neg, FlagL, Lit, NotLit, Pred) :-
        l_test_red_cut( FlagL, Lit, NotLit, PosAnc, (PosAnc,_), neg, Pred).

l_gen_test_pred( pos, FlagL, Lit, NotLit, Pred) :-
        l_test_red_cut( FlagL, Lit, NotLit, NegAnc, (_,NegAnc), pos, Pred).

l_gen_test_pred( neg, FlagL, Lit, _, Pred) :-
        l_test_factor( FlagL, Lit, NegAunt, (_,NegAunt), Pred).

l_gen_test_pred( pos, FlagL, Lit, _, Pred) :-
	l_test_factor( FlagL, Lit, PosAunt, (PosAunt,_), Pred).
     
l_gen_test_pred( neg, FlagL, Lit, _, Pred) :-
       	 l_test_factor_cut( FlagL, Lit, NegAunt, (_,NegAunt), Pred).

l_gen_test_pred( pos, FlagL, Lit, _, Pred) :-
     	 l_test_factor_cut( FlagL, Lit, PosAunt, (PosAunt,_), Pred).


%%% l_test_reg( FlagL, Head, Anc, AncT, PosNeg, PredL)
%%% Regularitaet und delayed und not delayed version, Type: reg

:- mode l_test_reg( ++, +, ?, +, ++, -).

l_test_reg( [_,off|_],            _, _, _,   _, _) :- !, fail.
l_test_reg( [_,_,_,_,off|_],      _, _, _,   _, _) :- !, fail.
l_test_reg( [_,_,_,_,pos|_],      _, _, _, pos, _) :- !, fail.
l_test_reg( [_,_,_,_,pos_dj|_],   _, _, _, pos, _) :- !, fail.
l_test_reg( [_,_,_,_,neg|_],      _, _, _, neg, _) :- !, fail.

% delayed version
l_test_reg( [Calculus,delay,_,_,_,_,AncestryRME|_], 
	    Head, Anc, Ancs, PosNeg, Pred) :- !,  
	((Calculus == me ; PosNeg == neg ; AncestryRME == on) ->
	    NeverIdentCall = p_never_identical(Head,Anc)
	;
	    NeverIdentCall = p_never_identical_upto_restart(Head,Anc)),
	pred_gen( Head, [!,NeverIdentCall,Head], 0, reg, default, 0, Pred),
	pred_get( litL, Pred, LitL), 
	lit_name( ancs, Ancs, LitL).

% not delayed version
l_test_reg( [Calculus,_,_,_,_,_,AncestryRME|_], 
	    Head, Anc, Ancs, PosNeg, Pred) :-
        ((Calculus = me ; PosNeg = neg ; AncestryRME = on) ->
	    IdentCall = p_identical(Head,Anc)
	;
	    IdentCall = p_identical_upto_restart(Head,Anc)),
        pred_gen( Head, [IdentCall,!,fail], 0, reg, default, 0, Pred),
        pred_get( head, Pred, LongHead), 
	lit_name( ancs, Ancs, LongHead).


%%% l_test_red( FlagL, Head, NoHead, Anc, AncT, PosNeg, PredL)
%%% Reduktion, Type: red

:- mode l_test_red( ++, +, +, ?, +, ++, -).

l_test_red( [_,_,off|_],        _, _, _, _,   _, _) :- !, fail.
l_test_red( [_,_,cut|_],        _, _, _, _,   _, _) :- !, fail.
l_test_red( [_,_,_,_,off|_],    _, _, _, _,   _, _) :- !, fail.
l_test_red( [_,_,_,_,pos|_],    _, _, _, _, neg, _) :- !, fail.
l_test_red( [_,_,_,_,pos_dj|_], _, _, _, _, neg, _) :- !, fail.
l_test_red( [_,_,_,_,neg|_],    _, _, _, _, pos, _) :- !, fail.
l_test_red( [rme,_,_,_,_,on|_], _, _, _, _, neg, _) :- !, fail.

l_test_red( [_,_,_,_,_,_,_,Count|_], Head, NotHead, Anc, Ancs, _, Pred) :-
	(Count == on ->
	    pred_gen( Head, [p_unifiable_n_count(NotHead,Anc,N)], 
	              0, red, default, N, Pred)
        ;
	    pred_gen( Head, [p_unifiable(NotHead,Anc)], 
	              0, red, default, 0, Pred)),
	pred_get( head, Pred, LongHead), 
        lit_name( ancs, Ancs, LongHead).  


%%% l_test_red_cut( FlagL, Head, NotHead, Anc, AncT, PosNeg, PredL) 
%%% Reduktion im Grundfall, Type: red_cut

:- mode l_test_red_cut( ++, +, +, ?, +, ++, -).

l_test_red_cut( [_,_,off|_],        _, _, _, _,   _, _) :- !, fail.
l_test_red_cut( [_,_,nocut|_],      _, _, _, _,   _, _) :- !, fail.
l_test_red_cut( [_,_,_,_,off|_],    _, _, _, _,   _, _) :- !, fail.
l_test_red_cut( [_,_,_,_,pos|_],    _, _, _, _, neg, _) :- !, fail.
l_test_red_cut( [_,_,_,_,pos_dj|_], _, _, _, _, neg, _) :- !, fail.
l_test_red_cut( [_,_,_,_,neg|_],    _, _, _, _, pos, _) :- !, fail.
l_test_red_cut( [rme,_,_,_,_,on|_], _, _, _, _, neg, _) :- !, fail.

l_test_red_cut( [_,_,_,_,_,_,_,Count|_], Head, NotHead, Anc, Ancs, _, Pred) :-
	(Count == on ->
	    pred_gen( Head, [p_identical_n_count(NotHead,Anc,N),!], 
	              0, red_cut, default, N, Pred)
        ;
	    pred_gen( Head, [p_identical(NotHead,Anc),!], 
                      0, red_cut, default, 0, Pred)),
 	pred_get( head, Pred, LongHead),
        lit_name( ancs, Ancs, LongHead).
	

%%% l_test_factor( FlagL, Head, Aunt, AuntT, PredL)
%%% Faktorisierung, Type: factor

:- mode l_test_factor( ++, +, ?, +, -).

l_test_factor( [_,_,_,off|_], _, _, _, _) :- !, fail.
l_test_factor( [_,_,_,cut|_], _, _, _, _) :- !, fail.

l_test_factor( [_,_,_,_,_,_,_,Count|_], Head, Aunt, Aunts, Pred) :-
	(Count == on ->
	    pred_gen( Head, [p_unifiable_n_count(Head,Aunt,N)], 
	              0, factor, default, N, Pred)
        ;
	    pred_gen( Head, [p_unifiable(Head,Aunt)], 
	              0, factor, default, 0, Pred)),
        pred_get( head, Pred, LongHead),
	lit_name( aunts, Aunts, LongHead).


%%% l_test_factor_cut( FlagL, Head, Aunt, AuntT, PredL) 
%%% Faktorisierung im Grundfall, Type: factor_cut

:- mode l_test_factor_cut( ++, +, ?, +, -).

l_test_factor_cut( [_,_,_,off|_], _, _, _, _) :- !, fail.
l_test_factor_cut( [_,_,_,nocut|_], _, _, _, _) :- !, fail.

l_test_factor_cut( [_,_,_,_,_,_,_,Count|_], Head, Aunt, Aunts, Pred) :-
	(Count == on ->
	    pred_gen( Head, [p_identical_n_count(Head,Aunt,N),!], 
	              0, factor_cut, default, N, Pred)
        ;
	    pred_gen( Head, [p_identical(Head,Aunt),!], 
                      0, factor_cut, default, 0, Pred)),
	pred_get( head, Pred, LongHead),
	lit_name( aunts, Aunts, LongHead).


% ---------- Extra Preds ----------

%%% l_gen_extra_preds
%%% einzelne zusaetzliche Praedikate

l_gen_extra_preds :-
	add_get_flags( AFlagL),
	l_gen_extra_pred( Pred),
	   add_code( AFlagL, Pred, OutPred),
	   pred_set( OutPred),
	fail.

l_gen_extra_preds.


%%% l_gen_extra_pred( ExtraPred)
%%% Generierung von Extrapraedikaten fuer bestimmte Kalkuelvarianten

:- mode l_gen_extra_pred( -).

% Ausfuehrung von Prologpraedikaten in Praemissen
l_gen_extra_pred( Pred) :-
	once( 
           def_const( theory, yes) ;
	   (def_const( simplify, yes), \+ protein_flag( sim_dynamic,off))),
	pred_gen( theory(y,[protein_prolog(Call)|Rest],ThAncs),
	          [protein_prolog(Call),theory(y,Rest,ThAncs)], 
		  0, th_prolog, default, 0, Pred).

% Theoriereduktion
l_gen_extra_pred( Pred) :- 
        once(
	   def_const( theory, yes) ;
	   (def_const( simplify, yes), \+ protein_flag( sim_dynamic,off))),
	protein_flag( th_reduction, ThReduction),
	(ThReduction == both ; ThReduction == cut),
	protein_flag( ancs, AncsFlag),
	AncsFlag \== off,
	((AncsFlag == pos ; AncsFlag == pos_dj) -> PosNeg = pos; true),
	( AncsFlag == neg                       -> PosNeg = neg; true),
	( var(PosNeg) ->
	    Test = p_call((PosNeg == pos -> TestPos ; TestNeg))
        ;
	    (PosNeg == pos -> Test = TestPos ; Test = TestNeg)),
	BodyL = [p_neg_shortlit(PosNeg,Lit,NotLit), Test, !,
                 theory(Flag,PremR,ThAncs)],
	(protein_flag( count, off) ->
	    TestPos = p_identical(NotLit,NegAnc),
	    TestNeg = p_identical(NotLit,PosAnc),
	    pred_gen( theory(Flag,[Lit|PremR],ThAncs), BodyL, 
                      0, th_red_cut, default, 0, Pred)
        ;
            TestPos = p_identical_n_count(NotLit,NegAnc,N),
	    TestNeg = p_identical_n_count(NotLit,PosAnc,N),
            pred_gen( theory(Flag,[Lit|PremR],ThAncs), BodyL, 
                      0, th_red_cut, default, N, Pred)),
	pred_get( litL, Pred, [Head|LongBodyL]),
	append( _, [LongThCall], LongBodyL),
	lit_name( ancs, (PosAnc,NegAnc), Head),
	lit_name( ancs, (PosAnc,NegAnc), LongThCall).

l_gen_extra_pred( Pred) :- 
        once(
	   def_const( theory, yes) ;
	   (def_const( simplify, yes), \+ protein_flag( sim_dynamic,off))),
	protein_flag( th_reduction, ThReduction),
	(ThReduction == both ; ThReduction == nocut),
	protein_flag( ancs, AncsFlag),
	AncsFlag \== off,
	((AncsFlag == pos ; AncsFlag == pos_dj) -> PosNeg = pos; true),
	( AncsFlag == neg                       -> PosNeg = neg; true),
	( var(PosNeg) ->
	    Test = p_call((PosNeg == pos -> TestPos ; TestNeg))
        ;
	    (PosNeg == pos -> Test = TestPos ; Test = TestNeg)),
	BodyL = [p_neg_shortlit(PosNeg,Lit,NotLit), Test, 
                 theory(Flag,PremR,ThAncs)],
        (protein_flag( count, off) ->
	    TestPos = p_unifiable(NotLit,NegAnc),
	    TestNeg = p_unifiable(NotLit,PosAnc),
	    pred_gen( theory(Flag,[Lit|PremR],ThAncs), BodyL, 
	              0, th_red, default, 0, Pred)
        ;
            TestPos = p_unifiable_n_count(NotLit,NegAnc,N),
	    TestNeg = p_unifiable_n_count(NotLit,PosAnc,N),
            pred_gen( theory(Flag,[Lit|PremR],ThAncs), BodyL, 
                      0, th_red, default, N, Pred)),
	pred_get( litL, Pred, [Head|LongBodyL]),
	append( _, [LongThCall], LongBodyL),
	lit_name( ancs, (PosAnc,NegAnc), Head),
	lit_name( ancs, (PosAnc,NegAnc), LongThCall).

% Theorieende
l_gen_extra_pred( Pred) :-
        once(
	   def_const( theory, yes) ;
	   (def_const( simplify, yes), \+ protein_flag( sim_dynamic,off))),
	pred_gen( theory(_,[],ThAncs), [], 0, th_end, default, 0, Pred),
	pred_get( head, Pred, Head),
	lit_name( ancs, ThAncs, Head).

% Ausfuehrung von Prologpraedikaten in Klauseln
l_gen_extra_pred( Pred) :- 
	pred_gen( protein_prolog( Call), [Call], 
	          0, prolog, default, 0, Pred).

% Zugriff auf Zusatzargumente
l_gen_extra_pred( Pred) :-
	def_get( use, ZArg, y),
	pred_gen( protein_zarg( ZArg, Arg), [], 0, prolog, default, 0, Pred),
	pred_get( head, Pred, Head),
	lit_name( ZArg, Arg, Head).

% ancestry restart
l_gen_extra_pred( RestartPred) :-
	once( protein_flag( calculus, rme)),
        once( protein_flag( ancestry_rme, on)),
  	functor( ShortHead, restart, 0),
        ZwiBodyL = [none,
                 p_call( member( ShortNegLit, PosAnc)),
	         p_call( ShortNegLit \== restart),
                 p_call( ShortNegLit \== query), 
	         p_call( ShortNegLit =.. ShortNegLitL),
	         p_call( append( ShortNegLitL, ZArgL, LongNegLitL)),
	         p_call( LongNegLit =.. LongNegLitL),
	         LongNegLit],
        (protein_flag( trace, internal) ->
	    append( ZwiBodyL,
                    [p_call( append( L, [ShortNegLit|_], PosAnc)),
	             p_call( length( [_|L], N))], 
                    BodyL)
        ;
            BodyL = ZwiBodyL,
            N = 0),  
	pred_gen( ShortHead, BodyL, N, query, default, 0, ZwiPred),
	pred_get( prio, ZwiPred, Prio),
	Prio1 is Prio - 1,
	pred_chg( prio, ZwiPred, Prio1, YwiPred),
	pred_chg( class, YwiPred, test, RestartPred),
	pred_get( litL, RestartPred, [LongHead,LongNone|_]),
	LongNone =.. [_|ZArgL],
	lit_name( ancs, (PosAnc,_), LongHead).

% delayed Restart
l_gen_extra_pred( RestartPred) :-
	once( protein_flag( mmr, MMR)),
	(MMR == gcwa ; MMR == model),
	pred_gen( restart, [p_call((protein_result(_) -> true;
                                      assert(protein_result(fail))))],
                  0, query, default, 0, ZwiPred),
        pred_get( prio, ZwiPred, Prio),
	Prio1 is Prio + 1,
	pred_chg( prio, ZwiPred, Prio1, YwiPred),
	pred_chg( class, YwiPred, last, RestartPred).

l_gen_extra_pred( Pred) :- 
	once(protein_flag( search, id_term)),
	pred_gen( any, [any], 0, inc, default, 0, Pred).


% ---------- Utility-Code ----------

%%% l_utility_code
%%% - Generierung Hilfspraedikat p_neg_shortlit( PosNeg, ShortLit, NegShortLit)
%%%   pro FA-Paar
%%% - Generierung Dynamic-Deklaration pro FA-Paar

% Dynamic-Deklarationen

l_utility_code :-
	protein_flag( dynamic, on),
	def_const( header, HeaderF),
	u_open_file( prolog, append, HeaderF),
	fail.

l_utility_code :-
	protein_flag( dynamic, on),
	protein_flag( calculus, hyper),
	def_const( zarg, ZARG),
	u_writeclause( prolog, :- dynamic any / ZARG),
	fail.

l_utility_code :-	
	protein_flag( dynamic, on),
	\+ protein_flag( calculus, hyper),
	def_const( zarg, ZARG),
	fnc_next( Fnc),
	   l_do_dynamic( Fnc, ZARG),
        fail.

l_utility_code :-
	protein_flag( dynamic, on), 
        u_writeclause( prolog, :- export h_assert_or_retract / 2),
	u_writeclause( prolog, h_assert_or_retract(assert,X) :- asserta( X)),
	u_writeclause( prolog, h_assert_or_retract(retract,X) :- retract( X)),
        fail.

% p_neg_shortlit
l_utility_code :-
	protein_flag( dynamic, off),
	fnc_next( Fnc),
	   l_neg_shortlit( Fnc),
        fail.

l_utility_code :- u_close_file( prolog), fail.

l_utility_code :- !.
 

%%% l_neg_shortlit( Fnc)
%%% Bildet aus einem Element der FncL ein p_neg_shortlit Praedikat.

:- mode l_neg_shortlit( ++).

l_neg_shortlit( (F,A)) :- 
	functor( Lit, F, A),
	(lit_test_neg( Lit) -> PosNeg = neg ; PosNeg = pos),
	lit_neg( Lit, NotLit),
	u_writeclause( prolog, p_neg_shortlit(PosNeg,Lit,NotLit)), !.


%%% l_dynamic( Fnc, ZARG)
%%% Dynamic-Deklaration aller Funktoren

:- mode l_dynamic( ++, ++).

l_do_dynamic( (Fnc,Arity), ZARG) :-
	N is ZARG + Arity,
	u_writeclause( prolog, :- dynamic Fnc / N), !.
