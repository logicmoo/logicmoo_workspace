%********************      t_pred.pl     ********************

% Praedikate zur Verwaltung der Pred-Struktur

% Die Pred-Struktur : pred( Head, Bodylist, Attribute, LitN, FA)
% Head          : Kopf des Preds
% BodyList      : Liste der Bodyliterale des Preds
% Attribute     : Attribute des Preds (naeheres siehe unten)
% LitN          : Nummer des Literals aus der urspruenglichen Klausel,
%          	  das in diesem Pred den Head stellt.
% FA            : (Functor,Arity) Charakteristik des Heads
% Prio          : Prioritaet des Preds
% Die Literale In Head und BodyL haben immer Zusatzargumente.

%************************************************************

:- module_interface( t_pred).

:- export pred_chg / 4,
	  pred_del / 1,
          pred_gen / 7,
	  pred_get / 3,	 
	  pred_init / 0,
	  pred_next / 1, pred_next / 2,
	  pred_set / 1,
          pred_struct / 10,
	  pred_test / 2,
	  pred_make_static / 0.

:- dynamic pred / 1.
 
:- begin_module( t_pred).

:- use_module( t_utils).
:- use_module( t_definitions).
:- use_module( t_fnc).
:- use_module( t_literal).
:- use_module( t_claus).

          
% ---------- Verwaltung der Pred-Struktur ----------

%%% pred_struct( Head, BodyL, ClausN, Type, Costs, LitN, FA, Prio, Class, Pred)
%%% Definition der pred-Struktur

pred_struct( Head, BodyL, ClausN, Type, Costs, LitN, FA, Prio, Class,
	     (ClausN,Costs,Prio,FA,Class,Head,BodyL,Type,LitN)).


%%% pred_gen( ShortHead, ShortBodyL, ClausN, Type, Costs, LitN, LongPred)
%%% - ergaenzt Literale in Head und Body um Zusatzargumente
%%% - Ermittlung der Kosten, F/A-Paar, Prioritaet
%%% - generiert Pred-Struktur

:- mode pred_gen( +, +, ++, ++, ++, ?, -).
	
pred_gen( ShortHead, ShortBodyL, ClausN, Type, InCosts, LitN, Pred) :-!,
        lit_add( ShortHead, LongHead),
	lit_add( ShortBodyL, LongBodyL),
	l_calc( costs, Type, ShortBodyL, InCosts, OutCosts),
	functor( ShortHead, F, A),
	l_calc( prio, Type, ShortBodyL, default, Prio),
	def_get( class, Type, Class),
	pred_struct( LongHead, LongBodyL, ClausN, Type, OutCosts, LitN, (F,A), 
                     Prio, Class, Pred),
        (Prio < 1 -> def_message( error, prio, Pred) ; true), !.


%%% l_calc( Flag, Type, BodyL, InValue, OutValue)
%%% Berechnet die Kosten bzw Prioritaet

:- mode l_calc( ++, ++, +, ++, -).

l_calc( costs, Type, BodyL, InCosts, (OutValue,Plus)) :-
	(InCosts == default -> 
	    def_get( costs, Type, (InValue,Plus)) ; InCosts = (InValue,Plus)),
	l_calc( length, Type, BodyL, InValue, OutValue).

l_calc( prio, Type, BodyL, InPrio, OutPrio) :-
	(InPrio == default ->
	    def_get( prio, Type, InValue) ; InValue = InPrio),
	l_calc( length, Type, BodyL, InValue, OutPrio).

l_calc( length, Type, BodyL, cl, Value) :- l_do_calc( Type, 0, BodyL, Value).
l_calc( length, Type, BodyL, cl+N, Value) :- l_do_calc( Type, N, BodyL, Value).
l_calc( length, _, _, Value, Value).


%%% l_do_calc( Type, N, BodyL, Value)
%%% berechnet Klausellaenge + N

:- mode l_do_calc( ++, ++, +, -).

l_do_calc( Type, N, BodyL, Value) :-
	(def_get( group, Type, th_start) ->
	    member( theory( _, ZwiLitL, _), BodyL),
	    append( ZwiLitL, BodyL, LitL)
        ;
	    LitL = BodyL),
	u_splitlist( def_test_special_lit, LitL, _, FalseL),
	length( FalseL, M),
	Value is M + N + 1.


%%% pred_get( Flag, Pred, Value)
%%% Liefert ein Argument der Pred-Struktur.

:- mode pred_get( ++, ?, ?).

pred_get( body,   Pred, V)       :- pred_struct(_,V,_,_,_,_,_,_,_,Pred).
pred_get( class,  Pred, V)       :- pred_struct(_,_,_,_,_,_,_,_,V,Pred).
pred_get( clausN, Pred, V)       :- pred_struct(_,_,V,_,_,_,_,_,_,Pred).
pred_get( cost,   Pred, V)       :- pred_struct(_,_,_,_,(V,_),_,_,_,_,Pred).
pred_get( costs,  Pred, V)       :- pred_struct(_,_,_,_,V,_,_,_,_,Pred).
pred_get( fa,     Pred, V)       :- pred_struct(_,_,_,_,_,_,V,_,_,Pred).
pred_get( head,   Pred, V)       :- pred_struct(V,_,_,_,_,_,_,_,_,Pred).
pred_get( litL,   Pred, [V1|V2]) :- pred_struct(V1,V2,_,_,_,_,_,_,_,Pred).
pred_get( litN,   Pred, V)       :- pred_struct(_,_,_,_,_,V,_,_,_,Pred).
pred_get( prio,   Pred, V)       :- pred_struct(_,_,_,_,_,_,_,V,_,Pred).
pred_get( type,   Pred, V)       :- pred_struct(_,_,_,V,_,_,_,_,_,Pred).

pred_get( theory, Pred, (Head,TestL,ThCall,PremL,ConcL)) :- 
	pred_test( theory, Pred),
	pred_get( litL, Pred, [Head|BodyL]),
	lit_analyze_th( BodyL, TestL, ThCall, PremL, ConcL).


%%% pred_chg( Flag, InPred, BodyL, OutPred)
%%% Tauscht den Arg eines Preds aus.

:- mode pred_chg( ++, +, +, -).

pred_chg( body, InPred, BodyL, OutPred) :-
	pred_struct( H,     _, CN, T, C, LN, FA, P, Cl, InPred),
	pred_struct( H, BodyL, CN, T, C, LN, FA, P, Cl, OutPred). 

pred_chg( prem, InPred, PremL, OutPred) :-
	pred_get( theory, InPred, (_,TestL,InThCall,_,ConcL)),
	lit_rpl_arg( InThCall, 2, PremL, OutThCall),
	append( TestL, [OutThCall|ConcL], BodyL),
	pred_chg( body, InPred, BodyL, OutPred).
	
pred_chg( conc, InPred, ConcL, OutPred) :-
	pred_get( theory, InPred, (_,TestL,ThCall,_,_)),
	append( TestL, [ThCall|ConcL], BodyL),
	pred_chg( body, InPred, BodyL, OutPred).

pred_chg( costs, InPred, Costs, OutPred) :-
	pred_struct( H, B, CN, T,     _, LN, FA, P, Cl, InPred),
	pred_struct( H, B, CN, T, Costs, LN, FA, P, Cl, OutPred).

pred_chg( prio, InPred, Prio, OutPred) :-
	pred_struct( H, B, CN, T, C, LN, FA,    _, Cl, InPred),
	pred_struct( H, B, CN, T, C, LN, FA, Prio, Cl, OutPred).

pred_chg( class, InPred, Class, OutPred) :-
	pred_struct( H, B, CN, T, C, LN, FA, P,     _, InPred),
	pred_struct( H, B, CN, T, C, LN, FA, P, Class, OutPred).


%%% pred_test( Flag, Pred)
%%% Test auf bestimmte Typklassen je nach Flag

:- mode pred_test( ++, +).

pred_test( disjunctive, Pred) :- !, 
	\+ pred_test( theory, Pred), 
	pred_get( litL, Pred, [Head|BodyL]),
	lit_neg( Head, NotHead), !, 
	u_splitlist( lit_test_neg, [NotHead|BodyL], [_,_|_], _).

pred_test( Flag, Pred) :- 
	pred_get( type, Pred, Type),
	claus_struct( default, Type, default, [], Claus), 
	claus_test( Flag, Claus).


%%% pred_init
%%% Loeschen aller pred-Praedikate
 
pred_init :- retract_all( pred(_)).


%%% pred_set( Pred)
%%% Assertieren des pred-Praedikats

:- mode pred_set( +).

pred_set( Pred) :- assert( pred(Pred)), !.


%%% pred_next( Pred)
%%% Liefert naechstes Pred

pred_next( Pred) :- pred( Pred).


%%% pred_del( Pred)
%%% Retractet Pred

pred_del( Pred) :- retract( pred(Pred)).


