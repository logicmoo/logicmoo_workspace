%********************    t_literal.pl    ********************

% Praedikate zur Literalmanipulation. 

%************************************************************

:- module_interface( t_literal).

:- export lit_add / 2,
	  lit_analyze_th / 5,
          lit_enclose / 4,
	  lit_get_ancs / 4,
	  lit_get_type / 2,
	  lit_name / 1,lit_name / 3,
	  lit_neg / 2,
	  lit_neg_fnc / 2,
	  lit_norm / 4,
	  lit_norm_one_lit / 2,
	  lit_rpl_arg / 4,
	  lit_set_aunts / 2,
	  lit_strip / 2,
	  lit_test_longlit / 1,
	  lit_test_neg / 1,
	  lit_test_neg_fnc / 1.

:- begin_module( t_literal).

:- use_module( t_definitions).
:- use_module( t_utils).
:- use_module( t_fnc).


% ---------- Literalmanipulation ----------

%%% lit_enclose( Flag, Type, InLitL, OutLitL)
%%% Markierung von Prologliteralen mit protein_prolog(Lit)

:- mode lit_enclose( ++, ?, +, -).

lit_enclose( prolog, pos, Lit, protein_prolog(Lit)) :-
	def_test_prolog_lit( Lit), !.

lit_enclose( restart, Wake, Lit, u_delay(Wake,Lit)) :-
	functor( Lit, restart, _), !.

lit_enclose( block, BlockN, Lit, (Lit, BlockN)) :- !.
lit_enclose(   list,     _, Lit,         [Lit]) :- !.
lit_enclose(      _,     _, Lit,           Lit) :- !.


%%% lit_rpl_arg( InLit, N, Arg, OutLit)
%%% Ersetzt ntes Argument des Literals

:- mode lit_rpl_arg( +, ++, +, -).

lit_rpl_arg( InLit, N, Arg, OutLit) :-
	InLit =.. InLitL,
	length( L, N),
	append( L, [_|R], InLitL),
	append( L, [Arg|R], OutLitL),
	OutLit =.. OutLitL.


% ---------- Negation ----------

%%% lit_neg( NegLit, Lit)
%%% negiert Literal

lit_neg( Var, Var) :- var( Var), !.
lit_neg( Lit, Lit) :- def_test_prolog_lit( Lit), !.
lit_neg( -Lit, Lit) :- !.
lit_neg( ~Lit, Lit) :- !.

lit_neg( Lit, NotLit) :-    
	Lit =..[Fnc|R1],            
	lit_neg_fnc( Fnc, NotFnc),
	(var(NotLit) ->
	    NotLit =..[NotFnc|R1]
	;
	    NotLit =..[NotFnc|R2],
	    R1 == R2).


%%% lit_neg_fnc( Fnc, NotFnc)
%%% negiert Funktor

lit_neg_fnc( true, false) :- !.
lit_neg_fnc( false, true) :- !. 
lit_neg_fnc( protein_prolog, protein_prolog) :- !.

lit_neg_fnc( Fnc, NotFnc) :-
	name( Fnc, FncL),
	name( not_, NotL),
	(append( NotL, NotFncL, FncL) ; append( NotL, FncL, NotFncL)), !,
	name( NotFnc, NotFncL).


%%% lit_norm( KZ, FlagL, InLitL, OutLitL)
%%% Normalisierung der Literal

:- mode lit_norm( ++, ++, ?, -).
	
% Error bei falscher Syntax
lit_norm( _, _, Lit, _) :- 
	var( Lit),
	def_message( error, syntax, Lit).

lit_norm( _, _, Lit, _) :- 
	once(def_lit( Lit, _, NormLit)),
	var(NormLit),
	def_message( error, syntax, Lit).

% Vereinfachungen
lit_norm( dis, _, [true], [true]).

lit_norm( dis, _, InLitL, [true]) :-
	memberchk( true, InLitL),
	def_message( warning, strange, InLitL).

lit_norm( con, _, [false], [false]).

lit_norm( con, _, InLitL, [false]) :- 
	memberchk( false, InLitL),
	def_message( warning, strange, InLitL).

% Kontraere Lierale
lit_norm( KZ, _, InLitL, OutLitL) :-
	member( Lit, InLitL),
	\+ def_test_prolog_lit( Lit), 
	Lit \= protein_prolog( _),
	lit_neg( Lit, NegLit),
	\+ checklist( \==(NegLit), InLitL), 
	(KZ == dis -> OutLitL = [true] ; OutLitL = [false]),
	incval( taut), !.

% doppelte Literale und Check der einzelnen Literale 
% anschliessend evtl reordering
lit_norm( KZ, FlagL, InLitL, OutLitL) :- 
	u_remove_dups( InLitL, ZwaLitL),
	maplist( lit_norm_one_lit, ZwaLitL, ZwiLitL), 
	(KZ == dis -> SubLit = false ; SubLit = true),
	subtract( ZwiLitL, [SubLit], ZwoLitL), 
	l_reorder( FlagL, ZwoLitL, OutLitL), !.


%%% lit_norm_one_lit( InLit, OutLit)
%%% Normierung einzelner Literale

:- mode lit_norm_one_lit( +, -).

% WARNING bei vordefinierten Literalen
lit_norm_one_lit( Lit, NormLit) :-
	once(def_lit( Lit, _, ZwiLit)),
	functor( ZwiLit, F, A),
	(def_prolog_lit( F, A, w) ; def_special_lit( F, A, w)),
	def_message( warning, special_lit, Lit), 
	lit_enclose( prolog, pos, ZwiLit, NormLit), !.

% Normierung negativer Literale
lit_norm_one_lit( Lit, NormLit) :-
	def_lit( Lit, neg, PosLit),
	lit_neg( PosLit, NormLit),
	fnc_set( NormLit), !.

% Normierung positiver Literale 
lit_norm_one_lit( Lit, NormLit) :- 
	def_lit( Lit, pos, ZwiLit),
	lit_enclose( prolog, pos, ZwiLit, NormLit),
	fnc_set( NormLit), !.


%%% l_reorder( FlagL, InLitL, OutLitL)
%%% Reordering der Literallisten

:- mode l_reorder( ++, +, -).

l_reorder( [off|_], LitL, LitL) :- !.

% Nicht bei Klauseln mit Prologaufrufen
l_reorder( _, LitL, LitL) :-
	lit_enclose( prolog, pos, _, PrologLit),
	memberchk( PrologLit, LitL).

l_reorder( [Flag|_], InLitL, OutLitL) :-
	l_do_reorder( Flag, InLitL, OutLitL),  
        (InLitL == OutLitL -> true ; incval( reorder)).


%%% l_do_reorder( Flag, InLitL, OutLitL) 
%%% Bewertung der Literale

:- mode l_do_reorder( ++, +, -).

l_do_reorder( _, [], []).
l_do_reorder( _, [Lit], [Lit]).

l_do_reorder( groundness, InLitL, OutLitL) :-
	maplist( term_variables, InLitL, VarL),
	maplist( length, VarL, LengthL),
	u_connect_lists( LengthL, InLitL, ConnectedL),
        sort( 1, =<, ConnectedL, SortedConnectedL),
	u_connect_lists( _, OutLitL, SortedConnectedL).


% ---------- Zusatzargumente ----------

%%% lit_add( Short, Long)
%%% Zufuegen der Zusatzargumente

:- mode lit_add( ?, -).

lit_add( Lit, Lit)               :- var( Lit), !.
lit_add( [], [])                 :- !.
lit_add( [ShortH|ShortR], LongL) :- !, maplist(lit_add,[ShortH|ShortR],LongL).

lit_add( ShortTerm, LongTerm) :- 
	u_makelist_semicolon( ShortTerm, [ShortH1,ShortH2|ShortR]), !,
	maplist( lit_add, [ShortH1,ShortH2|ShortR], LongL),
	u_makelist_semicolon( LongTerm, LongL).

lit_add( Lit, Lit) :- def_test_prolog_lit( Lit), !.

lit_add( ShortLit, LongLit) :- 
	def_const( zarg, ZArg),
	length( ZArgL, ZArg),
	ShortLit =.. ShortLitL,
	append( ShortLitL, ZArgL, LongLitL),
	LongLit =.. LongLitL,
	l_get_pos( check, LongLit, Pos),
	def_const( check, Check),
	arg( Pos, LongLit, Check).

 
%%% lit_strip( Long, Short)
%%% Entfernen der Zusatzargumente

:- mode lit_strip( +, -).

lit_strip( Lit, Lit)              :- def_test_prolog_lit( Lit), !.
lit_strip( [], [])                :- !.
lit_strip( [LongH|LongR], ShortL) :- !, maplist(lit_strip,[LongH|LongR],ShortL).

lit_strip( LongLit, ShortLit) :-
	def_const( zarg, ZArg),
	length( ZArgL, ZArg),
	LongLit =.. [Fnc|LongLitR],
	append( [Fnc|ShortLitR], ZArgL, [Fnc|LongLitR]),
        ShortLit =.. [Fnc|ShortLitR], !.


% ---------- Benennen der Zusatzparameter ----------

%%% lit_name( ZArg, Arg, Lit(L)) 
%%% Vergibt Variablennamen an Zusatzargument ZArg

:- mode lit_name( ++, ?, ?).

lit_name( X) :-
	lit_name( reg, _, X),	
	lit_name( depth, _, X),
	lit_name( ancs, _, X),
	lit_name( b_ancs, _, X),
	lit_name( aunts, _, X),
	lit_name( b_aunts, _, X),
	lit_name( block, _, X),
	lit_name( query, _, X),
	lit_name( delay, _, X),
	lit_name( trace, _, X).

lit_name( _, _, Var)               :- var( Var), !.
lit_name( ZArg, Arg, delay(_,Lit)) :- !, lit_name( ZArg, Arg, Lit).
lit_name( _, _, Lit)               :- def_test_prolog_lit( Lit), !.
lit_name( ZArg, _, _)              :- def_get( use, ZArg, n), !.
lit_name( _, _, [])                :- !.
lit_name( ZArg, Arg, [Lit|LitR]) :- !, checklist(lit_name(ZArg,Arg),[Lit|LitR]).

lit_name( ZArg, Arg, LongLit) :-
	l_get_pos( ZArg, LongLit, N),
	arg( N, LongLit, Arg), !.

lit_name( ZArg, Arg, Lit) :- def_message( error, zarg, (ZArg,Arg,Lit)).


% ---------- Tests ----------  

%%% lit_test_neg( Lit)
%%% Test auf negatives Literal

:- mode lit_test_neg( +).

lit_test_neg( Lit) :-
        functor( Lit, Fnc, _),
        lit_test_neg_fnc( Fnc), !.


%%% lit_test_neg_fnc( Fnc)
%%% Test auf negativen Functor

:- mode lit_test_neg_fnc( +).

lit_test_neg_fnc( false) :- !.

lit_test_neg_fnc( Fnc) :-
        name( Fnc, FncL),
        name( not_, NotL),
	append( NotL, _, FncL), !.


%%% lit_test_longlit( Lit)
%%% Testst ob Literal mit Zusatzargumenten	 

lit_test_longlit( Lit) :-
	functor( Lit, _, Arity),
	def_const( zarg, ZArg),
	Arity >= ZArg,
	l_get_pos( check, Lit, Pos),
	arg( Pos, Lit, TestCheck),
        def_const( check, Check),
	TestCheck == Check, !.


% ---------- Ancestors Berechnen ----------

%%% lit_get_ancs( FlagL, Head, Anc, Ancs)
%%% Berechnet neue Ancestor aus Head fuer die Bodyliterale

:- mode lit_get_ancs( ++, +, -). 

% negative Literale
lit_get_ancs( [AncsFlag], Head, Anc, (PosAnc,[Anc|NegAnc])) :-
	lit_test_neg( Anc),
	(AncsFlag == pos  ; AncsFlag == pos_dj ; 
         AncsFlag == both ; AncsFlag == both_dj), !,
	lit_name( ancs, (PosAnc,NegAnc), Head).

% positive Literale
lit_get_ancs( [AncsFlag], Head, Anc, ([Anc|PosAnc],NegAnc)) :-
	(AncsFlag == neg ; AncsFlag == both ; AncsFlag == both_dj), !,
	lit_name( ancs, (PosAnc,NegAnc), Head).

lit_get_ancs( _, Head, _, Ancs) :- lit_name( ancs, Ancs, Head).


% ---------- Aunts berechnen ----------
		
%%% lit_set_aunts( BodyL, Aunts)
%%% Aunts in Body setzen, das heisst in einem Literal werden die vorhergehenden
%%% Literale in die Auntlisten aufgenommen.

:- mode lit_set_aunts_in_body( +, ?).

lit_set_aunts( [], _) :- !.
lit_set_aunts( [Lit], Aunts) :- !, lit_name( aunts, Aunts, Lit).

lit_set_aunts( [LongLit|BodyR], Aunts) :-
	def_test_special_lit( LongLit), !,
	lit_name( aunts, Aunts, LongLit),
	lit_set_aunts( BodyR, Aunts).

lit_set_aunts( [LongLit|BodyR], (PosAunt,NegAunt)) :-
	lit_test_neg( LongLit), !,
	lit_name( aunts, (PosAunt,NegAunt), LongLit),
	lit_strip( LongLit, ShortLit),
	lit_set_aunts( BodyR, (PosAunt,[ShortLit|NegAunt])).

lit_set_aunts( [LongLit|BodyR], (PosAunt,NegAunt)) :-
	lit_name( aunts, (PosAunt,NegAunt), LongLit),
	lit_strip( LongLit, ShortLit),
	lit_set_aunts( BodyR, ([ShortLit|PosAunt],NegAunt)).


% ---------- Berechnungen ----------
 
%%% l_get_pos( ZArg, LongLit, Pos)
%%% Liefert Position eines ZArgs im Longlit

:- mode l_get_pos( ++, +, -).

l_get_pos( ZArg, LongLit, Pos) :-
	functor( LongLit, _, Arity),
	def_get( pos, ZArg, N),
	Pos is Arity - N,
	((Pos < 1 ; Pos > Arity) -> 
	    def_message( error, longlit, LongLit); true).


%%% lit_get_type( LitL, Type)
%%% Ermitteln des Klausltyps

:- mode lit_get_type( +, -).

lit_get_type( [query|_],   query)      :- !.
lit_get_type( [_],         fact)       :- !.
lit_get_type( [_,[],_],    th_fact)    :- !.
lit_get_type( [[_],_,_],   th_start_1) :- !.
lit_get_type( [[_|_],_,_], th_start)   :- !.
lit_get_type( _,           ext)        :- !.


% ---------- Zerlegungen ----------

%%% lit_analyze_th( BodyL, TestL, ThCallL, PremL, ConcL)
%%% Liefert Tests, Theorieaufruf und Conclusio aus Body

:- mode lit_analyze_th( +, -, -, -).

lit_analyze_th( [], [], [], [], []).

lit_analyze_th( [ThCall|ConcL], [], ThCall, PremL, ConcL) :-
	nonvar( ThCall),
	lit_strip( ThCall, theory( _, PremL, _)), !.

lit_analyze_th( [Lit|BodyR], [Lit|TestR], ThCall, PremL, ConcL) :- 
	lit_analyze_th( BodyR, TestR, ThCall, PremL, ConcL).


