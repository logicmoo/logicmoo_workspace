%********************     t_definitions    ********************

% Definition von
% - Konstanten
% - Statements des Inputfiles (stat) 
% - Flags
% - Zusatzargumente
% - Types der Preds und Clauses
% - Literaldeklarationen
% - Fehler- und Warningmeldungen

%************************************************************

:- module_interface( t_definitions).

:- global def_set_flag / 2.

:- export def_const / 2,
	  def_flag / 5,
          def_get / 3,
          def_init_flags / 0,
	  def_lit / 3,
	  def_message / 2, def_message / 3,
	  def_prolog_lit / 3,
	  def_set_const / 2,
	  def_set_flag / 1,
	  def_special_lit / 3,
	  def_stat / 1,
	  def_test / 2,
	  def_test_afterwards / 0,
	  def_test_prolog_lit / 1, def_test_prolog_lit / 2,
	  def_test_special_lit / 1, def_test_special_lit / 2,
	  protein_flag / 2,
	  protein_mode / 1.
	 
:- begin_module( t_definitions).

:- dynamic def_const / 2,
	   def_functor / 2,
           def_type / 10,
	   def_prolog_lit / 3,
	   def_special_lit / 3,
	   def_zarg / 5,
           protein_flag / 2.

:- use_module( t_utils).
:- use_module( t_literal).


protein_mode( compile).
	   
% ---------- Konstanten ----------

%%% def_const( Flag, Wert)
%%% Definition von Konstanten

:- mode def_const( ++, ?).

def_const( check, '_*').
def_const( zarg, 5).
def_const( header, 't_header_prolog.pl').
def_const( version, "2.33").
def_const( titel, Titel) :-
	def_const( version, Version),
	concat_string( ["********************    PROTEIN V", Version,
                   "   ********************"], Titel).
def_const( mode, plain).


% ---------- Negation ----------

%%% def_lit( Lit, pos/neg, PositiveLit)
%%% Definition der Literalsyntax

def_lit(  -(Lit), neg, Lit).
def_lit(  ~(Lit), neg, Lit).
def_lit(     Lit, pos, Lit).


% ---------- Statements ----------

%%% def_stat( Statement)
%%% Definiert Statements des Input Files und ausloesende Aktion

:- mode def_stat( +).

def_stat( scanner_flag(_,_)).
def_stat( lc_flag(_,_)).
def_stat( eqtrafo_flag(_,_)).
def_stat( :(partial_functions,_)).

def_stat( end_of_file)              :- u_close_file(tme).
def_stat( read(FName))              :- u_open_file(tme,read,FName).
def_stat( calculus(restart_me))     :- def_set_flag(calculus, rme).
def_stat( calculus(Value))          :- def_set_flag(calculus,Value).
def_stat( protein_flag(Flag,Value)) :- def_set_flag(Flag,Value).
def_stat( pttp_flag(Flag,Value))    :- def_set_flag(Flag,Value).
def_stat( depth_increment(Inc))     :- def_set_flag(depth_increment,Inc).
def_stat( prolog_pred(F/A))         :- def_set_prolog_lit((F,A)).

def_stat( theory_costs(_,_))        :-        
	def_message(warning, ignore, 'theory_costs / 2').

def_stat( begin(Mode))              :- 
	(Mode == prolog ; Mode == theory ; Mode == simplify ; 
         Mode = simplify(_)),
	\+ nonground( Mode),
	(Mode == simplify -> OutMode = simplify(default) ; OutMode = Mode),
	def_set_const( mode, OutMode).

def_stat( end(Mode))                :- 
	(Mode == prolog ; Mode == theory ; Mode == simplify ; 
         Mode = simplify(_)),	
	\+ nonground( Mode),
        (Mode == simplify -> OutMode = simplify(default) ; OutMode = Mode),
        (def_const( mode, OutMode) ; def_message( error, environment, Mode)),
	def_set_const( mode, plain).


% ---------- Flags ---------- 

%%% def_flag( Flag, Default, WerteL, Init, AbhCodeL)
%%% Existierenden Flags mit Defaultwerten und Liste der moeglichen Werte.
%%% AbhCodeL ist die Liste con Deklarationen, die abhaengig vom definierten Flag
%%% gesetzt werden. Dabei steht zuerst der Value, bei dem automatisch die
%%% Deklaration vorgenommen werden soll, dann folgt die Deklatration.
%%% KZ = init: Das Flag soll initialisiert werden.
%%% KZ = del:  Das Flag wird nicht assertiert.
%%% KZ = int:  Das Flag wird nicht von ausen gesetzt. Dient der Abkuerzung
%%%            von Flagkombinationen.
%%% Zusaetzliche Deklarationen werden bei def_test_afterwards vorgenommen.

def_flag( ancestry_rme, off, [on,off], init, []).
def_flag( ancs, both, [off,pos,neg,pos_dj,both_dj,both], init, 
                 [(off,        [def_set_zarg(ancs,n),
		                def_set_zarg(dj,n)]),
                  (oder(pos_dj,both_dj), 
                               [def_set_zarg(ancs,y),
			        def_set_zarg(dj,y)]),
                  (nicht(oder(off,oder(pos_dj,both_dj))), 
                               [def_set_zarg(ancs,y),
			        def_set_zarg(dj,n)])]).
def_flag( answers, one, [one,more,all], init, []).
def_flag( answer_set_handling, on, [on,off], init, []).
def_flag( calculus, me, [me,rme,hyper], init,
                     [(hyper, [def_set_flag(ancs,pos), 
                               def_set_flag(reduction,nocut),
                               def_set_flag(regularity,off),
                               def_set_flag(strict_rme,on),
	                       def_set_flag(th_nonewclauses,on),
		               def_set_costs((query,0)),
			       def_set_priority((query,cl+6)),
	                       def_set_costs((restart,0)),
			       def_set_priority((restart,cl+6)),
	                       def_set_costs((th_start,1,0)),
                               def_set_costs((th_start_1,1,0))])]).
def_flag( check_flags, [], [_], init, []).
def_flag( costs, defaults, [_], del, [(nicht(defaults), [def_set_costs( _)])]).
def_flag( count, on, [on,off], int, []).
def_flag( ctest, now, [now,store,end,protein], init, []).
def_flag( definite_answers, off, [on,off], init, []).
def_flag( delayed_rme, off, [cut,nocut,off], init, 
	                [(nicht(off),  [def_set_zarg(delay,y)]),
			 (off,         [def_set_zarg(delay,n)])]).
def_flag( depth_increment, 1, [_], init, []).
def_flag( dynamic, off, [on,off], init, []).
def_flag( ec_pruning, off, [on,off], init, 
                     [(on,  [def_set_flag(delayed_rme,nocut), 
                             def_set_zarg(b_ancs,y),
                             def_set_zarg(b_aunts,y),
			     def_set_zarg(block,y)]),
		      (off, [def_set_zarg(b_ancs,n),
                             def_set_zarg(b_aunts,n),
			     def_set_zarg(block,n)])]).
def_flag( factorisation, cut, [off,cut,nocut,both], init, 
                        [(off,        [def_set_zarg(aunts,n),
                                       def_set_zarg(b_aunts,n)]),
			 (nicht(off), [def_set_zarg(aunts,y)])]).
def_flag( rewrite, flat, [flat,deep], init, []).
def_flag( head_behind_prolog, off, [on,off], init, []).
def_flag( max_rew_cond, 0, [_], init, []).
def_flag( mmr, off, [off,wgcwa,gcwa,model], init,
                [(off,                     [def_set_zarg(block,n)]),
                 (nicht(off),              [def_set_flag(calculus,rme),
	                                    def_set_flag(delayed_rme,cut),
		                            def_set_flag(search,prolog),
					    def_set_zarg(block,y)]),
	         (oder(gcwa,model),        [def_set_zarg(b_ancs,y),
                                            def_set_zarg(b_aunts,y)]),
                 (nicht(oder(gcwa,model)), [def_set_zarg(b_ancs,n),
                                            def_set_zarg(b_aunts,n)])]).
def_flag( mode, pl1, [pl0,pl1], init,
                 [(pl0, [def_set_flag(factorisation,cut), 
                         def_set_flag(reduction,cut),
                         def_set_flag(regularity,nodelay),
                         def_set_flag(search,prolog),
                         def_set_flag(th_reduction,cut),
		         def_set_flag(th_regularity,nodelay)])]).
def_flag( out_stream, user, [_], n, []).
def_flag( priority, defaults, [_], del, 
                     [(nicht(defaults), [def_set_priority(_)])]).   
def_flag( query_reuse, on, [on,off], init, 
	                [(on,  [def_set_zarg(query,n)]),
		         (off, [def_set_zarg(query,y)])]).
def_flag( reduction, both, [off,cut,nocut,both], init, []).
def_flag( regularity, delay, [off,nodelay,delay], init, 
                       [(delay,        [def_set_zarg(reg,y)]),
		        (nicht(delay), [def_set_zarg(reg,n)])]).
def_flag( reorder, off, [off,groundness], init, []).
def_flag( search, id_tree, [prolog,id_tree,id_inf,id_term], init, 
                   [(prolog,        [def_set_zarg(depth,n)]),
		    (nicht(prolog), [def_set_zarg(depth,y)])]).
def_flag( selection_function, off, [on,off], init, []).
def_flag( sim_dynamic, nocut, [cut,nocut,mixed,off], init, []).
def_flag( sim_static, uncond, [off,uncond,all], init, []).
def_flag( sim_deletion, complete, [complete,keep,del], init, []).
def_flag( sim_focus, query, [query,all], init, []).
def_flag( sorting, costs, [costs,input], init, []).
def_flag( strict_rme, off, [on,off], init, []).
def_flag( th_nonewclauses, off, [on,off], init, []).
def_flag( th_reduction, both, [off,cut,nocut,both], init, []).
def_flag( th_regularity, delay, [off,nodelay,delay], init, []).
def_flag( th_sidelit_anc, off, [on,off], init, []).
def_flag( timeout, 0, [_], init, []).
def_flag( trace, info, [off,info,internal,dynamic(_)],
                 init, [(oder(off,info),        [def_set_zarg(trace,n)]),
		        (nicht(oder(off,info)), [def_set_zarg(trace,y)])]).
def_flag( translate, all, [all,plain_theory], init,
                     [(plain_theory, [def_set_flag(reduction,off),
	                              def_set_flag(regularity,off),
                                      def_set_flag(factorisation,off)])]).


% ---------- Zusatzarguemnte ----------

%%% def_zarg( ZArg, Order, Mode, Use, Pos)
%%% Definiert moegliche Zusatzargumente.
%%% Use = y gibt an, dass das Argument benutzt wird. Es steht dann an der 
%%% Arity - Pos Stelle im LongLit.

def_zarg( reg,     1,  ?, y, _). % Regularitaetstestflag
def_zarg( depth,   2,  ?, y, _). % aktuelle Tiefe
def_zarg( ancs,    3,  ?, y, _). % Ancestorlisten
def_zarg( b_ancs,  4,  ?, n, _). % Ancestorlisten mit Blocknummern
def_zarg( aunts,   5,  ?, y, _). % Auntlisten
def_zarg( b_aunts, 6,  ?, n, _). % Auntlisten mit Blocknummern
def_zarg( dj,      7,  ?, n, _). % KZ disjunctive Clause
def_zarg( block,   8,  ?, n, _). % Blocknummer
def_zarg( query,   9,  ?, n, _). % Klauselnummer der verwandten Query
def_zarg( delay,  10,  ?, n, _). % Delayvariable
def_zarg( trace,  11,  ?, n, _). % Beweisterm
def_zarg( check,  12, ++, y, 0). % Testargument


% ---------- Types ----------

%%% def_type( Type, Group, Class, Costs, Plus, Prio, Leaf, Text, Shape, Color) 
%%% - existierende Types fuer Attr-, Claus- und Pred-Struktur mit 
%%% - Gruppenzuordnung,
%%% - Klassenzuordnung: test fuer Vorabtests, ext fuer Kontrapositive, 
%%%                     last fuer abschliessende Praedikate
%%% - Defaultkosten und 
%%% - Plus, das auf die Costs addiert und die Mindestresourcen festlegt,
%%% - Prio, die Reihenfolge der Preds eines Heads im File bestimmt,
%%%   Prio = l: die Laenge der Bodies bestimmt die Prioritaet
%%% - Leaf == y falls der Beweisschritt einen den Pfad schliesst,
%%% - Text ist Information und
%%% - und Shape und Color legen die Knoten in Tview fest.

def_type( ext,            ext,   ext,    1, 0, cl, 
                     n, 'extension',                         1, "rgb:42/9a/a7").
def_type( fact,           ext,   ext,    0, 0, 6, 
                     y, 'extension with fact',               1, "rgb:42/9a/a7").
def_type( factor,      factor,   test,   0, 0, 5, 
                     y, 'factorisation without cut',         5, "rgb:cc/66/66").
def_type( factor_cut,  factor,   test,   0, 0, 3, 
                     y, 'factorisation with cut',            5, "rgb:cc/66/66").
% +++ ersetzen !!!
def_type( inc,           none,   last, 999, 0, 1, 
                     u, 'incrementation of the depth bound', -, "rgb:00/00/00").
def_type( none,          none,   last,   0, 0, 1, 
                     ?, '',                                  -, "rgb:00/00/00").
def_type( prolog,        none,   last,   0, 0, 1, 
                     y, 'invoking Prolog',                   6, "rgb:00/00/00").
def_type( query,          ext,    ext,   1, 0, cl+1, 
                     n, 'extension with query clause',       1, "rgb:00/00/80").
def_type( red,            red,   test,   0, 0, 4, 
                     y, 'reduction without cut',             3, "rgb:cc/33/99").
def_type( red_cut,        red,   test,   0, 0, 2, 
                     y, 'reduction with cut',                3, "rgb:cc/33/99").
def_type( reg,            reg,   test,   0, 0, 1, 
                     u, 'regularity test',                   -, "rgb:00/00/00").
def_type( restart,        ext,    ext,   1, 0, cl, 
                     n, 'extension with restart clause',     1, "rgb:00/00/80").
% sim( rew|eq|sim, con|dis|-, default|m|c|n)
def_type( sim(_,_,_),       simplify,  test,   0, 0, 7,
                     n, 'simplification rule',               0, "rgb:00/80/00").
def_type( sim(_,_),         simplify,  test,   0, 0, 7,
                     n, 'simplification rule',               0, "rgb:00/80/00").
def_type( th_end,      theory,   last,   0, 0, 4, 
                     d, 'end of theory inference',           -, "rgb:99/00/99").
% Die angegebenen Kosten werden auf die des Kontrapositivs aufaddiert.
% Die Kosten von ThExtensionen mit Fakten bleiben 0
def_type( th_ext,      theory,   last,   1, 0, cl, 
                     n, 'extension',                         1, "rgb:99/00/99").
def_type( th_fact,   th_start,   last,   0, 0, cl+7, 
                     n, 'total theory inference',            2, "rgb:99/00/99").
def_type( th_red,      theory,   test,   0, 0, 3, 
                     y, 'theory reduction without cut',      3, "rgb:99/00/99").
def_type( th_red_cut,  theory,   test,   0, 0, 2,
                     y, 'theory reduction with cut',         3, "rgb:99/00/99").
def_type( th_prolog,   theory,   last,   0, 0, 1, 
                     y, ' invoking Prolog',                  6, "rgb:99/00/99").
def_type( th_start,  th_start,   last,   1, 1, cl+1, 
                     n, 'partial theory inference',          2, "rgb:99/00/99").
def_type( th_start_1,th_start,   last,   1, 0, cl+1, 
                     n, 'partial theory inference',          2, "rgb:99/00/99").


% ---------- Literaldeklarationen ----------

%%% def_prolog_lit( Fnc, Arity, Warning)
%%% Definition der Prologliterale
%%% Die unter def_prolog_lit genannten Praedikate erhalten keine
%%% Zusatzparameter und werden in die Reduktionstests nicht einbezogen. 
%%% Sie werden von Prolog bewiesen.
%%% Warning = w, es wird ein WARNING beim lesen des Literals ausgegeben.

def_prolog_lit( p_call, 1, w).
def_prolog_lit( !, 0, w).
def_prolog_lit( ;, 2, n).
def_prolog_lit( delay, 2, w).
def_prolog_lit( fail, 0, w).
def_prolog_lit( getval, 2, w).
def_prolog_lit( incval, 1, w).
def_prolog_lit( protein_answer, 1, n).
def_prolog_lit( p_zarg, 2, n).
def_prolog_lit( p_assert_bck, 1, w).
def_prolog_lit( p_count, 1, w).
def_prolog_lit( p_dynamic, 9, w).
def_prolog_lit( p_identical, 2, w).
def_prolog_lit( p_identical_n_count, 3, w).
def_prolog_lit( p_identical_upto_restart, 2, w).
def_prolog_lit( p_mmr, 1, w).
def_prolog_lit( p_neg_shortlit, 3, w).
def_prolog_lit( p_never_identical, 2, w).
def_prolog_lit( p_never_identical_upto_restart, 2, w).
def_prolog_lit( p_rewrite, 7, w).
def_prolog_lit( p_trace, 6, w).
def_prolog_lit( p_unifiable, 2, w).
def_prolog_lit( p_unifiable_n_count, 3, w).
def_prolog_lit( p_weight, 2, w).
def_prolog_lit( setval, 2, w).
def_prolog_lit( trace, 1, w).


%%% def_special_lit( Fnc, Arity, Warning)
%%% Definition der Spezialliterale.
%%% Warning = w, es wird ein WARNING beim lesen des Literals ausgegeben.
%%% +++ Problemfall any. Wird nicht abgefangen, soll aber wie ein gewoehnliches
%%%     Literal behandelt werden.

def_special_lit( query, 0, n).
def_special_lit( restart, 0, n).
def_special_lit( theory, 3, w).
def_special_lit( none, 0, w).
def_special_lit( protein_prolog, 1, n).
def_special_lit( protein_zarg, 2, n).


% ---------- Meldungen ----------

%%% def_msg( Flag, Text))
%%% ErrorFlags und Texte
   
def_msg( costs,          "illegal costs").
def_msg( clausN,         "illegal clause number").
def_msg( environment,    "mismatched begin/end of environment"). 
def_msg( file,           "file does not exist:").
def_msg( flag,           "unknown flag or value"). 
def_msg( ignore,         "this statement will be ignored."). 
def_msg( longlit,        "kein Longlit").
def_msg( no_query, 
 "satisfiable because no query or pure negative clause found\n--- failure ---").
def_msg( out_stream,     "Output is not printed on screen.").
def_msg( query_reuse,    "flags query_reuse and ancestry_rme are off.").
def_msg( prio,           "illegal priority").
def_msg( special_lit,    "usage of an internal literal").
def_msg( strange,        "strange list of literals").
def_msg( syntax,         "invalid syntax of"). 
def_msg( trace,          "difficulties building trace term").
def_msg( type,           "illegal type").
def_msg( zarg,           "illegal zarg").
def_msg( _,              "unknown").


% ---------- Zugriff ----------

%%% def_get( Flag, Which, Value(s))
%%% Zugriff auf die verschiedenen Definitionen

def_get( class, Type, V)            :- def_type( Type,_,V,_,_,_,_,_,_,_).
def_get( costs, Type, (V1,V2))      :- def_type( Type,_,_,V1,V2,_,_,_,_,_).
def_get( group, Type, V)            :- def_type( Type,V,_,_,_,_,_,_,_,_).
def_get( info,  Type, (V1,V2,V3,V4)):- def_type( Type,_,_,_,_,_,V1,V2,V3,V4).
def_get( leaf,  Type, V)            :- def_type( Type,_,_,_,_,_,V,_,_,_).
def_get( mode,  ZArg, V)            :- def_zarg( ZArg,_,V,_,_).
def_get( pos,   ZArg, V)            :- def_zarg( ZArg,_,_,_,V). 
def_get( prio,  Type, V)            :- def_type( Type,_,_,_,_,V,_,_,_,_).
def_get( use,   ZArg, V)            :- def_zarg( ZArg,_,_,V,_). 
def_get( values,Flag, V)            :- def_flag( Flag,_,V,_,_). 


% ---------- Tests ----------

%%% def_test( Flag, Value)
%%% Test auf gueltigen Wert

def_test( costs, (Cost,Plus)) :- 
	integer(Cost), 
	integer(Plus), 
	Cost >= 0, 
	Plus >= 0.

def_test( costs, (cl,Plus)) :- integer(Plus), Plus >= 0.
def_test( costs, (cl+N,Plus)) :- integer(N), integer(Plus), Plus >= 0.
def_test( prio, Prio) :- integer(Prio), Prio > 0.
def_test( prio, cl).
def_test( prio, cl+N) :- integer(N).
def_test( type, Type) :- def_type( Type,_,_,_,_,_,_,_,_,_).


% ---------- Setzen ----------

%%% def_set_const( Const, Value)
%%% Aetzen von Konstanten

:- mode def_set_const( ++, +).

def_set_const( Const, Value) :- def_const( Const, Value), !.

def_set_const( Const, Value) :-
	retract_all( def_const(Const, _)),
	assert( def_const( Const, Value)).


%%% def_set_flag( FlagTerm)
%%% def_set_flag( Flag, Value)
%%% Setzt Flag

:- mode def_set_flag( ++).
:- mode def_set_flag( ++, +).

def_set_flag( protein_flag( Flag, Value)) :- def_set_flag( Flag, Value).

% diese Flags loesen nur eine Verarbeitung aus und werden nicht assertiert
def_set_flag( Flag, Value) :-
	def_flag( Flag, _, _, del, AbhCodeL), !,
	checklist( l_call( Value), AbhCodeL), !.

def_set_flag( Flag, Value) :-
	protein_flag( Flag, Value), 
	def_flag( Flag, _, _, _, AbhCodeL),
	checklist( l_call( Value), AbhCodeL), !.

def_set_flag( Flag, Value) :-
	def_test_flag( Flag, Value, AbhCodeL),
        (retract( protein_flag( Flag, _)) ; true),
        assert( protein_flag( Flag, Value)),
	checklist( l_call( Value), AbhCodeL), !.


%%% l_call( Value, AbhCodeL)
%%% Durchfuehren der Deklatrationen, falls die Values uebereinstimmen.
%%% nicht/1 darf nur als innerste Klammerung benutzt werden.

:- mode l_call( +, +).

l_call( Value, (Term,CallL)) :- 
	l_bool( Term, Value), 
	checklist( l_do_call(Value), CallL).

l_call( _, _).


%%% l_bool( Term, X)
%%% Testet Bollsche Ausdruecke

:- mode l_bool( ++, ++).

l_bool( oder(O1,O2), X) :- l_bool( O1, X) ; l_bool( O2, X).
l_bool( nicht(N),    X) :- \+ l_bool( N, X).
l_bool( X, X).


%%% l_do_call( Value, Call)
%%% Jetzt wird wirklich ausgefuehrt, wobei freie Variablen mit dem Value
%%% unifiziert werden.

:- mode l_do_call( ++, +).

l_do_call( Value, Call) :-
	Call =.. CallL,
	(member( Value, CallL) ; true),
	Call.


%%% def_set_zarg( ZArg, Use)
%%% Setzt Use-Argument der ZArg-Definition

:- mode def_set_zarg( ++, ++).

def_set_zarg( ZArg, Use) :- def_zarg( ZArg, _, _, Use, _).

def_set_zarg( ZArg, Use) :-
	def_const( zarg, N),
	(Use == y -> M is N + 1 ; M is N - 1),
	def_set_const( zarg, M),
	def_zarg( ZArg, Order, Mode, _, Pos), 
	retract_all( def_zarg( ZArg, _, _, _, _)),
	assert( def_zarg( ZArg, Order, Mode, Use, Pos)).


%%% def_set_costs( Term)
%%% Aendern der Defaultkostenangabe

:- mode def_set_costs( ++).

def_set_costs( (Type,Cost)) :-
	def_test( costs, (Cost,0)),
	def_type(Type,G,Cl,_,Pl,P,B,Tx,S,Co), !,
	retract_all( def_type(Type,_,_,_,_,_,_,_,_,_)),  
        assert( def_type(Type,G,Cl,Cost,Pl,P,B,Tx,S,Co)).

def_set_costs( (Type,Cost,Plus)) :-
	def_test( costs, (Cost,Plus)),
	def_type(Type,G,Cl,_,_,P,B,Tx,S,Co), !,
	retract_all( def_type(Type,_,_,_,_,_,_,_,_,_)),
        assert( def_type(Type,G,Cl,Cost,Plus,P,B,Tx,S,Co)).

def_set_costs( Term) :- def_message( error, costs, Term).


%%% def_set_priority( Term)
%%% Aendern der Defaultpriorityangaben

:- mode def_set_priority( ++).

def_set_priority( (Type, Prio)) :-
	def_test( prio, Prio),
        def_type(Type,G,Cl,C,Pl,_,B,Tx,S,Co), !,
	retract_all( def_type(Type,_,_,_,_,_,_,_,_,_)),
        assert( def_type(Type,G,Cl,C,Pl,Prio,B,Tx,S,Co)).

def_set_priority( Term) :- def_message( error, priority, Term).


%%% def_set_prolog_lit( (Functor,Arity))
%%% Deklariert neue Prologliterale

:- mode def_set_prolog_lit( ++).

def_set_prolog_lit( (F,A)) :- def_prolog_lit( F, A, _).
def_set_prolog_lit( (F,A)) :- assert( def_prolog_lit( F, A, n)).


% ---------- Initialisierungen ----------

%%% def_init_flags
%%% Initialisiert Flageintraege

def_init_flags :-
	def_set_const( theory, no),
	def_set_const( simplify, no),
	bagof( _, (def_flag( Flag1, _, _, KZ, _), 
                   (KZ == init ; KZ == int), 
	           retract_all( protein_flag(Flag1,_))), _),
	bagof( _, (def_flag( Flag2, Default, _, _, _),
	           \+ protein_flag( Flag2, _),
	           def_set_flag( Flag2, Default)), _).
       

% ---------- Tests ----------

%%% def_test_flag( Flag, Value, AbhFlagL)
%%% Testet, ob diese Flag-Konstellation gueltig ist, ansonsten ERROR.

:- mode def_test_flag( ++, ++, -).

def_test_flag( Flag, Value, AbhFlagL) :-
	def_flag( Flag, _, ValueL, _, AbhFlagL),
	member( Value, ValueL), !.

def_test_flag( Flag, Value, _) :-
	def_message( error, flag, protein_flag(Flag,Value)), !.


%%% def_test_afterwards
%%% Tests und Deklarationen nachdem der Iput File geparst wurde.

% neue th_ext-Kosten koennen erst hier eingestellt werden
def_test_afterwards :-
	def_const( simplify, yes),
	\+ protein_flag( sim_dynamic, off),
        once( def_set_costs( (th_ext,3))),
	fail.

% ARME braucht Regularity Argument fuer globale Regularitaet
def_test_afterwards :-
	protein_flag( calculus, rme),
	protein_flag( ancestry_rme, on),
	protein_flag( regularity, nodelay),
        once(def_set_zarg( reg, y)),
	fail.

% WARNING falls query_reuse und ancestry_rme off
def_test_afterwards :-
	\+ protein_flag( calculus, me),
	protein_flag( query_reuse, off),
	protein_flag( ancestry_rme, off),
	def_message( warning, query_reuse), 
	fail.

def_test_afterwards :-
	protein_flag( answers, more),
	\+ protein_flag( out_stream, user),
	def_message( warning, out_stream),
	fail.

% Setzen des internen Flags count
def_test_afterwards :-
	protein_flag( trace, Trace),
        (Trace == off ; Trace == info),
        protein_flag( mmr, MMR),
	(MMR == off ; MMR == wgcwa),
	protein_flag( ec_pruning, off),
        once( def_set_flag( count, off)),
	fail.

% hier gibt es Schwierigkeiten beim backtacken
def_test_afterwards :- 
	def_const( zarg, ZARG),
	N is ZARG - 1,
	l_set_pos( 1, N), !.
	

%%% l_set_pos( Order, Anz)
%%% Berechnung der Positionen

l_set_pos( _, -1) :- !.

l_set_pos( Order, Anz) :-
	def_zarg( _, Order, _, n, _),
	Order1 is Order + 1,
	l_set_pos( Order1, Anz).

l_set_pos( Order, Anz) :-
	def_zarg( ZArg, Order, Mode, y, _),
	retract_all( def_zarg( ZArg, _, _, _, _)),
	assert( def_zarg( ZArg, Order, Mode, y, Anz)),
	Order1 is Order + 1,
	Anz1 is Anz - 1, 
	l_set_pos( Order1, Anz1).


%%% def_test_prolog_lit( Lit)(Functor,Arity)
%%% Test auf Prologliteral, Variable oder deklariert als def_prolog_lit

:- mode def_test_prolog_lit( ++, ++).

def_test_prolog_lit( Lit) :- var( Lit), !.

def_test_prolog_lit( Lit) :- 
	functor( Lit, F, A),
	def_prolog_lit( F, A, _).

def_test_prolog_lit( F, A) :- def_prolog_lit( F, A, _), !.


%%% def_test_special_lit( Lit)(Functor,Arity)
%%% Test auf Spezialliteral, Prologliteral oder deklariert als def_specal_lit

:- mode def_test_special_lit( ++, ++).

def_test_special_lit( Lit) :- var( Lit), !.

def_test_special_lit( Lit) :-
	functor( Lit, F, A),
	def_test_special_lit( F, A), !.

def_test_special_lit( Lit) :- 
	lit_test_longlit( Lit),
	lit_strip( Lit, ShortLit),
	functor( ShortLit, F, A),
	def_special_lit( F, A , _), !.

def_test_special_lit( F, A) :- def_test_prolog_lit( F, A ), !.
def_test_special_lit( F, A) :- def_special_lit( F, A, _), !.

def_test_special_lit( F, A) :-
	lit_test_neg_fnc( F),
	lit_neg_fnc( F, NotF),
	def_special_lit( NotF, A, _), !.


%%% def_message( Type, Flag, (Text))
%%% Fehlermeldung mit und ohne Abbruch

:- mode def_message( ++, ++).
:- mode def_message( ++, ++, +).

def_message( Type, Flag) :- def_message( Type, Flag, "").

def_message( Type, Flag, Info) :-
        ((Type == error,   T1 = "ERROR: ") ; 
         (Type == warning, T1 = "WARNING: ") ;
         (Type == abort,   T1 = "")),
        def_msg( Flag, T2),
	u_out( "%n%w%w %w", [T1,T2,Info]), !,
        (Type == warning -> true ; u_abort).
