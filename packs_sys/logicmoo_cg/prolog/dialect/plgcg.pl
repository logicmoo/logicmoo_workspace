:- encoding(iso_latin_1).
:- module(prolog_plus_cg_reader,[cg_begin/0,cg_end/0,set_cg_file/1]).
:- set_module(class(library)).
:- nodebug(cg_inline).
:- use_module(library(cgprolog)).

:- multifile_data(cg/1).
:- multifile_data(cg_inline/1).

ppcg_expansion(IS, _, _):- var(IS), !, fail.
ppcg_expansion(_, In, _):- var(In), !, fail.
ppcg_expansion(_, end_of_file, _Out):- 
  source_location(F,_), retractall(t_l:cg_term_expand(_,F,_)),
  set_cg_file(false),
  fail.
ppcg_expansion(_, In, _):- \+ compound(In), !, fail.
ppcg_expansion(_, In, _):- \+ is_current_source_term(In), !, fail.
ppcg_expansion(IS, _, _):- compound(IS), \+ is_in_cg(IS), !, fail.
ppcg_expansion(_, In, Out):- ppcg_expand(In, Out), In\==Out, ignore((debugging(cg_inline),wdmsg(Out),nl)).



prolog_plus_cg_op(',').
prolog_plus_cg_op('->').
prolog_plus_cg_op('-').
prolog_plus_cg_op(OP):- current_op(Priority,_,OP),  7 is Priority mod 10.
inline_reader_ops(OPS):- OPS = [op(1157,yfx,'::'),op(1157,yfx,'>'),op(957,yfx,'<-'),op(957,yfx,'->'),op(1157,yfx,'=')].

inline_reader_ops(OPS):-                                    
 
 current_op(X1,Y1,('+')),
 current_op(X,Y,('->')),
 =(OPS,
  ([op(X,Y,('<-')), 
   op(X1,Y1,('*')),op(X1,Y1,('?')),op(X1,Y1,('@')),
   op(900,xfy,('<-')),op(1000,yfx,('->')),op(1100,xfy,('-')),op(1110,xfx,('-')),op(1100,yfx,('-')),op(500,xfx,(':')),
   op(300, fx,('?')),op(300, fx,('#')),op(300, fx,('*')),op(300, fx,('@')),
   op(300,yfx,('?')),op(300,yfx,('#')),op(300,yfx,('*')),op(300,yfx,('@')),
   op(1200,xfx,(':')),op(1200,xfx,('='))])).


term_to_cg(In,Out):-
  format(chars(Chars),' ~q. ',[In]),
  any_to_string(Chars,Str),
  % replace_in_string(['('='{',')'='}'],Str,Str0),
  replace_in_string(['\r'='\n'],Str,Str0),
  atom_codes(Str0,Codes),
  must_or_rtrace(tokenize_cg(Toks,Codes,[])),
  parse_cg(Out,Toks,[]),!,
  ignore((fail,Out\=@=In, with_no_operators((nl,display(bf(In)),nl,display(af(Out)),nl)))),!.


atom_unless_var(N,_):- \+ atomic(N),!.
atom_unless_var(N,_):- atom_concat('_',_,N).
atom_unless_var(N,_):- downcase_atom(N,N).
atom_unless_var(N,V):- N = V.


ppcg_expand(In, _):- debugging(cg_inline), display(In),nl,fail.
ppcg_expand(In, _Out):- \+ compound(In), !, fail.
ppcg_expand(In, _Out):- In = ( :- _ ),!, fail.
ppcg_expand((H:-B), Out):- !, is_ppcg_head(H), force_ppcg_expand((H:-B), Out).
ppcg_expand(H, Out):- !, is_ppcg_head(H), force_ppcg_expand(H, Out).

is_ppcg_head(In):- var(In),!.
is_ppcg_head(In):- compound(In), functor(In,F,A), prolog_plus_cg_op(F), member(A,[1,2]).

force_ppcg_expand(In, Out):- 
   implode_varnames_pred(atom_unless_var, In), 
   Out = cg_inline(In),!.

force_ppcg_expand(cg(In),Out) :-               
   implode_varnames_pred(=, In),
   term_to_cg(In,CG),
   current_why(UU),
   Out = (:- with_current_why(UU, assert_cg(cg(CG)))).

:- dynamic(t_l:cg_term_expand/3).

is_file_in_cg(F,CL):- 
  t_l:cg_term_expand(begin_cg,F,BL), (CL > BL), !, 
  \+ (t_l:cg_term_expand(end_cg,F,EL), ((EL > BL), (EL < CL))).

cg_begin:- source_location(F,L),assertz(t_l:cg_term_expand(begin_cg,F,L)),!, set_cg_file(true),!.
cg_end:- source_location(F,L),assertz(t_l:cg_term_expand(end_cg,F,L)), set_cg_file(false),!.


is_in_cg(_IS):- check_in_cg.


check_in_cg:- ignore(((source_location(F,L), fail,  (is_file_in_cg(F,L) ->  set_cg_file(true) ; set_cg_file(false))))),!,
 nb_current(cg_term_expand,true).
 

set_cg_file(TF):- nb_current(cg_term_expand,TF),!.
set_cg_file(TF):- nb_setval(cg_term_expand,TF),
  set_prolog_flag(allow_variable_name_as_functor,TF),
  (TF -> ((set_prolog_flag(encoding,iso_latin_1),style_check(-singleton))) ; style_check(+singleton)),
  (TF -> (inline_reader_ops(OPS), push_operators(OPS, Undo), asserta(undo_cg_file_ops(Undo)))
    ;ignore((retract(undo_cg_file_ops(Undo)),pop_operators(Undo)))),!.

ppcg_ge(In,Out):- In== (/), Out=!.

% :- style_check(-singleton).
  
term_expansion(In,IS,Out,OS) :- ppcg_expansion(IS,In,Out)-> IS=OS.
goal_expansion(In,Out) :- ppcg_ge(In,Out).

:- dynamic addInstance/2,eq/2,isInstanceOf/2,maximalJoin/6,phrase_imperative/2,read_sentence/1.
:- multifile addInstance/2,eq/2,isInstanceOf/2,maximalJoin/6,phrase_imperative/2,read_sentence/1.



:- cg_begin.

Universal > Person, Object, Action, Attribute, Proposition.


Person > Man, Woman.
Object > Pyramid, Cube, Sphere.
Action > Put, Push, Create, Move.
Attributc > Size, Color, Modality.


Color = blue, red.
Size = small, big.
Man = john.

:- discontiguous(lexicon/3).

lexicon("push", verb, [Push]-
		    -obj->[Object],
		    -on->[Object]     ).
lexicon("create", verb, [Create]-obj->[Object]-colorOf->[Color]).


lexicon("pyramid", noun, Pyramid).
lexicon("cube", noun, Cube).
lexicon("sphere", noun, Cube).

lexicon("small", adj, sizeOf, Size, small).
lexicon("red", adj, colorOf, Color, red).
lexicon("big", adj, sizeOf, Size, big).
lexicon("blue", adj, colorOf, Color, blue).

lexicon("on", prep, on).
lexicon("under", prep, under).
lexicon("left", prep, left).
lexicon("right", prep, right).

lexicon("the", art, x).
lexicon("a", art, x).
Verb(v, G) :- lexicon(v, verb, G).

Prep((v|P), P, V) :- lexicon(v, prep, V).

Art((v|P), P, V) :- lexicon(v, art, V), (/).
Art(P, P, undefined).

Noun((v|P), P, V) :- lexicon(v, noun, V).

Adj(A, R, T, V) :- lexicon(A, adj, R, T, V).

Shrdlu :-
  write("**** Welcome to the SHRDLU_PCG Program *******"),
  % new(aShrdlu_Canvas3D, "PrologPlusCG.Shrdlu_Canvas3D", '()'),
  read_sentence(_sentence),
  ShrdluDialog(_sentence), (/).

ShrdluDialog(("end", ".")) :- (/).
ShrdluDialog(_sentence) :-
  Semantic_Analysis(_sentence, _CG),
  write(_CG),	
  _CG,
  read_sentence(_s),
  ShrdluDialog(_s), (/).

semantic_analyzer :-
   read_sentence(P),
   phrase_imperative(P, G),
   write(G), (/).
                                                                                                               
Semantic_Analysis(_sentence, _CG) :- 
   imperative_sentence(_sentence, _CG).

% WAS [Proposition = G] - (mode) -> [ Modality = imperative] :- G.
['Proposition'='G']-mode->['Modality'=imperative]:-'G' .

[Create]-obj->[T_Obj : _IdObj]-colorOf->[Color = C] :-
   asserta(object([T_Obj : _IdObj]-colorOf->[Color = C]), '()'),
   write((T_Obj, _IdObj, C)),
   % execMethod(void, "PrologPlusCG.Shrdlu_Canvas3D", T_Obj, (_IdObj, C)),
   (/).

imperative_sentence((V|P1), 
                   [Proposition = G]-mode->[Modality = imperative]) :- 
   Verb(V, G_V),
   NP(P1, P2, E_NP1, S1),
   eq([T_Verb]-obj->E_N_G1, G_V),
   maximalJoin(G_V, E_N_G1, S1, E_NP1, G1_S1, _),
   complement(P2, T_Verb, G1_S1, G).

complement(("."), _, G, G) :- (/).
complement(P2, T_Verb, G1_S1, G) :-
   Prep(P2, P3, s_prep),
   NP(P3, ("."), E_NP2, S2),
   eq([T_Verb]-s_prep->E_N_G2, G1_S1),
   maximalJoin(G1_S1, E_N_G2, S2, E_NP2, G, _).


NP(P, P1, E, G) :-
   Art(P, P2, A1),
   AdjsSynt(P2, P3, L_Adjs),
   Noun(P3, P4, N),
   suiteNP(P4, P1, N, A1, L_Adjs, E, G), (/).

suiteNP((N1|P1), P1, N, A1, L_Adjs, E, G) :-
   not(lexicon(N1, _, _)),
   not(lexicon(N1, _, _, _, _)),
   traiteInst(N1, N),
   SemAdjs(L_Adjs, N, N1, G, E), (/).
suiteNP(P4, P1, N, A1, L_Adjs, E, G) :-
   SemAdjs(L_Adjs, N, A1, S, E1),
   AdjsSynt(P4, P1, L_Adjs2),
   SemAdjs(L_Adjs2, N, A1, S1, E11),
   maximalJoin(S, E1, S1, E11, G, E).

traiteInst(N1, N) :-
  isInstanceOf(N1, N), (/).
traiteInst(N1, N) :-
  addInstance(N1, N).

AdjsSynt((A|P), P1, (A|L_Adjs)) :-
  lexicon(A, adj, _, _, _),
  AdjsSynt(P, P1, L_Adjs), (/).
AdjsSynt(P, P, '()').

SemAdjs((A|P), N, A1, S, E_N_S) :-
   Adj(A, R1, T1, V1),
   eq(G, [N : A1]-R1->[T1 = V1]), 
   eq(G, E_N-R1->x),
   SemAdjs2(P, G, E_N, N, A1, S, E_N_S), (/).
SemAdjs('()', N, A1, G, E) :-
   eq(G, [N : A1]),
   eq(G, E-rel->[Universal]), (/).

SemAdjs2((A|P), G, E_N, N, A1, S, E_S) :-
   Adj(A, R, T, V),
   eq(G1, [N : A1]-R->[T = V]),
   eq(G1, E_N1-R->x),
   maximalJoin(G, E_N, G1, E_N1, G2, E_N2), 
   SemAdjs2(P, G2, E_N2, N, A1, S, E_S), (/).
SemAdjs2('()', G, E, _, _, G, E).


end_of_file.

cg([Cat: @every]-(On)->[Mat]).
cg([Mat #1]-(equal)->[Thingy #1]).
cg(['Man':imad]<-agnt-['Drive']-obj->['Car']).
cg([Cat#1]-(On)->[Mat #1]-(equal)->[Thingy #1]).
cg([Cat: ?x]-(equal)->M1-(On)->[Mat]).
cg([Cat: ?x]-(On)->[Mat]).
cg([Man:karim]<-agnt-[Drink]-obj->[Water]).
cg([Mat #1]<- (on)- [Cat #1]).
cg([Mat]<-(On)-[Cat: ?x]).
cg([Thingy #1]<-(equal)-[Mat #1]).
cg([Cat #1]-(On)->[Mat #1]-(equal)->[Thingy #1]).
cg([Man:karim]<-agnt-[Drink]-obj->[Water]).
cg([Thingy #1] <- (equal) -[Mat #1]<- (on)- [Cat#1]).
cg([Cat: @every]->(On)->[Mat]).
%cg([Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?z)).
cg(?x -(equal)-> [Thingy #1]).

cg(?x -(On)->[Mat #1]-(equal)->[Thingy #1]).
cg(?x -(On)->[Mat #1]).
cg([?x] -(equal)-> [Thingy #1]).
cg([?x]-(On)->[Mat #1]-(equal)->[Thingy #1]).
cg([Mat ?x]-(equal)->[Thingy #1]).
cg([Cat: ?x]-(On)->[Mat #1]-(equal)->[Thingy #2]).


:- begin_cg.
cg(

[Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]).


cg(
 [a] - (belives) -> 
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]).

cg([Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table).

cg([Begin]-
        -obj->[Session],
        -srce->[Proposition = [Press] -
       -obj -> [Key : enter]-partOf->[Keyboard],
       -agnt -> [Person : John] ],
        -agnt->[Person : John]).

cg(
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2])).

cg(
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]).

cg(
% ontology required (to load first): aminePlatform(/)samples(/)ontology(/)ManOntology2.xml
[Eat #0] -  
   - obj->[Apple],
   - manr->[Fast],
   - agnt->[Man]).


cg([Cat: ?x]-(equal)->M1-(On)->[Mat]).
cg([Cat: ?x]-(On)->[Mat].).
cg([Mat]<-(On)-[Cat: ?x].).




cg(

% (/)(/) ontology required (to load first): aminePlatform(/)samples(/)ontology(/)ManOntology2.xml
[Eat #0] -
      -obj->[Apple],
      -manr->[Fast],
      -agnt->[Man]

).


cg(
[Begin]-
        -obj->[Session],
        -srce->[Proposition = [Press] -
	       -obj -> [Key : enter]-partOf->[Keyboard],
	       -agnt -> [Person : John] ],
        -agnt->[Person : John]).

cg([Man:karim]<-agnt-[Drink]-obj->[Water]).

cg([Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table).


:- begin_cg.

cg([Man:karim]<-agnt-[Eat]-obj->[Apple]).

cg([Man:imad]<-agnt-[Drive]-obj->[Car]).

cg([Man:karim]<-agnt-[Drink]-obj->[Water]).

%cg([Man:karim]<-agnt-[Eat]-obj->[Apple]).



%cg([Cat: @every]->(On)->[Mat]).

%cg([Man:karim]<-agnt-[Drink]-obj->[Water]).


%cg([Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->table).
%cg([Man:imad]<-agnt-[Drive]-obj->[Car]).
%cg([Cat: ?x]-(On)->[Mat]).


cg([Cat: @every]->(On)->[Mat]).
cg(

[Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus].

).


cg(
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]].
).


 
cg([Go]-
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]).

cg([Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]).

cg(
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) -

   [Go2]).


cg(
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]).


