/** <module> utility_kb

This module models and manages the hierarchy of the KB's concepts.

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/


%% astrazione della gerarchia

:- module(utility_kb, [get_hierarchy/3]).

:- meta_predicate get_hierarchy(:,+,-).

:- use_module(classes).
:- use_module(library(ugraphs)).

:- use_module(library(tabling)).
:- table expl_combination/4.

:- table get_combined_expls(_,_,_,_,_,lattice(append_expl/3)). %get_hierarchy_ric/6,
%:- table get_single_expls/7. %(_,_,_,_,_,lattice(append_expl)).

:- multifile trill:hierarchy/1.
trill:hierarchy(M:H):-
  utility_kb:hierarchy_int(M:H).

hierarchy_int(M:H):-
  init_hierarchy(H0),
  findall(C,M:class(C),L1),
  findall(Class,M:classAssertion(Class,_Individual),L2),
  append(L1,L2,L3),
  sort(L3,L4),
  add_classes(H0,L4,H1),
  retractall(M:kb_hierarchy(_)),
  assert(M:kb_hierarchy(H1)),
  forall(M:equivalentClasses(CL),(M:kb_hierarchy(H2),add_equivalentClasses(H2,CL,H3),retractall(M:kb_hierarchy(_)),assert(M:kb_hierarchy(H3)))),
  forall(M:disjointClasses(CL),(M:kb_hierarchy(H4),add_disjointClasses(H4,CL,H5),retractall(M:kb_hierarchy(_)),assert(M:kb_hierarchy(H5)))),
  forall(M:disjointUnion(C,D),(M:kb_hierarchy(H6),add_disjointUnion(H6,C,D,H7),retractall(M:kb_hierarchy(_)),assert(M:kb_hierarchy(H7)))),
  forall(M:subClassOf(C,D),(M:kb_hierarchy(H8),add_subClassOf(H8,C,D,H9),retractall(M:kb_hierarchy(_)),assert(M:kb_hierarchy(H9)))),
  M:kb_hierarchy(H),
  H=TreeH-NC-TreeD-Classes-Expls,
  writeln(TreeH),
  writeln(NC),
  writeln(TreeD),
  writeln(Classes),
  writeln(Expls).

% inizializza la gerarchia albero con thing + numero classi + albero disjoint + dizionario fra nodi e classi
% init_hierarchy(kb{hierarchy:TreeH,nClasses:1,disjointClasses:TreeD,node2classes:Classes})
init_hierarchy(TreeH-1-TreeD-Classes-[]):-
  vertices_edges_to_ugraph([0,'n'],[],TreeH),
  Classes=classes{'n':'http://www.w3.org/2002/07/owl#Nothing',0:'http://www.w3.org/2002/07/owl#Thing'},
  vertices_edges_to_ugraph([],['n'-0,0-'n'],TreeD).

check_disjoint(H):-
  \+ check_disjoint_int(H),!.

check_disjoint_int(TreeH-_NC-TreeD-_Classes-_Expls):-
  edges(TreeD,E),  
  member(DC1-DC2,E),
  reachable(DC1,TreeH,DCL1),
  reachable(DC2,TreeH,DCL2),
  member(SameNode,[DC1|DCL1]),
  memberchk(SameNode,[DC2|DCL2]),!.

add_disjoint_link(TreeH-NC-TreeD0-Classes-Expls,C,C1,TreeH-NC-TreeD1-Classes-Expls):-
  PC=Classes.find(C),
  PC1=Classes.find(C1),
  ( dif(PC,PC1) -> % check consistenza kb
     add_edges(TreeD0,[PC-PC1,PC1-PC],TreeD1)
    ;
     fail
  ).


add_hierarchy_link(TreeH-NC-TreeD-Classes-Expls0,C,C1,TreeH-NC-TreeD-Classes-Expls):- % già in equivalentClasses
  PC=Classes.find(C),
  PC=Classes.find(C1),!,
  add_subClass_expl(Expls0,C,C1,Expls).

add_hierarchy_link(TreeH0-NC-TreeD-Classes0-Expls0,C,C1,TreeH-NC-TreeD-Classes-Expls):- % linkati al contrario C sub D, D sub C -> trasformo in equivalent
  PC=Classes0.find(C),
  PC1=Classes0.find(C1),
  are_subClasses_int(TreeH0-NC-TreeD-Classes0-Expls0,PC,PC1),!, % controlla non siano già linkati
  merge_classes_int(TreeH0-NC-TreeD-Classes0-Expls0,PC,PC1,TreeH-NC-TreeD-Classes-_Expls0), % merge_classes deve tenere conto di loop con più classi: C sub D sub E, E sub C
  add_subClass_expl(Expls0,C,C1,Expls).

add_hierarchy_link(TreeH0-NC-TreeD-Classes-Expls0,C,C1,TreeH-NC-TreeD-Classes-Expls):- % non linkati
  PC=Classes.find(C),
  PC1=Classes.find(C1),
  del_edges(TreeH0,[0-PC],TreeH1),
  add_edges(TreeH1,[PC1-PC],TreeH),
  add_subClass_expl(Expls0,C,C1,Expls).

are_subClasses_int(TreeH-_NC-_TreeD-_Classes-_Expls,C,C1):-
  reachable(C,TreeH,L),
  ( (memberchk(C1,L),!) ; (member(EquivalentClasses,L),is_list(EquivalentClasses),memberchk(C1,EquivalentClasses),!)).

/*
merge_classes_int(TreeH0-NC-TreeD-Classes0,PC,PC1,TreeH-NC-TreeD-Classes):- % uno collegato all'altro direttamente
  edges(TreeH0,E),
  memberchk(PC1-PC,E),!,
  del_vertices(TreeH0,[PC],TreeH1), %rimuovo il nodo per C e unisco tutto in quello di C1
  update_edges(E,PC,PC1,EU),
  add_edges(TreeH1,EU,TreeH),
  C1=Classes0.PC1,
  del_dict(PC,Classes0,C,Classes1),
  merge_dict_value(C,C1,CM),
  Classes=Classes1.put(PC1,CM).
*/

merge_classes_int(TreeH0-NC-TreeD-Classes0-Expls,PC,PC1,TreeH-NC-TreeD-Classes-Expls):- % non collegati all'altro direttamente
  edges(TreeH0,E),
  collect_classes_2_merge(E,PC,PC1,PCL), %contiene tutta la catena da PC a PC1 escluso
  del_vertices(TreeH0,PCL,TreeH1),
  update_edges(E,PCL,PC1,EU),
  add_edges(TreeH1,EU,TreeH),
  C1=Classes0.PC1,
  del_classes_from_dict(PCL,Classes0,CL,Classes1),
  merge_dict_value(CL,C1,CM),
  Classes=Classes1.put(PC1,CM).

%contiene tutta la catena da PC a PC1 escluso
collect_classes_2_merge(E,PC,PC1,[PC]):-
  memberchk(PC-PC1,E),!.

collect_classes_2_merge(E,PC,PC1,[PC|PCT]):-
  member(PC-PCInt,E),
  collect_classes_2_merge(E,PCInt,PC1,PCT).

% sostituisce gli archi del vecchio nodo con nuovi archi
update_edges([],_,_,[]):-!.

update_edges([PC-N|T],PCL,PC1,[PC1-N|TU]):-
  dif(N,PC1),
  memberchk(PC,PCL),!,
  update_edges(T,PCL,PC1,TU).

update_edges([N-PC|T],PCL,PC1,[N-PC1|TU]):-
  dif(N,PC1),
  memberchk(PC,PCL),!,
  update_edges(T,PCL,PC1,TU).

update_edges([_N0-_N1|T],PCL,PC1,TU):-
  update_edges(T,PCL,PC1,TU).

%cancella le classi dal dict creando lista di classi da inserire nel nodo di merge
del_classes_from_dict([],Classes,[],Classes):- !.

del_classes_from_dict([PC|TPC],Classes0,[C|TCL],Classes):-
  del_dict(PC,Classes0,C,Classes1),
  del_classes_from_dict(TPC,Classes1,TCL,Classes).

% unisce due nodi del dict
merge_dict_value(C,C1,CM):-
  is_list(C1),!,
  append(C,C1,CM0),
  flatten(CM0,CM).

merge_dict_value(C,C1,CM):-
  append(C,[C1],CM0),
  flatten(CM0,CM).

  
% aggiunge una classe, se la classe già esiste fallisce
add_class(TreeH0-NC0-TreeD-Classes0-Expls0,Class,TreeH-NC-TreeD-Classes-Expls):-
  \+ _=Classes0.find(Class),
  NC is NC0 + 1,
  Classes=Classes0.put(NC0,Class),
  add_edges(TreeH0,[0-NC0],TreeH), %% classe sotto owl:Thing
  add_subClass_expl(Expls0,Class,'http://www.w3.org/2002/07/owl#Thing',Expls).

% aggiunge una lista di classi
add_classes(H,[],H):- !.

add_classes(H0,[Class|T],H):-
  add_class(H0,Class,H1),!,
  add_classes(H1,T,H).

add_classes(H0,[_|T],H):-
  add_classes(H0,T,H).

% aggiunge un insieme di classi equivalenti, se c'è già un set contenente classi equivalenti li unisce, altrimenti aggiunge. Fallisce se ha giù il nodo con tutte le classi
add_equivalentClasses(H0,ClassList,H):-
  add_eqClass_hier(H0,ClassList,H1),
  add_subClasses_expl(H1,ClassList,H2),
  add_eqClass_expl(H2,equivalentClasses(ClassList),H).

add_eqClass_hier(TreeH0-NC-TreeD-Classes0-Expls,ClassList0,TreeH-NC-TreeD-Classes-Expls):-
  sort(ClassList0,ClassList),
  findall(NodeC,(member(OneOfClassList,ClassList),NodeC=Classes0.find(OneOfClassList)),Nodes),
  (length(Nodes,1) -> %% se già c'è un nodo non modifico gerarchia ma solo nodo in dict
    (Nodes=[Node],
     update_eqNode(TreeH0-NC-TreeD-Classes0-Expls,Node,ClassList,TreeH-NC-TreeD-Classes-Expls)
    )
   ;
    (Nodes=[PC1|PCL],!, %% se ci sono più nodi faccio merge
     edges(TreeH0,E),
     del_vertices(TreeH0,PCL,TreeH1),
     update_edges(E,PCL,PC1,EU),
     add_edges(TreeH1,EU,TreeH),
     C1=Classes0.PC1,
     del_classes_from_dict(PCL,Classes0,CL,Classes1),
     merge_dict_value(CL,C1,CM),
     Classes=Classes1.put(PC1,CM)
    )
  ).

update_eqNode(TreeH-NC-TreeD-Classes0-Expls,Node,ClassList,TreeH-NC-TreeD-Classes-Expls):-
  EqClasses=Classes0.get(Node),
  ( dif(EqClasses,ClassList) ->
    ( append(EqClasses,ClassList,UnsortedClassList),
      sort(UnsortedClassList,ClassSet),
      Classes=Classes0.put(Node,ClassSet) 
    )
   ;
    fail
  ).

add_eqClass_hier(TreeH0-NC0-TreeD-Classes0-Expls,ClassList,TreeH-NC-TreeD-Classes-Expls):-  %% se non c'è nodo lo aggiungo. NOTA: aggiunta di classi deve essere fatta PRIMA della gestione degli assiomi di sottoclasse
  NC is NC0 + 1,
  Classes=Classes0.put(NC0,ClassList),
  add_edges(TreeH0,[0-NC0],TreeH).

add_subClasses_expl(Expls,[],Expls):-!.

add_subClasses_expl(TreeH-NC-TreeD-Classes-Expls0,[Class|List],TreeH-NC-TreeD-Classes-Expls):-
  add_subClass_expl(Expls0,Class,'http://www.w3.org/2002/07/owl#Thing',Expls1),
  add_subClasses_expl(TreeH-NC-TreeD-Classes-Expls1,List,TreeH-NC-TreeD-Classes-Expls).


add_eqClass_expl(H0,Ax,H):-
  add_eqClass_simple_expl(H0,Ax,H1),
  add_eqClass_complex_expl(H1,Ax,H).
  
add_eqClass_expl(H0,ClassList,ClassList,H):-
  add_eqClass_simple_expl(H0,ClassList,ClassList,equivalentClasses(ClassList),H1),
  add_eqClass_complex_expl(H1,equivalentClasses(ClassList),H).

% aggiunge spiegazioni fra i componenti del singolo assioma
add_eqClass_simple_expl(TreeH-NC-TreeD-Classes-Expls0,equivalentClasses(ClassList),TreeH-NC-TreeD-Classes-Expls):-
  add_eqClass_simple_expl(Expls0,ClassList,ClassList,equivalentClasses(ClassList),Expls).

add_eqClass_simple_expl(TreeH-NC-TreeD-Classes-Expls0,disjointUnion(Class,DisjList),TreeH-NC-TreeD-Classes-Expls):-
  add_eqClass_simple_expl(Expls0,[Class,unionOf(DisjList)],[Class,unionOf(DisjList)],disjointUnion(Class,DisjList),Expls).

add_eqClass_simple_expl(Expls,[],_L,_Ax,Expls):- !.

add_eqClass_simple_expl(Expls0,[C|T],L,Ax,Expls):-
  add_eqClass_simple_expl(Expls0,C,T,L,Ax,Expls1),
  add_eqClass_simple_expl(Expls1,T,L,Ax,Expls).

add_eqClass_simple_expl(E,_C,[],_L,_Ax,E):- !.

add_eqClass_simple_expl(E0,C,[C|T],L,Ax,E):- !,
  add_eqClass_simple_expl(E0,C,T,L,Ax,E).

add_eqClass_simple_expl(E0,C,[C1|T],L,Ax,E):-
  ( member(ex(C,C1)-Ex,E0) ->
     ( delete(E0,ex(C,C1)-Ex,E1),
       add_eqClass_simple_expl([ex(C,C1)-[[Ax]|Ex]|E1],C,T,L,Ax,E)
     )
    ;
     add_eqClass_simple_expl([ex(C,C1)-[[Ax]]|E0],C,T,L,Ax,E)
  ).

% combina le spiegazioni per tutti i membri dell'equivalent axiom
add_eqClass_complex_expl(TreeH-NC-TreeD-Classes-Expls0,equivalentClasses(ClassList),TreeH-NC-TreeD-Classes-Expls):-
  member(C,ClassList),!, % prendo una classe a caso
  PC=Classes.find(C),
  combine_eqClass_expl(Expls0,Classes.PC,Expls).

add_eqClass_complex_expl(TreeH-NC-TreeD-Classes-Expls0,disjointUnion(Class,_ClassList),TreeH-NC-TreeD-Classes-Expls):-
  PC=Classes.find(Class),
  combine_eqClass_expl(Expls0,Classes.PC,Expls).

combine_eqClass_expl(E,[],E):- !.

combine_eqClass_expl(Expls0,[C|T],Expls):-
  combine_eqClass_expl(Expls0,C,T,Expls1),
  combine_eqClass_expl(Expls1,T,Expls).

combine_eqClass_expl(Expls0,_C,[],Expls):- !,
  sort(Expls0,Expls).

combine_eqClass_expl(Expls0,C,[C1|T],Expls):-
  %abolish_table_subgoals(expl_combination(_,_,_,_,_)),
  findall(Ex,expl_combination(Expls0,C,C1,[],Ex),Exs),
  ( member(ex(C,C1)-Exs0,Expls0) ->
     ( delete(Expls0,ex(C,C1)-Exs0,Expls1),
       combine_eqClass_expl([ex(C,C1)-Exs|Expls1],C,T,Expls)
     )
    ;
     combine_eqClass_expl([ex(C,C1)-Exs|Expls0],C,T,Expls)
  ).

expl_combination(Expls,C,C1,Used,Ex):-
  (member(ex(C,C1)-Ex0,Expls) ; member(ex(C1,C)-Ex0,Expls)),
  \+ (memberchk(C,Used), memberchk(C1,Used)),
  member(Ex,Ex0).

expl_combination(Expls,C,C1,Used,Ex):-
  (member(ex(C,C0)-Ex0,Expls) ; member(ex(C0,C)-Ex0,Expls)),
  dif(C0,C1),dif(C0,'http://www.w3.org/2002/07/owl#Thing'),
  \+ (memberchk(C,Used), memberchk(C0,Used)),
  member(Ex01,Ex0),
  expl_combination(Expls,C0,C1,[C,C0|Used],Ex1),
  append(Ex01,Ex1,ExT),
  sort(ExT,Ex).


%% add_disjountClasses(...) aggiunge classi e verifica non ci sia contraddizione. Fallisce se c'è inconsistenza
add_disjointClasses(H0,ClassList,H):-
  add_disjClass_hier(H0,ClassList,H1),
  add_disjClass_expl(H1,disjointClasses(ClassList),H).
  
add_disjClass_hier(H0,ClassList,H):-
  add_classes(H0,ClassList,H1),
  add_single_disjointClass(H1,ClassList,H).

add_single_disjointClass(H,[],H):- !.

add_single_disjointClass(H0,[C|T],H):-
  add_single_disjointClass_int(H0,C,T,H1),
  add_single_disjointClass(H1,T,H).


add_single_disjointClass_int(H,_C,[],H):- !.

add_single_disjointClass_int(H0,C,[C|T],H):- !,
  add_single_disjointClass_int(H0,C,T,H).

add_single_disjointClass_int(H0,C,[C1|T],H):-
  ( add_disjoint_link(H0,C,C1,H1) ->
     add_single_disjointClass_int(H1,C,T,H)
    ;
     fail
  ).

add_disjClass_expl(TreeH-NC-TreeD-Classes-Expls0,Ax,TreeH-NC-TreeD-Classes-Expls):-
  (Ax=..[disjointClasses,Arg] ; Ax=..[disjointUnion,_,Arg]),
  add_disjClass_expl(Expls0,Arg,Ax,Expls).

add_disjClass_expl(Expls,[],_Ax,Expls):- !.

add_disjClass_expl(Expls0,[C|T],Ax,Expls):-
  add_disjClass_expl(Expls0,C,T,Ax,Expls1),
  add_disjClass_expl(Expls1,T,Ax,Expls).

add_disjClass_expl(E,_C,[],_Ax,E):- !.

add_disjClass_expl(E0,C,[C|T],Ax,E):- !,
  add_disjClass_expl(E0,C,T,Ax,E).

add_disjClass_expl(E0,C,[C1|T],Ax,E):-
  ( member(dis(C,C1)-Ex,E0) ->
      ( delete(E0,dis(C,C1)-Ex,E1),
        add_disjClass_expl([dis(C,C1)-[[Ax]|Ex]|E1],C,T,Ax,E)
      )
    ;
      add_disjClass_expl([dis(C,C1)-[[Ax]]|E0],C,T,Ax,E)
  ).

%% add_disjointUnion(classExpression,set(classExpression)) da controllare cosa fa e aggiungere classi. Gestire bene l'assioma.
add_disjointUnion(H0,Class,DisjointUnion,H):-
  add_eqClass_hier(H0,[Class,unionOf(DisjointUnion)],H1),
  add_disjClass_hier(H1,DisjointUnion,H2),
  add_eqClass_expl(H2,disjointUnion(Class,DisjointUnion),H3),
  add_disjClass_expl(H3,disjointUnion(Class,DisjointUnion),H).


%% add_subClassOf(...)  deve aggiungere/modificare il ramo, controllando prima che le due classi non siano in un nodo di equivalence. Uno o entrambe le classi possono non essere presenti
add_subClassOf(H0,SubClass,SupClass,H):-
  add_classes(H0,[SubClass,SupClass],H1),
  add_hierarchy_link(H1,SubClass,SupClass,H),
  check_disjoint(H),!. % si può proseguire

add_subClass_expl(Expls0,C,C1,[ex(C,C1)-ExF|Expls]):-
  member(ex(C,C1)-Ex,Expls0),!,
  delete(Expls0,ex(C,C1)-Ex,Expls),
  sort([[subClassOf(C,C1)]|Ex],ExF).

add_subClass_expl(Expls,C,C1,[ex(C,C1)-[[subClassOf(C,C1)]]|Expls]).


% TODO aggiungere eventuali altri sottoclasse (someValuesFrom(R,C)->somevaluesFrom(R,D) con subClassOf(C,D))


get_hierarchy(M:Class,_Expl4Class,H4C):- %prende la gerarchia (KB) una classe e la spiegazione per arrivare a quella classe e resituisce l'insieme di tutte le classi con spiegazioni da quella in su
  M:kb_hierarchy(TreeH-_NC-_TreeD-Classes-Expls),
  Pos=Classes.find(Class),
  edges(TreeH,E),
  get_combined_expls(Class,Pos,E,Classes,Expls,H4C).



get_combined_expls(Class,Pos,E,Classes,Expls,H4C):-
  get_single_expls(Class,Pos,E,Classes,Expls,Class,H4C).

append_expl(AllExpl,[EndClass-NewExpl],NewAllExpl):-
  \+ memberchk(EndClass-_,AllExpl),!,
  append(AllExpl,[EndClass-NewExpl],NewAllExpl).

append_expl(AllExpl,[EndClass-NewExpl],NewAllExpl):-
  member(EndClass-OldExpl,AllExpl),
  delete(AllExpl,EndClass-OldExpl,AllExpl0),
  append(OldExpl,NewExpl,NewExplT),
  append(AllExpl0,[EndClass-NewExplT],NewAllExpl).


get_next(P,_E,Classes,P,NextClass):-
  EqClasses=Classes.P,
  is_list(EqClasses),
  member(NextClass,EqClasses).

get_next(P,E,Classes,NextP,NextClass):-
  member(NextP-P,E),
  \+ owl_f(NextP),
  NextClass=Classes.NextP,
  \+ is_list(NextClass).

get_next(P,E,Classes,NextP,NextClass):-
  member(NextP-P,E),
  \+ owl_f(NextP),
  EqClasses=Classes.NextP,
  is_list(EqClasses),
  member(NextClass,EqClasses).

get_single_expls(Class,P,E,Classes,Expls,_Start,[NextClass-[Expls4Class]]):-
  get_next(P,E,Classes,_NextP,NextClass),
  member(ex(Class,NextClass)-Exs,Expls),
  member(Expls4Class,Exs).

get_single_expls(Class,P,E,Classes,Expls,Start,[EndClass-[TotExpl]]):-
  get_next(P,E,Classes,NextP,NextClass),
  dif(NextClass,Start),
  member(ex(Class,NextClass)-Exs,Expls),
  member(Expl4Class,Exs),
  get_single_expls(NextClass,NextP,E,Classes,Expls,Start,[EndClass-[Expl4EndClass]]),
  append(Expl4Class,Expl4EndClass,TotExpl).


get_single_expls(Class,P,E,Classes,Expls,_Start,[NextClass-[[equivalentClasses(ListExpls4Class)]]]):-
  get_next(P,E,Classes,P,NextClass),
  member(ex(NextClass,Class)-Exs,Expls),
  member([equivalentClasses(ListExpls4Class)],Exs).

get_single_expls(Class,P,E,Classes,Expls,Start,[EndClass-[TotExpl]]):-
  get_next(P,E,Classes,P,NextClass),
  member(ex(NextClass,Class)-Exs,Expls),
  member([equivalentClasses(ListExpls4Class)],Exs),
  get_single_expls(P,NextClass,E,Classes,Expls,Start,[EndClass-[Expl4EndClass]]),
  append([equivalentClasses(ListExpls4Class)],Expl4EndClass,TotExpl).


get_one_expl(Pos,Class,E,Classes,Expls,Expl4Class,EndClass-H4C):-
  get_hierarchy_ric(Pos,Class,E,Classes,Expls,EndClass-[Expl]),
  append(Expl4Class,Expl,H4C).

get_hierarchy_ric(0,_C,_E,_Classes,_Expls,0-[]). % arrivato a owl:Thing

get_hierarchy_ric(n,_C,_E,_Classes,_Expls,n-[]). % arrivato a owl:Nothing

get_hierarchy_ric(P,Class,E,Classes,Expls,EndClass-[Ex0|Ex1]):-
  EqClasses=Classes.P,
  is_list(EqClasses),
  member(NextClass,EqClasses),
  member(ex(Class,NextClass)-Exs,Expls),
  member(Ex0,Exs),
  get_hierarchy_ric(P,NextClass,E,Classes,Expls,EndClass-Ex1).

get_hierarchy_ric(P,Class,E,Classes,Expls,EndClass-Expl):-
  member(NextP-P,E),
  (owl_f(NextP) -> 
    ( Expl = [], EndClass = Class)
   ;
    ( NextClass=Classes.NextP,
      member(ex(Class,NextClass)-Exs,Expls),
      member(Ex0,Exs),
      get_hierarchy_ric(NextP,NextClass,E,Classes,Expls,EndClass-Ex1),
      Expl=[Ex0|Ex1]
    )
  ).




% owl fixed classes (owl:Thing e owl:Nothing)
owl_f(0).
owl_f(n).












































/*



init_hierarchy(M,H,Cors,Expls):-
  %trovo tutte le classi
  findall(C,M:class(C),L1),
  findall(Class,M:classAssertion(Class,_Individual),L2),
  findall([C1,C2],M:subClassOf(C1,C2),L3),
  findall(CL,M:equivalentClasses(CL),L4),
  findall(CL,M:disjointClasses(CL),L5),
  findall([C1,C2],M:disjointUnion(C1,C2),L5),
  append([L1,L2],L6),
  append(L3,L7),
  append(L5,L8),
  append([L6,L7,L8],L9),
  merge_concept_lists(L9,L4,L10,Cors,LeqMD),
  vertices_edges_to_ugraph(L10,[],H0),
  expand_hierarchy(H0,Cors,L3,Expls0,H),
  append(Expls0,LeqMD,Expls).

expand_hierarchy(H0,Cors,L3,Expls,H):-
  vertices(H0,Vs),
  expand_hierarchy_int(H0,Cors,L3,Vs,Expls,H).

expand_hierarchy_int(H,_,[],_,[],H):- !.

expand_hierarchy_int(H0,Cors,[[C,D]|T],Vs,Expls,H):-
  member(C,Vs),!,
  add_edges(H0,[C-D],H1),
  expand_hierarchy_int(H1,Cors,T,Vs,Expls,H).
  
expand_hierarchy_int(H0,Cors,[[C,D]|T],Vs,[(N-D,C,D)|Expls],H):-
  member(N-Eq,Cors),
  memberchk(C,Eq),!,
  add_edges(H0,[N-D],H1),
  expand_hierarchy_int(H1,Cors,T,Vs,Expls,H).

expand_hierarchy_int(H0,Cors,[[C,D]|T],Vs,[(C-N,C,D)|Expls],H):-
  member(N-Eq,Cors),
  memberchk(D,Eq),!,
  add_edges(H0,[C-N],H1),
  expand_hierarchy_int(H1,Cors,T,Vs,Expls,H).


merge_concept_lists(LC,LEq,L,Cors,LeqMD):-
  merge_eq(LEq,LEqM,LeqMD),
  append(LEqM,CU0),
  sort(CU0,CU),
  subtract(LC,CU,LCC),
  name_eq_nodes(LEqM,1,Names,Cors),
  append(LCC,Names,L).

double_leq([],[]):-!.

double_leq([H|T],[(H-[H])|T0]):-
  double_leq(T,T0).

divide_leq([],[],[]):-!.

divide_leq([H-E|T],[H|TH],[E|TE]):-
  divide_leq(T,TE,TH).

merge_eq(LEq0,LEqM,LEqM0):-
  double_leq(LEq0,LEq),
  merge_eq(LEq,LEqM0),
  divide_leq(LEqM0,LEqM,_LEqMD).

merge_eq(LEq,LEqM):-
  merge_eq_int(LEq,LEqM0),
  ( LEq = LEqM0 ->
      LEqM=LEqM0
    ;
      merge_eq(LEqM0,LEqM)
  ).

merge_eq_int([],[]):- !.

merge_eq_int([H-EH|T],[HF-EF|LEqM0]):-
  member(X-EX,T),
  match(H,X),!,
  append(H,X,H0),
  sort(H0,HF),
  append(EH,EX,EF),
  delete(T,X-EX,T0),
  merge_eq_int(T0,LEqM0).

merge_eq_int([H|T],[H|LEqM0]):-
  merge_eq_int(T,LEqM0).

name_eq_nodes([],_,[],[]):-!.

name_eq_nodes([H|T],N,[N|TN],[N-H|TC]):-
  N1 is N + 1,
  name_eq_nodes(T,N1,TN,TC).

%%%%%% UTILITY %%%%%%
match(L1,L2) :- member(E,L1),memberchk(E,L2),!.

*/
