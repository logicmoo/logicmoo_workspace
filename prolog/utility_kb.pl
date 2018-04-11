/** <module> utility_kb

This module models and manages the hierarchy of the KB's concepts.

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/


%% astrazione della gerarchia

:- module(utility_kb, []).

:- use_module(classes).
:- use_module(library(ugraphs)).

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
  H=TreeH-NC-TreeD-Classes,
  writeln(TreeH),
  writeln(NC),
  writeln(TreeD),
  writeln(Classes).

% inizializza la gerarchia albero con thing + numero classi + albero disjoint + dizionario fra nodi e classi
% init_hierarchy(kb{hierarchy:TreeH,nClasses:1,disjointClasses:TreeD,node2classes:Classes})
init_hierarchy(TreeH-1-TreeD-Classes):-
  vertices_edges_to_ugraph([0,'n'],[],TreeH),
  Classes=classes{'n':'http://www.w3.org/2002/07/owl#Nothing',0:'http://www.w3.org/2002/07/owl#Thing'},
  vertices_edges_to_ugraph([],['n'-0,0-'n'],TreeD).

check_disjoint(_TreeH-_NC-TreeD-Classes,C,C1):-
  PC=Classes.find(C),
  PC1=Classes.find(C1),
  edges(TreeD,E),
  \+ memberchk(PC-PC1,E).

add_disjoint_link(TreeH-NC-TreeD0-Classes,C,C1,TreeH-NC-TreeD1-Classes):-
  PC=Classes.find(C),
  PC1=Classes.find(C1),
  ( dif(PC,PC1) -> % check consistenza kb
     add_edges(TreeD0,[PC-PC1,PC1-PC],TreeD1)
    ;
     fail
  ).

add_hierarchy_link(TreeH-NC-TreeD-Classes,C,C1,TreeH-NC-TreeD-Classes):- % già in equivalentClasses
  PC=Classes.find(C),
  PC=Classes.find(C1),!.

add_hierarchy_link(TreeH0-NC-TreeD-Classes0,C,C1,TreeH-NC-TreeD-Classes):- % linkati al contrario C sub D, D sub C -> trasformo in equivalent
  PC=Classes0.find(C),
  PC1=Classes0.find(C1),
  are_subClasses_int(TreeH0-NC-TreeD-Classes0,PC,PC1), % controlla non siano già linkati
  merge_classes_int(TreeH0-NC-TreeD-Classes0,PC,PC1,TreeH-NC-TreeD-Classes),!. % merge_classes deve tenere conto di loop con più classi: C sub D sub E, E sub C

add_hierarchy_link(TreeH0-NC-TreeD-Classes,C,C1,TreeH-NC-TreeD-Classes):- % non linkati
  PC=Classes.find(C),
  PC1=Classes.find(C1),
  del_edges(TreeH0,[0-PC],TreeH1),
  add_edges(TreeH1,[PC1-PC],TreeH).

are_subClasses_int(TreeH-_NC-_TreeD-_Classes,C,C1):-
  neighbours(C1,TreeH,L),
  ( (memberchk(C,L),!) ; (member(EquivalentClasses,L),memberchk(C,EquivalentClasses),!)).

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

% sostituisce gli archi del vecchio nodo con nuovi archi
update_edges([],_,_,[]):-!.

update_edges([PC-N|T],PC,PC1,[PC1-N|TU]):-
  update_edges(T,PC,PC1,TU).

update_edges([N-PC|T],PC,PC1,[N-PC1|TU]):-
  update_edges(T,PC,PC1,TU).

update_edges([N0-N|T],PC,PC1,[N0-N|TU]):-
  dif(N0,PC),
  update_edges(T,PC,PC1,TU).

% unisce due nodi del dict
merge_dict_value(C,C1,CM):-
  is_list(C),
  is_list(C1),!,
  append(C,C1,CM).

merge_dict_value(C,C1,CM):-
  is_list(C),
  \+ is_list(C1),!,
  append(C,[C1],CM).

merge_dict_value(C,C1,CM):-
  \+ is_list(C),
  is_list(C1),!,
  append([C],C1,CM).

merge_dict_value(C,C1,[C,C1]):- !.

  
% aggiunge una classe, se la classe già esiste fallisce
add_class(TreeH0-NC0-TreeD-Classes0,Class,TreeH-NC-TreeD-Classes):-
  \+ _=Classes0.find(Class),
  NC is NC0 + 1,
  Classes=Classes0.put(NC0,Class),
  add_edges(TreeH0,[0-NC0],TreeH). %% classe sotto owl:Thing

% aggiunge una lista di classi
add_classes(H,[],H):- !.

add_classes(H0,[Class|T],H):-
  add_class(H0,Class,H1),!,
  add_classes(H1,T,H).

add_classes(H0,[_|T],H):-
  add_classes(H0,T,H).

% aggiunge un insieme di classi equivalenti, se c'è già un set contenente classi equivalenti li unisce, altrimenti aggiunge. Fallisce se ha giù il nodo con tutte le classi
add_equivalentClasses(TreeH-NC-TreeD-Classes0,ClassList0,TreeH-NC-TreeD-Classes):-
  sort(ClassList0,ClassList),
  Node=Classes0.findOne(ClassList),!,
  EqClasses=Classes0.get(Node),
  ( dif(EqClasses,ClassList) ->
    ( append(EqClasses,ClassList,UnsortedClassList),
      sort(UnsortedClassList,ClassSet),
      Classes=Classes0.put(Node,ClassSet) %% se già c'è nodo non modifico gerarchia ma solo nodo in dict
    )
   ;
    fail
  ).

add_equivalentClasses(TreeH0-NC0-TreeD-Classes0,ClassList,TreeH-NC-TreeD-Classes):-  %% se non c'è nodo lo aggiungo. NOTA: aggiunta di classi deve essere fatta PRIMA della gestione degli assiomi di sottoclasse
  NC is NC0 + 1,
  Classes=Classes0.put(NC0,ClassList),
  add_edges(TreeH0,[0-NC0],TreeH).

%% add_disjountClasses(...) aggiunge classi e verifica non ci sia contraddizione. Fallisce se c'è inconsistenza
add_disjointClasses(H0,ClassList,H):-
  add_classes(H0,ClassList,H1),
  add_single_disjointClass(H1,ClassList,ClassList,H).

add_single_disjointClass(H,[],_,H):- !.

add_single_disjointClass(H0,[C|T],ClassList,H):-
  add_single_disjointClass_int(H0,C,ClassList,H1),
  add_single_disjointClass(H1,T,ClassList,H).


add_single_disjointClass_int(H,_C,[],H):- !.

add_single_disjointClass_int(H0,C,[C|T],H):- !,
  add_single_disjointClass_int(H0,C,T,H).

add_single_disjointClass_int(H0,C,[C1|T],H):-
  ( add_disjoint_link(H0,C,C1,H1) ->
     add_single_disjointClass_int(H1,C,[C1|T],H)
    ;
     fail
  ).

%% add_disjointUnion(classExpression,set(classExpression)) da controllare cosa fa e aggiungere classi. Gestire bene l'assioma.
add_disjointUnion(H0,Class,DisjointUnion,H):-
  add_equivalentClasses(H0,[Class,unionOf(DisjointUnion)],H1),
  add_disjointClasses(H1,DisjointUnion,H).


%% add_subClassOf(...)  deve aggiungere/modificare il ramo, controllando prima che le due classi non siano in un nodo di equivalence. Uno o entrambe le classi possono non essere presenti
add_subClassOf(H0,SubClass,SupClass,H):-
  add_classes(H0,[SubClass,SupClass],H1),
  check_disjoint(H1,SubClass,SupClass),!, % si può proseguire
  add_hierarchy_link(H1,SubClass,SupClass,H).
  





















































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
