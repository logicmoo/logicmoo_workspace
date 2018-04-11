/** <module> utility_kb

This module models and manages the hierarchy of the KB's concepts.

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/


%% astrazione della gerarchia

:- module(utility_kb, []).

:- use_module(library(ugraphs)).

:- multifile trill:hierarchy/1.
trill:hierarchy(M:H):-
  init_hierarchy(M,H,Cors,Expls),
  writeln(Cors),
  writeln(Expls).

% inizializza la gerarchia albero con thing + numero classi + dizionario fra nodi e classi
init_hierarchy(Tree-1-Classes):-
  vertices_edges_to_ugraph([0],[],Tree),
  Classes=classes{0:'http://www.w3.org/2002/07/owl#Thing'}.

% aggiunge una classe, se la classe già esiste fallisce
add_class(Tree0-NC0-Classes0,Class,Tree-NC-Classes):-
  \+ Classes0.find(Class),
  NC is NC0 + 1,
  Classes=Classes0.put(NC0,Class),
  add_edges(Tree0,[0-[NC0]],Tree). %% classe sotto owl:Thing

% aggiunge un insieme di classi equivalenti, se c'è già un set contenente classi equivalenti li unisce, altrimenti aggiunge. Fallisce se ha giù il nodo con tutte le classi
add_equivalentClasses(Tree0-NC0-Classes0,ClassList0,Tree-NC-Classes):-
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

add_equivalentClasses(Tree0-NC0-Classes0,ClassList,Tree-NC-Classes):-  %% se non c'è nodo lo aggiungo. NOTA: aggiunta di classi deve essere fatta PRIMA della gestione degli assiomi di sottoclasse
  NC is NC0 + 1,
  Classes=Classes0.put(NC0,ClassList),
  add_edges(Tree0,[0-[NC0]],Tree).

%% add_disjountClasses(...) aggiunge classi e verifica non ci sia contraddizione

%% add_disjointUnion(...) da controllare cosa fa e aggiungere classi. Gestire bene l'assioma.

%% add_subClassOf(...)  deve aggiungere/modificare il ramo, controllando prima che le due classi non siano in un nodo di equivalence. Uno o entrambe le classi possono non essere presenti




%% utility
M.find(C) := Pos :-
  C=M.Pos,
  !.

M.find(C) := Pos :-
  CL=M.Pos,
  memberchk(C,CL),
  !.

M.find(C) := fail,!.


M.findOne(LC) := Pos :-
  member(C,LC),
  Pos=M.find(C),!.

M.findOne(_) := fail,!.




















































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
