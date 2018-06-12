/** <module> utility_kb

This module models and manages the hierarchy of the KB's concepts.

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/


%% astrazione della gerarchia

:- module(utility_kb, [init_hierarchy/1,create_hierarchy/1,get_hierarchy/3,get_hierarchy/2]).

:- meta_predicate init_hierarchy(:).
:- meta_predicate create_hierarchy(+).
:- meta_predicate get_hierarchy(:,-).
:- meta_predicate get_hierarchy(+,+,-).

:- use_module(library(classes)).
:- use_module(library(ugraphs)).

:- use_module(library(tabling)).
:- table expl_combination/4.

:- table get_combined_expls(_,_,_,_,_,_,lattice(append_expl/3)). %get_hierarchy_ric/6,
%:- table get_single_expls/7. %(_,_,_,_,_,lattice(append_expl)).

:- multifile trill:hierarchy/1.
trill:hierarchy(M:H):-
  ( M:delay_hier(true) -> create_hierarchy(M) ; true ),
  M:kb_hierarchy(H).

create_hierarchy(M):-
  trill:clear_trill_db(M),
  utility_kb:hierarchy_int(M).

hierarchy_int(M):-
  init_hierarchy(M:H0),
  %findall(C,M:class(C),L1),
  %findall(Class,M:classAssertion(Class,_Individual),L2),
  %findall(I,M:namedIndividual(I),LI1),
  %findall(Individual,M:classAssertion(_Class,Individual),LI6),
  %findall(LIS,M:sameIndividual(LIS),LI2),
  %findall(LID,M:differentIndividuals(LID),LI3),
  %append(LI2,LI4),
  %append(LI3,LI5),
  %append([LI1,LI4,LI5,LI6],LIndNS),
  %sort(LIndNS,LInd),
  %append(L1,L2,L3),
  %sort(L3,L4),
  M:kb_atom(KB),
  time(add_classes(H0,KB.class,H01)),
  time(add_individuals(H01,KB.individual,H1)),
  retractall(M:kb_hierarchy(_)),
  assert(M:kb_hierarchy(H1)),
  time(forall(M:equivalentClasses(CL),(M:kb_hierarchy(H2),add_equivalentClasses(H2,CL,H3),retractall(M:kb_hierarchy(_)),assert(M:kb_hierarchy(H3))))),
  time(forall(M:disjointClasses(CL),(M:kb_hierarchy(H4),add_disjointClasses(H4,CL,H5),retractall(M:kb_hierarchy(_)),assert(M:kb_hierarchy(H5))))),
  time(forall(M:disjointUnion(C,D),(M:kb_hierarchy(H6),add_disjointUnion(H6,C,D,H7),retractall(M:kb_hierarchy(_)),assert(M:kb_hierarchy(H7))))),
  time(forall(M:subClassOf(C,D),(M:kb_hierarchy(H8),add_subClassOf(H8,C,D,H9),retractall(M:kb_hierarchy(_)),assert(M:kb_hierarchy(H9))))),
  time(search_and_add_complex_subClassOf(M)).
  %writeln(H.hierarchy),
  %writeln(H.nClasses),
  %writeln(H.disjointClasses),
  %writeln(H.classes),
  %writeln(H.explanations).


search_and_add_complex_subClassOf(M):-
  %M:kb_hierarchy(H10),
  M:kb_atom(KBA),
  %collect_all_classes(H10.classes, Classes),
  findall(_,process_classes_for_complex_subClassOf(M,KBA.class),_).

process_classes_for_complex_subClassOf(M,Classes):-
  complex_subClassOf(M,Classes,C,D,Ex),
  M:kb_hierarchy(H10),
  add_complex_subClassOf(H10,C,D,Ex,H11),
  retractall(M:kb_hierarchy(_)),
  assert(M:kb_hierarchy(H11)),
  fail.

% inizializza la gerarchia albero con thing + numero classi + albero disjoint + dizionario fra nodi e classi
% init_hierarchy(kb{hierarchy:TreeH,nClasses:1,disjointClasses:TreeD,node2classes:Classes})
init_hierarchy(M:kb{usermod:M,hierarchy:TreeH,nClasses:1,nIndividuals:0,disjointClasses:TreeD,classes:Classes,explanations:[],individuals:[]}):-
  vertices_edges_to_ugraph([0,'n'],[],TreeH),
  Classes=classes{'n':'http://www.w3.org/2002/07/owl#Nothing',0:'http://www.w3.org/2002/07/owl#Thing'},
  vertices_edges_to_ugraph([],['n'-0,0-'n'],TreeD).

check_disjoint(KB):-
  \+ check_disjoint_int(KB),!.

check_disjoint_int(KB):-
  edges(KB.disjointClasses,E),  
  member(DC1-DC2,E),
  reachable(DC1,KB.hierarchy,DCL1),
  reachable(DC2,KB.hierarchy,DCL2),
  member(SameNode,[DC1|DCL1]),
  memberchk(SameNode,[DC2|DCL2]),!.

add_disjoint_link(KB0,C,C1,KB):-
  Classes0=KB0.classes,
  PC=Classes0.find(C),
  PC1=Classes0.find(C1),
  ( dif(PC,PC1) -> % check consistenza kb
     ( add_edges(KB0.disjointClasses,[PC-PC1,PC1-PC],TreeD),
       KB=KB0.put(disjointClasses,TreeD)
     )
    ;
     fail
  ).


add_hierarchy_link(KB0,C,C1,KB):- % già in equivalentClasses
  Classes0=KB0.classes,
  PC=Classes0.find(C),
  PC=Classes0.find(C1),!,
  add_subClass_expl(KB0.usermod,KB0.explanations,C,C1,Expls),
  KB=KB0.put(explanations,Expls).

add_hierarchy_link(KB0,C,C1,KB):- % linkati al contrario C sub D, D sub C -> trasformo in equivalent
  Classes0=KB0.classes,
  PC=Classes0.find(C),
  PC1=Classes0.find(C1),
  add_hierarchy_link_int(KB0,PC,PC1,C,C1,KB).

add_hierarchy_link_int(KB0,PC,PC1,C,C1,KB):-
  time(are_subClasses_int(KB0,PC,PC1)),!, % controlla non siano già linkati
  time(merge_classes_int(KB0,PC,PC1,KB1)), % merge_classes deve tenere conto di loop con più classi: C sub D sub E, E sub C
  time(add_subClass_expl(KB1.usermod,KB1.explanations,C,C1,Expls)),
  KB=KB1.put(explanations,Expls).

add_hierarchy_link_int(KB0,PC,PC1,C,C1,KB):- % non linkati
  del_edges(KB0.hierarchy,[0-PC],TreeH1),
  add_edges(TreeH1,[PC1-PC],TreeH),
  add_subClass_expl(KB0.usermod,KB0.explanations,C,C1,Expls),
  KB=KB0.put([hierarchy=TreeH,explanations=Expls]).


add_hierarchy_link(KB0,C,C1,Expl,KB):- % già in equivalentClasses
  Classes0=KB0.classes,
  PC=Classes0.find(C),
  PC=Classes0.find(C1),!,
  add_subClass_expl(KB0.usermod,KB0.explanations,C,C1,Expl,Expls),
  KB=KB0.put(explanations,Expls).

add_hierarchy_link(KB0,C,C1,Expl,KB):- % linkati al contrario C sub D, D sub C -> trasformo in equivalent
  Classes0=KB0.classes,
  PC=Classes0.find(C),
  PC1=Classes0.find(C1),
  add_hierarchy_link_int(KB0,PC,PC1,C,C1,Expl,KB).

add_hierarchy_link_int(KB0,PC,PC1,C,C1,Expl,KB):-
  are_subClasses_int(KB0,PC,PC1),!, % controlla non siano già linkati
  merge_classes_int(KB0,PC,PC1,KB1), % merge_classes deve tenere conto di loop con più classi: C sub D sub E, E sub C
  add_subClass_expl(KB1.usermod,KB1.explanations,C,C1,Expl,Expls),
  KB=KB1.put(explanations,Expls).

add_hierarchy_link_int(KB0,PC,PC1,C,C1,Expl,KB):-
  del_edges(KB0.hierarchy,[0-PC],TreeH1),
  add_edges(TreeH1,[PC1-PC],TreeH),
  add_subClass_expl(KB0.usermod,KB0.explanations,C,C1,Expl,Expls),
  KB=KB0.put([hierarchy=TreeH,explanations=Expls]).


are_subClasses_int(KB,C,C1):-
  reachable(C,KB.hierarchy,L),
  %flatten(L0,L),
  memberchk(C1,L),!.

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

merge_classes_int(KB0,PC,PC1,KB):- % non collegati all'altro direttamente
  edges(KB0.hierarchy,E),
  collect_classes_2_merge(E,PC,PC1,PCL), %contiene tutta la catena da PC a PC1 escluso
  del_vertices(KB0.hierarchy,PCL,TreeH1),
  update_edges(E,PCL,PC1,EU),
  add_edges(TreeH1,EU,TreeH),
  Classes0=KB0.classes,
  C1=Classes0.PC1,
  del_classes_from_dict(PCL,KB0.classes,CL,Classes1),
  merge_dict_value(CL,C1,CM),
  Classes=Classes1.put(PC1,CM),
  KB=KB0.put([hierarchy=TreeH,classes=Classes]).

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
add_class(KB0,Class,KB):-
  Classes0=KB0.classes,
  \+ _=Classes0.find(Class),
  NC0=KB0.nClasses,
  NC is NC0 + 1,
  Classes=Classes0.put(NC0,Class),
  add_edges(KB0.hierarchy,[0-NC0],TreeH), %% classe sotto owl:Thing
  add_subClass_expl(KB0.usermod,KB0.explanations,Class,'http://www.w3.org/2002/07/owl#Thing',Expls),
  KB=KB0.put([hierarchy=TreeH,nClasses=NC,classes=Classes,explanations=Expls]).

% aggiunge una lista di classi
add_classes(H,[],H):- !.

add_classes(H0,[Ind|T],H):-
  add_class(H0,Ind,H1),!,
  add_classes(H1,T,H).

add_classes(H0,[_|T],H):-
  add_classes(H0,T,H).


% aggiunge un individuo, se già esiste fallisce
add_individual(KB0,Ind,KB):-
  Inds0=KB0.individuals,
  \+ member(Ind,Inds0),
  NI0=KB0.nIndividuals,
  NI is NI0 + 1,
  KB=KB0.put([nIndividuals=NI,individuals=[Ind|Inds0]]).

% aggiunge una lista di individui
add_individuals(H,[],H):- !.

add_individuals(H0,[Class|T],H):-
  add_individual(H0,Class,H1),!,
  add_individuals(H1,T,H).

add_individuals(H0,[_|T],H):-
  add_individuals(H0,T,H).

% aggiunge un insieme di classi equivalenti, se c'è già un set contenente classi equivalenti li unisce, altrimenti aggiunge. Fallisce se ha giù il nodo con tutte le classi
add_equivalentClasses(H0,ClassList,H):-
  add_eqClass_hier(H0,ClassList,H1),
  add_subClasses_expl(H1,ClassList,H2),
  add_eqClass_expl(H2,equivalentClasses(ClassList),H).

add_eqClass_hier(KB0,ClassList0,KB):-
  sort(ClassList0,ClassList),
  Classes0=KB0.classes,
  findall(NodeC,(member(OneOfClassList,ClassList),NodeC=Classes0.find(OneOfClassList)),Nodes),
  (length(Nodes,1) -> %% se già c'è un nodo non modifico gerarchia ma solo nodo in dict
    (Nodes=[Node],
     update_eqNode(KB0,Node,ClassList,KB)
    )
   ;
    (Nodes=[PC1|PCL],!, %% se ci sono più nodi faccio merge
     edges(KB0.hierarchy,E),
     del_vertices(KB0.hierarchy,PCL,TreeH1),
     update_edges(E,PCL,PC1,EU),
     add_edges(TreeH1,EU,TreeH),
     Classes0=KB0.classes,
     C1=Classes0.PC1,
     del_classes_from_dict(PCL,KB0.classes,CL,Classes1),
     merge_dict_value(CL,C1,CM),
     Classes=Classes1.put(PC1,CM),
     KB=KB0.put([hierarchy=TreeH,classes=Classes])
    )
  ).

update_eqNode(KB0,Node,ClassList,KB):-
  Classes0=KB0.classes,
  EqClasses=Classes0.get(Node),
  ( is_list(EqClasses) -> EqClassesList = EqClasses ; EqClassesList = [EqClasses]),
  ( dif(EqClassesList,ClassList) ->
    ( append(EqClassesList,ClassList,UnsortedClassList),
      sort(UnsortedClassList,ClassSet),
      Classes=Classes0.put(Node,ClassSet),
      KB=KB0.put(classes,Classes)
    )
   ;
    fail
  ).

add_eqClass_hier(KB0,ClassList,KB):-  %% se non c'è nodo lo aggiungo. NOTA: aggiunta di classi deve essere fatta PRIMA della gestione degli assiomi di sottoclasse
  NC0=KB0.nClasses,
  NC is NC0 + 1,
  Classes0=KB0.classes,
  Classes=Classes0.put(NC0,ClassList),
  add_edges(KB0.hierarchy,[0-NC0],TreeH),
  KB=KB0.put([hierarchy=TreeH,nClasses=NC,classes=Classes]).

add_subClasses_expl(KB,[],KB):-!.

add_subClasses_expl(KB0,[Class|List],KB):-
  add_subClass_expl(KB0.usermod,KB0.explanations,Class,'http://www.w3.org/2002/07/owl#Thing',Expls1),
  KB1=KB0.put(explanations,Expls1),
  add_subClasses_expl(KB1,List,KB).


add_eqClass_expl(KB0,Ax,KB):-
  add_eqClass_simple_expl(KB0,Ax,KB).%1),
%  add_eqClass_complex_expl(H1,Ax,H).
  
/*
add_eqClass_expl(KB0,ClassList,ClassList,KB):-
  add_eqClass_simple_expl(KB0,ClassList,ClassList,equivalentClasses(ClassList),KB1),
  add_eqClass_complex_expl(KB1,equivalentClasses(ClassList),KB).
*/

% aggiunge spiegazioni fra i componenti del singolo assioma
add_eqClass_simple_expl(KB0,equivalentClasses(ClassList),KB):-
  add_eqClass_simple_expl(KB0.usermod,KB0.explanations,ClassList,ClassList,equivalentClasses(ClassList),Expls),
  KB=KB0.put(explanations,Expls).

add_eqClass_simple_expl(KB0,disjointUnion(Class,DisjList),KB):-
  add_eqClass_simple_expl(KB0.usermod,KB0.explanations,[Class,unionOf(DisjList)],[Class,unionOf(DisjList)],disjointUnion(Class,DisjList),Expls),
  KB=KB0.put(explanations,Expls).

add_eqClass_simple_expl(_M,Expls,[],_L,_Ax,Expls):- !.

add_eqClass_simple_expl(M,Expls0,[C|T],L,Ax,Expls):-
  add_eqClass_simple_expl(M,Expls0,C,T,L,Ax,Expls1),
  add_eqClass_simple_expl(M,Expls1,T,L,Ax,Expls).

add_eqClass_simple_expl(_M,E,_C,[],_L,_Ax,E):- !.

add_eqClass_simple_expl(M,E0,C,[C|T],L,Ax,E):- !,
  add_eqClass_simple_expl(M,E0,C,T,L,Ax,E).

add_eqClass_simple_expl(M,E0,C,[C1|T],L,Ax,E):-
  trill:ax2ex(M,Ax,ExAx),
  ( member(ex(C,C1)-Ex,E0) ->
     ( member(ex(C1,C)-ExC,E0),
       delete(E0,ex(C,C1)-Ex,E1),
       delete(E1,ex(C1,C)-ExC,E2),
       trill:or_f(M,ExAx,Ex,ExOr),
       trill:or_f(M,ExAx,ExC,ExCOr),
       add_eqClass_simple_expl(M,[ex(C,C1)-ExOr,ex(C1,C)-ExCOr|E2],C,T,L,Ax,E)
     )
    ;
     add_eqClass_simple_expl(M,[ex(C,C1)-ExAx,ex(C1,C)-ExAx|E0],C,T,L,Ax,E)
  ).

% combina le spiegazioni per tutti i membri dell'equivalent axiom
add_eqClass_complex_expl(KB0,equivalentClasses(ClassList),KB):-
  member(C,ClassList),!, % prendo una classe a caso
  Classes0=KB0.classes,
  PC=Classes0.find(C),
  combine_eqClass_expl(KB0.usermod,KB0.explanations,Classes0.PC,Expls),
  KB=KB0.put(explanations.Expls).

add_eqClass_complex_expl(KB0,disjointUnion(Class,_ClassList),KB):-
  Classes0=KB0.classes,
  PC=Classes0.find(Class),
  combine_eqClass_expl(KB0.usermod,KB0.explanations,Classes0.PC,Expls),
  KB=KB0.put(explanations.Expls).

combine_eqClass_expl(_M,E,[],E):- !.

combine_eqClass_expl(M,Expls0,[C|T],Expls):-
  combine_eqClass_expl(M,Expls0,C,T,Expls1),
  combine_eqClass_expl(M,Expls1,T,Expls).

combine_eqClass_expl(_M,Expls0,_C,[],Expls):- !,
  sort(Expls0,Expls).

combine_eqClass_expl(M,Expls0,C,[C1|T],Expls):-
  %abolish_table_subgoals(expl_combination(_,_,_,_,_,_)),
  findall(Ex,expl_combination(M,Expls0,C,C1,[],Ex),Exs0),
  combine_all(M,Exs0,Exs),
  ( member(ex(C,C1)-Exs0,Expls0) ->
     ( member(ex(C1,C)-Exs0C,Expls0),
       delete(Expls0,ex(C,C1)-Exs0,Expls1),
       delete(Expls1,ex(C1,C)-Exs0C,Expls2),
       combine_eqClass_expl(M,[ex(C,C1)-Exs,ex(C1,C)-Exs|Expls2],C,T,Expls)
     )
    ;
     combine_eqClass_expl(M,[ex(C,C1)-Exs,ex(C1,C)-Exs|Expls0],C,T,Expls)
  ).

expl_combination(_M,Expls,C,C1,Used,Ex):-
  member(ex(C,C1)-Ex0,Expls),
  \+ (memberchk(C,Used), memberchk(C1,Used)),
  member(Ex,Ex0).

expl_combination(M,Expls,C,C1,Used,Ex):-
  member(ex(C,C0)-Ex0,Expls),
  dif(C0,C1),dif(C0,'http://www.w3.org/2002/07/owl#Thing'),
  \+ (memberchk(C,Used), memberchk(C0,Used)),
  member(Ex01,Ex0),
  expl_combination(M,Expls,C0,C1,[C,C0|Used],Ex1),
  trill:and_f(M,Ex01,Ex1,Ex).

combine_all(M,[],Ex):-
  trill:empty_expl(M,Ex).

combine_all(M,[H|T],Ex):-
  combine_all(M,T,Ex0),
  trill:or_f(M,Ex0,H,Ex).

%% add_disjountClasses(...) aggiunge classi e verifica non ci sia contraddizione. Fallisce se c'è inconsistenza
add_disjointClasses(KB0,ClassList,KB):-
  add_disjClass_hier(KB0,ClassList,KB1),
  add_disjClass_expl(KB1,disjointClasses(ClassList),KB).
  
add_disjClass_hier(KB0,ClassList,KB):-
  %add_classes(KB0,ClassList,KB1),
  add_single_disjointClass(KB0,ClassList,KB).

add_single_disjointClass(KB,[],KB):- !.

add_single_disjointClass(KB0,[C|T],KB):-
  add_single_disjointClass_int(KB0,C,T,KB1),
  add_single_disjointClass(KB1,T,KB).


add_single_disjointClass_int(KB,_C,[],KB):- !.

add_single_disjointClass_int(KB0,C,[C|T],KB):- !,
  add_single_disjointClass_int(KB0,C,T,KB).

add_single_disjointClass_int(KB0,C,[C1|T],KB):-
  ( add_disjoint_link(KB0,C,C1,KB1) ->
     add_single_disjointClass_int(KB1,C,T,KB)
    ;
     fail
  ).

add_disjClass_expl(KB0,Ax,KB):-
  (Ax=..[disjointClasses,Arg] ; Ax=..[disjointUnion,_,Arg]),
  add_disjClass_expl(KB0.usermod,KB0.explanations,Arg,Ax,Expls),
  KB=KB0.put(explanations,Expls).

add_disjClass_expl(_M,Expls,[],_Ax,Expls):- !.

add_disjClass_expl(M,Expls0,[C|T],Ax,Expls):-
  add_disjClass_expl(M,Expls0,C,T,Ax,Expls1),
  add_disjClass_expl(M,Expls1,T,Ax,Expls).

add_disjClass_expl(_M,E,_C,[],_Ax,E):- !.

add_disjClass_expl(M,E0,C,[C|T],Ax,E):- !,
  add_disjClass_expl(M,E0,C,T,Ax,E).

add_disjClass_expl(M,E0,C,[C1|T],Ax,E):-
  trill:ax2ex(M,Ax,ExAx),
  ( member(dis(C,C1)-Ex,E0) ->
      ( delete(E0,dis(C,C1)-Ex,E1),
        trill:or_f(M,ExAx,Ex,ExOr),
        add_disjClass_expl(M,[dis(C,C1)-ExOr|E1],C,T,Ax,E)
      )
    ;
      add_disjClass_expl(M,[dis(C,C1)-ExAx|E0],C,T,Ax,E)
  ).

%% add_disjointUnion(classExpression,set(classExpression)) da controllare cosa fa e aggiungere classi. Gestire bene l'assioma.
add_disjointUnion(KB0,Class,DisjointUnion,KB):-
  add_eqClass_hier(KB0,[Class,unionOf(DisjointUnion)],KB1),
  add_disjClass_hier(KB1,DisjointUnion,KB2),
  add_eqClass_expl(KB2,disjointUnion(Class,DisjointUnion),KB3),
  add_disjClass_expl(KB3,disjointUnion(Class,DisjointUnion),KB).

%% add_subClassOf(...)  deve aggiungere/modificare il ramo, controllando prima che le due classi non siano in un nodo di equivalence. Uno o entrambe le classi possono non essere presenti
add_subClassOf(KB0,SubClass,SupClass,KB):-
  %add_classes(KB0,[SubClass,SupClass],KB1),
  time(add_hierarchy_link(KB0,SubClass,SupClass,KB)),
  check_disjoint(KB),!. % si può proseguire

add_subClass_expl(M,Expls0,C,C1,[ex(C,C1)-ExF|Expls]):-
  member(ex(C,C1)-Ex,Expls0),!,
  delete(Expls0,ex(C,C1)-Ex,Expls),
  trill:ax2ex(M,subClassOf(C,C1),ExAx),
  trill:or_f(M,ExAx,Ex,ExF).

add_subClass_expl(M,Expls,C,C1,[ex(C,C1)-ExAx|Expls]):-
  trill:ax2ex(M,subClassOf(C,C1),ExAx).

add_subClass_expl(M,Expls0,C,C1,Expl,[ex(C,C1)-ExF|Expls]):-
  member(ex(C,C1)-Ex,Expls0),!,
  delete(Expls0,ex(C,C1)-Ex,Expls),
  trill:or_f(M,Expl,Ex,ExF).

add_subClass_expl(_M,Expls,C,C1,Expl,[ex(C,C1)-Expl|Expls]).


% TODO aggiungere eventuali altri sottoclasse (someValuesFrom(R,C)->somevaluesFrom(R,D) con subClassOf(C,D))


get_hierarchy(M:Class,H4C):- %prende la gerarchia (KB) una classe e la spiegazione per arrivare a quella classe e resituisce l'insieme di tutte le classi con spiegazioni da quella in su
  M:kb_hierarchy(KB),
  Classes=KB.classes,
  Pos=Classes.find(Class),
  edges(KB.hierarchy,E),
  get_combined_expls(KB.usermod,Class,Pos,E,Classes,KB.explanations,MH4C), MH4C = (_M,H4C).

get_hierarchy(KB,Class,H4C):- %prende la gerarchia (KB) una classe e la spiegazione per arrivare a quella classe e resituisce l'insieme di tutte le classi con spiegazioni da quella in su
  Classes=KB.classes,
  Pos=Classes.find(Class),
  edges(KB.hierarchy,E),
  get_combined_expls(KB.usermod,Class,Pos,E,Classes,KB.explanations,MH4C), MH4C = (_M,H4C).

get_combined_expls(M,Class,Pos,E,Classes,Expls,(M,H4C)):-
  get_single_expls(M,Class,Pos,E,Classes,Expls,[Class],H4C).

append_expl((M,AllExpl),(M,[EndClass-NewExpl]),(M,NewAllExpl)):-
  \+ memberchk(EndClass-_,AllExpl),!,
  append(AllExpl,[EndClass-NewExpl],NewAllExpl).

append_expl((M,AllExpl),(M,[EndClass-NewExpl]),(M,NewAllExpl)):-
  member(EndClass-OldExpl,AllExpl),
  delete(AllExpl,EndClass-OldExpl,AllExpl0),
  trill:or_f(M,OldExpl,NewExpl,NewExplT),
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

get_single_expls(_M,Class,P,E,Classes,Expls,Used,[NextClass-Expls4Class]):-
  get_next(P,E,Classes,_NextP,NextClass),
  \+ member(NextClass,Used),
  member(ex(Class,NextClass)-Expls4Class,Expls).

get_single_expls(M,Class,P,E,Classes,Expls,Used,[EndClass-TotExpl]):-
  get_next(P,E,Classes,NextP,NextClass),
  \+ member(NextClass,Used),
  member(ex(Class,NextClass)-Expls4Class,Expls),
  get_single_expls(M,NextClass,NextP,E,Classes,Expls,[NextClass|Used],[EndClass-Expls4EndClass]),
  trill:and_f(M,Expls4Class,Expls4EndClass,TotExpl).%,
%  sort(TotExpl0,TotExpl),
%  length(TotExpl0,LTE),
%  length(TotExpl,LTE).


/*
get_single_expls(Class,P,E,Classes,Expls,Start,[NextClass-[[equivalentClasses(ListExpls4Class)]]]):-
  get_next(P,E,Classes,P,NextClass),
  dif(NextClass,Start),
  member(ex(NextClass,Class)-Exs,Expls),
  member([equivalentClasses(ListExpls4Class)],Exs).

get_single_expls(Class,P,E,Classes,Expls,Start,[EndClass-[TotExpl]]):-
  get_next(P,E,Classes,P,NextClass),
  dif(NextClass,Start),
  member(ex(NextClass,Class)-Exs,Expls),
  member([equivalentClasses(ListExpls4Class)],Exs),
  get_single_expls(NextClass,P,E,Classes,Expls,Start,[EndClass-[Expl4EndClass]]),
  append([equivalentClasses(ListExpls4Class)],Expl4EndClass,TotExpl).
*/

/*
get_one_expl(M,Pos,Class,E,Classes,Expls,Expl4Class,EndClass-H4C):-
  get_hierarchy_ric(M,Pos,Class,E,Classes,Expls,EndClass-Expl),
  trill:and_f(M,Expl4Class,Expl,H4C).

get_hierarchy_ric(M,0,_C,_E,_Classes,_Expls,0-Expl):- % arrivato a owl:Thing
  trill:empty_expl(M,Expl).

get_hierarchy_ric(M,n,_C,_E,_Classes,_Expls,n-Expl):- % arrivato a owl:Nothing
  trill:empty_expl(M,Expl).

get_hierarchy_ric(M,P,Class,E,Classes,Expls,EndClass-Expl):-
  EqClasses=Classes.P,
  is_list(EqClasses),
  member(NextClass,EqClasses),
  member(ex(Class,NextClass)-Exs,Expls),
  member(Ex0,Exs),
  get_hierarchy_ric(M,P,NextClass,E,Classes,Expls,EndClass-Ex1),
  trill:and_f(M,Ex0,Ex1,Expl).

get_hierarchy_ric(M,P,Class,E,Classes,Expls,EndClass-Expl):-
  member(NextP-P,E),
  (owl_f(NextP) -> 
    ( Expl = [], EndClass = Class)
   ;
    ( NextClass=Classes.NextP,
      member(ex(Class,NextClass)-Exs,Expls),
      member(Ex0,Exs),
      get_hierarchy_ric(M,NextP,NextClass,E,Classes,Expls,EndClass-Ex1),
      trill:and_f(M,Ex0,Ex1,Expl)
    )
  ).
*/

%% add_complex_subClassOf(...)  deve aggiungere/modificare il ramo, controllando prima che le due classi non siano in un nodo di equivalence. Uno o entrambe le classi possono non essere presenti
add_complex_subClassOf(KB0,SubClass,SupClass,Expl,KB):-
  add_classes(KB0,[SubClass,SupClass],KB1),
  add_hierarchy_link(KB1,SubClass,SupClass,Expl,KB),
  check_disjoint(KB),!. % si può proseguire


collect_all_classes(DictClasses,Classes):-
  findall(C,get_dict(_,DictClasses,C),CL0),
  flatten(CL0,CL1),
  sort(CL1,Classes).


complex_subClassOf(M,Classes,C,D,Expl):-
  member(C,Classes),
  trill:find_sub_sup_class(M,C,D,Ax),
  trill:ax2ex(M,Ax,Expl).

complex_subClassOf(M,Classes,complementOf(C),D,Expl):-
  member(complementOf(C),Classes),
  trill:find_neg_class(C,D),
  trill:ax2ex(M,equivalentClasses([complementOf(C),D]),Expl).

complex_subClassOf(M,Classes,intersectionOf(Cs),D,Expl):-
  member(intersectionOf(Cs),Classes),
  member(D,Cs),
  trill:initial_expl(M,Expl).

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
