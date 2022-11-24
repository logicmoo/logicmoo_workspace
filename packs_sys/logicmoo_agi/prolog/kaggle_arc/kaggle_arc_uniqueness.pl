/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).


saved_group(Why,IndvS):-
  is_why_grouped(_TestID,_Count,Why,IndvS).

is_why_grouped(TestID,Count,Why,IndvS):-
  is_why_grouped_g(TestID,Count,Why,IndvSG),
  maplist(must_oid_to_object,IndvSG,IndvS).

must_oid_to_object(ID,O):- must_det_ll(oid_to_object(ID,O)).

save_grouped(Why,G):-
  into_group(G,GS),
  get_current_test(TestID),
  length(GS,Len),
  mapgroup(register_obj,GS),
  maplist(obj_to_oid,GS,GGG),
  %maplist(obj_to_oid,GS,OIDs),
  my_asserta_if_new(is_why_grouped_g(TestID,Len,Why,GGG)).


normal_group_form(Group,Group):-!.

:- dynamic(is_why_grouped_g/4).
why_grouped(Why,Group):-
  ensure_test(TestID),
  why_grouped(TestID,Why,Group).

why_grouped(TestID,Why,Group):- 
  (is_why_grouped(TestID,_,Why,Group)*->true; 
     ((is_list(Group)->length(Group,Len);true),is_why_grouped(TestID,Len,Why,Grp),same_members(=@=,Group,Grp))).

same_members(P2,G1,G2):- 
  select(E1,G1,GG1),select(E2,G2,GG2),
  call(P2,E1,E2), same_members(P2,GG1,GG2).

%select_group(TestID,Group,How):- no_repeats(Group,select_group0(TestID,Group,How)).
select_group(TestID,Group,How):- select_group0(TestID,Group,How).
select_group0(TestID,Group,How):-
  ((is_why_grouped(TestID,_,How1,Group1), % dif(Group1,Group2), 
    is_why_grouped(TestID,_,How2,Group2),
    Group1\==[], Group2\==[],
    Group1\==Group2,
    once((sub_term(E,How1),sub_var(E,How2))),
    %length(Group1,G1), length(Group2,G2), G1>G2,
  once((sub_term(E,How1),sub_var(E,How2))),
  %member(M1,Group1),member(M2,Group2),M1=M2,
  my_append(Group1,Group2,GroupJ), sort(GroupJ,Group),
  How = [How1,How2])) 
    *-> true ; is_why_grouped(TestID,_,How,Group).

select_group0(TestID,Group,obj_cache):- findall(O,obj_cache(TestID,O,_),GroupJ), GroupJ\==[], sort(GroupJ,Group).

:- arc_history(test_what_unique).
test_what_unique:- get_current_test(TestID), what_unique(TestID,n=0,n>10).


:- arc_history((get_current_test(TestID),what_unique(TestID,n=0,n>10))).
get_new_uniq_dict(Dict):- 
    ArgDict = _{sharedWith:_SharedWith,object:_Obj,trait:_Trait,groupSizeMask:_GroupSizeMask,
  actualGroupSize:_ActualGroupSize,countMask:_CountMask,
  actualCount:_ActualCount,otherL:_OtherL,slistL:_ListL,
  setL:_SetL,others:_TraitCountSets,how:_How,group:_Group},
  (var(Dict)->Dict=ArgDict ; Dict >:< ArgDict).

is_fti_step(most_unique).
most_unique(symmetry,VM):-
  List = VM.objs,
  last(List,Obj),
  set(VM.solution)= Obj.


  

what_unique:- get_current_test(TestID),what_unique(TestID).

what_unique(TestID):- 
   get_vm(VM),
   ((VM.id \= (TestID > _ * _)), ndividuator),
   get_vm(VM2), explain_uniqueness(VM2.objs).

what_unique(TestID,Dict):- is_map(Dict),!,what_unique_dict(TestID,Dict).
what_unique(TestID,Obj):- get_current_test(TestID),select_group(TestID,Group,_How), member(Obj,Group), must_det_ll(what_unique(TestID,Obj,Group)).
what_unique(TestID,Obj,Group):- (is_group(Group);is_object(Obj)),!,what_unique_obj(TestID,Obj,Group).
what_unique(TestID,CountMask,GroupSizeMask):-
  get_new_uniq_dict(Dict),
  Dict.groupSizeMask = GroupSizeMask,
  Dict.countMask = CountMask,!,
  what_unique_dict(TestID,Dict),
  report_unique(Dict).

get_peers(Obj,Peers):- 
  get_current_test(TestID),select_group(TestID,Group,_How), select(Obj,Group,Peers).

peerless_props(O1,Peers,PeerlessProps):-
 must_det_ll(( indv_props(O1,Props),
               (var(Peers)->get_peers(O1,Peers);true),
               (select(O1,Peers,PeersU)->true;PeersU=Peers),
  include(is_peerless_prop(PeersU),Props,PeerlessProps))).

not_peerless_props(O1,Peers,PeerlessProps):-
 must_det_ll(( indv_props(O1,Props),
               (var(Peers)->get_peers(O1,Peers);true),
               (select(O1,Peers,PeersU)->true;PeersU=Peers),
  include(not_peerless_prop(PeersU),Props,PeerlessProps))).

is_peerless_prop(Peers,Prop):- \+ sub_var(Prop,Peers).
not_peerless_prop(Peers,Prop):- sub_var(Prop,Peers).

what_unique_obj:- get_current_test(TestID),what_unique_obj(TestID,_,_).
what_unique_obj(TestID,Obj,Group):- 
  get_new_uniq_dict(Dict),
  Dict.group = Group,
  Dict.object = Obj,
  what_unique_dict(TestID,Dict),
  report_unique(Dict).

/*what_unique(TestID,CountMask,GroupSizeMask):-
 what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,TraitCounts,How),
 report_unique(SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,TraitCounts,How).
*/
report_unique(Dict):- var(Dict),get_new_uniq_dict(Dict),!,report_unique(Dict).
report_unique(Dict):- var(Dict.actualCount),!,get_current_test(TestID), what_unique_dict(TestID,Dict),report_unique(Dict).
report_unique(Dict):-
 must_det_ll((
  ArgDict = _{sharedWith:SharedWith,object:Obj,trait:Trait,groupSizeMask:GroupSizeMask,
  actualGroupSize:ActualGroupSize,countMask:CountMask,
  actualCount:ActualCount,otherL:OtherL,listL:ListL,
  setL:SetL,others:TraitCountSets,how:How,group:Group},
  (var(Dict)->Dict=ArgDict ; Dict >:< ArgDict),
  maplist_e(tersify,TraitCountSets,HTraitSetO),
  maplist_e(tersify,SharedWith,SharedWithO),
  maplist_e(tersify,Group,GroupO),
  maplist_e(tersify,Obj,ObjO),
  %(Obj\==[] -> ignore(print_grid(Obj)) ; true),
  format('~N'), pp(what_unique(ObjO=[ActualCount/ActualGroupSize-Trait],sharedWith=SharedWithO,
  setL/listL=SetL/ListL,others=HTraitSetO,how=How,
  groupSizeMask=GroupSizeMask,group:GroupO,countMask=CountMask,otherL=OtherL)))).

maplist_e(P2,A,B):- is_list(A),!,mapgroup(P2,A,B).
maplist_e(P2,A,B):- call(P2,A,B).

:- style_check(-singleton).
%:- arc_history(what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,Others,_How)).
:- style_check(+singleton).


obj_exclude(Obj,Group,Others):- var(Obj),!,select(Obj,Group,Others).
obj_exclude(Obj,Group,Others):- select(O,Group,Others),(O==Obj *-> true; Group=Others).


what_unique_dict(TestID,Dict):- 
  ArgDict = _{sharedWith:SharedWith,object:Obj,trait:Trait,groupSizeMask:GroupSizeMask,
  actualGroupSize:ActualGroupSize,countMask:CountMask,
  actualCount:ActualCount,otherL:OtherL,listL:ListL,
  setL:SetL,others:TraitCountSets,how:How,group:Group},
  (var(Dict)->Dict=ArgDict ; Dict >:< ArgDict),
  (var(Group)->(select_group(TestID,Group,How));true),
  obj_exclude(Obj,Group,Others),  
  length_criteria(Group,GroupSizeMask),
  length(Group,ActualGroupSize),
  mapgroup(each_trait,[Obj|Others],[_-ObjT|TraitList]),
  member(Trait,ObjT),
  \+ too_non_unique(Trait),
  \+ too_unique(Trait),
  found_in_o(Trait,TraitList,SharedWith),
  length_criteria(SharedWith,CountMask),
  length(SharedWith,ActualCount),
   freeze(B,\+ \+ (member(E,SharedWith), E==B)),
   my_partition(=(B-_),TraitList,_Mine,NotMine),
   length(NotMine,OtherL),   
   %dif(WTrait,Trait),
   functor(Trait,F,A),functor(WTrait,F,A),
   found_in_w(WTrait,NotMine,HTraitList),length(HTraitList,ListL),
   sort(HTraitList,HTraitSet),length(HTraitSet,SetL),
   findall(C-HTrait,(member(HTrait,HTraitSet),found_in_w(HTrait,NotMine,LS),length(LS,C)),TraitCounts),
   sort(TraitCounts,TraitCountSets),
   \+ filter_what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How).


explain_uniqueness(GroupWhole):-
  object_printables(GroupWhole,Group,GroupPP),
  get_current_test(TestID),!,
  forall(member(Obj,Group),
   (dash_chars,
    object_glyph(Obj,G), object_s_glyph(Obj,GC), object_grid(Obj,OG), 
    locally(nb_setval(color_index,[Obj|GroupPP]),print_side_by_side(GC,GroupPP,'explain_uniqueness',_,OG,G)),
    dmsg(uobj=Obj),!,
    forall(what_unique_obj(TestID,Obj,Group),true))),
  dash_chars.


% touching vs each dir
% size2D



:- style_check(-singleton).
filter_what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How):-
  OtherL=<1.

filter_what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How):-
 ListL=SetL, SetL>1.


/*

With each type of example we can have...

values_all_same|values_all_dif
values_where_1_stand_otherwise
values_where_2_stand_otherwise

*/



compare_objects([],[]):-!.
compare_objects(Objs,Interesting):- 
  maplist(indv_props_for_noteablity,Objs,ObjProps),
  flatten(ObjProps,FlatProps),
  maplist(functorize_props,FlatProps,Functors),
  sort(Functors,SortedFunctors),
  gather_props(SortedFunctors,FlatProps,ListOfLists),
  maplist(compare_values,ListOfLists,Diffs),
  include(\=([]),Diffs,Interesting).
  
functorize_props(iz(Prop),FA):- !, functorize_props(Prop,FA).
functorize_props(Prop,F/A):- functor(Prop,F,A).
gather_props([F/A|SortedFunctors],FlatProps,[(F-Candidates)|ListOfLists]):- 
  functor(Match,F,A), findall(Match,(member(Match,FlatProps);member(iz(Match),FlatProps)),Candidates),
  gather_props(SortedFunctors,FlatProps,ListOfLists).
gather_props([],_,[]).


compare_values(F-X,Notable):- predsort(using_compare(number_varz),X,S),length(X,N),length(S,NS),
  is_notable(F-NS/N,Notable).

:- dynamic(repress_non_notables/0).
is_changeable_param(repress_non_notables/0).
repress_non_notables.

:- dynamic(never_noteable/1).
is_changeable_param(never_noteable/1).
never_noteable(colors).
never_noteable(globalpoints).
never_noteable(P):- compound(P),functor(P,F,_),never_noteable(F).

is_prop_for_noteablity(P):- compound(P),functor(P,F,_),is_prop_for_noteablity(F),!.
is_prop_for_noteablity(X):- \+ never_noteable(X),!.

is_notable(_F-N/N,[]):- repress_non_notables, !.  
is_notable(_F-1/_,[]):- repress_non_notables, !.
is_notable(F-_,[]):- never_noteable(F),!.
is_notable(F-N/N,all_diff(F)):-!.
is_notable(F-1/_,all_same(F)):-!.
is_notable(F-S/N,notable(F,S/N)):-!.
%is_notable(F-S/N,Notable):- F-S/N = Notable.

   number_varz(I,C):- copy_term(I,C),numbervars(C,0,_,[attvar(skip)]).

:- style_check(+singleton).

found_in_w(Trait,List,L):- 
  findall(E,(member(_-Traits,List),sub_term(E,Traits),nonvar(E), \+ \+ (Trait = E) ),L).

found_in_o(Trait,List,L):- 
 findall(Obj,(member(Obj-Traits,List),sub_term(E,Traits),nonvar(E), \+ \+ (Trait =@= E)),L).


%each_1trait(Obj,self(Obj)).
each_1trait(Var,T):- var(Var),!, enum_object(Var),each_1trait(Var,T).
each_1trait(obj(L),T):- !, each_1trait(L,T).
each_1trait(iz(L),T):-  !, each_1trait(L,T).
each_1trait(L,T):- is_list(L),!,member(E,L),each_1trait(E,T).

each_1trait(T,T):- \+ too_verbose(T). 

each_trait(Obj,Obj-S):- findall(T,each_1trait(Obj,T),L),list_to_set(L,S).


too_unique(P):- compound(P),!,compound_name_arity(P,F,_),!,too_unique(F).
%too_unique(obj_to_oid).
too_unique(globalpoints).
%too_unique(o).
too_unique(link).
too_unique(obj_to_oid).
too_unique(birth).
%good_overlap(shape).

good_overlap(P):- compound(P),!,compound_name_arity(P,F,_),!,good_overlap(F).
good_overlap(localpoints).
good_overlap(rot2L).

too_non_unique(P):- compound(P),!,compound_name_arity(P,F,_),!,too_non_unique(F).
too_non_unique(grid_size).
too_non_unique(grid_sz).
%too_non_unique(birth).
too_non_unique(grid).
too_non_unique(changes).

%too_non_unique(amass).

length_criteria(List,P):- compound(P), P=..[F,n,L],C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), P=..[F,L], C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), length(List,I), !, call(call,P,I).
length_criteria(List,N):- length(List,N).

tesT_compare_objects:- compare_objects([
    obj([amass(1),shape([point_01_01]),colors([cc(yellow,1.0)]),localpoints([yellow-point_01_01]),
      vis2D(1,1),rot2L(sameR),loc2D(4,9),changes([]),iz(dots),iz(shape(dot)),iz(solid),iz(jagged(true)),center2G(4,9),% obj_to_oid(t(af902bf9)>(tst+0)*in,37),globalpoints([yellow-point_04_09]),
      grid_size(10,10),iz(important)]),
    obj([amass(1),shape([point_01_01]),colors([cc(yellow,1.0)]),localpoints([yellow-point_01_01]),vis2D(1,1),rot2L(sameR),loc2D(4,6),changes([]),iz(dots),iz(shape(dot)),iz(solid),iz(jagged(true)),center2G(4,6),obj_to_oid(t(af902bf9)>(tst+0)*in,39),globalpoints([yellow-point_04_06]),grid_size(10,10),iz(important)]),
    obj([amass(1),shape([point_01_01]),colors([cc(yellow,1.0)]),localpoints([yellow-point_01_01]),vis2D(1,1),rot2L(sameR),loc2D(1,6),changes([]),iz(dots),iz(shape(dot)),iz(solid),iz(jagged(true)),center2G(1,6),obj_to_oid(t(af902bf9)>(tst+0)*in,40),globalpoints([yellow-point_01_06]),grid_size(10,10),iz(important)]),
    obj([amass(1),shape([point_01_01]),colors([cc(yellow,1.0)]),localpoints([yellow-point_01_01]),vis2D(1,1),rot2L(sameR),loc2D(10,5),changes([]),iz(dots),iz(shape(dot)),iz(solid),iz(jagged(true)),center2G(10,5),obj_to_oid(t(af902bf9)>(tst+0)*in,41),globalpoints([yellow-point_10_05]),grid_size(10,10),iz(important)]),
    obj([amass(1),shape([point_01_01]),colors([cc(yellow,1.0)]),localpoints([yellow-point_01_01]),vis2D(1,1),rot2L(sameR),loc2D(6,5),changes([]),iz(dots),iz(shape(dot)),iz(solid),iz(jagged(true)),center2G(6,5),obj_to_oid(t(af902bf9)>(tst+0)*in,42),globalpoints([yellow-point_06_05]),grid_size(10,10),iz(important)]),
    obj([amass(1),shape([point_01_01]),colors([cc(yellow,1.0)]),localpoints([yellow-point_01_01]),vis2D(1,1),rot2L(sameR),loc2D(10,1),changes([]),iz(dots),iz(shape(dot)),iz(solid),iz(jagged(true)),center2G(10,1),obj_to_oid(t(af902bf9)>(tst+0)*in,43),globalpoints([yellow-point_10_01]),grid_size(10,10),iz(important)]),
    obj([amass(1),shape([point_01_01]),colors([cc(yellow,1.0)]),localpoints([yellow-point_01_01]),vis2D(1,1),rot2L(sameR),loc2D(6,1),changes([]),iz(dots),iz(shape(dot)),iz(solid),iz(jagged(true)),center2G(6,1),obj_to_oid(t(af902bf9)>(tst+0)*in,44),globalpoints([yellow-point_06_01]),grid_size(10,10),iz(important)])],
    OUTPUT),
  print(OUTPUT).
