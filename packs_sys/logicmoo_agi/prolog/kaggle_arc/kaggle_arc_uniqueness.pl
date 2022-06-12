/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


save_grouped(H,G):-
  sort(G,GS),
  my_asserta_if_new(why_grouped(H,GS)).

my_asserta_if_new((H:-B)):- !,must_det_l( (clause(H,B,Ref),clause(HH,BB,Ref), H+B=@=HH+BB)-> true ; asserta(H:-B)).
my_asserta_if_new(HB):- my_asserta_if_new(HB:-true).

:- dynamic(why_grouped/2).
select_group(Group,How):-
  ((why_grouped(How1,Group1), % dif(Group1,Group2), 
    why_grouped(How2,Group2), 
    Group1\==Group2,
    once((sub_term(E,How1),sub_var(E,How2))),
    %length(Group1,G1), length(Group2,G2), G1>G2,
  once((sub_term(E,How1),sub_var(E,How2))),
  %member(M1,Group1),member(M2,Group2),M1=M2,
  append(Group1,Group2,GroupJ), sort(GroupJ,Group),
  How = [How1,How2]))  *-> true ; why_grouped(How,Group).


:- add_history(what_unique).
what_unique:- what_unique(n=0,n>10).

:- add_history(what_unique(n=0,n>10)).
what_unique(CountMask,GroupSizeMask):-
 what_unique(SharedWith,ObjL,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,TraitCounts,How),
  maplist(tersify,TraitCounts,HTraitSetO),
  maplist(tersify,SharedWith,SharedWithO),
  maplist(tersify,ObjL,ObjLO),
  print_grid(ObjL),
 pt(what_unique(ObjLO=[ActualCount/ActualGroupSize-Trait],sharedWith=SharedWithO,
  setL/listL=SetL/ListL,others=HTraitSetO,how=How,
  groupSizeMask=GroupSizeMask,countMask=CountMask,otherL=OtherL)).

:- style_check(-singleton).
:- add_history(what_unique(SharedWith,ObjL,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,Others,_How)).
:- style_check(+singleton).
what_unique(SharedWith,ObjL,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,TraitCountSets,How):-   
  select_group(Group,How),
  length_criteria(Group,GroupSizeMask),
  length(Group,ActualGroupSize),
  ObjL=[Obj],
  member(Obj,Group),  
  ((select(O,Group,Others),O==Obj) -> true ; Group=Others),
  maplist(each_trait,[Obj|Others],[_-ObjT|TraitList]),
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
   \+ filter_what_unique(SharedWith,ObjL,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How).

:- style_check(-singleton).
filter_what_unique(SharedWith,ObjL,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How):-
  OtherL=<1.

filter_what_unique(SharedWith,ObjL,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How):-
 ListL=SetL, SetL>1.

:- style_check(+singleton).

found_in_w(Trait,List,L):- findall(E,(member(_-Traits,List),sub_term(E,Traits),nonvar(E), \+ \+ (Trait = E) ),L).
found_in_o(Trait,List,L):- findall(Obj,(member(Obj-Traits,List),sub_term(E,Traits),nonvar(E), \+ \+ (Trait =@= E)),L).


%each_1trait(Obj,self(Obj)).
each_1trait(obj(L),T):- !, each_1trait(L,T).
each_1trait(object_shape(L),T):- !, each_1trait(L,T).
each_1trait(L,T):- is_list(L),!,member(E,L),each_1trait(E,T).

each_1trait(T,T):- \+ too_verbose(T). 

each_trait(Obj,Obj-S):- findall(T,each_1trait(Obj,T),L),list_to_set(L,S).


too_unique(P):- compound(P),compound_name_arity(P,F,_),!,too_unique(F).
too_unique(object_indv_id).
too_unique(globalpoints).
%good_overlap(shape).

good_overlap(P):- compound(P),compound_name_arity(P,F,_),!,good_overlap(F).
good_overlap(localpoints).
good_overlap(rotation).

too_non_unique(P):- compound(P),compound_name_arity(P,F,_),!,too_non_unique(F).
too_non_unique(grid_size).
too_non_unique(birth).
too_non_unique(grid).
too_non_unique(changes).

%too_non_unique(mass).

length_criteria(List,P):- compound(P), P=..[F,n,L],C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), P=..[F,L], C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), length(List,I), !, call(call,P,I).
length_criteria(List,N):- length(List,N).


