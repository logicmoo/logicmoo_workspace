/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
/*

Rule types

i1 to o1
i2 to o1
o1 to o1


*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).

show_interesting_props(Named,ObjsI,ObjsO):-
  banner_lines(cyan,4),
  show_interesting_named_props(input(Named),ObjsI),
  banner_lines(white,2),
  show_interesting_named_props(output(Named),ObjsO),
  banner_lines(cyan,4).

show_interesting_props(_Named,ObjsI,ObjsO):-
   append(ObjsO,ObjsI,Objs),
   show_interesting_props_gojs(Objs).

show_interesting_props_gojs(Objs):- u_dmsg(show_interesting_props_gojs(Objs)).
  %8731374e


show_interesting_named_props(Named,Objs):-
   show_three_interesting_groups(Named,Objs,Groups),
   banner_lines(cyan,3),
   forall(member(G,Groups),
     show_interesting_group(Named,G)).

show_interesting_group(Named,Title-Objs):- 
  banner_lines(cyan,2),
  pp(cyan, Title),
   print_ss(group(Named,Title)=Objs),
   banner_lines(cyan,1).

show_three_interesting_groups(Named,In,Groups):-
  extend_obj_proplist(In,Objs),
  findall(Prop,(member(obj(O),Objs),member(Prop,O), \+ skip_ku(Prop) ),Props),
  sort_safe(Props,SProps),
  print_interesting_named_groups(props(Named),SProps),
  maplist(make_unifier,SProps,UProps), predsort(using_compare(numbered_vars),UProps,SUProps),  
  print_interesting_named_groups(suprops(Named),SUProps),
  %count_each(SProps,Props,GroupsWithCounts),
  length(Objs,L),
  group_quals(SUProps,SProps,L,KUProps),
  print_interesting_named_groups(kuprops(Named),KUProps),
  objs_with_props(KUProps,Objs,L,Groups),
  nop(print_ss(groups=Groups)).

print_interesting_named_groups(Named,KUProps):- 
   w_section(title(Named),pp(KUProps)).

numbered_vars(A,B):- copy_term(A,B),numbervars(B,0,_,[attvars(skip)]).

skip_ku(link(_,_)).
skip_ku(iz(media(_))).
skip_ku(changes(_)).
skip_ku(o(_,_,_,_)).
skip_ku(pg(_,_,_,_)).
skip_ku(cc(C,_)):- is_color(C).

objs_with_props([KU-_|Props],Objs,OL,GO):- skip_ku(KU),!,objs_with_props(Props,Objs,OL,GO).

objs_with_props([KU-_|Props],Objs,OL,GO):-
  findall(O,(member(O,Objs),has_prop(KU,O)),KL), length(KL,KLL), 
  KLL==OL,!,objs_with_props(Props,Objs,OL,GO).
objs_with_props([_-List|Props],Objs,OL,GO):-
  findall(Prop-OO,(member(Prop,List),
     (findall(O,(member(O,Objs),has_prop(Prop,O)),OO))),         G),
  objs_with_props(Props,Objs,OL,Groups),
  append(G,Groups,GO).
objs_with_props([],_,_,[]).

group_quals([U|SUProps],SProps,L,[U-ListUS|KUProps]):- findall(U,member(U,SProps),ListU),
  length(ListU,LUL),LUL\==L, sort_safe(ListU,ListUS),
   ListUS=[_,_|_],!,group_quals(SUProps,SProps,L,KUProps).
group_quals([_|SUProps],SProps,L,KUProps):-  group_quals(SUProps,SProps,L,KUProps).
group_quals([],_SProps,_,[]).



dont_change(Var):- copy_term(Var,VarC),freeze(VarC,fail), freeze(Var,Var=@=VarC).


select_subgroup(Objs,GroupName,Count,SubObjs):- 
  into_group(Objs,GObjs),
  findall(O-GroupName,(member(O,GObjs),is_in_subgroup(O,GroupName)),Memberships),
  maplist(arg(2),Memberships,GroupNames),
  get_ccs(GroupNames,GroupNameCC),
  member(cc(GroupName,Count),GroupNameCC),
  findall(O,member(O-GroupName,Memberships),SubObjs).

object_prop(O,Prop):- indv_props_list(O,Props),member(Prop,Props).

objects_props(SubObjs,Props):-
  findall(Prop,(member(O,SubObjs),object_prop(O,Prop)),Props).
objects_names_props(SubObjs,Props):-
  findall(F-Prop,(member(O,SubObjs),object_prop(O,Prop),prop_name(Prop,F)),Props).

prop_name(Prop,F):- \+ compound(Prop),!,F=Prop.
prop_name(Prop,Name):- compound_name_arguments(Prop,F,[A|_]),
   (number(A)-> Name =F ; compound_name_arguments(Name,F,[A])).

object_group_cc(Objs,GroupName,SubObjs,Count,NamesCC,ValuesCC):-
  select_subgroup(Objs,GroupName,Count,SubObjs),
  objects_names_props(SubObjs,Props),
  maplist(arg(1),Props,Names),get_ccs(Names,NamesCC),
  maplist(arg(2),Props,Values),get_ccs(Values,ValuesCC).

group_prop(P):- group_prop(P,_).
group_prop(giz(g(in)),input).
group_prop(giz(g(out)),output).
group_prop(cc(fg,0),bg_obj).
group_prop(cc(bg,0),fg_obj).

group_prop(fg_colors_count(1),single_fg_color).
group_prop(fg_colors_count(Two),multicolor):- freeze(Two,Two>1).


is_in_subgroup_c(Obj,holes(Count)):- findall(Y, (indv_props(Obj,link(contained_by,Y)),is_bg_object(Y)), L), length(L,Count).
is_in_subgroup_c(Obj,fg_parts(Count)):- findall(Y, (indv_props(Obj,link(contained_by,Y)),is_fg_object(Y)), L), length(L,Count).
is_in_subgroup_c(Obj,bg_containers(Count)):- findall(Y, (indv_props(Obj,link(contains,Y)),is_bg_object(Y)), L), length(L,Count).
is_in_subgroup_c(Obj,fg_containers(Count)):- findall(Y, (indv_props(Obj,link(contains,Y)),is_fg_object(Y)), L), length(L,Count).
is_in_subgroup_c(Obj,fg_overlap(Count)):- findall(Y, (indv_props(Obj,link(overlaps(_,_),Y)),is_fg_object(Y)), L), length(L,Count).
is_in_subgroup_c(Obj,fg_overlapped_by(Count)):- findall(Y, (indv_props(Obj,link(overlapped_by(_,_),Y)),is_fg_object(Y)), L), length(L,Count).


is_in_subgroup(Obj,Prop):- var(Obj),!, enum_object(Obj),is_in_subgroup(Obj,Prop).
is_in_subgroup(Obj,iz(IZ)):- group_prop(Prop,IZ), has_prop(Prop,Obj).
is_in_subgroup(Obj,Prop):- is_in_subgroup_c(Obj,Prop).
is_in_subgroup(Obj,nth_fg_color(Nth,Color)):- unique_fg_colors(Obj,List),nth1(Nth,List,Color).
%is_in_subgroup(Obj,ansestors(N,Set)):-transitive_sets(ansestor,Obj,Set,N).
%is_in_subgroup(Obj,descendants(N,Set)):-transitive_sets(descendant,Obj,Set,N).
%is_in_subgroup(Obj,tiouching(N,Set)):- nontransitive_set(touching,Obj,Set,N).
%is_in_subgroup(Obj,seeing(N,Set)):- nontransitive_set(seeing,Obj,Set,N).
%is_in_subgroup(Obj,insideOf(N,Set)):-transitive_sets(insideOf,Obj,Set,N).
%is_in_subgroup(Obj,contains(N,Set)):-transitive_sets(contains,Obj,Set,N).
%is_in_subgroup(Obj,Prop):- has_prop(Prop,Obj).
%is_in_subgroup(_,all).




transitive_sets(P2,Obj,Set,N):- findall(n(P,List),(trans(P2,Obj,List),List\==[],length(List,P)),Lists),sort_safe(Lists,Set),length(Set,N).
nontransitive_set(P2,Obj,Set,N):- findall(Other,call(P2,Obj,Other),List),sort_safe(List,Set),length(Set,N).

trans(P2,Obj,Out):- obj_to_oid(Obj,OID),trans_no_loop(P2,[OID],Obj,Out).

trans_no_loop(P2,Skip,Obj,Out):- 
  ((call_oid_objs(P2,Obj,Other),v_obj(Other,That), \+ member(That,Skip))
    *->(Out=[Other|Others],trans_no_loop(P2,[That|Skip],That,Others))
    ;Out=[]).

v_obj(v(OID,_Info),OID):-!.
v_obj(Obj,Obj).

call_oid_objs(P2,OID,Other):- atom(OID),!,oid_to_obj(OID,Obj),call(P2,Obj,Other).
call_oid_objs(P2,Obj,Other):- call(P2,Obj,Other).

ansestor(Obj,Other):- has_prop(link(subsume,Other,subsumed_by(_,_)),Obj).
descendant(Obj,Other):- has_prop(link(subsume,Other,subsuming(_,_)),Obj).
touching(Obj,v(Other,Info)):- has_prop(link(dir_touching,Other,Info),Obj).
seeing(Obj,v(Other,Info)):- has_prop(link(dir_seeing,Other,Info),Obj).
insideOf(Obj,Other):- has_prop(link(insideOf,Other,_),Obj).
contains(Obj,Other):- has_prop(link(contains,Other,_),Obj).


% =====================================================================
is_fti_step(extend_obj_proplists).
% =====================================================================
%really_group_vm_priors(_VM):-!.
extend_obj_proplists(VM):- extend_obj_proplist(VM.objs,set(VM.objs)).


% =====================================================================
is_fti_step(group_vm_priors).
% =====================================================================
group_vm_priors(VM):-
 if_arc_option(group_vm_priors,
  really_group_vm_priors(VM)).

% =====================================================================
is_fti_step(really_group_vm_priors).
% =====================================================================
%really_group_vm_priors(_VM):-!.
really_group_vm_priors(VM):-
 must_det_ll((
  ObjsG = VM.objs,
  %print_list_of(ppnl,ObjsG),
  TID_GID=tid_gid(VM.id,VM.gid),
  check_tid_gid(TID_GID,VM.grid_o),
  group_prior_objs(TID_GID,ObjsG,Objs),  
  gset(VM.objs) = Objs)).

% bg from fg
relivant_divide(is_fg_object).
relivant_group(G):- relivant_divide(G).
relivant_group(not_has_prop(mass(1))).
%relivant_divide(has_prop(cc(bg,0))).
% seperate input from output
%relivant_divide(has_prop(giz(g(in)))).
%relivant_divide(unique_fg_color_count_eq_1).

unique_fg_color_count_eq_1(Obj):- unique_fg_colors(Obj,II),II=1.

group_prior_objs(Why,ObjsIn,WithPriors):- 
 relivant_divide(RelivantDivide),
 my_partition(RelivantDivide,ObjsIn,FG,BG),
 ((FG\==[],BG\==[])),!,
 print_ss([splitting_group(RelivantDivide)=FG,splitting_group(RelivantDivide)=BG]), 
 group_prior_objs(Why,FG,FGWithPriors),
 group_prior_objs(Why,BG,BGWithPriors),
 append(FGWithPriors,BGWithPriors,WithPriors),!. 

group_prior_objs(Why,ObjsIn,WithPriors):- fail,
 once(combine_same_globalpoints(ObjsIn,Objs)),
 ObjsIn\=@=Objs,!,
 group_prior_objs(Why,ObjsIn,WithPriors).

group_prior_objs(Why,Objs,WithPriors):- 
 must_det_ll((
 %print_list_of(debug_as_grid,group_prior_objs,Objs),!,
 get_prior_labels(Objs,Lbls),
 keysort(Lbls,N),
 length(N,Len),
 Title = Why+Len,
 nop(noisey_debug(print_premuted_objects(Title))),
 w_section(title(add_priors(Title)),
  %print_tree(groupPriors=Lbls,[max_depth(200)]),
  add_priors(Lbls,Objs,WithPriors)))).

print_premuted_objects(Why,Objs):- Objs==[],!,dmsg(no_objs(Why)).
print_premuted_objects(Why,Objs):- 
  must_det_ll((
  orule_first(Objs,ORF), reverse(ORF,RORF),
  visible_first(ORF,VF), reverse(VF,RVF),
  largest_first(ORF,LF),
  smallest_first(ORF,SF),
  %sort_by_vertical_size(Objs,Sorted),
  grid_size(Objs,IH,IV),!,
  create_vis_layers([],IH,IV,0,RORF,Sets),!,
  %nop(maplist(title_objs,ORF,TitledObjs)),
  %maplist(obj_short_props,ORF,TitledObjs),
  pp(breakdown_of(Why)),
  append([
     orule_first=ORF,
     rev_orule_first=RORF,
     visible_first=VF,
     rev_visible_first=RVF,
     largest_first=LF,
     smallest_first=SF],Sets,SS),
  print_ss(SS),
  print_obj_short_props(ORF))).


create_vis_layers(_Fallback,_IH,_IV,_LayerNum,[],[]):-!.
create_vis_layers(Fallback,IH,IV,LayerNum,InC,[layer(LayerNum2)=BestOrder|Rest]):- 
  filter_shown(IH,IV,InC,InShown,InHidden),!,
  LayerNum2 is LayerNum+1,
  append(InShown,Fallback,Fallback2),
  list_to_set(Fallback2,BestOrder),
  ((InHidden==[];equal_sets(InHidden,InC))-> Rest=[] ; create_vis_layers(Fallback2,IH,IV,LayerNum2,InHidden,Rest)).

equal_sets(A,B):- sort_safe(A,AA),sort_safe(B,BB),AA=@=BB.


% sprop_piority(Class,Priority).

%sprop_piority(pg(OG,_,_,0),0).
%sprop_piority(birth(i3(_)),0).
%sprop_piority(birth(i2(_)),0).
sprop_piority(iz(flag(hidden)),9).
sprop_piority(iz(media(shaped)),0).
sprop_piority(iz(info(combined)),1).
sprop_piority(iz(media(image)),2).
sprop_piority(iz(info(by_color)),3).
sprop_piority(iz(info(glyphic)),2).


resize_inf(X,N):- is_list(X),!,length(X,N).
resize_inf(X,X).

member_or_iz(Prop,Ps):- member(Prop,Ps).
member_or_iz(Prop,Ps):- member(iz(Prop),Ps).
member_or_iz(Prop,Ps):- member(birth(Prop),Ps).
member_or_iz(Prop,Ps):- member(giz(Prop),Ps).

combine_number(_F1,O1,O2,O):- O is ((abs(O1-O2)+1)*O1)+(O2*30).
%combine_number(_F1,O1,O2,O):- O is (abs(O1-O2)+1)*O1+(O2*30).


visible_first(IndvS0,IndvO):- predsort_two_p2(visible_priority,visible_first_order,IndvS0,IndvO).
%visible_pred(F1,I,O):- ranking_pred(F1,I,O),!.
visible_first_order(I,VArea):-  vis_area(I,Area), globalpoints(I,Ps),length(Ps,L), VArea is Area/L.
visible_priority(Indv,Priority):- mass(Indv,1),!,Priority=7.
visible_priority(Indv,Priority):- mass(Indv,0),!,Priority=8.
visible_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
visible_priority(_,1):-!.

predsort_two_p2(P2a,P2b,IndvS,IndvO):-
  findall((SortOn1+SortOn2)-Indv,(member(Indv,IndvS),call(P2a,Indv,SortOn1),call(P2b,Indv,MSize),resize_inf(MSize,SortOn2)),All),
  keysort(All,AllK),
  maplist(arg(2),AllK,IndvO).  

orule_first(IndvS0,IndvO):- predsort_two_p2(orule_priority,orule_first_order,IndvS0,IndvO).
%orule_pred(F1,I,O):- ranking_pred(F1,I,O),!.
orule_first_order(I,VArea):-  hybrid_order(I,VArea).
orule_priority(Indv,Priority):- is_bg_object(Indv),!,Priority=9.
orule_priority(Indv,Priority):- area(Indv,1),!,Priority=6.
orule_priority(Indv,Priority):- has_prop(cc(fg,1),Indv),!,Priority=5.
orule_priority(Indv,Priority):- sub_var(iz(i()),Indv),Priority=1.
%orule_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
orule_priority(_,3).



smallest_first(IndvS0,IndvO):- smallest_first(mass,IndvS0,IndvO).
smallest_pred(F1,I,O):- ranking_pred(F1,I,O),!.
smallest_pred(_,I,O):- mass(I,O).
smallest_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
smallest_priority(_,1).
smallest_first(P2,IndvS0,IndvO):- predsort_two_p2(P2,smallest_priority,IndvS0,IndvO).

largest_first(IndvS0,IndvO):- largest_first(mass,IndvS0,IndvO).
largest_pred(F1,I,O):- ranking_pred(F1,I,O),!.
largest_pred(_,I,O):- mass(I,O).
largest_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
largest_priority(_,1).
largest_first(P2,IndvS0,IndvR):-   
 sort_safe(IndvS0,IndvS),
 %must_det_ll
 ((
  findall((Priority+Size)-Indv,(member(Indv,IndvS),largest_priority(Indv,NPriority),call(P2,Indv,Size),Priority is - NPriority),All),
  keysort(All,AllK),
  maplist(arg(2),AllK,IndvO),
  reverse(IndvO,IndvR))).

largest_first_nonbg(IndvS,IndvOB):-  
  largest_first(mass,IndvS,IndvO),
  remove_bgs(IndvO,IndvL,BGIndvS),
  my_append(IndvL,BGIndvS,IndvOB).




/*
prior_name_by_size(VM,IndvS0,Name):- 
  Title = save_as_obj_group(Name),
  if_t((true;number(_R)),
   (length(IndvS0,Len),
    if_t(Len>0,
     must_det_ll((
      %add_prior_placeholder(Name,IndvS0,IndvS1),
      override_object(birth(Name),IndvS0,IndvS1)
      gset(VM.objs)=IndvS1,
     /*
      %rank_priors(Name,IndvS1,IndvSL),
      gset(VM.objs)=IndvSL,
      nop((( Name == i_nsew ) -> DebugThis = full ; luser_getval(debug,DebugThis)->true; DebugThis=true)),
      nop(( DebugThis\==false -> Feedback = debug_as_grid(Title) ; Feedback = print_info)),
      nop(ignore((DebugThis\==false, print_grid(VM.h,VM.v,Title,IndvSL)))),     
      %dash_chars,
      */
      addObjectOverlap(VM,IndvSL)))))),!.
*/
/*
%add_prior_placeholder(_,I,I):-!.
add_prior_placeholder(Name,IndvS0,IndvS9):- is_list(IndvS0),!,
  length(IndvS0,Len),
  maplist(add_prior_placeholder(Len,Name),IndvS0,IndvS9).

add_prior_placeholder(_Len,Name,IndvS0,IndvS9):- 
  override_object(birth(Name),IndvS0,IndvS9),!.

add_prior_placeholder(Len,Name,IndvS0,IndvS9):- 
  (has_prop(pg(OG,Len,_,Name),IndvS0)-> IndvS0=IndvS9 ; 
    ((has_prop(pg(OG,Was,Other,Name),IndvS0)-> delq(IndvS0,pg(OG,Was,Other,Name),IndvS1) ; IndvS0=IndvS1),
     override_object(pg(OG,Len,nil,Name),IndvS1,IndvS9))),!.
*/

object_get_priors(X,S):- var(X),!, enum_object(X), object_get_priors(X,S).
object_get_priors(X,S):- is_object(X), !, must_det_ll((indv_props_list(X,Ps),
  findall(I,(member(P,Ps),props_object_prior(P,I)),L),L\==[],list_to_set(L,S))).

get_prior_labels(Objs,PriorsWithCounts):- must_det_ll((is_list(Objs),
  findall(Name,(member(Obj,Objs),object_get_priors(Obj,Name)),AllPriorsL),
  append(AllPriorsL,AllPriors),
  sort_safe(AllPriors,PriorsSet),
  my_partition(never_prior,PriorsSet,_Unused,PriorsSetClean),
  count_each(PriorsSetClean,AllPriors,PriorsWithCounts))).

never_prior(giz(_)).
never_prior(oid(_)).
%never_prior(pg(_,_,_,_)).

ranking_pred(rank1(F1),I,O):- Prop=..[F1,O], indv_props_list(I,Ps),member_or_iz(Prop,Ps),!.
ranking_pred(rank1(F1),I,O):- !, catch(call(F1,I,O),_,fail),!.
ranking_pred(rankA(F1),I,O):- append_term(F1,O,Prop), indv_props_list(I,Ps),member_or_iz(Prop,Ps),!.
ranking_pred(rankA(F1),I,O):- !, catch(call(F1,I,O),_,fail),!.
%ranking_pred(rank2(F1),I,O):- Prop=..[F1,O1,O2], indv_props_list(I,Ps),member_or_iz(Prop,Ps),!,combine_number(F1,O1,O2,O).
%ranking_pred(rank2(F1),I,O):- !, catch(call(F1,I,O1,O2),_,fail),!,combine_number(F1,O1,O2,O).
ranking_pred(_F1,I,O):- mass(I,O).

has_prop(Prop,Obj):- var(Obj),!, enum_object(Obj),has_prop(Prop,Obj).
has_prop(Prop,Obj):- is_grid(Obj),grid_props(Obj,Props),!,member(Prop,Props).
has_prop(Prop,Objs):- is_list(Objs),!,forall(member(Obj,Objs),has_prop(Prop,Obj)).
has_prop(Prop,ObjRef):- \+ is_object(ObjRef),!,atom(ObjRef),into_obj(ObjRef,Obj),!,has_prop(Prop,Obj).



has_prop(Props,Obj):- is_list(Props),!,member(Q,Props),has_prop(Q,Obj).

has_prop(Prop,Obj):- var(Prop), !, indv_props_list(Obj,L), member(Prop,L).
%has_prop(Var,_Obj):- var(Var),!, fail.
has_prop(Prop,Obj):- indv_props_list(Obj,L), member(Q,L), (Q=@=Prop -> true ; ( Q = Prop)).
has_prop(lbl(Lbl),Obj):- !, is_prior_prop(Lbl,Obj).

has_prop(Lbl ,Obj):- atom(Lbl),!, is_prior_prop(Lbl,Obj),!.
has_prop(and(A,B),Obj):- !, has_prop(A,Obj),has_prop(B,Obj).
has_prop(call_1(A),Obj):- !, has_prop(AA,Obj),call(A,AA).
has_prop(not(A),Obj):- !, \+ has_prop(A,Obj).
has_prop(or(A,B),Obj):- !, (has_prop(A,Obj);has_prop(B,Obj)).
has_prop(rank1(Lbl),Obj):- atom(Lbl),!, is_prior_prop(rank1(Lbl),Obj),!.
%has_prop(rank2(Lbl),Obj):- atom(Lbl),!, is_prior_prop(rank2(Lbl),Obj),!.
has_prop(rankA(Lbl),Obj):- nonvar(Lbl),!, is_prior_prop(rankA(Lbl),Obj),!.

never_a_prior(P):- var(P),!,fail.
never_a_prior(link).
never_a_prior(info).
never_a_prior(by_color).
never_a_prior(nsew).
never_a_prior(colormass).

never_a_prior(giz(X)):- !, fail, never_a_prior(X).
never_a_prior(iz(X)):- !, never_a_prior(X).
never_a_prior(P):- compound(P),functor(P,What,_),never_a_prior(What).
never_a_prior(P):- rankNever(P).

props_object_prior(V,_):- var(V),!,fail.
props_object_prior(Prop,_):- never_a_prior(Prop),!,fail.
%props_object_prior(pg(OG,_,_,L),O):- props_object_prior(L,O)
props_object_prior(pg(_OG,_,_,L),L):-!.
props_object_prior(mass(_),rank1(mass)).


props_object_prior(cc(Color,_Value),rankA(cc(Color))):-!.
props_object_prior(birth(S),L):- !, fail, first_atom_value(S,L), L\= indiv(_), \+ never_a_prior(L).
props_object_prior(iz(S),iz(L)):- !, props_object_prior(S,L).
%props_object_prior(iz(S),L):- !, props_object_prior(S,L).
props_object_prior(S,L):- S\=info(_), first_atom_value(S,L), \+ rankOnly(L), \+ never_a_prior(L).

first_atom_value(S,S):- atom(S),!.
first_atom_value(S,_):- \+ compound(S),!,fail.
first_atom_value(S,rank1(F)):- S=..[F,A],comparable_value(A).
%first_atom_value(S,rank2(F)):- S=..[F,A,B],F\=='/', comparable_value(A),comparable_value(B).
first_atom_value(S,O):- arg(1,S,E),atomic(E),E\==[],!,O=S, O \= (_/_).
first_atom_value(S,O):- arg(2,S,E),first_atom_value(E,O),O \= (_/_).

%comparable_value(A):- compound(A),!,A=(_/_).
comparable_value(A):- is_color(A),!.
comparable_value(A):- number(A),!.
%comparable_value(A):- atom(A),!.


rankOnly(center2G(_,_)).
rankOnly(loc2D(_,_)).

rankNever(vis2G(_,_)).



is_prior_prop(Lbl,Obj):- object_prior(Obj,L),L=Lbl,!.
%is_prior_prop(Lbl,Obj):- has_prop(Lbl,Obj),!.
object_prior(Obj,E):- object_get_priors(Obj,L),member(E,L).

add_priors([Lbl-Count|PriorsWithCounts],Objs,LF):- number(Count), !, add_prior(Count,Lbl,Objs,Mid),!, add_priors(PriorsWithCounts,Mid,LF).
add_priors([Count-Lbl|PriorsWithCounts],Objs,LF):- number(Count), !, add_prior(Count,Lbl,Objs,Mid),!, add_priors(PriorsWithCounts,Mid,LF).
add_priors([Lbl|PriorsWithCounts],Objs,LF):- !, add_prior(_,Lbl,Objs,Mid),!, add_priors(PriorsWithCounts,Mid,LF).
add_priors(_,IO,IO).

add_prior(N,Lbl,Objs,ObjsWithPrior):- 
  %is_list(Objs),
  %print_list_of(ppnl,Title,N),
  my_partition(is_prior_prop(Lbl),Objs,Lbld,Unlbl),  
  %add_prior_placeholder(Lbl,Lbld,RLbld),
  length(Lbld,LL),  

  rank_priors(Lbl,Lbld,RLbldR),
  nop(print_grid(Lbl->N/LL,RLbldR)),
  write('\t '), writeq(Lbl->N/LL),write(' <p/>\t'),
  append([Unlbl,RLbldR],ObjsWithPrior).  

/*
prior_name_by_size(_VM,[],_Name):-!.
prior_name_by_size(VM,IndvS0,Name):-  
  rank_priors(Name,IndvS0,IndvSL),
  addObjectOverlap(VM,IndvSL).
*/
rank_priors(_RelivantDivide,GType,FG,SFO1):-
  rank_priors(GType,FG,SFO1).

rank_priors(GType,Objs,SFO):-
 relivant_divide(RelivantDivide),
 my_partition(RelivantDivide,Objs,FG,BG),
 (FG\==[],BG\==[]),!,
 rank_priors(RelivantDivide,GType,FG,SFO1),
 rank_priors(inv(RelivantDivide),GType,BG,SFO2),
 append(SFO1,SFO2,SFO).

rank_priors(GType,Objs,SFO):-
 must_det_ll((
   %add_prior_placeholder(GType,Group,Objs),
   smallest_first(smallest_pred(GType),Objs,SF),
   ignore((relivant_group(OG), maplist(OG,Objs))),
   length(SF,SFLen),
   nop(SFLen < 2 -> pp(red,rank_priors(GType,SFLen)); pp(green,rank_priors(GType,SFLen))),   
   add_rank(OG,GType,SFLen,SF,SFO),
   nop(maybe_show_ranking(GType,SFO)))).


maybe_show_ranking(GType,SFO):-
 dash_chars,dash_chars,dash_chars,dash_chars,
 maplist(object_grid,SFO,G),
  length(SFO,Len),
  print_list_of(pp,setOF(GType),SFO),
  print_ss(G),
  u_dmsg(setOF(GType)=Len),atrace.


%has_order(O,P1, Code ):- 
% Code = ((
add_rank(OG,GType,ZType,IndvS,IndvSO):- add_rank(OG,GType,ZType,1,IndvS,IndvSO).

add_rank(_OG,_GType,_ZType,_,[],[]):-!.
%add_rank(OG,_GType,_ZType,1,IO,IO):-!.

add_rank(_OG,_GType,_ZType,N,[L],[L]):-  N==1, !.
add_rank(OG,GType,ZType,N,[L],[Obj]):-   N>1, !, set_rank(OG,GType,ZType,L,firstOF,Obj).
add_rank(OG,GType,ZType,N,[L],[Obj]):-  N==1, !, set_rank(OG,GType,ZType,L,firstAndLastOF,Obj).
add_rank(OG,GType,ZType,N,[L|IndvS],[Obj|IndvSO]):- N = 1, set_rank(OG,GType,ZType,L,lastOF,Obj), 
 N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).
add_rank(OG,GType,ZType,N,[L|IndvS],[Obj|IndvSO]):- % (GType = rank1(_);GType = rank2(_)),!, 
 set_rank(OG,GType,ZType,L,/*nthOF**/(N),Obj), 
 N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).
add_rank(OG,GType,ZType,N,[L|IndvS],  [L|IndvSO]):- 
 N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).


set_rank(OG,GType,ZType,L,N,Obj):-
  get_setarg_p1(nb_setarg,I,L,P1), 
  compound(I), I = pg(OG,ZType,_,GType),
  II = pg(OG,ZType,N,GType), 
  call(P1 ,II),!, Obj = L.

set_rank(OG,GType,ZType,L,N,Obj):- 
   II = pg(OG,ZType,N,GType), 
   override_object([II],L,Obj),!.


