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

/*
Props [Not] Shared Between

  - All Input Grids
  - All Output Grids

  - All Grids

  - Pair #1 Input
  - Pair #1 Output
  - Pair #1 Input and Output

  - Pair #2 Input
  - Pair #2 Output
  - Pair #2 Input and Output

  - Pair #1 Input and Pair #2 Output

*/
more_than_1_done(TestID):-
  is_why_grouped_g(TestID, _, individuate(_, two(I1, _O1)),_),
  is_why_grouped_g(TestID, _, individuate(_, two(I2, _O2)),_), 
  I1\==I2,!.


interesting_selectors(Name,Tst,Num,IO):-
  interesting_selectors0(Name0,Tst,Num,IO),
  maybe_aformat(Name0,Name00),Name00=Name.
  
interesting_selectors0('Training I/O',trn,_,_):-  get_current_test(TestID),more_than_1_done(TestID).
interesting_selectors0('Training Input',trn,_,in):-  get_current_test(TestID),more_than_1_done(TestID).
interesting_selectors0('Training Output',trn,_,out):-  get_current_test(TestID),more_than_1_done(TestID).
interesting_selectors0('Pair #~w I/O'-[NumP1],trn,Num,_):- current_example_nums(trn,Num, NumP1).
interesting_selectors0('Pair #~w Out'-[NumP1],trn,Num,out):- current_example_nums(trn,Num, NumP1).
interesting_selectors0('Pair #~w In'-[NumP1],trn,Num,in):- current_example_nums(trn,Num, NumP1).
interesting_selectors0('All Input'-[],_,_,in).
interesting_selectors0('Test #~w Input'-[NumP1],tst,Num,in):- current_example_nums(tst,Num, NumP1).
%interesting_selectors('All I/O'-[],_,_,_).

/*

      testIO+shared             
  /                   \
testI shared,         TestO shared
              pairIO
      /                     \
    pairI shared          pairO

pairI unshared         pairO unshared
             pairIO
*/
current_example_nums(Example,Num,NumP1):- get_current_test(TestID),kaggle_arc(TestID,Example+Num,_,_),NumP1 is Num + 1.

interesting_compares(trn+N*in, F1,  trn+N *out,F2):- current_example_nums(trn,N, _),filter_pairs(F1,F2).
interesting_compares(trn+N*in, F1,  trn+N2*in,F2) :- current_example_nums(trn,N,N2),filter_pairs(F1,F2).
interesting_compares(trn+N*out,F1,  trn+N2*out,F2):- current_example_nums(trn,N,N2),filter_pairs(F1,F2).
interesting_compares(tst+N*in, F1,  trn+N *in,F2) :- current_example_nums(tst,N, _),filter_pairs(F1,F2).

filter_pairs(whole,whole).
%filter_pairs(shared,shared). filter_pairs(unshared,shared). filter_pairs(shared,unshared). filter_pairs(unshared,unshared).

make_up_selector_name(Trn_num_io,Named):-  
  trn_num_io(Trn_num_io,Trn,Num,IO),
  interesting_selectors(NameI,Trn1,Num1,IO1), match_selector(Trn1,Trn),match_selector(IO1,IO),match_selector(Num1,Num),
  once(maybe_aformat(NameI,Named)).
maybe_aformat(Fmt-Args,Named):- nonvar(Fmt), format(atom(Named),Fmt,Args),!. 
maybe_aformat(Named,Named).

match_selector(Trn1,Trn):- var(Trn1),var(Trn),!.
match_selector(Trn1,Trn):- Trn1=@=Trn.
%match_selector(Trn1,Trn):- var(Trn),Trn1=Trn.

trn_num_io(Trn_Num_IO,Trn,Num,IO):- var(Trn_Num_IO),!, Trn_Num_IO=((Trn+Num)*IO).
trn_num_io((Trn+Num)*IO,Trn,Num,IO):-!.
trn_num_io(Trn+Num*IO,Trn,Num,IO):-!.

select_filtered_group(TestID,Named,Trn_num_io,Filter,Objects):- trn_num_io(Trn_num_io,Trn,Num,IO),
  with_individuated_cache(true,
    forall(kaggle_arc_io(TestID,Trn+Num,IO,G),individuate(complete,G,_Objs))),
  select_some_objects(TestID,Trn,Num,IO,Filter,Objects),
  make_up_selector_name(Trn+Num*IO,Named).


%select_some_objects(TestID,Trn,Num,IO,whole,Objs):- !, test_grouped_io(TestID,[(TestID>(Trn+Num)*IO)],[],Objs).
select_some_objects(TestID,Trn,Num,IO,Filter,Objects):-
  test_grouped_io(TestID,[(TestID>(Trn+Num)*IO)],[],Objs),
  filter_objects(Objs,Filter,Objects).

filter_objects(Objs,Filter,Objects):-
  reorganize_objs(Objs,Shared,PropsUnique,PropsDistibuted),
  named_filter_proptyple(Filter,Objs,Shared,PropsUnique,PropsDistibuted,Objects).
 
  %atom(Filter).
%named_filter_proptyple(Filter,Objs,Shared,PropsUnique,PropsDistibuted,Objects).
named_filter_proptyple(unshared,_,_,_,Unshared,Unshared).
named_filter_proptyple(shared,_,Shared,_,_,Objs):- (Shared\==[]->Objs=[Shared];Objs=[]).
named_filter_proptyple(whole,All,_,_,_,All).
named_filter_proptyple(Else,_,_,L,_,[Else|Objs]):- select(Else,L,Rest), findall(Else,member(Else,Rest),Objs).


pair_two_groups(TestID,Name1+Filter1,Name2+Filter2,Objs1,Objs2):-
  interesting_compares(Mask1,Filter1,Mask2,Filter2),
  select_filtered_group(TestID,Name1,Mask1,Filter1,Objs1),
  select_filtered_group(TestID,Name2,Mask2,Filter2,Objs2).

never_share_prop(oid(_)).
never_share_prop(was_oid(_)).

next_change(Next,Objects):- list_to_set(Objects,SObjects),list_to_set(Next,SNext), SObjects\=@=SNext.

reorganize_objs([O|Objects],Shared,PropsUnique,PropsDistibuted):- \+ is_list(O),
 must_det_ll((
  treed_plist(O,OOO), O\=@=OOO,!,
  my_maplist(treed_plist,Objects,PropLists),
  reorganize_objs([OOO|PropLists],Shared,PropsUnique,PropsDistibuted))).

reorganize_objs(Objects,[Prop|Shared],PropsUnique,PropsDistibuted):-
  member(O,Objects),member(Prop,O),
  \+ never_share_prop(Prop),
  my_maplist(variant_select(Prop),Objects,Next),!,
  must_det_ll((
  next_change(Next,Objects),
  reorganize_objs(Next,Shared,PropsUnique,PropsDistibuted))).

reorganize_objs(Objects,Shared,[unique(OID,NewObj,Prop)|PropsUnique],PropsDistibuted):- 
  select(Obj,Objects,Rest),
  member(O,Rest),member(Prop,O),
  member(Prop,Obj),
  my_maplist(variant_select(Prop),Rest,NewRest),!,
 must_det_ll((
  member(oid(OID),Obj),
  make_unifiable_cc(Prop,UProp), 
  select(UProp,Obj,NewObj),
  Next = [NewObj|NewRest],
  next_change(Next,Objects),
  reorganize_objs(Next,Shared,PropsUnique,PropsDistibuted))).


/*
reorganize_objs(Objects,Shared,[alone(OID,AloneProps)|PropsUnique],PropsDistibuted):- fail,
  select(Obj,Objects,Rest),
  member(oid(OID),Obj),
  findall(Prop,
    (member(Prop,Obj), make_unifiable(Prop,UProp), \+ ((member(RO,Rest),member(UProp,RO)))),
     AloneProps),
  include(not_in(AloneProps),Obj,NewObj),
  Next = [NewObj|Rest],
  next_change(Next,Objects),
  reorganize_objs(Next,Shared,PropsUnique,PropsDistibuted).

reorganize_objs(Objects,Shared,[missing(OID,Prop)|PropsUnique],PropsDistibuted):- fail,
  select(Obj,Objects,Rest),
  my_maplist(variant_select(Prop),Rest,NewRest),
  member(oid(OID),Obj),
  Next = [Obj|NewRest],
  next_change(Next,Objects),
  reorganize_objs(Next,Shared,PropsUnique,PropsDistibuted).
*/
/*
reorganize_objs(Objects,Shared,PropsUnique,[Versions|PropsDistibuted]):-
  length(Objects,Len),member(Obj,Objects),member(Prop,Obj),  
  variance_had(Prop,Objects,Versions,Variance), Variance = Len,
  reorganize_objs([RestProps|Rest],Shared,PropsUnique,PropsDistibuted).
*/
reorganize_objs(Objects,[],[],Objects):-!.
reorganize_objs([],[],[],[]):-!.


%show_groups(_TestID):-!.
show_groups(TestID):- ensure_test(TestID),
  show_filtered_groups(TestID),
  show_pair_groups(TestID).
  
show_filtered_groups(_TestID):-!.
show_filtered_groups(TestID):- ensure_test(TestID),
  forall( interesting_selectors(_,Trn,Num,IO),
   forall( member(Filter,[shared,unshared]),
   must_det_ll((
     select_filtered_group(TestID,Named,(Trn+Num*IO),Filter,Objs),
     length(Objs,Len),
     w_section(interesting_selectors(Named,Filter,Len), 
       must_det_ll((
         nop(ignore(((ground((Trn+Num*IO))->print(Objs); (Len<10 ->print(Objs); true))))),
         print_grouped_props(Named+Filter,Objs)))))))).

show_pair_groups(_TestID):-!.
show_pair_groups(TestID):- ensure_test(TestID),
  forall(no_repeats(vars(Name1+Filter1,Name2+Filter2),
   pair_two_groups(TestID,Name1+Filter1,Name2+Filter2,Objs1,Objs2)),
    ignore((
      Objs1\==[],Objs2\==[],
      Objs1\==Objs2,   
      Filter1==Filter2,

      %Filter1\==whole,Filter2\==whole,
      (Name1==Name2->Filter1\==Filter2;true),
      (Filter1==Filter2->Name1\==Name2;true),
      %functor(Filter1,F1,_),functor(Filter2,F2,_), F1\==alone,F2\==alone, %F1==F2,  
      
      append(Objs1,Objs2,OBJS),list_to_set(OBJS,OBJSET),
      length(OBJS,WP1),length(OBJSET,WP2), WP1 == WP2,
      % pp(Name1+Filter1-Name2+Filter2 = Objs1->Objs2),
       once(show_interesting_props(vs(Name1+Filter1,Name2+Filter2),Objs1,Objs2))
      ))).

rules_from(Objs1,Objs2,Objects):- Objects=(Objs1->Objs2).

show_info_about_objects(TestID,Named,Ors,Ands):-
  test_grouped_io(TestID,Ors,Ands,IO),
  w_section(Named,print_grouped_props(Named,IO)).


test_grouped_io(TestID,Ors,Ands,Objs):-
  findall(Indv,
   (is_why_grouped(TestID,_Count,_Why,IndvS), member(Indv,IndvS),
    ((is_list(Ors),Ors\==[])->once((member(OR,Ors),sub_term(E,Indv),nonvar(E),E=OR));true),
    ((is_list(Ands) -> \+ \+ ((forall(member(Trn,Ands),sub_var(Trn,Indv))));true))),
   IndvSIO),
  list_to_set(IndvSIO,IndvSet),
  Objs = IndvSet.
/*
test_grouped_io(TestID,ExampleNum,IO,Objs):-
  Example+Num = ExampleNum,
  is_why_grouped_g(TestID,_Count, individuate(complete,two(ID,_)),IndvSG),
  once(testid_name_num_io(ID,TestID,Example,Num,_)),
   my_maplist(must_oid_to_object,IndvSG,IndvS),
   (var(IO)->member(IO,[in,out]);true),
   once(include(sub_var(IO),IndvS,IndvSIO)),
   Objs = IndvSIO.
*/

test_grouped(TestID,ExampleNum,I,O):-  
  test_grouped_io(TestID,ExampleNum,in,I),
  once(test_grouped_io(TestID,ExampleNum,out,O)).

show_prop_counts(TestID):-
 ExampleNum=trn+_,
 ROptions=complete,
  forall(kaggle_arc(TestID,ExampleNum,GridIn,GridOut),
  must_det_ll((
   individuate_pair(ROptions,GridIn,GridOut,InC,OutC),
   print_grouped_props(TestID>ExampleNum*in,InC),
   print_grouped_props(TestID>ExampleNum*out,OutC)))).


:- thread_local(t_l:objs_others/4).
show_interesting_props(Named,InC,OutC):-
 must_det_ll((
  extend_grp_proplist(InC,ObjsI),
  extend_grp_proplist(OutC,ObjsO),
  banner_lines(cyan,4),
  w_section('INPUT PROPS',
    locally(t_l:objs_others(inputs,ObjsI,ObjsO,outputs),
      (print_grid(output(Named),ObjsI),
       print_grouped_props(input(Named),ObjsI),
       !))),
  banner_lines(white,2),
  w_section('OUTPUT PROPS',
    locally(t_l:objs_others(outputs,ObjsO,ObjsI,inputs),
      (print_grid(output(Named),ObjsO),
       print_grouped_props(output(Named),ObjsO),
       !))),
  
  banner_lines(white,2),
  %show_interesting_merge(Named,ObjsI,ObjsO),  
  %show_template_matches(t_m(Named),ObjsI,ObjsO),
  %nop(show_interesting_comp_diffs(Named,objs_to_which_props,templify_cc,ObjsI,ObjsO)),
  %nop(show_interesting_comp_diffs(Named,variance_counts,first_number,ObjsI,ObjsO)),
  %append(ObjsO,ObjsI,Objs),show_interesting_props_gojs(Objs),
  show_changes_and_match_template(m_t(Named),ObjsI,ObjsO),
  banner_lines(cyan,4))).


show_interesting_merge(Named,ObjsI,ObjsO):-
  append(ObjsO,ObjsI,ObjsAll),
  w_section(show_interesting_merge(Named),
    locally(t_l:objs_others(both,ObjsAll,ObjsAll,both),
      print_grouped_props(both(Named),ObjsAll))),!.

show_interesting_props_gojs(Objs):- u_dmsg(show_interesting_props_gojs(Objs)).
  %8731374e

count_each_value(Objs,CountOfEachL):-
  flat_props(Objs,AllProps),
  maplist(make_unifiable_cc,AllProps,ASProps),
  variant_list_to_set(ASProps,SUProps),
  count_each(SUProps,AllProps,CountOfEachL),!.

variance_counts(Objs,CountOfEachL):-
  flat_props(Objs,AllProps),
  maplist(make_unifiable_cc,AllProps,ASProps),
  variant_list_to_set(ASProps,SUProps),
  maplist(variance_had_count(Objs),SUProps,Variance),
  maplist(into_kv,Variance,SUProps,CountOfEachL).

into_kv(K,V,K-V).
from_kv(K-V,K,V).
variance_had_count(Objs,UHAD,Variance):- 
  variance_had_counts(_Common,UHAD,Objs,_Versions,_Missing,_VersionsByCount,Variance),!.
variance_had_count(_Objs,_UHAD,0).

   

show_changes_and_match_template(Named,ObjsI,ObjsO):-
 must_det_ll((
   maplist(variance_counts,[ObjsI,ObjsO],[VC1,VC2]),
   intersection(VC1,VC2,_Shared,VD1,VD2),append(VD1,VD2,VCB),
    maplist(prop_name,VD1,VN1),
    maplist(prop_name,VD2,VN2),
   %print_each_ss(variance_counts(Named),VD1,VD2),
   show_changed_diffs([c([b,_,_])],Named,first_number,VD1,VD2),
   nop(maplist(prop_name,VCB,VCN)),
   VCN=VNN,
   intersection(VN1,VN2,VNN,_,_),
   pp_saved(vnn=VNN), pp_saved(vcn=VCN),
   maplist(objs_to_which_props,[ObjsI,ObjsO],[WP1,WP2]),
   %show_changed_diffs(Named,templify_cc,WP1,WP2),
   append(WP1,WP2,WPB),
   include(equals_same_p2(prop_name,VCN),WPB,WPBT),
   pp_saved(wPBT=WPBT),
   maplist(templify_cc,WPBT,Templ),
   variant_list_to_set(Templ,Templates),   
   pp_saved(templates=Templates),
   maplist(show_matching_templates(Named,VNN,WP1,WP2),Templates,Info),
   sum_list(Info,Shown),
   (Shown==0 ->
     (writeln(shown=Shown),maplist(show_matching_templates(Named,_,WP1,WP2),Templates,Info));
     true),
   !)).

equals_same_p2(P2,VCN,I2):- call(P2,I2,O2),!, \+ \+ member(O2,VCN).

show_matching_templates(Named,VCN,WP1,WP2,T,Info):-
  must_det_ll((
   include(p1_call(chk(=(T))),WP1,LL1),
   include(p1_call(chk(=(T))),WP2,LL2),
   %numbervars(T,1100,_,[]),
   if_t(((LL1\==[],LL2\==[])),
    must_det_ll((color_print(green,Named),write(': '),color_print(white,T),nl,
     maplist(prop_value,LL1,N1), maplist(prop_value,LL2,N2),    
     ignore((
     (nonvar(VCN)->once(overlapping_element(prop_name,VCN,N1);overlapping_element(prop_name,VCN,N2));true),
      Info = 1,
      print_each_ss2(N1,N2)))))))),
  ignore(Info=0).

overlapping_element(P2,L1,L2):- member(E1,L1),member(E2,L2),once((call(P2,E1,F1),call(P2,E2,F2))), F1 =@= F2,!.

show_interesting_comp_diffs(Named,P2,P2A,ObjsI,ObjsO):- 
 must_det_ll((
   call(P2,ObjsI,HAD1), call(P2,ObjsO,HAD2),
   w_section(show_interesting_compare(P2,Named),
     must_det_ll((
      show_changed_diffs(t(P2,Named),Named,P2A,HAD1,HAD2)))))).

pp_non_nil_e(_,PP):- PP == [],!.
pp_non_nil_e(Named,PP):- nl,listify(PP,LL),wqs(Named),nl,maplist(pp,LL),nl.

changed_diffs(PropC1,PropC2,LS1,LS2,Same,LLS1,LLS2):-
 must_det_ll((
  intersection(PropC1,PropC2,Same,WP1,WP2),
  predsort(sort_on(prop_name),WP1,LS1), predsort(sort_on(prop_name),WP2,LS2),
  list_to_set(WP1,LLS1), list_to_set(WP2,LLS2))).

show_changed_diffs(Show,Named,Pred2,PropC1,PropC2):-
 must_det_ll((
 changed_diffs(PropC1,PropC2,LS1,LS2,Same,LLS1,LLS2), 
  \+ \+ if_t(member(c(Which),Show),show_changed_by_sides(Which,["Changed",Named],prop_name,LS1,LS2)),
  \+ \+ if_t(member(s(Which),Show),pp_non_nil_e(["Same",Named],Same)),
  \+ \+ if_t(member(d(Which),Show),show_changed_by_sides(Which,["Different",Named],Pred2,LLS1,LLS2)))).

show_changed_by_sides(Show,Named,Pred2,PropS1,PropS2):-
 must_det_ll((
  changed_by_sides(Pred2,PropS1,PropS2,LeftOnly,RightOnly,A1O,A2O),
  \+ \+ if_t(member(l,Show),pp_non_nil_e(["Left Only",Named],LeftOnly)),
  \+ \+ if_t(member(r,Show),pp_non_nil_e(["Right Only",Named],RightOnly)),
  \+ \+ if_t(member(b,Show),ignore(((((A1O\==[] ; A2O\==[])),!,nl,wqs(["Both",Named]),nl,maplist(print_each_ss2,A1O,A2O))))))).


changed_by_sides(Pred2,PropS1,PropS2,LeftOnly,RightOnly,A1O,A2O):-
 must_det_ll((
  maplist(Pred2,PropS1,ArgPropS1), maplist(Pred2,PropS2,ArgPropS2),
  append(ArgPropS1,ArgPropS2,ArgPropS),variant_list_to_set(ArgPropS,ArgPropSort),
  maplist(grp_by(Pred2,PropS1),ArgPropSort,A1L),
  maplist(grp_by(Pred2,PropS2),ArgPropSort,A2L),
  maplist(into_kv,A1L,A2L,OLairs),
  findall(K,member(K-[],OLairs),LeftOnly),
  findall(K,member([]-K,OLairs),RightOnly),
  findall(K-V,(member(K-V,OLairs),K\==[],V\==[]),Both),
  maplist(from_kv,Both,A1O,A2O))).


grp_by(P2,PPS1,AE,L):- 
  findall(A,(member(A,PPS1),call(P2,A,AE)),L).

first_number(A,AE):- nonvar(AE),first_number(A,AE1),!,AE1=AE.
first_number(A,AE):- sub_term(AE,A),number(AE),!.
first_number(_,inf).

print_each_ss(Title,L1E,L2E):-
  listify(L1E,WP1), listify(L2E,WP2),
  print_each_ss2([Title,'~n'|WP1],[Title,'~n'|WP2]).

%print_each_ss2([],L2E):- nl,print("Only Right "),!,variant_list_to_set(L2E,LL),maplist(pp,LL),!.
%print_each_ss2(L1E,[]):- nl,print("Only Left "),!,variant_list_to_set(L1E,LL),maplist(pp,LL),!.
print_each_ss2(L1E,[]):-!,print_each_ss2(["Only Left"],L1E).
print_each_ss2([],L1E):-!,print_each_ss2(["Only Right"],L1E).
print_each_ss2(L1E,L2E):- listify(L1E,WP1), listify(L2E,WP2),print_each_ss2_2(WP1,WP2),!.

pp_saved(A=B):- is_list(B),!,maplist(pp_saved_nv(A),B),pp(A=B).
pp_saved(A=B):- pp_saved_nv(A,B),!,pp(A=B).
pp_saved(A,B):- pp_saved_nv(A,B),pp(B).

pp_saved_nv(A,B):- must_det_ll((remember_propcounts(_Named,pp_saved,A,B))).

print_each_ss2_2(WP1,WP2):-
 must_det_ll((
   wots(S1,maplist(pp_saved(left),WP1)),
   wots(S2,maplist(pp_saved(right),WP2)),
   atomic_list_concat(SS10,'\n',S1),
   atomic_list_concat(SS20,'\n',S2),
   max_width(SS10,SS1,100),
   max_width(SS20,SS2,100),
   make_longest_len(SS1,SS2,SSS1,SSS2),
   print_to_string11(write,0,SSS1,SSS1A,Lad1),Lad is Lad1,
   maplist(print_to_string_using_up(Lad,''),SSS1A,SSS1B), 
   print_side_by_side0(SSS1B,_,SSS2))).

max_width([E|SS20],[E|SS2],Hundred):- atom_length(E,Len),Len=<Hundred,!,max_width(SS20,SS2,Hundred).
max_width([E|SS20],[EE|SS2],Hundred):- sub_atom(E,0,Hundred,_,EE),sub_atom(E,Hundred,_,0,NE),
 atom_concat('        ',NE,NEE),
  max_width([NEE|SS20],SS2,Hundred).
max_width([],[],_).

print_to_string_using_up(Pad,'',SSS1A,S):- wots(S,wp_using_up(Pad,'',SSS1A)). 
wp_using_up(Total,Comma,S):- display_length(S,Len),Used is Total-Len, write(Comma),wp_s_up(S,Used).
wp_s_up(S,Used):- Used>0, write(S),wp_n_sp(Used),!.
wp_s_up(S,_):- write(S),!. wp_n_sp(N):- N>0,!,write(' '),Nm1 is N -1,wp_n_sp(Nm1).  wp_n_sp(_).




print_treeified_props(Objs):-
  print_treeified_props(treeified_props,Objs),!.
print_treeified_props(Named,Objs):-
 must_det_ll((  
  my_maplist(treed_plist,Objs,PropLists),
  color_print(cyan,call(print_ptree(Named,PropLists))))).

make_longest_len(WP1,WP2,L11,L22):-
 length(WP1,LL1),length(WP2,LL2),
 (LL1>LL2 -> (L11=WP1,ensure_length(WP2,LL1,'',L22));
   (L22=WP2,ensure_length(WP1,LL2,'',L11))).

ensure_length(WP1,Len,With,New):- length(WP1,Len0),
  Extend is Len-Len0,Extend>0,!,
  make_list(With,Extend,More),
  append(WP1,More,New).
ensure_length(WP1,_Len,_With,WP1).


make_ss(RawPropList,SS):- 
  member(oid(OID),RawPropList),member(grid(Grid),RawPropList),wots(SS,print_grid(OID,Grid)),!.
make_ss(RawPropList,SS):- 
  member(oid(OID),RawPropList),oid_to_obj(OID,PObj),wots(SS,print_grid(OID,[PObj])),!.
%make_ss(RawPropList,SS):- wots(SS,print(SS)).


treed_plist(Obj,PropList):-
  must_det_ll((indv_props_list(Obj,RawPropList),  
  treed_props_list(RawPropList,PropList0),!,
  ((fail,make_ss(PropList0,SS))->
     append(PropList0,[(SS)],PropList);  append(PropList0,[],PropList)))).

treed_props_list(RawPropLists,PropLists):- is_list_of_lists(RawPropLists),!,maplist(treed_props_list,RawPropLists,PropLists).
treed_props_list(RawPropLists,PropLists):-
 must_det_ll((
  %include(p1_not(p1_arg(_,is_gridoid)),RawPropLists,RawPropLists0),
  =(RawPropLists,RawPropLists0),
  %care_to_count(RawPropLists0,RawPropLists1),
  include(p1_not(skip_ku),RawPropLists0,PropLists))),!.

fix_skip_ku([L|List],Fixed):- is_list(L),!,maplist(fix_skip_ku,[L|List],Fixed).
fix_skip_ku(PropList,Fixed):- include(p1_not(skip_ku),PropList,Fixed).

print_ptree(Named,RRRR):- 
 must_det_ll((
  fix_skip_ku(RRRR,RRR),
  treeify_props(Named,RRR,Tree),
  remember_tree(Named,Tree),
  tersify_gridoids(Tree,TTree),

  with_pre(print_tree_no_nl(Named=TTree)))).

tersify_gridoids(Tree,TTree):- map_pred1(replace_gridoids,Tree,TTree).
replace_gridoids(Tree,TTree):- is_points_list(Tree),length(Tree,F),F>4,!,length(Four,4),append(Four,_,Tree),append(Four,'...',TTree),!.
replace_gridoids(G,O):- is_grid(G),!,tersify(G,O).
replace_gridoids(Tree,TTree):- is_grid(Tree),length(Tree,F),F>4,!,length(Four,4),append(Four,_,Tree),append(Four,'...',TTree),!.
%tersify_gridoids(Tree,TTree):- is_grid(Tree),length_s(Tree,F),F>4,!,length_s(Tree,F),append(Four,_,Tree),append(Four,'...',TTree),!.
replace_gridoids(G,O):- is_gridoid(G),!,tersify(G,O).

contains_enough_for_print(Props,_):- var(Props),!,fail.
contains_enough_for_print(obj(Props),Print):- !, contains_enough_for_print(Props,Print).
contains_enough_for_print([P|Props],G):- is_obj_props(Props),!,(contains_enough_for_print(Props,G);
  (compound(P),arg(_,P,G),is_gridoid(G))).

:- abolish(is_prop2d/1).
:- dynamic(is_prop2d/1).

is_obj_props(Props):- is_list(Props), Props\==[], maplist(is_prop1,Props).
is_prop1(Prop):- ( \+ compound(Prop); Prop=[_|_] ; Prop=(_-_)),!,fail.
is_prop1(Prop):- is_prop2(Prop),!.
is_prop1(Prop):- ( is_point(Prop) ; is_color(Prop);  is_object(Prop) ; is_grid_cell(Prop)),!,fail.
is_prop1(P):- functor(P,F,A),functor(T,F,A),asserta(is_prop2d(T)),!.

is_prop2(P):- is_prop2d(P).
is_prop2(P):- compound_name_arity(P,_,N),N>=2.
is_prop2(grid(_)). is_prop2(f_grid(_)). is_prop2(pen(_)). is_prop2(unique_colors(_)).
is_prop2(rul(_)). is_prop2(localpoints(_)). is_prop2(globalpoints(_)).
is_prop2(iz(_)).  is_prop2(giz(_)).  is_prop2(oid(_)). 
is_prop2(mass(_)). is_prop2(rot2D(_)). 
%is_prop2(center2D(_,_)). %is_prop2(rotSize2D(_,_)). %is_prop2(grid_ops(_,_)). 
% is_prop2(grid_rep(_,_)). is_prop2(points_rep(_,_)).
%is_prop2(nth_fg_color(_,_)).  %is_prop2(uprop_was(_,_)).  %is_prop2(links_count(_,_)).
%is_prop2(cc(_,_)). %is_prop2(vis2D(_,_)). is_prop2(loc2D(_,_)). %is_prop2(cc(_,_)).
%is_prop2(pg(_,_,_,_)). %is_prop1(Prop):- uprop_was(
is_prop(Prop):- is_prop1(Prop),!.
is_prop(Prop):- writeln(user_error,not(is_prop(Prop))),itrace,!,fail.

% =====================================================================
is_fti_step(extend_obj_proplists).
% =====================================================================
extend_obj_proplists(VM):- extend_grp_proplist(VM.objs,set(VM.objs)).

extend_grp_proplist(Grp,GrpO):- Grp==[],!,GrpO=[].
extend_grp_proplist(Grp,GrpO):- user:extend_grp_proplist0(Grp,GrpO),!.

%extend_grp_member_proplist(Var,NewObj):- var(Var),!, enum_object(Var),extend_grp_proplist(Var,NewObj).
dont_extend_proplist(Grp):- \+ compound(Grp),!.
dont_extend_proplist(Grp):- \+ ((sub_term(E,Grp),compound(E),E=oid(_))),!.
dont_extend_proplist(Grp):- \+ \+ ((sub_term(E,Grp),compound(E),E=links_count(_,_))),!.
extend_grp_proplist0(Grp,GrpO):- dont_extend_proplist(Grp),!,GrpO=Grp.
extend_grp_proplist0(Grp,GrpO):-
  must_det_ll((
   maplist(extend_grp_member_proplist(Grp),Grp,GrpM),
           externalize_links(GrpM,GrpO))).

extend_grp_member_proplist(Grp,Props,OUTL):- is_list_of_prop_lists(Props),!,maplist(extend_grp_member_proplist(Grp),Props,OUTL).
extend_grp_member_proplist(Grp,[obj(Obj)],[obj(OUT)]):- extend_grp_member_proplist(Grp,Obj,OUT),!.
extend_grp_member_proplist(Grp,obj(Obj),obj(OUT)):-!, extend_grp_member_proplist(Grp,Obj,OUT).

extend_grp_member_proplist(_Grp,Props,OUTL):- must_det_ll(is_obj_props(Props)), length(Props,Len), Len==1,!,Props=OUTL.
extend_grp_member_proplist(Grp,Props,OUTL):- 
  Obj = obj(Props),
  findall(P,extend_obj_prop(Grp,Obj,P),NewProps),
  flatten(NewProps,NewPropsF),
  override_object(NewPropsF,Props,Obj1),
  fix_dumb_props(1,Obj1,Obj2),
  %override_object(Props,Obj1,OUT),
  indv_props_list(Obj2,OUTL).

fix_dumb_props(N,Obj1,[pen([cc(Color,1)])|Obj2]):- N==1, fail,
  select(pen([cc(Color,1)]),Obj1,ObjM1), \+ sub_var(wfg,ObjM1),
  is_real_color(Color),is_fg_color(Color),
  subst001(ObjM1,Color,wfg,ObjM),
  N1 is N +1,!,  
  fix_dumb_props(N1,ObjM,Obj2).
fix_dumb_props(_,Obj1,Obj1).

%lazy_prop(Prop):-  algo_list(Algo), arg(_,v(grid_ops(Algo,_NormOps),iz(algo_sid(Algo,_NormShapeID)),grid_rep(Algo,_NormGrid)),Prop).
%extend_obj_prop(_Grp,Obj,Props):- fail, once((localpoints(Obj,P),vis2D(Obj,H,V),points_to_grid(H,V,P,Grid), grid_props(Grid,Props))).
extend_obj_prop(_Grp,Obj,Prop):- compound(Obj), Obj = obj(List), missing_obj_props(Obj,List,Prop).
extend_obj_prop(Grp,Obj,Prop):- is_in_subgroup(Grp,Obj,Prop).

%extend_obj_or_proplist(In,PLists):- is_list(In), last(In,Obj),is_object(Obj),!,must_det_ll((extend_grp_proplist(In,Objs), my_maplist(indv_props_list,Objs,PLists))).
%extend_obj_or_proplist(In,PLists):- is_list(In), last(In,Obj),is_list(Obj),!, must_det_ll((my_maplist(indv_props_list,In,PLists))).
%extend_obj_or_proplist(In,In).

%%%indv_props_list(PA,PAP):- must_det_ll((extend_grp_proplist(PA,Obj), indv_props_list(Obj,PAP))),!.
 

non_interesting_props(OProps):- var(OProps),!.
non_interesting_props([]).
non_interesting_props(Obj):- is_object(Obj),!.
non_interesting_props(Obj):- is_grid(Obj),!.
non_interesting_props([Obj]):-!, non_interesting_props(Obj).

into_obj_props1(I,O):- I==[],!,O=[].
into_obj_props1(N,Out):- is_object(N),!,indv_props_list(N,Out).
into_obj_props1(N,Out):- is_obj_props(N),!,Out=N.
into_obj_props1(N,Out):- is_list(N),!,maplist(into_obj_props,N,Out).
into_obj_props(N,Out):- into_obj_props1(N,Out),!.
into_obj_props(N,Out):- prop_group_equation(N,M),into_obj_props1(M,Out).

prop_group_equation(all,Out):- !,prop_group_equation(find(((_Trn+_N)*_IO)),Out).
prop_group_equation(in,Out):- !,prop_group_equation(find((_+_)*in),Out). 
prop_group_equation(out,Out):- !,prop_group_equation(find((_+_)*out),Out). 
prop_group_equation(trn,Out):- !,prop_group_equation(find((trn+_)*_),Out). 
prop_group_equation(props(N),Out):- !,into_obj_props(N,Out).
prop_group_equation((TrnN*IO),Out):- compound(TrnN),((Trn+N)=TrnN),!,prop_group_equation(find((Trn+N)*IO),Out). 
prop_group_equation((Trn+(NIO)),Out):- compound(NIO),((N*IO)=NIO),!,prop_group_equation(find((Trn+N)*IO),Out). 
prop_group_equation(in(N),Out):- !,prop_group_equation(find((_+N)*in),Out). 
prop_group_equation(tst(N),Out):- !,prop_group_equation(find((tst+N)*in),Out). 
prop_group_equation(tst,Out):- !,prop_group_equation(find((tst+0)*in),Out). 
prop_group_equation(out(N),Out):- !,prop_group_equation(find((_+N)*out),Out). 
prop_group_equation(trn(N),Out):- !,prop_group_equation(find((trn+N)*_),Out). 
prop_group_equation(flat(A),Out):- !, prop_group_equation(A,AA), flatten(AA,Out).
prop_group_equation(set(A),Out):- !, prop_group_equation(A,AA), list_to_set(AA,Out).
prop_group_equation(shared(Stuf),Out):- !, prop_group_equation(Stuf,Mid),
  filter_objects(Mid,shared,Out).
prop_group_equation(unshared(Stuf),Out):- !, prop_group_equation(Stuf,Mid),
  filter_objects(Mid,unshared,Out).
prop_group_equation(shared(A,B),Out):- !, prop_group_equation(A,AA), prop_group_equation(B,BB),
  grp_intersection(AA,BB,Out,_,_).
prop_group_equation(unshared(A,B),Out):- !, prop_group_equation(A,AA), prop_group_equation(B,BB),
  grp_intersection(AA,BB,_,AAA,BBB),append(AAA,BBB,Out).  
prop_group_equation(+(A,B),Out):- !, prop_group_equation(A,AA), prop_group_equation(B,BB),append(AA,BB,Out).  
prop_group_equation(-(A,B),Out):- !, prop_group_equation(A,AA), prop_group_equation(B,BB),
  grp_intersection(AA,BB,_,Out,_).
prop_group_equation(find(TrnIO),Objs):- nonvar(TrnIO),
  get_current_test(TestID),
  test_grouped_io(TestID,[TrnIO],[TestID],Objs).
prop_group_equation(O,OO):- is_prop1(O),!,O=OO.
prop_group_equation(O,OO):- is_list(O),maplist(prop_group_equation,O,OO).
prop_group_equation(O,OO):- into_obj_props1(O,OO).

is_list_of_nonlists(L):- is_list(L), \+ (last(L,E),is_list(E)).
is_list_of_lists(L):- is_list(L), \+ \+ (last(L,E),is_list(E)).

grp_intersection(A,B,Shared,AA,BB):- is_list_of_nonlists(A),is_list_of_nonlists(B),!,intersection(A,B,Shared,AA,BB).
grp_intersection(A,B,Shared,AA,BB):- is_list_of_nonlists(B),is_list_of_lists(A),!,grp_intersection(B,A,Shared,BB,AA).
grp_intersection(A,B,Shared,AA,BB):- is_list_of_nonlists(A),is_list_of_lists(B),!,maplist(intersection(A),B,Shared,AA,BB).
grp_intersection(A,B,FA_Shared,FB_AA,FA_BB):- is_list_of_lists(A),is_list_of_lists(B),!,
  flatten(B,FB),grp_intersection(A,FB,_FB_Shared,FB_AA,_FB_BB),
  flatten(A,FA),grp_intersection(FA,B,FA_Shared,_FA_AA,FA_BB),
  !.


print_grouped_props(Named,OProps):- 
  non_interesting_props(OProps),!, print(print_non_interesting_props(Named)->OProps).
%print_grouped_props(Named,Obj):- \+ is_list(Obj), !, pp(print_grouped_props(Named)=Obj).

print_grouped_props(Named,In):- 
  extend_grp_proplist(In,Objs),
  set_test_id_io(Named),
  print_grouped_props2(Named,Objs),!,
  print_grouped_props1(Named,Objs),!.

print_grouped_props1(Named,In):-
  must_det_ll((
   extend_grp_proplist(In,Objs),
   print_treeified_props(Named,Objs),
   banner_lines(green,1))).

print_grouped_props2(Named,In):- 
 must_det_ll((
   extend_grp_proplist(In,Objs),
   must_det_ll((hack_prop_groups(Named,Objs))),
   banner_lines(green,1))),!.

print_grouped_props3(Named,In):-
 must_det_ll((
   extend_grp_proplist(In,ObjsG),
   consider_for_rank(ObjsG,Objs,_),
   %hack_prop_groups(Named,Objs),
   show_three_interesting_groups(Named,Objs,Groups),
   %banner_lines(cyan,3),
   groups_to_groupsets(Groups,GroupSets),
   forall(member(GObjs,GroupSets),
    (findall(Title,member(Title-GObjs,Groups),Titles),
      show_interesting_group(Named,Titles-GObjs))))).

groups_to_groupsets(Groups,GroupSets):-
  my_maplist(arg(2),Groups,GroupsList), predsort(variants_equal,GroupsList,GroupSets).

show_interesting_group(Named,Title-Objs):- 
  length_s(Objs,Len),
  mass(Objs,Mass),
  sformat(SGroup,'~w Object(s) ~w mass (~q)',[Len,Mass,Named]), 
  w_section(title(SGroup),
    (pp(Title),nl,print_ss(Objs))).

ssort(A,B):- list_to_set(A,B).

show_three_interesting_groups(Named,Objs,Groups):-
  findall(Prop,(member(obj(O),Objs),member(Prop,O), not_skip_ku(Prop) ),Props),
  ssort(Props,SProps),
  print_interesting_named_groups(props(Named),SProps),
  my_maplist(make_unifiable_cc,SProps,UProps), variant_list_to_set(UProps,SUProps),  
  print_interesting_named_groups(suprops(Named),SUProps),
  %count_each(SProps,Props,GroupsWithCounts),
  length_s(Objs,L),
  group_quals(SUProps,SProps,L,KUProps),
  print_interesting_named_groups(kuprops(Named),KUProps),
  objs_with_props(KUProps,Objs,L,Groups),
  nop(print_ss(groups=Groups)).

print_interesting_named_groups(Named,KUProps):- 
   w_section(title(Named),pp(kk(KUProps))).

numbered_vars(A,B):- copy_term(A,B),numbervars(B,0,_,[attvar(skip)]).

%skip_ku(pg(_,_,FL,_)):- !, FL \==firstOF, FL \==lastOF. 

%dontmatter for in = unshared('Pair #1 In'+shared,'Pair #2 In'+shared)
%matter for 1= 

priority_pg(rank1(_)).
%priority_pg(rankA(cc(_))).

skip_ku(Var):- var(Var),!,fail.
skip_ku(Var):- atomic(Var),!,fail.
skip_ku(S):- priority_prop(S),!,fail.
%skip_ku(pg(_,_,_,_)).
%skip_ku(pg(is_fg_object,_,_,_)).
%skip_ku(link(sees([_,_|_]),_)).
%skip_ku(link(sees(_),_)).
skip_ku(area(_)).
skip_ku(localpoints(_)).
skip_ku(links_count(sees,_)).
skip_ku(occurs_in_links(sees,_)).
skip_ku(grid_rep(comp,_)).
skip_ku(iz(media(_))).
skip_ku(shape_rep( _,_)).
skip_ku(points_rep( _,_)).
skip_ku(globalpoints(_)).
skip_ku(center2G(_,_)).
skip_ku(changes(_)).
skip_ku(o(_,_,_,_)).
skip_ku( elink( sees(_),_)).
skip_ku(giz(iv(_))).
%skip_ku(cc(C,_)):- is_real_color(C),!.
%skip_ku(giz(KU)):- nop(skip_ku(KU)),!.
skip_ku(giz(KU)):- skip_ku(KU),!.
skip_ku(giz(gido(_))).
skip_ku(giz(testid_example_io(_))).
skip_ku(giz(KU)):- \+ has_subterm(number,KU), \+ has_subterm(in_or_out,KU).
skip_ku(iz(KU)):- skip_ku(KU),!.
%skip_ku(iz(info(_))).
%skip_ku(iz(_)).
skip_ku(_-KU):- skip_ku(KU),!.


priority_prop(Var):- var(Var),!,fail.
priority_prop(pg(_,PG,_,_)):- priority_pg(PG),!.
priority_prop(pg(_,_,_,_)).
priority_prop((algo_sid(norm,_))).
priority_prop((stype(_))).
priority_prop(iz(P)):- priority_prop(P),!.
priority_prop(giz(P)):- priority_prop(P),!.
priority_prop(_-P):- priority_prop(P),!.
priority_prop(pen(_)).
%priority_prop(iv(_)).
priority_prop(sid(_)).
priority_prop(cc(fg,_)).
priority_prop(cc(bg,_)).
priority_prop(occurs_in_links(contained_by,_)).

ku_rewrite_props(Var,Var):- var(Var),!.
ku_rewrite_props(List0,List9):- is_grid(List0),!,List9=List0.
%ku_rewrite_props(link(sees([cc(S,_)]),_),link(sees([cc(S,_)]),_)).
%ku_rewrite_props(link(S,_),link(S,_)):-!.
ku_rewrite_props(S-A,S-B):- ku_rewrite_props(A,B),!.
ku_rewrite_props(A,A):- is_prop1(A),!.
ku_rewrite_props(Props,OUTL):- is_list_of_prop_lists(Props),!,maplist(ku_rewrite_props,Props,OUTL).
ku_rewrite_props(List0,List9):- is_group(List0),!,mapgroup(ku_rewrite_props,List0,List9).
ku_rewrite_props(obj(List0),obj(List9)):-!,ku_rewrite_props(List0,List9).
ku_rewrite_props(List0,List9):- is_obj_props(List0),!,
  include(not_skip_ku,List0,List1),
  my_maplist(ku_rewrite_props,List1,List2),
  variant_list_to_set(List2,List9).
ku_rewrite_props(A,A).

objs_with_props([KU-_|Props],Objs,OL,GO):- skip_ku(KU),!,objs_with_props(Props,Objs,OL,GO).

objs_with_props([KU-_|Props],Objs,OL,GO):-
  findall(O,(member(O,Objs),has_prop(KU,O)),KL), length_s(KL,KLL), 
  KLL==OL,!,objs_with_props(Props,Objs,OL,GO).
objs_with_props([_-List|Props],Objs,OL,GO):-
  findall(Prop-OO,(member(Prop,List),
     (findall(O,(member(O,Objs),has_prop(Prop,O)),OO))),         G),
  objs_with_props(Props,Objs,OL,Groups),
  append(G,Groups,GO).
objs_with_props([],_,_,[]).

group_quals([U|SUProps],SProps,L,[U-ListUS|KUProps]):- findall(U,member(U,SProps),ListU),
  length_s(ListU,LUL),LUL\==L, ssort(ListU,ListUS),
   ListUS=[_,_|_],!,group_quals(SUProps,SProps,L,KUProps).
group_quals([_|SUProps],SProps,L,KUProps):-  group_quals(SUProps,SProps,L,KUProps).
group_quals([],_SProps,_,[]).



dont_change(Var):- copy_term(Var,VarC),freeze(VarC,fail), freeze(Var,Var=@=VarC).


select_subgroup(Objs,GroupName,Count,SubObjs):- 
  into_group(Objs,GObjs),
  findall(O-GroupName,(member(O,GObjs),is_in_subgroup(Objs,O,GroupName)),Memberships),
  my_maplist(arg(2),Memberships,GroupNames),
  get_ccs(GroupNames,GroupNameCC),
  member(cc(GroupName,Count),GroupNameCC),
  findall(O,member(O-GroupName,Memberships),SubObjs).

object_prop(O,Prop):- indv_props_list(O,Props),member(Prop,Props).

objects_props(SubObjs,Props):-
  findall(Prop,(member(O,SubObjs),object_prop(O,Prop)),Props).
objects_names_props(SubObjs,Props):-
  findall(F-Prop,(member(O,SubObjs),object_prop(O,Prop),prop_name(Prop,F)),Props).

prop_value(Prop,Value):- nonvar(Value),prop_value(Prop,Var),!,Value=Var.
prop_value(List,Value):- is_list(List),!,maplist(prop_value,List,Value),!.
prop_value(Prop,Value):- prop_first_value(Prop,Value).

prop_first_value(Prop,Value):- nonvar(Value),prop_first_value(Prop,Var),!,Value=Var.
prop_first_value(Prop,Value):- \+ compound(Prop),!,Value=Prop.
prop_first_value(\+ Prop,Value):- nonvar(Prop),!,prop_first_value(Prop,Value).
prop_first_value(_ - Prop,Value):- nonvar(Prop),!,prop_first_value(Prop,Value).
prop_first_value(List,Value):- is_list(List),!,member(Prop,List),prop_first_value(Prop,Value),!.
prop_first_value(Value,Value).

prop_name(Prop,Named):- nonvar(Named),prop_name(Prop,Var),!,Named=Var.
prop_name(Prop,Named):- prop_first_value(Prop,Value), value_to_name(Value,Named),!.

value_to_name(Value,Named):- nonvar(Named),value_to_name(Value,Named2),!,Named2=Named.
value_to_name(Value,Named):- \+ compound(Value),!,Value=Named.
value_to_name(Value,Named):- make_unifiable_cc(Value,Named),!.
value_to_name(Value,Named):- make_unifiable_cc(Value,UProp),
   compound_name_arguments(UProp,F,Args),
   include(nonvar,Args,OArgs),
   (OArgs ==[] -> Named=F ; compound_name_arguments(Named,F,OArgs)).


mostly_fg_objs(OutCR,OutCR):-!.
mostly_fg_objs(ObjsG,FG):- my_partition(is_fg_object,ObjsG,FG,BG),length(FG,FGL),length(BG,BGL),FGL>BGL,!.
%mostly_fg_objs(InCRFGBG,OutCR):- include(is_fg_object,InCRFGBG,OutCR),!.
mostly_fg_objs(OutCR,OutCR):-!.

object_group_cc(Objs,GroupName,SubObjs,Count,NamesCC,ValuesCC):-
  select_subgroup(Objs,GroupName,Count,SubObjs),
  objects_names_props(SubObjs,Props),
  my_maplist(arg(1),Props,Names),get_ccs(Names,NamesCC),
  my_maplist(arg(2),Props,Values),get_ccs(Values,ValuesCC).

group_prop(P):- group_prop(P,_).
group_prop(giz(g(in)),i_o(input)).
group_prop(giz(g(out)),i_o(output)).
group_prop(cc(fg,0),fg_or_bg(iz_bg)).
group_prop(cc(bg,0),fg_or_bg(iz_fg)).
group_prop(Pof2,Extra):- compound(Pof2),compound_name_arguments(Pof2,F,List),append(Left,[A],List),
  classify_n(A,Plural),
  append([F|Left],[Plural],AmmountL),
  compound_name_arguments(Extra,ammount,AmmountL),!.
classify_n(Var,any):- var(Var),!.
classify_n(0,zero).
classify_n(1,zero).
classify_n(Two,any):- number(Two),!, Two>=2.


link_rel(sees(_)).
link_rel(overlaps(_,_)).
link_rel(overlapped_by(_,_)).
link_rel(contains).
link_rel(contained_by).
link_rel(P,F):- link_rel(P),functor(P,F,_).

/*
is_in_subgroup_c(Contained_by,Functor,_Grp,Obj,count_of_bg_links(Functor,Count)):- findall(Y, (indv_props(Obj,link(Contained_by,Y)),is_bg_object(Y)), L), length_s(L,Count).
is_in_subgroup_c(Contained_by,Functor,_Grp,Obj,count_of_fg_links(Functor,Count)):- findall(Y, (indv_props(Obj,link(Contained_by,Y)),is_fg_object(Y)), L), length_s(L,Count).
is_in_subgroup_c(Contained_by,Functor,Grp,YObj,links_from_bg_count(Functor,Count)):- obj_to_oid(YObj,Y), link_rel(Contained_by,Functor),findall(Obj, (member(Obj,Grp),indv_props(Obj,link(Contained_by,Y)),is_bg_object(Obj)), L), length_s(L,Count).
is_in_subgroup_c(Contained_by,Functor,Grp,YObj,links_from_fg_count(Functor,Count)):- obj_to_oid(YObj,Y), link_rel(Contained_by,Functor),findall(Obj, (member(Obj,Grp),indv_props(Obj,link(Contained_by,Y)),is_fg_object(Obj)), L), length_s(L,Count).
*/

fyl_functor_count(FYL,Functor,Count):-
  my_maplist(arg(1),FYL,Fs),list_to_set(Fs,FSet),
  member(Functor,FSet),
  findall(_,member(Functor-_,FYL),L),length_s(L,Count).

link_functor(contains,contains).
link_functor(sees(_),sees).
link_functor(contained_by,contained_by).



missing_obj_props(Obj,List,Prop):-
 Prop = grid_ops(Algo,NormOps),
 algo_list(Algo), \+ member(Prop,List), grid_ops(Obj,Algo,NormOps).
missing_obj_props(Obj,List,Prop):-
 Prop = grid_rep(Algo,NormOps),
 algo_list(Algo), \+ member(Prop,List), grid_rep(Obj,Algo,NormOps).
missing_obj_props(Obj,List,Prop):-
 Prop = iz(algo_sid(Algo,NormShapeID)),
 algo_list(Algo),
 \+ member(Prop,List), 
 grid_rep(Obj,Algo,NormGrid),local_shape_id(NormGrid,NormShapeID).

missing_obj_props(Obj,List,Prop):-
  Prop = links_count(Functor,Count),
  \+ member(Prop,List),
  obj_link_count(Obj,Functor,Count).



%is_in_subgroup(Grp,Obj,Prop):- var(Obj),!, enum_object(Obj),is_in_subgroup(Grp,Obj,Prop).
is_in_subgroup(Grp,Obj,Prop):- nonvar(Grp),var(Obj),!,member(Obj,Grp),is_in_subgroup(Grp,Obj,Prop).
is_in_subgroup(Grp,Obj,Prop):- var(Grp),var(Obj),!,findall(Obj,enum_object(Obj),Grp),is_in_subgroup(Grp,Obj,Prop).

%is_in_subgroup(_Grp,Obj,links_count(Functor,Count)):-   obj_link_count(Obj,Functor,Count).

is_in_subgroup(Grp,YObj,occurs_in_links(Functor,Count)):- is_list(Grp),
  obj_to_oid(YObj,Y),
  findall(Functor-Obj, 
    (member(Obj,Grp),indv_props_list(Obj,PL),member(link(Contained_by,YY),PL),YY=Y,functor(Contained_by,Functor,_)), FYL), 
  fyl_functor_count(FYL,Functor,Count).

is_in_subgroup(_Grp,Obj,iz(IZ)):- group_prop(Prop,IZ), has_prop(Prop,Obj).
is_in_subgroup(_Grp,Obj,nth_fg_color(Nth,Color)):- unique_fg_colors(Obj,List),
 sort_color_by_mass(Obj,List,Sorted),nth1(Nth,Sorted,Color).

obj_link_count(Obj,Functor,Count):- 
  link_functor(Contained_by,Functor),
  findall(_, (indv_props(Obj,link(Contained_by,_))), FYL1),
  (FYL1==[] -> findall(_, (indv_props(Obj,elink(Contained_by,_))), FYL); FYL=FYL1),
  length_s(FYL,Count).



%is_in_subgroup(Grp,Obj,ansestors(N,Set)):-transitive_sets(ansestor,Obj,Set,N).
%is_in_subgroup(Grp,Obj,descendants(N,Set)):-transitive_sets(descendant,Obj,Set,N).
%is_in_subgroup(Grp,Obj,tiouching(N,Set)):- nontransitive_set(touching,Obj,Set,N).
%is_in_subgroup(Grp,Obj,seeing(N,Set)):- nontransitive_set(seeing,Obj,Set,N).
%is_in_subgroup(Grp,Obj,insideOf(N,Set)):-transitive_sets(insideOf,Obj,Set,N).
%is_in_subgroup(Grp,Obj,contained_by(N,Set)):-transitive_sets(contained_by,Obj,Set,N).
%is_in_subgroup(Grp,Obj,Prop):- has_prop(Prop,Obj).
%is_in_subgroup(Grp,_,all).
not_skip_ku(P):- \+ skip_ku(P).

indv_eprops_list(Indv,List9):- is_prop1(Indv),!,List9=[Indv].
indv_eprops_list(Indv,List9):- 
  indv_props_list(Indv,List0),
  ku_rewrite_props(List0,List9).

var_e(E,S):- E==S,!.
var_e(E,S):- (nonvar(E);attvar(E)),!,E=@=S.

variant_list_to_set([E|List],Out):- select(S,List,Rest),var_e(E,S),!, variant_list_to_set([E|Rest],Out).
variant_list_to_set([E|List],[E|Out]):- !, variant_list_to_set(List,Out).
variant_list_to_set(H,H).

flat_props(PropLists,OUTL):- is_list_of_prop_lists(PropLists),!,flatten(PropLists,OUTL).
flat_props(Objs,EList):-
  maplist(indv_eprops_list,Objs,PropLists),
  flatten(PropLists,List), %list_to_set(List,Set),
  include(not_skip_ku,List,EList).

hack_prop_groups(Named,Objs):-
 must_det_ll((
  flat_props(Objs,EList),
  w_section(print_elists(Named),
    print_elists_hack_objs(Named,EList,Objs,HackedObjs)),
  banner_lines(orange,2),
  nop((my_maplist(arg(1),HackedObjs,RRR),
    w_section(hack_prop_groups(Named), (print_ptree(hacked(Named),RRR), banner_lines(yellow,2))))),  
  ignore(skip_if_ansi(nop(print_propset_groups(Named,Objs,EList)))))).

obj_had_vbo(Objs,HAD,VbO):-
  variance_had_counts(_Common,HAD,Objs,_Versions,_Missing,VersionsByCount,_Variance),
  vesion_uniqueness(VersionsByCount,VersionsByOccurancesN),
  vesion_uniqueness(VersionsByOccurancesN,VbO),!.

which_props(UPropsSetGSet,Objs,WhichCounts):-
   list_to_set(UPropsSetGSet,UPropsSetGSetOO),
   variant_list_to_set(UPropsSetGSetOO,UPropsSetGSetOOL),
   maplist(obj_had_vbo(Objs),UPropsSetGSetOOL,VCWs),
   list_to_set(VCWs,VCWsS1),reverse(VCWsS1,VCWsS2),
   %maplist(indicate_priority,VCWsS2,SS),
   VCWsS2 = SS,!,
   list_to_set(SS,WhichCounts), !.
   %predsort(sort_on(prop_priority),VCWsS2,WhichCounts).

indicate_priority(I,S -> I):- indicate_priority0(I,S).
indicate_priority0([1-(Good),Not1-(_-_)|_],one(Not1,Good)):- Not1\==1,!.
indicate_priority0(_,                      wait(_,_,_,_)).
   

prop_priority([_],0-inf):-!.
prop_priority([1-(N-_),Not1-(_-_)|_],1-N):- Not1\==1,!.
prop_priority([1-(_-_),   1-(N-_)|_],2-N):-!.
prop_priority(_,                     3-inf).



var_to_underscore(Var,_):- plain_var(Var),!.
print_elists_hack_objs(Named,Props0,Objs,HackedObjs):-
 HackedObjs = Hacked,
 must_det_ll((
  care_to_count(Props0,Props),
  into_test_id_io(Named,TestID,ExampleNum,IO),
  pp(into_test_id_io(Named,TestID,ExampleNum,IO)),
 % HackedObjs = Splits,
  length_s(Objs,BaseSize),
  variant_list_to_set(Props,PropsSet),
  count_each(PropsSet,Props,CountOfEach0),
  %predsort(sort_on(arg(2)),CountOfEachL,CountOfEach0),
  list_to_set(CountOfEach0,CountOfEach),
  %mpp(countOfEach=CountOfEach),
  ignore(my_maplist(remember_propcounts(Named,count),CountOfEach)),
  remember_propcounts(Named,countE,CountOfEach,Props,PropsSet),
  my_maplist(make_unifiable_cc,PropsSet,UPropsSet),
  map_pred(var_to_underscore,UPropsSet,UPropsSetG),
  variant_list_to_set(UPropsSetG,UPropsSetGSet),
  which_props(UPropsSetGSet,Objs,WhichCounts),
  (pp(whichCounts=WhichCounts)),
  count_each(UPropsSetGSet,UPropsSetG,GroupsWithCountsL),
  variant_list_to_set(GroupsWithCountsL,GroupsWithCountsLVS),
  predsort(sort_on(arg(2)),GroupsWithCountsLVS,GroupsWithCounts),!,
  variant_list_to_set(GroupsWithCounts,GroupsWithCountsW),
  list_to_set(GroupsWithCountsW,GroupsWithCountsWP),
  variant_list_to_set(GroupsWithCountsWP,GroupsWithCountsWPO),
  make_splitter(GroupsWithCountsWPO,CountOfEach,SSplits),list_to_set(SSplits,CSplits),
  store_splits(Named,BaseSize,CSplits,Splits),
  %nop
  (print_ptree(countOfEachU(Named),Splits)),
  ignore(my_maplist(remember_propcounts(Named,diversity),GroupsWithCountsWPO)),
  remember_propcounts(Named,diversityE,GroupsWithCountsWPO),
  replace_props_with_stats(GroupsWithCountsWPO,CountOfEach,Objs,HackedObjsM),
  my_maplist(ku_rewrite_props,HackedObjsM,Hacked),
  nop(pp(hackedObjs(Named)=HackedObjs)))).

store_splits(Named,BaseSize,CSplits,Splits):-
  ignore((my_maplist(save_1split(Named,BaseSize),CSplits))),
  clean_splits(BaseSize,CSplits,Splits).

%save_1split(Named,Six,(1-_)->[Six-CSplits]):- my_maplist(remember_propcounts(Named,nth(_)),[CSplits]).
%save_1split(Named,Six,(Six-_)->List):- length_s(List,Six),my_maplist(p1_call(chk(=(1-_))),List),
save_1split(Named,_,(N-Div)->CSplits):- my_maplist(remember_propcounts(Named,Div,N),CSplits).  
%save_1split(Named,_,(1-_)->[1-CSplits]):- !.
%save_1split(Named,_,(N)->List):- predsort(sort_on(last_type_is_value),List,Sort).
%save_1split(Named,_,_).

clean_splits(BaseSize,CSplits,SplitsO):- is_list(CSplits),!,
  my_maplist(clean_1split(BaseSize),CSplits,Splits),
  include(\==(''),Splits,SplitsO).
clean_splits(_,I,I).

clean_1split(Six,(1-_)->[Six-_CSplits],'').
clean_1split(Six,(1-_)->[Six-CSplits],ignore(CSplits)).
clean_1split(_,(1-_)->[1-CSplits],1-CSplits).
%clean_1split(Six,(Six-_)->List,Sort):- length_s(List,Six),my_maplist(arg(2),List,Last),predsort(sort_on(last_type_is_value),Last,Sort).
clean_1split(Six,(Six-_)->List,(Six)->Sort):- length_s(List,Six),my_maplist(arg(2),List,Last),predsort(sort_on(last_type_is_value),Last,Sort).
clean_1split(_,(N)->List,(N)->Sort):- predsort(sort_on(last_type_is_value),List,Sort).
clean_1split(_,X,X).

last_type_is_value(I,O):- last_type_is(number,I,O).
last_type_is_value(I,O):- last_type_is(is_list,I,O).
last_type_is_value(I,O):- last_type_is(is_color,I,O).
last_type_is_value(I,I).

last_type_is(P1,List,Last):- is_list(List),reverse(List,Term),member(E,Term),last_type_is(P1,E,Last).
last_type_is(P1,Numb,Numb):- p1_call(P1,Numb),!.
last_type_is(P1,Term,Last):- compound(Term),compound_name_arguments(Term,_,Args),reverse(Args,List),!,member(E,List),last_type_is(P1,E,Last).

make_splitter([N-UProp|WithCountsWPO],CountOfEach,OutSplits):-
  my_partition(sameps(UProp),CountOfEach,Used,Unused),
  made_split(N,UProp,Used,Out),
  (Out==[]->OutSplits=Splits;OutSplits=[Out|Splits]),
  make_splitter(WithCountsWPO,Unused,Splits).
make_splitter([],_,[]).

made_split(_N,_UProp,[],[]).
made_split(N,UProp,List,Out):-variant_list_to_set(List,Set),List\=@=Set,!,made_split(N,UProp,Set,Out).
made_split(N,UProp,List,Out):-list_to_set(List,Set),List\=@=Set,!,made_split(N,UProp,Set,Out).
made_split(_,UProp,List,((Len-UProp)->List)):- length_s(List,Len).
sameps(UProp,_-Prop):- \+ Prop \= UProp.

set_test_id_io(Named):-
  must_det_ll((into_test_id_io1(Named,TestID,ExampleNum,IO),
  nb_setval(prior_asserts,to(TestID,ExampleNum,IO)))).
into_test_id_io(Named,TestID,ExampleNum,IO):- var(Named),!, must_det_ll((nb_current(prior_asserts,to(TestID,ExampleNum,IO)))),!.
into_test_id_io(Named,TestID,ExampleNum,IO):-
  into_test_id_io1(Named,TestID,ExampleNum,IO),
  nb_setval(prior_asserts,to(TestID,ExampleNum,IO)).

%into_test_id_io1(Named+Filter,TestID,ExampleNum,IO):- into_test_id_io1(Named,TestID,ExampleNum,IO),!.
into_test_id_io1(Named+Filter,TestID,ExampleNum,IO+Filter):- into_test_id_io1(Named,TestID,ExampleNum,IO),!.
into_test_id_io1(Named,TestID,Example+Num,IO):- \+ compound(Named),!, 
 must_det_ll((interesting_selectors(Named,Example,Num,IO),get_current_test(TestID))),!.
into_test_id_io1(Named+Filter,TestID,Named,Filter):- !, get_current_test(TestID),!.
into_test_id_io1(Named,TestID,Example+Num,IO):- testid_name_num_io(Named,TestID,Example,Num,IO),nonvar(Num),!.
into_test_id_io1(vs(Name1+Filter1,Name2+Filter2),TestID,
  vs(Example+Num,(Example2+Num2)),
  vs(IO,IO2,vs(Filter1,Filter2))):- 
  interesting_selectors(Name1,Example,Num,IO),get_current_test(TestID),
  interesting_selectors(Name2,Example2,Num2,IO2),!.
into_test_id_io1(input(TestID>ExampleNum),TestID,ExampleNum,in).
into_test_id_io1(both(TestID>ExampleNum),TestID,ExampleNum,in_out).
into_test_id_io1(output(TestID>ExampleNum),TestID,ExampleNum,out).
into_test_id_io1(Named,TestID,ExampleNum,IO):- Named=..[IO,TestID>ExampleNum],!.
into_test_id_io1(Named,TestID,ExampleNum,IO):- Named=..[IO,TestID,ExampleNum],!.
into_test_id_io1(Named,TestID,Named,Named):-get_current_test(TestID),!.



remember_propcounts(Named,Diversity,N-Prop):- !, into_test_id_io(Named,TestID,ExampleNum,IO),
  assert_if_new(propcounts(TestID,ExampleNum,IO,Diversity,N,Prop)).
remember_propcounts(Named,Diversity,Prop):- into_test_id_io(Named,TestID,ExampleNum,IO),
  assert_if_new(propcounts(TestID,ExampleNum,IO,Diversity,Prop)).

remember_propcounts(Named,Diversity,B,N-Prop):- !, into_test_id_io(Named,TestID,ExampleNum,IO),
  assert_if_new(propcounts(TestID,ExampleNum,IO,Diversity,B,N,Prop)).
remember_propcounts(Named,Diversity,B,Prop):- into_test_id_io(Named,TestID,ExampleNum,IO),
  assert_if_new(propcounts(TestID,ExampleNum,IO,Diversity,B,Prop)).

remember_propcounts(Named,Diversity,B,Prop,A):- into_test_id_io(Named,TestID,ExampleNum,IO),
  assert_if_new(propcounts(TestID,ExampleNum,IO,Diversity,B,Prop,A)).


:- abolish(propcounts/5).
:- abolish(propcounts/6).
:- abolish(propcounts/7).

:- dynamic(propcounts/5).
:- dynamic(propcounts/6).
:- dynamic(propcounts/7).

sort_obj_props(How,obj(Props),obj(Sorted)):-
  predsort(How,Props,Sorted).

replace_props_with_stats(SortedWithCounts,CountOfEach,obj(Objs),obj(HackedObjs)):- 
  !,my_maplist(replace_props_with_stats(SortedWithCounts,CountOfEach),Objs,Hacked),
  list_to_set(Hacked,HackedObjsR),reverse(HackedObjsR,List0),!,
  include(not_skip_ku,List0,List1),
  my_maplist(ku_rewrite_props,List1,HackedObjs).

replace_props_with_stats(SortedWithCounts,CountOfEach,Objs,HackedObjs):-
  is_list(Objs),!,my_maplist(replace_props_with_stats(SortedWithCounts,CountOfEach),Objs,HackedObjs).



replace_props_with_stats(SortedWithCounts,CountOfEach,Prop,OProp):-  by_freq(SortedWithCounts,CountOfEach,Prop,OProp),!.


by_freq(_,_,N-P,N-P):-!.
by_freq(SortedWithCounts,CountOfEach,Prop,OProp):-
   ((member(N1-UProp,SortedWithCounts), \+ \+ (UProp=Prop))),!,
   ((member(N2-CProp,CountOfEach),CProp=@=Prop)
      ->
        (OProp=((N1/N2)-Prop))
      ;
        (OProp=u(N1)-Prop)).
by_freq(_SortedWithCounts,CountOfEach,Prop,(c(N2)-Prop)):-
   ((member(N2-CProp,CountOfEach),CProp=@=Prop)),!.
by_freq(_,_,P,0-P):-!.

print_propset_groups(Named,Objs,EList):- 
  w_section(print_propset_groups(Named),print_propset_groups_now(Named,Objs,EList)).

print_propset_groups_now(Named,Objs,EList):-
  my_maplist(has_props_set(Objs),EList,GrpList),
  predsort(sort_on(length_s),GrpList,GrpSetR),reverse(GrpSetR,GrpSet),
  with_tag_ats(table,class("tblo rows_distinct"),
   (html_table_row([th(props),td('&nbsp;'),th(objects),td('&nbsp')]),
    my_maplist(print_prop_groups(Named,EList,Objs),GrpSet),
    forall(select(O1,Objs,Rest),print_prop_groups(Named,EList,Rest,O1,[])))).

has_props_set(Objs,Prop,Set):- include(has_prop(Prop),Objs,Set).
print_prop_groups(_Named,_ESet,_,[]):-!.
print_prop_groups(_Named,_ESet,_,[_]):-!.
print_prop_groups(Named,EList,All,Group):-
  include(not_in(Group),All,Except),
  [O1|Rest]=Group,
  print_prop_groups(Named,EList,Except,O1,Rest).

print_prop_groups(Named,_ESet,Except,O1,Rest):-
  [O1|Rest]=Group,
  indv_props_list(O1,PropsIKU), include(not_skip_ku,PropsIKU,Props),

  findall(Prop,(member(Prop,Props), \+ any_have_prop(Except,Prop), 
                               forall(member(O,Rest),has_prop(Prop,O))),UniqueToGroup),

  t_l:objs_others(_,_These,Those,_),
  findall(O2,(member(O2,Those), forall(member(UProp,UniqueToGroup),has_prop(UProp,O2))),OtherObjs),

  asserta_if_new(prop_groups(Named,Group,UniqueToGroup,Except,OtherObjs)),

  my_maplist(into_gui_item,Group,Grids),
  my_maplist(into_gui_item,OtherObjs,OGrids),
  %OGrids=[],
  print_prop_grids_list(UniqueToGroup,Grids,OGrids),!.

remember_tree(_,_).

print_prop_grids_list(UniqueToGroup,Grids,OGrids):- 
 html_table_row([
  th(with_tag_style(pre,"width: 225px; font-size: 14px",write_tall(UniqueToGroup))), 
  td('&nbsp;'),
  td(write_scrollable(print_table_rows_2([Grids,OGrids ]))),
  td('&nbsp;')]),!.
%print_prop_grids_list(UniqueToGroup,Grids,OGrids):- print_table([[with_tag(pre,write_tall(UniqueToGroup)),print_table([[print_ss(Grids)],[print_ss(OGrids)]])]]).

%print_table_rows_2([RowA,RowB]):- !, print_side_by_side(RowA),!,print_side_by_side(RowB),!.
print_table_rows_2([RowA,[]]):- 
  t_l:objs_others(These,_,_,_),
  with_tag_style('table','border: 2px solid blue;', my_maplist(html_table_row,[[th(These)|RowA]])).
print_table_rows_2([RowA,RowB]):- 
  make_rows_same_length([RowA,[],RowB],[Row1,Spaces,Row2]),
  Spacer = [td(" ")|Spaces],
  t_l:objs_others(These,_,_,Those),
  with_tag_style('table','border: 2px solid blue;', my_maplist(html_table_row,[Spacer,Spacer,[th(These)|Row1],Spacer,[th(Those)|Row2],Spacer,Spacer])).

write_scrollable(Goal):- with_tag_class(div,scrollable,Goal).

any_have_prop(Except,Prop):- member(O,Except),has_prop(Prop,O),!.

transitive_sets(P2,Obj,Set,N):- findall(n(P,List),(trans(P2,Obj,List),List\==[],length_s(List,P)),Lists),ssort(Lists,Set),length_s(Set,N).
nontransitive_set(P2,Obj,Set,N):- findall(Other,p2_call(P2,Obj,Other),List),list_to_set(List,Set),length_s(Set,N).

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
contained_by(Obj,Other):- has_prop(link(contained_by,Other,_),Obj).




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
  check_tid_gid(TID_GID,VM.start_grid),
  consider_for_rank(ObjsG,FG,BG),
  add_rankings(TID_GID,FG,Objs),  
  append(Objs,BG,FGBG),
  gset(VM.objs) = FGBG)).

consider_for_rank(ObjsG,FG,BG):- my_partition(is_fg_object,ObjsG,FG,BG),
 FG\==[],BG\==[].
consider_for_rank(ObjsG,FG,[]):- ObjsG=FG,!.

% bg from fg
relivant_divide(is_fg_object).
relivant_group(G):- relivant_divide(G).
relivant_group(not_has_prop(mass(1))).
%relivant_divide(has_prop(cc(bg,0))).
% seperate input from output
%relivant_divide(has_prop(giz(g(in)))).
%relivant_divide(unique_fg_color_count_eq_1).

unique_fg_color_count_eq_1(Obj):- unique_fg_colors(Obj,II),II=1.


add_rankings(Objs,WithPriors):- add_rankings(cuz,Objs,WithPriors).



add_rankings(Why,ObjsIn,WithPriors):- fail,
 relivant_divide(RelivantDivide),
 my_partition(RelivantDivide,ObjsIn,FG,BG),
 ((FG\==[],BG\==[])), 
 fail,
 !,
 print_ss([splitting_group(RelivantDivide)=FG,splitting_group(RelivantDivide)=BG]), 
 add_rankings(Why,FG,FGWithPriors),
 add_rankings(Why,BG,BGWithPriors),
 append(FGWithPriors,BGWithPriors,WithPriors),!. 

add_rankings(Why,ObjsIn,WithPriors):- fail,
 once(combine_same_globalpoints(ObjsIn,Objs)),
 ObjsIn\=@=Objs,!, 
 add_rankings(Why,Objs,WithPriors).

%add_rankings(_,Objs,Objs):-!.
add_rankings(Why,Objs,WithPriors):- 
  add_how_simular(Objs,Simulars),
  group_prior_objs0(Why,Simulars,WithPriors),!.

group_prior_objs0(Why,Objs,WithPriors):- 
 must_det_ll(group_prior_objs1(Why,Objs,WithPriors)),!,
 (Objs=@=WithPriors -> wdmsg(group_prior_objs0_same) ; wdmsg(group_prior_objs0_DIFFF)),
 !.

add_how_simular(ObjsIn,ObjsIn):-!.
add_how_simular(ObjsIn,Simulars):-
  add_how_simular(ObjsIn,ObjsIn,Simulars).

add_how_simular([],_,[]):-!.
add_how_simular([obj(O)|Objs],ObjsIn,[obj(OO)|Simulars]):- 
  select(obj(O),ObjsIn,Rest),
  add_how_common(O,Rest,OO),
  add_how_simular(Objs,ObjsIn,Simulars).

add_how_common([],_,[]):-!.
add_how_common([Prop|O],Rest,[Prop,simular(Prop,N)|OO]):- 
  Prop\=giz(_), Prop\=simular(_,_), Prop\=link(_,_),
  Prop\=oid(_),
  \+ (compound_name_arity(Prop,_,A),A>2),
  % \+ ( arg(2,Prop,E),number(E) ),
  % Prop\=pg(_,_,_,_),
  findall(_,(sub_term(E,Rest),E==Prop),L),length(L,N),
  %prop_name(Prop,Name),!,
  add_how_common(O,Rest,OO).
add_how_common([Prop|O],Rest,[Prop|OO]):-
  add_how_common(O,Rest,OO).
  

group_prior_objs1(Why,Objs,WithPriors):-   
 must_det_ll((
 %print_list_of(show_indiv,add_rankings,Objs),!,
 flat_props(Objs,Flat),
 maplist(make_unifiable_cc,Flat,UFlat),
 variant_list_to_set(UFlat,Lbls),
 length_s(Lbls,Len),
 Title = Why+Len,
 length(Objs,ObjsLen),

 w_section(title(add_priors(Title)),
  %print_tree(groupPriors=Lbls,[max_depth(200)]),
  with_tag_class(div,nonpre,
     add_uset_priors(ObjsLen,Lbls,Objs,WithPriors))))).

skip_prior(HAD):- \+ compound(HAD),!.
skip_prior(HAD):- \+ \+ not_care_to_count(HAD),!.
%skip_prior(HAD):- HAD=simular(_,_),!,fail.
skip_prior(HAD):- compound_name_arity(HAD,_,N),!,N>1.

add_uset_priors(_,[],Objs,Objs):-!.
add_uset_priors(ObjsLen,[HAD|Lbls],Objs,WithPriors):-
  skip_prior(HAD), !, add_uset_priors(ObjsLen,Lbls,Objs,WithPriors).
add_uset_priors(ObjsLen,[HAD|Lbls],Objs,WithPriors):-
  skip_prior(HAD), !, add_uset_priors(ObjsLen,Lbls,Objs,WithPriors).

add_uset_priors(ObjsLen,[HAD|Lbls],Objs,WithPriors):-  
 must_det_ll((
  variance_had_counts(Common,HAD,Objs,Versions,Missing,VersionsByCount,Variance))),
   \+ \+ (member(V,Versions),has_subterm(number,V)),
  must_det_ll((((  Variance==1;  (fail, length(Objs,Len),Len==Variance,Missing==[])),fail)
   -> add_uset_priors(ObjsLen,Lbls,Objs,WithPriors)
   ; (add_1uset_prior(ObjsLen,Common,VersionsByCount,Objs,NObjs), add_uset_priors(ObjsLen,Lbls,NObjs,WithPriors)))),!.

add_uset_priors(ObjsLen,[_|Lbls],Objs,WithPriors):-
  add_uset_priors(ObjsLen,Lbls,Objs,WithPriors),!.

add_1uset_prior(ObjsLen,Common,VersionsByCount,Objs,NObjs):- 
  vesion_uniqueness(VersionsByCount,VersionsByOccurancesN),
  vesion_uniqueness(VersionsByOccurancesN,VbO),
 maplist(add_prior_info(Objs,ObjsLen,Common,VbO),Objs,NObjs).

add_prior_info(Objs,ObjsLen,Common,VbO,obj(List),obj(NewList)):-
  add_prior_info_1(Objs,ObjsLen,Common,VbO,List,NewList),!.
add_prior_info(Objs,ObjsLen,Common,VbO,(List),(NewList)):- 
  add_prior_info_1(Objs,ObjsLen,Common,VbO,List,NewList),!.

add_prior_info_1(_Objs,ObjsLen,_Common,VbO,PropList,OUT):- is_list(PropList),
  length(VbO,Rankers), %Rankers>1,
  find_version(VbO,Prop,N1,N2,PropList),
  member(Prop,PropList),
  %prop_name(Prop,Name),  
  value_to_name(Prop,Name),
  R = pg(Rankers,Name,rank1,N2),  
  \+ member(R,PropList),  
  rank_size(Rankers,N2,Size),
  %subst(PropList,Prop,R,PropListR),
  PropList = PropListR,
  nop(_=pg(Size,Name,rank3,Size)),
  extra_rank_prop(ObjsLen,Name,N1,ExtraProp),
  append(PropListR,[R,ExtraProp,pg(ObjsLen,Name,simulars,N1)],OUTE),!,
  include(some_pgs_and_props(PropList),OUTE,OUT).

add_prior_info_1(_Objs,_ObjsLen,_Common,_VersionsByCount,PropList,PropList).

extra_rank_prop(ObjsLen,Name,N1,pg(_,Name,rankLS,largest)):- ObjsLen==N1,!.
extra_rank_prop(_,Name,1,pg(_,Name,rankLS,smallest)):-!.
extra_rank_prop(_,Name,_,pg(_,Name,rankLS,mediumest)).

use_simulars(_):- fail.
use_rank(mass(_)).
%redundant_prop(_,_):-!,fail.
redundant_prop(_,nth_fg_color(N1,_)):- N1==1.
redundant_prop(Props,unique_colors([FG])):- sub_var(pen([cc(FG,1)]),Props),!.
redundant_prop(Props,cc(FG,_)):- is_real_fg_color(FG),sub_var(pen([cc(FG,1)]),Props),!.
redundant_prop(Props,center2D(_,_)):- sub_compound(loc2D(_,_),Props).
%redundant_prop(Props,center2D(X,Y)):- sub_var(center2G(X,Y),Props).
%redundant_prop(Props,center2G(X,Y)):- sub_var(center2D(X,Y),Props).

some_pgs_and_props(_,pg(_,Name,simulars,_)):- !, use_simulars(Name),!.
some_pgs_and_props(_,pg(_,Name,rank1,_)):- !, use_rank(Name),!.
some_pgs_and_props(PropList,Name):- \+ redundant_prop(PropList,Name).

rank_size(_,1,3):-!.
rank_size(M,N,2):- N\==M,!.
rank_size(N,N,1):-!.

find_version(VbO,Prop,N1,N2,PropList):-
  member(Version,VbO),deepest_kv(Version,N2,Prop),has_nprop(Prop,PropList),arg(1,Version,N1),!.
find_version(VbO,OProp,N1,N2,_PropList):-
  member(Version,VbO),deepest_kv(Version,N2, \+ Prop),ignore(arg(1,Version,N1)),!,
  ignore(\+ Prop = OProp).


deepest_kv(_-(K-V),O,VV):- nonvar(K), !, deepest_kv(K-V,O,VV).
deepest_kv(O-V,O,V).
deepest_kv(V,_,V).

%:- reconsult(kaggle_arc_individuation).

/*
 clumped_r([1,2,3,3],O).
O = [1-1,1-2,2-3].

*/

select_which(CntC,Order):- keysort(CntC,Order2),maplist(arg(2),Order2,Order).
clumped_r(Cnts,CntR):- clumped(Cnts,CntC),maplist(swap_vk,CntC,CntR).
swap_vk(K-V,V-K).

reordering(Order,I,V):- nth1(V,Order,E),E==I,!.
reordering(_,_,inf).


vesion_uniqueness(I,O):- vesion_uniqueness1(I,O),!.

vesion_uniqueness1(I,O):- 
  vesion_uniqueness(I,I,M),list_to_set(M,M1),=(M1,O),!.

%number_cards(M1,O):- 

vesion_uniqueness([],_,[]):-!.
vesion_uniqueness([C-H|T],I,[(Len-(C-H))|O]):- 
   findall(_,member(C-_,I),L),length(L,Len),
   vesion_uniqueness(T,I,O),!.

vesion_uniqueness2(I,O):- predsort(sort_on(version_count),I,O).
version_count(N-_,N):-!.
version_count(_,inf):-!.
version_magnitude(_-Version,N):- !,version_magnitude(Version,N).
version_magnitude(Version,R):- findall(E,((sub_term(E,Version),comparable_value(E))),N),reverse(N,R).

has_nprop(F,Obj):- compound(F), (F= (_-Prop)),!,has_nprop(Prop,Obj).
has_nprop(\+ (Prop),Obj):- \+ has_nprop(Prop,Obj).
has_nprop(Prop,Obj):- sub_compound(Prop,Obj),!.
%has_nprop(Prop,Obj):- is_object(Obj),!,has_prop(Prop,Obj).

%has_nprop(\+ (Prop),Obj):- !, \+ has_prop(Prop,Obj).


group_prior_objs_oldway(Why,Objs,WithPriors):- 
 must_det_ll((
 %print_list_of(show_indiv,add_rankings,Objs),!,
 get_prior_labels(Objs,_PriorsSetClean,_AllPriors,Lbls),
 keysort(Lbls,N),
 length_s(N,Len),
 Title = Why+Len,
 nop(noisey_debug(print_premuted_objects(Title))),
 w_section(title(add_priors(Title)),
  %print_tree(groupPriors=Lbls,[max_depth(200)]),
  with_tag_class(div,nonpre,add_priors(Lbls,Objs,WithPriors))))).

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
  %nop(my_maplist(title_objs,ORF,TitledObjs)),
  %my_maplist(obj_short_props,ORF,TitledObjs),
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

equal_sets(A,B):- sort(A,AA),sort(B,BB),AA=@=BB.


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


resize_inf(X,N):- is_list(X),!,length_s(X,N).
resize_inf(X,X).

member_or_iz(Prop,Ps):- member(Prop,Ps).
member_or_iz(Prop,Ps):- member(iz(Prop),Ps).
member_or_iz(Prop,Ps):- member(birth(Prop),Ps).
member_or_iz(Prop,Ps):- member(giz(Prop),Ps).

%combine_number(_F1,O1,O2,O):- O is ((abs(O1-O2)+1)*O1)+(O2*30).
combine_number(_F1,O1,O2,O):- O is sqrt(O1*O1+O2*O2).

length_s(L,S):- (is_list(L);var(L)),!,length(L,S).
length_s(L,S):- itrace,fail,length(L,S).

visible_first(IndvS0,IndvO):- predsort_two_p2(visible_priority,visible_first_order,IndvS0,IndvO).
%visible_pred(F1,I,O):- ranking_pred(F1,I,O),!.
visible_first_order(I,VArea):-  vis_area(I,Area), globalpoints(I,Ps),length_s(Ps,L), VArea is Area/L.
visible_priority(Indv,Priority):- mass(Indv,1),!,Priority=7.
visible_priority(Indv,Priority):- mass(Indv,0),!,Priority=8.
visible_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
visible_priority(_,1):-!.

predsort_two_p2(P2a,P2b,IndvS,IndvO):-
  findall((SortOn1+SortOn2)-Indv,(member(Indv,IndvS),call(P2a,Indv,SortOn1),call(P2b,Indv,MSize),resize_inf(MSize,SortOn2)),All),
  keysort(All,AllK),
  my_maplist(arg(2),AllK,IndvO).  

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
 ssort(IndvS0,IndvS),
 %must_det_ll
 ((
  findall((Priority+Size)-Indv,(member(Indv,IndvS),largest_priority(Indv,NPriority),call(P2,Indv,Size),Priority is - NPriority),All),
  keysort(All,AllK),
  my_maplist(arg(2),AllK,IndvO),
  reverse(IndvO,IndvR))).

largest_first_nonbg(IndvS,IndvOB):-  
  largest_first(mass,IndvS,IndvO),
  remove_bgs(IndvO,IndvL,BGIndvS),
  my_append(IndvL,BGIndvS,IndvOB).

care_to_count(RawPropLists,PropLists):- is_list_of_lists(RawPropLists),!,maplist(care_to_count,RawPropLists,PropLists).
care_to_count(GSS,GS):- is_list(GSS),include(is_care_to_count,GSS,GS).


not_care_to_count(Var):- var(Var), !, fail.
not_care_to_count(pg(_,_,_,_)).
not_care_to_count(pg(T,_,N1,N2)):- (N1 == 1 ; N2 == 1 ; N1 == T ; N2 == T), !, fail.
%not_care_to_count(oid(_)).
not_care_to_count(giz(_)).
not_care_to_count(elink(_,_)).

not_care_to_count(_):- !, fail.
not_care_to_count(Cmpd):- arg(_,Cmpd,E),is_gridoid(E),!, \+ is_grid(E).
not_care_to_count(Cmpd):- arg(_,Cmpd,E),is_points_list(E),!.
%not_care_to_count(iz(info(_))).
%not_care_to_count(iz(HasNumber)):- sub_term(N,HasNumber),number(N),!.
is_care_to_count(P):- not_care_to_count(P),!,fail.
is_care_to_count(_).

has_subterm(P1,HasNumber):- sub_term(N,HasNumber),call(P1,N),!.

variance_had_counts(Common,HAD,RRR,Versions,Missing,VersionsByCount,Variance):-
 %HAD\=simular(_,_),
 must_det_ll((  
  make_unifiable_cc(HAD,UHAD),
  findall(RR,(member(RR,RRR), once((indv_props_list(RR,R), \+ member(UHAD,R)))),Missing),
  length(Missing,ML),
  findall(UHAD,(member(RR,RRR), indv_props_list(RR,R), member(UHAD,R)),VersionL),
  variant_list_to_set(VersionL,VersionSet),
  some_min_unifier(VersionSet,Common),nonvar(Common),
  number_from_magnitude(VersionSet,VersionSetNumbered),
  count_each(VersionSet,VersionL,CountOfEachL),
  subst_2L(VersionSet,VersionSetNumbered,CountOfEachL,CountOfEachNumbered),
  
  (ML == 0
  ->(Versions = VersionSet, CountOfEachNumbered=CountOfEachNumberedNN)
  ; (Versions=[(\+(UHAD))|VersionSet],CountOfEachNumberedNN=[ML-(0- \+(UHAD))|CountOfEachNumbered])),  
 length(Versions,Variance),
 vesion_uniqueness2(CountOfEachNumberedNN,VersionsByCount),
 ignore(((nb_current(prior_asserts,to(TestID,ExampleNum,IO)),
    \+ propcounts(TestID,ExampleNum,IO, variance_had_count_set(_,_),        UHAD,_,_),
    assert_if_new(
       propcounts(TestID,ExampleNum,IO, variance_had_count_set(Variance,ML),Common,Versions,CountOfEachNumberedNN))))))).

number_from_magnitude(VersionSet,VersionSetNumbered):-
  predsort(using_compare(version_magnitude),VersionSet,VersionSetOrdered),
  lists:number_list(VersionSetOrdered,1,VersionSetOrderedNumberedVK),
  maplist(swap_vk,VersionSetOrderedNumberedVK,VersionSetOrderedNumberedKV),
  subst_2L(VersionSetOrdered,VersionSetOrderedNumberedKV,VersionSet,VersionSetNumbered).


/*
variance_had(HAD,RRR,Versions,Variance):-
  make_unifiable_cc(HAD,UHAD),
  findall(UHAD,(member(R,RRR),member(UHAD,R)),VersionL),
  list_to_set(VersionL,Versions),
  length(Versions,Variance).
*/
is_length(N,L):- length_s(L,N).



treeify_props(_Named,[],[]):-!.
%treeify_props(Named,One,[One]):- \+ is_list(One), !.
treeify_props(_Named,One,One):- \+ is_list(One), !.
treeify_props(_Named,[One],One):-!.
%treeify_props(_Named,One,One):- length_s(One,1), !.
%treeify_props(_Named,One,One):- my_maplist(is_length(1),One), !.
%treeify_props(_Named,One,One):- my_maplist(is_length(2),One), !.
%treeify_props(Named,RRR,R):- sort_vertically(RRR,RR),RRR\=@=RR,!,treeify_props(Named,RR,R).
%treeify_props(Named,L,LM):- my_maplist(simpl_ogs,L,MM),L\=@=MM,!,treeify_props(Named,MM,LM).
%treeify_props(Named,RRR,R):- sort_vertically(RRR,RR),RRR\=@=RR,!,treeify_props(Named,RR,R).
%treeify_props(Named,RRR,OUT):- length(RRR,2),!,OUT=RRR.
%treeify_props(Named,RRR,OUT):- length(RRR,3),!,OUT=RRR.
treeify_props(Named,RRR,HAD -> RO):- member(R,RRR),member(HAD,R), my_maplist(variant_select(HAD),RRR,RR),treeify_props(Named,RR,RO).

treeify_props(Named,RRR, R):- is_list_of_prop_lists(RRR), \+ sub_compound(pg(_,_,_,_),RRR), once(add_rankings(RRR,RR)),RRR\=@=RR,!,treeify_props(Named,RR, R).
treeify_props(Named,RRR, R):- length(RRR,DontDivOnThisNumber), 
  treeify_props(Named,DontDivOnThisNumber,RRR, R),!,ignore(maybe_unite_oids(Named,R)).
%treeify_props(Named,RRR,RR):- attempt_min_unifier_select(Named,RRR,R),RRR\=@=R,!,treeify_props(Named,R,RR), maybe_unite_oids(Named,RR).
%treeify_props(Named,RRR,RR):- sort_vertically(RRR,R), always_attempt_min_unifier_select(Named,R,RR),!,maybe_unite_oids(Named,RR).
treeify_props(Named,RRR,RR):- sort_vertically(RRR,RR),!,ignore(maybe_unite_oids(Named,RR)).


 :- discontiguous treeify_props/5.
/*
treeify_props(Named,_DontDivOnThisNumber,RRR, OUTPUT):- is_list(RRR), fail,
  flatten(RRR,Props),
  ku_rewrite_props(Props,GSS), 
  care_to_count(GSS,PropsSet),
  %count_each(PropsSet,Props,CountOfEachL),
  %predsort(sort_on(arg(2)),CountOfEachL,CountOfEach),
  %pp(countOfEach=CountOfEach),  
  my_maplist(make_unifiable_cc,PropsSet,UPropsSet),
  map_pred(var_to_underscore,UPropsSet,UPropsSetG),
  variant_list_to_set(UPropsSetG,UPropsSetGSet),
  count_each(UPropsSetGSet,UPropsSetG,GroupsWithCountsL),
  variant_list_to_set(GroupsWithCountsL,GroupsWithCountsLVS),
  predsort(sort_on(arg(2)),GroupsWithCountsLVS,GroupsWithCounts),!,
  variant_list_to_set(GroupsWithCounts,GroupsWithCountsW),
  list_to_set(GroupsWithCountsW,KS), [_N-HAD|_] = KS,
  findall(HAD,member(HAD,PropsSet),HHH),!,
  treeify_props_these_next(Named,HAD,HHH,RRR,OUTPUT).

treeify_props(Named,DontDivOnThisNumber,RRR, remove(UProp)->OUT):- fail, 
 member(R,RRR), member(HAD,R), \+ divide_on_very_last(HAD,RRR),
 variance_had_counts(UProp,HAD,RRR,Versions,_Missing,_CountOfEach,Variance),
 DontDivOnThisNumber==Variance,
 my_maplist(select_safe_always(Versions,UProp),_,RRR,RR),
 treeify_props(Named,RR,OUT).
*/
objs_to_which_props(Objs,List):- 
  flat_props(Objs,Props),
  maplist(make_unifiable_cc,Props,UProps),
  variant_list_to_set(UProps,PropsSet),
  my_maplist(make_unifiable_cc,PropsSet,UPropsSet),
  map_pred(var_to_underscore,UPropsSet,UPropsSetG),
  variant_list_to_set(UPropsSetG,UPropsSetGSet),
  which_props(UPropsSetGSet,Objs,List).

objs_to_had(Objs,HAD):- 
  objs_to_which_props(Objs,List),!,
  member(1-One,List),deepest_kv(One,_,HAD).

 treeify_props(Named,_DontDivOnThisNumber,Objs, (one(EACH) -> EACHOUT)):- fail,
  objs_to_had(Objs,HAD), 
  variance_had_counts(UProp,HAD,Objs,Versions,Missing,_CountOfEach,_Variance),
  term_variables(UProp,EVars),
  subst_between(EVars,UProp,Objs,NEWRRR),
  (((Objs=@=NEWRRR)) 
    -> ( treeify_versions(Named,EACH,UProp,Versions,Objs,Missing,OUT), always_attempt_min_unifier_select(EACH,OUT,EACHOUT))
    ; ( flag(nvs,X,X),
        numbervars(EVars,X,New,[]),
        flag(nvs,X,New),
        treeify_props(Named,NEWRRR,EACHOUT))).


treeify_props(Named,DontDivOnThisNumber,RRR, OUT):-
  treeify_props(divide_on_very_last,Named,DontDivOnThisNumber,RRR, OUT),!.

%treeify_props(Named,DontDivOnThisNumber,RRR, OUT):-  treeify_props(Named,DontDivOnThisNumber,RRR, OUT),!.

treeify_props(Named,DontDivOnThisNumber,RRR, OUT):-
  treeify_props(not_divide_on_never,Named,DontDivOnThisNumber,RRR, OUT),!.

not_divide_on_never(A,_):- \+ divide_on_never(A).

not_divide_on_very_last(A,B):- \+ divide_on_very_last(A,B).




treeify_props(P1,Named,DontDivOnThisNumber,RRR, (EACH -> EACHOUT)):- 
 EACH = each((SubEach>=SEG)*Variance*UProp),
 TopDiv is round(DontDivOnThisNumber /2)-1,
 member(SEG,[TopDiv,5,3,2,1]),
 member(R,RRR), member(HAD,R), \+ call(P1,HAD,RRR),
 %HAD\=pen(_),
 variance_had_counts(UProp,HAD,RRR,Versions,Missing,CountOfEach,Variance), 
 DontDivOnThisNumber\==Variance,
  maplist(arg(1),CountOfEach,CountL),
  list_to_set(CountL,CountOfEachSorted),
 CountOfEachSorted=[SubEach], 
 SubEach>=SEG,
 term_variables(UProp,EVars),
 subst_between(EVars,UProp,RRR,NEWRRR),
 print(coe = CountOfEach),
 (((RRR=@=NEWRRR)) 
   -> ( treeify_versions(Named,EACH,UProp,Versions,RRR,Missing,OUT), always_attempt_min_unifier_select(EACH,OUT,EACHOUT))
   ; ( flag(nvs,X,X),
       numbervars(EVars,X,New,[]),
       flag(nvs,X,New),
       treeify_props(Named,NEWRRR,EACHOUT))).

subst_between(Vars,UProp,RRR,NEWRRR):-
  maplist(upropify(Vars,UProp),RRR,NEWRRR).

upropify(EVars,UProp,Props,NewProps):-
  copy_term(EVars+UProp,Vars+UUProp),% term_variables(UUProp,Vars),
  select(UUProp,Props,Props1),
  subst_vals(UProp,EVars,Vars,Props1,NVPairs,URProps),
  URProps\=@=Props1,!, append([UProp|URProps],NVPairs,NewProps).
upropify(_,_,Props,Props).


subst_vals(_UProp,_,[],Props,[],Props).
subst_vals(UProp,[EV|EVars],[V|Vars],Props,[uprop_was(UProp,EV=V)|More],NEWRRR):- \+ is_ftVar(V),(compound(V);atom(V)),
  subst001(Props,V,EV,Mid), subst_vals(UProp,EVars,Vars,Mid,More,NEWRRR),!.
subst_vals(UProp,[_|EVars],[_|Vars],Props,More,NEWRRR):- subst_vals(UProp,EVars,Vars,Props,More,NEWRRR).
  
treeify_props(P1,Named,DontDivOnThisNumber,RRR, [+(UHAD -> HADRR), +( ( \+ UHAD) -> NHADRR)]):-
 DontDivOnThisNumber>=3,
 select(R,RRR,Rest), Rest\==[],
 member(HAD,R), \+ call(P1,HAD,RRR), make_unifiable_cc(HAD,UHAD), \+ (member(R2,Rest),member(UHAD,R2)),
 UHAD\=cc(_,_),
 treeify_props(Named,[R],HADRR),
 treeify_props(Named,Rest,NHADRR).

treeify_props(P1,Named,DontDivOnThisNumber,RRR, uprop(UProp)->EACHOUT):- 
 member(R,RRR),
 Best = best(Variance,Counts,UProp,Versions,Missing),
 findall(Best,
   (member(HAD,R), \+ call(P1,HAD,RRR),
      variance_had_counts(UProp,HAD,RRR,Versions,Missing,Counts,Variance),
      (Missing==[] ->  DontDivOnThisNumber\==Variance ; true)),
   BestUProps),
  list_to_set(BestUProps,SortedBestUProps),
  uniquest(DontDivOnThisNumber,SortedBestUProps,Best),
  UProp\=pen(_),
  term_variables(UProp,EVars),
  subst_between(EVars,UProp,RRR,NEWRRR),
 (((RRR=@=NEWRRR)) 
  -> (treeify_versions(Named,best(Variance,Counts),UProp,Versions,RRR,Missing,EACHOUT))
   ; ( flag(nvs,X,X),
       numbervars(EVars,X,New,[]),
       flag(nvs,X,New),
       treeify_props(Named,NEWRRR,EACHOUT))),!.

uniquest(LL,BestUProps,Best):- 
  my_partition(chk(=(best(_,_,_,[]))),BestUProps,HasMissing,NoneMissing),
  HasMissing\==[],NoneMissing\==[],
  uniquest(LL,HasMissing,BestHasMissing),
  uniquest(LL,NoneMissing,BestNoneMissing),
  uniquest(LL,[BestHasMissing,BestNoneMissing],Best),!. 
uniquest(LL,BestUProps,Best):- uniquest1(LL,BestUProps,Best),!.
uniquest(_LL,[Best,_,_|_UProps],Best).

uniquest1(_,[Best,best(V2,_,_,_,_)|_],Best):- Best = best(V1,_,_,_,_), V1\==V2,!.
uniquest1(LL,[best(V1,_,_,_,_)|List],Best):- my_exclude(chk(=(best(V1,_,_,_,_))),List,Betters),!,uniquest1(LL,Betters,Best).

/*
treeify_props(Named,DontDivOnThisNumber,RRR,[ (yes0(HAD)=HL/NL/Variance)->FHAVES ,  (not0(HAD)=NL/HL/Variance)->FHAVENOTS ]):-  fail,
  
  lots_of_prop(RRR,N,HAD), N\==DontDivOnThisNumber, \+ divide_on_very_last(HAD,RRR),
  my_partition(member(HAD),RRR,HAVES,HAVENOTS),
  length_s(HAVES,HL),length_s(HAVENOTS,NL),

  HL>1, NL>1,
  variance_had(HAD,RRR,_Versions,Variance), 
  my_maplist(variant_select(HAD),HAVES,HAVESS),%HAVESS\=[[]|_],
  treeify_props(Named,HAVESS,FHAVES),
  treeify_props(Named,HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[].

treeify_props(Named,DontDivOnThisNumber,RRR,                                                                OUT):- fail,
  
  lots_of_prop(RRR,N,HAD), N\==DontDivOnThisNumber,N\==1, \+ divide_on_very_last(HAD,RRR),
  my_partition(member(HAD),RRR,HAVES,HAVENOTS),
  length_s(HAVES,HL),length_s(HAVENOTS,NL), 

  HL>0, NL>0,
  variance_had(HAD,RRR,_Versions,Variance), Variance == 1,
  my_maplist(variant_select(HAD),HAVES,HAVESS),%HAVESS\=[[]|_],
  treeify_props(Named,HAVESS,FHAVES),
  treeify_props(Named,HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[],
 (HL>NL -> OUT = [ (not5(HAD)=NL/HL/Variance)->FHAVENOTS , HAD->FHAVES ] ;
    OUT = [ (yes51(HAD)=HL/NL/Variance)->FHAVES ,  (not51(HAD)=NL/HL/Variance)->FHAVENOTS ]),!.

treeify_props(Named,DontDivOnThisNumber,RRR,[ (not11(HAD)=NL/HL)->FHAVENOTS ,  (yes11(HAD)=HL/NL)->FHAVES ]):- fail,
  
  lots_of_prop(RRR,N,HAD), N\==DontDivOnThisNumber,N\==1, \+ divide_on_very_last(HAD,RRR),
  my_partition(member(HAD),RRR,HAVES,HAVENOTS),
  length_s(HAVES,HL),length_s(HAVENOTS,NL), 

  HL>1, NL==1,  
  my_maplist(variant_select(HAD),HAVES,HAVESS), 
  treeify_props(Named,HAVESS,FHAVES),
  treeify_props(Named,HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[].

treeify_props(Named,DontDivOnThisNumber,RRR,[ (yes1(HAD)=HL/NL)->FHAVES ,  (not1(HAD)=NL/HL)->FHAVENOTS ]):- fail,
  
  lots_of_prop(RRR,N,HAD), N\==DontDivOnThisNumber, \+ divide_on_very_last(HAD,RRR),
  my_partition(member(HAD),RRR,HAVES,HAVENOTS),
  length_s(HAVES,HL),length_s(HAVENOTS,NL), 

  HL==1, NL>1, 
  
  variance_had(HAD,RRR,_Versions,Variance), Variance==2,
  my_maplist(variant_select(HAD),HAVES,HAVESS), 
  treeify_props(Named,HAVESS,FHAVES),
  treeify_props(Named,HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[].


lots_of_prop(RRR,N,HAD):- 
  flatten(RRR,GF),
  ssort(GF,GSS), 
  care_to_count(GSS,GS), 
  count_each(GS,GF,UC),
  keysort(UC,KS),
  member(N-HAD,KS).
*/

variant_member(E,List):- member(V,List),E=@=V,!.

templify_cc(WP1,WP2):- \+ compound(WP1),!,WP2=WP1.
templify_cc(N-WP1,N-_):- \+ compound(WP1),!.

templify_cc([H|T],[HH|TT]):- !, templify_cc(H,HH),templify_cc(T,TT).

templify_cc(A-(B-(C-(_-_))),A-(B-(C-(_-_)))):- nonvar(C),!.
%templify_cc(A-B-C-_-WP1,A-B-C-_-_):- compound(WP1),!.


templify_cc(N-M-WP1,N-WP2):- !, templify_cc(M-WP1,WP2).
templify_cc(N-WP1,N-_):- is_prop1(WP1),!.
templify_cc(N-WP1,N-WP2):- !, templify_cc(WP1,WP2).


make_unifiable_cc(WP1,WP2):- \+ compound(WP1),!,WP2=WP1.
make_unifiable_cc(N-WP1,N-_):- \+ is_list(WP1),!.
make_unifiable_cc(N-WP1,N-WP2):- !, make_unifiable_cc(WP1,WP2).
make_unifiable_cc([H|T],[HH|TT]):- !, make_unifiable_cc(H,HH),make_unifiable_cc(T,TT).
make_unifiable_cc(cc(C,N),cc(_,N)):- is_real_color(C),!.
make_unifiable_cc(cc(N,_),cc(N,_)):-!.
make_unifiable_cc(oid(_),oid(_)):-!.
make_unifiable_cc(recolor(N,_),recolor(N,_)):-!.
make_unifiable_cc(iz(symmetry_type(N,_)),iz(symmetry_type(N,_))):-!.
make_unifiable_cc(pg(_,G,M,_),pg(_,UG,M,_)):-!,make_unifiable_cc(G,UG).
make_unifiable_cc(O,U):- make_unifiable(O,U).

count_objs(B,O):- \+ is_list(B),!, O = 0.
count_objs(B,O):- \+ ( member(E,B), is_list(E)), \+ ( member(E,B), compound(E), functor(E,F,_), upcase_atom(F,UF),downcase_atom(F,UF)),!,O=1.
count_objs(B,S):- my_maplist(count_objs,B,N),sum_list(N,SS),!,SS=S.

treeify_versions(StoredName,Title,UProp,Versions,HAVENOTS,Missing,OUTS):-
  must_det_ll(treeify_n_versions(StoredName,Title,UProp,Versions,HAVENOTS,Missing,OUTS)).

treeify_n_versions(StoredName,Title,UProp,[Prop|Versions],RRR,Missing,[(Prop->HAVESSTREE)|OUTS]):-
  my_partition(variant_member(Prop),RRR,HAVES,HAVENOTS),
  my_maplist(variant_select(Prop),HAVES,HAVESS),!,
  treeify_props(StoredName,HAVESS,HAVESSTREE),
  treeify_n_versions(StoredName,Title,UProp,Versions,HAVENOTS,Missing,OUTS),
  maybe_unite_oids(StoredName,HAVES),!.
 
treeify_n_versions(_StoredName,_Title,_,    [],_,[],[]):-!.
treeify_n_versions(StoredName,_Title,UProp,[],_,Missing,[((\+ (UProp)) ->MissingTREE)]):-
  treeify_props(StoredName,Missing,MissingTREE),
  maybe_unite_oids(StoredName,Missing),!.

any_to_oid(Obj,_):- \+ compound(Obj),!,fail.
any_to_oid(_->Obj,OID):-any_to_oid(Obj,OID).
any_to_oid(Obj,OID):-is_object(Obj),obj_to_oid(Obj,OID).
any_to_oid(Obj,OID):-is_list(Obj),member(oid(OID),Obj).

maybe_unite_oids(StoredName,Missing):- compound(Missing), Missing = (_ -> List),!, maybe_unite_oids(StoredName,List).
maybe_unite_oids(StoredName,Missing):-
   assert_in_testid(is_objgrp(StoredName,Missing)),
   oids_from(Missing,OIDS),
   length(OIDS,N),
   assert_in_testid(is_oidlist(StoredName,N,OIDS)),
   pp(red,is_oidlist(StoredName,N,OIDS)).


oids_from(Missing,OIDS):-
   findall(OID,(sub_term(E,Missing),compound(E),E=oid(OID)),OIDS).


always_attempt_min_unifier_select(Named,R,RRRR):- attempt_min_unifier_select(Named,R,RRRR),!.
always_attempt_min_unifier_select(_Named,R,R).

attempt_min_unifier_select(Named,N->R,N->RR):- nonvar(R),!, attempt_min_unifier_select(Named,R,RR).
attempt_min_unifier_select(Named,R,UProps->RRRR):- is_list(R),
  member(O,R),member(Prop,O),
  make_unifiable_cc(Prop,UProp),
  findall(UProp,(member(RO,R),member(UProp,RO)),UProps),
  some_min_unifier(UProps,Common),nonvar(Common),
  Common\=@=UProp,
  attempt_min_unifier_select_n(Named,Common,UProps,R,RRRR).


attempt_min_unifier_select_n(Named,Common,_UProps,RR,Common-Actuals->RRRR):-
  maplist(select_variant_or_unify,Common,Actuals,RR,RRR),!,
  treeify_props(Named,RRR,RRRR).

attempt_min_unifier_select_n(Named,Common,UProps,RR,Common-UCommons->RRRR):-
  maplist(select_safe_always(UProps,Common),UCommonsL,RR,RRR),append(UCommonsL,UCommons),
  treeify_props(Named,RRR,RRRR).

select_safe_always(_UProps,E,[O],L,R):- select_variant_or_unify(E,O,L,R),!.
select_safe_always( UProps,_,[O],L,R):- member(E,UProps), select_variant_or_unify(E,O,L,R),!.
select_safe_always( _,_,[],L,L).

variant_select(HAD,R,RR):- select(H,R,RR), H=@=HAD.

variant_select_always(HAD,R,RR):- select(H,R,RR), H=@=HAD,!.
variant_select_always(_,R,R).

select_variant_or_unify(HAD,U,R,RR):- variant_select(HAD,R,RR),!,U=HAD.
select_variant_or_unify(HAD,H,R,RR):- select(H,R,RR), shall_count_as_same(HAD,H). 


treeify_props_these_next(Named,HAD,HHH,RRR,HAD->OUTPUT):-
  findall(H->RObjLO,(member(H,HHH),h_for(Named,H,RRR,RObjLO)),OUTPUT).

divide_after(giz(example_num(_)),iz(cenGX(_))).
divide_after(giz(example_num(_)),iz(cenGY(_))).
divide_after(giz(example_num(_)),center2D(_,_)).
divide_after(giz(_),loc2D(_,_)).

divide_on_very_last(HAD,_):- divide_on_very_last(HAD),!.
divide_on_very_last(HAD,RRR):- divide_after(HAD,R),member(RR,RRR),member(R,RR),!.
divide_on_very_last(links_count(sees,_)).
divide_on_very_last(iz(Num)):- has_subterm(integer,Num).
divide_on_very_last(occurs_in_links(sees,_)).
divide_on_very_last(elink(sees(_),_)).
%divide_on_very_last(grid_ops(comp,_)).
%divide_on_very_last(grid_ops(norm,_)).
divide_on_very_last(INOUT):- has_subterm(in_or_out,INOUT).
%divide_on_very_last(pg(_,_,_,_)).
divide_on_very_last(was_oid(_)).

divide_on_very_last(P):- divide_on_never(P).
divide_on_never(oid(_)).
divide_on_never(giz(_)).


in_or_out(TextIO):- atom(TextIO),in_or_out1(IO),atom_concat(_,IO,TextIO).
in_or_out1(output). in_or_out1(input). in_or_out1(out). in_or_out1(in). in_or_out1(io).

h_for(Named,H,RRR,RObjLO):- 
  findall(RObj,(member(Obj,RRR),select(H,Obj,RObj)),RObjL),
  treeify_props(Named,RObjL,RObjLO).

/*
  my_partition(member(HAD),RRR,HAVES,HAVENOTS), 
  length_s(HAVES,HL),length_s(HAVENOTS,NL),
  my_maplist(variant_select(HAD),HAVES,HAVESS),%HAVESS\=[[]|_],
  treeify_props(Named,HAVESS,FHAVES),
  treeify_props(Named,HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[].
*/

%:- functor(VV,vv,9009),nb_linkval(vv9009,VV).
:- length(VV,9009),nb_linkval(vv9009,VV).
variant_copy(V,C):- ground(V),!,V=C.
variant_copy(V,C):- copy_term(V,C),term_variables(C,CTV),append(CTV,_,OVV),nb_current(vv9009,VV),OVV=VV,!.

sort_vertically(RRR,RR):- \+ is_list(RRR),!,RR=RRR.
sort_vertically(RRR,RRO):- my_maplist(sort_vertically,RRR,RRR0),list_to_set(RRR0,R),
 predsort(first_equals(variant,sort_on(get_loc2D)),R,RR),reverse(RR,RRO),!.
get_loc2D(List,loc2D(V,H)):- sub_term(E,List),compound(E),E=loc2D(H,V),!. 
get_loc2D(P,loc2D(inf,P)).





/*
prior_name_by_size(VM,IndvS0,Named):- 
  Title = save_as_obj_group(Named),
  if_t((true;number(_R)),
   (length_s(IndvS0,Len),
    if_t(Len>0,
     must_det_ll((
      %add_prior_placeholder(Named,IndvS0,IndvS1),
      override_object(birth(Named),IndvS0,IndvS1)
      gset(VM.objs)=IndvS1,
     /*
      %rank_priors(Named,IndvS1,IndvSL),
      gset(VM.objs)=IndvSL,
      nop((( Named == i_nsew ) -> DebugThis = full ; luser_getval(debug,DebugThis)->true; DebugThis=true)),
      nop(( DebugThis\==false -> Feedback = show_indiv(Title) ; Feedback = print_info)),
      nop(ignore((DebugThis\==false, print_grid(VM.h,VM.v,Title,IndvSL)))),     
      %dash_chars,
      */
      addObjectOverlap(VM,IndvSL)))))),!.
*/
/*
%add_prior_placeholder(_,I,I):-!.
add_prior_placeholder(Named,IndvS0,IndvS9):- is_list(IndvS0),!,
  length_s(IndvS0,Len),
  my_maplist(add_prior_placeholder(Len,Named),IndvS0,IndvS9).

add_prior_placeholder(_Len,Named,IndvS0,IndvS9):- 
  override_object(birth(Named),IndvS0,IndvS9),!.

add_prior_placeholder(Len,Named,IndvS0,IndvS9):- 
  (has_prop(pg(OG,Len,_,Named),IndvS0)-> IndvS0=IndvS9 ; 
    ((has_prop(pg(OG,Was,Other,Named),IndvS0)-> delq(IndvS0,pg(OG,Was,Other,Named),IndvS1) ; IndvS0=IndvS1),
     override_object(pg(OG,Len,nil,Named),IndvS1,IndvS9))),!.
*/

object_get_priors(X,S):- var(X),!, enum_object(X), object_get_priors(X,S).
object_get_priors(X,S):- is_object(X), !, must_det_ll((indv_props_list(X,Ps),
  findall(I,(member(P,Ps),props_object_prior(P,I)),L),L\==[],list_to_set(L,S))).

get_prior_labels(Objs,PriorsSetClean,AllPriors,PriorsWithCounts):- must_det_ll((is_list(Objs),
  findall(Named,(member(Obj,Objs),object_get_priors(Obj,Named)),AllPriorsL),
  append(AllPriorsL,AllPriors),
  ssort(AllPriors,PriorsSet),
  my_partition(never_prior,PriorsSet,_Unused,PriorsSetClean),
  count_each(PriorsSetClean,AllPriors,PriorsWithCounts))).

never_prior(giz(_)).
never_prior(oid(_)).
%never_prior(pg(_,_,_,_)).


ranking_pred(rank1(F1),I,Value):- atom(F1),Prop=..[F1,Value], indv_props_list(I,Ps),member_or_iz(Prop,Ps),!.
ranking_pred(rank1(F1),I,Value):- atom(F1),!, catch(call(F1,I,Value),_,fail),!.
ranking_pred(rankA(F1),I,Value):- nonvar(F1),append_term(F1,Value,Prop), indv_props_list(I,Ps),member_or_iz(Prop,Ps),!.
ranking_pred(rankA(F1),I,Value):- nonvar(F1),!, catch(call(F1,I,Value),_,fail),!.
ranking_pred(rank2(F1),I,Value):- atom(F1),Prop=..[F1,O1,O2], indv_props_list(I,Ps),member_or_iz(Prop,Ps),!,combine_number(F1,O1,O2,Value).
%ranking_pred(rank2(F1),I,Value):- !, catch(call(F1,I,O1,O2),_,fail),!,combine_number(F1,O1,O2,Value).
ranking_pred(_F1,I,Value):- mass(I,Value).


indv_props_list_one(L,[Obj]):- is_prop1(L),!,Obj=L.
indv_props_list_one(Obj,L):- indv_props_list(Obj,L).
has_prop(Prop,Obj):- var(Obj),!, enum_object(Obj),nonvar(Obj),has_prop(Prop,Obj).
has_prop(Prop,Obj):- var(Prop),!,indv_props_list_one(Obj,L),!, member(Prop,L).
has_prop(and(A,B),Obj):- !, has_prop(A,Obj),has_prop(B,Obj).
has_prop(call_1(A),Obj):- !, has_prop(AA,Obj),call(A,AA).
has_prop(( \+ A),Obj):- !, \+ has_prop(A,Obj).
has_prop(not(A),Obj):- !, \+ has_prop(A,Obj).
has_prop(or(A,B),Obj):- !, (has_prop(A,Obj);has_prop(B,Obj)).
has_prop(Prop,Props):- is_obj_props(Props),!,member(Q,Props),(Q=@=Prop -> ! ; ( Q = Prop)).
has_prop(Prop,Obj):- is_grid(Obj),!,grid_props(Obj,Props),!,member(Prop,Props).
has_prop(Prop,ObjRef):- atom(ObjRef),into_obj(ObjRef,Obj),ObjRef\=@=Obj,!,has_prop(Prop,Obj).
has_prop(Prop,List):- is_list(List),!,member(Obj,List),has_prop(Prop,Obj).
%has_prop(Prop,Objs):- is_list(Objs),last(Objs,O),is_object(O),!,forall(member(Obj,Objs),has_prop1(Prop,Obj)).
has_prop(_Props,Obj):- is_cons(Obj), \+ is_list(Obj),!,ds,fail.
has_prop(Prop,Obj):- has_prop1(Prop,Obj).


has_prop1(rank1(Lbl),Obj):- atom(Lbl),!, is_prior_prop(rank1(Lbl),Obj),!.
%has_prop1(rank2(Lbl),Obj):- atom(Lbl),!, is_prior_prop(rank2(Lbl),Obj),!.
has_prop1(rankA(Lbl),Obj):- nonvar(Lbl),!, is_prior_prop(rankA(Lbl),Obj),!.
has_prop1(lbl(Lbl),Obj):- !, is_prior_prop(Lbl,Obj).
has_prop1(Lbl ,Obj):- atom(Lbl),!, is_prior_prop(Lbl,Obj),!.
%has_prop1(Prop,Objs):- is_list(Objs),member(Q,L), (Q=@=Prop -> ! ; ( Q = Prop)).
%has_prop1(Var,_Obj):- var(Var),!, fail.
has_prop1(Prop,Obj):- indv_props_list(Obj,L), member(Q,L), (Q=@=Prop -> ! ; ( Q = Prop)).


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
props_object_prior(pg(_OG,L,_,_),L):-!.
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
  length_s(Lbld,LL),  
  rank_priors(Lbl,Lbld,RLbldR),
  nop(print_grid(Lbl->N/LL,RLbldR)),
  write('\t '), writeq(Lbl->N/LL),write(' <p/>\t'),
  append([Unlbl,RLbldR],ObjsWithPrior).  


/*
prior_name_by_size(_VM,[],_Name):-!.
prior_name_by_size(VM,IndvS0,Named):-  
  rank_priors(Named,IndvS0,IndvSL),
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
  % pp(rank_priors(GType)),
   %add_prior_placeholder(GType,Group,Objs),
   smallest_first(smallest_pred(GType),Objs,SF),
   ignore((relivant_group(OG), my_maplist(OG,Objs))),
   length_s(SF,SFLen),
   %nop
   %(SFLen < 2 -> pp(red,rank_priors(GType,SFLen)); pp(green,rank_priors(GType,SFLen))),  
   reverse(SF,SFR),
   add_rank(OG,GType,SFLen,SFR,SFO),
   nop(maybe_show_ranking(GType,SFO)))).


maybe_show_ranking(GType,SFO):-
 dash_chars,dash_chars,dash_chars,dash_chars,
 my_maplist(object_grid,SFO,G),
  length_s(SFO,Len),
  print_list_of(pp,setOF(GType),SFO),
  print_ss(G),
  u_dmsg(setOF(GType)=Len),atrace.


%has_order(O,P1, Code ):- 
% Code = ((
add_rank(OG,GType,ZType,IndvS,IndvSO):- add_rank(OG,GType,ZType,1,IndvS,IndvSO).

add_rank(_OG,_GType,_ZType,_,[],[]):-!.
%add_rank(OG,_GType,_ZType,1,IO,IO):-!.

add_rank(_OG,_GType,_ZType,N,[L],[L]):-  N==1, !.
%add_rank(OG,GType,ZType,N,[L],[Obj]):-   N>1, !, value_sym(firstOF,FO),set_rank(OG,GType,ZType,L,FO,Obj).
%add_rank(OG,GType,ZType,N,[L],[Obj]):-  N==1, !, set_rank(OG,GType,ZType,L,N,Obj),!. %firstAndLastOF
%add_rank(OG,GType,ZType,N,[L|IndvS],[Obj|IndvSO]):- value_sym(lastOF,_LO),set_rank(OG,GType,ZType,L,1,Obj), 
% N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).
add_rank(OG,GType,ZType,N,[L|IndvS],[Obj|IndvSO]):- % (GType = rank1(_);GType = rank2(_)),!, 
 set_rank(OG,GType,ZType,L,/*nthOF**/(N),Obj), 
 N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).
add_rank(OG,GType,ZType,N,[L|IndvS],  [L|IndvSO]):- 
 N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).

value_sym(firstOF,1).
value_sym(lastOF,1).
set_rank(OG,GType,ZType,L,N,Obj):-
  get_setarg_p1(nb_setarg,I,L,P1), 
  compound(I), I = pg(OG,GType,ZType,_),
  II = pg(OG,GType,ZType,N), 
  call(P1 ,II),!, Obj = L.

set_rank(OG,GType,ZType,L,N,Obj):- 
   II = pg(OG,GType,ZType,N), 
   override_object([II],L,Obj),!.

%:- retractall(arc_cache:individuated_cache(_,_,_,_,_)).
%:- ignore(muarc:clear_all_caches).
%:- luser_setval(use_individuated_cache,false).



read_terms_from_atom(D,Atom):-
  open_string(Atom,Stream),
  repeat,
   read_term(Stream,D,[]),
   ((D == end_of_file) -> !, fail ; true).

%learn_ilp:- ensure_test(TestID),learn_ilp(TestID).
learn_ilp(TestID):- 
  ensure_test(TestID),
  must_det_ll((
    ensure_test(TestID),
    %abolish(arc_test_properties/3), dynamic(arc_test_properties/3),
    abolish(is_for_ilp/4), dynamic(is_for_ilp/4),
    %compile_and_save_hints(TestID),
    individuate_all_pairs_from_hints(TestID),
    with_individuated_cache(true, 
     forall(kaggle_arc(TestID,ExampleNum,I,O),
      must_det_ll(learn_ilp(TestID,ExampleNum,I,O)))),!,
    listing(is_for_ilp/4),
    dump_ilp_files(S))),
    compute_scene_change(TestID),
    must_det_ll(write_ilp_file(TestID,S,logicmoo_ex)),
    solve_via_scene_change(TestID).

:- abolish(is_for_ilp/4).
:- dynamic(is_for_ilp/4).
learn_ilp(TestID,ExampleNum,GridIn,GridOut):-  
 ExampleNum = (trn+_),!,
  must_det_ll((
    set_example_num(ExampleNum),
    individuate_pair(complete,GridIn,GridOut,InC,OutC),
    into_ilp_int(ExampleNum,ExampleID),
    assert_ilp(TestID,ExampleNum,liftcover_models,begin(model(ExampleID))),
    maplist(make_ilp_pred(TestID,ExampleNum,lhs),InC),
    maplist(make_ilp_pred(TestID,ExampleNum,rhs),OutC),!,
    assert_ilp(TestID,ExampleNum,liftcover_models,end(model(ExampleID))),
    assert_ilp(TestID,ExampleNum,liftcover_models,[]),
    assert_ilp(TestID,ExampleNum,liftcover_models,[]),
  !)).
learn_ilp(TestID,ExampleNum,GridIn,GridOut):-  
 ExampleNum = (tst+_),!,
 must_det_ll((
   set_example_num(ExampleNum),
   individuate_pair(complete,GridIn,GridOut,InC,OutC),
   into_ilp_int(ExampleNum,ExampleID),
   assert_ilp(TestID,ExampleNum,_,"/*"),
   assert_ilp(TestID,ExampleNum,liftcover_models,begin(model(ExampleID))),
   maplist(make_ilp_pred(TestID,ExampleNum,lhs),InC),
   maplist(make_ilp_pred(TestID,ExampleNum,rhs),OutC),!,
   assert_ilp(TestID,ExampleNum,liftcover_models,end(model(ExampleID))),
   assert_ilp(TestID,ExampleNum,_,"*/"))).

into_ilp_int(Int,Int):- integer(Int),!.
into_ilp_int(Example+Num,ExampleID):- % integer(Int).
   atomic_list_concat([Example,Num],'_',ExampleID),!.

assert_ilp(TestID,ExampleNum,File,Term):- ground(ExampleNum+File), clause_asserted(is_for_ilp(TestID,ExampleNum,File,Term)),!.
assert_ilp(TestID,ExampleNum,File,Term):- pp(File=Term),!, 
  assert_if_new(is_for_ilp(TestID,ExampleNum,File,Term)),!.

safe_oid(OID,OOID):- atomic_list_concat(['o',_Glyph,Iv,_TV,_UUID,_Trn,Num,IO],'_',OID),
                     atomic_list_concat(['obj',Num,Iv,IO],'_',OOID).


%make_ilp_pred(_TestID,Example+_Num,_LRSide,_Obj):- Example==tst,!.
make_ilp_pred(TestID,Example+Num,LRSide,ObjIn):- 
 must_det_ll((   
   into_obj(ObjIn,Obj),
   ExampleNum = Example+Num,
   obj_to_oid(Obj,OIDUS),
   safe_oid(OIDUS,OID),
   %id_shape(SID,LPs),next_to_32(LPs,Shape),   
   rhs_obj_format(ArgNames),
   assert_ilp_object(TestID,ExampleNum,LRSide,Obj,OID,ArgNames))).


rhs_obj_format([loc2D,rot2D,pen_color,rotSize2D(grav),vis2D,mass,iz(sid)]).

into_2arg(_,_, V1,V2,A2):- V2==true,!, A2 = V1.
into_2arg(_,_,_V1,V2,A2):- V2==false,!, A2 = V2.
into_2arg(N,_, V1,V2,A2):- into_hv(N,V1,V2,A2).

into_hv(N,V1,V2,A2):- atom(N), !, A2=..[N,V1,V2].
into_hv(N,V1,V2,A2):- A2=..[hv,N,V1,V2].

ilp_object_props(Obj,Props,OID,LRhs,Named,V,Pred,Prop):-
  must_det_ll(ilp_object_props_1(Obj,Props,OID,LRhs,Named,V,Pred,Prop)).

ilp_object_props_1(_Obj,Props,OID,LRhs,iz(E),V,Pred,iz(P)):- append_term(E,V,P), member(iz(P),Props),!,
   functor(P,EF,_), atomic_list_concat([LRhs,'iz',EF],'_',EFIZ), Pred=..[EFIZ,OID,V].
ilp_object_props_1(_Obj,Props,OID,LRhs,iz(E),A2,Pred,iz(P)):- append_term(E,V1,P0), append_term(P0,V2,P),
   member(iz(P),Props),!,
   functor(P,EF,_), atomic_list_concat([LRhs,'iz',EF],'_',EFIZ), Pred=..[EFIZ,OID,V1,V2],into_2arg(E,EF,V1,V2,A2).
ilp_object_props_1(_Obj,Props,OID,LRhs,(E),V,Pred,(P)):- E\=iz(_), append_term(E,V,P), member((P),Props),!,
   functor(P,EF,_), atomic_list_concat([LRhs,EF],'_',EFIZ), Pred=..[EFIZ,OID,V].
ilp_object_props_1(_Obj,Props,OID,LRhs,(E),A2,Pred,(P)):- E\=iz(_), append_term(E,V1,P0), append_term(P0,V2,P), member((P),Props),!,
   functor(P,EF,_),atomic_list_concat([LRhs,EF],'_',EFIZ), Pred=..[EFIZ,OID,V1,V2],into_2arg(E,EF,V1,V2,A2).
ilp_object_props_1(Obj,_Props,OID,LRhs,(E),V,Pred,Prop):- E\=iz(_), append_term(E,Obj,P0),append_term(P0,V,P), call(P),!,
  functor(P,EF,_), atomic_list_concat([LRhs,EF],'_',EFIZ),
  append_term(E,OID,P1),append_term(P1,V,PredR),append_term(E,V,Prop),
  PredR=..[_|PredL], Pred=..[EFIZ|PredL].

%oid_to_lhs(OID,NewRef):- oid_to_lhs_hide(OID,NewRef),!.
oid_to_lhs_hide(OID,NewRef):- 
 must_det_ll((
   into_obj(OID,Obj),
   indv_props_list(Obj,Props),
   lhs_obj_format(Fmt),
   maplist(ilp_object_props(Obj,Props,OID,lhs),Fmt,_Args,_TypeSig,PropL),
   %writeq(NewRef=TypeSig),nl,
   %NewRef =.. [lhs|Args],!,
   NewRef = lhs_obj(PropL))).

lhs_obj_format([loc2D,rot2D,pen,rotSize2D(grav),vis2D,mass,iz(sid)]).

%maplist(un_lhs(B),TypeSig,Prop)

pen_color(Obj,Color):- (pen(Obj,[cc(Color,_)])->true;(pen(Obj,PenInfo),Color=pen(PenInfo))),!.


assert_ilp_object(TestID,ExampleNum,LRSide,Obj,OID,ArgNames):-
 must_det_ll((
   into_ilp_int(ExampleNum,ExampleID),
   indv_props_list(Obj,Props),
   if_t(LRSide==lhs, assert_ilp_typed(LRSide,TestID,ExampleNum,liftcover_models,lhs_peice(ExampleID,OID))),
   if_t(LRSide==rhs, assert_ilp_typed(LRSide,TestID,ExampleNum,liftcover_models,rhs_peice(ExampleID,OID))),
   maplist(ilp_object_props(Obj,Props,OID,LRSide),ArgNames,Args,TypeSig,_),
   maplist(assert_ilp_typed(LRSide,TestID,ExampleNum,liftcover_models),TypeSig),
   Side =..[LRSide,ExampleID|Args],
   if_t(LRSide==rhs, assert_ilp_typed(LRSide,TestID,ExampleNum,exs,pos(Side))),
   if_t(LRSide==lhs, assert_ilp_typed(LRSide,TestID,ExampleNum,bk,Side)))).

next_to_32(I,O):- next_to_32s(I,M),atom_chars(O,M),!.
next_to_32s([N32|Ints],[36|Rest]):- N32==10, !,next_to_32s(Ints,Rest),!.
%next_to_32s([N32,N|Ints],[N|Rest]):- (N32==32;N32==109), number(N), N>31,N\==95,N<128,!,next_to_32s(Ints,Rest),!.
%next_to_32s([N|Ints],Rest):- number(N),next_to_32s(Ints,Rest),!.
next_to_32s([N|Ints],[N|Rest]):- number(N),next_to_32s(Ints,Rest),!.
next_to_32s([],[]).
next_to_32s(S,N32):- string(S),with_output_to(codes(Chars),write(S)),!,next_to_32s(Chars,N32).
next_to_32s(S,N32):- into_grid(S,G),into_ngrid(G,SS),ngrid_to_sgrid(SS,SSS),
 numbervars(SSS,999,_,[singletons(true)]),with_output_to(codes(Chars), write(SSS)),!,
 next_to_32s(Chars,N32).

assert_ilp_typed(LRSide,TestID,ExampleNum,File,Term):- 
  ignore(remember_types(LRSide,TestID,Term)),
  assert_ilp(TestID,ExampleNum,File,Term).

remember_types(LRSide,TestID,pos(Term)):-!, remember_types(LRSide,TestID,Term).
remember_types(LRSide,TestID,Term):-
 must_det_ll((
  compound_name_arguments(Term,F,Args),
  compound_name_arity(Term,F,A),
  atomic_list_concat(NameL,'_',F), reverse(NameL,NameR),
  maplist(must_guess_types(LRSide,NameR,Term),Args,Types),
  compound_name_arguments(TypeTerm,F,Types),
  if_t(LRSide=rhs,assert_ilp(TestID,_,determination(0),output(F/A))),
  if_t(LRSide=lhs,assert_ilp(TestID,_,determination(1),input_cw(F/A))),
  if_t(LRSide=rhs,assert_ilp(TestID,_,determination(2),modeh(*,TypeTerm))),
  if_t(LRSide=lhs,assert_ilp(TestID,_,determination(3),modeb(*,TypeTerm))),
  !)).

must_guess_types(LRSide,NameR,Term,Args,Types):- must_det_ll(guess_types(LRSide,NameR,Term,Args,Types)).

guess_types(_,[peice|_],T,A,+peice):- arg(2,T,O),A==O,!.
guess_types(_,[peice|_],_,_,+scope):-!.
guess_types(_,_,T,A,+peice):- arg(1,T,O),A==O,!.
guess_types(_,[mass|_],_,_,nat900).
guess_types(_,_,_,Arg,+nat30):- integer(Arg),between(1,30,Arg),!.
guess_types(_,_,_,Arg,+(#(color))):- is_color(Arg),!.
%guess_types(_,_,_,Arg,+pred2):- atom(Arg),!.
%guess_types(_,_,_,Arg,+unknown(Arg)): -compound(Arg).
guess_types(lhs,[Type|_],_,_Arg,-Type).
guess_types(rhs,[Type|_],_,_Arg,+Type).

ngrid_to_sgrid(SS,SSS):- mapgrid(into_sarg,SS,SSS).
into_sarg(A,A):- \+ compound(A),!.
into_sarg(A-fg,A):-!.
into_sarg(_-bg,bg):-!.
into_sarg(A,A).

dump_ilp_files:- dump_ilp_files(_).
dump_ilp_files(S):-
 must_det_ll((
   get_current_test_atom(TestAtom),
   if_t(var(S),sformat(S,'out/ilp/~w',[TestAtom])),
   make_directory_path(S),
   write_ilp_file(TestID,S,bias),
   write_ilp_file(TestID,S,bk),
   write_ilp_file(TestID,S,exs),!,
   write_ilp_file(TestID,S,metagol_ex),!,
   write_ilp_file(TestID,S,input),!,
   write_ilp_file(TestID,S,foil_ex),!,
   write_ilp_file(TestID,S,liftcover_ex),!,
   wdmsg(ls(S)),
   ls(S))).

write_ilp_file(TestID,Dir,Bias):-
  sformat(S,'~w/~w.pl',[Dir,Bias]),
  setup_call_cleanup(open(S,write,Out,[]),
     forall(get_is_for_ilp(TestID,_,Bias,Term),
      output_term(Out,Term)),
    close(Out)).

:- op(500,yf,in_cmt).

output_term(_,Nil):- var(Nil),!.
output_term(_,Nil):- Nil==[],!,nl.
output_term(_, (:- Nil)):- Nil==[],!,nl.
output_term(_, (:- Nil)):- var(Nil),!.
output_term(Out,Term):- \+ ground(Term),!, 
  \+ \+ (numbervars(Term,15,_,[singletons(true)]),!,output_term(Out,Term)).

output_term(Out,(:- Term)):- string(Term),!,format(Out,'~N~w~n',[Term]).
output_term(Out,Term):- string(Term),!,format(Out,'~N~w~n',[Term]).
output_term(Out, :- (Term in_cmt)):- format(Out,'~N% :- ~q. ~n',[Term]).
output_term(Out,Term in_cmt):- format(Out,'~N% ~q. ~n',[Term]).
output_term(Out,Term):- format(Out,'~N~q. ~n',[Term]).

get_is_for_ilp(_,_,input, :-(D) ):- 
   member(D,
     [
        (style_check(-discontiguous)),
        (use_module(library(aleph))),
        (if(current_predicate(use_rendering/1))),
        (use_rendering(prolog)),
        (endif),
        [],
        (aleph_set(verbosity, 1)),
        (aleph_set(interactive, false)),
        (aleph_set(i,4)),
        (aleph_set(nodes,10000)),
        [],
        (aleph),
        []]),
   D\==[].


get_is_for_ilp(_,_,input, :-(D) ):- get_is_for_ilp(_,_,determination, D ).

get_is_for_ilp(TestID,common,logicmoo_ex,is_accompany_changed_db(TestID,IO,P,Same)):-
  is_accompany_changed_db(TestID,IO,P,Same).


get_is_for_ilp(_,_,liftcover_ex,D):- read_terms_from_atom(D, '

:-use_module(library(slipcover)). 
:-if(current_predicate(use_rendering/1)). 
:-use_rendering(prolog). 
:-endif. 
:-sc. 

:- set_sc(verbosity,3).
:- set_sc(depth_bound,false).
:- set_sc(neg_ex,given).

bg([]).

in([]).

input_cw(incr_nat30/2).
input_cw(color_change/2).


determination(OF/OA,IF/IA):-
  input_cw(IF/IA),output(OF/OA).

  

fold(trn_0,[trn_0]).
fold(trn_1,[trn_1]).
fold(trn_2,[trn_2]).

').

get_is_for_ilp(_,_,liftcover_ex,D):-get_is_for_ilp(_,_,determination, D ).

get_is_for_ilp(_,_,determination, D ):- 
   member(D,
     [
        modeh(*,rhs(+state,+nat30,+nat30,+color,+nat30,+nat30,+rotation,+nat900,+shape,+list))  in_cmt,
        modeb(*,lhs(+state,+nat30,+nat30,#(color),+nat30,+nat30,+rotation,+nat900,+shape,+list))  in_cmt,
        modeb(*,my_geq(+nat30,-#(nat30))) in_cmt,
        modeb(*,my_leq(+nat30,-#(nat30))) in_cmt,
        modeb(*,my_add(+nat30,+nat30,-nat30)) in_cmt,
        modeb(*,my_mult(+nat30,#(nat30),-nat30))  in_cmt,

        [],
        (lazy_evaluate(my_add/3)) in_cmt,
        (lazy_evaluate(my_geq/2)) in_cmt,
        (lazy_evaluate(my_leq/2)) in_cmt,
        (lazy_evaluate(my_mult/3)) in_cmt,
        [],
        determination(rhs/7,lhs/7) in_cmt,
        determination(rhs/7,color_change/2) in_cmt,
        determination(rhs/7,incr_nat30/2) in_cmt,
        determination(rhs/7,my_geq/2) in_cmt,
        determination(rhs/7,my_leq/2) in_cmt,
        determination(rhs/7,my_add/3) in_cmt,
        determination(rhs/7,my_mult/3) in_cmt,

        []]),
   D\==[].

get_is_for_ilp(A,B,input, D ):- get_is_for_ilp(A,B,bias,D).
get_is_for_ilp(A,B,input, D ):- ((D = (:-begin_bg));get_is_for_ilp(A,B,bk,D);(D = (:-end_bg))).
get_is_for_ilp(A,B,input, D ):- ((D = (:-begin_in_pos));get_is_for_ilp(A,B,exs,D);(D = (:-end_in_pos))).

get_is_for_ilp(A,B,metagol_ex, D ):- get_is_for_ilp(A,B,bias,D).
get_is_for_ilp(A,B,metagol_ex, D ):- get_is_for_ilp(A,B,bk,D).
get_is_for_ilp(A,B,metagol_ex, D ):- get_is_for_ilp(A,B,exs,D).

%get_is_for_ilp(A,B,liftcover_ex, D ):- get_is_for_ilp(A,B,input, D ).
get_is_for_ilp(A,B,foil_ex, D ):- get_is_for_ilp(A,B,input, D ). 

get_is_for_ilp(_,_,bias,D):- 
   member(D,
  [ (:-style_check(-discontiguous)),
    max_body(6),
    max_vars(8),
    non_magic(4),
    %enable_pi,
    head_pred(rhs,7)  in_cmt,
    body_pred(lhs,7)  in_cmt,
    body_pred(child,2) in_cmt,
 /* 
    body_pred(cenGX,2),
    body_pred(cenGY,2),
    body_pred(mass,2),
    body_pred(color,2),
    body_pred(medium,1),
    body_pred(large,1),
    body_pred(position,3),
    body_pred(rotation,2),
    body_pred(traits,2),
    body_pred(contact,2),*/
    %:- clause(C), #count{V : clause_var(C,V),var_type(C,V,ex)} != 1,
    body_pred(incr_nat30_by,3) in_cmt,
    body_pred(incr_nat30,2),
    body_pred(color_change,2),    
    body_pred(my_add,3), 
    body_pred(my_geq,2), 
    body_pred(my_leq,2),
    body_pred(my_mult,3),
    bounds(my_add,1,(0,29)),
    bounds(my_geq,1,(1,30)),
    bounds(my_leq,1,(1,30)),
    bounds(my_mult,1,(1,10)),
    direction(color_change,(out,out)) in_cmt, 
    direction(incr_nat30,(out,out)) in_cmt,
    direction(my_add,(in,in,out)) in_cmt,
    direction(my_geq,(in,out)) in_cmt,
    direction(my_leq,(in,out)) in_cmt,
    direction(my_mult,(in,out,in)) in_cmt,
/*
    direction(large,(out)),
    direction(medium,(out)),
    direction(small,(out)),

    direction(cenGX,(in,out)),
    direction(cenGY,(in,out)),
    direction(child,(in,out)),
    direction(color,(in,out)),
    direction(contact,(in,out)),
    direction(orientation,(in,out)),
    direction(piece,(in,out)),
    direction(position,(in,out,out)),
    direction(rotation,(in,out)),
    direction(size,(in,out)),
*/
    type(my_add,(nat30, nat30, nat30)),
    type(my_mult,(nat30, nat30, nat30)),
    type(my_geq,(nat30,nat30)),
    type(my_leq,(nat30,nat30)),
    type(incr_nat30,(nat30,nat30)),
    type(color_change,(color,color)),
    
   /* 
    type(large,(nat30)),
    type(medium,(nat30)),
    type(small,(nat30)),*/
    %    ExampleID, OID,   X,      Y,Color,SH,SV,RotG,Size,Shape
 
    direction(rhs,(in,in,in,in,in,in,in,in,in,in)) in_cmt,
    type(rhs,(state,loc2D,rot2D,color,vis2D,rotSize2D,nat900,shape)),
    direction(lhs,(out,out,out,out,out,out,out,out,out,out)) in_cmt,
    type(lhs,(state,loc2D,rot2D,color,vis2D,rotSize2D,nat900,shape)),

    /*
    type(cenGX,(piece,nat30)), type(cenGY,(piece,nat30)),
    type(color,(piece,color)),
    type(orientation,(piece,orientation)),
    type(rotation,(piece,rotation)),
    type(piece,(state,piece)),
    type(position,(piece,nat30,nat30)),    
    type(mass,(piece,nat900)),
    type(contact,(piece,piece)),
    type(child,(piece,piece)),
    
    */
    %type(incr_nat30_by,(nat30,nat30,nat30)),
    %magic_value(rhs,1),
    %magic_value(rhs,2),
    %magic_value(rhs,3),
    %magic_value(rhs,4),
    %magic_value(rhs,5),
    %magic_value(rhs,6),
    %magic_value_all,
    %magic_value_type(color),
    %magic_value_type(nat30),
    %magic_value_type(nat900),
    %magic_value_type(piece),
    %magic_value_type(shape),    
    magic_type(color),
    magic_type(nat30),
    magic_value_type(color),
    magic_value_type(nat30),
    numerical_pred(my_add,3),
    numerical_pred(my_geq,2),
    numerical_pred(my_leq,2),
    numerical_pred(my_mult,3),


    []]).

get_is_for_ilp(_,_,bk,D):- 
   member(D,
   [ %(:- dynamic contact/2), large(10), medium(5),small(1),
    (:- use_module(library(clpfd))),
    (incr_nat30(A,B) :- B #= A + 1),
    (color_change(_,_)),
    (my_geq(A,B) :- nonvar(A), nonvar(B), !, A>=B),    
    (my_leq(A,B) :- nonvar(A), nonvar(B), !, A=<B),
    (my_add(A,B,C) :- nonvar(A), nonvar(B), integer(A), integer(B), C is A+B),
    (my_add(A,B,C) :- nonvar(A), nonvar(C), integer(A), integer(C), B is C-A),
    (my_add(A,B,C) :- nonvar(C), nonvar(B), integer(B), integer(C), A is C-B),
    (my_mult(A,B,C) :- nonvar(A), nonvar(B), integer(A), integer(B), C is A*B),
    (my_mult(A,B,C) :- nonvar(A), nonvar(C), integer(A), integer(C), \+(A=0.0), \+(A=0), B is C/A),
    (my_mult(A,B,C) :- nonvar(C), nonvar(B), integer(B), integer(C), \+(A=0.0), \+(A=0), A is C/B)]).


get_is_for_ilp(_,_,bk,D):- read_terms_from_atom(D, '

:- use_module(library(clpfd)).

size(30).

at_left(hv(1,_)).

at_top(hv(_,1)).

at_bottem(hv(_,Y)):- size(Y).
at_right(hv(X,_)):- size(X).

right(hv(X1,Y),hv(X2,Y)):-
    size(Size),
    X1 #< Size,
    X2 #= X1 + 1.

left(hv(X1,Y),hv(X2,Y)):-
    X1 #> 1,
    X2 #= X1 - 1.

down(hv(X,Y1),hv(X,Y2)):-
    size(Size),
    Y1 #< Size,
    Y2 #= Y1 + 1.

up(hv(X,Y1),hv(X,Y2)):-
    Y1 #> 1,
    Y2 #= Y1 - 1.

').


get_is_for_ilp(A,B,determination, D ):- get_is_for_ilp(A,B,determination(0), D ).
get_is_for_ilp(A,B,determination, D ):- get_is_for_ilp(A,B,determination(1), D ).
get_is_for_ilp(A,B,determination, D ):- get_is_for_ilp(A,B,determination(2), D ).
get_is_for_ilp(A,B,determination, D ):- get_is_for_ilp(A,B,determination(3), D ).
get_is_for_ilp(A,B,determination, D ):- get_is_for_ilp(A,B,determination(4), D ).
get_is_for_ilp(A,B,liftcover_ex, D ):- get_is_for_ilp(A,B,bk, D ).
get_is_for_ilp(A,B,liftcover_ex,D):- is_for_ilp(A,B,liftcover_models,D).
get_is_for_ilp(A,B,C,D):- is_for_ilp(A,B,C,D).





