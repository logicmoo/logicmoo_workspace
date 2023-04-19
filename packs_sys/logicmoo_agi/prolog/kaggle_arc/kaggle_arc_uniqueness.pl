/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


% ARC challenged me to write code that i knew i could but would take me some learning to do. 
% thought i could put off until after funding..
% Really I thought it was more something Fabrizio could write. 
% but evidently it is considered impressive feat
/*
he big joke i am working on to really sting everyone is i have told several people exactly how my code works.. and they just don't believe anyone could make it work the way i did .. my explanations are so incredulous sounding ..  but yet the system does work and does everything i say it does..
So as the community sees my ARC stuff violate their beliefs, and the logicmoo stuff sounding so equally incredulous might be worth trusting me on (edited)

*/


% MOO mean multi user OO universe
% OO (Object Orientated) is sort of passee now but i ideally the logicmoo meaning is based on 
% an abstration ("OO") of a refernce to a multi-propertied thing
% rether than defining these properties by soem instances of objects, I define them as calls to logical relations defined by situation calculus 
% these things have simularities that make them groupable for arbitrary reasons


:- include(kaggle_arc_header).

:- dynamic(is_for_ilp/4).
:- dynamic(is_accompany_changed_db/4).
clear_scene_rules(TestID):- 
  forall(is_accompany_changed_db(TestID,IO,P,Same),
     ignore(retract(is_accompany_changed_db(TestID,IO,P,Same)))).

count_of(G,N):- findall(G,G,L),variant_list_to_set(L,S),length(S,N).

dont_notice(oid(_)).
dont_notice(giz(_)).
dont_notice(global2G(_,_)).
dont_notice(link(sees(_),_)).
%dont_notice(links_count(sees,_)).
%dont_notice(occurs_in_links(sees,_)).
dont_notice(link(contains,_)).
dont_notice(occurs_in_links(contained_by,_)).
dont_notice(iz(i_o(_))).
dont_notice(P):- compound(P),arg(_,P,E),is_gridoid(E),!.
dont_notice(P):- compound(P),!,compound_name_arity(P,F,_),!,dont_notice(F).
dont_notice(F):- \+ atom(F),!,fail.
dont_notice(oid).
dont_notice(giz).
dont_notice(shape_rep).

do_notice(pg(_,_,rank1,_)).
do_notice(pg(_,_,_,_)).

ok_notice(X):- \+ \+ do_notice(X),!.
ok_notice(X):- \+ dont_notice(X).

has_propcounts(TestID):- 
 forall(current_example_nums(TestID,ExampleNum),
  ( \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(in,IO)),
    \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(out,IO)))).

%ensure_propcounts(_TestID):-!.
ensure_propcounts(TestID):- ensure_test(TestID),ensure_propcounts1(TestID).
ensure_propcounts1(TestID):- has_propcounts(TestID),!.
ensure_propcounts1(TestID):- once((with_pair_mode(whole_test,
    with_luser(menu_key,'o',once(ndividuator(TestID)))))),has_propcounts(TestID),!.
ensure_propcounts1(TestID):- show_prop_counts(TestID), my_assertion(has_propcounts(TestID)),!.



props_change(TestID,IO,E):-
  ensure_propcounts(TestID),
  findall(P-I_or_O,counts_change(TestID,_,I_or_O,P,_,_),L),list_to_set(L,S),!,member(E-IO,S).

in_out_atoms(in,out).

counts_change(TestID,ExampleNum,Out,X,N2,N1):- in_out_atoms(In,Out),
   ensure_propcounts(TestID),
   propcounts(TestID, ExampleNum, Out, count, N1, X), ok_notice(X),
   ExampleNum = trn+_,
   (propcounts(TestID, ExampleNum, In, count, N2, X) -> true ; N2=0), N1\==N2.

counts_change(TestID,ExampleNum,In,X,N1,N2):- in_out_atoms(In,Out),
   ensure_propcounts(TestID),
   propcounts(TestID, ExampleNum, In, count, N1, X), ok_notice(X),
   ExampleNum = trn+_,
   (propcounts(TestID, ExampleNum, Out, count, N2, X) -> true ; N2=0), N1\==N2.

compute_scene_change(TestID):-
 with_pair_mode(whole_test, 
 must_det_ll((banner_lines(red,4),
  ensure_test(TestID),
  clear_scene_rules(TestID),  
  compute_scene_change_pass1(TestID),
  banner_lines(orange,4),
  compute_scene_change_pass2(TestID),
  banner_lines(yellow,4),
  compute_scene_change_pass3(TestID),
  banner_lines(blue,4),
  nop(compute_scene_change_pass4(TestID))))).


compute_scene_change_pass1(TestID):- 
  show_object_dependancy(TestID).

compute_scene_change_pass2(TestID):-
  forall(props_change(TestID,IO,P),
    forall(prop_can(IO,P,Same),
      assert_ilp_new(is_accompany_changed_db(TestID,IO,P,Same)))).

assert_ilp_new(Term):- clause_asserted(Term),!.
assert_ilp_new(Term):- pp(assert_ilp_new=Term),!, assert_if_new(Term).

assert_become_new(Term):- clause_asserted(Term),!.
assert_become_new(Term):- pp_obj_to_grids(assert_become_new=Term),!, assert_if_new(Term).


solve_via_scene_change(TestID):-  
 ensure_test(TestID),
 clear_scene_rules(TestID),
 (\+ is_accompany_changed_db(TestID,_,_,_) -> compute_scene_change(TestID) ; true),
 show_scene_change_rules(TestID),
 %ExampleNum=_+_,
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore((solve_via_scene_change_rules(TestID,ExampleNum)))),
 show_object_dependancy(TestID).

solve_via_scene_change_rules(TestID,ExampleNum):-
  kaggle_arc(TestID,ExampleNum,_,Expected),
  banner_lines(green,4),
  obj_group5(TestID,ExampleNum,in,ROptions,Objs),Objs\==[],
  solve_obj_group(TestID,ExampleNum,in,ROptions,Objs,OObjs),
  dash_chars,
  print_ss(wqs(solve_via_scene_change(ExampleNum)),Objs,OObjs),
  dash_chars,
  into_solid_grid(OObjs,OurSolution),  
  into_solid_grid(Expected,ExpectedOut),
  count_difs(ExpectedOut,OurSolution,Errors),
  print_ss(wqs(solve_via_scene_change_rules(TestID,ExampleNum,errors=Errors)),ExpectedOut,OurSolution),
  
  (Errors == 0 ->  banner_lines(green,4) ; (banner_lines(red,4),show_scene_change_rules(TestID),!,fail)).
   




show_scene_change_rules(TestID):-
  ensure_test(TestID),
  (\+ is_accompany_changed_db(TestID,_,_,_) -> compute_scene_change(TestID) ; true),
  banner_lines(cyan,4),
  show_assumed_mapped(TestID),
  banner_lines(cyan,3),
   Ele = ac2(P,IO,Same),
   findall(Ele,is_accompany_changed_computed(TestID,IO,P,Same),List),
   sort(List,SetR),reverse(SetR,Set),
   forall(member(Ele,Set),
     (list_to_conjuncts(Same,Conj),pp(P:-Conj),writeln('.'))), 
  banner_lines(cyan,4).


compute_scene_change_pass3(TestID):-
   findall(IO-P,is_accompany_changed_db(TestID,IO,P,_),Ps),
   variant_list_to_set(Ps,Set),
   maplist(compute_scene_change_pass3a(TestID),Set),
   maplist(compute_scene_change_pass3b(TestID),Set),
   maplist(compute_scene_change_pass3c(TestID),Set).

compute_scene_change_pass4(TestID):-
   compute_scene_change_pass3(TestID).

compute_scene_change_pass3a(TestID,IO-P):- 
   findall(Same,is_accompany_changed_db(TestID,IO,P,Same),List),
   List=[_,_|_],
   flatten(List,SameF), variant_list_to_set(SameF,SameS),
    update_accompany_changed_db(TestID,IO,P,SameS).
compute_scene_change_pass3a(_,_).

compute_scene_change_pass3b(TestID,IO-P):- 
   findall(Same,is_accompany_changed_db(TestID,IO,P,Same),List),
   flatten(List,SameF), variant_list_to_set(SameF,SameS),
   correct_antes1(TestID,IO,P,SameS,Kept), Kept\==[],!, % pp(P=compute_scene_change_pass3([SameS,Kept])),
  update_accompany_changed_db(TestID,IO,P,Kept).
compute_scene_change_pass3b(_,_).

compute_scene_change_pass3c(TestID,IO-P):-
   make_unifiable_u(P,Prop),
   is_accompany_changed_db(TestID,IO,P,Same),
   is_accompany_changed_db(TestID,IO,Prop,DSame),
   P\=@=Prop,
   maplist(make_unifiable_u,DSame,USame),
   intersection(Same,USame,Kept,_,_),Kept\==[],
   update_accompany_changed_db(TestID,IO,P,Kept).
compute_scene_change_pass3c(_,_).


update_accompany_changed_db(TestID,IO,P,Kept):- Kept\==[],
   forall(retract(is_accompany_changed_db(TestID,IO,P,_)),true),
   assert_become_new(is_accompany_changed_db(TestID,IO,P,Kept)).

at_least_one_overlap(DSame,Same):-
  member(DS,DSame),member(S,Same),
  (DS=@=S;other_val(S,DS)),!.

correct_antes1(TestID,IO,P,Same,SL):- 
  findall(S,
   (member(S,Same),
     \+ \+ ((
       forall((is_accompany_changed_db(TestID,IO,DP,DSame),at_least_one_overlap(DSame,Same)),
          ((P==DP)-> true; (member(DS,DSame),other_val(S,DS))))))),
   SL), SL\==[],!.
correct_antes1(_TestID,_P,Same,Same).

/*
correct_antes2(TestID,IO,P,Same,Kept):-    
   is_accompany_changed_db(TestID,DP,DSame),
   other_val(P,DP),
   include(other_vals_frm(DSame),Same,Kept), Kept\==[],!.
correct_antes2(_TestID,_P,Same,Same).
other_vals_from(DSame,E):- member(DS,DSame),other_val(E,DS),!.
 
*/

correct_antes3(TestID,IO,P,Same,SameS):-
 share_level(Level), 
  my_partition(not(shared_prop_U(TestID,IO,P,Level)),Same,_Lost,SameS).
  
/*
correct_antes3(TestID,IO,P,Same,SameS):-
 share_level(Level), 
  include(shared_prop_U(TestID,IO,P,Level),Same,SameS),!.
correct_antes3(TestID,IO,P,Same,SameS):-
  include(shared_prop_U(TestID,IO,P,_),Same,SameS),!.

*/
correct_antes3(_,_,Same,Same):-!.

share_level(all). %share_level(5). share_level(4). 
share_level(3). share_level(2).

shared_prop_U(TestID,IO,P,ShareLevel,Same):- var(TestID),!,get_current_test(TestID),!,shared_prop_U(TestID,IO,P,ShareLevel,Same).
shared_prop_U(TestID,IO,P,ShareLevel,Same):- var(P),!,ensure_prop_change(IO,P),shared_prop_U(TestID,IO,P,ShareLevel,Same).
shared_prop_U(TestID,IO,P,ShareLevel,Same):- var(ShareLevel),!,share_level(ShareLevel),shared_prop_U(TestID,IO,P,ShareLevel,Same).

shared_prop_U(TestID,IO,P,ShareLevel,Same):- var(Same),!,
  is_accompany_changed_computed(TestID,IO,P,SameL),
  member(Same,SameL),
  shared_prop_U(TestID,IO,P,ShareLevel,Same).
/*
shared_prop_U(TestID,IO,P,_,Same):- !,
  forall(current_example_nums(TestID,ExampleNum),
  (set_example_num(ExampleNum),
   findall(O,(kaggle_arc_io(TestID,ExampleNum,out,Grid),individuate(complete,Grid,Objs), 
     %print_ss(wqs(individuate(ExampleNum)),Grid,Objs),
     member(O,Objs)),OL1),
    %copy_term(P,PP), copy_term(PP,PPP),
  findall(O,(member(O,OL1),has_prop(P,O)),OL),
  %print_ss(Same,OL1,OL),
  % writeq(P),    
   forall(member(O,OL),has_prop(Same,O)))).
*/

shared_prop_U(TestID,_P,all,Same):- 
  forall(current_example_nums(TestID,ExampleNum), is_prop_in(TestID,ExampleNum,Same)).

shared_prop_U(TestID,_P,ShareLevel,Same):- number(ShareLevel), 
  findall(+,((current_example_nums(TestID,ExampleNum),
     \+ \+ is_prop_in(TestID,ExampleNum,Same))),L),
    length(L,N),%trace,
    N>=ShareLevel.

is_prop_in(TestID,ExampleNum,Same):-
   obj_group_io(TestID,ExampleNum,in,Objs),  
   \+ \+ (member(O,Objs),has_prop(Same,O)),!.
    
solve_obj_group(TestID,ExampleNum,IO,ROptions,Objs,OObjs):-
  my_maplist(solve_obj(TestID,ExampleNum,IO,ROptions),Objs,OObjs).

solve_obj(_TestID,_ExampleNum,_IO,_ROptions,Obj,Obj):- is_bg_object(Obj),!.
solve_obj(TestID,_ExampleNum,_IO_Start,_ROptions,Obj,OObj):- 
 IO=_,
 must_det_ll((findall(P,
   (is_accompany_changed_verified(TestID,IO,P,Same),
     flatten(Same,Rest), forall(member(R,Rest),has_prop(R,Obj))),PsL),
 list_to_set(PsL,Ps))),Ps\==[],
   wots(SS,writeln(Ps)),
   override_object_1(Ps,Obj,OObj),
   into_solid_grid([OObj],SG),
   dash_chars,
   print_ss(override_object(SS),[Obj],SG).
  
solve_obj(_TestID,_ExampleNum,_IO,_ROptions,Obj,Obj).

override_object_1([],IO,IO):-!.
override_object_1([H|T],I,OO):-  override_object_1(H,I,M),!, override_object_1(T,M,OO).
override_object_1(pen([cc(Red,N)]),Obj,OObj):- pen(Obj,[cc(Was,N)]), subst(Obj,Was,Red,OObj),!.
override_object_1(O,I,OO):- override_object(O,I,OO),!.

is_accompany_changed_verified(TestID,IO,P,Same):-
  is_accompany_changed_computed(TestID,IO,P,Same), Same\==[].

is_accompany_changed_computed(TestID,IO,P,Same):-
   is_accompany_changed_db(TestID,IO,P,Same) *->true ; prop_can(IO,P,Same). 
   
ensure_prop_change(IO,Prop):- 
  (var(Prop)->props_change(_TestID,IO,Prop);true).

prop_can0(IO,Prop,Can):-
  ensure_prop_change(IO,Prop),
  prop_can1(IO,Prop,Can1),
  prop_cant(IO,Prop,Cant),
  once(intersection(Can1,Cant,_,Can,_)),!.

prop_can(IO,Prop,CanO):-
  ensure_prop_change(IO,Prop),
  prop_can1(IO,Prop,Can1),
  prop_cant(IO,Prop,Cant),
  once((intersection(Can1,Cant,_,Can,_),
  prop_can_cant(IO,Prop,Can1,Cant,Can,CanO))).

negate_props(CanL,ReallyCant):- is_list(CanL),!,maplist(negate_props,CanL,ReallyCant).
negate_props(Can, \+ Can).

prop_can_cant(_IO,_Prop,_Can1,_Cant,Can,CanO):- Can\==[],!,Can=CanO.
prop_can_cant(IO,Prop,_Can1,_Cant1,Can,ReallyCant):- Can==[],
  findall( (\+ NonProp),(ensure_prop_change(IO,NonProp),other_val(Prop,NonProp)),CantL1),
  findall(E,(ensure_prop_change(IO,NonProp),other_val(Prop,NonProp),(prop_can0(IO,NonProp,C),negate_props(C,E),E\==[])),CantL2),  
  append([CantL1,CantL2],ReallyCant),!.
prop_can_cant(_Prop,_IO,Can1,_Cant1,Can,Can1):- Can==[],!.

prop_cant(IO,Prop,Set):-
  ensure_prop_change(IO,Prop),
  findall(Cant,
    ((enum_object(O),has_prop(giz(g(out)),O),has_prop(cc(bg,0),O),
      not_has_prop(Prop,O),indv_props_list(O,List),member(Cant,List))),Flat),
   list_to_set(Flat,Set).

enum_object_ext(O,IO):-
  ensure_test(TestID), current_example_nums(TestID,ExampleNum), obj_group_io_1(TestID,ExampleNum,IO,O).

obj_group_io_1(TestID,ExampleNum,IO,O):- obj_group_io_1a(TestID,ExampleNum,IO,O).
obj_group_io_1(TestID,ExampleNum,IO,O):- obj_group_io_1b(TestID,ExampleNum,IO,O), \+ obj_group_io_1a(TestID,ExampleNum,IO,O).

obj_group_io_1a(TestID,ExampleNum,IO,O):- obj_group_io(TestID,ExampleNum,IO,Objs),Objs\==[],member(O,Objs).
obj_group_io_1b(TestID,ExampleNum,IO,O):- arc_cache:map_pairs(TestID,ExampleNum,IO,Left,Right), member(O,[Left,Right]),has_prop(giz(g(IO)),O).
  


%in_out
%grp([in,out])-> out


prop_can1(IO,Prop,Can):-  
  ensure_prop_change(IO,Prop),
  findall(O,
    ((enum_object_ext(O,out),has_prop(giz(g(out)),O),has_prop(cc(bg,0),O),
      has_prop(Prop,O))),[I|L]),
  indv_props_list(I,List),
  findall(P,(member(P,List),P\=@=Prop,ok_notice(P),forall(member(E,L),has_prop(P,E))),Can).



%xlisting(propcounts+variance_had_count_set+(pen([cc(yellow,1)]);links_count(contains,4))-'$spft$').
/*
   propcounts( TestID,
           ExampleNum, IO,
           variance_had_count_set(2,0),
           P,
           PL,
           [ 1-(2-links_count(contains,4)),
             1-(1-links_count(contains,3))]),
*/
contains_same([],_):- !.
contains_same([E|L],P):- sub_var(E,P),!,contains_same(L,P).

find_peers_with_same(TestID,IO,P,Same,NewSame):- select(S,Same,Next),S=@=P,!,find_peers_with_same(TestID,IO,P,Next,NewSame).
find_peers_with_same(TestID,IO,P,Same,NewSame):- 
   sub_term(Color,P),is_real_color(Color), sub_term(N,P),number(N),
   my_partition(contains_same([Color]),Same,SameW,SameWO),SameW\==[], SameWO\==[],!,
   find_peers_with_same(TestID,IO,P,SameWO,NewSame).
find_peers_with_same(_,_,Same,Same):-!.
   
   

   

merge_xtra_props_ac1([ac1(PO)|AC3],Same):- !, merge_xtra_props_ac1_3(PO,AC3,Same), Same\==[].
merge_xtra_props_ac1_3(PO,[ac1(PO2)|MORE],OUT):-
  intersection(PO,PO2,IPO),
  merge_xtra_props_ac1_3(IPO,MORE,OUT).
merge_xtra_props_ac1_3(PO,[],PO).

merge_xtra_props_ac2([ac2(_,Same)],Same):-!.
merge_xtra_props_ac2(AC2,Same):-
 select(ac2(ExampleNum,PO1),AC2,AC3),
 select(ac2(ExampleNum,PO2),AC3,AC4),
 intersection(PO1,PO2,Some),Some\==[],!,
 merge_xtra_props_ac2([ac2(ExampleNum,Some)|AC4],Same).
merge_xtra_props_ac2(AC2,Same):-
 select(ac2(ExampleNum,PO1),AC2,AC3),
 select(ac2(ExampleNum2,PO2),AC3,AC4),
 ExampleNum \== ExampleNum2,
 intersection(PO1,PO2,Some),Some\==[],!,
 merge_xtra_props_ac2([ac2(ExampleNum,Some)|AC4],Same).

merge_xtra_props_ac2([ac2(ExampleNum,PO1)|AC3],[ac2(ExampleNum,PO1)|Same]):-
  merge_xtra_props_ac2(AC3,Same),!.
merge_xtra_props_ac2(Same,Same):-!.


changing_props(TestID,X1,X2):- 
 ensure_test(TestID),
 findall(X1-InOut,props_change(TestID,InOut,X1),X1L),
 variant_list_to_set(X1L,X1S),
 member(X1-IO,X1S),
 member(X2-IO,X1S),
% X1@>X2,
 other_val(X1,X2). 

other_val(X1,X2):- X1\=@=X2, same_prop_names(X1,X2),!.
same_prop_names(X1,X2):- 
  compound(X1),compound(X2), same_functor(X1,X2),!,
  make_unifiable_u(X1,U1), make_unifiable_u(X2,U2),  U1 =@= U2.

make_unifiable_u(Atom,U):- atomic(Atom),!,freeze(U,atomic(U)).
make_unifiable_u(link(sees(L),A),link(sees(U),B)):- !, maplist(make_unifiable_u,[A|L],[B|U]).
make_unifiable_u(X1,U1):- make_unifiable_cc(X1,U1),!.

accompany_change2(TestID,ExampleNum,[X1=P1O,X2=P2O,common=Intersect]):-
 changing_props(TestID,X1,X2),
 accompany_change(TestID,ExampleNum,X1,Props1,_NotProps1),
 accompany_change(TestID,ExampleNum,X2,Props2,_NotProps2),
 once((
   intersection(Props1,Props2,Intersect,P1,P2),
   include(has_other_val(Props2),P1,P1O),
   include(has_other_val(Props1),P2,P2O))).

has_other_val(Props1,X2):- member(X1,Props1),other_val(X1,X2),!.


accompany_change(TestID,ExampleNum,X,Props,NotProps):-
  var(TestID),!,ensure_test(TestID),
  accompany_change(TestID,ExampleNum,X,Props,NotProps).
accompany_change(TestID,ExampleNum,X,Props,NotProps):-
  var(ExampleNum),!,current_example_nums(TestID,ExampleNum),
  accompany_change(TestID,ExampleNum,X,Props,NotProps).
accompany_change(TestID,ExampleNum,X,Props,NotProps):-
  var(X),!,props_change(TestID,_,X),
  accompany_change(TestID,ExampleNum,X,Props,NotProps).

accompany_change(TestID,ExampleNum,X,Props,NotProps):-     
   once((counts_change(TestID,ExampleNum,_I_or_O,X,N1,N2), N1<N2)),!,
   %no_repeats_var(Out),
  once(( obj_group_gg(TestID,ExampleNum,_In,Out),
         once((my_partition(has_prop(X),Out,HasPropsO,NotHasPropsO),
         common_props(HasPropsO,Common),  common_props(NotHasPropsO,NotCommon),
         intersection(Common,NotCommon,_,Props1,NotProps))),
         include(\=@=(X),Props1,Props),
         Props\==[])).

accompany_change(TestID,ExampleNum,X,Props,NotProps):- fail,
   counts_change(TestID,ExampleNum,_I_or_O,X,N1,N2), N1>N2,
  once((
   %obj_group_io(TestID,ExampleNum,in,In), my_partition(has_prop(X),In,HasPropsI,NotHasPropsI),
  %obj_group_io(TestID,ExampleNum,out,Out),
  other_objs_than_group(TestID,ExampleNum,out,Out),
  my_partition(not_has_prop(X),Out,HasPropsO,NotHasPropsO),
  %my_partition(not_has_prop(X),Others,HasPropsOthers,NotHasPropsOthers),
  common_props(HasPropsO,Common),
  common_props(NotHasPropsO,NotCommon),
  intersection(Common,NotCommon,_,Props,NotProps),Props\==[])).


common_props([O|Objs],Props):-
   indv_props_list(O,List),
   findall(P,(member(P,List),\+ dont_notice(P),forall(member(E,Objs),has_prop(P,E))),Props).

current_example_nums(TestID,ExampleNum):- 
  (var(TestID)->get_current_test(TestID);true),
  ignore((ExampleNum=trn+_)), kaggle_arc(TestID,ExampleNum,_,_). 



save_how_io(HowIn,HowOut):- 
  get_current_test(TestID),save_how_io(TestID,HowIn,HowOut).
save_how_io(TestID,HowIn,HowOut):- 
  assert_test_property(TestID,common,indiv_how(in),HowIn),
  assert_test_property(TestID,common,indiv_how(out),HowOut),!.



obj_group_gg(TestID,ExampleNum,InC,OutC):-
   current_example_nums(TestID,ExampleNum),
   no_repeats_var(OutC),set_example_num(ExampleNum),
   obj_group5(TestID,ExampleNum,in,HowIn,InC), InC\==[],  length(InC,L),

   (((obj_group5(TestID,ExampleNum,out,HowOut,OOut),length(OOut,L),save_how_io(TestID,HowIn,HowOut)))
     ;obj_group5(TestID,ExampleNum,out,_,OOut)),   
   OutC = OOut.

other_objs_than_group(TestID,ExampleNum,IO,Others):-
  findall(O,(current_example_nums(TestID,OExampleNum),
    ExampleNum\==OExampleNum,
    obj_group_io(TestID,OExampleNum,IO,Objs),
    member(O,Objs)),Others).

all_io_objs(TestID,IO,Others):-
  findall(O,(current_example_nums(TestID,ExampleNum), 
   obj_group_io(TestID,ExampleNum,IO,Objs), member(O,Objs)),Others).

with_individuated_cache(TF,Goal):- locally(nb_setval(use_individuated_cache,TF),Goal).

obj_group_io(TestID,ExampleNum,IO,Objs):-
 arc_test_property(TestID,common,indiv_how(IO),How),!,
 current_example_nums(TestID,ExampleNum), 
 with_individuated_cache(true,
   once(obj_group5(TestID,ExampleNum,IO,How,Objs))).

obj_group_io(TestID,ExampleNum,IO,Objs):- 
 current_example_nums(TestID,ExampleNum),
 with_individuated_cache(true,
   once(obj_group5(TestID,ExampleNum,IO,_,Objs))).

obj_group5(TestID,ExampleNum,IO,ROptions,Objs):- var(TestID),
  ensure_test(TestID),!,obj_group5(TestID,ExampleNum,IO,ROptions,Objs).  
obj_group5(TestID,ExampleNum,IO,ROptions,Objs):- var(ROptions),
 arc_test_property(TestID,common,indiv_how(IO),ROptions),!,
 obj_group5(TestID,ExampleNum,IO,ROptions,Objs).
obj_group5(TestID,ExampleNum,IO,ROptions,Objs):-
 kaggle_arc_io(TestID,ExampleNum,IO,Grid),
  set_example_num(ExampleNum),
  ((fail, arc_cache:individuated_cache(TestID,TID,GOID,ROptions,Objs), Objs\==[],
  once((testid_name_num_io_0(TID,_,Example,Num,IO),
        testid_name_num_io_0(GOID,_,Example,Num,IO))))*-> true ; grid_to_objs(Grid,ROptions,Objs)).



% =============================================================
show_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore((show_object_dependancy(TestID,ExampleNum)))).

show_object_dependancy(TestID,ExampleNum):-  
  forall(obj_group_gg(TestID,ExampleNum,LHSObjs,RHSObjs),
    show_object_dependancy(TestID>ExampleNum,LHSObjs,RHSObjs)).

show_object_dependancy(TestIDExampleNum,LHSObjs,RHSObjs):-
  maybe_remove_bg(LHSObjs,LHSObjs1),
  maybe_remove_bg(RHSObjs,RHSObjs1),
  calc_object_dependancy(LHSObjs1,RHSObjs1,Groups),
  pp_obj_to_grids(show_object_dependancy(TestIDExampleNum)==>Groups).
  %maplist(assert_map_groups(TestID,ExampleNum,in),Groups),!.


% =============================================================
print_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore((print_object_dependancy(TestID,ExampleNum)))).
print_object_dependancy(TestID,ExampleNum):-  
  forall(arc_cache:map_group(TestID,ExampleNum,IO,LeftRight),
    pp_obj_to_grids(map_group(TestID,ExampleNum,IO,LeftRight))),
  forall(arc_cache:map_pairs(TestID,ExampleNum,IO,Left,Right),
    pp_obj_to_grids(map_pairs(TestID,ExampleNum,IO,Left,Right))).

pp_obj_to_grids(TermWithObjs):-
  into_solid_grid_strings(TermWithObjs,TermWithGrids),
  pp(TermWithGrids),!.

into_solid_grid_strings(TermWithObjs,TermWithGrids):-
  sub_term(Obj,TermWithObjs),Obj\=@=TermWithObjs,is_mapping(Obj),
  into_solid_grid_strings(Obj,Grid),!,
  subst001(TermWithObjs,Obj,Grid,TermWithObjsAndGrids),
  into_solid_grid_strings(TermWithObjsAndGrids,TermWithGrids).
into_solid_grid_strings(TermWithObjs,TermWithGrids):-
  sub_term(Obj,TermWithObjs),Obj\=@=TermWithObjs,is_grid(Obj),
  into_solid_grid_str(Obj,Grid),!,
  subst001(TermWithObjs,Obj,Grid,TermWithObjsAndGrids),
  into_solid_grid_strings(TermWithObjsAndGrids,TermWithGrids).
into_solid_grid_strings(TermWithObjs,TermWithGrids):-
  sub_term(Obj,TermWithObjs),Obj\=@=TermWithObjs,is_object(Obj),
  into_solid_grid_str(Obj,Grid),!,
  subst001(TermWithObjs,Obj,Grid,TermWithObjsAndGrids),
  into_solid_grid_strings(TermWithObjsAndGrids,TermWithGrids).
into_solid_grid_strings(TermWithGrids,TermWithGrids).
%  \+ arc_cache:map_group(TestID,ExampleNum,IO,LeftRight),

into_solid_grid_str(Obj,GridStr):- into_solid_grid(Obj,Grid),!,wots(GridStr,print_grid(Grid)).

% =============================================================
clear_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore((clear_object_dependancy(TestID,ExampleNum)))).
clear_object_dependancy(TestID,ExampleNum):-  
  forall(arc_cache:map_group(TestID,ExampleNum,IO,LeftRight),
    retract(arc_cache:map_group(TestID,ExampleNum,IO,LeftRight))),
  forall(arc_cache:map_pairs(TestID,ExampleNum,IO,Left,Right),
    retract(arc_cache:map_pairs(TestID,ExampleNum,IO,Left,Right))).


% =============================================================
calc_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore((calc_object_dependancy(TestID,ExampleNum)))).

calc_object_dependancy(TestID,ExampleNum):-  
  forall(obj_group_gg(TestID,ExampleNum,LHSObjs,RHSObjs),
    calc_object_dependancy(TestID,ExampleNum,LHSObjs,RHSObjs)).

calc_object_dependancy(TestID,ExampleNum,LHSObjs,RHSObjs):-
  maybe_remove_bg(LHSObjs,LHSObjs1),
  maybe_remove_bg(RHSObjs,RHSObjs1),
  calc_object_dependancy(LHSObjs1,RHSObjs1,Groups),
  maplist(assert_map_groups(TestID,ExampleNum,in),Groups),!.


assert_map_groups(TestID,ExampleNum,IO,LeftRight):-!,
  into_lst(LeftRight,LeftRightList),
  if_t(LeftRightList\=[_,_], print_ss(map_group(TestID,ExampleNum,IO)=LeftRightList)),
  assert_become_new(arc_cache:map_group(TestID,ExampleNum,IO,LeftRight)),
  assert_map_pair_list(TestID,ExampleNum,IO,LeftRight).

assert_map_pair_list(_TestID,_ExampleNum,_IO,[]):-!.
assert_map_pair_list(TestID,ExampleNum,IO,[Left,Right]):- is_object(Left), is_object(Right), !, assert_map_pairs(TestID,ExampleNum,IO,Left,Right),!.
assert_map_pair_list(TestID,ExampleNum,IO,[Left|Right]):- into_lst(Left,L1),[Left]\=@=L1,append(L1,Right,LR),!,assert_map_pair_list(TestID,ExampleNum,IO,LR).
assert_map_pair_list(TestID,ExampleNum,IO,[Left,Right,M|More]):- 
  assert_map_pair_list(TestID,ExampleNum,IO,[Left,Right]),!,
  assert_map_pair_list(TestID,ExampleNum,IO,[Right,M|More]).

assert_map_pairs(TestID,ExampleNum,IO,Left,Right):-
  %print_ss(map_pair(TestID,ExampleNum,IO),Left,Right),
  assert_become_new(arc_cache:map_pairs(TestID,ExampleNum,IO,Left,Right)),!.


:- dynamic(arc_cache:map_pairs/5).
:- dynamic(arc_cache:map_group/4).

   
  
% sort_by_generation(Grps,SortedByGen):-predsort(sort_on(by_generation),Grps,SortedByGen).
sort_by_generation(Grps,Grps).

maybe_remove_bg(RHSObjs,RHSObjs1):- my_partition(is_fg_object,RHSObjs,RHSObjs1,Rest),RHSObjs1\==[],Rest\==[],!.
maybe_remove_bg(RHSObjs,RHSObjs).

is_mapping_list([O|GrpL]):- is_mapping(O),is_list(GrpL),maplist(is_mapping,GrpL).
is_mapping(Grp):- is_functor(grp,Grp).

calc_object_dependancy(Nil,Mappings,RestLR):- maplist(is_bg_object,Nil),
   is_mapping_list(Mappings),!, Mappings=RestLR.
calc_object_dependancy(Mappings,Nil,RestLR):- maplist(is_bg_object,Nil),
   is_mapping_list(Mappings),!, Mappings=RestLR.
calc_object_dependancy(Nil,Objs,RestLR):- maplist(is_bg_object,Nil),
   split_sorted(Objs,SplitLHS,SplitRHS),
   SplitLHS\==[],SplitRHS\==[],!,
   calc_object_dependancy(SplitLHS,SplitRHS,RestLR).

calc_object_dependancy(LHSObjs,RHSObjs,RestLR):- 
  length(LHSObjs,Left),length(RHSObjs,Right),Left>Right,calc_object_dependancy(RHSObjs,LHSObjs,RestLR).
calc_object_dependancy(LHSObjs,RHSObjs,PairsLHSgain):- 
   map_left_to_right(LHSObjs,RHSObjs,RestLR,Unused),
   calc_object_dependancy(RestLR,Unused,PairsLHSgain).

map_left_to_right(LHSObjs,RHSObjs,[Pairs|RestLR],Unused):-
  select_pair(LHSObjs,RHSObjs,Left,Right,LHSRest,RHSRest),
  make_pairs(Left,Right,Pairs),
  map_left_to_right(LHSRest,RHSRest,RestLR,Unused).
map_left_to_right([],RHSRest,[],RHSRest).


select_pair(LHSObjs,RHSObjs,Left,Right,LHSRest,RHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  find_prox_mappings(Left,map_left_to_right,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  find_prox_mappings(Right,map_right_to_left,LHSObjs,[Left|_]).
select_pair(LHSObjs,RHSObjs,Left,Right,LHSRest,RHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  find_prox_mappings(Left,map_left_to_right,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),!.

remove_object(RHSObjs,Left,RHSObjsMI):- select(Left,RHSObjs,RHSObjsMI),!.
remove_object(RHSObjs,_,RHSObjs).

into_lst(ObjsL,[ObjsL]):-var(ObjsL),!.
into_lst(grp(ObjsL),Lst):-!,into_lst(ObjsL,Lst).
into_lst(ObjsL,Lst):- is_list(ObjsL),!,maplist(into_lst,ObjsL,LstL),append(LstL,Lst).
into_lst(ObjsL,[ObjsL]).

prime_factor(N, D) :-
    find_prime_factor(N, 2, D).

find_prime_factor(N, D, D) :- 0 is N mod D.
find_prime_factor(N, D, R) :- D < N,
    (0 is N mod D
    -> (N1 is N/D, find_prime_factor(N1, D, R))
    ;  (D1 is D + 1, find_prime_factor(N, D1, R))
    ).

split_sorted(Objs,SplitLHS,SplitRHS):- 
  my_partition(is_bg_object,Objs,SplitLHS,SplitRHS), SplitLHS\==[], SplitRHS\==[].

split_sorted(Objs,SplitLHS,SplitRHS):-
 length(Objs,Len),
 prime_factor(Len,Prime),
 split_sorted(Objs,Len,Prime,SplitLHS,SplitRHS).

split_sorted(Objs,_Len,Prime,SplitLHS,SplitRHS):- 
 variance_counts(Objs,PropObjsounts),
 findall(E,(member(E,PropObjsounts),sub_var(Prime,E)),EL),
 member(E,EL),into_prop(E,P),
 my_partition(has_prop(P),Objs,SplitLHS,SplitRHS),!.

split_sorted(Objs, Len,Prime,SplitLHS,SplitRHS):- 
 Half is Len div Prime,
 count_each_value(Objs,PropObjsounts),
 findall(E,(member(E,PropObjsounts),sub_var(Prime,Half)),EL),
 member(E,EL),into_prop(E,P),
 my_partition(has_prop(P),Objs,SplitLHS,SplitRHS),!.

into_prop(CC,P):- sub_term(E,CC),compound(E),is_prop1(E),!,E=P.

make_pairs(LHS,RHS,grp([LHS,RHS])).




saved_group(Why,IndvS):-
  is_why_grouped(_TestID,_Count,Why,IndvS).

is_why_grouped(TestID,Count,Why,IndvSO):-
  is_why_grouped_g(TestID,Count,Why,IndvSG),
  once(maplist(must_oid_to_object,IndvSG,IndvS)),
  IndvSO=IndvS.

must_oid_to_object(ID,O):- must_det_ll(oid_to_obj(ID,O)).

save_grouped(Why,G):-
  into_group(G,GS),
  get_current_test(TestID),
  length(GS,Len),
  mapgroup(register_obj,GS),
  maplist(obj_to_oid_u,GS,GGG),
  %maplist(obj_to_oid,GS,OIDs),
  my_asserta_if_new(is_why_grouped_g(TestID,Len,Why,GGG)).

obj_to_oid_u(Obj,OID):- obj_to_oid(Obj,OID).

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
  my_append(Group1,Group2,GroupJ), sort_safe(GroupJ,Group),
  How = [How1,How2])) 
    *-> true ; is_why_grouped(TestID,_,How,Group).

select_group0(TestID,Group,obj_cache):- findall(O,obj_cache(TestID,O,_),GroupJ), GroupJ\==[], sort_safe(GroupJ,Group).






















too_unique(P):- compound(P),!,compound_name_arity(P,F,_),!,too_unique(F).
%too_unique(obj_to_oid).
too_unique(globalpoints).
%too_unique(o).
too_unique(link).
too_unique(obj_to_oid).
too_unique(/*b*/iz).
%good_overlap(colorlesspoints).

good_overlap(P):- compound(P),!,compound_name_arity(P,F,_),!,good_overlap(F).
good_overlap(localpoints).
good_overlap(rot2D).

too_non_unique(P):- compound(P),!,compound_name_arity(P,F,_),!,too_non_unique(F).
too_non_unique(grid_size).
too_non_unique(grid_sz).
%too_non_unique(/*b*/iz).
too_non_unique(grid).
too_non_unique(changes).




compare_objects([],[]):-!.
compare_objects(Objs,Interesting):- 
  maplist(indv_props_for_noteablity,Objs,ObjProps),
  flatten(ObjProps,FlatProps),
  maplist(functorize_props,FlatProps,Functors),
  sort_safe(Functors,SortedFunctors),
  gather_props(SortedFunctors,FlatProps,ListOfLists),
  maplist(compare_values,ListOfLists,Diffs),
  include(\=([]),Diffs,Interesting).
  
functorize_props(iz(Prop),FA):- !, functorize_props(Prop,FA).
functorize_props(Prop,F/A):- functor(Prop,F,A).
gather_props([F/A|SortedFunctors],FlatProps,[(F-Candidates)|ListOfLists]):- 
  functor(Match,F,A), findall(Match,(member(Match,FlatProps);member(iz(Match),FlatProps)),Candidates),
  gather_props(SortedFunctors,FlatProps,ListOfLists).
gather_props([],_,[]).


compare_values(F-X,Notable):- predsort_using_only(number_varz,X,S),length(X,N),length(S,NS),
  is_notable(F-NS/N,Notable).

:- dynamic(repress_non_notables/0).
is_changeable_param(repress_non_notables/0).
repress_non_notables.

:- dynamic(never_noteable/1).
is_changeable_param(never_noteable/1).
never_noteable(colors_cc).
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




%too_non_unique(mass).

length_criteria(List,P):- compound(P), P=..[F,n,L],C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), P=..[F,L], C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), length(List,I), !, call(call,P,I).
length_criteria(List,N):- length(List,N).

tesT_compare_objects:- compare_objects([
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),
      vis2D(1,1),rot2D(sameR),loc2D(4,9),changes([]),iz(type(dots)),iz(type(dot)),iz(filltype(solid)),iz(jagged(true)),center2G(4,9),% obj_to_oid(t(af902bf9)>(tst+0)*in,37),globalpoints([yellow-point_04_09]),
      grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(4,6),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(4,6),obj_to_oid(t(af902bf9)>(tst+0)*in,39),globalpoints([yellow-point_04_06]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(1,6),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(1,6),obj_to_oid(t(af902bf9)>(tst+0)*in,40),globalpoints([yellow-point_01_06]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(10,5),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(10,5),obj_to_oid(t(af902bf9)>(tst+0)*in,41),globalpoints([yellow-point_10_05]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(6,5),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(6,5),obj_to_oid(t(af902bf9)>(tst+0)*in,42),globalpoints([yellow-point_06_05]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(10,1),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(10,1),obj_to_oid(t(af902bf9)>(tst+0)*in,43),globalpoints([yellow-point_10_01]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(6,1),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(6,1),obj_to_oid(t(af902bf9)>(tst+0)*in,44),globalpoints([yellow-point_06_01]),grid_size(10,10),iz(important)])],
    OUTPUT),
  print(OUTPUT).


get_peers(Obj,Peers):- 
  get_current_test(TestID),select_group(TestID,Group,_How), select(Obj,Group,Peers).

peerless_props(O1,Peers,PeerlessProps):-
 must_det_ll(( indv_props_list(O1,Props),
               (var(Peers)->get_peers(O1,Peers);true),
               (select(O1,Peers,PeersU)->true;PeersU=Peers),
  include(is_peerless_prop(PeersU),Props,PeerlessProps))).

not_peerless_props(O1,Peers,PeerlessProps):-
 must_det_ll(( indv_props_list(O1,Props),
               (var(Peers)->get_peers(O1,Peers);true),
               (select(O1,Peers,PeersU)->true;PeersU=Peers),
  include(not_peerless_prop(PeersU),Props,PeerlessProps))).

is_peerless_prop(Peers,Prop):- \+ sub_var(Prop,Peers).
not_peerless_prop(Peers,Prop):- sub_var(Prop,Peers).

