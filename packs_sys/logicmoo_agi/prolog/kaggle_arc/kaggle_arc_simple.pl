/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- include(kaggle_arc_header).

:- ensure_loaded(kaggle_arc_grid_size).


solve_easy:- get_current_test(Name),solve_easy(Name).

solve_easy(Name):- 
  fix_test_name(Name,TestID,ExampleNum),
  ignore(some_current_example_num(ExampleNum)),
  forall(kaggle_arc(TestID,ExampleNum,In,Out),try_easy_io(TestID>ExampleNum,In,Out)),
  ((ExampleNum\=tst+_)-> 
    forall(kaggle_arc(TestID,tst+N,In,Out),try_easy_io(TestID>tst+N,In,Out))).

try_something_easy(rot180).
try_something_easy(=).
try_something_easy(run_dsl(E)):- fail, test_info(_,human(E)).

maybe_try_something_easy(I,M,P2):-  try_something_easy(P2), call(P2,I,M).
maybe_try_something_easy(I,M,Did):- fail_over_time(4,try_something(Did,I,M),fail),!.

try_easy_io(Name,I,O):-
 ignore((
  Template = try_something(W,Did,I,M,SS),
  findall(Template,
    (wots(SS,weto(maybe_try_something_easy(I,M,Did))),count_changes(M,O,1,W),(W==1->!;true)),
     List),
  sort(List,[Template|_]),
  %ignore((call(P2,I,II),call(P2,O,OO),
  %reduce_grid(GridIn+GridOut,IOps,II+OO),!,
  (W==1 -> Color=green; Color = yellow),
  must_det_ll(print_side_by_side(Color,M,easyResult(Name,Did),_,O,easyExpected(Name=W))))), !. 



grid_w_obj(Grid,Why,Objs):-
  (var(Grid)->arc_grid(Grid);true),
  ROptions = complete,
  individuate(ROptions,Grid,_IndvS),  
  why_grouped(Why,GS),
  member(Objs,GS).

grid_grouped(Grid,Why,Objs):- 
 (var(Grid)->arc_grid(Grid);true),
  ROptions = complete,
  individuate(ROptions,Grid,IndvS),
  regroups(IndvS,Why,Objs).

group_same_props(IndvS0,Ps):-  guard_invs(IndvS0,IndvS),
  group_props(IndvS,PropsSet),
  findall(Prop,(member(Prop,PropsSet),maplist(has_prop(Prop),IndvS)),Ps).


group_same_props(IndvS0,P1N,GsOO):-  guard_invs(IndvS0,IndvS),
   findall(Have-Prop,(group_same_prop(IndvS,Prop,Have,HN),HN\==[],length(Have,HH),call(P1N,HH)),Gs),
   sort(Gs,GsO),combine_keys(GsO,GsOO).

combine_keys([],[]):-!.
combine_keys([K1-V1|GsO],[K1-Props|GsOO]):- my_partition(=(K1-_),[K1-V1|GsO],G1,G2),
 maplist(arg(2),G1,Props),combine_keys(G2,GsOO).

group_same_prop(IndvS,Prop,Have,HaveNots):-
  group_props(IndvS,PropsSet),
  member(Prop,PropsSet),
  my_partition(has_prop(Prop),IndvS,Have,HaveNots).

member_prop(Prop,Obj,Actual):-
  member_prop(Prop,Obj,_Template,Actual).

member_prop(Prop,Obj,Template,Actual):-
  indv_props(Obj,List),generalize(Prop,Template),nonvar(Template),copy_term(Template,Actual),member(Actual,List).

group_at_least_1_diff_props(IndvS0,Prop,Obj,HaveNots,Actuals):- guard_invs(IndvS0,IndvS),
  group_props(IndvS,PropsSet),
  member(Prop,PropsSet),
  once((my_partition(has_prop(Prop),IndvS,Have,HaveNots),
  Have=[Obj])),
  maplist(member_prop(Prop),HaveNots,Actuals).

group_all_diff_props(IndvS0,Prop,Obj,HaveNots,OtherProp):- guard_invs(IndvS0,IndvS),
  group_at_least_1_diff_props(IndvS,Prop,Obj,HaveNots,Actuals),
  list_to_set(Actuals,AS),AS = [OtherProp].

guard_invs(IndvS0,IndvS):- is_group(IndvS0),!,IndvS0=IndvS.
guard_invs(IndvS0,IndvS):- var(IndvS0), !, no_repeats(IndvS,gen_group(IndvS)),IndvS0=IndvS.
guard_invs(IndvS0,IndvS):- into_group(IndvS0,IndvS).

gen_group(IndvS):-
arc_grid(_,Grid), \+ \+ individuate(complete,Grid,_),
  why_grouped(_Why,IndvS),IndvS\==[].

group_diff_props(IndvS0,Ps):- guard_invs(IndvS0,IndvS),
  group_props(IndvS,PropsSet),
  findall(Prop,(member(Prop,PropsSet),\+ maplist(has_prop(Prop),IndvS)),Ps).

group_props(IndvS,PropsSet):- 
  findall(Props,(member(Obj,IndvS),indv_props(Obj,Props)),PropsL),
  append(PropsL,PropsF),list_to_set(PropsF,PropsSet).

group_uprops(IndvS0,UPropsSet):- guard_invs(IndvS0,IndvS),
  group_props(IndvS,PropsSet),simplify_props(IndvS,PropsSet,L),list_to_set(L,UPropsSet).

not_has_prop(Prop,Obj):- \+ has_prop(Prop,Obj).

relax_prop(iz(S1),iz(R1)):- !, relax_prop(S1,R1).
relax_prop(S1,R1):- compound(S1),ground(S1),!,relax_prop1(S1,R1),\+ ground(R1),!.
relax_prop(S1,R1):- R1 = S1.

relax_prop1(S1,R1):- relax_prop2(S1,R1)*->true;generalize(S1,R1).

relax_prop2(o(X,Y,_),o(X,Y,_)).
relax_prop2(loc2D(X,_),loc2D(X,_)).
relax_prop2(loc2D(_,Y),loc2D(_,Y)).


simplify_props(IndvS,[R1|Props],SPropsF):- never_group_on(R1), !,simplify_props(IndvS,Props,SPropsF).
simplify_props(IndvS,[R1|Props],SPropsF):- maplist(haz_prop(R1),IndvS), !,simplify_props(IndvS,Props,SPropsF).
simplify_props(IndvS,Props,[R1|SPropsF]):- 
  select(S1,Props,More),\+ never_group_on(S1),
  select(S2,More,More2),\+ never_group_on(S2),
  relax_prop(S1,R1),relax_prop(S2,R2),R1=@=R2,
  %ground(S1),ground(S2),
  \+ maplist(haz_prop(R1),IndvS),
  my_partition(=(R2),More2,_Remove,Keep),!,
  simplify_props(IndvS,Keep,SPropsF).
simplify_props(_,A,A).

pregroup1(iz(media(shaped))).
pregroup1(iz(media(image))).
pregroup1(iz(chromatic(N,BGN))):- between(1,10,N),between(0,2,BGN).
pregroup1(o(sf(_),_,How)):- dif(How,i_repair_patterns).


never_uprop(localpoints(_)).
never_group_on(o(I,_,_)):- I == i_repair_patterns.
never_group_on(P):- never_uprop(P).

regroups(IndvS,[Why1,Why2],[Obj|Grp]):-
  group_uprops(IndvS,PropsSet),
  propset_indivs(PropsSet,OtherProps,IndvS,Why1,Grp1),
  length(Grp1,N1),N1>2,
  propset_indivs(OtherProps,_,Grp1,Why2,Grp2),
  length(Grp2,N2),N2=1,Grp2 = [Obj],
  select(Obj,Grp1,Grp).

regroups(IndvS,[pairs,Why1],Grp1):-
  group_props(IndvS,PropsSet),
  propset_indivs(PropsSet,_OtherProps,IndvS,Why1,Grp1),
  length(Grp1,N1),N1=2.

propset_indivs(PropsSet,OtherProps,IndvS,Why,Grp):- 
  select(Why,PropsSet,OtherProps),
  include(haz_prop(Why),IndvS,Grp).

haz_prop(P,O):- has_prop(P,O).

:- export(grid_part/2).
grid_part(Grid,Info):- var(Grid), get_current_test(TestID), some_current_example_num(ExampleNum),!,
  kaggle_arc_io(TestID,ExampleNum,_,Grid),
  grid_part(Grid,Info).

%grid_part(Grid,InfoR):- nth1(X,Grid,Info),VInfo=..[v|Info],InfoR=..[row,X,VInfo].
%grid_part(Grid,InfoR):- rot90(Grid,Grid90),nth1(X,Grid90,Info),VInfo=..[v|Info],InfoR=..[col,X,VInfo].
%grid_part(Grid,NObjs):- wno(individuate(complete,Grid,Objs)), maplist_n(1,number_obj,Objs,NObjs).

%cheapest_desc(Grid

number_obj(N,obj(List),obj([ord(N)|List])).
/*
  Obj = obj(List),
  loc2D(Obj,X,Y),obj_to_oid(Obj,_,MyID),
 % atomic_list_concat([obj,X,Y],'_',Key),
  localpoints_include_bg(Obj,LocalPoints),
  points_to_grid(X,Y,LocalPoints,Grid),mapgrid(sometimes_assume(=,bg),Grid),
  select(colorless_points(Shape),List,Rest2),mapgrid(sometimes_assume(=,bg),Shape),
  Rest3 = Rest2,
  must_det_ll((remove_too_verbose(MyID,Rest3,TV00))),flatten([TV00],TV0),
  must_det_ll((include(not_too_verbose,TV0,TV1),maplist(fix_iz,TV1,TV))),!,
  member(MrT,[oform(Shape),ogrid(Grid)|TV])*/

%grid_part(Grid,P):- globalpoints(Grid,Points),member(P,Points).

%object_info(obj(List)

%grid(Type,ConstructorData,[rot270]),CacheOfCalls).

%is_graid(TestID>ExampleNum*IO,TestID,ExampleNum,IO).

is_graid((TestID > (ExampleNum*IO)),G):- kaggle_arc_io(TestID,ExampleNum,IO,G).

:- export(is_graid/2).
%grid_aid(ID,TestID>ExampleNum*IO):- is_graid(Grid,TestID,ExampleNum,IO),format(ID,).

point(Grid,Color,X,Y):- is_graid(Grid,G),nth1(Y,G,R),nth1(X,R,Color).

%g_size(Grid,X,Y):- is_graid(Grid,G),grid_size(G,X,Y).

grid_points(Grid,Points):-  is_graid(Grid,G),globalpoints(G,Points).
grid_point(Grid,point(X,Y,Color)):- point(Grid,Color,X,Y).

grid_object(Grid,amass(1),Point):- grid_point(Grid,Point).
grid_object(Grid,amass(2),point2(Dir,[(HV1)-(HV2)],Color)):- 
  globalpoints(Grid,Ps),select(Color-HV1,Ps,Pss),select(Color-HV2,Pss,_), 
  is_adjacent_point(HV1,Dir,HV2).

grid_object(Grid,amass(N),object(Points,Color)):- 
  is_graid(Grid,G),enum_colors(Color), \+ \+ grid_point(Grid,point(_,_,Color)),
  length(Points,N),Points = [HV1,HV2,HV3|AdjRest],
  globalpoints(G,Ps),select(Color-HV1,Ps,Pss),select(Color-HV2,Pss,Psss),select(Color-HV3,Psss,Rest), 
  HV1 @< HV2,
  is_adjacent_point(HV1,_Dir1,HV2),is_adjacent_point(HV2,_Dir2,HV3),
  ColorHV4 = Color-HV4,
  findall(HV4,(member(ColorHV4,Rest),is_adjacent_point(HV1,_,HV5),is_adjacent_point(HV5,_,HV4)),AdjRest).

:- include(kaggle_arc_footer).

