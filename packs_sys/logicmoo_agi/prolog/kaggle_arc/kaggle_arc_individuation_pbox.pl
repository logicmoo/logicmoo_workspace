/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

is_real_color_or_wfg(SX):- (SX == fg; SX == wbg),!.
is_real_color_or_wfg(SX):- is_real_color(SX),!.

:- dynamic(gclip_cache/10).
make_gclip_cache_0:-
 forall((
   make_grid_cache(GH,GV,G),
   between(1,GH,OH),between(1,GV,OV),
   MH is GH-OH, MV is GV-OV,MH>=1,MV>=1,
   between(1,MH,SGH),between(1,MV,SGV), 
   EH is OH+SGH-1, EV is OV+SGV-1),
   ignore(once((clip(OH,OV,EH,EV,G,SG),writeq(gclip_cache(GH,GV,SGH,SGV,EH,EV,OH,OV,G,SG)),writeln('.'))))).

make_gclip_cache_file:- exists_file('make_gclip_cache.pl'),!.
make_gclip_cache_file:- 
 tell('make_gclip_cache.pl'),

 format(':- style_check(-singleton). ~n'),
 make_gclip_cache_0,
 told.

make_gclip_cache:- 
 my_time((
 make_gclip_cache_file,
 consult('make_gclip_cache.pl'))),
 functor(G,gclip_cache,10),
 predicate_property(G,number_of_clauses(NC)),
 debug_m(indiv(pbox),gclip_cache=NC).

%=====================================================================
is_fti_step(maybe_pbox_vm).
%=====================================================================
%:- luser_setval(use_individuated_cache,false).

maybe_pbox_vm(VM):- VM.option_pboxes==false,!.
maybe_pbox_vm(VM):- var(VM.option_pboxes), !, 
 (need_pboxes(VM.start_grid) -> (set(VM.option_pboxes)=true) ; (set(VM.option_pboxes)=false)),
  maybe_pbox_vm(VM).
maybe_pbox_vm(VM):- pbox_vm(VM),!.


need_pboxes(VM):- 
  testid_name_num_io(VM.id,TestID,_Example,_Num,IO),!,
  forall(kaggle_arc_io(TestID,trn+_,IO,G), \+ \+ (whole_row_or_col(C,G),C\==black)). 

whole_row_or_col(C,Center):- member(Row,Center), my_maplist(=(C),Row),!.
whole_row_or_col(C,Center):- Center\==[], Center\==[[]], rot90(Center,Cols),member(Col,Cols), my_maplist(=(C),Col),!.
not_whole_row_or_col(C,Center):- \+ whole_row_or_col(C,Center).


   

:- nodebug(indiv(pbox)).
%=====================================================================
is_fti_step(pbox_vm).
%=====================================================================
pbox_vm(VM):- !,
   %GH is round(VM.h*2/3), GV is round(VM.v*2/3),
 must_det_ll((
  GH is round(VM.h + 0), GV is round(VM.v + 0),
  %findall(size2D(H,V),(l_s_4sides(H,V),H=<GH),Sizes_L_S),
  findall(size2D(H,V),(s_l_4sides(H,V),V=<GV),Sizes_S_L),
  GridI0=VM.start_grid,
  pbox_vm(GH,GV,GridI0,Sizes_S_L,VM))).


gather_sizes(_VM,[],[]):-!.
gather_sizes(VM,[T|Types],Sizes):- 
  gather_sizes(VM,T,Sizes1),
  gather_sizes(VM,Types,Sizes2),
  append(Sizes1,Sizes2,List),list_to_set(List,Sizes).
gather_sizes(VM,P3,Sizes1):- findall(size2D(H,V),call(P3,VM,H,V),Sizes1).

:- dynamic(special_sizes/2).
%=====================================================================
is_fti_step(pbox_vm_special_sizes).
%=====================================================================

pbox_vm_special_sizes(H,V,VM):- !,
  pbox_vm_special_sizes_list([size2D(H,V)],VM).

pbox_vm_special_sizes(VM):-
  pbox_vm_special_sizes([special_sizes_v_h_sorted_s_l,special_sizes_v_h_sorted_l_s],VM),!.


pbox_vm_special_sizes(Types,VM):- !,
 must_det_ll((
  gather_sizes(VM,Types,Sizes),  
  GH is round(VM.h + 0), GV is round(VM.v + 0),
  findall(size2D(H,V),(member(size2D(H,V),Sizes),V=<GV,H=<GH),Sizes_S_L),
  pbox_vm_special_sizes_list(Sizes_S_L,VM))).

pbox_vm_special_sizes_list(Sizes_S_L,VM):-
 must_det_ll((
  GH is round(VM.h + 0), GV is round(VM.v + 0),
  GridI0=VM.grid,
  Objs=VM.objs,
  length(Sizes_S_L,A),
  pbox_vm_special_sizes(Objs,A,GH,GV,GridI0,Sizes_S_L,VM))).

color_mass_points_to_sizes(Points,Sizes):-
  my_maplist(arg(1),Points,Colors),
  sort_safe(Colors,SColors),
  findall(size2D(SizeX,SizeY),(member(Color,SColors),
    include(sub_var(Color),Points,CPoints),
    vis2D_and_1x1_larger(CPoints,SizeX,SizeY)),Sizes).

vis2D_and_1x1_larger(CPoints,SizeX,SizeY):- vis2D(CPoints,SizeX0,SizeY0),
  ((SizeX,SizeY)=(SizeX0,SizeY0);((SizeY is SizeY0+1,SizeX is SizeX0+1))).

pbox_vm_special_sizes(Objs,A,GH,GV,GridI0,Sizes_S_L,VM):- 
  length(Objs,Len),
  (Len=<2;A<40),!,pbox_vm(GH,GV,GridI0,Sizes_S_L,VM).
pbox_vm_special_sizes(Objs,A,GH,GV,GridI0,Sizes_S_L,VM):- 
 must_det_ll((
  my_maplist(obj_size2D,Objs,HVList),
  globalpoints(GridI0,Points),color_mass_points_to_sizes(Points,Sizes),
  append(Sizes,HVList,NEAR),
  debug_m(indiv(pbox),NEAR),
  include(near_size(GH,GV,NEAR),Sizes_S_L,UseSizes),
  length(UseSizes,B), u_dmsg(reduced(A->B)),
  pbox_vm(GH,GV,GridI0,UseSizes,VM))).

factor_of(V,GV):- R is GV rem V, ( R==0; (R \== 1, 0 is GV rem R, GV is R+V )),!.
obj_size2D(Obj,size2D(H,V)):- vis2D(Obj,H,V).

near_size(_,_,List,size2D(H,V)):- (near_size1(List,H,V);near_size1(List,V,H)),!.
near_size(GH,GV,_,size2D(H,V)):- factor_of(H,GH),factor_of(V,GV).
near_size1(List,H,V):- within1of(H,HH),within1of(V,VV), member(size2D(HH,VV),List).
within1of(H,HH):- freeze(HH, ( D is abs(H-HH), D =< 1 )).

list_upto(Size,List,Some):- length(List,L),(L=<Size ->Some=List ; (length(Some,Size),append(Some,_,List))).

pbox_vm(GH,GV,GridI0,Sizes_S_L,VM):-
   % \+ \+ (length(Sizes_S_L,A),list_upto(20,Sizes_S_L,Some),u_dmsg(use_sizes(A)),print(Some)),
   get_black(Black),
   subst_2L([wbg,bg],[Black,Black],GridI0,GridI),
   mapgrid(assign_plain_var_with(Black),GridI,GridM),
   shoot_lines_on_black_grid(GridM,Grid),
   add_top_bot_left_right(Top,_T,Grid,_B,Bot,LLeft,_LL,_RR,RRight,XSG),
   add_top_bot_left_right(Top2,_T2,XSG,_B2,Bot2,LLeft2,_LL2,_RR2,RRight2,_XSG2),
   %list_to_set(T,TColors),list_to_set(B,BColors),list_to_set(RR,RColors),list_to_set(LL,LColors),
   %intersection(TColors,BColors,BlackAndBorderV), intersection(LColors,RColors,BlackAndBorderH),
   %writeln(blackAndBorderHV=[BlackAndBorderH,BlackAndBorderV]),   
   maplist_ls(=(N),Top), maplist_ls(=(S),Bot), maplist_ls(=(W),LLeft), maplist_ls(=(E),RRight),
   maplist_ls(=(N),Top2), maplist_ls(=(S),Bot2), maplist_ls(=(W),LLeft2), maplist_ls(=(E),RRight2),!,
   % nth1(1,XSG,RTop),maplist_ls(=(N),RTop), last(XSG,RBot),maplist_ls(=(S),RBot),
   if_t( \+ (GH<4,GV<4),if_t(find_gridline_color(Grid,C),if_t(C\==black,NSEW=[C,C,C,C]))),
   NSEW=[N,S,E,W],
   if_t(nonvar(C),((nth1(1,XSG,RTop),maplist_ls(=(N),RTop), last(XSG,RBot),maplist_ls(=(S),RBot)))),
   %if_t((GH<4,GV<4),NSEW=[n,s,e,w]),
   %if_t(var(N),my_maplist(ignore_equal(black),NSEW)),
   \+ \+ ((ignore_equal_e(NSEW,['N','S','E','W']),print_side_by_side(XSG-xsg,GridI-grid))),
  localpoints_include_bg(Grid,Points),!,
  %begin_i_pbox_l(Grid,NSEW,XSG,Points,  Points2,VM,s_l(p1),Sizes_L_S), if_t(Points\==Points2,gset(VM.lo_points)=Points2),
  begin_i_pbox_l(Grid,NSEW,XSG,Points,  Points1,VM,s_l(p1),Sizes_S_L), if_t(Points\==Points1,gset(VM.lo_points)=Points1),
  %begin_i_pbox_l(Grid,NSEW,XSG,Points1, Points2,VM,l_s(p2),Sizes_L_S), if_t(Points\==Points2,gset(VM.lo_points)=Points2),
  %begin_i_pbox_l(Grid,NSEW,XSG,Points,  Points3,VM,l_s(p1),Sizes_L_S), if_t(Points\==Points3,gset(VM.lo_points)=Points3),
  % begin_i_pbox_l(Grid,NSEW,XSG,Points3, Points9,VM,s_l(p2),Sizes_S_L), if_t(Points\==Points9,gset(VM.lo_points)=Points9),
  !.

begin_i_pbox_l(Grid,NSEW,XSG,Points5,Points9,VM,S_L,Sizes_S_L):-
  copy_term(NSEW+XSG,CNSEW+CXSG),
  debug_m(indiv(pbox),begin(S_L)), my_time(must_det_ll((i_pbox_l([],Recs,Grid,CNSEW,CXSG,Points5,Points9,VM,S_L,Sizes_S_L)))),
  debug_m(indiv(pbox),Recs),!.



   %nop(i_pbox(VM,l_s(p2),SizesRectS)), nop(i_pbox(VM,s_l(p2),SizesSquareR)), nop(i_pbox(VM,s_l(p2),SizesRectR)).

%colors_of(T,TColors):- list_to_set(T,TColors).

shoot_lines_on_black_grid(Grid,Grid):-!.
shoot_lines_on_black_grid(Grid,GridO):- find_gridline_color(Grid,C), \+ is_black(C),!,GridO=Grid.
shoot_lines_on_black_grid(Grid,GridO):- Color=black,
  h_and_v(shoot_lines_on_colored_row(LC,Color),Grid,GridO),
  LC=wbg.

% back half contained_by middle if odd V
grid_halves(Grid,Left,Odd,Right):- length(Grid,L), (is_odd(L)->Odd=odd;Odd=even), M is floor(L/2), length(Left,M), append(Left,Right,Grid).

find_gridline_color(Grid,C):- find_rowline_color(Grid,C) -> true ; (rot90(Grid,Grid90), find_rowline_color(Grid90,C)).
find_rowline_color(Grid,C):- grid_halves(Grid,_,OddEven,GridM), !, find_halfline_color(OddEven,GridM,C),!.

is_odd(N):- number(N), 1 is N rem 2.
find_halfline_color(odd,[[C|Row]|_],C):- my_maplist(==(C),Row).
find_halfline_color(_,Grid,C):- member([C|Row],Grid),my_maplist(==(C),Row), \+ is_black(C),!.
find_halfline_color(_,Grid,C):- member([C|Row],Grid),my_maplist(==(C),Row), is_black(C),!.

at_least_two_colors(IBorderS,RColors):- flatten_set_bf(IBorderS,Colors),
  include(is_real_color,Colors,RColors),
  RColors = [_,_|_], \+ member(bg,RColors).


shoot_lines_on_colored_row(LC,C,Grid,GridO):- my_maplist(shoot_lines_on_rows(LC,C),Grid,GridO).

shoot_lines_on_rows(LC,C,Row,NewRow):- my_maplist(=(C),Row),!,length(Row,L),make_list(LC,L,NewRow).
shoot_lines_on_rows(_,_,Row,Row).

/*
SX-y-SX
|   |
y   y
|   |
SX-y-SX
*/
quick_test_menu(pbox).

pbox :-  clsmake, fail.
/*
pbox :-  pbox(t('05f2a901')).
pbox :-  pbox(t('06df4c85')).
pbox :-  pbox(t('60b61512')).
pbox :-  pbox(t('0b148d64')).
pbox :-  pbox(t('09629e4f')).*/
pbox :-  pbox(_).
pbox(Name):-   
  update_changes,
  (var(Name)-> true; testid_name_num_io(Name,TestID,Example,Num,IO)),
  ExampleNum=Example+Num,
  with_debugging(indiv(pbox),
  (nonvar(IO) 
   -> forall(kaggle_arc_io(TestID,ExampleNum,IO,G),ignore(pbox_io(TestID,ExampleNum,IO,G))) 
    ; forall(kaggle_arc(TestID,ExampleNum,I,O),ignore(pbox_pair(TestID,ExampleNum,I,O))))).

is_bg_color_or_var(C):- is_bg_color(C) ; \+ is_fg_color(C).
is_fg_color_or_var(C):- is_fg_color(C) ; \+ is_bg_color(C).

test_pbox:- test_p2(pbox_pair(_TestID,_What)).
quick_test_menu(test_pbox).


pbox_indivs:- 
  with_task_pairs(TestID,ExampleNum,I,O,pbox_pair(TestID,ExampleNum,I,O)).

pbox_pair(TestID,ExampleNum,GridIn,GridOut):-
  with_debugging(indiv(pbox),
    (debug_m(indiv(pbox),?- test_p2(pbox_pair(TestID,ExampleNum))),
      i_pair(i_pbox,GridIn,GridOut))).

pbox_io(TestID,ExampleNum,IO,G0):-
  kaggle_arc_io(TestID,ExampleNum,IO,_),
  (into_grid(G0,G)->true;into_grid((TestID>ExampleNum*IO),G)), 
  duplicate_term(G,GG),
  ignore(kaggle_arc_io(TestID,ExampleNum,IO,GG)),
  set_current_test(TestID),
  debug_m(indiv(pbox),?- pbox_io(TestID,ExampleNum,IO)),
  my_time((i_pbox(GG,Objs),
  pbox_io_result(TestID,ExampleNum,IO,GG,Objs))).

pbox_io_result(TestID,ExampleNum,IO,G,[]):- !,
 print_grid(wqs(red,no_result_for(?-pbox(TestID>ExampleNum*IO))),G).
/*
pbox_io_result(TestID,ExampleNum,IO,G,[Objs]):- !,
 obj_global_grid(Obj,OGrid),
 print_side_by_side(orange,G,one_result_for(?-pbox(TestID>ExampleNum*IO)),_,OGrid,(TestID>ExampleNum*IO)),!.
*/
pbox_io_result(TestID,ExampleNum,IO,G,Objs):- !,
 once((my_maplist(obj_global_grid,Objs,OGG), print_side_by_side(OGG))),!,
 print_side_by_side(cyan,G,(?-pbox(TestID>ExampleNum*IO)),_,print_grid(Objs),(TestID>ExampleNum*IO)),!.


i_pbox(GridIn,Objs):- 
  ROptions=i_pbox,
  PairName=i_pbox,
  with_individuated_cache(false,
  ((do_ig(ROptions,GridIn,IndvS),
  into_grid(GridIn,Grid),
  locally(nb_setval(debug_indiv,t),
    show_individuated_nonpair(PairName,ROptions,GridIn,Grid,IndvS))),
   maybe_subdiv(IndvS,Objs))).

maybe_subdiv([OO],Objs):- object_grid(OO,G),i(i_pbox,G,Objs),!.
maybe_subdiv(Objs,Objs).

obj_global_grid(SX,G-wqs(DSC)):- localpoints(SX,Grid),make_bg_visible(Grid,G), vis2D(SX,VH,VV), loc2D(SX,OH,OV),!,
  DSC=[loc2D(OH,OV),vis2D(VH,VV)].
obj_global_grid(SX,G-wqs(DSC)):- global_grid(SX,Grid),make_bg_visible(Grid,G), vis2D(SX,VH,VV), loc2D(SX,OH,OV),!,
  DSC=[loc2D(OH,OV),vis2D(VH,VV)].

not_in_eq(Set,V):- \+ member_eq(V, Set).
member_eq(V,Set):- member(VV,Set),VV==V,!.

no_rule(_,_).
dif_fg(SX,SY):- nop((freeze(SX,freeze(SY,((is_fg_color(SX);is_fg_color(SY))->dif(SX,SY)))))).


add_top_bot(Top,T,B,Bot,[],[Top]):- B=Bot,T=Top,B=T.
add_top_bot(Top,T,B,Bot,[],[Top,Bot]):- !, B=Bot,T=Top.
add_top_bot(Top,T,B,Bot,Inside,TInB):-
 must_det_ll((  
  last(Inside,B), [T|_]=Inside,
  length(T,H), length(Top,H), length(Bot,H),
  append([Top|Inside],[Bot],TInB))).

make_squarish(BorderRule,Inside,NewSearch):-
 must_det_ll((
  add_top_bot_left_right(Top,T,Inside,B,Bot,LLeft,LL,RR,RRight,NewSearch),
  my_maplist(BorderRule,Top,T), my_maplist(BorderRule,Bot,B),  my_maplist(BorderRule,LL,LLeft), my_maplist(BorderRule,RRight,RR))).

add_top_bot_left_right(Top,T,Inside,B,Bot,LLeft,LL,RR,RRight,NewSearch):-
 must_det_ll((
  add_top_bot(Top,T,B,Bot,Inside,TInB),
  c_r(add_top_bot(Left,L,R,Right),TInB,NewSearch),
  append([_|LL],[_],L),
  append([_|LLeft],[_],Left),
  append([_|RR],[_],R),
  append([_|RRight],[_],Right))),!.

%pbox_phase_check(A, B):- A=s_l(_)-> B=s_l(_); B=l_s(_).
pbox_phase_check(A, B):- A==B.
%pbox_phase_check(_, _).

:- abolish(cached_make_search_box,8).
:- dynamic(cached_make_search_box/8).

make_search_box(H,V,Center,Inside,CACHE,Find,IBorder,OBorder):- 
  % between(1,32,H),between(1,32,V),
  %s_l_4sides(H,V),
  HH is H,VV is V, once(make_search_box_m1(HH,VV,Center,Inside,CACHE,Find,IBorder,OBorder)).

make_search_box_m1(H,V,Center,Inside,CACHE,Find,IBorder,OBorder):- cached_make_search_box(H,V,Center,Inside,CACHE,Find,IBorder,OBorder),!.
make_search_box_m1(H,V,Center,Inside,CACHE,Find,IBorder,OBorder):-
  make_search_box_fresh_w_borders(H,V,Center,Inside,CACHE,Find,IBorder,OBorder),  
  asserta(cached_make_search_box(H,V,Center,Inside,CACHE,Find,IBorder,OBorder)).  

make_search_box_fresh_w_borders(H,V,Center,Inside,CACHE,Find,IBorder,OBorder):-
  make_search_box_fresh(H,V,Center,Inside,Find),
  CACHE = _{izmap:false,h:_,v:_,ogx:_,ogy:_,find:Find,inside:Inside,objFound:_,which:_,objMade:_,
    oBorder:BorderFind,iBorder:IBorder,oBorderTrimmed:OBorder,iBorderTrimmed:BorderInsideTrimmed},
  nsew_edges(Inside,IBorder),
  nsew_edges(Find,BorderFind),
  nsew_edges_trimed(Inside,BorderInsideTrimmed),
  nsew_edges_trimed(Find,OBorder),!.

make_search_box_fresh(1,1,Center,Inside,Find):- 
  Center=[], [[_]]=Inside, make_squarish(no_rule,Inside,  Find).
make_search_box_fresh(H,V,Center,Inside,Find):- 
  (H=<2;V=<2),!,make_grid(H,V,Inside), 
  Center=[],
  make_squarish(no_rule,Inside, Find).
make_search_box_fresh(H,V,Center,Inside,Find):-
     HH is H-2 ,VV is V-2, make_grid(HH,VV,Center),
     make_squarish(no_rule,Center,  Inside), make_squarish(no_rule,Inside,  Find).


/*
i_pbox_detect(L_S,H,V,XSG,[n,s,e,w],Inside,OH,OV):- 
 H>=2,V>=2,
 once((
   make_grid(H,V,Inside),
   make_squarish(no_rule,Inside,Find,Inside,OBorder))),
  % print_grid(in,Inside),ptv(in=Inside), print_grid(ns,Find),ptv(ns=Find), print_grid(s,XSG),ptv(s=XSG),
  ogs_11(OH,OV,Find,XSG),
  \+ \+ different_enough(L_S,Inside,Find,Inside,OBorder).
*/
trim_ends(T,TT):- append([_|TT],[_],T),!.
trim_ends(_,[]):-!.

trim_rends(I,O):- reverse(I,R),trim_lends(R,RR),reverse(RR,O).
trim_lends([_|T],T):-!.
trim_lends(_,[]):-!.
 
not_irow(C,T):- trim_ends(T,TT), \+ my_maplist(=(C),TT).
not_edge(C,Find):- append([T|_],[B],Find), not_irow(C,T),  not_irow(C,B),
  rot90(Find,Find90),append([R|_],[L],Find90), not_irow(C,R), not_irow(C,L).


nsew_edges_trimed(Find,[A,B,CR,DR]):- nsew_edges(Find,Border),my_maplist(trim_ends,Border,[A,B,C,D]), reverse(C,CR),reverse(D,DR).

member_e(List,E):- nonvar(E), member(E,List),!.
maybe_swap(C1,C2,C1,C2). maybe_swap(C1,C2,C2,C1).

experiment(G):- call(G).

%list_to_set_bf([L|List],SetOL):- is_list(L),!,my_maplist(list_to_set_bf,[L|List],SetOL).
list_to_set_bf(List,SetO):- sort_safe(List,Set), ( (select(B,Set,R), B==black) -> SetO=[black|R] ; SetO=Set).
maplist_ls(P1,List):- flatten_set_bf(List,Set),my_maplist(P1,Set).
member_ls(P1,List):- flatten_set_bf(List,Set),member(P1,Set).
black_and(F,C):- flatten_set_bf(F,[Black,C]), Black == black.
member1(C,N):- select(CC,N,R), C==CC, \+ (member(C2,R),C2==C).
flatten_set_bf([F|Rest],S):- is_list(F),!,append([F|Rest],L),!,list_to_set_bf(L,BF),!,BF=S.
flatten_set_bf(F,S):- list_to_set_bf(F,BF),!,BF=S.

which_partof_square(Which, OBJ,Find,Inside,Center, IsRim, OH, FX, OV, FY):-
  (var(Which)->member(Which,[inside, find, center, iborder]);true),  /*oborder center, ,*/
  once((Which=@=find -> (IsRim=filltype(solid),OBJ=Find,OH is FX-1, OV is FY-1) ;
   (Which=@=center -> (IsRim=filltype(solid),OBJ=Center,OH is FX+1, OV is FY+1) ;  
    (Which=@=iborder -> (IsRim=rim_of,rim_of(Inside,OBJ),OH is FX, OV is FY) ;  
     (Which=@=oborder -> (IsRim=rim_of,rim_of(Find,OBJ),OH is FX-1, OV is FY-1) ;  
      (Which=@=inside -> (IsRim=filltype(solid),OBJ=Inside,OH is FX, OV is FY))))))).

foundbox_why(FBWHY,WHY):- is_list(FBWHY),!,my_maplist(foundbox_why,FBWHY,WHY).
foundbox_why(FBWHY,[iz(media(shaped)),iz(type(FBWHY))]):-!.

i_pbox_l(SoFar,SoFar,_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,_):- Points==[], !, debug_m(indiv(pbox),pointless(L_S)).
i_pbox_l(SoFar,SoFar,_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,[]):- !, debug_m(indiv(pbox),complete(L_S)).
%i_pbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,_):- L_S \= s_l(p2), L_S \= l_s(p1), experiment(L_S \= l_s(p2)), !, debug_m(indiv(pbox),complete(L_S)).
%i_pbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,_):- L_S \= s_l(p1), L_S \= l_s(p1), experiment(L_S \= l_s(p2)), !, debug_m(indiv(pbox),complete(L_S)).

i_pbox_l(SoFarI,SoFarOut,Grid,NSEW,XSG,Points,Points9,VM,L_S,OUT):- OUT = [Size2D|Sizes],
  Size2D = size2D(H,V),
  grid_size(Grid,TGX,TGY),
  other_grid_size(Grid,OGX,OGY),
  (H>1,V>1),
  
  Rec = writeq(rec(IsRim,OH,OV,HH,VV)),
 once((
  
  % \+ (L_S=l_s(p1);L_S=s_l(p1);L_S=l_s(p2)),
% experiment( (L_S = l_s(p1) -> Which = inside; Which = center)),  
  Area is (H-1)*(V-1),
  length(Points,Len), Len>=Area,
  make_search_box(H,V,Center,Inside,CACHE,Find,IBorder,OBorder))),
  CACHE.h=TGX,CACHE.v=TGY,CACHE.ogx=OGX,CACHE.ogy=OGY,
  ogs_11(FX,FY,Find,XSG),  % (NSEW = _ ; NSEW = [BG,BG,BG,BG]), 

  % any untried search ?
  (\+ \+ (which_partof_square(_Which, OBJ1,Find,Inside,Center, IsRim, OH, FX, OV, FY), 
     grid_size(OBJ1,HH,VV),
      \+ member(Rec,SoFarI))),

  arg(_,v([BG,BG,BG,BG],[n,s,e,w]),NSEW),

  once((
  my_maplist(flatten_set_bf,[Center,Inside,Find],[CenterS,InsideS,FindS]),
  my_maplist(list_to_set_bf,IBorder,IBorderS),
  my_maplist(list_to_set_bf,OBorder,OBorderS))),
  %InsideS\==[black],
  %member(Which,[center,inside]),

  found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS, Which,FBWHY),  
  once((which_partof_square(Which, OBJ,Find,Inside,Center, IsRim, OH, FX, OV, FY), grid_size(OBJ,HH,VV), \+ member(Rec,SoFarI))),

/*  (Which=@=find -> (IsRim=filltype(solid),OBJ=Find,OH is FX-1, OV is FY-1) ;
   (Which=@=center -> (IsRim=filltype(solid),OBJ=Center,OH is FX, OV is FY) ;  
    (Which=@=iborder -> (IsRim=rim_of,rim_of(Inside,OBJ),OH is FX, OV is FY) ;  
     (Which=@=oborder -> (IsRim=rim_of,rim_of(Find,OBJ),OH is FX-1, OV is FY-1) ;  
      (Which=@=inside -> (IsRim=filltype(solid),OBJ=Inside,OH is FX, OV is FY) 
      ; (writeq(Which),ibreak, OH is FX-0, OV is FY-0)))))),*/
  once(( OBJ \=@=Grid, OBJ \==[],  %CACHE.objFound = OBJ, %CACHE.which = Which, %CACHE.objMade = Obj, %append(SoFarI,[Rec],SoFarMid),
%  fix_obj(
  OBJ\=Grid,
  obj_gpoints(Grid,OBJ,OH,OV,GOPoints),
  intersection(Points,GOPoints,Intersection,LeftOver,Unknown))),
  nop(Intersection\==[]),!, nop(Unknown==[]),
  %format('~N~q.~n',[USING]),  
  %\+ \+ (grid_size(OBJ,HH,VV), \+ (HH==TGX, VV==TGY)),
 must_det_ll((
  foundbox_why(FBWHY,WHY),
  listify(WHY,WHYL),
  make_indiv_object(VM,[iz(type(pbox)),iz(flag(always_keep)),
     iz(info(dont_reduce)),loc2D(OH,OV),vis2D(HH,VV)|WHYL],GOPoints,Obj),
  debug_c(indiv(pbox),
   ((\+ \+ ((
     ignore_equal_e(NSEW,['N','S','E','W']),
    %grid_size(OBJ,HH,VV), EH is OH+HH-1,EV is OV+VV-1, clip(OH,OV,EH,EV,Grid,OGrid), print_side_by_side(cyan,OGrid,SY,_,OBJ,F),
      dash_chars,dash_chars,
      print_ss([Find,OBJ,Obj]),
     writeg([Rec,o=loc2D(OH,OV),WHY,L_S,Size2D,CACHE, centerS=CenterS,insideS=InsideS,findS=FindS,
          iborderS=IBorderS,oborderS=OBorderS, nsew=NSEW]),
      dash_chars))))),
   %OHM1 is OH -1,OVM1 is OV -1, EHP1 is OH+HH, EVP1 is OV+VV,  clip(OHM1,OVM1,EHP1,EVP1,Grid,SGrid))),
  i_pbox_l([Rec|SoFarI],SoFarOut,Grid,NSEW,XSG,LeftOver,Points9,VM,L_S,[Size2D|Sizes]))). 

i_pbox_l(SoFarI,SoFarO,Grid,NSEW,XSG,Points,Points9,VM,L_S,[_|Sizes]):- i_pbox_l(SoFarI,SoFarO,Grid,NSEW,XSG,Points,Points9,VM,L_S,Sizes).



existingObject(VM,GOPoints):- 
  member_ls(O,VM.objs),globalpoints_include_bg(O,Ps),
  GOPoints==Ps,!.

obj_gpoints(Grid,OBJ,OH,OV,GOPoints):-
   grid_size(OBJ,H,V),
   obj_gpoints(Grid,OBJ,OH,OV,H,V,GOPoints).

obj_gpoints(_Grid,OBJ,OH,OV,H,V,GOPoints):- is_grid(OBJ),!,
   HH is OH+H-1,VV is OV+V-1,
   hv_point(HH,VV,HV2),
   hv_point(1,1,HV1),
  localpoints_include_bg(OBJ,OPoints), 
 ((member(W-HV1,OPoints),is_fg_color(W)) -> OPoints=OOPoints ; OOPoints=[black-HV1|OPoints]),
 ((member(W-HV2,OOPoints),is_fg_color(W)) -> OOPoints=OOOOPoints ; OOOOPoints=[black-HV2|OOPoints]),
   offset_points(OH,OV,OOOOPoints,GOPoints).

obj_gpoints(Grid,OBJ,OH,OV,H,V,GOPoints):- 
  EX is OH+H-1, EY is OV+V-1,
  clip(OH,OV,EX,EY,Grid,OBJ),!,
  obj_gpoints(Grid,OBJ,OH,OV,H,V,GOPoints).


rim_of(Find,HeadNewMidFooter):- 
  append([Top|OldMid],[Bot],Find), 
  rot90(OldMid,OldMid90),
  append([Left|OldMidMid],[Rigth],OldMid90), 
  length(Left,H), length(OldMidMid,V),
  make_grid(H,V,NewOldMid),
  append([Left|NewOldMid],[Rigth],NewOldMid90), 
  rot270(NewOldMid90,NewMid),  
  append([Top|NewMid],[Bot],HeadNewMidFooter).

recolor_into(H,_,H).

hollow_out(C,XSG,SH,SV,EH,EV,Rest):-
  P2=recolor_into(C),
  repaint_area(XSG,SH,SV,EH,EV,P2,Rest).

repaint_area(XSG,SH,SV,EH,EV,P2,Rest):-
   P4=repaint_this(SH,SV,EH,EV,P2),
   mapgrid_hv(P4,XSG,Rest).

repaint_this(SH,SV,EH,EV,P2,H,V,Color,NColor):- 
  SH=<H,H=<EH, SV=<V,V=<EV,
  call(P2,Color,NColor).

mapgrid_hv(P4,XSG,Rest):-  maplist_n(1,edit_grid_row(P4),XSG,Rest).
edit_grid_row(P4,Sy,RowIn,RowOut):- maplist_n(1,edit_grid_col(P4,Sy),RowIn,RowOut).
edit_grid_col(P4,Sy,Sx,Color,NColor):- (call(P4,Sx,Sy,Color,NColor)->true;Color=NColor).
edge_or_center(1,C,[C|_]).
edge_or_center(center,C,List):- length(List,Len), Center is 1 + floor(Len/2),nth1(Center,List,C),!.


:- style_check(-singleton).

/* ___________
  | T T T T T |
  | T t t t T |
  | T t   t T |
  | T t t t T |
  | T T T T T |
   -----------
*/
not_all_same(C,List):- \+ my_maplist(==(C),List).
is_all_same(C,List):- my_maplist(=(C),List).

:- discontiguous found_box/19. 

%195ba7dc
is_sub_grid_object(CACHE,FX,FY,SX,SY,TGX,TGY,OGX,OGY,[iz(media(image)),subgrid(IX,IY,2,1),subgrid_hv(SX,SY),subgrid_loc(IX,IY)]):- 
  \+ arc_common_property(containsAll(_)),
  %this_grid_is_multiple_of_other(CACHE),
 % TGX=OGX,TGY=OGY, % searching for somethning the size of the other grid
  TGX is OGX*2+1, TGY is OGY, 
  SX is OGX, SY is OGY,
  FY=1,
 (FX=1->IX=1;((FX=:=OGX+1)->IX=2)).


is_sub_grid_object(CACHE,FX,FY,SX,SY,TGX,TGY,OGX,OGY,[iz(media(image)),subgrid(IX,IY,SX,SY),subgrid_hv(SX,SY),subgrid_loc(IX,IY)]):- 
  \+ arc_common_property(containsAll(_)),
  %this_grid_is_multiple_of_other(CACHE),
 % TGX=OGX,TGY=OGY, % searching for somethning the size of the other grid
  0 is TGX rem SX, 
  0 is TGY rem SY, % fits prefectly
  NX is TGX / SX, 
  NY is TGY / SY, 
  %NX=<5, NY=<5,
  NX*NY =< 10, % no bigger than 10 cells 

  0 is (FX-1) rem SX, 
  0 is (FY-1) rem SY,  % fits prefectly

  %make up a index for it
  IX is (FX-1)/SX+1,
  IY is (FY-1)/SY+1,!,
  subgrid_hv(SX,SY)\==subgrid_hv(10,10).

% is_sub_grid_object(FX,FY,H,V,TGX,TGY,OGX,OGY,Type)
is_sub_grid_object(CACHE,FX,FY,SX,SY,TGX,TGY,OGX,OGY,[iz(media(image)),subgrid(IX,IY,SX,SY),subgrid_hv(SX,SY),subgrid_loc(IX,IY)]):- 
  \+ arc_common_property(containsAll(_)),
  this_grid_is_multiple_of_other(CACHE),
  SX=OGX,SY=OGY, % searching for somethning the size of the other grid

  0 is (FX-1) rem OGX, 0 is (FY-1) rem OGY, % fits prefectly
  IX is ((FX-1)/OGX)+1, IY is ((FY-1)/OGY)+1,!, %make up a index for it
  subgrid_hv(SX,SY)\==subgrid_hv(10,10).


this_grid_is_multiple_of_other(CACHE):-
  CACHE.h=TGX,CACHE.v=TGY,CACHE.ogx=OGX,CACHE.ogy=OGY,
    (TGX > OGX ; TGX > OGY), !, % this grid is larger in some way
  0 is TGX rem OGX, 0 is TGX rem OGY. % This grid size is multiple of other grid


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, Why):- 
  (H>2,V>2),
  CACHE.h=TGX,CACHE.v=TGY,CACHE.ogx=OGX,CACHE.ogy=OGY,
  is_sub_grid_object(CACHE,FX,FY,H,V,TGX,TGY,OGX,OGY,Why).

found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   What, Why):- 
  (enum_fg_colors(Black);Black=black),
  once(((H>2,V>2),
  NSEW=[B,B,B,B],B = Black,
  maplist_ls(==(B),IBorderS),
  \+ whole_row_or_col(B,Center))),
  member(What=Why,[iborder=border_frame(H,V,[B]),center=framed_image(H,V,[B])]).


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, 
     twoOrThreeMakeSquare(CCs)):- \+ this_grid_is_multiple_of_other(CACHE),
  (H>1,V>1),  
  %H=V,H=3,  
  %NSEW=[B,B,B,B],ignore(B = Black),
  %\+ intersection(IBorderS,OBorderS),
  %must_det_ll((
  Inside\=@=Grid,
  flatten_set_bf(Inside,CCs),
  (H==V -> CCs=[_,_|_] ; CCs = [_,_]),
  black = Black, 
  (\+ (member(Black,CCs))),
  SH is FX , SV is FY , 
  EH is FX + H-1, EV is FY + V-1,
  hollow_out(black,Grid,SH,SV,EH,EV,Rest),
  mapgrid(is_not_in(CCs),Rest).

 /*
found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, solidSquares(C)):- 
  (H>1,V>1), NSEW=[A,B,L,D], A=B,L=D,A=L, 
  InsideS=[C], is_real_color_or_wfg(C), my_maplist(not_all_same(C),CACHE.oBorderTrimmed),!.
*/


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, 
                                                                                 solidIBorder(C,was_center,flag(always_keep))):- 
  (H>1,V>1), NSEW=[A,B,L,D], A=B,L=D,A=L,
  IBorderS=[C],is_real_color_or_wfg(C), % \+ maplist_ls(==(C),OBorderS), 
  my_maplist(not_all_same(C),CACHE.oBorderTrimmed),!.


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, 
                                                                                solidOBorder(C,flag(always_keep))):- 
  %once(L_S=l_s(p2);L_S=l_s(p2)),
  (H>1,V>1), NSEW=[A,B,L,D], A=B,L=D,A=L, flatten_set_bf(CACHE.oBorder,OBorderAll),OBorderAll=[C], 
  %maplist_ls(=(C),OBorderS),
  is_real_color_or_wfg(C), 
   my_maplist(not_all_same(C),IBorderS), 
   my_maplist(not_all_same(C),CACHE.iBorderTrimmed),
   \+ whole_row_or_col(C,Inside), !.


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, 
                                                                                   oBorder_black(B,C)):- fail,
  (H>1,V>1), 
  NSEW=[B,B,B,B],B = black,
  maplist_ls(=(C),CACHE.oBorder),C==black,
  my_maplist(not_all_same(C),CACHE.iBorder),
  \+ whole_row_or_col(C,Inside),!.


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,  inside /*iborder*/, 
                                                                dashedIBorder(PatternProp,C,CSi,IBorderS)):- %fail,
  (H>1,V>1), at_least_two_colors(IBorderS,Colors), 
     member(C,Colors), \+ member(C,CenterS),  
  \+ sub_var(fg,Colors), \+ sub_var(wbg,Colors),
  \+ sub_var(fg,Find), \+ sub_var(wbg,Find),
  no_repeats(good_borders(CACHE,[iBorderTrimmed,iBorder],CSi,PatternProp)),
  at_least_two_colors(CSi,Two),
  (Two=[A,B] -> (is_fg_color(A), is_fg_color(B)) ; Two=[_,_,_|_]).

good_borders(CACHE,Options,CSi,PatternProp):- member(PatternProp,Options),
  CACHE.PatternProp = [CSi|List], CSi\==[fg], my_maplist(==(CSi),List).


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, dashedIBorder1(C,IBorderS)):- 
  (H>1,V>1),
  member_ls(C,IBorderS),
  \+ (member_ls(D,Inside),C\==D, whole_row_or_col(D,Center)),
  once(my_maplist(edge_or_center(N,C),CACHE.iBorder)),
  maplist_ls(\==(C),CACHE.oBorder), \+ member(C,CenterS),!.

found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, dashedIBorder2(C,IBorderS)):- 
  my_maplist(member(C),IBorderS), %maplist_ls(\==(C),OBorderS),
  SH is FX + 1, SV is FY + 1, EH is FX + H, EV is FY + V,
  repaint_area(recolor_into(xxx),XSG,SH,SV,EH,EV,Rest),
  %print_grid(hollow_out,Rest),
  flatten_set_bf(Rest,RestGrid), \+ member(C,RestGrid),!.


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, solidInIBorder(C)):- 
  member([C],IBorderS),is_real_color_or_wfg(C),my_maplist(==([C]),IBorderS), 
  (\+ member(==([C]),OBorderS) -> true ; C==black),
   \+ whole_row_or_col(C,Inside),!.


found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, solidSquares(C,info(always_keep))):- 
  (H>1,V>1), InsideS=[C],  NSEW=[n,s,e,w],
  is_real_color_or_wfg(C),
  my_maplist(not_all_same(C),CACHE.oBorderTrimmed),!.

%found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, _):- 
% once(L_S=l_s(p1);L_S=s_l(p1)),!,fail.
found_box(Grid,L_S,NSEW,FX,FY,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,  find /*oborder*/,   
                                                                              dashedOBorder(PatternProp,C,CSo,OBorderS)):- fail,
  (H>1,V>1), at_least_two_colors(OBorderS,Colors), member(C,Colors), \+ member(C,InsideS),
  no_repeats(good_borders(CACHE,[oBorderTrimmed,oBorder],CSo,PatternProp)).


with_debugging(On,Goal):-
  (prolog_debug:debugging(On, TF, TO)-> true ; (TF = false,TO =[])),
  asserta((prolog_debug:debugging(On, true, [user_error|TO]):-!), Ref),
  call_cleanup(Goal,erase(Ref)).

wno_debugging(On,Goal):-
  (prolog_debug:debugging(On, TF, TO)-> true ; (TF = false,TO =[])),
  asserta((prolog_debug:debugging(On, false, []):-!), Ref),
  call_cleanup(Goal,erase(Ref)).
   
%:- include(kaggle_arc_individuation_pbox_2).
