/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

end_of_file.

%=====================================================================
is_fti_step(maybe_cbox_vm).
%=====================================================================
%:- luser_setval(individuated_cache,false).

maybe_cbox_vm(VM):- VM.option_cboxes==false,!.
maybe_cbox_vm(VM):- var(VM.option_cboxes), !, 
 (need_cboxes(VM.grid_o) -> (set(VM.option_cboxes)=true) ; (set(VM.option_cboxes)=false)),
  maybe_cbox_vm(VM).
maybe_cbox_vm(VM):- cbox_vm(VM),!.


need_cboxes(VM):- 
  testid_name_num_io(VM.id,TestID,_Example,_Num,IO),!,
  forall(kaggle_arc_io(TestID,trn+_,IO,G), \+ \+ (whole_row_or_col(C,G),C\==black)). 


%=====================================================================
is_fti_step(cbox_vm).
%=====================================================================
cbox_vm(VM):- !,
   %GH is round(VM.h*2/3), GV is round(VM.v*2/3),
   GH is round(VM.h + 0), GV is round(VM.v + 0),
   findall(size2D(H,V),(l_s_4sides(H,V),H=<GH),Sizes_L_S),
   findall(size2D(H,V),(s_l_4sides(H,V),V=<GV),Sizes_S_L),
   GridI=VM.grid_o,
   get_black(Black),
   mapgrid(assign_plain_var_with(Black),GridI,GridM),
   shoot_lines_on_black_grid(GridM,Grid),

   

   add_top_bot_left_right(Top,_T,Grid,_B,Bot,LLeft,_LL,_RR,RRight,XSG),
   add_top_bot_left_right(Top2,_T2,XSG,_B2,Bot2,LLeft2,_LL2,_RR2,RRight2,_XSG2),
   %list_to_set(T,TColors),list_to_set(B,BColors),list_to_set(RR,RColors),list_to_set(LL,LColors),
   %intersection_univ(TColors,BColors,BlackAndBorderV), intersection_univ(LColors,RColors,BlackAndBorderH),
   %writeln(blackAndBorderHV=[BlackAndBorderH,BlackAndBorderV]),   
   maplist_ls(=(N),Top), maplist_ls(=(S),Bot), maplist_ls(=(W),LLeft), maplist_ls(=(E),RRight),
   maplist_ls(=(N),Top2), maplist_ls(=(S),Bot2), maplist_ls(=(W),LLeft2), maplist_ls(=(E),RRight2),
   % nth1(1,XSG,RTop),maplist_ls(=(N),RTop), last(XSG,RBot),maplist_ls(=(S),RBot),
   if_t( \+ (GH<4,GV<4),if_t(find_gridline_color(Grid,C),if_t(C\==black,NSEW=[C,C,C,C]))),
   NSEW=[N,S,E,W],
   if_t(nonvar(C),((nth1(1,XSG,RTop),maplist_ls(=(N),RTop), last(XSG,RBot),maplist_ls(=(S),RBot)))),
   if_t((GH<4,GV<4),NSEW=[n,s,e,w]),
   %if_t(var(N),maplist(ignore_equal(black),NSEW)),
   \+ \+ ((ignore_equal_e(NSEW,['N','S','E','W']),print_side_by_side(XSG-xsg,GridI-grid))),
  localpoints_include_bg(Grid,Points),!,
  begin_i_cbox_l(Grid,NSEW,XSG,Points,  Points1,VM,s_l(1),Sizes_S_L),
  begin_i_cbox_l(Grid,NSEW,XSG,Points1, Points2,VM,l_s(2),Sizes_L_S),
  if_t(Points\==Points2,set(VM.points)=Points2),
  begin_i_cbox_l(Grid,NSEW,XSG,Points,  Points3,VM,l_s(1),Sizes_L_S),
  begin_i_cbox_l(Grid,NSEW,XSG,Points3, Points9,VM,s_l(2),Sizes_S_L),
  if_t(Points\==Points9,set(VM.points)=Points9),
  !.

begin_i_cbox_l(Grid,NSEW,XSG,Points5,Points9,VM,S_L,Sizes_S_L):-
  copy_term(NSEW+XSG,CNSEW+CXSG),
  dmsg(begin(S_L)), my_time(must_det_ll((i_cbox_l(Grid,CNSEW,CXSG,Points5,Points9,VM,S_L,Sizes_S_L)))),!.

/*
X-y-X
|   |
y   y
|   |
X-y-X
*/
quick_test_menu(cbox).

cbox :-  once(update_and_fail_cls),fail.
/*
cbox :-  cbox(t('05f2a901')).
cbox :-  cbox(t('06df4c85')).
cbox :-  cbox(t('60b61512')).
cbox :-  cbox(t('0b148d64')).
cbox :-  cbox(t('09629e4f')).*/
cbox :-  cbox(_).

cbox(Name):-   
  update_changes,
  (var(Name)-> true; testid_name_num_io(Name,TestID,Example,Num,IO)),
  ExampleNum=Example+Num,
  (nonvar(IO) 
   -> forall(kaggle_arc_io(TestID,ExampleNum,IO,G),ignore(cbox_io(TestID,ExampleNum,IO,G))) 
    ; forall(kaggle_arc(TestID,ExampleNum,I,O),ignore(cbox_pair(TestID,ExampleNum,I,O)))).


test_cbox:- test_p2(cbox_pair(_TestID,_What)).
quick_test_menu(test_cbox).


%:- set_test_cmd2(print_all_info_for_test).

cbox_indivs:- 
  with_task_pairs(TestID,ExampleNum,I,O,cbox_pair(TestID,ExampleNum,I,O)).

cbox_pair(TestID,ExampleNum,I,O):-
   wdmsg(?- test_p2(cbox_pair(TestID,ExampleNum))),
   cbox_io(TestID,ExampleNum,in,I), cbox_io(TestID,ExampleNum,out,O).

cbox_io(TestID,ExampleNum,IO,G0):-
  kaggle_arc_io(TestID,ExampleNum,IO,_),
  (into_grid(G0,G)->true;into_grid((TestID>ExampleNum*IO),G)), 
  duplicate_term(G,GG),
  ignore(kaggle_arc_io(TestID,ExampleNum,IO,GG)),
  set_current_test(TestID),
  wdmsg(?- cbox_io(TestID,ExampleNum,IO)),
  my_time((i_cbox(GG,Objs),
  cbox_io_result(TestID,ExampleNum,IO,GG,Objs))).

cbox_io_result(TestID,ExampleNum,IO,G,[]):- !,
 print_grid(wqs(red,no_result_for(?-cbox(TestID>ExampleNum*IO))),G).

/*
cbox_io_result(TestID,ExampleNum,IO,G,[Objs]):- !,
 obj_global_grid(Obj,OGrid),
 print_side_by_side(orange,G,one_result_for(?-cbox(TestID>ExampleNum*IO)),_,OGrid,(TestID>ExampleNum*IO)),!.
*/

cbox_io_result(TestID,ExampleNum,IO,G,Objs):- !,
 once((maplist(obj_global_grid,Objs,OGG), print_side_by_side(OGG))),!,
 print_side_by_side(cyan,G,(?-cbox(TestID>ExampleNum*IO)),_,print_grid(Objs),(TestID>ExampleNum*IO)),!.

i_cbox(GridIn,Objs):- 
  ROptions=cbox_vm,
  do_ig(ROptions,GridIn,IndvS),
  into_grid(GridIn,Grid),
  locally(nb_setval(debug_as_grid,t),
   locally(nb_setval(individuated_cache,false),
    show_individuated_nonpair(igo,ROptions,GridIn,Grid,IndvS))),
  maybe_subdiv(IndvS,Objs).


i_cbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,_):- Points==[], !, wdmsg(pointless(L_S)).
i_cbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,[]):- !, wdmsg(complete(L_S)).
i_cbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,[size2D(H,V)|Sizes]):- 
  get_black(BG),
  make_search_box(H,V,Center,Inside,Find,IBorder,OBorder),
  ogs_11(FH,FV,Find,XSG),
  member(NSEW,[[BG,BG,BG,BG],[n,s,e,w]]),
  maplist(flatten_set_bf,[Center,Inside,Find],[CenterS,InsideS,FindS]),
  maplist(list_to_set_bf,IBorder,IBorderS),
  maplist(list_to_set_bf,OBorder,OBorderS),  
  found_box(L_S,NSEW,FH,FV,Find,Center,Inside,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS, Which,WHY),
  (OBJ=@=find -> (break, OH is FH-1, OV is FV-1) ;
   (Which=@=center -> (OBJ=Center,OH is FH, OV is FV) ;  
    (Which=@=inside -> (OBJ=Inside,OH is FH, OV is FV) 
     ; (writeq(Which),break, OH is FH-0, OV is FV-0)))),
  OBJ \=@=Grid,
  OBJ \==[],
  obj_gpoints(OBJ,OH,OV,GOPoints),
  intersection_univ(Points,GOPoints,Intersection,LeftOver,Unknown), 
  Intersection\==[],!,
  functor(L_S,F,_),functor(WHY,Y,_),
  USING = w(Which,Y,L_S,H,V,centerS=CenterS,insideS=InsideS,findS=FindS,iborderS=IBorderS,oborderS=OBorderS,nsew=NSEW),
  format('~N~q.~n',[USING]),
  must_det_ll((
    nop(Unknown==[]),  
  ((\+ \+ (ignore_equal_e(NSEW,['N','S','E','W']),
    %grid_size(OBJ,HH,VV), EH is OH+HH-1,EV is OV+VV-1, clip(OH,OV,EH,EV,Grid,OGrid), print_side_by_side(cyan,OGrid,Y,_,OBJ,F),
    print_side_by_side(yellow,
      Find,USING,_,
      OBJ,cpmt(o=loc2D(OH,OV),size2D(H,V)))))),

  make_indiv_object(VM,[birth(cbox(Y,F))],GOPoints,_Obj),
  OHM1 is OH -1,OVM1 is OV -1,
  grid_size(OBJ,HH,VV), EHP1 is OH+HH, EVP1 is OV+VV,  clip(OHM1,OVM1,EHP1,EVP1,Grid,SGrid),
  (( L_S=s_l(_),
     OBJ==center,
     fail,
     iz_symmetry(SGrid,R), R \=symmetry(none),   
     obj_gpoints(SGrid,OHM1,OVM1,SOPoints),
     intersection_univ(LeftOver,SOPoints,SIntersect,LeftOver2,_),
     SIntersect\==[],
     make_indiv_object(VM,[birth(cbox2(Y,F))],SIntersect,Obj2),
     print_side_by_side(cyan,SGrid,Y,_,[Obj2],F))-> true
   ; (LeftOver2=LeftOver)),
  i_cbox_l(Grid,NSEW,XSG,LeftOver2,Points9,VM,L_S,[size2D(H,V)|Sizes]))). 

i_cbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,[_|Sizes]):- i_cbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,Sizes).
