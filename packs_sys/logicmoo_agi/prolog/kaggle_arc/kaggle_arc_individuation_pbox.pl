/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

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

 format(':- style_check(-singletons). ~n'),
 make_gclip_cache_0,
 told.

make_gclip_cache:- 
 my_time((
 make_gclip_cache_file,
 consult('make_gclip_cache.pl'))),
 functor(G,gclip_cache,10),
 predicate_property(G,number_of_clauses(NC)),
 wdmsg(gclip_cache=NC).

%=====================================================================
is_fti_step(maybe_pbox_vm).
%=====================================================================
%:- luser_setval(individuated_cache,false).

maybe_pbox_vm(VM):- VM.option_pboxes==false,!.
maybe_pbox_vm(VM):- var(VM.option_pboxes), !, 
 (need_pboxes(VM.grid_o) -> (set(VM.option_pboxes)=true) ; (set(VM.option_pboxes)=false)),
  maybe_pbox_vm(VM).
maybe_pbox_vm(VM):- pbox_vm(VM),!.


need_pboxes(VM):- 
  testid_name_num_io(VM.id,TestID,_Example,_Num,IO),!,
  forall(kaggle_arc_io(TestID,trn+_,IO,G), \+ \+ (whole_row_or_col(C,G),C\==black)). 

whole_row_or_col(C,Center):- member(Row,Center), maplist(=(C),Row).
whole_row_or_col(C,Center):- Center\==[], Center\==[[]], rot90(Center,Cols),member(Col,Cols), maplist(=(C),Col).
not_whole_row_or_col(C,Center):- \+ whole_row_or_col(C,Center).



%=====================================================================
is_fti_step(pbox_vm).
%=====================================================================
pbox_vm(VM):- !,
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
   %intersection(TColors,BColors,BlackAndBorderV), intersection(LColors,RColors,BlackAndBorderH),
   %writeln(blackAndBorderHV=[BlackAndBorderH,BlackAndBorderV]),   
   maplist_ls(=(N),Top), maplist_ls(=(S),Bot), maplist_ls(=(W),LLeft), maplist_ls(=(E),RRight),
   maplist_ls(=(N),Top2), maplist_ls(=(S),Bot2), maplist_ls(=(W),LLeft2), maplist_ls(=(E),RRight2),
   % nth1(1,XSG,RTop),maplist_ls(=(N),RTop), last(XSG,RBot),maplist_ls(=(S),RBot),
   if_t( \+ (GH<4,GV<4),if_t(find_gridline_color(Grid,C),if_t(C\==black,NSEW=[C,C,C,C]))),
   NSEW=[N,S,E,W],
   if_t(nonvar(C),((nth1(1,XSG,RTop),maplist_ls(=(N),RTop), last(XSG,RBot),maplist_ls(=(S),RBot)))),
   if_t((GH<4,GV<4),NSEW=[n,s,e,w]),
   %if_t(var(N),maplist(ignore_equal(black),NSEW)),
   \+ \+ ((maplist(ignore_equal,NSEW,['N','S','E','W']),print_side_by_side(XSG-xsg,GridI-grid))),
  localpoints_include_bg(Grid,Points),!,
  begin_i_pbox_l(Grid,NSEW,XSG,Points,  Points1,VM,s_l(1),Sizes_S_L),
  begin_i_pbox_l(Grid,NSEW,XSG,Points1, Points2,VM,l_s(2),Sizes_L_S),
  if_t(Points\==Points2,set(VM.points)=Points2),
  begin_i_pbox_l(Grid,NSEW,XSG,Points,  Points3,VM,l_s(1),Sizes_L_S),
  begin_i_pbox_l(Grid,NSEW,XSG,Points3, Points9,VM,s_l(2),Sizes_S_L),
  if_t(Points\==Points9,gset(VM.points)=Points9),
  !.

begin_i_pbox_l(Grid,NSEW,XSG,Points5,Points9,VM,S_L,Sizes_S_L):-
  copy_term(NSEW+XSG,CNSEW+CXSG),
  dmsg(begin(S_L)), my_time(must_det_ll((i_pbox_l(Grid,CNSEW,CXSG,Points5,Points9,VM,S_L,Sizes_S_L)))),!.



   %nop(i_pbox(VM,l_s(2),SizesRectS)), nop(i_pbox(VM,s_l(2),SizesSquareR)), nop(i_pbox(VM,s_l(2),SizesRectR)).

colors_of(T,TColors):- list_to_set(T,TColors).

shoot_lines_on_black_grid(Grid,Grid):-!.
shoot_lines_on_black_grid(Grid,GridO):- find_gridline_color(Grid,C), \+ is_black(C),!,GridO=Grid.
shoot_lines_on_black_grid(Grid,GridO):- Color=black,
  h_and_v(shoot_lines_on_colored_row(LC,Color),Grid,GridO),
  LC=wbg.

% back half contains middle if odd V
grid_halves(Grid,Left,Odd,Right):- length(Grid,L), (is_odd(L)->Odd=odd;Odd=even), M is floor(L/2), length(Left,M), append(Left,Right,Grid).

find_gridline_color(Grid,C):- find_rowline_color(Grid,C) -> true ; (rot90(Grid,Grid90), find_rowline_color(Grid90,C)).
find_rowline_color(Grid,C):- grid_halves(Grid,_,OddEven,GridM), !, find_halfline_color(OddEven,GridM,C),!.

is_odd(N):- number(N), 1 is N rem 2.
find_halfline_color(odd,[[C|Row]|_],C):- maplist(==(C),Row).
find_halfline_color(_,Grid,C):- member([C|Row],Grid),maplist(==(C),Row), \+ is_black(C),!.
find_halfline_color(_,Grid,C):- member([C|Row],Grid),maplist(==(C),Row), is_black(C),!.



shoot_lines_on_colored_row(LC,C,Grid,GridO):- maplist(shoot_lines_on_rows(LC,C),Grid,GridO).

shoot_lines_on_rows(LC,C,Row,NewRow):- maplist(=(C),Row),!,length(Row,L),make_list(LC,L,NewRow).
shoot_lines_on_rows(_,_,Row,Row).

/*
X-y-X
|   |
y   y
|   |
X-y-X
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
  (nonvar(IO) 
   -> forall(kaggle_arc_io(TestID,ExampleNum,IO,G),ignore(pbox_io(TestID,ExampleNum,IO,G))) 
    ; forall(kaggle_arc(TestID,ExampleNum,I,O),ignore(pbox_pair(TestID,ExampleNum,I,O)))).

is_bg_color_or_var(C):- is_bg_color(C) ; \+ is_fg_color(C).
is_fg_color_or_var(C):- is_fg_color(C) ; \+ is_bg_color(C).

test_pbox:- test_p2(pbox_pair(_TestID,_What)).
quick_test_menu(test_pbox).


pbox_indivs:- 
  with_test_pairs(TestID,ExampleNum,I,O,pbox_pair(TestID,ExampleNum,I,O)).

pbox_pair(TestID,ExampleNum,I,O):-
   wdmsg(?- test_p2(pbox_pair(TestID,ExampleNum))),
   pbox_io(TestID,ExampleNum,in,I), pbox_io(TestID,ExampleNum,out,O).

pbox_io(TestID,ExampleNum,IO,G0):-
  kaggle_arc_io(TestID,ExampleNum,IO,_),
  (into_grid(G0,G)->true;into_grid((TestID>ExampleNum*IO),G)), 
  duplicate_term(G,GG),
  ignore(kaggle_arc_io(TestID,ExampleNum,IO,GG)),
  set_current_test(TestID),
  wdmsg(?- pbox_io(TestID,ExampleNum,IO)),
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
 once((maplist(obj_global_grid,Objs,OGG), print_side_by_side(OGG))),!,
 print_side_by_side(cyan,G,(?-pbox(TestID>ExampleNum*IO)),_,print_grid(Objs),(TestID>ExampleNum*IO)),!.

i_pbox(GridIn,Objs):- 
  ROptions=i_pbox,
  do_ig(ROptions,GridIn,IndvS),
  into_grid(GridIn,Grid),
  locally(nb_setval(debug_as_grid,t),
   locally(nb_setval(individuated_cache,false),
    show_individuated_nonpair(igo,ROptions,GridIn,Grid,IndvS))),
  maybe_subdiv(IndvS,Objs).

maybe_subdiv([OO],Objs):- object_grid(OO,G),i(i_pbox,G,Objs),!.
maybe_subdiv(Objs,Objs).

obj_global_grid(X,G-wqs(DSC)):- localpoints(X,Grid),make_bg_visible(Grid,G), vis2D(X,VH,VV), loc2D(X,OH,OV),!,
  DSC=[loc2D(OH,OV),vis2D(VH,VV)].
obj_global_grid(X,G-wqs(DSC)):- global_grid(X,Grid),make_bg_visible(Grid,G), vis2D(X,VH,VV), loc2D(X,OH,OV),!,
  DSC=[loc2D(OH,OV),vis2D(VH,VV)].

not_in_eq(Set,V):- \+ member_eq(V, Set).
member_eq(V,Set):- member(VV,Set),VV==V,!.

no_rule(_,_).
dif_fg(X,Y):- nop((freeze(X,freeze(Y,((is_fg_color(X);is_fg_color(Y))->dif(X,Y)))))).


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
  maplist(BorderRule,Top,T), maplist(BorderRule,Bot,B),  maplist(BorderRule,LL,LLeft), maplist(BorderRule,RRight,RR))).

add_top_bot_left_right(Top,T,Inside,B,Bot,LLeft,LL,RR,RRight,NewSearch):-
 must_det_ll((
  add_top_bot(Top,T,B,Bot,Inside,TInB),
  h_as_v(add_top_bot(Left,L,R,Right),TInB,NewSearch),
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
  CACHE = _{oBorder:BorderFind,iBorder:IBorder,oBorderTrimmed:OBorder,iBorderTrimmed:BorderInsideTrimmed},
  nsew_edges(Inside,IBorder),
  nsew_edges(Find,BorderFind),
  nsew_edges_trimed(Inside,BorderInsideTrimmed),
  nsew_edges_trimed(Find,OBorder),!.

make_search_box_fresh(1,1,Center,Inside,Find):- 
  Center=[], [[_]]=Inside, make_squarish(no_rule,Inside,  Find).
make_search_box_fresh(H,V,Center,Inside,Find):- 
  (H<3;V<3),!,make_grid(H,V,Inside), 
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
 
not_irow(C,T):- trim_ends(T,TT), \+ maplist(=(C),TT).
not_edge(C,Find):- append([T|_],[B],Find), not_irow(C,T),  not_irow(C,B),
  rot90(Find,Find90),append([R|_],[L],Find90), not_irow(C,R), not_irow(C,L).


nsew_edges_trimed(Find,Trimmed):- nsew_edges(Find,Border),maplist(trim_ends,Border,Trimmed).

member_e(List,E):- nonvar(E), member(E,List),!.
maybe_swap(C1,C2,C1,C2). maybe_swap(C1,C2,C2,C1).


%list_to_set_bf([L|List],SetOL):- is_list(L),!,maplist(list_to_set_bf,[L|List],SetOL).
list_to_set_bf(List,SetO):- list_to_set(List,Set), ( (select(B,Set,R), B==black) -> SetO=[black|R] ; SetO=Set).
maplist_ls(P1,List):- flatten_set_bf(List,Set),maplist(P1,Set).
member_ls(P1,List):- flatten_set_bf(List,Set),member(P1,Set).
black_and(F,C):- flatten_set_bf(F,[Black,C]), Black == black.
member1(C,N):- select(CC,N,R), C==CC, \+ (member(C2,R),C2==C).
flatten_set_bf(F,S):- flatten([F],L),list_to_set_bf(L,BF),!,BF=S.


i_pbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,_):- Points==[], !, wdmsg(pointless(L_S)).
i_pbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,[]):- !, wdmsg(complete(L_S)).
i_pbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,_):- L_S \= s_l(2),!.
i_pbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,[size2D(H,V)|Sizes]):- 
 once((
  % \+ (L_S=l_s(1);L_S=s_l(1);L_S=l_s(2)),
  L_S = s_l(2),
  Area is (H-1)*(V-1),
  length(Points,Len), Len>=Area,
  make_search_box(H,V,Center,Inside,CACHE,Find,IBorder,OBorder))),
  ogs_11(FH,FV,Find,XSG),  % (NSEW = _ ; NSEW = [BG,BG,BG,BG]), %member(NSEW,[[BG,BG,BG,BG],[n,s,e,w]]),
  once((maplist(flatten_set_bf,[Center,Inside,Find],[CenterS,InsideS,FindS]),
  maplist(list_to_set_bf,IBorder,IBorderS),
  maplist(list_to_set_bf,OBorder,OBorderS))),
  once(found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS, Which,WHY)),
  (Which=@=find -> (OBJ=Find,OH is FH-1, OV is FV-1) ;
   (Which=@=center -> (OBJ=Center,OH is FH, OV is FV) ;  
    (Which=@=inside -> (OBJ=Inside,OH is FH, OV is FV) 
     ; (writeq(Which),break, OH is FH-0, OV is FV-0)))),
  once((
  OBJ \=@=Grid,
  OBJ \==[],
  obj_gpoints(OBJ,OH,OV,GOPoints),
  intersection(Points,GOPoints,Intersection,LeftOver,Unknown))),
  Intersection\==[],!,
  USING = w(Which,WHY,L_S,H,V,centerS=CenterS,insideS=InsideS,findS=FindS,iborderS=IBorderS,oborderS=OBorderS,nsew=NSEW),
  format('~N~q.~n',[USING]),
  must_det_ll((
    nop(Unknown==[]),  
  ((\+ \+ (maplist(ignore_equal,NSEW,['N','S','E','W']),
    %grid_size(OBJ,HH,VV), EH is OH+HH-1,EV is OV+VV-1, clip(OH,OV,EH,EV,Grid,OGrid), print_side_by_side(cyan,OGrid,Y,_,OBJ,F),
    print_side_by_side(yellow,
      Find,USING,_,
      OBJ,cpmt(o=loc2D(OH,OV),size2D(H,V)))))),
  grid_size(OBJ,HH,VV),
  make_indiv_object(VM,[birth(pbox(WHY,L_S)),iz(always_keep),vis2D(HH,VV)],GOPoints,_Obj),
  OHM1 is OH -1,OVM1 is OV -1,
  grid_size(OBJ,HH,VV), EHP1 is OH+HH, EVP1 is OV+VV,  clip(OHM1,OVM1,EHP1,EVP1,Grid,SGrid),
  (( fail,
     L_S=s_l(_),
     OBJ==center,     
     iz_symmetry(SGrid,R), R \=symmetry(none),   
     obj_gpoints(SGrid,OHM1,OVM1,SOPoints),
     intersection(LeftOver,SOPoints,SIntersect,LeftOver2,_),
     SIntersect\==[],
     make_indiv_object(VM,[birth(pbox2(Y,F))],SIntersect,Obj2),
     print_side_by_side(cyan,SGrid,Y,_,[Obj2],F))-> true
   ; (LeftOver2=LeftOver)),
  i_pbox_l(Grid,NSEW,XSG,LeftOver2,Points9,VM,L_S,[size2D(H,V)|Sizes]))). 

i_pbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,[_|Sizes]):- i_pbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,Sizes).

existingObject(VM,GOPoints):- 
  member_ls(O,VM.objs),globalpoints_include_bg(O,Ps),
  GOPoints==Ps,!.

obj_gpoints(OBJ,OH,OV,GOPoints):- localpoints_include_bg(OBJ,OPoints), offset_points(OH,OV,OPoints,GOPoints).


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
   ¯¯¯¯¯¯¯¯¯¯¯
*/
not_all_same(C,List):- \+ maplist(==(C),List).
is_all_same(C,List):- maplist(=(C),List).

found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, _):- 
 once(L_S=l_s(1);L_S=s_l(1)),!,fail.

 /*
found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, solidSquares(C)):- 
  (H>1,V>1), NSEW=[A,B,L,D], A=B,L=D,A=L, 
  InsideS=[C], is_real_color(C), maplist(not_all_same(C),CACHE.oBorderTrimmed),!.
*/

found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, solidSquares(C,always_keep)):- 
  (H>1,V>1), InsideS=[C], !, NSEW=[n,s,e,w],
  is_real_color(C),
  maplist(not_all_same(C),CACHE.oBorderTrimmed).


found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   center, solidIBorder(C,always_keep)):- 
  (H>1,V>1), NSEW=[A,B,L,D], A=B,L=D,A=L,
  IBorderS=[C],is_real_color(C), % \+ maplist_ls(==(C),OBorderS), 
  maplist(not_all_same(C),CACHE.oBorderTrimmed),!.

found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   OBJ, solidOBorder(C,always_keep)):- 
  %once(L_S=l_s(2);L_S=l_s(2)),
  (H>2,V>2), NSEW=[A,B,L,D], A=B,L=D,A=L, flatten_set_bf(CACHE.oBorder,OBorderAll),OBorderAll=[C], 
  %maplist_ls(=(C),OBorderS),
  is_real_color(C), maplist(not_all_same(C),IBorderS), maplist(not_all_same(C),CACHE.iBorderTrimmed),
   \+ whole_row_or_col(C,Inside), !,
   (inside = OBJ).

found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, oBorder_black):- 
  NSEW=[B,B,B,B],B = black,
  maplist_ls(=(C),CACHE.oBorder),C==black,
  maplist(not_all_same(C),CACHE.iBorder),
  \+ whole_row_or_col(C,Inside),!.

found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, dashedIBorder(C)):- 
  (H>2,V>2),
  member_ls(C,IBorderS),
  \+ (member_ls(D,Inside),C\==D, whole_row_or_col(D,Center)),
  once(maplist(edge_or_center(N,C),CACHE.iBorder)),
  maplist_ls(\==(C),CACHE.oBorder), \+ member(C,CenterS),!.

found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, dashedIBorder2(C)):- 
  maplist(member(C),IBorderS), %maplist_ls(\==(C),OBorderS),
  SH is FH + 1, SV is FV + 1, EH is FH + H, EV is FV + V,
  repaint_area(recolor_into(xxx),XSG,SH,SV,EH,EV,Rest),
  %print_grid(hollow_out,Rest),
  flatten_set_bf(Rest,RestGrid), \+ member(C,RestGrid),!.


found_box(L_S,NSEW,FH,FV,Find,Center,Inside,CACHE,XSG,H,V,CenterS,InsideS,FindS,IBorderS,OBorderS,   inside, solidInIBorder(C)):- 
  member([C],IBorderS),is_real_color(C),maplist(==([C]),IBorderS), 
  (\+ member(==([C]),OBorderS) -> true ; C==black),
   \+ whole_row_or_col(C,Inside),!.
   
%:- include(kaggle_arc_individuation_pbox_2).
