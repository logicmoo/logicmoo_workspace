/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- use_module(library(lists)).




%is_symgrid(t('3631a71a')*_*out).
is_symgrid(t(c444b776)*_*out).
is_symgrid(v(f9d67f8b)*_*out).
is_symgrid(v(de493100)*_*in).
is_symgrid(v(f9d67f8b)*_*in).


is_symgrid(N):- 
    kaggle_arc(T,Trn,_,_),
    member(T,
      [t('1b60fb0c'), t('3631a71a'), t('4938f0c2'), 
      t('4c5c2cf0'), t('9ecd008a'), t(b8825c91), 
      t(e40b9e2f),
      t('47c1f68c'),
      
      t('9d9215db'), 
      []]),
      member(INOUT,[in,out]),
      member(Trn,[trn+_,tst+_]),
      N=T*Trn*INOUT,      
      known_gridoid(N,G),
      nonvar_or_ci(G),
      grid_size(G,H,V), H>12, V>12,
      wdmsg(is_need(N)),
      wdmsg(is_hard(N)).

is_hard(t('3631a71a')*(trn+0)*in).
is_hard(t('47c1f68c')*(tst+0)*in).
is_hard(t('4c5c2cf0')*_*in).
is_hard(t('9d9215db')*_*in).
is_hard(t('4938f0c2')*_*in).

is_need(t('4c5c2cf0')*_*out).
is_need(t('4938f0c2')*_*out).
is_need(t('9d9215db')*_*out).

% is_bad(t('9d9215db')*_*in).
  
/*
*/
repair_symmetry:- clsmake, repair_symmetry0.

:- add_history(repair_symmetry).

repair_symmetry0:- 
 forall(
 (is_symgrid(Symgrid),
  \+ is_hard(Symgrid),
  \+ is_need(Symgrid),
  known_gridoid(Symgrid,Grid),
  print_grid(Grid)),
  ignore((
   wdmsg(repair_symmetry),
   test_symmetry_code(Grid,GridO),
   wdmsg(success(symmetry_code)),
   print_grid(GridO)))),!.


crop(X,Y,G,GO):- make_grid(X,Y,GO),maplist_until(aligned_rows,G,GO).


maplist_until(Pred2,[X|XX],[Y|YY]):- call(Pred2,X,Y),maplist_until0(Pred2,XX,YY),!.
%maplist_until(Pred2,[_|XX],[_|YY]):- maplist_until(Pred2,XX,YY),!.
maplist_until(_,[],[]).

maplist_until0(Pred2,[X|XX],[Y|YY]):- call(Pred2,X,Y)->maplist_until0(Pred2,XX,YY).
maplist_until0(_,_,[]).

maplist_until(Pred2,[X|XX]):- call(Pred2,X)->maplist_until(Pred2,XX).
maplist_until(_,_).


empty_or_open(L):- \+ \+ L=[].

first_quarter(Grid,GridQ):- first_half(Grid,GL),first_half(GL,GridQ).
first_half(Grid,GL):- length(Grid,L),H is floor(L/2), length(GL,H), append(GL,_,Grid).
second_half(Grid,GL):- length(Grid,L),H is floor(L/2), length(GL,H), append(_,GL,Grid).

aligned_rows([E1,E2|L],[E1,E2|R]):- 
 aligned_rows0(L,R), !,
 allowed([E1,E2|L]),
 allowed([E1,E2|R]).

allowed([E1,E2|R]):- \+ all_black([E1,E2|R]).


all_black(R):- \+ \+ R =[],!.
all_black([H|R]):- H==black, all_black(R).

rows_align([],_):-!. % shorter image fragment
rows_align(_,[]):-!. % taller image fragment
rows_align([Row1|Rest1],[Row2|Rest2]):- 
 aligned_rows0(Row1,Row2),!,
 rows_align(Rest1,Rest2).

aligned_rows0([],_):-!. % ( empty_or_open(L) ; empty_or_open(R) ), !.
aligned_rows0(_,[]):-!.
aligned_rows0([E|L],[E|R]):- !, aligned_rows0(L,R).

no_symmetry_yet(Row):- maplist(plain_var,Row),!. % no data yet
no_symmetry_yet(Row):- maplist(=(_),Row),!. % all same element

sort_row_by_num_colors(G,G0):-
   maplist(row_color_changes,G,C),keysort(C,KS),reverse(KS,Now),
   maplist(arg(2),Now,G0).

row_color_changes(List,0-List):- (plain_var(List);List==[]),!.
row_color_changes([H|List],C-[H|List]):- row_color_changes(0,H,List,R), C is floor(R/2).

row_color_changes(Res,_,[],Res):-!.
row_color_changes(PrevCh,H,[N|List],Res):- plain_var(N),!,row_color_changes(PrevCh,H,List,Res).
row_color_changes(PrevCh,H,[N|List],Res):- H\==N, row_color_changes(PrevCh+1,N,List,Res).
row_color_changes(PrevCh,H,[N|List],Res):- H==N,  row_color_changes(PrevCh,N,List,Res).

append_n_times(PL,0,PL):- !.
append_n_times(PL,N,Rest):- N2 is N -1,
    append_n_times(PL,N2,Was), append(PL,Was,Rest).
   
:- dynamic(c_n_pattern_l/4).
c_n_pattern:- \+ \+ (c_n_pattern_l(I,PL,Full,F),c_n_pattern_l(I,PL,Full,F)),!.
c_n_pattern:- 
 forall(
  (between(4,15,P),length(PL,P),append_n_times(PL,8,Full)),
  assert(c_n_pattern_l(P,PL,Full,'->->->->'))),
 forall(
  (between(2,10,P),length(L,P),reverse(L,R),append(L,R,PL),append_n_times(PL,15,Full)),
   assert(c_n_pattern_l(P,PL,Full,'<><><><>'))),
  %(between(2,10,P),length(PL,P),repeat_until_30(L,P,Full)),
  % assert(c_n_pattern_l(P,PL,Full,'122333'))),
  nop(listing(c_n_pattern_l/4)).
    
:- c_n_pattern.

:- dynamic(c_n_reverse_l/7).
c_n_reverse:- \+ \+ (c_n_reverse_l(I,C,P,RowO,PLL,CL,RL),c_n_reverse_l(I,C,P,RowO,PLL,CL,RL)),!.
c_n_reverse:- 
                      forall((
                      between(0,5,C),length(CL,C),

                      between(0,10,P),length(PL,P),
                      between(10,10,L),length(LL,L),
                      I is P+L,
                      once((append(PL,LL,PLL),
                      reverse(PLL,RL),
                      append([PLL,CL,RL],Row),
                      numbervars(Row,0,_),
                      append(Row,_,RowO)))),
        assert(c_n_reverse_l(I,C,P,RowO,PLL,CL,RL))),
                      nop(listing(c_n_reverse_l/7)).

:- c_n_reverse.
/*
quaderants_and_center_rays(Grid9x9,QuadsO,CenterO,RaysO):- 
  [[ Q2, _CN,   Q1],
   [_CW, _CC,  _CE],
   [ Q3, _CS,   Q4]] = Grid9x9,
   flipH(Q1,Q1R), flipV(Q3,Q3R), flipHV(Q4,Q4R),
   gensym('CRef_',CRef),
   CommonQ = [object_shape(quadrant),object_shape(pattern(CRef))],
   Quads = [obj([grid(Q2),rot(same),loc_xy(CRef,-1,-1)|CommonQ]),
             obj([grid(Q1R),rot(flipH),loc_xy(CRef,1,-1)|CommonQ]),
             obj([grid(Q3R),rot(flipV),loc_xy(CRef,-1,1)|CommonQ]),
             obj([grid(Q4R),rot(flipHV),loc_xy(CRef,1,1)|CommonQ])],
   get_center_rays(CRef,Grid9x9,Center,Rays),
   maplist(filter_empty_grids,[Quads,Center,Rays],[QuadsO,CenterO,RaysO]).

get_center_rays(_CRef,_Grid9x9,[],[]):-!.
get_center_rays(CRef,Grid9x9,Center,Rays):- 
  [[_Q2,  CN, _Q1],
   [ CW,  CC,  CE],
   [_Q3,  CS, _Q4]] = Grid9x9,
   rot180(CW,CWR), rot270(CN,CNR), rot90(CS,CSR),
   CommonR = [object_shape(divider(CRef)),object_shape(ray(CRef))],
   
   Rays  = [obj([grid(CE),rot(same),   loc_xy(CRef,1,0)|CommonR]),
             obj([grid(CWR),rot(rot180),loc_xy(CRef,-1,0)|CommonR]),
             obj([grid(CSR),rot(rot90), loc_xy(CRef,0,1)|CommonR]),
             obj([grid(CNR),rot(rot270),loc_xy(CRef,0,-1)|CommonR])],
   Center =  [obj([grid(CC),rot(same),loc_xy(CRef,0,0)|object_shape(center(CRef))])],!.


filter_empty_grids(List,ListO):- include(obj_has_form,List,ListO).
obj_has_form(obj(List)):- member(grid(Grid),List),grid_has_points(Grid).

grid_has_points(Empty):- Empty=@=[[_]],!,fail.
grid_has_points(Empty):- is_empty_grid(Empty),!,fail.
grid_has_points(G):- is_grid(G),!.
  */ 

incr(X,X1):- X1 is X + 1.

clip_quadrant(CRef,SXC,SXC,EXC,EYC,GN,H,V,SXQ4,SYQ4,EXQ4,EYQ4,G,Same,OBJL):-
  clip(SXQ4,SYQ4,EXQ4,EYQ4,G,Q4),
  call(Same,Q4,LikeQ4),
  globalpoints(LikeQ4,LGPoints),  
  Width is EXQ4-SXQ4+1,Height is EYQ4-SYQ4+1,
  globalpoints(Q4,LPoints),
  offset_points(SXQ4,SYQ4,LPoints,GPoints),
  make_indiv_object(GN,H,V,1,1,Width,Height, LGPoints,
    [object_shape(quadrant(CRef,Same)),
     object_shape(pattern(CRef,SXC,SXC,EXC,EYC)),
     rotation(Same),
     vis_hv(Width,Height),
     loc_xy(SXQ4,SYQ4),
     globalpoints(GPoints),
     center_info(CRef,SXC,SXC,EXC,EYC) /*,
     grid(LikeQ4)*/ ],OBJL).
  

clip_ray(CRef,SXC,SXC,EXC,EYC,GN,H,V,SXQ4,SYQ4,EXQ4,EYQ4,G,Same,OBJ):-
nop((
  clip(SXQ4,SYQ4,EXQ4,EYQ4,G,Q4),
  CommonQ = [object_shape(divider(CRef,Same)),object_shape(ray(CRef,SXC,SXC,EXC,EYC))],
  call(Same,Q4,LikeQ4),
  globalpoints(Q4,LPoints),
  offset_points(SXQ4,SYQ4,LPoints,GPoints),
  embue_points1(GN,H,V,SXQ4,SYQ4,EXQ4,EYQ4,GPoints,Ps),!,
  append([[rotation(Same),
     center_info(CRef,SXC,SXC,EXC,EYC),grid(LikeQ4)],CommonQ,Ps],OBJL),
  OBJ=obj(OBJL))).


%rotation(obj(L),G):- member(rotation(G),L).

%detect_grid(Grid,E):- 

grid_to_3x3_objs(Ordered,Grid,NewIndiv4s,Keep):- 
  notrace(catch(call_with_time_limit(2,find_and_use_pattern_gen(Grid,Image9x9)),time_limit_exceeded, 
   (wdmsg(time_limit_exceeded),fail))),
  %catch(find_and_use_pattern_gen(Grid,Image9x9),E, (wdmsg(E),fail)),
  %rtrace(find_and_use_pattern_gen(Grid,Image9x9)),
  flatten(Image9x9,Flat),
  include(nonvar_or_ci,Flat,Grids),
  maybe_repair_image(Ordered,Grids,NewIndiv4s,Keep).

consensus(ColorAdvice,GridS,H,V,VGrid):-
 forall(between(1,V,Y),
  forall(between(1,H,X),
   ignore(consensus1(ColorAdvice,GridS,X,Y,VGrid)))).
consensus1(ColorAdvice,GridS,X,Y,VGrid):- 
  findall(C,(member(G,GridS),get_color_at(X,Y,G,C)),L),
  consensus22(ColorAdvice,L,R),
  ignore((% nonvar_or_ci(R),is_color(R),
  nb_set_local_point(X,Y,R,VGrid))),!.

nb_set_local_point(H,V,C,Grid):- assertion(is_grid(Grid)),!, 
  ignore((nth1(V,Grid,Row),(Row==[]-> true;nb_set_nth1(H,Row,C)))).


my_partition(_,[],[],[]):-!.
my_partition(P1,[H|L],[H|I],E):- \+ \+ call(P1,H),!,
  my_partition(P1,L,I,E).
my_partition(P1,[H|L],I,[H|E]):- 
   my_partition(P1,L,I,E),!.
my_partition(P1,H,I,HE):- dumpST,break,
  my_partition(P1,[H],I,HE).

consensus22(ColorAdvice,L,C):- 
  my_partition(plain_var,L,Vars,Rest0),
  my_partition(=@=(ColorAdvice),Rest0,_,Rest),
  my_partition(is_bg_color,Rest,BGC,Rest1),
  my_partition(is_black,Rest1,Blk,Rest2),
  my_partition(is_fg_color,Rest2,Color,Other),!,
  consensus2(Vars,BGC,Blk,Color,Other,C),!.

%consensus2(Vars,BG,Blk,Color,Other,C).

is_four([A,B,C,D],A):- A=@=B,A=@=C,A=@=D,!.
is_four([A,B,C],A):- A=@=B,A=@=C,!.
%is_four([A,B,C,D],A):- A=@=B,C\=@=D,!.
%is_four([C,C],C).
:- style_check(-singleton).
consensus2(Vars,BG,Blk,Color,Other,C):- is_four(Color,C),!.
consensus2(Vars,BG,Blk,Color,Other,C):- is_four(BG,C),!.
consensus2(Vars,BG,Blk,Color,Other,C):- is_four(Other,C),!.
consensus2(Vars,BG,Blk,Color,Other,C):- is_four(Blk,C),!.
consensus2(Vars,BG,Blk,Color,Other,C):- is_four(Vars,C),!.
consensus2(Vars,BG,Blk,[C|Color],Other,C).
consensus2(Vars,BG,[C|Blk],Color,Other,C).
consensus2(Vars,[C|BG],Blk,Color,Other,C).
:- style_check(+singleton).


maybe_repair_image(Ordered,Objects,CorrectObjects,Keep):- 
  maplist(object_grid,Objects,AllGrids),
  predsort(sort_on(colored_pixel_count),AllGrids,Grids),
  (all_rows_align(Grids)
    -> (Keep=[], format('~N'),writeln('Must be perfect...'),CorrectObjects = Objects);
    repair_patterned_images(Ordered,Objects,Grids,CorrectObjects,Keep)).

all_rows_align([Big|Rest]):- maplist(rows_align(Big),Rest).

max_hv(Objects,H,V):- 
  findall(size(H,V),(member(O,Objects),vis_hv(O,H,V)),Sizes),
  sort(Sizes,SizesS),
  reverse(SizesS,[size(H,V)|_]),!.

repair_patterned_images(Ordered,Objects,Grids,CorrectObjects,Keep):-
  max_hv(Objects,H,V),
  make_grid(H,V,Result),
  writeln('Training hard...'),
  advise_color(ColorAdvice,Ordered),
  consensus(ColorAdvice,Grids,H,V,Result),
  localpoints(Result,LPoints),
  maplist(replace_local_points(LPoints,_),Grids,CorrectGrids),!,
  all_rows_align(CorrectGrids), 
  print_grid(Result), dmsg(result),
  maplist(replace_diffs(LPoints),Objects,CorrectObjects),!,
  my_partition(same_lcolor(ColorAdvice),Ordered,Keep,_),!.

advise_color(ColorAdvice,Ordered):- member_color(Ordered,ColorAdvice).
advise_color(ColorAdvice,Ordered):- enum_colors(ColorAdvice), \+ member_color(Ordered,ColorAdvice).
advise_color(ColorAdvice,Ordered):- member_color(Ordered,ColorAdvice).

member_color(Ordered,ColorAdvice):- member(Obj,Ordered),color(Obj,ColorAdvice).

replace_diffs(LPoints,Obj,NewObj):- 
 must_det_l((
  vis_hv(Obj,H,V),  
  my_partition(point_between(1,1,H,V),LPoints,Points,_),
  rebuild_from_localpoints(Obj,Points,NewObj))),
  print_grid(NewObj),!.

point_between(LoH,LoV,HiH,HiV,Point):- point_to_hvc(Point,H,V,_),
  between(LoH,HiH,H), between(LoV,HiV,V).

  

sort_on(C,R,A,B):- (A==B-> R=0 ; (call(C,A,AA),call(C,B,BB),!,compare(R,AA+A,BB+B))).
using_compare(C,R,A,B):- (A==B-> R=0 ; (call(C,A,AA),call(C,B,BB),!,compare(R,AA,BB))).
colored_pixel_count(A,AA):- object_grid(A,G),
  findall(E,(sub_term(E,G), is_fg_color(E)),L),
  length(L,AA).


/*
4-Way Symmetry

  
|SXQ2  |SXCC  |SXQ4  
       |      |      
   EXQ2|  EXCC|  EXQ4|
|--------------------| 
|      |      |      | SYQ2 
|  Q2  |  CN  | Q1   | 
|      |      |      | EYQ2
|------+------+------| 
|      |      |      | SYCC
|  CW  |  CC  | CE   | 
|      |      |      | EYCC
|------+------+------| 
|      |      |      | SYQ4
|  Q3  |  CS  | Q4   |
|      |      |      | EYQ4
|--------------------| 
*/


mirror_xy(CXL,CYL,CX,CY,SXQ2,SYQ2,EXQ2,EYQ2,SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,EYQ4,G):-
   grid_size(G,EXQ4,EYQ4),
   SXQ2=SYQ2,SXQ2=1,
   CXL=CYL,
   length(CX,CXL),length(CY,CYL),
   mirror_hv(EXQ2,CX,EYQ2,CY,EXQ4,EYQ4,G),
   %sort_row_by_num_colors(G,Rows),
   %print_grid(Rows),
   %wqnl(cx=CX+EXQ2),print_grid(EXQ2,EYQ2,G),wqnl(cy=CY+EYQ2),
   check_mirror_xy(CX,CY,SXQ2,SYQ2,EXQ2,EYQ2,SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,EYQ4).

check_mirror_xy([],[],1,1,EXQ2,EYQ2,0,0,0,0,SXQ4,SYQ4,EXQ4,EYQ4):-
     SXQ4 is EXQ2+1,
     SYQ4 is EYQ2+1,!,
     nop((EXQ4,EYQ4)).


check_mirror_xy(CX,CY,SXQ2,_SYQ2,EXQ2,EYQ2,SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,_EYQ4):- 
   
   length(CX,LCLX),SXCC is EXQ2 + 1,EXCC is LCLX + EXQ2 -1,SXQ4 is LCLX + EXQ2,
   maplist(assertion,[SXQ2=<EXQ2,EXQ2=<SXCC,EXQ2=<SXCC,EXCC=<SXQ4,SXQ4=<EXQ4]),
   length(CY,LCLY),SYCC is EYQ2 + 1,EYCC is LCLY + EYQ2 -1,SYQ4 is LCLY + EYQ2,
   !.





find_and_use_pattern_gen(G,Grid9x9):- 
 grid_to_id(G,GN), grid_size(G,H,V),
 gensym('CRef_',CRef),
  [[Q2,  CN, Q1],
   [CW, _CC, CE],
   [Q3,  CS, Q4]] = Grid9x9,
 quietly(mirror_xy(_CXL,_CYL,_CX,_CY,SXQ2,SYQ2,EXQ2,EYQ2,
   SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,EYQ4,G)),!,
  clip_quadrant(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXQ2,SYQ2,EXQ2,EYQ2,G,flipHV,Q2),
  clip_quadrant(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXQ4,SYQ2,EXQ4,EYQ2,G,flipV,Q1),
  clip_quadrant(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXQ2,SYQ4,EXQ2,EYQ4,G,flipH,Q3),
  clip_quadrant(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXQ4,SYQ4,EXQ4,EYQ4,G,same,Q4),
  %clip_ray(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXCC,SYCC,EXCC,EYCC,G,CC),    
  clip_ray(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXCC,SYQ2,EXCC,EYQ2,G,rot270,CN),
  clip_ray(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXCC,SYQ4,EXCC,EYQ4,G,rot90,CS),
  clip_ray(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXQ4,SYCC,EXQ4,EYCC,G,same,CE),
  clip_ray(CRef,EXQ2,EYQ2,SXQ4,SYQ4,GN,H,V,SXQ2,SYCC,EXQ2,EYCC,G,rot180,CW),
  nop(print_symmetry(localpoints,Q2,Q1,Q3,Q4)),!.


print_symmetry(How,Q2,Q1,Q3,Q4):-  
  call(How,Q1,QL1), call(How,Q2,QL2),call(How,Q3,QL3),call(How,Q4,QL4),
  wdmsg("printed_pie II   I"),
  print_side_by_side(QL2,QL1),
  print_side_by_side(QL3,QL4),
  wdmsg("printed_pie III  IV"),
  !.



make_empty_grid(GO):- GO=_.

clip(0,0,0,0,_,GO):-  !, make_empty_grid(GO).
clip(0,_,_,_,_,GO):-  !, make_empty_grid(GO).
clip(SX,SY,EX,EY,_,GO):- (EY<SY ; EX<SX) , !, make_empty_grid(GO).
%clip(SX,SY,EX,EY,G,GO):- G==[[]],!,GO=[[]].
clip(SX,SY,EX,EY,G,GO):-
  %assertion(SX=<EX),assertion(SY=<EY),
  SzX is EX-SX+1,
  SzY is EY-SY+1,
  %print_grid(G),
  make_grid(SzX,SzY,GO),
  copy_to_clip_r(1,1,1,SX,SX,SY,EX,EY,G,GO),
  nop(print_grid(GO)).

copy_to_clip_r(AtX,AtY,ResetX,ResetSX,SX,SY,EX,EY,G,GO):-

  ignore((hvc_value(SX,SY,C,G),nb_set_chv(C,AtX,AtY,GO))),
  incr(SX,SX2),
 ((SX2=<EX) -> 
    (incr(AtX,AtX2),copy_to_clip_r(AtX2,AtY,ResetX,ResetSX,SX2,SY,EX,EY,G,GO)) ;
    (incr(SY,SY2),
      ((SY2=<EY) -> (incr(AtY,AtY2),copy_to_clip_r(ResetX,AtY2,ResetX,ResetSX,ResetSX,SY2,EX,EY,G,GO));
         true))).

nb_set_nth1_oob(_,[],_):-!.
nb_set_nth1_oob(1,Row,C):- !, nb_setarg(1,Row,C).
nb_set_nth1_oob(N,[_|Row],C):- Nm1 is N -1, nb_set_nth1_oob(Nm1,Row,C).

nb_set_chv(C,H,V,Grid):- nth1(V,Grid,Row),nb_set_nth1_oob(H,Row,C).
hvc_value(H,V,C,Grid):-  nth1(V,Grid,Row),nth1(H,Row,C).

idealistic_symmetric_xy_3x3(
[[Q2,         CN,        flipH(Q2)],
 [CW,        _CC,        flipH(CW)],
 [flipV(Q2),  flipV(CN), flipHV(Q2)]]).



test_symmetry_code(Grid,Grids):- grid_to_3x3_objs([],Grid,Grids,_Keep).
%test_symmetry_code(Grid,Grids):- repair_symmetry(Grid,Grids).

repair_symmetry(G,GR):-
/*
   [[Q2,  CN,   Q1],
    [CW,  CC,   CE],
    [Q3,  CS,   Q4]] = Grid,

  [[Q2R,  CNR,   Q1R],
   [CWR,   CC,   CER],
   [Q3R,  CSR,   Q4R]] = Repair,
*/
 find_and_use_pattern_gen(G,GR),!.

assemble(
 [[Q2,  CN,   Q1],
  [CW,  CC,   CE],
  [Q3,  CS,   Q4]],
  GR):-
        join_cols([Q2,  CN,   Q1],RowsA),
        join_cols([CW,  CC,   CE],RowsC),
        join_cols([Q3,  CS,   Q4],RowsC),
        append([RowsA,RowsC,RowsC], GR).


is_empty_grid(Empty):- make_empty_grid(MG),MG=@=Empty,!.
is_empty_grid(Empty):-  (Empty==[] ; Empty ==[[]]),!.

is_symmetric_h(G):- mirror_h(_,_,G),!.


:- fixup_exports.



%mirror_h(I,C,G):- include(not_no_symmetry_yet,G,G0),sort_row_by_num_colors(G0,G1),mirror_lr(I,C,G1,_).
%mirror_h(I,C,G):- mirror_lr(I,C,G,_),!.

mirror_h(I,C,G):- mirror_lrw(I,C,G,_).
mirror_h(I,C,G):- flipV(G,G1),mirror_lrw(I,C,G1,_).
mirror_v(I,C,G):- rot270(G,G0),mirror_lrw(I,C,G0,_).
mirror_v(I,C,G):- rot270(G,G0),flipV(G0,G1),mirror_lrw(I,C,G1,_).

mirror_hv(EXQ2,CX,EYQ2,CY,H,V,G):- mirror_h(EXQ2,CX,G),mirror_v(EYQ2,CY,G),TY is EYQ2*3, TY > V, TX is EXQ2*3, TX > H,!.
mirror_hv(EXQ2,CX,EYQ2,CY,_H,_V,G):- mirror_hh(EXQ2,CX,G),mirror_vv(EYQ2,CY,G).

mirror_hh(I,C,G):- mirror_lr(I,C,G,_).
mirror_vv(I,C,G):- rot270(G,G0),mirror_lr(I,C,G0,_),!.
    
mirror_row(I,C,Row,L):- append(C,[E1|RR],Right), append(L,Right,Row),reverse(L,[E1|LL]),aligned_rows(LL,RR),length(L,I).
% mirror_row(L,CL,Row,I):-  append(Row,_Rest,RowO), c_n_reverse_l(I,_C,_P,RowO,L,CL,_R).

% mirror_row(L,C,Row,I):- append(C,[E1|RR],Right), append(L,Right,Row),reverse(L,[E1|LL]),aligned_rows(LL,RR),length(L,I).

mirror_lrw(_I,_C,[],_GL):- !. 
mirror_lrw(I,C,G,GL):- maplist_until(mirror_row(I,C),G,GL),
   reverse(G,GR), maplist_until0(mirror_row(I,C),GR,_).

mirror_lr(I,C,G,GL):- first_half(G,G0),mirror_lr0(I,C,G0,GL).
mirror_lr(I,C,G,GL):- second_half(G,G0),mirror_lr0(I,C,G0,GL).
%mirror_lr(I,C,G,GL):- mirror_lr0(I,C,G,GL).
mirror_lr0(_I,_C,[],_GL):- !.
mirror_lr0(I,C,G,GL):- maplist(mirror_row(I,C),G,GL),!.

not_no_symmetry_yet(P):- \+ no_symmetry_yet(P).




