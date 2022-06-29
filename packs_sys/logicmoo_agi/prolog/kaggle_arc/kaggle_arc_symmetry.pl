/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- use_module(library(lists)).




%is_symgrid(t('3631a71a')*_*out).
%is_symgrid(t(c444b776)*_*out).
is_symgrid(t('83302e8f')*_*in).
is_symgrid('8a371977').
is_symgrid('695367ec').
is_symgrid('7447852a').
is_symgrid('c3202e5a').
is_symgrid('5a5a2103').
is_symgrid(t('9ecd008a')*(tst+0)*in).
is_symgrid(v(f9d67f8b)*_*out).
is_symgrid(v(de493100)*_*in).
is_symgrid(v(f9d67f8b)*_*in).


is_symgrid(N):- 
    kaggle_arc(T,Trn,_,_),
    member(T,
      [t('1b60fb0c'), t('3631a71a'), t('4938f0c2'), 
      t('4c5c2cf0'), t(b8825c91), 
      t(e40b9e2f),
      t('47c1f68c'),
      
      t('9d9215db'), 
      []]),
      member(INOUT,[in,out]),
      member(Trn,[trn+_,tst+_]),
      N=T*Trn*INOUT,      
      known_gridoid(N,G),
      nonvar_or_ci(G),
      grid_size(G,H,V), H>10, V>10,
      wdmsg(is_need(N)),
      wdmsg(is_hard(N)).
is_symgrid(N):- rp_test(N).

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
test_repair_symmetry:- clsmake, repair_symmetry0.

:- add_history(test_repair_symmetry).

repair_symmetry0:- 
 forall(
 (is_symgrid(Symgrid),
  \+ is_hard(Symgrid),
  % \+ is_need(Symgrid),
  set_current_test(Symgrid),
  known_gridoid(Symgrid,Grid),
  nop(print_grid(Grid))),
  ignore((
   dash_chars,
   grid_to_id(Grid,ID),
   wdmsg(repair_symmetry(ID)),
   (test_symmetry_code(Grid,GridS,RepairedResult) -> wdmsg(success(symmetry_code(ID)));(wdmsg(none(symmetry_code(ID))),fail)),
   print_side_by_side(Grid,GridS),print_grid(_,_,test_RepairedResult,RepairedResult)))).


crop(X,Y,G,GO):- make_grid(X,Y,GO),maplist_until(aligned_rows_u,G,GO).


maplist_until(Pred2,[X|XX],[Y|YY]):- call(Pred2,X,Y),maplist_until0(Pred2,XX,YY),!.
%maplist_until(Pred2,[_|XX],[_|YY]):- maplist_until(Pred2,XX,YY),!.
maplist_until(_,[],[]).

maplist_until0(Pred2,[X|XX],[Y|YY]):- call(Pred2,X,Y)->maplist_until0(Pred2,XX,YY).
maplist_until0(_,_,[]).

maplist_until(Pred2,[X|XX]):- call(Pred2,X)->maplist_until(Pred2,XX).
maplist_until(_,_).

maplist_until_count(N,Pred2,[X|XX],[Y|YY],[Z|ZZ]):- call(Pred2,X,Y,Z)->(maplist_until_count(M,Pred2,XX,YY,ZZ),N is M+1).
maplist_until_count(0,_,_,_,[]).
maplist_until_count(N,Pred2,[X|XX],[Y|YY]):- call(Pred2,X,Y)->(maplist_until_count(M,Pred2,XX,YY),N is M+1).
maplist_until_count(0,_,_,[]).
maplist_until_count(N,Pred2,[X|XX]):- call(Pred2,X)->(maplist_until_count(M,Pred2,XX),N is M+1).
maplist_until_count(0,_,[]).


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
aligned_rows0([E|L],[EE|R]):- E=@=EE,!, aligned_rows0(L,R).

aligned_rows_u([],_):-!. % ( empty_or_open(L) ; empty_or_open(R) ), !.
aligned_rows_u(_,[]):-!.
aligned_rows_u([E|L],[E|R]):- aligned_rows_u(L,R).

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
   CommonQ = [iz(quadrant),iz(pattern(CRef))],
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
   CommonR = [iz(divider(CRef)),iz(ray(CRef))],
   
   Rays  = [obj([grid(CE),rot(same),   loc_xy(CRef,1,0)|CommonR]),
             obj([grid(CWR),rot(rot180),loc_xy(CRef,-1,0)|CommonR]),
             obj([grid(CSR),rot(rot90), loc_xy(CRef,0,1)|CommonR]),
             obj([grid(CNR),rot(rot270),loc_xy(CRef,0,-1)|CommonR])],
   Center =  [obj([grid(CC),rot(same),loc_xy(CRef,0,0)|iz(center(CRef))])],!.


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
    [iz(quadrant(CRef,Same)),
     iz(pattern(CRef,SXC,SXC,EXC,EYC)),
     rotation(Same),
     vis_hv(Width,Height),
     loc_xy(SXQ4,SYQ4),
     globalpoints(GPoints),
     center_info(CRef,SXC,SXC,EXC,EYC) /*,
     grid(LikeQ4)*/ ],OBJL).
  

clip_ray(CRef,SXC,SXC,EXC,EYC,GN,H,V,SXQ4,SYQ4,EXQ4,EYQ4,G,Same,OBJ):-
nop((
  clip(SXQ4,SYQ4,EXQ4,EYQ4,G,Q4),
  CommonQ = [iz(divider(CRef,Same)),iz(ray(CRef,SXC,SXC,EXC,EYC))],
  call(Same,Q4,LikeQ4),
  globalpoints(Q4,LPoints),
  offset_points(SXQ4,SYQ4,LPoints,GPoints),
  embue_points1(GN,H,V,SXQ4,SYQ4,EXQ4,EYQ4,GPoints,Ps),!,
  append([[rotation(Same),
     center_info(CRef,SXC,SXC,EXC,EYC),grid(LikeQ4)],CommonQ,Ps],OBJL),
  OBJ=obj(OBJL))).


%rotation(obj(L),G):- member(rotation(G),L).

%detect_grid(Grid,E):- 

grid_to_3x3_objs(VM,Ordered,Grid,NewIndiv4s,Keep,RepairedResult):- 
  notrace(catch(call_with_time_limit(4,find_and_use_pattern_gen(Grid,Image9x9)),time_limit_exceeded, 
   (wdmsg(time_limit_exceeded),fail))),
  %catch(find_and_use_pattern_gen(Grid,Image9x9),E, (wdmsg(E),fail)),
  %rtrace(find_and_use_pattern_gen(Grid,Image9x9)),
  must_det_ll((
  flatten(Image9x9,Flat),
  include(nonvar_or_ci,Flat,Grids),
  maybe_repair_image(VM,Ordered,Grids,NewIndiv4s,Keep,RepairedResult))).

test_see_symmetry:- clsmake, time((findall(_,(arc_grid(G),see_symmetry(G)),L))),length(L,N),writeln(test_see_symmetry=N).

 % time((forall(arc_grid(G),test_see_symmetry(G)))).

test_see_symmetry(G):- see_symmetry(G),!.

% 6cdd2623

% see_symmetry(VM):- Grid = VM.grid, see_symmetry(VM,vm.h,VM.v,Grid).
append_nth1([], 0, L, L).
append_nth1([H|T], N2, L, [H|R]) :-
    append_nth1(T, N, L, R),
    N2 is N+1.


see_symmetry3(GH,_GV,How,G,GG):- 
  numlist(3,GH,Row),
  D = [Row],
  dif(Same,black),
  make_list(Same,GH,ColorLine),
  maplist(dif(ColorLine),[E1,E2,E3,E4]),
  append([L,[E1,ColorLine,E2],Stuff1,[E3,ColorLine,E4],Stuff2,Start],G), % member(E,Stuff1),member(E,Stuff2),
  append([L,[E1,D,ColorLine,D,E2],Stuff1,[E3,D,ColorLine,D,E4],Stuff2,Start],GG),
  length(L,L1),
  length(Stuff1,L2),
  length(Stuff2,L3),
  How = first(1,Same,L1,L2,L3).

see_symmetry3(GH,_GV,How,G,GG):- 
  numlist(2,GH,Row),
  D = [Row],
  dif(Same,black),
  make_list(Same,GH,ColorLine),
  maplist(dif(ColorLine),[E1,E2,E3,E4]),
  append([L,[E1,ColorLine,ColorLine,E2],Stuff1,[E3,ColorLine,ColorLine,E4],Stuff2,Start],G), % member(E,Stuff1),member(E,Stuff2),
  append([L,[E1,D,ColorLine,ColorLine,D,E2],Stuff1,[E3,D,ColorLine,ColorLine,D,E4],Stuff2,Start],GG),
  length(L,L1),
  length(Stuff1,L2),
  length(Stuff2,L3),
  How = first(2,Same,L1,L2,L3).

see_symmetry3(GH,GV,How,G,GG):- 
  bg_last(Same),
  make_list(Same,GH,ColorLine),
  Start = [ColorLine,ColorLine|_],
  append(RL,Start,G),
  see_symmetry4(GH,GV,How,RL,Start,GG),!.
see_symmetry3(GH,GV,How,G,GG):- 
  bg_last(Same),
  make_list(Same,GH,ColorLine),
  Start = [ColorLine|_],
  append(RL,Start,G),
  see_symmetry4(GH,GV,How,RL,Start,GG),!.

see_symmetry3(GH,GV,How,G,GG):-
  grid_size(G,GH,GV),
  numlist(1,GH,Row),
  D = [Row],
  bg_last(Same),
  make_list(Same,GH,Blackline),
  V is floor(GV/7), 
  Vh is floor(GV/2), 
  length(L,V),!,
  between(0,V,AL),
  LL is V-AL,
  length(A,LL),
  dif(NAB,Blackline),
  A = [NAB,_|_],
  B = [BE],%[_|_],
  C = [_,_|_],
  maplist(dif(BE),A),  
 % maplist(dif(Blackline),A),
  append([L,A,B,C,A,B,R],G),
  member(E,A),member(E,C),
  dif(A,C),
  length(C,CL), CL<Vh,
  append([L,D,A,D,B,D,C,D,A,D,B,D,R],GG),!,
  
  How = [repeats(V,LL,1,CL)].

see_symmetry3(GH,GV,How,G,GG):- fail,
  grid_size(G,GH,GV),
  RL = [_|_],
  append(RL,Start,G),
  %V is floor(GV/7), 
  %length(RL,V),!,
  see_symmetry4(GH,GV,How,RL,Start,GG).

bg_last(Same):- (dif(Same,black);Same=black).

see_symmetry4(GH,GV,How,L,G,GG):- 
  grid_size(G,GH,GV),
  D = [Row],
  bg_last(Same),
  make_list(Same,GH,Blackline),
  numlist(1,GH,Row),
  length(L,V),!,
  between(0,V,AL),
  LL is V-AL,
  length(A,LL),
  dif(NAB,Blackline),
  A = [NAB,_|_],
  B = [BE],%[_|_],
  C = [_,_|_],
  maplist(dif(BE),A),  
 % maplist(dif(ColorLine),A),
  append([L,A,B,C,A,B,R],G),
  member(E,A),member(E,C),
%  dif(A,C),
  Vh is floor(GV/2), 
  length(C,CL), CL<Vh,
  append([L,D,A,D,B,D,C,D,A,D,B,D,R],GG),!,
  
  How = [repeats(V,LL,1,CL)].


see_symmetry3_HV(How,G,GG):- grid_size(G,GH,GV),see_symmetry3(GH,GV,How,G,GG).

see_symmetry2(How,G,GG):- see_symmetry3_HV(How,G,GG),!.
see_symmetry2([rot90|How],G,GG):- rot90(G,G90),see_symmetry3_HV(How,G90,GG90),rot270(GG90,GG),!.

see_symmetry1(How,G,GG):- see_symmetry2(How,G,GG),!.
see_symmetry1([flipH|How],G,GG):- flipH(G,G90),see_symmetry2(How,G90,GG90),flipH(GG90,GG),!.
see_symmetry1([diaroll(2)|How],G,GG):- roll_d(2,G,G90),see_symmetry2(How,G90,GG90),roll_d(2,GG90,GG),!.

see_symmetry0([diaroll(1)|How],G,GG):- roll_d(1,G,G90),see_symmetry1(How,G90,GG90),roll_d(1,GG90,GG),!.
see_symmetry0([rot90|How],G,GG):- rot90(G,G90),see_symmetry1(How,G90,GG90),rot270(GG90,GG),!.
see_symmetry0([flipV|How],G,GG):- flipV(G,G90),see_symmetry1(How,G90,GG90),flipV(GG90,GG),!.
see_symmetry0(How,G,GG):- see_symmetry1(How,G,GG),!.


roll_d(N,G,GG):- maplist_n(N,roll_h,G,GGR),!,reverse(GGR,GG).
roll_h(N,G,GG):- length(LL,N),append(LL,RR,G),append(RR,LL,GG).


ping_indiv_grid(see_symmetry).

see_symmetry(G):-
  see_symmetry0(How,G,GG),
  print_side_by_side(GG,G),!,
  writeln(How).
%see_symmetry(G):- !, roll_d(G,GGG),!,roll_dr(GGG,GG),!,
%  print_side_by_side(GG,G),!.

see_symmetry(G):-
  grid_size(G,GH,GV),
  list_to_set(G,UR),
  member(E,UR),
  %length(UR,V),
  V is floor(GV/2), 
  numlist(1,GH,Row),
  D = [Row],
  %print_grid(ur,G),
  length(Top,V),
  append(Top,Bottem,G),
  append_nth1(V2,V2C,[E|V3],Bottem),
  append_nth1(V0,V0C,[E|V1],Top),

  append_nth1(V10,_V10C,[E1|V11],V1),
  append_nth1(V20,_V20C,[E1|V21],V2),

  append([V0,D,V10,D,V11,D,V20,D,V21,V,V3],NG),
  print_grid(in,G),
  print_grid(ng(V0C,V,V2C),NG),
  nop((print_grid(D,V0),
  print_grid(v1,V1),
  print_grid(v2,V2),
  print_grid(v3,V3))),!.




/*

  


  nth1(N,Grid,E),nth1(N2,Grid,E),

   (wdmsg(time_limit_exceeded),fail))),
  %catch(find_and_use_pattern_gen(Grid,Image9x9),E, (wdmsg(E),fail)),
  %rtrace(find_and_use_pattern_gen(Grid,Image9x9)),
  must_det_ll((
  flatten(Image9x9,Flat),
  include(nonvar_or_ci,Flat,Grids),
  maybe_repair_image(VM,Ordered,Grids,NewIndiv4s,Keep,RepairedResult))).
*/

consensus(ColorAdvice,GridS,H,V,VGrid):-
 forall(between(1,V,Y),
  forall(between(1,H,X),
   ignore(consensus1(ColorAdvice,GridS,X,Y,VGrid)))).
consensus1(ColorAdvice,GridS,X,Y,VGrid):- 
  findall(C,(member(G,GridS),get_color_at(X,Y,G,C)),L),
  consensus22(_How,ColorAdvice,L,R),
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

consensus22(How,ColorAdvice,L,C):- 
  my_partition(plain_var,L,Vars,Rest0),
  my_partition(=@=(ColorAdvice),Rest0,_,Rest),
  my_partition(is_bg_color,Rest,BGC,Rest1),
  my_partition(is_black,Rest1,Blk,Rest2),
  my_partition(is_fg_color,Rest2,Color,Other),!,
  consensus2(How,Vars,BGC,Blk,Color,Other,C),!.

%consensus2(How,Vars,BG,Blk,Color,Other,C).

is_four([A,B,C,D],A):- A=@=B,A=@=C,A=@=D,!.
is_four([A,B,C],A):- A=@=B,A=@=C,!.
%is_four([A,B,C,D],A):- A=@=B,C\=@=D,!.
%is_four([C,C],C).
:- style_check(-singleton).
consensus2(How,Vars,BG,Blk,Color,Other,C):- is_four(Color,C),!.
consensus2(How,Vars,BG,Blk,Color,Other,C):- is_four(BG,C),!.
consensus2(How,Vars,BG,Blk,Color,Other,C):- is_four(Other,C),!.
consensus2(How,Vars,BG,Blk,Color,Other,C):- is_four(Blk,C),!.
consensus2(How,Vars,BG,Blk,Color,Other,C):- is_four(Vars,C),!.
consensus2(How,Vars,BG,Blk,[C|Color],Other,C).
consensus2(How,Vars,BG,[C|Blk],Color,Other,C).
consensus2(How,Vars,[C|BG],Blk,Color,Other,C).
:- style_check(+singleton).

print_grid_i(O):- print_grid(O),!.
print_grid_i(O):- dumpST,trace,print_grid(O),!.

maybe_repair_image(VM,Ordered,Objects,CorrectObjects,Keep,RepairedResult):- 
  maplist(object_grid,Objects,AllGrids),
  AllGrids=Grids , %once(predsort(sort_on(colored_pixel_count),AllGrids,Grids);sort(AllGrids,Grids)),
  (all_rows_can_align(Grids)
    -> (Keep=[], format('~N'),writeln('Must be perfect...'),CorrectObjects = Objects);
    repair_patterned_images(VM,Ordered,Objects,Grids,CorrectObjects,Keep,RepairedResult)).

all_rows_can_align([Big|Rest]):- maplist(rows_can_align(Big),Rest).
rows_can_align(A,B):- \+ A\=B.
rows_can_align([],_):-!. % shorter image fragment
rows_can_align(_,[]):-!. % taller image fragment
rows_can_align([Row1|Rest1],[Row2|Rest2]):- 
 rows_can_align(Row1,Row2),!,
 rows_can_align(Rest1,Rest2).

all_rows_will_align([Big|Rest]):- maplist(rows_will_align(Big),Rest).
rows_will_align(A,_):- A == [], !. % shorter image fragment
rows_will_align(_,B):- B == [], !. % taller image fragment
rows_will_align([Row1|Rest1],B):- is_list(B),!, B=[Row2|Rest2],
 rows_will_align(Row1,Row2),!,
 rows_will_align(Rest1,Rest2).
rows_will_align(A,B):- A=B,!.

max_hv(Objects,H,V):- 
  findall(size(H,V),(member(O,Objects),vis_hv(O,H,V)),Sizes),
  sort(Sizes,SizesS),
  reverse(SizesS,[size(H,V)|_]),!.

makes_prefect_result(_VM,H,V,ColorAdvice,Grids,RepairedResult):-  
  maplist(unbind_color(ColorAdvice),Grids,UGrids),
  make_grid(H,V,RepairedResult),
  all_rows_will_align([RepairedResult|UGrids]).

prefect_result(VM,H,V,_Ordered,Grids,RepairedResult,ColorAdvice):-
  enum_colors(ColorAdvice),
  makes_prefect_result(VM,H,V,ColorAdvice,Grids,RepairedResult),!.

prefect_result(_VM,H,V,Ordered,Grids,RepairedResult,ColorAdvice):-
  make_grid(H,V,RepairedResult),
  advise_color(ColorAdvice,Ordered),
  consensus(ColorAdvice,Grids,H,V,RepairedResult),!.

repair_patterned_images(VM,Ordered,Objects,Grids,CorrectObjects,Keep,RepairedResult):-
 %must_det_ll
 ((
  max_hv(Objects,H,V),
  writeln('Training hard...'),
  prefect_result(VM,H,V,Ordered,Grids,RepairedResult,ColorAdvice),
  %print_grid(_,_,repairedResult,RepairedResult),
  localpoints_include_bg(RepairedResult,LPoints),
%  pt(RepairedResult),
  %pt(LPoints),
 %together(( localpoints_include_bg(RepairedResult,LPoints),hv_c_value(LPoints,C,1,2))),
 % together((maplist(set_local_points(LPoints),Grids,CorrectGrids),
 %           maplist(check_my_local_points(LPoints),CorrectGrids),
 %   all_rows_can_align(CorrectGrids))),
  maplist(replace_diffs(LPoints),Objects,CorrectObjects),!,
  my_partition(same_lcolor(ColorAdvice),Ordered,Keep,_))),!.

together(Goal):- call(Goal),!.

check_my_local_points([],_Grid):- !.
check_my_local_points([Point|List],Correct):- 
 must_det_ll(together((point_to_hvc(Point,H,V,Expected),
 (\+ (hv_c_value_or(Correct,Found,H,V,Expected),Found==Expected) -> set_local_points(Point,Correct,Correct) ; true),
 (hv_c_value_or(Correct,Found,H,V,Expected),Found==Expected)))),
 check_my_local_points(List,Correct).

advise_color(ColorAdvice,Ordered):- member_color(Ordered,ColorAdvice).
advise_color(ColorAdvice,Ordered):- enum_colors(ColorAdvice), \+ member_color(Ordered,ColorAdvice).
advise_color(ColorAdvice,Ordered):- member_color(Ordered,ColorAdvice).

member_color(Ordered,ColorAdvice):- member(Obj,Ordered),color(Obj,ColorAdvice).

replace_diffs(LPoints,Obj,NewObj):- 
 must_det_ll((
  vis_hv(Obj,H,V),
  my_partition(point_between(1,1,H,V),LPoints,Points,_),
  localpoints(Obj,LP),
  intersection(LP,Points,_Same,LPOnly,LPointOnly),
  ((LPOnly ==[], LPointOnly ==[]) -> NewObj = Obj ;
  (
   % pt(LPOnly), pt(LPointOnly),
  rebuild_from_localpoints(Obj,Points,NewObjM))))),
  override_object(repaired(pattern),NewObjM,NewObj),
    %print_grid_i(NewObj),
  nop(show_shape(NewObj)),!.

point_between(LoH,LoV,HiH,HiV,Point):- point_to_hvc(Point,H,V,_),
  between(LoH,HiH,H), between(LoV,HiV,V).

  

sort_on(C,R,A,B):- (A==B-> R=0 ; (call(C,A,AA),call(C,B,BB),!,compare(R,AA+A,BB+B))).
using_compare(C,R,A,B):- (A==B-> R=0 ; ( (must_det_ll((call(C,A,AA),call(C,B,BB),!,compare(R,AA,BB)))))).
colored_pixel_count(A,Count):- is_points_list(A),fg_color_count(A,Count),!.
colored_pixel_count(G,Count):- is_grid(G), fg_color_count(G,Count),!.
colored_pixel_count(A,Count):- is_object(A),localpoints(A,G), fg_color_count(G,Count),!.
colored_pixel_count(A,Count):- is_list(A),!,maplist(colored_pixel_count,A,Summe),sum_list(Summe,Count),!.
colored_pixel_count(A,1):- atomic(A),is_fg_color(A),!.
colored_pixel_count(_,0).

fg_color_count(G,AA):- must_det_ll((findall(E,(sub_term(E,G),\+ plain_var(E),is_fg_color(E)),L),length(L,AA))).

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



/*
Repetitive Patterns

  
       |      |      
 PatWidth|  EXCC|  EXQ4|
|--------------------| 
|      |      |      | SYQ2 
|  Q2  |  CN  | Q1   | 
|      |      |      | EYQ2
|------+------+------| 
*/


list_n_times(List,1,List).
list_n_times(List,2,Times):- append(List,List,Times).
list_n_times(List,3,Times):- append([List,List,List],Times).
list_n_times(_List,0,[]).
list_n_times(List,N,Times):- N>3,make_list_inited(N,List,A),append(A,Times).

repeat_pat0(cols(2),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern],Row),            C1\==C2.

repeat_pat0(cols(3),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern,Div,Pattern],Row),            C1\==C2.

repeat_pat0(cols(4),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern,Div,Pattern,Div,Pattern],Row),            C1\==C2.

repeat_pat0(cols(5),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern],Row),  C1\==C2.

repeat_pat0(cols(6),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern],Row),  C1\==C2.

repeat_pat0(cols(7),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern],Row),  C1\==C2.

repeat_pat0(cols(8),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern],Row),  C1\==C2.

repeat_pat0(cols(9),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern],Row),  C1\==C2.

repeat_pat0(cols(10),Div,[],Row,Pattern):-
  [C1|_C1Pattern] = Pattern,
  [C2|_XLL] = Div,
  append([Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern,Div,Pattern],Row),  C1\==C2.


%repeat_pat0(n_d_cols,Div,Slack,Row,Pattern):-
%  append([Pattern,Div,Pattern,Slack],Row).

%repeat_pat0(no_div,[],Slack,Row,Pattern):-
%  append([Pattern,Pattern,Pattern,Slack],Row).

maybe_pad_start_end(Row,RowP):- append([_,_|RowP],[_,_],Row).
maybe_pad_start_end(Row,RowP):- append([_|RowP],[_],Row).
maybe_pad_start_end(Row,Row).

suggest_div([_],1).
suggest_div([D,D],2).
suggest_div([],0).
repeat_pat(Type,PatWidth,DivW,Div,Row0,Pattern):-
  maybe_pad_start_end(Row0,Row),
  (nonvar(PatWidth)-> length(Pattern,PatWidth); Pattern=[_,_|_]),
  (nonvar(DivW)-> length(Div,DivW); suggest_div(Div,DivW)),
  repeat_pat0(Type,Div,Slack,Row,Pattern),
  (var(PatWidth)-> length(Pattern,PatWidth); true),
  (var(DivW)-> length(Div,DivW); true),
  length(Slack,SlackW),
  once((SlackW>PatWidth) -> append([_,Pattern,_],Slack);true).
%repeat_pat(varry_div(Was),PatWidth,DivW,_,Row,Pattern):-
%  repeat_pat0(Was,PatWidth,DivW,_,Row,Pattern).

h_pattern_length0(G,Type,PatWidth,DivW,StartV,PatHeight,Pattern,Div):- 
  suggest_div(Before,StartV),
  append(Before,[Row|RRest],G),
  repeat_pat(Type,PatWidth,DivW,_,Row,Left),  
  maplist_until_count(Count,repeat_pat(Type,PatWidth,DivW,Div),RRest,HVPat),
  Count>3,
  ( maplist_until_count(_PatHeight,repeat_pat(Type,PatWidth,DivW,Div),Before,BeforePat)->
     (StartV=0,append(BeforePat,[Left|HVPat],Pattern));
     (length(Before,StartV),[Left|HVPat]=Pattern)),
  length(Pattern,PatHeight).

h_pattern_length(G,Type,PatWidth,DivW,StartV,PatHeight,Pattern,Div):-
  h_pattern_length0(G,Type,PatWidth,DivW,StartV,PatHeight,Pattern,Div),
  %\+ all_bg(Pattern),
  nop(verify_if_can_repeat(G,StartV,Pattern,Div,PatHeight)).

verify_if_can_repeat(G,StartV,Pattern,Div,PatHeight):- 
  length(G,IH),
  RestGV is IH - StartV - PatHeight,!,
  once((RestGV =< PatHeight) -> true ; 
    (forall(member(R,G),append([_,Div,_],R)),
     maplist(append,Pattern,_,PatternO),
     length(RemainingRows,RestGV),
     append(_,RemainingRows,G),
     append([_,PatternO,_],RemainingRows))).

all_bg(Pattern):- get_bgc(BG),is_all_color(BG,Pattern).
is_all_color(BG,Pattern):- is_list(Pattern),!,maplist(is_all_color(BG),Pattern).
is_all_color(BG,Pattern):- Pattern=@=BG.

find_colorfull_idioms(G):- 
  set_current_test(G),
  h_pattern_length(G,_Type,PatWidth,_DivW,_StartV,PatV,Pattern,_Div),!,
  PatV>2,
  PatWidth>2,
  dash_chars,
  add_shape_lib(as_is,Pattern),
  add_shape_lib(pair,Pattern),
  print_grid(Pattern),!,
  writeln(find_colorfull_idioms),
  dash_chars.

is_fti_step(find_repetition).

find_repetition(VM):-
  Grid = VM.grid,
  forall(ping_indiv_grid(P1),call(P1,Grid)),!.

ping_indiv_grid(find_colorfull_idioms).
ping_indiv_grid(test_reps).

test_reps:-  clsmake, 
  forall(rp_test(G),ignore(test_reps(G))).
:- add_history(test_reps).
test_reps(G):-
  set_current_test(G),
  h_pattern_length(G,Type,PatWidth,DivW,StartV,PatV,Pattern,Div),!,
  PatV>1,
  dash_chars,
  dash_chars,
  add_shape_lib(pair,Pattern),
  grid_size(G,H,V),
  writeln(pattern),
  %grid_to_id(G,ID),
  print_grid(G),
  writeln(repired_result),
  pt(rp(patW=PatWidth,type=Type,divW=DivW,startV=StartV,patV=PatV)),
  %pt(Grid9x9),
  print_grid(Pattern),!,
  ignore(((Div\==[]),print_grid([Div]))),!,
  StartH=0,
  nop(must_det_ll((gen_pattern(H,V,StartH,StartV,Pattern,Div,same,NewGrid),
  print_grid(NewGrid)))),  
  dash_chars,!.

gen_pattern(H,V,StartH,StartV,Pattern,Div,NextP2,NewGridO):-
  GH is H - StartH,
  GV is V - StartV,
  (StartV > 0 -> make_grid(StartV,H,StartGrid) ; StartGrid=[]),
  nop(StartGrid=StartGrid),
  gen_pattern(GH,GV,Pattern,Div,NextP2,NewGridO),!.
  % crop(H,V,NewGridO,NewGrid).
  
gen_pattern(GH,GV,Pattern,Div,_NextP2,NewGrid):-
  length(Div,DivW),
  grid_size(Pattern,PH,PV),PDW is PH+DivW,PDV is PV,
  make_list_inited(PDV,Div,DivA),
  maplist(append,DivA,Pattern,DivAPattern),
  TimesH is GH div PDW,
  gen_w_pattern(TimesH,Pattern,DivAPattern,Patterns),
  %crop(GH,PDV,Patterns,CropPatterns),
  CropPatterns=Patterns,
  grid_size(CropPatterns,CPH,_CPV),
  TimesHDiv is CPH div DivW-1,
  append_n_times(Div,TimesHDiv,RowSep),
  print_grid(CropPatterns),
  print_grid([RowSep]),
  append([RowSep,CropPatterns],SepCropPatterns),
  %print_grid(SepCropPatterns),
  TimesV is GV div PDV -1,
  append_n_times(SepCropPatterns,TimesV,NewGrid).
  %make_list_inited(TimesV,CropPatterns,CropPatterns,NewGrid).

gen_w_pattern(0,Pattern,_DivAPattern,Pattern):-!.
gen_w_pattern(Times,PatternIn,DivAPattern,Patterns):-
  maplist(append,PatternIn,DivAPattern,PatternsMid),
  TimesM is Times - 1, gen_w_pattern(TimesM,PatternsMid,DivAPattern,Patterns).

rp_test0(X):- X = [[black,black,black,black,black,black,black,black,black,yellow,black,black,black,black,black,black,black,black,black],[black,black,black,black,black,orange,black,black,black,yellow,black,black,black,black,black,orange,black,black,black],[black,black,black,red,black,black,black,black,black,yellow,black,black,black,red,black,black,black,black,black],[black,black,red,black,black,black,black,black,black,yellow,black,black,red,black,black,black,black,black,black],[black,green,black,black,black,green,black,black,black,yellow,black,green,black,black,black,green,black,black,black],[black,black,black,black,black,black,black,black,black,yellow,black,black,black,black,black,black,black,black,black],[black,black,black,cyan,orange,black,black,black,black,yellow,black,black,black,cyan,orange,black,black,black,black],[black,black,black,black,cyan,black,black,green,black,yellow,black,black,black,black,cyan,black,black,green,black],[black,orange,black,black,black,black,black,black,black,yellow,black,orange,black,black,black,black,black,black,black],[yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow,yellow],[black,black,black,black,black,black,black,black,black,yellow,black,black,black,black,black,black,black,black,black],[black,black,black,black,black,orange,black,black,black,yellow,black,black,black,black,black,orange,black,black,black],[black,black,black,red,black,black,black,black,black,yellow,black,black,black,red,black,black,black,black,black],[black,black,red,black,black,black,black,black,black,yellow,black,black,red,black,black,black,black,black,black],[black,green,black,black,black,green,black,black,black,yellow,black,green,black,black,black,green,black,black,black],[black,black,black,black,black,black,black,black,black,yellow,black,black,black,black,black,black,black,black,black],[black,black,black,cyan,orange,black,black,black,black,yellow,black,black,black,cyan,orange,black,black,black,black],[black,black,black,black,cyan,black,black,green,black,yellow,black,black,black,black,cyan,black,black,green,black],[black,orange,black,black,black,black,black,black,black,yellow,black,orange,black,black,black,black,black,black,black]].
rp_test0(G):- into_grid(t(c444b776)*(trn+0)*out,G).
rp_test0(G):- arc_grid(G).

rp_test(X):- rp_test0(X).
rp_test(Y):- rp_test0(X),rot90(X,Y).




/*
 %grid_to_id(G,GN), grid_size(G,H,V),
 Q2 = HVPat, Q1 = HVPat,
 Q3 = HVPat, Q4 = HVPat,
 CW = CYL,CE = CYL, CN = CXL, CS = CXL,
 %gensym('CRef_',CRef),
  [[Q2,  CN, Q1],
   [CW, _CC, CE],
   [Q3,  CS, Q4]] = Grid9x9.
*/



find_and_use_pattern_gen(G,Grid9x9):- 
 grid_to_id(G,GN), grid_size(G,H,V),
 gensym('CRef_',CRef),
  [[Q2,  CN, Q1],
   [CW, _CC, CE],
   [Q3,  CS, Q4]] = Grid9x9,
 (mirror_xy(_CXL,_CYL,_CX,_CY,SXQ2,SYQ2,EXQ2,EYQ2,
   SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,EYQ4,G)-> writeln(did_find_pattern_gen); 
   (writeln(did_NOT_find_pattern_gen),nop(mirror_xy(_CXL,_CYL,_CX,_CY,SXQ2,SYQ2,EXQ2,EYQ2,
   SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,EYQ4,G)),fail)),
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



test_symmetry_code(Grid,Grids,RepairedResult):- 
  grid_to_3x3_objs(_VM,[],Grid,Grids,_Keep,RepairedResult).
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




