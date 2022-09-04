/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- ensure_loaded(kaggle_arc_symmetry).
%tell(s),ignore((nl,nl,test_pairs(Name,ExampleNum,In,Out),format('~N~q.~n',[test_pairs_cache(Name,ExampleNum,In,Out)]),fail)),told.


is_row_len(N,L):- L=[_|_],length(L,N).

:- dynamic(backfill/1).


set_on_grid(OH,OV,Grid,O):- is_object(O),globalpoints(O,Ps),!,set_on_grid(OH,OV,Grid,Ps).
set_on_grid(OH,OV,Grid,List):- is_list(List),maplist(set_on_grid(OH,OV,Grid),List).
set_on_grid(OH,OV,Grid,C-Point):- nonvar_or_ci(C),
  grid_color_code(C,I),
  hv_point(H,V,Point),
  HH is H - OH + 1, 
  VV is V - OV + 1,
  nth1(VV,Grid,Row),nb_set_nth1(HH,Row,I).


grid_dim(G,v_hv(H,V)):- grid_size(G,H,V).
%grid_size(O,offset_ranges(_,_,_,_,H,V)):- is_grid(O),grid_size(O,H,V).
%grid_size(P,S):- grid_size(P,S).

props_of_points(E,Ns):- findall(obj(Ps),member(obj(Ps),E),Ns).


black_first(SK,[cc(Z,CN)|BF]):- Z=black, select(cc(Z,CN),SK,BF),!.
black_first(BF,[cc(Z,0)|BF]):- Z=black.

no_black(SK,BF):-select(cc(Z,_),SK,BF),is_black(Z),!.
no_black(SK,BF):-select(Z,SK,BF),is_black(Z),!.
no_black(BF,BF).


%pixel_colors(GH,CC):- (is_group(GH);is_object(GH)),!,globalpoints(GH,GP),pixel_colors0(GP,CC).
pixel_colors(GH,CC):- quietly(pixel_colors0(GH,CC)).
pixel_colors0(GH,CC):- is_grid(GH),!,append(GH,CC).
pixel_colors0(GH,CC):- is_list(GH),!,maplist(pixel_colors0,GH,PG),my_append(PG,CC).
pixel_colors0(GH,CC):- is_colorish(GH),!,CC=GH.
pixel_colors0(Cell,[C]):- is_point(Cell),!,only_color_data_or(fg,Cell,C).
pixel_colors0(GH,CC):- globalpoints_include_bg(GH,GP),!,maplist(only_color_data_or(fg),GP,CC).
%pixel_colors0(options(_),[]):-!.

only_color_data_or(Alt,Cell,Color):- only_color_data(Cell,Color)->true;Color=Alt.

%sub_term(G,GH), is_grid(G),!,flatten(G,GF),include(is_grid_color,GF,GL),maplist(color_name,GL,CC).
%pixel_colors(G,GL):- findall(Name,(sub_term(CP,G),compound(CP),CP=(C-_),color_name(C,Name)),GL).

unique_colors(G,SUC):- colors(G,GF),quietly(maplist(arg(1),GF,UC)),sort(UC,SUC).
unique_color_count(G,Len):- unique_colors(G,UC),length(UC,Len).
colors_count_size(G,UC):- colors(G,GS),length(GS,UC).

into_cc(SK,BFO):- maplist(into_cc1,SK,BFO).
into_cc1(N-C,cc(Nm,CN)):- CN is N,!,color_name(C,Nm).

colors_count_black_first(G,BF):- colors(G,SK),black_first(SK,BF).
colors_count_no_black(G,BF):- colors(G,SK),no_black(SK,BF).

:- decl_pt(prop_h,all_colors_count(is_object_or_grid, list)).
all_colors_count(G,CC):- 
  pixel_colors(G,All), 
  findall(Nm-C,(enum_colors_test(C),occurs:count((sub_term(Sub, All), \+ \+ cmatch(C,Sub)), Nm)),BF),
  into_cc(BF,CC),!.

:- decl_pt(prop_h,some_colors_count(is_object_or_grid, list)).
some_colors_count(G,CC):- 
  pixel_colors(G,All), 
  findall(Nm-C,(enum_colors_test(C),occurs:count((sub_term(Sub, All), \+ \+ cmatch(C,Sub)), Nm),Nm\==0),BF),
  into_cc(BF,CC),!.

enum_colors_test(C):- no_repeats(C,enum_colors_test0(C)).
enum_colors_test0(C):- get_bgc(C).
enum_colors_test0(C):- C=black, \+ enum_fg_colors(C).
enum_colors_test0(C):- enum_fg_colors(C), C \== wbg, C\== '#444455'.
enum_colors_test0(fg).
enum_colors_test0(bg).
enum_colors_test0(is_colorish).
enum_colors_test0(var).


num_objects(G,NO):- compute_shared_indivs(G,GS),length(GS,NO).

make_box(X,_,G):- make_grid(X,X,G).


% S=[[1,2,3],[4,x,6],[7,8,0]],grow([[same,same],[same,same]],S, X).
join_cols([],[]).
join_cols([Grid1,Grid2],Grid):- is_grid(Grid1), !,append_left(Grid1,Grid2,Grid).
join_cols([Grid|Grids],GridO):- !,join_cols(Grid,Grids,GridO).

join_cols(Grid1,[],Grid1):-!.
join_cols(Grid1,[Grid2|Grids],Result):-   
  append_left(Grid1,Grid2,NewGrid),
  join_cols(NewGrid,Grids,Result).
 
% grow([[same,same]],[[a,b,c]], [[a,b,c,a,b,c]]).
append_left(Grid1,[],Grid1):-!.
append_left(Grid1,Empty,Grid1):- is_empty_grid(Empty),!.
append_left(Grid1,Grid2,Grid):- length(Grid1,Len),assertion(length(Grid2,Len)),maplist(my_append,Grid1,Grid2,Grid).

append_down(Grid1,Grid2,Grid):- my_append(Grid1,Grid2,Grid).

grow_row([],_,[]).
grow_row([C1    ],Grid,G1):- !, call(C1,Grid,G1).
grow_row([C1|Row],Grid,GM):- !, call(C1,Grid,G1),grow_row(Row,Grid,GR),append_left(G1,GR,GM).
grow([],       _  ,[]).
grow([[Self]],Grid,GridO):- !, call(Self,Grid,GridO).
grow([Row|Rows],Grid,G1GridO):- grow_row(Row,Grid,G1), grow(Rows,Grid,GridO),my_append(G1,GridO,G1GridO).

no_run_dsl(GridO,_Self,GridO).

largest_indiv(I,O):- into_group(I,M),I\=@=M,!,largest_indiv(M,O).
largest_indiv(Grid,[Points]):- compute_shared_indivs(Grid,Is),largest_first(Is,[Points|_]).

smallest_indiv(I,O):- into_group(I,M),I\=@=M,!,smallest_indiv(M,O).
smallest_indiv(Grid,Points):- compute_shared_indivs(Grid,Iss),largest_first(Iss,Is),remove_bgs(Is,IndvL,_BGIndvS),last(IndvL,Points).

background_indiv(I,O):- into_group(I,M),I\=@=M,!,background_indiv(M,O).
background_indiv(Grid,BGIndvS):-  compute_shared_indivs(Grid,Is),remove_bgs(Is,_IndvL,BGIndvS).

/*
largest_indiv(Grid,Points):- compute_shared_indivs(Grid,[Points|_]).
largest_indiv(Points,Grid,Grid):- largest_indiv(Grid,Points).

smallest_indiv(Grid,Points):- compute_shared_indivs(Grid,Is),last(Is,Points),points_to_grid(Points,Points).
smallest_indiv(Points,Grid,Grid):- smallest_indiv(Grid,Points).
*/

set_color(Color,Next,_ColorTrail,G0,G9):- add_global_points(Color,Next,G0,G9).


shoot_ray(ColorTrail,Origin,Dir,G0,G9):- color(Origin,Color),shoot_ray(ColorTrail,Origin,Dir,Color,inf,1,0,0,[],G0,G9).
shoot_ray(_ColorTrail,_Origin,_Dir,_Color,0,_Width,_WidenSpeed,_Skip,_ColorRules,G9,G9):- !.
shoot_ray(ColorTrail,Origin,Dir,Color,Fuel,Width,WidenSpeed,Skip,ColorRules,G0,G9):- 
  hv_point(_H,_V,Origin),
  is_adjacent_point(Origin,Dir,Next),
  set_color(Color,Next,ColorTrail,G0,G1),
    Width2 is Width + WidenSpeed,
    FuelReduced is Fuel-1,
    shoot_ray(ColorTrail,Origin,Dir,Color,FuelReduced,Width2,WidenSpeed,Skip,ColorRules,G1,G9).


%fill_from_point(Point,Color,DirsAllow):-

trim_to_square(G0,G9):- get_bgc(BG),
  into_grid(G0,G),
  trim_unused_vert_square(BG,G,G1),
  trim_unused_vert_square(BG,G1,G2),
  trim_unused_vert_square(BG,G2,G3),
  trim_unused_vert_square(BG,G3,G9).

  trim_unused_vert_square(_,[],[]).
  %trim_unused_vert_square(_,_,GridO,GridO):-grid_size(GridO,H,W),H=W,!.
  trim_unused_vert_square(BG,[Row|Grid],Grid90):- maplist(is_bg_or_var(BG),Row),rot90(Grid,[Col|Grid90]),
     maplist(is_bg_or_var(BG),Col).
  trim_unused_vert_square(_,G1,Grid90):- rot90(G1,Grid90).


 
trim_to_rect(G0,G9):- into_grid(G0,G),trim_grid_to_rect(G,G9).
trim_grid_to_rect(G,G9):-
 get_bgc(BG),
 trim_unused_vert(BG,G,G1),rot90(G1,G2),trim_unused_vert(BG,G2,G3),rot270(G3,G8),
 remove_color(BG,G8,G9).

  trim_unused_vert(_,[],[]):-!.
  trim_unused_vert(BG,[Row|Grid],GridO):- maplist(is_bg_or_var(BG),Row),!,trim_unused_vert(BG,Grid,GridO).
  trim_unused_vert(BG,GridR,GridO):- append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),!,trim_unused_vert(BG,Grid,GridO).
  trim_unused_vert(_,G,G).

%:- luser_setval(grid_bgc,8).

if_bgc_then_int(X,C,B,A):- \+compound(B),B\==[],is_bg_or_var(X,B), A=C, !.
set_bg(C0,Grid,GridO):- color_code(C0,CC),  luser_setval(grid_bgc,X), get_bgc(X),
  is_grid(Grid),!,grid_color_code(CC,GC), map_pred(if_bgc_then_int(X,GC), Grid, GridO),!.
set_bg(C0,Grid,GridO):- color_code(C0,C),  luser_setval(grid_bgc,X), get_bgc(X),map_pred(if_bgc_then(X,C), Grid, GridO),!.
  if_bgc_then(X,C,B,A):- \+compound(B),is_bg_or_var(X,B), A=C, !.

shave_away_1s(Grid,GridO):- compute_shared_indivs(Grid,Is), include(\=([_,_|_]),Is,I1s), remove_global_points(I1s,Grid,GridO).

replace_obj(Obj,Obj1,In,Out):- remove_obj(Obj,In,Mid),add_obj(Obj1,Mid,Out).

remove_obj(Obj,In,Out):- globalpoints(Obj,Points),remove_global_points(Points,In,Out).

add_obj(Obj,In,Out):- globalpoints(Obj,Points),set_local_points(Points,In,Out).



remove_global_points([],Grid,Grid):- !.
remove_global_points(Obj,Grid,GridO):- is_grid(Obj),!, localpoints_include_bg(Obj,Points),remove_global_cpoints(Points,Grid,GridO).
remove_global_points([H|T],Grid,GridO):- is_points_list([H|T]), !, remove_global_cpoints([H|T],Grid,GridO).
remove_global_points(Obj,Grid,GridO):- is_group(Obj), globalpoints(Obj,Points),remove_global_cpoints(Points,Grid,GridO).
remove_global_points(Obj,Grid,GridO):- is_object(Obj), globalpoints(Obj,Points),remove_global_cpoints(Points,Grid,GridO).
remove_global_points([H|T],Grid,GridO):- !, remove_global_points(H,Grid,GridM),remove_global_points(T,GridM,GridO).
remove_global_points(Point,Grid,GridO):- remove_global_cpoints(Point,Grid,GridO).

remove_global_cpoints([],Grid,Grid):- !.
remove_global_cpoints([H|T],Grid,GridO):- !, remove_global_cpoints(H,Grid,GridM),remove_global_cpoints(T,GridM,GridO).

remove_global_cpoints(CPoint,G,GO):- is_points_list(G), point_to_hvc(CPoint,H,V,OldC),hv_point(H,V,Point), !, 
  replace_in_points(Point,_NewC,OldC,G,GO).

remove_global_cpoints(Point,Grid,GridO):- point_to_hvc(Point,H,V,IfSame),                                                                     
                               get_color_at(H,V,Grid,Old), 
                               (same_color(IfSame,Old) -> (get_bgc(New),replace_global_hvc_point(H,V,New,Old,Grid,GridO));
                                  Grid=GridO).

%remove_global_cpoints(Point,Grid,GridO):- set_local _points(,Point,Grid,GridO).
remove_global_cpoints(Point,Grid,Grid):-  nop(wdmsg(warn(skip(remove_global_points(Point))))).

same_color(IfSame,Old):- \+ \+ IfSame = Old.


pred_global_points(Pred7,Obj,Grid,GridO):- pred_global_points(Pred7,fg,Obj,Grid,GridO).
pred_global_points(_Pred7,_Color,[],Grid,Grid):- !.
pred_global_points(Pred7,Color,Obj,Grid,GridO):- is_grid(Obj),!, localpoints_include_bg(Obj,Points),pred_global_cpoints(Pred7,Color,Points,Grid,GridO).
pred_global_points(Pred7,Color,[H|T],Grid,GridO):- is_points_list([H|T]), !, pred_global_cpoints(Pred7,Color,[H|T],Grid,GridO).
pred_global_points(Pred7,Color,Obj,Grid,GridO):- is_group(Obj), globalpoints(Obj,Points),pred_global_cpoints(Pred7,Color,Points,Grid,GridO).
pred_global_points(Pred7,Color,Obj,Grid,GridO):- is_object(Obj), globalpoints(Obj,Points),pred_global_cpoints(Pred7,Color,Points,Grid,GridO).
pred_global_points(Pred7,Color,[H|T],Grid,GridO):- !, pred_global_points(Pred7,Color,H,Grid,GridM),pred_global_points(Pred7,Color,T,GridM,GridO).
pred_global_points(Pred7,Color,Point,Grid,GridO):- pred_global_cpoints(Pred7,Color,Point,Grid,GridO).
pred_global_cpoints(_Pred7,_Color,[],Grid,Grid):- !.
pred_global_cpoints(Pred7,Color,[H|T],Grid,GridO):- !, pred_global_cpoints(Pred7,Color,H,Grid,GridM),pred_global_cpoints(Pred7,Color,T,GridM,GridO).
pred_global_cpoints(Pred7,FG,Point,Grid,GridO):- FG == fg, point_to_hvc(Point,H,V,C), !, hv_c_value_or(Grid,Old,H,V,_), call(Pred7,H,V,C,Old,Grid,GridO).
pred_global_cpoints(Pred7,Color,Point,Grid,GridO):- point_to_hvc(Point, H,V,_),  hv_c_value_or(Grid,Old,H,V,_),  call(Pred7,H,V,Color,Old,Grid,GridO).
%pred_global_cpo ints(Pred7,Color,Point,Grid,GridO):- set_local _points(Pred7,,Point,Grid,GridO).
pred_global_cpoints(Pred7,Color,Point,Grid,Grid):-  nop(wdmsg(warn(skip(pred_global_points(Pred7,Color,Point))))).



add_global_points(Obj,Grid,GridO):-
 get_fg_label(FGL),
 add_global_points(FGL,Obj,Grid,GridO).

add_global_points(_Color,[],Grid,Grid):- !.
add_global_points(Color,Obj,Grid,GridO):- is_grid(Obj),!, localpoints_include_bg(Obj,Points),add_global_cpoints(Color,Points,Grid,GridO).
add_global_points(Color,[H|T],Grid,GridO):- is_points_list([H|T]), !, add_global_cpoints(Color,[H|T],Grid,GridO).
add_global_points(Color,Obj,Grid,GridO):- is_group(Obj), globalpoints(Obj,Points),add_global_cpoints(Color,Points,Grid,GridO).
add_global_points(Color,Obj,Grid,GridO):- is_object(Obj), globalpoints(Obj,Points),add_global_cpoints(Color,Points,Grid,GridO).
add_global_points(Color,[H|T],Grid,GridO):- !, add_global_points(Color,H,Grid,GridM),add_global_points(Color,T,GridM,GridO).
add_global_points(Color,Point,Grid,GridO):- add_global_cpoints(Color,Point,Grid,GridO).

add_global_cpoints(_Color,[],Grid,Grid):- !.
add_global_cpoints(Color,[H|T],Grid,GridO):- !, add_global_cpoints(Color,H,Grid,GridM),add_global_cpoints(Color,T,GridM,GridO).
add_global_cpoints(FG,Point,Grid,GridO):- get_fg_label(FGL), FG == FGL, point_to_hvc(Point, H,V,C), !, replace_global_hvc_point(H,V,C,_,Grid,GridO).
add_global_cpoints(Color,Point,Grid,GridO):- point_to_hvc(Point, H,V,_),   replace_global_hvc_point(H,V,Color,_,Grid,GridO).
%add_global_cpoints(Color,Po int,Grid,GridO):- set_local _points(,Point,Grid,GridO).
add_global_cpoints(Color,Point,Grid,Grid):-  nop(wdmsg(warn(skip(add_global_points(Color,Point))))).



/*
set_local_ po ints(Points,Grid,GridO):-  set_loca l_points(fg,Points,Grid,GridO).
set_local _points(C,[H|T],Grid,GridO):- !, set_local_points(C,H,Grid,GridM),set_loc al_points(C,T,GridM,GridO).
set_loca l_points(_,[],Grid,Grid):-!.
set_local_po ints(C,Point,Grid,GridO):- point_t o_hvc(H,V,Old,Point), replace_loca l_point(H,V,C,Old,Grid,GridO).
*/

%set_local_points([],Grid,Grid):- !.
%set_local_points([H|T],Grid,GridO):- !, set_local_points(H,Grid,GridM),set_local_points(T,GridM,GridO).
set_local_points(Point,Grid,GridO):- replace_local_points(Point,_AnyOldColor,Grid,GridO),!.
set_local_points(Point,Grid,GridO):- arcST,ignore((rrtrace((replace_local_points(Point,_AnyOldColor,Grid,GridO))),break)),!.
%set_local_points(Point,Grid,GridO):- set_local_points(,Point,Grid,GridO).
%set_local_points(Point,Grid,Grid):-  wdmsg(warn(skip(set_local_points(Point)))).

replace_grid_point(H,V,NewC,OldC,Grid,Grid):- %copy_term(OldC,OldCC),
  nth1(V,Grid,Row),ignore((nth1(H,Row,OldCC),!, OldCC\==NewC, ( \+ OldCC \= OldC), nb_set_nth1(H,Row,NewC))),!,
   nb_set_nth1(V,Grid,Row).
replace_grid_point(_H,_V,_NewC,_OldC,Grid,Grid):-!.


nb_set_nth1(1,Row,C):- !, (Row==[]->true; nb_setarg(1,Row,C)).
%nb_set_nth1(1,Row,C):- !, nb_setarg(1,Row,C).
nb_set_nth1(N,[_|Row],C):- Nm1 is N -1, nb_set_nth1(Nm1,Row,C).

set_all_fg(C0,Grid,GridO):- color_code(C0,C),get_bgc(X),map_pred(if_not_bgc_then(X,C), Grid, GridO).
if_not_bgc_then(X,C,B,A):- is_color_dat(B), \+ is_bg_or_var(X,B), A=C, !.


learn_mapping_stateful(In,Out):- get_bgc(BG),
   must_det_ll((
   maplist(unbind_color(BG),[In,Out],[In1,Out1]),
   maplist(colors_to_vars,[In1,Out1],[In2,Out2]), 
   In2=Out2,
   asserta_new(backfill(Out2)))).

apply_mapping_stateful(Grid,G):- into_grid(G,Grid),unbind_color(0,Grid,GridO),ignore(backfill_vars(GridO)).


compute_max_color(Color1,Grid,Grid):- colors_count_no_black(Grid,[cc(Color1,_)|_]).
compute_next_color(Color1,Grid,Grid):- colors_count_no_black(Grid,[_,cc(Color1,_)|_]).

subst_color(Color1,Color2,Grid,NewGrid):- 
  quietly((
   color_code(Color1,Num1),
   color_code(Color2,Num2),
   subst001(Grid,Num1,Num2,NewGrid))).

equal_color(Color,Color).

remove_color(Color,In0,Out):-
 duplicate_term(In0,In),
 copy_term(In0,In),
 remove_color0(Color,In,Out0),!,
  duplicate_term(Out0,Out),
  copy_term(Out0,Out),
 nop((print_side_by_side(silver,In,remove_color,_,Out,c(Color)))),!.


remove_color0(Color,Grid,NewGrid):-
   nth1(_,Grid,Row,MidGrid),maplist(==(Color),Row),!,
   remove_color0(Color,MidGrid,NewGrid).
remove_color0(Color,G,NewGrid):-
   duplicate_term(G,G0),
   rot90(G0,Grid),
   nth1(_,Grid,Row,Rest),maplist(==(Color),Row),
   duplicate_term(Rest,Rest0),
   rot270(Rest0,MidGrid),!,
   remove_color0(Color,MidGrid,NewGrid).
remove_color0(Color,Grid,NewGrid):-
   get_bgc(Cell), subst_color(Color,Cell,Grid,NewGrid),
   nop(set_vm(grid,NewGrid)).


blank_color(Color1,Grid,NewGrid):- get_bgc(Cell), subst_color(Color1,Cell,Grid,NewGrid).
swap_colors(Color1,Color2,Grid,NewGrid):- subst_color(Color1,Swap1,Grid,MGrid),
                                          subst_color(Color2,Color1,MGrid,NewGrid),
                                          color_code(Color2,Swap1).


do_set_all_fg_colors(Color,I,O):- \+ compound(I),is_fg_color(I),O=Color.
set_all_fg_colors(Color,Grid,NewGrid):- is_grid(Grid),!,mapgrid(set_all_fg_colors(Color),Grid,NewGrid).
set_all_fg_colors(Color,List,NewList):- is_list(List),!,maplist(set_all_fg_colors(Color),List,NewList).

set_all_fg_colors(Color,Obj,NewObj):- is_object(Obj),!,unique_colors(Obj,Colors),include(is_fg_color,Colors,FGCs),
  findall(FGC-ColorCopy,(member(FGC,FGCs),copy_term(Color,ColorCopy)),Replaces), subst_1L(Replaces,Obj,NewObj).

set_all_fg_colors(Color,Grid,NewGrid):- map_pred(do_set_all_fg_colors(Color),Grid,NewGrid).


do_set_all_bg_colors(Color,I,O):- \+ compound(I),is_bg_color(I),O=Color.
set_all_bg_colors(Color,Grid,NewGrid):- is_grid(Grid),!,mapgrid(set_all_bg_colors(Color),Grid,NewGrid).
set_all_bg_colors(Color,Grid,NewGrid):- is_list(Grid),!,maplist(set_all_bg_colors(Color),Grid,NewGrid).
set_all_bg_colors(Color,Grid,NewGrid):- map_pred(do_set_all_bg_colors(Color),Grid,NewGrid).


%do_set_all_fg_colors(Color,CPoint,NewCPoint):- is_cpoint(CPoint),CPoint=C-Point,hv_point(_,_,Point),is_fg_color(C),NewCPoint=Color-Point.

blur(Op,G0,GG):- into_grid(G0,G),call(Op,G,GGG),replace_local_points(GGG,black,G,GG).

backfill_vars(GridO):- clause(backfill(GridO),true).

unbind_color_whole(Num1,Var,_):- (plain_var(Var);plain_var(Num1)),!,Num1==Var.
unbind_color_whole(Num1,Grid,GridO):- is_grid(Grid),!,mapgrid(unbind_color_whole(Num1),Grid,GridO).
unbind_color_whole(Num1,Grid,GridO):- is_list(Grid),!,maplist(unbind_color_whole(Num1),Grid,GridO).
unbind_color_whole(Num1,Num2,_):- \+ compound(Num2),!, Num1=Num2.
unbind_color_whole(Num1,_-Num1,_).

unbind_color(Color1,Grid,GridO):- is_grid(Grid),!,grid_color_code(Color1,Num1),unbind_color0(Num1,Grid,GridO).
unbind_color(Color1,Grid,GridO):- color_code(Color1,Num1),subst001(Grid,Num1,_,GridO).

unbind_color0(Num1,Grid,GridO):- is_list(Grid),!,maplist(unbind_color0(Num1),Grid,GridO).
unbind_color0(Num1,Num1,_):-!.
unbind_color0(_,Num1,Num1).

colors_to_vars(G,GridNew):- into_grid(G,Grid),G\=@=Grid,!,colors_to_vars(Grid,GridNew).
colors_to_vars(Grid,GridO):- colors_to_vars(_,_,Grid,GridO).
colors_to_vars(Vars,Grid,GridO):-  colors_to_vars(_Colors,Vars,Grid,GridO).
colors_to_vars(Colors,Vars,Grid,GridO):- (plain_var(Colors)->unique_colors(Grid,Colors);true),!,
  length(Colors,Len),length(Vars,Len),
   subst_cvars(Colors,Vars,Grid,GridO),
   pt(Grid-->GridO).

subst_cvars([],[],A,A):-!. 
subst_cvars([F|FF],[R|RR],S,D):- !, freeze(R,(\=(R,_-_))),subst001(S,F,R,M), subst_cvars(FF,RR,M,D).

/*
colors_to_vars(B,A,Grid,GridO):- is_list(Grid),!,maplist(colors_to_vars(B,A),Grid,GridO).
colors_to_vars(F,R,S,D):- nth1(N,F,E),E==S,nth1(N,R,D),!.
colors_to_vars(_,_,V,V).
*/

%add_borders(C,G,GridNew):- G\=@=Grid,!,add_borders(C,Grid,GridNew).
/*
add_borders(Color,Grid,GridO):- 
 grid_size(Grid,H,V),
 get_training(VM),
 wno(
  (run_dsl(VM,[replace_row_e(1,Color),
  replace_row_e(V,Color),
  replace_col_e(1,Color),
  replace_col_e(H,Color)],Grid,GridO))),!.
*/
add_borders(Color,Grid,GridO):- 
 grid_size(Grid,H,V),
  replace_row_e(1,Color,Grid,Grid0),
  replace_row_e(V,Color,Grid0,Grid1),
  replace_col_e(1,Color,Grid1,Grid2),
  replace_col_e(H,Color,Grid2,GridO),!.



cls_with(Color1,G,Grid):- into_grid(G,Old),grid_color_code(Color1,Num1),cls_with_0(Num1,Old,Grid),!.
cls_with_0(Color1,Old,Grid):- is_list(Old),!,maplist(cls_with_0(Color1),Old,Grid).
cls_with_0(Color1,_,Color1).

get_colums(G,GridNew):- into_grid(G,Grid),G\=@=Grid,!,get_colums(Grid,GridNew).
get_colums(Grid,Cols):- Grid = [Row|_], length(Row,Width),
  get_colum_l(Width,Grid,Cols).

get_colum_l(0,_,[]):-!.
get_colum_l(Width,Grid,[Col|Cols]):- 
  get_colum(Width,Grid,Col),
  Wm1 is Width -1, 
  get_colum_l(Wm1,Grid,Cols).

get_colum(G,GridNew):- into_grid(G,Grid),G\=@=Grid,!,get_colum(Grid,GridNew).
get_colum(N,Grid,Col):- maplist(nth1(N),Grid,Col).

make_var_grid(H,V,G):- make_grid(H,V,G),numbervars(G,0,_N).

replace_row(N,Row,Grid,NewGrid):- grid_size(Grid,H,V), replace_row(N,Row,Grid,H,V,NewGrid).
replace_row(N,Row,Grid,H,V,NewGrid):- N<0, NewN is V + N+1,!,replace_row(NewN,Row,Grid,H,V,NewGrid).
replace_row(N,Row,Grid,_,_,NewGrid):- set_nth1(N,Grid,Row,NewGrid).

replace_row_e(N,E,Grid,NewGrid):- grid_size(Grid,H,V), make_list(E,H,Row), replace_row(N,Row,Grid,H,V,NewGrid),!.
replace_col_e(N,E,Grid,NewGrid):- grid_size(Grid,H,V), make_list(E,V,Col), replace_col(N,Col,Grid,H,V,NewGrid).

replace_col(N,Col,Grid,NewGrid):- grid_size(Grid,H,V), replace_col(N,Col,Grid,H,V,NewGrid).
replace_col(N,Col,Grid,H,V,NewGrid):- N<0, NewN is H + N+1,!,replace_col(NewN,Col,Grid,H,V,NewGrid).

replace_col(N,Col,Grid,_,V,NewGrid):- Nm1 is N - 1, length(Col,V),maplist(replace_col_at_0(Nm1),Col,Grid,NewGrid).

replace_col_at_0(N,Col,Row,NewRow):- length(Left,N),my_append(Left,[_|Right],Row),my_append(Left,[Col|Right],NewRow).


get_surround_3x3(Grid,H,V,Result):-
  surround_3x3(Template),maplist(get_dir_color(Grid,H,V),Template,Result).

get_dir_color(Grid,H,V,Dir,C):- move_dir(1,H,V,Dir,1,1,NX,NY), hv_c_value(Grid,C,NX,NY).


% Random Non Blk Eles
first_color(Grid1,C1):- sub_term(C1,Grid1),is_color(C1), \+ is_bg_color(C1).

% Grid v_hv/resize
make_lengths(N,L):- length(L,N).

get_inf(30).
get_neg_inf(X):- get_inf(Inf), X is 0-Inf.


points_range(Points,LoH,LoV,HiH,HiV,H,V):- get_neg_inf(NInf), get_inf(Inf), 
  calc_range(Inf,Inf,NInf,NInf,NInf,NInf,Points,LoH,LoV,HiH,HiV,H,V),!.


points_range2(Points,offset_ranges(LoH,LoV,HiH,HiV,H,V)):- get_inf(Inf),  get_neg_inf(NInf),
  calc_range(Inf,Inf,NInf,NInf,NInf,NInf,Points,LoH,LoV,HiH,HiV,H,V).
% v_hv(Points,v_hv(H,V)):- points_range(Points,_LoH,_LoV,_HiH,_HiV,H,V).

close_color(brown,orange).
close_color(green,cyan).

grid_size_term(I,size(X,Y)):- grid_size(I,X,Y),!.

%grid_size(Points,H,V):- is_map(Points),!,Points.grid_size=grid_size(H,V).
grid_size(NIL,1,1):- NIL==[],!.
grid_size(Points,H,V):- is_points_list(Points),!,points_range(Points,_LoH,_LoV,_HiH,_HiV,H,V),!.
grid_size(ID,H,V):- is_grid_size(ID,H,V),!.
grid_size(G,H,V):- is_graid(G,GG),!, grid_size(GG,H,V).
grid_size(G,H,V):- is_map(G),H = G.h,V = G.v,!,grid_size_nd(G,H,V),!.
grid_size(G,H,V):- is_grid(G),!,grid_size_nd(G,H,V),!.
grid_size(G,X,Y):- is_group(G),!,mapgroup(grid_size_term,G,Offsets),sort(Offsets,HighToLow),last(HighToLow,size(X,Y)).
grid_size(I,X,Y):- is_object(I),indv_props(I,L),(member(grid_size(X,Y),L);member(v_hv(X,Y),L)),!.
%grid_size(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.
%grid_size(G,H,V):- quietly(is_object(G)), !, v_hv(G,H,V).
%grid_size([G|G],H,V):- is_list(G), length(G,H),length([G|G],V),!.
grid_size(Points,H,V):- pmember(grid_size(H,V),Points),ground(H-V),!.
%grid_size([G|G],H,V):- is_list(G),is_list(G), grid_size_nd([G|G],H,V),!.
%grid_size(O,_,_):- trace_or_throw(no_grid_size(O)).
grid_size(_,30,30).

:- system:import(grid_size/3).
:- ansi_term:import(grid_size/3).

calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,Var,WLoH,WLoV,WHiH,WHiV,WH,WV):- plain_var(Var),!.
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,grid_size(IH,IV),WLoH,WLoV,WHiH,WHiV,H,V):- !,
  max_min(WV,IV,V,_),max_min(WH,IH,H,_).
%calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,v_hv(IH,IV),WLoH,WLoV,WHiH,WHiV,H,V):- !,
%  max_min(WV,IV,V,_),max_min(WH,IH,H,_).
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,[E|L],LoH,LoV,HiH,HiV,H,V):- !,
  calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,E,MLoH,MLoV,MHiH,MHiV,MH,MV),
  calc_range(MLoH,MLoV,MHiH,MHiV,MH,MV,L,LoH,LoV,HiH,HiV,H,V).
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,[],WLoH,WLoV,WHiH,WHiV,WH,WV).
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,offset_ranges(ILoH,ILoV,IHiH,IHiV,IH,IV),LoH,LoV,HiH,HiV,H,V):- 
  max_min(WLoV,ILoV,_,LoV),max_min(WHiV,IHiV,HiV,_),max_min(WV,IV,V,_),
  max_min(WLoH,ILoH,_,LoH),max_min(WHiH,IHiH,HiH,_),max_min(WH,IH,H,_),!.
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,Point,LoH,LoV,HiH,HiV,H,V):- 
  point_to_hvc(Point,IH,IV,C),nonvar_or_ci(C), !,
  max_min(WLoV,IV,_,LoV),max_min(WHiV,IV,HiV,_),max_min(HiV,WV,V,_),
  max_min(WLoH,IH,_,LoH),max_min(WHiH,IH,HiH,_),max_min(HiH,WH,H,_),!.

calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,_,WLoH,WLoV,WHiH,WHiV,WH,WV):- !.


grid_size_nd(L,_,_):- \+ var(L), \+ is_grid(L), !, fail.
grid_size_nd([G|Grid],H,V):- is_list(G), length(G,H),length([G|Grid],V),!.
grid_size_nd([C,R|Rows],H,V):- 
   (plain_var(Rows)->between(2,32,V);!), 
   length([C,R|Rows],V),
   (plain_var(R)->between(1,32,H);true), 
   length(R,H),
   (is_list(C)->true;(length(C,H),maplist(make_lengths(H),Rows))).
grid_size_nd([L],H,(1)):- (plain_var(L)->between(1,32,H);true), length(L,H).


%points_to_grid(Points,Grid):- is_grid(Points),Points=Grid,!.
points_to_grid(Points,Grid):- is_grid(Points),!,must_det_ll(Grid=Points).
points_to_grid(Points,Grid):- is_points_list(Points), !, must_det_ll(grid_size(Points,H,V)), !, points_to_grid(H,V,Points,Grid).
points_to_grid(Points,Grid):- must_det_ll(grid_size(Points,H,V)), !, points_to_grid(H,V,Points,Grid).
%points_to_grid([Points|More],Grid):- is_grid(Points),grid_size(Points,H,V),duplicate_term(Points,Grid),calc_add_points(H,V,Grid,More),!.
%points_to_grid(Points,Grid):- is_points_list(Points),!,points_to_grid(30,30,Points,Grid).

points_to_grid(H,V,Points,Grid):- var(H),var(V),must_det_ll(grid_size(Points,H,V)),!,points_to_grid(H,V,Points,Grid).
points_to_grid(H,V,Points,Grid):- make_grid(H,V,Grid), calc_add_points(1,1,Grid,Points).

calc_add_points(OH,OV,Grid,Grid):- is_grid(Grid),!,globalpoints_maybe_bg(Grid,Points),!,maplist(calc_add_points(OH,OV,Grid),Points).
calc_add_points(OH,OV,Grid,Group):- is_group(Group),!,mapgroup(calc_add_points(OH,OV,Grid),Group).
calc_add_points(OH,OV,Grid,Points):- is_object(Obj),!,globalpoints_maybe_bg(Obj,Points),!,maplist(calc_add_points(OH,OV,Grid),Points).
calc_add_points(OH,OV,Grid,Points):- is_list(Points),!,maplist(calc_add_points(OH,OV,Grid),Points).
%calc_add_points(OH,OV,_,Obj):- plain_var(Obj),arcST,trace_or_throw(var_calc_add_points(OH,OV,Obj)).
calc_add_points(OH,OV,Grid,ColorInt):- integer(ColorInt), color_name(ColorInt,Color),!, add_h_v_c(Grid,OH,OV,Color).
calc_add_points(OH,OV,Grid,Color):- is_color(Color),!, add_h_v_c(Grid,OH,OV,Color).
calc_add_points(OH,OV,Grid,Point):- point_to_hvc(Point,H,V,C),!, HH is H -OH +1, VV is V - OV +1,  add_h_v_c(Grid,HH,VV,C).
calc_add_points(OH,OV,Grid,_-Point):- point_to_hvc(Point,H,V,C),!, HH is H -OH +1, VV is V - OV +1,  add_h_v_c(Grid,HH,VV,C).
calc_add_points(OH,OV,Grid,Obj):- trace,globalpoints(Obj,Points),!,maplist(calc_add_points(OH,OV,Grid),Points).
%calc_add_points(_OH,_OV,_,obj(_)):-

%add_h_v_c(Grid,H,V,C):- plain_var(C),!,nop(add_h_v_c(Grid,H,V,C)).
add_h_v_c(Grid,H,V,C):- hv_c_value(Grid,Was,H,V),
 (Was=C->true;(nth1(V,Grid,Row),nb_set_nth1(H,Row,C),!,nb_set_nth1(V,Grid,Row))).

copy_cells(B,A,H,HH):- call(B,H),!,call(A,HH).
copy_cells(_,_,H,H):- \+ is_list(H),!.
copy_cells(_,_,[],[]):-!. 
copy_cells(B,A,[H|T],[HH|TT]):-!, copy_cells(B,A,H,HH), copy_cells(B,A,T,TT).


:- fixup_exports.

