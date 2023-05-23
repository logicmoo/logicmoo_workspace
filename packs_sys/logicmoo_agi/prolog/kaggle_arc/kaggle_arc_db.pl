/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

%:- dynamic(row_mem/34).
:- dynamic(gid_glyph_oid/3).

%point_atoms :- false.
point_atoms :- true.

:- dynamic(cindv/3).
:- dynamic(cindv/4).
:- dynamic(cindv/5).
:- dynamic(cindv/6).
:- multifile(cindv/3).
:- multifile(cindv/4).
:- multifile(cindv/5).
:- multifile(cindv/6).

:- dynamic(oid_to_global_grid/2).

:- dynamic(zmem/3).

%show_time_gt_duration(Goal):- show_time_gt_duration(0.23,Goal,true). 
% 793,184,238 inferences, 160.894 CPU in 160.897 seconds (100% CPU, 4929859 Lips)
show_time_gt_duration(Timespan,Goal):- show_time_gt_duration(Timespan,Goal,true). 
show_time_gt_duration(Timespan,Goal,Goal2):- 
  get_time(Now),setup_call_cleanup(true,Goal,maybe_report_time(Timespan,Now,Goal,Goal2)).

maybe_report_time(Timespan, Was,Goal,Goal2):- get_time(Now), Elapse is Now - Was,
  ignore((Elapse>Timespan , 
    pp(show_time_gt_duration(Elapse>Timespan,Goal)),
    once(Goal2 ),
    pp(yellow,maybe_report_time(Elapse>Timespan,Goal)))).

neg_h_v_area(size2D(H,V),VAL):- NArea is - (H * V),  max_min(H,V,Hi,_Lo), DifHV is - abs(H-V)*Hi, VAL is NArea+DifHV.

:- abolish(l_s_4sides,2).
:- abolish(s_l_4sides,2).

:- dynamic(l_s_4sides/2).
:- dynamic(s_l_4sides/2).

predsort_using_only(P2,List,Sorted):- predsort(using_compare(P2),List,Sorted).
using_compare(C,R,A,B):- (A==B-> R=(=) ; must_det_ll((call(C,A,AA),call(C,B,BB),!,compare(R,AA,BB)))),!.
predsort_on(P2,List,Sorted):- predsort(sort_on(P2),List,Sorted).
sort_on(C,R,A,B):- (A==B-> R= (=) ; must_det_ll((call(C,A,AA),call(C,B,BB),!,compare(R,AA+A,BB+B)))),!.
variants_equal(R,A,B):- first_equals(=@=,compare,R,A,B).
first_equals(P2,P3,R,A,B):- call(P2,A,B) -> R= (=) ; call(P3,R,A,B).
:- 
   findall(size2D(HV,HV),between(1,32,HV),SizesSquareS),
   findall(size2D(H,V),(between(1,32,H),between(1,32,V),H\=V),SizesRect),
   predsort_on(neg_h_v_area,SizesRect,SizesRectS),
   %reverse(SizesSquareS,SizesSquareR), reverse(SizesRectS,SizesRectR),
  % list_to_set([size2D(3,3),size2D(2,2)|SizesSquareS],Sizes_L_S),
   append(SizesSquareS,SizesRectS,AllSizes),   
   predsort_on(neg_h_v_area,AllSizes,Sizes_L_S),

   forall(member(size2D(H,V),Sizes_L_S),assertz(l_s_4sides(H,V))),
   reverse(Sizes_L_S,Sizes_S_L),
   forall(member(size2D(H,V),Sizes_S_L),assertz(s_l_4sides(H,V))),
   !.

erase_grid(GID):-
  %id_grid_cells(GID,Grid)
  retractall(is_grid_size(GID,_,_)),
  retractall(nmem(GID,_,_)),
  retractall(dmem(GID,_, _,_,_,_)),
  retractall(smem(GID,_, _)),
  retractall(cmem(GID,_,_)),
  retractall(cmem_hv(GID,_,_,_)),
  erase_objects(GID).

erase_objects(GID):-
  forall(gid_glyph_oid(GID,_,OID),erase_obj(OID)),
  forall(gid_type_oid(GID,_,OID),erase_obj(OID)),
  retractall(omem(GID,_,_)),
  retractall(gid_glyph_oid(GID,_,_)),
  retractall(gid_type_oid(GID,_,_)).

erase_obj(OID):-
  retractall(omem(_,OID,_)),
  retractall(cindv(OID,_,_)),
  retractall(cindv(OID,_,_,_)),
  retractall(cindv(OID,_,_,_,_)),
  retractall(gid_glyph_oid(GID,_,OID)),
  retractall(gid_type_oid(GID,_,OID)).

assert_id_cells(ID,Points):- my_maplist(assert_id_cell(ID),Points).
assert_id_cell(ID,-(C,HV)):- assert(cmem(ID,HV,C)).


:- dynamic(is_grid_size/3).
% Grid to_fast_workspace

lcmem(OID,LHV,C):-  
  cindv(OID,loc2P,Loc2P),gid_type_oid(GID,_,OID),
  point_plus(Loc2P,LHV,GHV),
  cmem(GID,GHV,C).


%assert_id_grid_cells(Grid):- is_grid(Grid),grid_to_gid(Grid,GID),!,assert_id_grid_cells(GID,Grid).
%assert_id_grid_cells(GID):- oid_to_gridoid(GID,Grid), assert_id_grid_cells(GID,Grid).

%ensure_gid(_,_):- \+ luser_getval(generate_gids,true),!,fail.
ensure_gid(Grid,GID):- atom(Grid),!,GID=Grid. %,!,assert_id_grid_cells(GID,_),!. 
ensure_gid(Grid,GID):- grid_to_gid(Grid,GID),assert_id_grid_cells(GID,Grid),!.
  
/*
assert_id_grid_cells2(ID,_SH,_SV,Grid):- is_grid(Grid),!,
 forall(nth1(N,Grid,Row),
   forall(list_to_row_mem(ID,N,Row,Mem),arc_assert(Mem))).
*/
assert_id_grid_cells(GID, Grid):- var(Grid),!,oid_to_gridoid(GID,Grid),!,assert_id_grid_cells(GID,Grid).
assert_id_grid_cells(GID, Grid):- var(GID),grid_to_gid(Grid,GID),!,assert_id_grid_cells(GID,Grid).
assert_id_grid_cells(GID,_Grid):- nonvar(GID), is_grid_size(GID,_,_),!.
assert_id_grid_cells(GID, Grid):- 
 %throw(all_in_emem(assert_id_grid_cells(GID,Grid))),
   grid_size(Grid,SH,SV),
   %((cmem(GID,_,_);gid_glyph_oid(GID,_,_))-> erase_grid(GID) ; true),
   retractall(is_grid_size(GID,_,_)),
   %retractall(is_cmem(GID,_,_)),retractall(is_cmem_hv(GID,_,_,_)),
   retractall(cmem(GID,_,_)),retractall(cmem_hv(GID,_,_,_)),
   assert(is_grid_size(GID,SH,SV)),
   assert_grid_cmem(GID,SH,SV,Grid),
   remake_texture(GID),
   %show_grid_texture(GID),
   cache_grid_objs(GID).
   %show_grid_objs(GID).

ensure_cmem(GID):- \+ \+ cmem(GID,_,_),!.
ensure_cmem(GID):- 
   grid_size(GID,SH,SV),
   oid_to_gridoid(GID,Grid),
   assert_grid_cmem(GID,SH,SV,Grid).

assert_grid_cmem(GID,SH,SV,Grid):- var(Grid),!,ignore((ground(SH+SV),assert_if_new(is_grid_size(GID,SH,SV)))).
assert_grid_cmem(ID,SH,SV,Grid):-
   my_assertion(is_grid(Grid)),
   forall(between(1,SH,H),
    forall(between(1,SV,V),
     ignore((hv_c_value(Grid,C,H,V),
       assert_hvc_cell(ID,H,V,C))))).
assert_hvc_cell(_,_,_,C):- plain_var(C). % free_cell(C),!.
%assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(is_cmem(ID,HV,C)),assert(is_cmem_hv(ID,H,V,C)).
assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(cmem(ID,HV,C)),assert(cmem_hv(ID,H,V,C)).

show_grid_texture(GID):-
  ensure_cmem(GID),
 %% ensure_gid(Grid,GID),
  findall((S-C)-HV1,(cmem(GID,HV1,C),(smem(GID,HV1,S)->true;S='*')),Points),
  mgrid_size(GID,H,V),print_grid(H,V,GID,Points).

%:- dynamic(is_cmem/3).
%cmem(GID,HV,C):- is_cmem(GID,HV,C),C\==black.
:- dynamic(smem/3).

get_color_at(H,V,Grid,C):-
  ((nth1(V,Grid,Row),nth1(H,Row,C))*->true;C=_).

get_color_at_point(Grid,Point,C):- hv_point(H,V,Point), get_color_at(H,V,Grid,C).
  
:- dynamic(tid_to_gids/2).
tid_to_gids(TID,GID) :- awc,!, (clause(tid_to_gids(TID,GID),true)*-> true ; term_to_oid(TID,GID)).

find_test_gids(TestID,Type,GID) :- awc,!, find_test_grids(TestID,Type,Grid), grid_to_gid(Grid,GID).

find_test_grids(TestID,visible,Grid):- test_grids(TestID,Grid).
find_test_grids(TestID,train,Grid):- kaggle_arc_io(TestID,trn,_,Grid).
find_test_grids(TestID,test_input,Grid):- kaggle_arc_io(TestID,tst,in,Grid).
find_test_grids(TestID,train_input,Grid):- kaggle_arc_io(TestID,trn,in,Grid).
find_test_grids(TestID,train_output,Grid):- kaggle_arc_io(TestID,trn,out,Grid).

term_to_oid(v(A)>(B+C)*D,Atom):- my_maplist(atomic,[A,B,C,D]),atomic_list_concat([v,A,B,C,D],'_',Atom),!.
term_to_oid(t(A)>(B+C)*D,Atom):- my_maplist(atomic,[A,B,C,D]),atomic_list_concat([t,A,B,C,D],'_',Atom),!.
term_to_oid(T,A):- (compound(T)->term_to_atom(T,A);(atom(T)->T=A;term_to_atom(T,A))).

point_to_hvc(Var,_,_,_):- var(Var),!,fail.
point_to_hvc(Point,  H,V,fg):- is_ncpoint(Point),!, hv_point(H,V,Point).
%point_to_hvc(CD-Point,H,V,C):- var_or_color_data(CD,C),must(hv_point(H,V,Point)),!.
point_to_hvc(CD-Point,H,V,CD):- is_ncpoint(Point),!, hv_point(H,V,Point),!.
%point_to_hvc((C,D)-Point,H,V,(C,D)):- is_ncpoint(Point),!, hv_point(H,V,Point),!.
%point_ to_hvc(H,V,_,H,V).
%point_ to_hvc(Inf,Inf,offset_ranges(_,_,_,_)).
var_or_color_data(CD,C):- only_color_data(CD,C),!.
var_or_color_data(C,C).

make_default_grid(C,H,V,Grid):- ensure_make_grid(H,V,Grid),set_grid_default(C,Grid).

set_grid_default(C,Grid):- mapgrid(ignore_equal(C),Grid).

make_grid(H,V,Grid):- (H<1;V<1),!,u_dmsg(make_grid(H,V,Grid)),!,
  with_toplevel_pp(ansi,((write('<pre>'),arcST,ibreak))),!,fail.
make_grid(H,V,Grid):- between(1,40,H),between(1,40,V),  % max_min(H,0,HH,_), max_min(V,0,VV,_), %max_min(HH,32,_,HHH),max_min(VV,32,_,VVV),!,    
   ensure_make_grid(H,V,G),G=Grid.

ensure_make_grid(H,V,Grid):- make_grid_cache(H,V,Grid),!. 
ensure_make_grid(H,V,Grid):- make_fresh_grid(H,V,Grid), assertz(make_grid_cache(H,V,Grid)).

make_fresh_grid(1,1,[[_]]):-!.
make_fresh_grid(_,0,[]):- !.
make_fresh_grid(0,1,[[]]):-!.
make_fresh_grid(0,N,Grid):- N>1,!, make_list([],N,Grid).
make_fresh_grid(H,V,Grid):- length(Grid,V), my_maplist(make_lengths(H),Grid),!.


% Grid vis2D/resize
make_lengths(N,L):- length(L,N).

:- dynamic(make_grid_cache/3).
make_grid_cache:-
 my_time((forall(s_l_4sides(GH,GV), make_grid(GH,GV,_)))).

insert_row_of(N,Cell,Grid,NewGrid):- grid_size(Grid,H,V),make_list(Cell,H,Row),insert_row(N,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,NewGrid):- grid_size(Grid,H,V), insert_row(N,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,H,V,NewGrid):- N<0, NewN is V + N+1,!,insert_row(NewN,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,H,_,NewGrid):- length(Row,H),length(Left,N),append(Left,Right,Grid),append(Left,[Row|Right],NewGrid).

insert_col(N,Col,Grid,NewGrid):- grid_size(Grid,H,V), insert_col(N,Col,Grid,H,V,NewGrid).
insert_col(N,Col,Grid,H,V,NewGrid):- N<0, NewN is H + N+1,!,insert_col(NewN,Col,Grid,H,V,NewGrid).
insert_col(N,Col,Grid,_,V,NewGrid):- length(Col,V),my_maplist(insert_col_at(N),Col,Grid,NewGrid).


insert_col_at(N,Col,Row,NewRow):- length(Left,N),append(Left,Right,Row),append(Left,[Col|Right],NewRow).

insert_ele(N,V,L,NL):- length(Left,N),append(Left,Right,L),append(Left,[V|Right],NL).

delete_row(N,Grid,NewGrid):- N < 0, length(Grid,L), DR is L+N+1,delete_row(DR,Grid,NewGrid).
delete_row(N,Grid,NewGrid):- length(Left,N),append(Left,[_|Right],Grid),append(Left,Right,NewGrid).

delete_col(N,Grid,NewGrid):- my_maplist(delete_row(N),Grid,NewGrid).

map_nth(P,N,Grid):- nth1(N,Grid,E),call(P,E).
map_row(P,N,Grid):- map_nth(my_maplist(P),N,Grid).
map_col(P,N,Grid):- my_maplist(map_nth(P,N),Grid).


maybe_glyph(G,_,Glyph):- is_object(G), object_glyph(G,Glyph), !.
maybe_glyph(_G,N,Code):- i_glyph(N,Code),!.
maybe_glyph(G,_,Glyph):- is_grid(G),grid_dot(Glyph),!.
maybe_glyph(_,N,N).

is_visible(Obj):- \+ has_prop(iz(flag(hidden)),Obj).

is_pred_sorted_object_grid(O):- last(O,I), \+ is_visible(I).

object_printables(Objs,GroupVis,GroupPP):- 
 %is_pred_sorted_object_grid(Objs),
 !,GroupVis=GroupPP,GroupVis=Objs.
  
object_printables(Objs,GroupVis,GroupPP):-
  visible_first(Objs,SF),
  my_partition(is_visible,SF,GroupVis,GroupInv),
  append([GroupVis,GroupInv,Objs],GroupP),list_to_set(GroupP,GroupPP),!.

grid_color_and_glyph(Points,C,GN,H,V):- %is_object_group(Points), 
  object_printables(Points,_,ObjList),
  gridoid_color(ObjList,C,H,V),
  gridoid_glyph(ObjList,GN,H,V),!.
grid_color_and_glyph(Points,C,GN,H,V):- %is_object_group(Points), 
  gridoid_color(Points,C,H,V),
  gridoid_glyph(Points,GN,H,V),!.

gridoid_color(Points,C,H,V):-
 ((nb_current(color_index,ObjList),ObjList\==[]) -> ObjList=List ; Points=List),
  from_gridoid(List,C,_N,H,V,_G),!.

gridoid_glyph(Points,GN,H,V):-
 ((nb_current(color_index,ObjList),ObjList\==[]) -> ObjList=List ; Points=List),
  from_gridoid(List,_C,N,H,V,G), maybe_glyph(G,N,GN).

%from_gridoid(Points,C,GN,H,V):- from_gridoid(Points,C,N,H,V,G), maybe_glyph(G,N,GN).
%from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G),hv_c_value(G,C,H,V),nonvar_or_ci(C), \+ is_bg_color(C), \+ bg_sym(C), !.
%from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G),hv_c_value(G,C,H,V),nonvar_or_ci(C),!.
from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G), \+ cant_use(G),hv_c_value(G,C,H,V).

cant_use(G):- is_object(G), has_prop(G,iz(bfc(bg))),!.

%hv_c_value(O,_Color,_H,_V):- is_object(O), iz(O,iz(info(combined))), !, fail.
hv_c_value(O,_Color,_H,_V):-  plain_var(O),!,fail.
hv_c_value(O,_Color,_H,_V):-  is_ftVar(O),!,fail.
hv_c_value(O,Color,H,V):- is_cpoint(O),!,O=(Color-Point),hv_point(H,V,Point),!.
hv_c_value(In,C,H,V):- compound(In),In=in(O),!,hv_c_value(O,C,H,V).
hv_c_value(O,Color,H,V):- is_grid(O),!,nth1(V,O,Row),nth1(H,Row,Color),!.
hv_c_value([],_Color,_H,_V):-  !,fail.
hv_c_value(diff(_-> New),C,H,V):-!,hv_c_value(New,C,H,V).
hv_c_value(diff(_),_C,_H,_V):-!, fail.
hv_c_value(O,C,_H,_V):- is_colorish(O),!,C=O.
hv_c_value(O,GN,H,V):- is_vm_map(O),O.objs\==[],!,hv_c_value(O.objs,GN,H,V).
hv_c_value(O,GN,H,V):- is_vm_map(O),!,hv_c_value(O.grid,GN,H,V).

hv_c_value(ID,C,H,V):- (var(H);var(V)),!,arcST,atrace, hv_point(H,V,_),hv_c_value(ID,CC,H,V),CC=C.
hv_c_value(O,Color,H,V):- is_object(O),!,globalpoints(O,Ps),hv_c_value(Ps,Color,H,V).
hv_c_value(O,Color,H,V):- is_list(O), is_cpoints_list(  O),!,hv_point(H,V,Point),member(Color-Point,O).
hv_c_value(O,FGL   ,H,V):- is_list(O), my_maplist(is_ncpoint,O),!,hv_point(H,V,Point),member(Point,O),get_fg_label(FGL).
hv_c_value(O,FGL   ,H,V):- is_ncpoint(O),!,O=Point,hv_point(H,V,Point),!,get_fg_label(FGL).

%hv_c_value(G,Color,H,V):- is_group(G),!,into_list(G,L),member(E,L),hv_c_value(E,Color,H,V),!.
%hv_c_value(O,Color,H,V):- known_gridoid(O,G),!,hv_c_value(G,Color,H,V).
hv_c_value(G,Color,H,V):- my_assertion(into_list(G,L)),!,member(E,L),hv_c_value(E,Color,H,V),!.
%hv_c_value(O,Color,H,V):- is_object(O),localpoints(O,Ps),hv_c_value(Ps,Color,H,V).
%hv_c_value(L,Color,H,V):- is_list(L), member(E,L),hv_c_value(E,Color,H,V),!.

point_c_value(Point,C,Grid):- hv_point(Point,H,V),hv_c_value(Grid,C,H,V).


hv_cg_value(O,_Color,_H,_V):-  var(O),!,fail.
hv_cg_value(ID,C,H,V):- (var(H);var(V)),!, hv_point(H,V,_),hv_cg_value(ID,CC,H,V),CC=C.
hv_cg_value(Grid,Color,H,V):- is_grid(Grid),!,nth1(V,Grid,Row),nth1(H,Row,Color).
hv_cg_value(O,GN,H,V):- is_vm_map(O),O.objs\==[],!,hv_cg_value(O.objs,GN,H,V).
hv_cg_value(O,GN,H,V):- is_vm_map(O),!,hv_cg_value(O.grid,GN,H,V).
hv_cg_value(O,Color-GN,H,V):- is_object(O), hv_c_value(O,Color,H,V),obj_to_oid(O,GN),nonvar_or_ci(GN),!.

hv_cg_value([G|Points],CN,H,V):- quietly(( is_list(Points), is_object_or_grid(G))), 
   grid_color_and_glyph([G|Points],C,N,H,V),CN=(C-N),!.
%hv_cg_value(O,CN,H,V):- (has_index(color_index);has_index(glyph_index)),
%   grid_color_and_glyph(O,C,N,H,V),CN=(C-N),!.


%hv_cg_value(ID,C,H,V):- nonvar(H) -> (hv_point(H,V,HV),cmem(ID,HV,C)); (cmem(ID,HV,C),hv_point(H,V,HV)).

%hv_cg_value(Points,C,H,V):- is_list(Points),member(C-HV,Points),hv_point(H,V,HV).

hv_cg_value_or(Grid,C,_H,_V,Else):- (Grid==[];var(Grid)),!,Else=C.
hv_cg_value_or(Grid,C,H,V,Else):- hv_cg_value(Grid,C,H,V)*->true;C=Else.

hv_c_value_or(Grid,C,_H,_V,Else):- (Grid==[];var(Grid)),!,Else=C.
hv_c_value_or(Grid,C,H,V,Else):- hv_c_value(Grid,C,H,V)*->true;C=Else.

pgt:- clsmake,!,pgt(Obj),once(print_grid(Obj)).

pgt(Obj):- pgt1(Obj).
pgt(Obj):- pgt2(Obj).
pgt(Obj1-Obj2):- pgt1(Obj1),pgt2(Obj2).
pgt([Obj1]-[Obj2]):- pgt1(Obj1),pgt2(Obj2).
pgt([Obj1,Obj2]):- pgt1(Obj1),pgt2(Obj2).
pgt1(Obj):-  hv_point(1,1,HV0101),
  Obj = obj( [ mass(536),
         shape_rep(grav, [ HV0101, point_02_01]),
         colors_cc( [ cc(red, 190.0), cc(silver, 132.0), cc(green, 55.0), cc(cyan, 53.0),
                   cc(blue, 45.0), cc(yellow, 36.0), cc(orange, 25.0)]),
         localpoints( [ red-HV0101, silver-point_02_01]), vis2D(3, 1), rot2D(sameR), loc2D(3, 1),
         changes([]), iz(info(combined)),
         iz(shape(rectangle)), iz(multicolored),
         iz(shape(polygon)), %obj _to_oid(v('0ad4ef5')>(trn+0)*in, 21),
       %  globalpoints( [ red-HV0101, silver-point_02_01]),
         grid_size(8, 8)]).

pgt2(Obj):- hv_point(1,1,HV0101), 
  Obj = obj( [ mass(536),
         shape_rep(grav, [ HV0101, point_02_01]),
         colors_cc( [ cc(red, 190.0), cc(silver, 132.0), cc(green, 55.0), cc(cyan, 53.0),
                   cc(blue, 45.0), cc(yellow, 36.0), cc(orange, 25.0)]),
         localpoints( [ red-HV0101, silver-point_02_01]), vis2D(3, 1), rot2D(sameR), loc2D(2, 1),
         changes([]), iz(info(combined)),
         iz(shape(rectangle)), iz(multicolored),
         iz(shape(polygon)), %obj _to_oid(v('a1d4ef5')>(trn+0)*in, 66),
        %  globalpoints( [ red-HV0101, silver-point_02_01]),
         grid_size(8, 8)]).

%hv_cg_value(ID,C,H,V):- row_mem_nth(H,ID,V,C).

has_index(CI):-  luser_getval(CI,List),List\==[],is_list(List),!.


hv_member(HV,C,O):- is_grid(O),!,fail,globalpoints(O,Ps),hv_member(Ps,C,HV),!.
hv_member(HV,C,O):- is_object(O),!,globalpoints(O,Ps),hv_member(Ps,C,HV),!.
hv_member(HV,C,Points):- member(C-HV,Points),!.
% hv_member(HV,C,Points):- sub_term(C-HV,Points),!.
/*b_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),set_nth1(H,Row,C).
nb_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),nb_set_nth1(H,Row,C).
b_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),rplc_nth1(H,Row,OldC,NewC).
nb_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),nb_rplc_nth1(H,Row,OldC,NewC).

*/

my_assertion_is_color(_):-!.
my_assertion_is_color(X):- my_assertion(is_color(X)).
%replace_local_hvcpoint(_H,_V,NewC,OldC,Point,G,GO):-nonvar_or_ci(G),!,G=obj(L),is_list(L),
replace_global_hvc_point(H,V,NewC,OldC,Grid,GridO):- is_grid(Grid),!, my_assertion_is_color((NewC)), replace_grid_point(H,V,NewC,OldC,Grid,GridO).
replace_global_hvc_point(H,V,NewC,OldC,G,GO):- hv_point(H,V,Point), must_det_ll(replace_global_point_color(Point,NewC,OldC,G,GO)).

%replace_global_hvc_point(Point,OldC,G,GO):- trace_or_throw(unknown_target_type(replace_global_hvc_point(Point,OldC,G,GO))).

replace_global_point_color(Point,NewC,OldC,G,GO):- is_object(G), !,
    globalpoints(G,Points),
    replace_in_points(Point,NewC,OldC,Points,RPoints),
    setq(G,[globalpoints(RPoints)],GO).
replace_global_point_color(Point,NewC,OldC,G,GO):- is_grid(G),!, point_to_hvc(Point,H,V,_),my_assertion_is_color((NewC)), 
  replace_grid_point(H,V,NewC,OldC,G,GO),!.
replace_global_point_color(Point,NewC,OldC,G,GO):- is_list(G),!, my_maplist(replace_global_point_color(Point,NewC,OldC),G,GO).
replace_global_point_color(Point,NewC,OldC,G,GO):- replace_local_point_color(Point,NewC,OldC,G,GO).


replace_in_points(Point,NewC,OldC,G,GO):- (var(NewC);is_bg_color(NewC)),!, ((select(OldC-Point,G,GO), \+ is_bg_color(OldC))->true; GO=G).
replace_in_points(Point,NewC,OldC,G,GO):- select(C-Point,G,Rest), !, ((\+ \+ OldC = C )-> GO= [NewC-Point|Rest] ; GO=G).
replace_in_points(Point,NewC,OldC,G,GO):- attvar(OldC),!, (\+ (OldC \= noexisting(Point,NewC))-> GO= [NewC-Point|G] ; G=GO).
replace_in_points(Point,NewC,OldC,G,GO):- (var(OldC);is_bg_color(OldC)),!, GO= [NewC-Point|G].



%replace_local_points(Obj,Grid,GridO):- is_group(Obj), localpoints(Obj,Points),replace_local_points(Points,Grid,GridO).
%replace_local_points([H|T],Grid,GridO):- is_points_list([H|T]), !, replace_local_points([H|T],Grid,GridO).
%replace_local_points([H|T],Grid,GridO):- !, replace_local_points(H,Grid,GridM),replace_local_points(T,GridM,GridO).

%replace_grid_points([],_,Grid,Grid):-!.
replace_grid_points(List,Grid,GridO):- replace_local_points(List,_OldC,Grid,GridO).
replace_grid_points(List,OldC,Grid,GridO):- replace_local_points(List,OldC,Grid,GridO).

replace_local_points(Nil,_OldC,G,G):- Nil ==[], !.
replace_local_points(Obj,OldC,Grid,GridO):- is_grid(Obj),!, localpoints_include_bg(Obj,Points),replace_local_points(Points,OldC,Grid,GridO).
replace_local_points([H|T],OldC,G,GO):- !,  replace_local_points(H,OldC,G,MGO), replace_local_points(T,OldC,MGO,GO). 
replace_local_points(Obj,OldC,Grid,GridO):- is_object(Obj), localpoints(Obj,Points),replace_local_points(Points,OldC,Grid,GridO).

replace_local_points(Point,OldC,G,GO):- is_grid(G),!, point_to_hvc(Point,H,V,NewC),my_assertion_is_color((NewC)), 
  replace_grid_point(H,V,NewC,OldC,G,GO),!.
replace_local_points(Point,OldC,G,GO):- point_to_hvc(Point,H,V,NewC),nop(my_assertion_is_color((NewC))), 
  hv_point(H,V,Colorless),
  must_det_ll(replace_local_point_color(Colorless,NewC,OldC,G,GO)),!.
replace_local_points(Point,OldC,G,GO):- trace_or_throw(unknown_target_type(replace_local_points(Point,OldC,G,GO))).

replace_local_point_color(Point,NewC,OldC,G,GO):- is_points_list(G),!, replace_in_points(Point,NewC,OldC,G,GO).
replace_local_point_color(Point,NewC,OldC,G,GO):- is_list(G),!, my_maplist(replace_local_point_color(Point,NewC,OldC),G,GO).
replace_local_point_color(Point,NewC,OldC,G,GO):- is_object(G), !,
    localpoints(G,Points),     
    replace_in_points(Point,NewC,OldC,Points,RPoints),
    %loc2D(G,OH,OV),offset_point(OH,OV,Point,LPoint),shape_rep(grav,G,NCPoints), my_maplist(replace_in_points(Point,NewC,OldC),NCPoints,RNCPoints), ,shape_rep(grav,RNCPoints)
    setq(G,localpoints(RPoints),GO).
replace_local_point_color(Point,NewC,OldC,G,GO):- trace_or_throw(unknown_target_type(replace_local_point_color(Point,NewC,OldC,G,GO))).





create_movements:- 
 show_time_gt_duration(0.22,(
 forall( between(1,30,H),
  forall(between(1,30,V),
  calc_movement(H,V))))).


:- dynamic(hv_point/3).
:- dynamic(is_adjacent_point/3).
:- dynamic(is_adjacent_hv/5).
:- export(hv_point/3).
:- export(is_adjacent_point/3).
:- export(is_adjacent_hv/5).

hv(H,V,hv(H,V)).

calc_movement(H,V):- forall((nav(Dir,HO,VO),Dir\==c), save_calc_movement(H,V,Dir,HO,VO)).

save_calc_movement(H,V,Dir,HO,VO):- point_atoms,!, H2 is HO+H, V2 is VO+V,
  muarc_mod(M),
  ignore((between(1,32,H2), between(1,32,V2), 
     format(atom(HV),'point_~|~`0t~d~2+_~|~`0t~d~2+',  [H,V]),
     format(atom(HV2),'point_~|~`0t~d~2+_~|~`0t~d~2+', [H2,V2]),
     %hv_point(H,V,HV),
     %hv_point(H2,V2,HV2),
    assert_if_new(M:is_adjacent_point_impl(HV,Dir,HV2)),
    assert_if_new(M:hv_point(H,V,HV)),
    true)).
   % assert_if_new(M:is_adjacent_hv(H,V,Dir,H2,V2)))).
save_calc_movement(_H,_V,_Dir,_HO,_VO).


is_adjacent_point(HV,Dir,HV2):- lazy_p3(is_adjacent_point_impl,HV,Dir,HV2).

is_adjacent_hv(H,V,Dir,H2,V2):- nav(Dir,HO,VO), Dir\==c, pluz(H,HO,H2), pluz(V,VO,V2).

:- if(\+ point_atoms).
hv_point(H,V,hv(H,V)).
is_adjacent_point_impl(HV,Dir,HV2):- ground(Dir),is_adjacent_hv(H,V,Dir,H2,V2),hv_point(H,V,HV), hv_point(H2,V2,HV2),!.
is_adjacent_point_impl(HV,Dir,HV2):- hv_point(H,V,HV), is_adjacent_hv(H,V,Dir,H2,V2), hv_point(H2,V2,HV2).
:- endif.



lazy_p3(P3,V,VO,V2):- nonvar(V),!,((nonvar(VO);nonvar(V2))->call(P3,V,VO,V2);(freeze(VO,call(P3,V,VO,V2)),freeze(V2,call(P3,V,VO,V2)))).
lazy_p3(P3,V,VO,V2):- nonvar(V2),!,((nonvar(VO);nonvar(V))->call(P3,V,VO,V2);(freeze(VO,call(P3,V,VO,V2)),freeze(V,call(P3,V,VO,V2)))).
lazy_p3(P3,V,VO,V2):- nonvar(VO),!,freeze(V2,call(P3,V,VO,V2)),freeze(V,call(P3,V,VO,V2)).
lazy_p3(P3,V,VO,V2):- freeze(V2,lazy_p3(P3,V,VO,V2)),freeze(V,lazy_p3(P3,V,VO,V2)),freeze(VO,lazy_p3(P3,V,VO,V2)).

:- use_module(library(clpfd)).
pluz(V,VO,V2):- VO==0,!,V=V2.
%pluz(V,VO,V2):- V2 #= V+VO.
pluz(V,VO,V2):- lazy_p3(plus,V,VO,V2).
/*
pluz(V,VO,V2):- VO==0,!,V=V2.
pluz(V,VO,V2):- nonvar(V),!,((nonvar(VO);nonvar(V2))->plus(V,VO,V2);(freeze(VO,plus(V,VO,V2)),freeze(V2,plus(V,VO,V2)))).
pluz(V,VO,V2):- nonvar(V2),!,((nonvar(VO);nonvar(V))->plus(V,VO,V2);(freeze(VO,plus(V,VO,V2)),freeze(V,plus(V,VO,V2)))).
pluz(V,VO,V2):- nonvar(VO),!,((nonvar(V2);nonvar(V))->plus(V,VO,V2);(freeze(V2,plus(V,VO,V2)),freeze(V,plus(V,VO,V2)))).
pluz(V,VO,V2):- freeze(V2,pluz(V,VO,V2)),freeze(V,pluz(V,VO,V2)),freeze(VO,pluz(V,VO,V2)).
*/

is_adjacent_2points(HV,Dir,HV2,HV3):-  is_adjacent_point(HV,Dir,HV2),is_adjacent_point(HV2,Dir,HV3).

create_points_plus:- show_time_gt_duration(0.3,create_points_plus_now).

create_points_plus_now:- \+ point_atoms,!.
create_points_plus_now:- 
 forall(
  (between(1,30,H1),between(1,30,V1),
   between(1,30,H2),between(1,30,V2)),
  (H is H1 + H2 -1,
   V is V1 + V2 -1,
   ignore(
    (hv_point(H, V, HV),
     hv_point(H1,V1,HV1),
     hv_point(H2,V2,HV2),
     assert_point_plus_if_needed(HV1,HV2,HV))))).


:- dynamic(is_point_plus/3). % number_of_clauses(214426)

assert_point_plus_if_needed(HV1,HV2,HV):- point_plus(HV1,HV2,HV),!.
assert_point_plus_if_needed(HV1,HV2,HV):- assert(is_point_plus(HV1,HV2,HV)).

point_plus(hv(H1,V1),hv(H2,V2),hv(H3,V3)):- \+ point_atoms,!, 
  H3 #= H1 + H2 -1,
  V3 #= V1 + V2 -1.
  
point_plus(HV0101,HV,HV):- hv_point(1,1,HV0101), hv_point(_,_,HV).
point_plus(HV,HV0101,HV):- hv_point(1,1,HV0101), hv_point(_,_,HV).
point_plus(HV1,HV2,HV):- is_point_plus(HV1,HV2,HV).

point_minus(HV,HV2,HV1):- point_plus(HV1,HV2,HV).
%point_minus(HV,HV2,HV1):-point_plus(HV2,HV1,HV),!.


is_adj_point_es(HV1,HV2):- is_adjacent_point(HV1,s,HV2).
is_adj_point_es(HV1,HV2):- is_adjacent_point(HV1,e,HV2).
is_adj_point_es_d(HV1,HV2):- is_adj_point_es(HV1,HV2).
is_adj_point_es_d(HV1,HV2):- is_adjacent_point(HV1,se,HV2).
is_adj_point_es_d(HV1,HV2):- is_adjacent_point(HV1,sw,HV2).

is_adj_point_wn(HV1,HV2):- is_adjacent_point(HV1,n,HV2).
is_adj_point_wn(HV1,HV2):- is_adjacent_point(HV1,w,HV2).
is_adj_point_wn_d(HV1,HV2):- is_adj_point_wn(HV1,HV2).
is_adj_point_wn_d(HV1,HV2):- is_adjacent_point(HV1,nw,HV2).
is_adj_point_wn_d(HV1,HV2):- is_adjacent_point(HV1,ne,HV2).

is_adj_point_nsew(HV1,HV2):- is_adj_point_es(HV1,HV2).
is_adj_point_nsew(HV1,HV2):- is_adj_point_wn(HV1,HV2).
is_adj_point_d(HV1,HV2):- is_adjacent_point(HV1,ne,HV2).
is_adj_point_d(HV1,HV2):- is_adjacent_point(HV1,nw,HV2).
is_adj_point_d(HV1,HV2):- is_adjacent_point(HV1,se,HV2).
is_adj_point_d(HV1,HV2):- is_adjacent_point(HV1,sw,HV2).
is_adj_point_colormass(C1,HV1,HV2):- is_adjacent_point(HV1,Dir,HV2), freeze(C1,(C1\==black ->  true ; \+ is_diag(Dir))).

is_adj_point_type(C1,colormass(_),HV1,HV2):- is_adj_point_colormass(C1,HV1,HV2).
%is_adj_point_type(C1,v_line,HV1,HV2):- (is_adjacent_point(HV1,n,HV2);is_adjacent_point(HV1,n,HV2)).
%is_adj_point_type(C1,h_line,HV1,HV2):- (is_adjacent_point(HV1,w,HV2);is_adjacent_point(HV1,e,HV2)).
is_adj_point_type(_C1,nsew(_),HV1,HV2):- is_adj_point_nsew(HV1,HV2).
is_adj_point_type(_C1,diamonds,HV1,HV2):- is_adj_point_d(HV1,HV2).

:- dynamic(gid_type_oid/3).



grid_type_oid(Grid,Type,OID):- ensure_gid(Grid,GID), cache_grid_objs_for(Type,GID), gid_type_oid(GID,Type,OID).

:- dynamic(is_gridmass/2).

mmass(Grid,Mass):- \+ luser_getval(generate_gids,true),!, mass(Grid,Mass).
mmass(Grid,Mass):- ensure_gid(Grid,GID), Grid\==GID,!,mmass(GID,Mass).
mmass(GID,Mass):- is_gridmass(GID,Mass),!.
mmass(GID,Mass):- 
  findall(_,(cmem(GID,_,C),C\==black),L),length(L,Mass),
  assert(is_gridmass(GID,Mass)).

mgrid_size(Grid,H,V):- luser_getval(generate_gids,true),!, ensure_gid(Grid,GID), is_grid_size(GID,H,V).
mgrid_size(Grid,H,V):- grid_size(Grid,H,V).

/*
is_nsew_same_as_colormass(Grid):-
  ensure_gid(Grid,GID),
  grid_object_points(GID,nswe,Nswe),
  grid_object_points(GID,colormass(N),Colormass),!,
  sort_safe(Colormass,S1),sort_safe(Nswe,S2),
  S1\=@=S2.

is_nsew_same_as_colormass_count(Grid):-
  ensure_gid(Grid,GID),
  grid_object_count(GID,nswe,Nswe),
  grid_object_count(GID,colormass(N),Colormass),!,
  Nswe\=@=Colormass.
*/

ensure_obj(OID,Obj):- into_obj(OID,Obj).

show_oid(OID):-
 (var(OID)->gid_type_oid(_GID,Type,OID);true),
 oid_to_points(OID,Points),
 ignore(gid_type_oid(_,Type,OID)),
 print_grid(oid(Type,OID),Points).



oid_to_glyph(OID,Glyph):- atom_chars(OID,[_,_,Glyph|_]).

tmem(GID,HV2,Type):- omem(GID,HV2,OID), gid_type_oid(GID,Type,OID).

ensure_indv_type(Type):- member(Type,[countz,nsew(6),colormass(6),colormass(1),fg(4)]).


test_show_grid_objs(TestID):- ensure_test(TestID), 
  show_test_objs(TestID).

show_test_objs(TestID):- every_grid(show_grid_objs,TestID).

show_grid_objs(Grid):- true, 
  ensure_gid(Grid,GID),show_gid_objs(GID).

show_gid_objs(GID):- true, 
  cache_grid_objs(GID),
  grid_object_texture_points(GID,_,Texture),
  flatten(Texture,TextureF),
  grid_object_glyph_points(GID,_,Glyphs),
  flatten(Glyphs,GlyphsF),
  print_side_by_side(GID,TextureF,GlyphsF).


show_grid_objs_typed(Grid):- true, 
  ensure_gid(Grid,GID),cache_grid_objs(GID),
  findall(Type=G,(ensure_indv_type(Type),
    %grid_object_glyph_points(GID,Type,Group),
    grid_object_texture_points(GID,Type,Group),
  
    flatten(Group,G),G\==[]),Grids),
  my_maplist(arg(2),Grids,Flatme),flatten(Flatme,All),
  show_grid_objs_list(GID,All,Grids).
  %print_ss([all=All|Grids]),!.
  
  
  %print_grid((GID),All).
show_grid_objs_list(GID,_All,Grids):- Grids=[N=V],!,dash_chars,print_grid(GID=N,V),dash_chars.
show_grid_objs_list(GID,All,Grids):- dash_chars,print_ss([GID=All|Grids]),dash_chars,!.

grid_object_points(Grid,Type,Groups):-
  ensure_gid(Grid,GID),
  gid_object_points(GID,Type,Groups).

gid_object_points(GID,Type,Groups):-
  luser_setval(generate_gids,true),
  \+ \+ cache_grid_objs_for(Type,GID),
  findall(Points,(gid_type_oid(GID,Type,OID),oid_to_points(OID,Points),Points\==[]),Groups).

oid_to_points(OID,Points):- findall(C-HV,(omem(GID,HV,OID),cmem(GID,HV,C)),Points).

grid_object_glyph_points(Grid,Type,Groups):-
  ensure_gid(Grid,GID),
  gid_object_glyph_points(GID,Type,Groups).

gid_object_glyph_points(GID,Type,Groups):-
  cache_grid_objs_for(Type,GID),
  findall(Points,(gid_type_oid(GID,Type,OID),oid_to_glyph_points(OID,Points)),Groups).

oid_to_glyph_points(OID,Points):- oid_to_glyph(OID,Glyph),
  findall(GC-HV,(omem(GID,HV,OID),cmem(GID,HV,C),(nmem(GID,HV,0)->GC=C;(GC=(Glyph-C)))),Points).

grid_object_texture_points(Grid,Type,Groups):-
  ensure_gid(Grid,GID),
  gid_object_texture_points(GID,Type,Groups).

gid_object_texture_points(GID,Type,Groups):-
  cache_grid_objs_for(Type,GID),
  findall(Points,(gid_type_oid(GID,Type,OID),oid_to_texture_points(OID,Points)),Groups).

oid_to_texture_points(OID,Points):- oid_to_glyph(OID,Glyph),
  findall(GC-HV,(omem(GID,HV,OID),cmem(GID,HV,C),
    (nmem(GID,HV,0)->GC=(C);(smem(GID,HV,Sym)->(GC=(Sym-C));(GC=(Glyph-C))))),Points).

%grid_object_defs(Grid,Type,Groups):-
%  ensure_gid(Grid,GID), vm_for_gid(GID,VM),!.

grid_object_count(Grid,Type,Len):-
  ensure_gid(Grid,GID),
  grid_object_points(GID,Type,Groups),length(Groups,Len).

% each_o(Grid,Type,OID),show_oid(OID).

grid_type_obj(Grid,Type,OBJ):- ensure_gid(Grid,GID), 
  cache_grid_objs(GID),!,gid_type_oid(GID,Type,OID),ensure_obj(OID,OBJ).

%% 2,467,553,363 inferences, 478.682 CPU in 479.364 seconds (100% CPU, 5154887 Lips)
precache_all_grid_objs:- 
  luser_setval(generate_gids,true),
  time(forall(all_arc_test_name_unordered(TestID),cache_grid_objs_for_test(TestID))).

cache_grid_objs_for_test(TestID):- 
 forall(kaggle_arc(TestID,_Example,I,O),
    (ignore(cache_grid_objs(I)),ignore(cache_grid_objs(O)))).

% 810,000
cache_grid_objs_for(_Type,Grid):- ensure_gid(Grid,GID),cache_grid_objs(GID).

cache_grid_objs(TestID):- is_valid_testname(TestID),!, cache_grid_objs_for_test(TestID).
cache_grid_objs(Grid):- \+ atom(Grid),!, ensure_gid(Grid,GID),cache_grid_objs(GID).
cache_grid_objs(GID):- gid_type_oid(GID,_,_),!.
cache_grid_objs(GID):-
  Shown = _,
  show_time_gt_duration(0.3, 
   (forall(ensure_indv_type(Type),cache_grid_objs_now(Type,GID)),
    forall(how_count(_,How,_),grid_obj_count_1(GID,How,_))),
    (Shown = 1,show_grid_objs(GID))),
  ignore(((   var(Shown),is_grid_obj_count(GID,Type,Count),Count>=100,show_grid_objs(GID),show_obj_counts(GID)))),
  ignore(((nonvar(Shown),is_grid_obj_count(GID,Type,Count),Count>=40,                    show_obj_counts(GID)))),!.

show_obj_counts(GID):- findall(Type=Count,grid_obj_count(GID,Type,Count),Out),pp(GID=Out),!.

:- dynamic(is_grid_obj_count/3).

grid_obj_counts(Grid,Counts):-
   findall(Type=Count,grid_obj_count(Grid,Type,Count),Counts).

grid_obj_count(Grid,Type,Count):- ensure_gid(Grid,GID),
  cache_grid_objs(GID),!,
  grid_obj_count_1(GID,Type,Count).

grid_obj_count_1(GID,Type,Count):- var(Type),!,how_count(_,Type,_),grid_obj_count_1(GID,Type,Count).
grid_obj_count_1(GID,Type,Count):- is_grid_obj_count(GID,Type,Count),!.
grid_obj_count_1(GID,Type,Count):- 
   how_count(GID,Type1,Goal),Type=@=Type1, !, 
   findall(_,Goal,L),length(L,Count), assert(is_grid_obj_count(GID,Type,Count)).

how_count(GID,all,Goal):- Goal = gid_type_oid(GID,_,_).
how_count(GID,fg,Goal):- Goal = ((gid_type_oid(GID,_,OID),\+ cindv(OID,unique_color,element,black))).
how_count(GID,bg,Goal):- Goal = ((gid_type_oid(GID,_,OID), cindv(OID,unique_color,element,black))).
how_count(GID,fg(Type),Goal):- ensure_indv_type(Type), Goal = (gid_type_oid(GID,Type,OID),\+ cindv(OID,unique_color,element,black)).
how_count(GID,bg(Type),Goal):- ensure_indv_type(Type), Goal = (gid_type_oid(GID,Type,OID), cindv(OID,unique_color,element,black)).
%how_count(GID,Color,Goal):- enum_fg_colors(Color), Goal = (cindv(OID,unique_color,element,Color)).
%how_count(GID,Type,Goal):- ensure_indv_type(Type), Goal = (gid_type_oid(GID,Type,_)).

%cache_gid_objs(GID):- show_time_gt_duration(0.4, forall(ensure_indv_type(Type),cache_grid_objs_now(Type,GID)), show_grid_objs(GID)).

is_obj_count_all_lt_15(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,all,Count),Count<15.
is_obj_count_all_gt_40(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,all,Count),Count>40.
is_obj_count_all_gt_200(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,all,Count),Count>200.
is_obj_count_nsew_zero(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,fg(nsew(_)),C),C==0.
is_obj_count_diamonds_not_zero(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,fg(diamonds),C),C>0.
is_obj_count_colormass_zero(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,fg(colormass(_)),C),C==0.



cache_grid_objs_now(Type,GID):- gid_type_oid(GID,Type,_),!.
%cache_grid_objs_now(Type,GID):- var(Type),!,ensure_indv_type(Type),cache_grid_objs(Type,GID).
cache_grid_objs_now(Type,GID):- cache_grid_objs1(Type,GID).

type_may_have(GID,Type,HV1):- 
  \+ (omem(GID,HV1,OOID), gid_type_oid(GID,OType,OOID), cant_reuse(Type,OType)).
cant_reuse(X,Y):- can_reuse(X,Y),!,fail.
cant_reuse(_,_).
can_reuse(fg(_),Other):- !, Other\=fg(_).
can_reuse(_,fg(_)).
%cant_reuse(X,X):-!.
%cant_reuse(A,B):- can_reuse(A,B),!,fail.
%can_reuse(X,colormass(N)):- X \== colormass(N).
%can_reuse(X,diamonds):- X \== diamonds.
%can_reuse(nsew(N),X):- X\==nsew(N).

:- dynamic(nmem/3).
:- dynamic(dmem/6).
%smem(GID,HV1,'0'):- nmem(GID,HV1,0).
%smem(GID,HV1,'~'):- nmem(GID,HV1,8).

nswe2dir(v(n,s)). 
nswe2dir(v(e,w)). 
nswe2dir(v(ne,sw)). 
nswe2dir(v(se,nw)).

remake_texture(GID):-
 retractall(nmem(GID,_,_)),
 retractall(smem(GID,_,_)),
 retractall(dmem(GID,_,_,_,_)),
 ensure_texture(GID).



ok_adjacent(GID,HV1):- smem(GID,HV1,'0'),!,fail.
ok_adjacent(GID,HV1):- smem(GID,HV1,'1'),!,fail.
ok_adjacent(GID,HV1):- nmem(GID,HV1,0),!,fail.
ok_adjacent(_,_).

redo_texture_hv_neigbors(GID,HV1):- 
  forall((is_adjacent_point(HV1,_Dir,HV2),ok_adjacent(GID,HV2), \+ over_edge_of(HV2,GID)),redo_texture_hv(GID,HV2)).

redo_texture_hv(GID,HV1):-
  retractall(dmem(GID,HV1,_,_,_,_)), 
  retractall(nmem(GID,HV1,_)),
  retractall(smem(GID,HV1,_)),
  must_det_ll((
  do_texture_hv(GID,HV1),
  nmem(GID,HV1,N),
  dmem(GID,HV1,NS,EW,UP,DN), 
  once(n_d_s(N,[NS,EW,UP,DN],S)),
  asserta(smem(GID,HV1,S)))).

do_texture_hv(GID,HV1):- \+ cmem(GID,HV1,_C1),!, dmsg(missing(cmem(GID,HV1))).
do_texture_hv(GID,HV1):-
 must_det_ll((
  cmem(GID,HV1,C1),
  findall(Dirs,
   (nswe2dir(V),
     findall(Dir,(arg(_,V,Dir),is_adjacent_point(HV1,Dir,HV2),cmem(GID,HV2,C1),ok_adjacent(GID,HV1),ok_adjacent(GID,HV2)),Dirs)),NSs),
  once(\+ dmem(GID,HV1,_,_,_,_) -> (DMem=..[dmem,GID,HV1|NSs], assert(DMem)) ; true),
  once(\+ nmem(GID,HV1,_) -> (flatten(NSs,L),length(L,N), assert(nmem(GID,HV1,N))) ; true))).


ensure_texture(GID):- \+ cmem(GID,_,_), ensure_cmem(GID), fail.

ensure_texture(GID):- \+ cmem(GID,_,_), dmsg(cant_ensure_cmem(GID)),!,ibreak.

%ensure_texture(GID):- \+ ( cmem(GID,HV,_), (( \+ smem(GID,HV,_) ); \+ nmem(GID,HV,_); \+ dmem(GID,HV,_,_,_,_) )),!,
%  dmsg(cant_ensure_texture(GID)),!. % 

ensure_texture(GID):-
  cmem(GID,HV1,_), once( (\+ (nmem(GID,HV1,_)) ; \+ dmem(GID,HV1,_,_,_,_))),
  once(do_texture_hv(GID,HV1)),
  fail.

ensure_texture(GID):-
  nmem(GID,HV1,N), \+ smem(GID,HV1,_),
  dmem(GID,HV1,NS,EW,UP,DN), 
  once(n_d_s(N,[NS,EW,UP,DN],S)),
  assert(smem(GID,HV1,S)),
  fail.

ensure_texture(GID):-
  mgrid_size(GID,H,V),on_edge_become(S,B),smem(GID,HV1,S),
  \+ \+ on_edge_of(HV1,grid(H,V),_),
  replace_smem(GID,HV1,S,B),
  fail.

ensure_texture(GID):-
  this_has_dir_obj_become(S,Dir,Obj,B),smem(GID,HV1,S),is_adjacent_point(HV1,Dir,HV2),smem(GID,HV2,Obj),
  replace_smem(GID,HV1,S,B),
  fail.

ensure_texture(GID):- each_color(GID,C), C\==black,
  findall(S,(cmem(GID,HV1,C),smem(GID,HV1,S)),Syms),
  get_ccs(Syms,SymCC),
  member(cc('0',S1s),SymCC),
  on_edge_become(Weak,_),
  member(cc(Weak,S2s),SymCC),S1s>=S2s,
  smem(GID,HV1,Weak),
  replace_smem(GID,HV1,Weak,'0'),fail.

ensure_texture(GID):- 
  on_edge_become(Weak,_), 
  smem(GID,HV1,Weak),once((nmem(GID,HV1,3), \+ cmem(GID,HV1,black))), replace_smem(GID,HV1,Weak,'0'),fail.

ensure_texture(GID):- fail,
  on_edge_become(Weak,_),
  smem(GID,HV1,Weak),nmem(GID,HV1,1),is_adjacent_point(HV1,_,HV2), smem(GID,HV2,Weak),nmem(GID,HV2,1),
  replace_smem(GID,HV1,Weak,'0'), \+ cmem(GID,HV1,black), replace_smem(GID,HV2,Weak,'0'),fail.

ensure_texture(_).

each_color(GID,C):-findall(C1,cmem(GID,_,C1),Set),get_ccs(Set,CCs),member(cc(C,_),CCs).

replace_smem(GID,HV1,S,B):- retractall(smem(GID,HV1,_)),
  asserta(smem(GID,HV1,B)),
  ignore(((S\==B,B=='0')->redo_texture_hv_neigbors(GID,HV1);true)).

this_has_dir_obj_become('+','n','#','|').
this_has_dir_obj_become('+','s','#','|').
this_has_dir_obj_become('+','e','#','-').
this_has_dir_obj_become('+','w','#','-').

this_has_dir_obj_become('+','n','X','-').
this_has_dir_obj_become('+','s','X','-').
this_has_dir_obj_become('+','e','X','|').
this_has_dir_obj_become('+','w','X','|').


this_has_dir_obj_become('#','n','~','~').
this_has_dir_obj_become('#','s','~','~').
this_has_dir_obj_become('#','e','~','~').
this_has_dir_obj_become('#','w','~','~').

this_has_dir_obj_become('^','n','-','0').
this_has_dir_obj_become('v','s','-','0').
this_has_dir_obj_become('>','e','-','0').
this_has_dir_obj_become('<','w','-','0').


this_has_dir_obj_become('!','n','|','|').
this_has_dir_obj_become('!','s','|','|').
this_has_dir_obj_become('=','e','-','-').
this_has_dir_obj_become('=','w','-','-').


this_has_dir_obj_become('|','n','~','*').
this_has_dir_obj_become('|','s','~','*').
this_has_dir_obj_become('-','e','~','*').
this_has_dir_obj_become('-','w','~','*').



on_edge_of(HV1,_,n):- hv_point(_,1,HV1).
on_edge_of(HV1,_,e):- hv_point(1,_,HV1).
on_edge_of(HV1,grid(H,_),w):- hv_point(H,_,HV1).
on_edge_of(HV1,grid(_,V),s):- hv_point(_,V,HV1).

over_edge_of(HV1,_GID):- hv_point(_,VV,HV1),VV<1,!.
over_edge_of(HV1,_GID):- hv_point(HH,_,HV1),HH<1,!.
over_edge_of(HV1,GID):- mgrid_size(GID,H,V),hv_point(HH,VV,HV1),!,(HH>H;VV>V).
over_edge_of(HV1,grid(H,_)):- hv_point(HH,_,HV1),HH>H,!.
over_edge_of(HV1,grid(_,V)):- hv_point(_,VV,HV1),VV>V,!.

n_d_s(8,_,'~').
n_d_s(0,_,'0').

n_d_s(1,[[_],[],[],[]],'!').
n_d_s(1,[[],[_],[],[]],'=').
n_d_s(1,[[],[],[_],[]],'7').
n_d_s(1,[[],[],[],[_]],'`').
n_d_s(7,[[],[],_,_],'*').
n_d_s(7,[[],_,[],[]],'|').
n_d_s(7,[_,[],[],[]],'-').

n_d_s(_,[[_,_],[_,_],_,_],'#').
n_d_s(_,[_,_,[_,_],[_,_]],'X').

n_d_s(N,[A,B,_,_],'+'):- (A\==[],B\==[]),N\==5,!.

n_d_s(3,[[],[],[_,_],[_]],'/').
n_d_s(3,[[],[],[_],[_,_]],'\\').

n_d_s(2,[_,_,[ne],[se]],'<').
n_d_s(2,[_,_,[sw],[se]],'^').
n_d_s(2,[_,_,[ne],[nw]],'v').
n_d_s(2,[_,_,[sw],[nw]],'>').

n_d_s(_,[[_,_],[],_,_],'|').
n_d_s(_,[[],[_,_],_,_],'-').



n_d_s(_,[[],[],[_,_],[]],'/').
n_d_s(_,[[],[],[],[_,_]],'\\').

n_d_s(_,[[_,_],_,_,_],'|').
n_d_s(_,[_,[_,_],_,_],'-').
n_d_s(_,[[_],[_],_,_],'+').


n_d_s(_,[[],[e],[ne],[se]],'(').
n_d_s(_,[[],[w],[sw],[nw]],')').
n_d_s(_,[[s],[],[sw],[se]],'h').
n_d_s(_,[[n],[],[ne],[nw]],'y').

n_d_s(N,[_,_,[ne],[se]],'<'):- member(N,[5,3,2]).
n_d_s(N,[_,_,[sw],[se]],'^'):- member(N,[5,3,2]).
n_d_s(N,[_,_,[ne],[nw]],'v'):- member(N,[5,3,2]).
n_d_s(N,[_,_,[sw],[nw]],'>'):- member(N,[5,3,2]).

n_d_s(_,[[_],[],_,_],'|').
n_d_s(_,[[],[_],_,_],'-').
n_d_s(_,[_,_,[],[]],'+').

n_d_s(_,[[],[],_,_],'x').

n_d_s(N,_,S):- atom_number(S,N).

on_edge_become('=','-').
on_edge_become('!','|').
on_edge_become('`','\\').
on_edge_become('7','/').
on_edge_become('(','-').
on_edge_become(')','-').
on_edge_become('h','|').
on_edge_become('y','|').



cache_grid_objs1(Var,GID):- var(Var),!,ensure_indv_type(Var),cache_grid_objs1(Var,GID).
cache_grid_objs1(countz,GID):- !, ignore(ensure_texture(GID)).

cache_grid_objs1(Type,GID):-
  type_min_len(Type,MinLen),
  cache_grid_objs_minlen(Type,GID,MinLen).
cache_grid_objs1(_,_).

cache_grid_objs_minlen(Type,GID,MinLen):- 
   cmem(GID,HV1,C1), \+ omem(GID,HV1,_), \+ ok_adjacent(GID,HV1), 1>=MinLen, 
   new_obj_points(GID,Type,C1,[C1-HV1],1),!,
   cache_grid_objs1(Type,GID).

cache_grid_objs_minlen(Type,GID,MinLen):-
  cmem(GID,HV1,C1), type_may_have(GID,Type,HV1),
  continue_obj(C1,GID,Type,[HV1],Points,Len), Len>=MinLen,!,
  new_obj_points(GID,Type,C1,Points,Len),
  cache_grid_objs1(Type,GID).


continue_obj(C1,GID,Type,Points,Out,Len):- 
  cont_obj_execpt(C1,GID,Type,Points,Points,Out),
  length(Out,Len).

color_compat(_,C1,C1):-!.
color_compat(fg(_),C1,C2):- !, is_fg_color(C1),is_fg_color(C2). 
color_compat(colors_in(List),C1,C2):- member(C1,List),member(C2,List).

cont_obj_execpt(C1,GID,Type,[HV1|Points],Already,[HV1|Out]):-   
  is_adj_point_type(C1,Type,HV1,HV2),HV1\==HV2,  \+ member(HV2,Already), 
  cmem(GID,HV2,C2),color_compat(Type,C1,C2),ok_adjacent(GID,HV2),  
  type_may_have(GID,Type,HV2),!,
 cont_obj_execpt(C1,GID,Type,[HV2,HV1|Points],[HV2|Already],Out).
cont_obj_execpt(C1,GID,Type,[HV1|Points],Already,[HV1|Out]):-  !,
  cont_obj_execpt(C1,GID,Type,Points,Already,Out).
cont_obj_execpt(__,_GID,_Type,[],_Already,[]):-!.

type_min_len(colormass(N),N).
type_min_len(fg(N),N).
type_min_len(nsew(N),N).
%type_min_len(diamonds,3).




assert_omem_points(_,_,[]):-!.
assert_omem_points(GID,OID,[HV1]):-  !, assert_omem_point(GID,OID,HV1).
assert_omem_points(GID,OID,[HV1|Points]):- assert_omem_point(GID,OID,HV1), assert_omem_points(GID,OID,Points).

assert_omem_point(GID,OID,((S-C1)-HV1)):-
  assert_omem_point(GID,OID,C1-HV1),
  asserta_if_new(smem(GID,HV1,S)).
assert_omem_point(GID,OID,C1-HV1):-
  assert_omem_point(GID,OID,HV1),
  asserta_if_new(cmem(GID,HV1,C1)).
assert_omem_point(GID,OID,HV1):- 
 ignore((
   omem(GID,HV1,OOID),
   retractall(omem(GID,HV1,OOID)),
   update_object(OOID))),
   assert(omem(GID,HV1,OID)).

as_obj_gpoints(C1,[Point|Points],[GPoint|GPoints]):-
  as_obj_gpoint(C1,Point,GPoint),
  as_obj_gpoints(C1,Points,GPoints).
as_obj_gpoints(_C1,[],[]).


as_obj_gpoint(_C1,((S-C)-Point),C-Point):- nonvar(S),!.
as_obj_gpoint(_C1,((C)-Point),C-Point):- !.
as_obj_gpoint( C1,Point,C1-Point):- !.

new_obj_points(GID,Type,C1,Points,Len):- new_obj_points(GID,Type,C1,Points,Len,_OID).

new_obj_points(GID,Type,C1,Points,Len,OID):-
  as_obj_gpoints(C1,Points,GPoints),  
  Overrides =[],
  gpoints_to_iv_info(GPoints,LCLPoints,LocX,LocY,PenColors,Rot2L,Iv,Overrides,LPoints,Grid,SH,SV,SizeY,SizeX,CentX,CentY),
  _List=[shape_rep(grav,LCLPoints),loc2D(LocX,LocY),pen(PenColors),rot2D(Rot2L),iv(Iv),localpoints(LPoints),
    grid(Grid),rotSize2D(grav,SH,SV),viz2D(SizeY,SizeX),center2D(CentX,CentY)],
  int2glyph(Iv,Glyph),
  %name(Glyph,[Iv]),!,
  atomic_list_concat(['o_',Glyph,'_',Iv,'_',GID],OID),
  assert(gid_type_oid(GID,Type,OID)),
  assert_omem_points(GID,OID,Points),
  length(Points,Len),
  if_t(nonvar(C1),assert(cindv(OID,unique_color,element,OID,C1))),
  assert(cindv(OID,size,Len)).

update_object(OID):- 
   gid_type_oid(GID,Type,OID), 
   update_object(GID,Type,OID).

update_object(GID,_Type,OID):-
     \+ \+ omem(_,_,OID),!,     
     findall(_,omem(GID,_,OID),L),length(L,Len),
     retractall(cindv(OID,size,_)),
     assert(cindv(OID,size,Len)).

update_object(GID,Type,OID):-
  ignore(( 
    \+ omem(GID,_,OID),
    gid_type_oid(GID,Type,OID),
    %retractall(cindv(OID,_)),
    retractall(gid_type_oid(GID,Type,OID)),
    retractall(is_grid_obj_count(GID,_,_)),
    retract_object(GID,OID,_))).


new_omem_NEVER(GID,Type,OID):- 
  new_omem_NEVER(GID,Type,OID,_Glyph).
new_omem_NEVER(GID,Type,OID,Glyph):- 
  flag(GID,X,X+1),int2glyph(X,Glyph),
  name(Glyph,[ID]),!,
  atomic_list_concat(['o_',Glyph,'_',ID,'_',GID],OID),
  assert(gid_type_oid(GID,Type,OID)).



save_arc_db_temp_cache:-
  setup_call_cleanup(tell('muarc_cache/arc_db_temp_cache.pl'), write_arc_db_temp_cache, told).

write_arc_db_temp_cache:-
  write('
   :- dynamic(muarc_tmp:cached_tests/2).
   :- dynamic(muarc_tmp:cached_tests_hard/2).
   :- dynamic(muarc_tmp:test_info_cache/2).

  '),
  listing(cached_tests/2),
  listing(cached_tests_hard/2),
  listing(test_info_cache/2),


  listing(cindv/3),
  listing(cindv/4),
  listing(cindv/5),
  listing(cindv/6),
  listing(cmem_hv/4),
  listing(gid_type_oid/3),
  listing(is_grid_obj_count/3),
  listing(is_grid_size/3),
  listing(is_gridmass/2),
  listing(omem/3),
  listing(smem/3),
  listing(dmem),
  listing(nmem),
  listing(zmem),
  told.

load_arc_db_temp_cache:-
  load_files('muarc_cache/arc_db_temp_cache',[qcompile(auto)]).

:- dynamic(is_shape_id_for/2).
is_shape_id_for_init([hv(1,1)],sid_11).
is_shape_id_for_init([hv(1,1),hv(2,1)],sid_12).
is_shape_id_for_init([hv(1,1),hv(2,1),hv(3,1)],sid_13).
is_shape_id_for_init([hv(1,1),hv(2,1),hv(3,1),hv(4,1)],sid_14).

is_shape_id_for_init([],sid_0).

is_shape_id_for_init([hv(1,1),hv(1,2)],sid_21).
is_shape_id_for_init([hv(1,1),hv(2,1),hv(1,2),hv(2,2)],sid_22).
is_shape_id_for_init([hv(1,1),hv(2,1),hv(3,1),hv(1,2),hv(2,2),hv(3,2),hv(1,3),hv(2,3),hv(3,3)],sid_33).
is_shape_id_for_init([hv(1,1),hv(2,1),hv(3,1),hv(1,2),hv(3,2),hv(1,3),hv(2,3),hv(3,3)],sid_323).

assert_sid(Ps,ID):- my_maplist(hv_to_point,Ps,Points),assert_if_new(is_shape_id_for(Points,ID)).

hv_to_point(hv(H,V),Point):- hv_point(H,V,Point).
create_builtin_sids:- forall(is_shape_id_for_init(Ps,ID),assert_sid(Ps,ID)).

:- include(kaggle_arc_footer).
:- initialization(create_movements).
:- initialization(create_points_plus).
:- initialization(create_builtin_sids).

