/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- dynamic(row_mem/34).

erase_grid(GID):- retractall(cmem(GID,_,_)), 
  forall(retract(cindv(GID,_,ID)), erase_grid(ID)).


assert_id_cells(ID,Points):- maplist(assert_id_cell(ID),Points).
assert_id_cell(ID,-(C,HV)):- assert(cmem(ID,HV,C)).
assert_hvc_cell(_,_,_,C):- plain_var(C). % free_cell(C),!.
assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(cmem(ID,HV,C)).


:- dynamic(is_grid_size/3).
% Grid to_fast_workspace

assert_id_grid_cells(ID,Grid):-
 %throw(all_in_emem(assert_id_grid_cells(ID,Grid))),
   grid_size(Grid,SH,SV),
   erase_grid(ID),
   retractall(is_grid_size(ID,_,_)),
   assert(is_grid_size(ID,SH,SV)),
   assert_id_grid_cells2(ID,SH,SV,Grid).
   

assert_id_grid_cells2(ID,_SH,_SV,Grid):- is_grid(Grid),!,
 forall(nth1(N,Grid,Row),
   (list_to_row_mem(ID,N,Row,Mem),assert(Mem))).

assert_id_grid_cells2(ID,SH,SV,Grid):-
   forall(between(1,SH,H),
    forall(between(1,SV,V),
     ignore((hv_c_value(Grid,C,H,V),assert_hvc_cell(ID,H,V,C))))).


get_color_at(H,V,Grid,C):-
  nth1(V,Grid,Row),nth1(H,Row,C).
get_color_at(Point,Grid,C):-
  hv_point(H,V,Point),
  nth1(V,Grid,Row),nth1(H,Row,C).

list_to_row_mem(ID,N,List, row_mem(ID,N,A01, A02, A03, A04, A05, A06, A07, A08, A09, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32)):-
  my_append(List,_,[A01, A02, A03, A04, A05, A06, A07, A08, A09, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32]).

:- dynamic(row_mem_access/35).

row_mem_access([],ID,N, B01, B02, B03, B04, B05, B06, B07, B08, B09, B10, B11, B12, B13, B14, B15, 
  B16, B17, B18, B19, B20, B21, B22, B23, B24, B25, B26, B27, B28, B29, B30, B31, B32):-!,
  row_mem(ID,N, B01, B02, B03, B04, B05, B06, B07, B08, B09, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21, B22, B23, B24, B25, B26, B27, B28, B29, B30, B31, B32).
 
row_mem_access([post_xform(More,P2)],ID,N, B01, B02, B03, B04, B05, B06, B07, B08, B09, B10, B11, B12, B13, B14, B15, 
 B16, B17, B18, B19, B20, B21, B22, B23, B24, B25, B26, B27, B28, B29, B30, B31, B32):-!,
 row_mem_access(More,ID,N,A01, A02, A03, A04, A05, A06, A07, A08, A09, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32),
  xform(P2,A01,B01), xform(P2,A02,B02), xform(P2,A03,B03), xform(P2,A04,B04), xform(P2,A05,B05), xform(P2,A06,B06), 
  xform(P2,A07,B07), xform(P2,A08,B08), xform(P2,A09,B09), xform(P2,A10,B10), xform(P2,A11,B11), xform(P2,A12,B12), 
  xform(P2,A13,B13), xform(P2,A14,B14), xform(P2,A15,B15), xform(P2,A16,B16), xform(P2,A17,B17), xform(P2,A18,B18), 
  xform(P2,A19,B19), xform(P2,A20,B20), xform(P2,A21,B21), xform(P2,A22,B22), xform(P2,A23,B23), xform(P2,A24,B24), 
  xform(P2,A25,B25), xform(P2,A26,B26), xform(P2,A27,B27), xform(P2,A28,B28), xform(P2,A29,B29), xform(P2,A30,B30),
  xform(P2,A31,B31), xform(P2,A32,B32).

row_mem_access([xform(P2)|More],ID,N, A01, A02, A03, A04, A05, A06, A07, A08, A09, A10, A11, A12, A13, A14, A15, 
 A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32):- !,
 xform(P2,A01,B01), xform(P2,A02,B02), xform(P2,A03,B03), xform(P2,A04,B04), xform(P2,A05,B05), xform(P2,A06,B06), 
 xform(P2,A07,B07), xform(P2,A08,B08), xform(P2,A09,B09), xform(P2,A10,B10), xform(P2,A11,B11), xform(P2,A12,B12), 
 xform(P2,A13,B13), xform(P2,A14,B14), xform(P2,A15,B15), xform(P2,A16,B16), xform(P2,A17,B17), xform(P2,A18,B18), 
 xform(P2,A19,B19), xform(P2,A20,B20), xform(P2,A21,B21), xform(P2,A22,B22), xform(P2,A23,B23), xform(P2,A24,B24), 
 xform(P2,A25,B25), xform(P2,A26,B26), xform(P2,A27,B27), xform(P2,A28,B28), xform(P2,A29,B29), xform(P2,A30,B30),
 xform(P2,A31,B31), xform(P2,A32,B32),
 row_mem_access(More,ID,N,B01, B02, B03, B04, B05, B06, B07, B08, B09, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21, B22, B23, B24, B25, B26, B27, B28, B29, B30, B31, B32).

row_mem_access([rev|More],ID,N,A01, A02, A03, A04, A05, A06, A07, A08, A09, A10, A11, A12, A13, A14, A15, 
   A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32):-!,
   row_mem_access(More,ID,N,A32, A31, A30, A29, A28, A27, A26, A25, A24, A23, A22, A21, A20, A19, A18, A17, A16, A15, A14, A13, A12, A11, A10, A09, A08, A07, A06, A05, A04, A03, A02, A01).  

:- 
  length(S1,32),length(S2,32),length(MissingH,32),% length(BLANK,32),
  RMA = row_mem_access,
  forall(between(0,31,I),
 must_det_ll((length(Left,I),length(Gather,I),
   H is I + 1,
   H = W,
   length(USE,W),
   my_append(Left,S1,All),   
   my_append(S2,_,All),
   my_append(USE,_,All),

   my_append(Gather,[C|Right],S2),
   my_append(Gather,[_|Right],MissingH),
  AS1=..[row_mem,ID,V|S1],
  AS2=..[row_mem,ID2,V|S2],
  CH=..[row_mem,ID,V|S2],
  BLANKH=..[row_mem,ID,V|MissingH],
  %ASBLANK=..[row_mem,ID,V|BLANK],

  SL2=..[RMA,[shift_l(I)|More],ID,V|S2],
  SA1=..[RMA,More,ID,V|S1],
  assert_if_new((SL2 :- SA1)),
  SR1=..[RMA,[shift_r(I)|More],ID,V|S1],
  SA2=..[RMA,More,ID,V|S2],
  assert_if_new((SR1 :- SA2)),

  assert_if_new((row_mem_len_list(ID,V,W,USE) :- AS1)),  
  assert_if_new((row_mem_shift_match(I,ID,V,ID2):- AS1,AS2)),
  assert_if_new((row_mem_dig(ID,V,W,AS1):- AS1)),
  assert_if_new((row_mem_nth(ID,H,V,C):- AS1)),
  assert_if_new((row_mem_set_nth(ID,H,V,C):- ignore(retract(BLANKH)),assert(CH))),
  assert_if_new((row_mem_offset(AS1,AS2))),
  true))).

xform(P2,X,Y):- call(P2,X,Y).

row_mem_get_list(ID,N,USE) :- is_grid_size(ID,H,_),row_mem_len_list(ID,N,H,USE).

point_to_hvc(Point, H,V,_):- atomic(Point),!, hv_point(H,V,Point),!.
point_to_hvc(C-Point,H,V,C):- must(nonvar(Point)),must(hv_point(H,V,Point)),!.
%point_ to_hvc(H,V,_,H,V).
%point_ to_hvc(Inf,Inf,offset_ranges(_,_,_,_)).

make_grid(H,V,Grid):- max_min(H,1,HH,_), max_min(V,1,VV,_),
   max_min(HH,32,_,HHH),max_min(VV,32,_,VVV),!,   
   grid_size_nd(Grid,HHH,VVV),!.

insert_row(N,Row,Grid,NewGrid):- grid_size(Grid,H,V), insert_row(N,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,H,V,NewGrid):- N<0, NewN is V + N+1,!,insert_row(NewN,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,H,_,NewGrid):- length(Row,H),length(Left,N),my_append(Left,Right,Grid),my_append(Left,[Row|Right],NewGrid).

insert_col(N,Col,Grid,NewGrid):- grid_size(Grid,H,V), insert_col(N,Col,Grid,H,V,NewGrid).
insert_col(N,Col,Grid,H,V,NewGrid):- N<0, NewN is H + N+1,!,insert_col(NewN,Col,Grid,H,V,NewGrid).
insert_col(N,Col,Grid,_,V,NewGrid):- length(Col,V),maplist(insert_col_at(N),Col,Grid,NewGrid).


insert_col_at(N,Col,Row,NewRow):- length(Left,N),my_append(Left,Right,Row),my_append(Left,[Col|Right],NewRow).

insert_ele(N,V,L,NL):- length(Left,N),my_append(Left,Right,L),my_append(Left,[V|Right],NL).

delete_row(N,Grid,NewGrid):- N < 0, length(Grid,L), DR is L+N+1,delete_row(DR,Grid,NewGrid).
delete_row(N,Grid,NewGrid):- length(Left,N),my_append(Left,[_|Right],Grid),my_append(Left,Right,NewGrid).

delete_col(N,Grid,NewGrid):- maplist(delete_row(N),Grid,NewGrid).

map_nth(P,N,Grid):- nth1(N,Grid,E),call(P,E).
map_row(P,N,Grid):- map_nth(maplist(P),N,Grid).
map_col(P,N,Grid):- maplist(map_nth(P,N),Grid).


maybe_glyph(G,_,Glyph):- is_object(G), object_glyph(G,Glyph), !.
maybe_glyph(_G,N,Code):- i_glyph(N,Code),!.
maybe_glyph(G,_,Glyph):- is_grid(G),grid_dot(Glyph),!.
maybe_glyph(_,N,N).


grid_color_and_glyph(Points,C,GN,H,V):- %is_object_group(Points), 
  smallest_first(Points,ObjList),
  gridoid_color(Points,C,H,V),
  gridoid_glyph(ObjList,GN,H,V),!.
grid_color_and_glyph(Points,C,GN,H,V):- %is_object_group(Points), 
  gridoid_color(Points,C,H,V),
  gridoid_glyph(Points,GN,H,V),!.

gridoid_color(Points,C,H,V):-
 ((nb_current(color_index,ObjList),ObjList\==[e]) -> ObjList=List ; Points=List),
  from_gridoid(List,C,_N,H,V,_G),!.

gridoid_glyph(Points,GN,H,V):-
 ((nb_current(glyph_index,ObjList),ObjList\==[e]) -> ObjList=List ; Points=List),
  from_gridoid(List,_,N,H,V,G), maybe_glyph(G,N,GN).

%from_gridoid(Points,C,GN,H,V):- from_gridoid(Points,C,N,H,V,G), maybe_glyph(G,N,GN).
%from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G),hv_c_value(G,C,H,V),nonvar_or_ci(C), \+ is_bg_color(C), \+ bg_sym(C), !.
%from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G),hv_c_value(G,C,H,V),nonvar_or_ci(C),!.
from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G),hv_c_value(G,C,H,V).


%hv_c_value(O,_Color,_H,_V):- is_object(O), iz(O,combined), !, fail.
hv_c_value(ID,C,H,V):- (var(H);var(V)),!,dumpST,trace, hv_point(H,V,_),hv_c_value(ID,CC,H,V),CC=C.
hv_c_value(O,Color,H,V):- is_grid(O),!,nth1(V,O,Row),nth1(H,Row,Color),!.
hv_c_value(O,Color,H,V):- is_list_of(is_cpoint,  O),!,hv_point(H,V,Point),member(Color-Point,O).
hv_c_value(O,fg   ,H,V):- is_list_of(is_nc_point,O),!,hv_point(H,V,Point),member(Point,O).
hv_c_value(O,Color,H,V):- is_cpoint(O),!,O=(Color-Point),hv_point(H,V,Point),!.
hv_c_value(O,fg   ,H,V):- is_nc_point(O),!,O=Point,hv_point(H,V,Point),!.
hv_c_value(O,Color,H,V):- is_object(O),!,globalpoints(O,Ps),hv_c_value(Ps,Color,H,V).
%hv_c_value(G,Color,H,V):- is_group(G),!,into_list(G,L),member(E,L),hv_c_value(E,Color,H,V),!.
hv_c_value(G,Color,H,V):- my_assertion(into_list(G,L)),!,member(E,L),hv_c_value(E,Color,H,V),!.
%hv_c_value(O,Color,H,V):- is_object(O),localpoints(O,Ps),hv_c_value(Ps,Color,H,V).
%hv_c_value(L,Color,H,V):- is_list(L), member(E,L),hv_c_value(E,Color,H,V),!.

hv_cg_value_or(Grid,C,_H,_V,Else):- Grid==[],!,Else=C.
hv_cg_value_or(Grid,C,H,V,Else):- hv_cg_value(Grid,C,H,V)*->true;C=Else.

hv_c_value_or(Grid,C,_H,_V,Else):- Grid==[],!,Else=C.
hv_c_value_or(Grid,C,H,V,Else):- hv_c_value(Grid,C,H,V)*->true;C=Else.

pgt:- clsmake,!,pgt(Obj),once(print_grid(Obj)).

pgt(Obj):- pgt1(Obj).
pgt(Obj):- pgt2(Obj).
pgt(Obj1-Obj2):- pgt1(Obj1),pgt2(Obj2).
pgt([Obj1]-[Obj2]):- pgt1(Obj1),pgt2(Obj2).
pgt([Obj1,Obj2]):- pgt1(Obj1),pgt2(Obj2).
pgt1(Obj):-
  Obj = obj( [ mass(536),
         shape( [ point_01_01, point_02_01]),
         colors( [ cc(red, 190.0), cc(silver, 132.0), cc(green, 55.0), cc(cyan, 53.0),
                   cc(blue, 45.0), cc(yellow, 36.0), cc(orange, 25.0)]),
         localpoints( [ red-point_01_01, silver-point_02_01]), vis_hv(3, 1), rotation(same), loc_xy(3, 1),
         changes([]), iz(combined),
         iz(rectangle), iz(multicolored),
         iz(polygon), object_indv_id(v('0ad4ef5')*(trn+0)*in, 21),
       %  globalpoints( [ red-point_01_01, silver-point_02_01]),
         grid_size(8, 8)]).

pgt2(Obj):- Obj = 
      obj( [ mass(536),
         shape( [ point_01_01, point_02_01]),
         colors( [ cc(red, 190.0), cc(silver, 132.0), cc(green, 55.0), cc(cyan, 53.0),
                   cc(blue, 45.0), cc(yellow, 36.0), cc(orange, 25.0)]),
         localpoints( [ red-point_01_01, silver-point_02_01]), vis_hv(3, 1), rotation(same), loc_xy(2, 1),
         changes([]), iz(combined),
         iz(rectangle), iz(multicolored),
         iz(polygon), object_indv_id(v('a1d4ef5')*(trn+0)*in, 66),
        %  globalpoints( [ red-point_01_01, silver-point_02_01]),
         grid_size(8, 8)]).

%hv_cg_value(ID,C,H,V):- row_mem_nth(H,ID,V,C).

has_index(CI):-  nb_current(CI,List),List\==[],is_list(List),!.


hv_cg_value(ID,C,H,V):- (var(H);var(V)),!, hv_point(H,V,_),hv_cg_value(ID,CC,H,V),CC=C.
hv_cg_value(O,GN,H,V):- is_dict(O),append([O.objs],[O.grid],I),!,hv_cg_value(I,GN,H,V).
hv_cg_value(O,Color-GN,H,V):- is_object(O),hv_c_value(O,Color,H,V),object_indv_id(O,_Tst,GN),nonvar_or_ci(GN),!.
hv_cg_value(Grid,Color,H,V):- is_grid(Grid),!,hv_c_value(Grid,Color,H,V).
hv_cg_value([G|Points],CN,H,V):- quietly(( is_list(Points), is_object_or_grid(G))), 
   grid_color_and_glyph([G|Points],C,N,H,V),CN=(C-N),!.
%hv_cg_value(O,CN,H,V):- (has_index(color_index);has_index(glyph_index)),
%   grid_color_and_glyph(O,C,N,H,V),CN=(C-N),!.


%hv_cg_value(ID,C,H,V):- nonvar(H) -> (hv_point(H,V,HV),cmem(ID,HV,C)); (cmem(ID,HV,C),hv_point(H,V,HV)).

%hv_cg_value(Points,C,H,V):- is_list(Points),member(C-HV,Points),hv_point(H,V,HV).

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
replace_global_point_color(Point,NewC,OldC,G,GO):- is_list(G),!, maplist(replace_global_point_color(Point,NewC,OldC),G,GO).
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
replace_local_point_color(Point,NewC,OldC,G,GO):- is_list(G),!, maplist(replace_local_point_color(Point,NewC,OldC),G,GO).
replace_local_point_color(Point,NewC,OldC,G,GO):- is_object(G), !,
    localpoints(G,Points),     
    replace_in_points(Point,NewC,OldC,Points,RPoints),
    %loc_xy(G,OH,OV),offset_point(OH,OV,Point,LPoint),shape(G,NCPoints), maplist(replace_in_points(Point,NewC,OldC),NCPoints,RNCPoints),,shape(RNCPoints)
    setq(G,localpoints(RPoints),GO).
replace_local_point_color(Point,NewC,OldC,G,GO):- trace_or_throw(unknown_target_type(replace_local_point_color(Point,NewC,OldC,G,GO))).





create_movements:- 
 forall( between(1,30,H),
  forall(between(1,30,V),
  calc_movement(H,V))).

:- initialization(create_movements).

:- dynamic(hv_point/3).
:- dynamic(is_adjacent_point/3).
:- dynamic(is_adjacent_hv/5).
:- export(hv_point/3).
:- export(is_adjacent_point/3).
:- export(is_adjacent_hv/5).

hv(H,V,hv(H,V)).

calc_movement(H,V):- forall(nav(Dir,HO,VO), save_calc_movement(H,V,Dir,HO,VO)).

save_calc_movement(H,V,Dir,HO,VO):- H2 is HO+H, V2 is VO+V,
  muarc_mod(M),
  ignore((between(1,32,H2), between(1,32,V2), 
     format(atom(HV),'point_~|~`0t~d~2+_~|~`0t~d~2+',  [H,V]),
    format(atom(HV2),'point_~|~`0t~d~2+_~|~`0t~d~2+', [H2,V2]),
    assert_if_new(M:is_adjacent_point(HV,Dir,HV2)),
    assert_if_new(M:hv_point(H,V,HV)),
    assert_if_new(M:is_adjacent_hv(H,V,Dir,H2,V2)))).
  
is_adjacent_2points(HV,Dir,HV2,HV3):-  is_adjacent_point(HV,Dir,HV2),is_adjacent_point(HV2,Dir,HV3).

:- fixup_exports.

