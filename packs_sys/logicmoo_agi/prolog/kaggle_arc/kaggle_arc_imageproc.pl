
%tell(s),ignore((nl,nl,test_pairs(Name,ExampleNum,In,Out),format('~N~q.~n',[test_pairs_cache(Name,ExampleNum,In,Out)]),fail)),told.

is_grid_cell(C):- (var(C);is_grid_color(C)),!.
is_grid([[C|H]|R]):- notrace((is_grid_cell(C),is_list(H),is_list(R),
  length([C|H],L),
  maplist(is_row_len(L),R))).

is_row_len(N,L):- length(L,N).

:- dynamic(backfill/1).


set_on_grid(OH,OV,Grid,O):- is_object(O),globalpointlist(O,Ps),!,set_on_grid(OH,OV,Grid,Ps).
set_on_grid(OH,OV,Grid,List):- is_list(List),maplist(set_on_grid(OH,OV,Grid),List).
set_on_grid(OH,OV,Grid,C-Point):- nonvar(C),
  grid_color_code(C,I),
  hv_point(H,V,Point),
  HH is H - OH + 1, 
  VV is V - OV + 1,
  nth1(VV,Grid,Row),nb_set_nth1(HH,Row,I).


grid_dim(G,grid_size(H,W)):- grid_size(G,H,W).
%grid_size(O,offset_ranges(_,_,_,_,H,V)):- is_grid(O),grid_size(O,H,V).
%grid_size(P,S):- grid_size(P,S).
is_point_obj(O,Color,Point):- nonvar(O),O= Color-Point,!.
is_point_obj(O,Color,Point):- is_object(O),object_size(O,H,V), !, hv(H,V)==hv(1,1),
  globalpointlist(O,[Color-Point]),!.



props_of_points(E,Ns):- findall(obj(Ps),member(obj(Ps),E),Ns).

%is_objectlist([G|V]):- is_object(G),is_list(V),maplist(is_object,V).
is_objectlist([G|V]):- is_cpoint(G),is_list(V),maplist(is_cpoint,V).

black_first(SK,[color_count(Z,NC)|BF]):- is_black(Z), select(color_count(Z,NC),SK,BF),!.
black_first(BF,[color_count(Z,0.0)|BF]):- is_black(Z).

no_black(SK,BF):-select(color_count(Z,_),SK,BF),is_black(Z),!.
no_black(SK,BF):-select(Z,SK,BF),is_black(Z),!.
no_black(BF,BF).

is_color(C):- atom(C), color_int(C,N),integer(N).
is_black(black).
is_black(0).

pixels(G,CC):- is_grid(G),!,flatten(G,GF),include(is_grid_color,GF,GL),maplist(color_name,GL,CC).
pixels(G,GL):- findall(Name,(sub_term(CP,G),compound(CP),CP=(C-_),color_name(C,Name)),GL).
unique_colors(G,UC):- pixels(G,GF),sort(GF,UC).
colors_count_size(G,UC):- colors_count_no_black(G,GS),length(GS,UC).

into_cc(SK,BFO):- maplist(into_cc1,SK,BFO).
into_cc1(N-C,color_count(Nm,NC)):- NC is float(N),color_name(C,Nm).
colors_count_black_first(G,BF):- colors_count(G,SK),black_first(SK,BF).
colors_count_no_black(G,BF):- colors_count(G,SK),no_black(SK,BF).

num_objects(G,NO):- individuals(G,GS),length(GS,NO).

make_box(X,_,G):- make_unassigned_grid(X,X,G).

into_grid(G,G):- is_grid(G),!.
into_grid(Points,Grid):-
  grid_size(Points,GH,GV),
  make_unassigned_grid(GH,GV,Grid),
  get_bgc(BG),
  forall(between(1,GV,V),
   ((nth1(V,Grid,Row),forall(between(1,GH,H),      
     (hv_value_or(Points,NC,H,V,BG)->
        nc_to_cint(NC,C),
        nb_set_nth1(H,Row,C)))))),!.

into_grid(P,G):- points_to_grid(P,G),!.


move_above_itself(I,M):- move_dir_itself(1,n,I,M). 
move_rightof_itself(I,M):- move_dir_itself(1,e,I,M). 

%decl_pt(move_dir_itself(int,dir,object,+)).
%move_dir_itself(N,D,I,M):- check_args(move_dir_itself(N,D,I,M),MaybeCut),(MaybeCut==t->!;true).
move_dir_itself(N,D,I,M):- is_object(I),object_size(I,SX,SY), move_scale_dir_object(SX,SY,N,D,I,M).
move_dir_itself(N,D,L,LM):- is_objectlist(L),!,maplist(move_dir_itself(N,D),L,LM).
move_dir_itself(N,D,I,O):- into_objectlist(I,M),M\=@=I,!,move_dir_itself(N,D,M,O).

move_dir_object(N,D,I,M):- move_scale_dir_object(1,1,N,D,I,M).

move_scale_dir_object(X,Y,N,D,I,M):- is_object(I),!,
 must_det_l((
  object_offset(I,OX,OY),
  move_dir(N,OX,OY,D,X,Y,NX,NY),
  (NY<1 -> M=I ; move_object(NX,NY,I,M)))).
move_scale_dir_object(N,D,L,LM):- is_objectlist(L),!,maplist(move_scale_dir_object(N,D),L,LM).
move_scale_dir_object(N,D,I,O):- into_objectlist(I,M),M\=@=I,!,move_scale_dir_object(N,D,M,O).

move_object(NX,NY,I,M):- is_object(I),!,
 must_det_l((
  (NY<1 -> M=I ;
  ( localpointlist(I,LPoints),
    offset_points(NX,NY,LPoints,GPoints),
    setq(I,[globalpointlist(GPoints),object_offset(NX,NY)],M))))).
move_object(H,V,L,LM):- is_objectlist(L),!,maplist(move_object(H,V),L,LM).
move_object(H,V,I,O):- into_objectlist(I,M),M\=@=I,!,move_object(H,V,M,O).


is_black_or_bg(BG):- get_bgc(BG).
is_black_or_bg(black).
is_black_or_bg(0).



% S=[[1,2,3],[4,x,6],[7,8,0]],grow([[same,same],[same,same]],S, X).

% grow([[same,same]],[[a,b,c]], [[a,b,c,a,b,c]]).
append_left(Grid1,[],Grid1):-!.
append_left(Grid1,Grid2,Grid):- length(Grid1,Len),assertion(length(Grid2,Len)),maplist(append,Grid1,Grid2,Grid).
append_down(Grid1,Grid2,Grid):- append(Grid1,Grid2,Grid).

grow_row([],_,[]).
grow_row([C1],Grid,G1):- !, run_dsl(C1,Grid,G1).
grow_row([C1|Row],Grid,GM):- !, run_dsl(C1,Grid,G1),grow_row(Row,Grid,GR),append_left(G1,GR,GM).
grow([],_,[]).
grow([[Self]],Grid,GridO):- !, run_dsl(Self,Grid,GridO).
grow([Row|Rows],Grid,G1GridO):- grow_row(Row,Grid,G1), grow(Rows,Grid,GridO),append(G1,GridO,G1GridO).



largest_indiv(I,O):- into_objectlist(I,M),I\=@=M,!,largest_indiv(M,O).
largest_indiv(Grid,[Points]):- individuals(Grid,Is),largest_first(Is,[Points|_]).

smallest_indiv(I,O):- into_objectlist(I,M),I\=@=M,!,smallest_indiv(M,O).
smallest_indiv(Grid,Points):- individuals(Grid,Iss),largest_first(Iss,Is),remove_bgs(Is,IndvL,_BGIndvS),last(IndvL,Points).

background_indiv(I,O):- into_objectlist(I,M),I\=@=M,!,background_indiv(M,O).
background_indiv(Grid,BGIndvS):-  individuals(Grid,Is),remove_bgs(Is,_IndvL,BGIndvS).

/*
largest_indiv(Grid,Points):- individuals(Grid,[Points|_]).
largest_indiv(Points,Grid,Grid):- largest_indiv(Grid,Points).

smallest_indiv(Grid,Points):- individuals(Grid,Is),last(Is,Points),points_to_grid(Points,Points).
smallest_indiv(Points,Grid,Grid):- smallest_indiv(Grid,Points).
*/
  

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


trim_to_rect(G0,G9):-
 into_grid(G0,G),
 get_bgc(BG),
 trim_unused_vert(BG,G,G1),rot90(G1,G2),trim_unused_vert(BG,G2,G3),rot270(G3,G9).

  trim_unused_vert([],[]).
  trim_unused_vert(BG,[Row|Grid],GridO):- maplist(is_bg_or_var(BG),Row),trim_unused_vert(BG,Grid,GridO).
  trim_unused_vert(BG,GridR,GridO):- append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),trim_unused_vert(BG,Grid,GridO).
  trim_unused_vert(_,G,G).

%:- nb_setval(grid_bgc,8).

if_bgc_then_int(X,C,B,A):- \+compound(B),B\==[],is_bg_or_var(X,B), A=C, !.
set_bg(C0,Grid,GridO):- color_code(C0,CC),  nb_setval(grid_bgc,X), get_bgc(X),
  is_grid(Grid),!,grid_color_code(CC,GC), map_pred(if_bgc_then_int(X,GC), Grid, GridO),!.
set_bg(C0,Grid,GridO):- color_code(C0,C),  nb_setval(grid_bgc,X), get_bgc(X),map_pred(if_bgc_then(X,C), Grid, GridO),!.
  if_bgc_then(X,C,B,A):- \+compound(B),is_bg_or_var(X,B), A=C, !.

shave_away_1s(Grid,GridO):- individuals(Grid,Is), include(\=([_,_|_]),Is,I1s), remove_points(I1s,Grid,GridO).

remove_points([H|T],Grid,GridO):- !, remove_points(H,Grid,GridM),remove_points(T,GridM,GridO).
remove_points([],Grid,Grid):-!.
remove_points(Point,Grid,GridO):- as_hv_point(_,_,C,Point), nonvar(C), get_bgc(BG),set_point(BG,Point,Grid,GridO).
remove_points(Point,Grid,Grid):- wdmsg(warn(skip(remove_points(Point)))).

set_point(C,Point,Grid,GridO):- as_hv_point(H,V,_,Point),!,
  replace_point(H,V,C,Grid,GridO).


set_point([H|T],Grid,GridO):- !, set_point(H,Grid,GridM),set_point(T,GridM,GridO).
set_point([],Grid,Grid):-!.
set_point(Point,Grid,GridO):- as_hv_point(H,V,C,Point), replace_point(H,V,C,Grid,GridO).

replace_point(H,V,C,Grid,GridO):- duplicate_term(Grid,GridO),nth1(V,GridO,Row),nb_set_nth1(H,Row,C).

nb_set_nth1(1,Row,C):- !, nb_setarg(1,Row,C).
nb_set_nth1(N,[_|Row],C):- Nm1 is N -1, nb_set_nth1(Nm1,Row,C).

set_all_fg(C0,Grid,GridO):- color_code(C0,C),get_bgc(X),map_pred(if_not_bgc_then(X,C), Grid, GridO).
if_not_bgc_then(X,C,B,A):- is_color_dat(B), \+ is_bg_or_var(X,B), A=C, !.


:- nb_delete(grid_bgc).
set_bgc(C):- atom(C),color_code(C,N),C\==N,!,set_bgc(N).
set_bgc(C):- var(C),nb_delete(grid_bgc).
set_bgc(C):- nb_setval(grid_bgc,C),!.
get_bgco(X):- nb_current(grid_bgc,X),is_color_dat(X),!.

get_bgc(X):- get_bgco(X),!.
get_bgc(X):- is_black(X).


is_bg_or_var(_,X):- free_cell(X),!.
is_bg_or_var(BG,X):- X==BG.


free_cell(Var):- var(Var),!.
free_cell(C):- get_bgco(X),C==X.
%free_cell(0).
%free_cell(8).



%trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(BG,GridR,GridO):- append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(_,G,G).*/

learn_mapping_stateful(In,Out):- get_bgc(BG),
   must_det_l((
   maplist(unbind_color(BG),[In,Out],[In1,Out1]),
   maplist(colors_to_vars,[In1,Out1],[In2,Out2]), 
   In2=Out2,
   asserta_new(backfill(Out2)))).

rot90(Grid,NewGrid):-  rot180(Grid,GridM),rot270(GridM,NewGrid). 
rot180(Grid,NewGrid):- flipH(Grid,RowRev),flipV(RowRev,NewGrid).
rot270(Grid0,NewGrid):- into_grid(Grid0,Grid), get_colums(Grid,NewGrid).
flipH(Grid0,FlipH):- into_grid(Grid0,Grid), maplist(reverse,Grid,FlipH).
flipV(Grid0,FlipV):- into_grid(Grid0,Grid), reverse(Grid,FlipV).
rocketship(Grid,Grid).
apply_mapping_stateful(Grid,G):- into_grid(G,Grid),unbind_color(0,Grid,GridO),ignore(backfill_vars(GridO)).


gravity(N,D,G,GridNew):- into_grid(G,Grid),G\=@=Grid,!,gravity(N,D,Grid,GridNew).
gravity(1,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridNew).
gravity(N,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridM),(Grid\=@=GridM->(Nm1 is N-1,gravity(Nm1,n,GridM,GridNew));GridNew=GridM).
gravity(N,s,Grid,GridNew):-!,flipV(Grid,FlipV),gravity(N,n,FlipV,GridM),flipV(GridM,GridNew).
gravity(N,w,Grid,GridNew):-!,rot90(Grid,GridRot),gravity(N,n,GridRot,GridM),rot270(GridM,GridNew).
gravity(N,e,Grid,GridNew):-!,rot270(Grid,GridRot),gravity(N,n,GridRot,GridM),rot90(GridM,GridNew).

compute_max_color(Color1,Grid,Grid):- colors_count_no_black(Grid,[color_count(Color1,_)|_]).
compute_next_color(Color1,Grid,Grid):- colors_count_no_black(Grid,[_,color_count(Color1,_)|_]).

subst_color(Color1,Color2,Grid,NewGrid):- notrace((color_code(Color1,Num1),color_code(Color2,Num2),subst(Grid,Num1,Num2,NewGrid))).
blacken_color(Color1,Grid,NewGrid):- black_cell(Cell), subst_color(Color1,Cell,Grid,NewGrid).
swap_colors(Color1,Color2,Grid,NewGrid):- subst_color(Color1,Swap1,Grid,MGrid),
                                          subst_color(Color2,Color1,MGrid,NewGrid),
                                          color_code(Color2,Swap1).

backfill_vars(GridO):- clause(backfill(GridO),true).

unbind_color(Color1,Grid,GridO):- is_grid(Grid),!,grid_color_code(Color1,Num1),unbind_color0(Num1,Grid,GridO).
unbind_color(Color1,Grid,GridO):- color_code(Color1,Num1),subst(Grid,Num1,_,GridO).

unbind_color0(Num1,Grid,GridO):- is_list(Grid),!,maplist(unbind_color0(Num1),Grid,GridO).
unbind_color0(Num1,Num1,_):-!.
unbind_color0(_,Num1,Num1).

%colors_to_vars(G,GridNew):- is_grid(G,Grid),G\=@=Grid,!,colors_to_vars(Grid,GridNew).
colors_to_vars(Grid,GridO):- unique_colors(Grid,Colors),
  (is_grid(Grid)-> maplist(grid_color_code,Colors,B) ; color_code(Colors,B)),
  subst_list_vars(B,_,Grid,GridO).

  subst_list_vars([],[],A,A):-!.
  subst_list_vars([F|FF],[R|RR],S,D):-     
    !,subst(S,F,R,M), subst_list_vars(FF,RR,M,D).
/*
subst_list_vars(B,A,Grid,GridO):- is_list(Grid),!,maplist(subst_list_vars(B,A),Grid,GridO).
subst_list_vars(F,R,S,D):- nth1(N,F,E),E==S,nth1(N,R,D),!.
subst_list_vars(_,_,V,V).
*/

add_borders(C,G,GridNew):- into_grid(G,Grid),G\=@=Grid,!,add_borders(C,Grid,GridNew).
add_borders(Color1,Grid,GridO):- grid_color_code(Color1,Num1),!,add_borders_0(Num1,Grid,GridO),!.

add_borders_0(Num1,Grid,GridO):- 
 grid_size(Grid,H,V),
 must_det_l((
  replace_row_e(1,Num1,Grid,G1),
  replace_row_e(V,Num1,G1,G2),
  replace_col_e(1,Num1,G2,G3),
  replace_col_e(H,Num1,G3,GridO))).


cls_with(Color1,G,Grid):- into_grid(G,Old),grid_color_code(Color1,Num1),cls_with_0(Num1,Old,Grid),!.
%cls_with(Color1,Old,Grid):- color_code(Color1,Num1),cls_with_0(Num1,Old,Grid),!.
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

affected_by_gravity(E2),


gravity_1_n_0([],[]).
gravity_1_n_0([Row1,Row2|Grid],GridNew):- nth1(Col,Row1,E1),nth1(Col,Row2,E2),
  black_cell(E1), \+ black_cell(E2),
  set_nth1(Col,Row1,E2,Row1Mod),set_nth1(Col,Row2,E1,Row2Mod),
  gravity_1_n_0([Row1Mod,Row2Mod|Grid],GridNew).
gravity_1_n_0([Row1|Grid],[Row1|GridNew]):- gravity_1_n_0(Grid,GridNew).

black_cell(Cell):- is_black(Black),grid_color_code(Black,Cell).

replace_row(N,Row,Grid,NewGrid):- grid_size(Grid,H,V), replace_row(N,Row,Grid,H,V,NewGrid).
replace_row(N,Row,Grid,H,V,NewGrid):- N<0, NewN is V + N+1,!,replace_row(NewN,Row,Grid,H,V,NewGrid).
replace_row(N,Row,Grid,_,_,NewGrid):- set_nth1(N,Grid,Row,NewGrid).

replace_row_e(N,E,Grid,NewGrid):- grid_size(Grid,H,V), make_list(H,E,Row), replace_row(N,Row,Grid,H,V,NewGrid),!.
replace_col_e(N,E,Grid,NewGrid):- grid_size(Grid,H,V), make_list(V,E,Col), replace_col(N,Col,Grid,H,V,NewGrid).

replace_col(N,Col,Grid,NewGrid):- grid_size(Grid,H,V), replace_col(N,Col,Grid,H,V,NewGrid).
replace_col(N,Col,Grid,H,V,NewGrid):- N<0, NewN is H + N+1,!,replace_col(NewN,Col,Grid,H,V,NewGrid).

replace_col(N,Col,Grid,_,V,NewGrid):- Nm1 is N - 1, length(Col,V),maplist(replace_col_at_0(Nm1),Col,Grid,NewGrid).

replace_col_at_0(N,Col,Row,NewRow):- length(Left,N),append(Left,[_|Right],Row),append(Left,[Col|Right],NewRow).

facing_triangles(
[[xx,se,nw,xx],
 [se,se,nw,nw],
 [ne,ne,sw,sw],
 [xx,ne,sw,xx]]).

get_surround_3x3(Grid,H,V,Result):-
  surround_3x3(Template),maplist(get_dir_color(Grid,H,V),Template,Result).

get_dir_color(Grid,H,V,Dir,C):- move_dir(1,H,V,Dir,1,1,NX,NY), hv_value(Grid,NX,NY,C).

surround_3x3(
[[nw,n,ne],
  [w,c,e],
 [sw,s,se]]).
%star_grow_h(obj(L),

%grid_to_id(Grid,ID):- 

erase_grid(GID):- retractall(cmem(GID,_,_)), 
  forall(retract(cindv(GID,_,ID)), erase_grid(ID)).


assert_id_cells(ID,Points):- maplist(assert_id_cell(ID),Points).
assert_id_cell(ID,-(C,HV)):- assert(cmem(ID,HV,C)).
assert_hvc_cell(_,_,_,C):- var(C). % free_cell(C),!.
assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(cmem(ID,HV,C)).


:- dynamic(is_grid_size/3).
% Grid to_fast_workspace
assert_id_grid_cells(ID,Grid):-
 throw(all_in_emem(assert_id_grid_cells(ID,Grid))),
   grid_size(Grid,SH,SV),
   retractall(is_grid_size(ID,_,_)),
   assert(is_grid_size(ID,SH,SV)),
   erase_grid(ID),   
   forall(between(1,SH,H),
    forall(between(1,SV,V),
     ignore((hv_value(Grid,C,H,V),assert_hvc_cell(ID,H,V,C))))).


% Random Non Blk Eles
first_color(Grid1,C1):- sub_term(C1,Grid1),C1 \= 0,is_color(C1).

% Grid object_size/resize
make_lengths(N,L):- length(L,N).

points_range(Points,LoH,LoV,HiH,HiV,H,V):- calc_range(inf,inf,-inf,-inf,-inf,-inf,Points,LoH,LoV,HiH,HiV,H,V).

points_range(Points,offset_ranges(LoH,LoV,HiH,HiV,H,V)):- calc_range(inf,inf,-inf,-inf,-inf,-inf,Points,LoH,LoV,HiH,HiV,H,V).
% object_size(Points,object_size(H,V)):- points_range(Points,_LoH,_LoV,_HiH,_HiV,H,V).

close_color(brown,orange).
close_color(green,cyan).

%grid_size(Points,H,V):- is_dict(Points),!,Points.grid_size=grid_size(H,V).
grid_size(ID,H,V):- is_grid_size(ID,H,V),!.
%grid_size(Grid,H,V):- notrace(is_object(Grid)), !, object_size(Grid,H,V).
grid_size(Grid,H,V):- is_grid(Grid),grid_size_nd(Grid,H,V),!.
grid_size(Points,H,V):- pmember(grid_size(H,V),Points),ground(H-V),!.
grid_size(Grid,H,V):-  grid_size_nd(Grid,H,V),!.
grid_size(Points,H,V):- points_range(Points,_LoH,_LoV,_HiH,_HiV,H,V).
grid_size(_,30,30).


calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,Var,WLoH,WLoV,WHiH,WHiV,WH,WV):- var(Var),!.
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,grid_size(IH,IV),WLoH,WLoV,WHiH,WHiV,H,V):- !,
  max_min(WV,IV,V,_),max_min(WH,IH,H,_).
%calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,object_size(IH,IV),WLoH,WLoV,WHiH,WHiV,H,V):- !,
%  max_min(WV,IV,V,_),max_min(WH,IH,H,_).
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,[E|L],LoH,LoV,HiH,HiV,H,V):- !,
  calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,E,MLoH,MLoV,MHiH,MHiV,MH,MV),
  calc_range(MLoH,MLoV,MHiH,MHiV,MH,MV,L,LoH,LoV,HiH,HiV,H,V).
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,[],WLoH,WLoV,WHiH,WHiV,WH,WV).
calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,offset_ranges(ILoH,ILoV,IHiH,IHiV,IH,IV),LoH,LoV,HiH,HiV,H,V):- 
  max_min(WLoV,ILoV,_,LoV),max_min(WHiV,IHiV,HiV,_),max_min(WV,IV,V,_),
  max_min(WLoH,ILoH,_,LoH),max_min(WHiH,IHiH,HiH,_),max_min(WH,IH,H,_),!.

calc_range(WLoH,WLoV,WHiH,WHiV,WH,WV,Point,LoH,LoV,HiH,HiV,H,V):- as_hv_point(IH,IV,C,Point),!,
  ignore((nonvar(C),
  max_min(WLoV,IV,_,LoV),max_min(WHiV,IV,HiV,_),max_min(HiV,WV,V,_),
  max_min(WLoH,IH,_,LoH),max_min(WHiH,IH,HiH,_),max_min(HiH,WH,H,_))),!.

  
ap(scotch_patterns). ap(rug_patterns). ap(rougue_like). ap(space_invaders).
ap(shapes_on_black). ap(lines_on_black). ap(answer_keys). ap(repeating_codes).

ap(color_changes).
ap(holes).

ap(spins).  ap(contained). ap(sticky). ap(immobile). ap(mobile). ap(gravity).
ap(thick0). ap(thick1). ap(thick2). ap(thick3). 
ap(dashed).  ap(two_color). ap(multi_color). ap(monochrome).
ap(underneath). ap(painted_surface).
ap(movement_group). ap(controls_others).
ap(holds_dots).  ap(filler).  ap(blank). 

ap(changes).
ap(diagonal_line). ap(horizontal_line). ap(vertical_line). ap(open_edge). ap(container).  ap(ray).

ap(rotated45). ap(resizes). ap(diamond).
apv(square(len)). apv(round(h,w)). apv(triangle). apv(rectangular(h,w)). apv(polygon(sides)).
apv(localcolorlesspointlist(num)).  apv(facing(dir)). apv(min(n)). apv(max(n)).  apv(object_size(h,w)). apv(object_offset(h,w)). 
apv(scale(n)).  apv(ext_key(k)). apv(io_bud(k)). apv(linked_bud(k)).

apv(points_old([])).
apv(sub_points([])).


grid_size_nd([C,R|Rows],H,V):- 
   (var(Rows)->between(2,30,V);!), 
   length([C,R|Rows],V),
   (var(R)->between(1,30,H);true), 
   length(R,H),
   (is_list(C)->true;(length(C,H),maplist(make_lengths(H),Rows))).
grid_size_nd([L],H,(1)):- (var(L)->between(1,30,H);true), length(L,H).



points_to_grid(Points,Grid):- is_grid(Points),Points=Grid,!.
points_to_grid([Points|More],Grid):- is_grid(Points),Points=Grid,grid_size(Points,H,V),calc_add_points(H,V,Grid,More),!.
points_to_grid(Points,Grid):- 
  once((
  notrace(grid_size(Points,H,V)),
  make_unassigned_grid(H,V,Grid),
  dmsg(calc_add_points(1,1,Grid,Points)),
  calc_add_points(1,1,Grid,Points))).

calc_add_points(OH,OV,_,Obj):- var(Obj),dumpST,throw(var_calc_add_points(OH,OV,Obj)).
calc_add_points(OH,OV,Grid,Obj):- is_grid(Obj),globalpointlist(Obj,Points),maplist(calc_add_points(OH,OV,Grid),Points).
calc_add_points(OH,OV,Grid,Points):- is_list(Points),!,maplist(calc_add_points(OH,OV,Grid),Points).
calc_add_points(_OH,_OV,_,obj(_)).
calc_add_points(_OH,_OV,_,prop(_)).
calc_add_points(_OH,_OV,_,offset_ranges(_,_,_,_,_,_)).
calc_add_points(OH,OV,Grid,Point):- as_hv_point(H,V,C,Point),HH is H -OH +1, VV is V - OV +1,  add_h_v_c(Grid,HH,VV,C).
%add_h_v_c(Grid,H,V,C):- var(C),!,nop(add_h_v_c(Grid,H,V,C)).
add_h_v_c(Grid,H,V,C):- hv_value(Grid,Was,H,V),ignore(Was=C).
as_hv_point(H,V,C,C-Point):- hv_point(H,V,Point),!.
as_hv_point(H,V,_,Point):- hv_point(H,V,Point),!.
as_hv_point(H,V,_,H,V).
%as_hv_point(inf,inf,offset_ranges(_,_,_,_)).


make_unassigned_grid(H,V,Grid):- grid_size_nd(Grid,H,V),!.

make_grid(H,V,grid{g:Grid,indv:nil,istyle:nil,bgc:BG,start:Start,end:End,
  todo_grid:[],todo_indvs:[guess_istyle],todo_points:[make_points]}):-
  assertion(ground(make_grid(H,V))),
  hv_point(1,1,Start),
  hv_point(H,V,End),
  make_unassigned_grid(H,V,Grid),get_bgc(BG).

insert_row(N,Row,Grid,NewGrid):- grid_size(Grid,H,V), insert_row(N,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,H,V,NewGrid):- N<0, NewN is V + N+1,!,insert_row(NewN,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,H,_,NewGrid):- length(Row,H),length(Left,N),append(Left,Right,Grid),append(Left,[Row|Right],NewGrid).

insert_col(N,Col,Grid,NewGrid):- grid_size(Grid,H,V), insert_col(N,Col,Grid,H,V,NewGrid).
insert_col(N,Col,Grid,H,V,NewGrid):- N<0, NewN is H + N+1,!,insert_col(NewN,Col,Grid,H,V,NewGrid).
insert_col(N,Col,Grid,_,V,NewGrid):- length(Col,V),maplist(insert_col_at(N),Col,Grid,NewGrid).

insert_col_at(N,Col,Row,NewRow):- length(Left,N),append(Left,Right,Row),append(Left,[Col|Right],NewRow).

insert_ele(N,V,L,NL):- length(Left,N),append(Left,Right,L),append(Left,[V|Right],NL).

delete_row(N,Grid,NewGrid):- N < 0, length(Grid,L), DR is L+N+1,delete_row(DR,Grid,NewGrid).
delete_row(N,Grid,NewGrid):- length(Left,N),append(Left,[_|Right],Grid),append(Left,Right,NewGrid).

delete_col(N,Grid,NewGrid):- maplist(delete_row(N),Grid,NewGrid).

map_nth(P,N,Grid):- nth1(N,Grid,E),call(P,E).
map_row(P,N,Grid):- map_nth(maplist(P),N,Grid).
map_col(P,N,Grid):- maplist(map_nth(P,N),Grid).


is_list_of_gridoids([G|V]):- \+ is_grid([G|V]), is_gridoid(G), is_list(V), maplist(is_gridoid,V).

is_gridoid(G):- var(G),!, fail.
is_gridoid(G):- is_grid(G),!.
is_gridoid(G):- is_object(G),!.
%is_gridoid(G):- is_cpoint(G),!.
is_gridoid(G):- is_list_of_gridoids(G).

maybe_glyph(G,_,GN):- is_grid(G),grid_dot(GN),!.
maybe_glyph(G,_,Code):- object_indv_id(G,_Tst,GN),nonvar(GN),!,i_sym(GN,Code).
maybe_glyph(_G,N,Code):-i_sym(N,Code).

from_gridoid(Points,C,GN,H,V):- from_gridoid(Points,C,N,H,V,G), maybe_glyph(G,N,GN).
from_gridoid(Points,C,N,H,V,G):- nth0(N,Points,G),hv_value0(G,C,H,V),nonvar(C),C\==black,C\==0,!.
from_gridoid(Points,C,N,H,V,G):- nth0(N,Points,G),hv_value0(G,C,H,V),nonvar(C),!.
from_gridoid(Points,C,N,H,V,G):- nth0(N,Points,G),hv_value0(G,C,H,V).

hv_value0(O,Color,H,V):- is_object(O),globalpointlist(O,Ps),hv_value(Ps,Color,H,V),!.
hv_value0(O,Color,H,V):- hv_value(O,Color,H,V).

hv_value_or(Grid,C,H,V,Else):- hv_value(Grid,C,H,V)*->true;C=Else.
hv_value(ID,C,H,V):- cmem(ID,HV,C),hv_point(H,V,HV),!.
hv_value(Points,C-N,H,V):- is_list(Points), is_list_of_gridoids(Points), from_gridoid(Points,CC,N,H,V),grid_color_code(CC,C),!.
hv_value(Grid,Color,H,V):- is_grid(Grid),nth1(V,Grid,Row),nth1(H,Row,C),color_name(C,Color),!.
hv_value(O,Color-GN,H,V):- is_object(O),globalpointlist(O,Ps),hv_value(Ps,Color,H,V),object_indv_id(O,_Tst,GN),nonvar(GN),!.
hv_value(O,Color,H,V):- is_object(O),globalpointlist(O,Ps),hv_value(Ps,Color,H,V),!.
%hv_value(Points,C,H,V):- nonvar(H),!, hv_point(H,V,HV), sub_term(C-HV,Points),atom(HV).
hv_value(Points,C,H,V):- nonvar(H),nonvar(V),hv_point(H,V,HV),!, hv_member(HV,C,Points),!.
hv_value(Points,C,H,V):- member(C-HV,Points),hv_point(H,V,HV).

hv_member(HV,C,O):- is_object(O),!,globalpointlist(O,Ps),hv_member(Ps,C,HV),!.
hv_member(HV,C,Points):- member(C-HV,Points),!.
hv_member(HV,C,Points):- sub_term(C-HV,Points),!.
/*b_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),set_nth1(H,Row,C).
nb_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),nb_set_nth1(H,Row,C).
b_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),rplc_nth1(H,Row,OldC,NewC).
nb_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),nb_rplc_nth1(H,Row,OldC,NewC).

*/



is_object(obj(O)):- is_list(O).
%is_object(H):- is_list(H),maplist(is_cpoint,H).

is_cpoint(C):- \+ compound(C),!,fail.
is_cpoint(offset_ranges(C,_,_,_,_,_)):- !, nonvar(C).
is_cpoint(obj(_)).
is_cpoint(prop(_)).
is_cpoint(prop(_,_)).
is_cpoint(C-P):- is_color(C),!,atom(P).
is_cpoint(C-P):- var(C),!,atom(P).


create_movements:- 
 forall( between(1,30,H),
  forall(between(1,30,V),
  calc_movement(H,V))).

:- initialization(create_movements).

calc_movement(H,V):- forall(nav(Dir,HO,VO), save_calc_movement(H,V,Dir,HO,VO)).

save_calc_movement(H,V,Dir,HO,VO):- H2 is HO+H, V2 is VO+V,
  ignore((between(1,30,H2), between(1,30,V2), 
     format(atom(HV),'point_~|~`0t~d~2+_~|~`0t~d~2+',  [H,V]),
    format(atom(HV2),'point_~|~`0t~d~2+_~|~`0t~d~2+', [H2,V2]),
    assert_if_new(is_adjacent_point(HV,Dir,HV2)),
    assert_if_new(hv_point(H,V,HV)),
    assert_if_new(is_adjacent_point(H,V,Dir,H2,V2)))).
  

nav(s,0,1). nav(e, 1,0). nav(w,-1,0). nav(n,0,-1).
nav(se, 1,1). nav(sw,-1,1). nav(nw,-1,-1). nav(ne, 1,-1).
nav(c,0,0).

move_dir(N,OX,OY,Dir,SX,SY,NX,NY):- nav(Dir,X,Y), NX is OX + (X*SX*N), NY is OY + (Y*SY*N).

reverse_dir(D,R):- nav(D,X,Y),RX is -X, RY is -Y,nav(R,RX,RY).

is_non_diag(X):- nav(X,0,_);nav(X,_,0).
is_diag(D):- nav(D,X,Y),X\==0,Y\==0. % \+ is_non_diag(X).
turn_left_45(s,sw). turn_left_45(sw,w). turn_left_45(w,nw). turn_left_45(nw,n). 
turn_left_45(n,ne). turn_left_45(ne,e). turn_left_45(e,se). turn_left_45(se,s).
turn_right_45(X,Y):-turn_left_45(Y,X).


copy_cells(B,A,H,HH):- call(B,H),!,call(A,HH).
copy_cells(_,_,H,H):- \+ is_list(H),!.
copy_cells(_,_,[],[]):-!. 
copy_cells(B,A,[H|T],[HH|TT]):-!, copy_cells(B,A,H,HH), copy_cells(B,A,T,TT).
  

same_grid(Grid1,Grid1).

