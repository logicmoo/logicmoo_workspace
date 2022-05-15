:- encoding(iso_latin_1).
:- set_prolog_flag(encoding,iso_latin_1).
:- dynamic('$exported_op'/3).
:- multifile('$exported_op'/3).
:- system:ensure_loaded(library(logicmoo_common)).
%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).
:- dynamic((fav/2,ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).
load_json_files(F,Mask):- 
  absolute_file_name(Mask,AbsMask),
  expand_file_name(AbsMask,FullNames),
  maplist(file_base_name,FullNames,BaseNames),
  maplist(file_name_extension,Names,_,BaseNames),
  maplist(load_json_file(F),Names,FullNames),!.

load_json_file(F, BaseName, FullName):- Name=..[F,BaseName], 
  setup_call_cleanup(open(FullName,read,In),
   json_read(In,Term,[]),
   close(In)),
  load_json_of_file(Name,file,Term),!.

  load_json_of_file(Name,Type,json(Term)):-! , load_json_of_file(Name,Type,Term).
  load_json_of_file(Name,_,Type=Value):-!, load_json_of_file(Name,Type,Value).
  load_json_of_file(Name,train,T):-!,load_json_of_file(Name,trn,T).
  load_json_of_file(Name,test,T):-!,load_json_of_file(Name,tst,T).
  load_json_of_file(Name,Type,[input=In,output=Out]):-assert_if_new(kaggle_arc_json(Name,Type,In,Out)),!.
  load_json_of_file(Name,Type,[H|T]):- !, forall(nth0(N,[H|T],E), load_json_of_file(Name,Type+N,E)).
  load_json_of_file(N,T,V):- wdmsg(load_json_of_file(N,T,V)),!.

:- load_json_files(t,'./data/training/*.json').
:- load_json_files(v,'./data/evaluation/*.json').
kaggle_arc(TName,Type,In,Out):- kaggle_arc_json(TName,Type,In,Out).
fix_test_name(X,X):- var(X),!.
fix_test_name(X,X):- kaggle_arc(X,_,_,_).
fix_test_name(X,Y):- compound(X),!,arg(_,X,E),nonvar(E),fix_test_name(E,Y).
fix_test_name(X,t(X)):- kaggle_arc(t(X),_,_,_).
fix_test_name(X,v(X)):- kaggle_arc(v(X),_,_,_).

print_trainer0:- arc(t('25d487eb')).
print_eval0:- arc(v('009d5c81')).

nth_fact(P,I):- clause(P,true,Ref),nth_clause(P,I,Ref).

test_name(Name):- 
  findall(Name,kaggle_arc(Name,_,_,_),All),
  sort(All,AllS),member(Name,AllS).

:- dynamic(fav/2).

fav(A,B):- nonvar(A),nonvar(B),
 asserta(fav(A,B),Ref),!,
 call_cleanup((cls,mmake,arc1(A)),erase(Ref)),!.

fav(X,[]):- clause(fav(X),true).

fav(t('5521c0d9'),[lmDSL([with_each_indiv,move_above_itself])]).
fav(t('6d58a25d'),[debug_indiv]).
fav(v('e41c6fd3'),[debug_indiv]).
fav(v('1d398264'),[debug_indiv]).
fav(v('94133066'),[lmDSL([largest_indiv,trim_to_rect,rot90,flipV])]).
fav(t('5582e5ca'),[lmDSL([compute_max_color(C1),cls_with(C1)])]).
fav(v('e9bb6954'),[print_grid,indiv(min(8)),e('box of nine draw outward, if you hit a drawn line blacken it')]).
fav(v('762cd429'),[]).
fav(t('3c9b0459'),[lmDSL([rot180])]).
fav(t('6150a2bd'),[lmDSL([rot180])]).
fav(t('a79310a0'),[lmDSL([gravity(1,s),swap_colors(cyan,red)])]).
fav(t('d511f180'),[lmDSL([swap_colors(cyan,grey)])]).
fav(t('d511f180'),[lmDSL([compute_max_color(C1),compute_next_color(C2),swap_colors(C2,C1)])]).
fav(t('73251a56'),[learn([learn_mapping_stateful]),lmDSL([apply_mapping_stateful])]).
fav(t('f76d97a5'),[lmDSL([compute_max_color(C1),compute_next_color(C2),blacken_color(C1),subst_color(C2,C1)])]).
fav(t('6f8cd79b'),[lmDSL([add_borders(cyan)])]).
fav(t('ae4f1146'),[learn([call(set_bgc(cyan))]),lmDSL([largest_indiv,trim_to_rect,set_bg(cyan)])]).
fav(t('a48eeaf7'),[lmDSL([largest_indiv(I),tiny_individuals(Is),gravity_to(Is,I)])]).
fav(t('8d5021e8'),[lmDSL([grow([[rot180, flipV],
                               [flipH, same],
                              [rot180, flipV]])])]).

fav(t('7b6016b9'),[lmDSL([fillFromBorder(green),subst_color(black,red)])]).
fav(t('810b9b61'),[lmDSL([find_individuals([hollow,boxes],I),indiv_set_color(I,green)])]).
fav(t('44d8ac46'),[lmDSL([find_individuals([hollow,boxes,inside([squares])],I),indiv_fill_color(I,red)])]).
fav(t('9aec4887'),[indiv(color_blind),todo_sol([find_individuals([hollow,inside([squares])],I),rest_indivdual(Is),put_inside(Is,I),
  if_edge_strong([color=C]),touch(Is,Point),set_point(Point,C)])]).
%fav(t('db3e9e38'),[lmDSL([flipV,C1=orange,C2=blue,[],flipV]).
fav(t('1cf80156'),[lmDSL([trim_to_rect])]).
%fav(t(_),[lmDSL([fillFromBorder(none,yellow)])]).
fav(t('47c1f68c'),[lmDSL([compute_max_color(C1),compute_next_color(C2),
 
 blacken_color(C1),subst_color(C2,C1),
 trim_to_square,
 grow([[same,rot90],
      [rot270,rot180]])])]).

fav(t('447fd412'),[lmDSL([find_two_color_indivs,find_lesser_block,select_scaled_versions,builds,create_greater_blocks])]).
fav(t('ed36ccf7'),[lmDSL([rot270])]).
fav(t('5c2c9af4'),[lmDSL([two_closest_dots_to_edge,make_a_box,grow_box_that_much_bigger,grow_box_that_much_bigger,grow_box_that_much_bigger])]).
fav(t('27a28665'),[learn([shape_to_color]),lmDSL([make_box(1),shape_to_color(C),cls_with(C)])]).
fav(t('6cf79266'),[learn([find(nines),remove_them]),lmDSL(reverse_learned)]).
fav(t('97999447'),[lmDSL([find_ones,until_edges([copy_right(grey),copy_right(same)])])]).
fav(t('dae9d2b5'),[lmDSL([find_by_color,combine_all,set_all_fg(magenta)])]).
fav(t('3631a71a'),[learn([find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves])]).

fav(t('9ecd008a'),[learn([find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves]),
    lmDSL([indiv_is_one_hole,fix_image,selected_indiv,trim_to_rect])]).
fav(t('5117e062'),[lmDSL([find_two_color_indivs,selected_indiv,trim_to_rect,main_color,paint_landscape])]).
fav(t('7f4411dc'),[lmDSL([shave_away_1s])]).
fav(t('1b60fb0c'),[learn([find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves]),
    lmDSL([new_things_are_a_color,fix_image])]).
fav(t('d6ad076f'),[lmDSL([find_smaller,shoot_at_other,wide_beam])]).
fav(t('6d58a25d'),[print_grid,"the blue object is a downward beam maker, each beam must connect to one of its colors "]).
fav(t('23b5c85d'),[lmDSL([smallest_indiv,trim_to_rect])]).
%fav(t('23b5c85d'),[b7249182
fav(t('8be77c9e'),[lmDSL([grow([[same],[flipV]])])]).

make_box(X,_,G):- make_unassigned_grid(X,X,G).
%c:- forall(clause(fav(A,B),true),add_history1((fav(A,B)))).
:- add_history1(arc).
/*
first i look to see if the grid sizes are purporional, if not i look to see if the output grid can be recognised on the input
if not i look to see if the input grid can be recognised on the output

f35d900a

the input has a small number of points_only .. the same number on each image
two points_only are the same color, oh there are two pairs of points_only
there is a grey dashed box made up from the points_only
no two grey dots can touch though
the grey dots seem to originate from the four points_only
teh grey has to come out at least once.. after than if they are going to become two they skip
arround each color point there is a box of the oposting color



 cbded52d

we have a blue background and black TTT board .. to blue openings are made of 2x2s
in each squares there can be a color.. each color has a default pos on the 2x2
if like in the game of TTT you can win, but not diagonlly.. place the color on that area


 150deff5.json

  there is a hairything and it is soem color we will paint the entire thing another color
  now we will find the jaggedity bits and paint magenta in dashes of three lonbg until all that is left is 2x2 s



1) identify background and individuation



     




*/
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


largest_indiv(Points,Grid,Grid):- individuals(Grid,[Points|_]).
smallest_indiv(Points,Grid,Grid):- individuals(Grid,Is),last(Is,Points).

largest_indiv(Grid,GridO):-  largest_indiv(Points,Grid,_),points_to_grid(Points,GridO).
smallest_indiv(Grid,GridO):-  smallest_indiv(Points,Grid,_),points_to_grid(Points,GridO).

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
  is_grid(Grid),!,color_int(CC,GC), pred_subst(if_bgc_then_int(X,GC), Grid, GridO),!.
set_bg(C0,Grid,GridO):- color_code(C0,C),  nb_setval(grid_bgc,X), get_bgc(X),pred_subst(if_bgc_then(X,C), Grid, GridO),!.
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
nb_set_nth1(N,[_|Row],C):- Nm1 is N -1,nb_set_nth1(Nm1,Row,C).

set_all_fg(C0,Grid,GridO):- color_code(C0,C),get_bgc(X),pred_subst(if_not_bgc_then(X,C), Grid, GridO).
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

/*  
   trim_unused_vert_square(BG,Grid,GridO).
trim_unused_vert_square(BG,GridR,GridO):- append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),trim_unused_vert_square(BG,Grid,GridO).
trim_unused_vert_square(_,G,G).*/

%fav(t('25d487eb'),[lmDSL([rocketship])]).
%fav(v('20818e16'),[guess_bg,indiv(min(2))]).

%(fav(_,P)/(flatten([P],Flat),member(E,Flat))) ==> fav_trait(E).

:- forall((fav(_,P),flatten([P],Flat),member(E,Flat)), assert_if_new(fav_trait(E))).


learn_mapping_stateful(In,Out):- 
   must_det_l((unbind_color(0,[In,Out],[In1,Out1]),
   colors_to_vars([In1,Out1],[In2,Out2]), In2=Out2,
   asserta_new(backfill(Out2)))).

rot90(Grid,NewGrid):-  rot180(Grid,GridM),rot270(GridM,NewGrid). 
rot180(Grid,NewGrid):- flipH(Grid,RowRev),flipV(RowRev,NewGrid).
rot270(Grid,NewGrid):- get_colums(Grid,NewGrid).
flipH(Grid,FlipH):- maplist(reverse,Grid,FlipH).
flipV(Grid,FlipV):- reverse(Grid,FlipV).
rocketship(Grid,Grid).
apply_mapping_stateful(Grid,GridO):- unbind_color(0,Grid,GridO),ignore(backfill_vars(GridO)).

gravity(1,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridNew).
gravity(N,n,Grid,GridNew):-!,gravity_1_n_0(Grid,GridM),(Grid\=@=GridM->(Nm1 is N-1,gravity(Nm1,n,GridM,GridNew));GridNew=GridM).
gravity(N,s,Grid,GridNew):-!,flipV(Grid,FlipV),gravity(N,n,FlipV,GridM),flipV(GridM,GridNew).
gravity(N,w,Grid,GridNew):-!,rot90(Grid,GridRot),gravity(N,n,GridRot,GridM),rot270(GridM,GridNew).
gravity(N,e,Grid,GridNew):-!,rot270(Grid,GridRot),gravity(N,n,GridRot,GridM),rot90(GridM,GridNew).

compute_max_color(Color1,Grid,Grid):- colors_count_no_black(Grid,[color_count(Color1,_)|_]).
compute_next_color(Color1,Grid,Grid):- colors_count_no_black(Grid,[_,color_count(Color1,_)|_]).

subst_color(Color1,Color2,Grid,NewGrid):- notrace((color_code(Color1,Num1),color_code(Color2,Num2),subst(Grid,Num1,Num2,NewGrid))).
blacken_color(Color1,Grid,NewGrid):- subst_color(Color1,0,Grid,NewGrid).
swap_colors(Color1,Color2,Grid,NewGrid):- subst_color(Color1,Swap1,Grid,MGrid),
                                          subst_color(Color2,Color1,MGrid,NewGrid),
                                          color_code(Color2,Swap1).

backfill_vars(GridO):- clause(backfill(GridO),true).

unbind_color(Color1,Grid,GridO):- color_code(Color1,Num1),unbind_color0(Num1,Grid,GridO).
unbind_color0(Num1,Grid,GridO):- is_list(Grid),!,maplist(unbind_color0(Num1),Grid,GridO).
unbind_color0(Num1,Num1,_):-!.
unbind_color0(_,Num1,Num1).

colors_to_vars(Grid,GridO):- unique_colors(Grid,Colors0),include(integer,Colors0,Colors),
 length(Colors,Len),length(Vars,Len),
 subst_list_vars(Colors,Vars,Grid,GridO).

subst_list_vars(B,A,Grid,GridO):- subst_list_vars_0(B,A,Grid,GridO).

  subst_list_vars_0([],_,A,A):-!.
  subst_list_vars_0([F|FF],[R|RR],S,D):- !,subst(S,F,R,M), subst_list_vars_0(FF,RR,M,D).
/*
subst_list_vars_0(B,A,Grid,GridO):- is_list(Grid),!,maplist(subst_list_vars_0(B,A),Grid,GridO).
subst_list_vars_0(F,R,S,D):- nth1(N,F,E),E==S,nth1(N,R,D),!.
subst_list_vars_0(_,_,V,V).
*/

add_borders(Color1,Grid,GridO):- color_code(Color1,Num1),!,add_borders_0(Num1,Grid,GridO),!.

add_borders_0(Num1,Grid,GridO):- 
 grid_size(Grid,H,V),
 must_det_l((
  replace_row_e(1,Num1,Grid,G1),
  replace_row_e(V,Num1,G1,G2),
  replace_col_e(1,Num1,G2,G3),
  replace_col_e(H,Num1,G3,GridO))).

cls_with(Color1,Old,Grid):- color_code(Color1,Num1),cls_with_0(Num1,Old,Grid),!.
cls_with_0(Color1,Old,Grid):- is_list(Old),!,maplist(cls_with_0(Color1),Old,Grid).
cls_with_0(Color1,_,Color1).

get_colums(Grid,Cols):- Grid = [Row|_], length(Row,Width),
  get_colum_l(Width,Grid,Cols).

get_colum_l(0,_,[]):-!.
get_colum_l(Width,Grid,[Col|Cols]):- 
  get_colum(Width,Grid,Col),
  Wm1 is Width -1, 
  get_colum_l(Wm1,Grid,Cols).

get_colum(N,Grid,Col):- maplist(nth1(N),Grid,Col).

gravity_1_n_0([],[]).
gravity_1_n_0([Row1,Row2|Grid],GridNew):- nth1(Col,Row1,E1),nth1(Col,Row2,E2),E1=0,E2\==0,!,
  set_nth1(Col,Row1,E2,Row1Mod),set_nth1(Col,Row2,0,Row2Mod),
  gravity_1_n_0([Row1Mod,Row2Mod|Grid],GridNew).
gravity_1_n_0([Row1|Grid],[Row1|GridNew]):- gravity_1_n_0(Grid,GridNew).
   
set_nth1(1,[_|Row],E,[E|Row]):-!.
set_nth1(N,[W|Row],E,[W|RowMod]):- Nm1 is N-1, set_nth1(Nm1,Row,E,RowMod).

make_list(0,_,[]):-!.
make_list(1,E,[E]):-!.
make_list(N,E,[E|List]):- Nm1 is N -1,make_list(Nm1,E,List).

replace_row(N,Row,Grid,NewGrid):- grid_size(Grid,H,V), replace_row(N,Row,Grid,H,V,NewGrid).
replace_row(N,Row,Grid,H,V,NewGrid):- N<0, NewN is V + N+1,!,replace_row(NewN,Row,Grid,H,V,NewGrid).
replace_row(N,Row,Grid,_,_,NewGrid):- set_nth1(N,Grid,Row,NewGrid).

replace_row_e(N,E,Grid,NewGrid):- grid_size(Grid,H,V), make_list(H,E,Row), replace_row(N,Row,Grid,H,V,NewGrid),!.
replace_col_e(N,E,Grid,NewGrid):- grid_size(Grid,H,V), make_list(V,E,Col), replace_col(N,Col,Grid,H,V,NewGrid).

replace_col(N,Col,Grid,NewGrid):- grid_size(Grid,H,V), replace_col(N,Col,Grid,H,V,NewGrid).
replace_col(N,Col,Grid,H,V,NewGrid):- N<0, NewN is H + N+1,!,replace_col(NewN,Col,Grid,H,V,NewGrid).

replace_col(N,Col,Grid,_,V,NewGrid):- Nm1 is N - 1, length(Col,V),maplist(replace_col_at_0(Nm1),Col,Grid,NewGrid).

replace_col_at_0(N,Col,Row,NewRow):- length(Left,N),append(Left,[_|Right],Row),append(Left,[Col|Right],NewRow).


contains_nonvar(N,Info):- sub_term(E,Info),nonvar(E),E=N,!.
debug_indiv:- test_config(nodebug_indiv),!,fail.
debug_indiv:- test_config(indiv(_)), \+ test_config(nodebug_indiv),!.
test_info(Name,InfoF):- findall([Inf],user:fav(Name,Inf),Info),flatten(Info,InfoF).
fav(X):- clause(fav(X,_),true).

test_names_by_hard(Name):- findall(Name,fav(Name),List),list_to_set(List,Set),member(Name,Set).
test_names_by_hard(Name):-   
  findall(Hard-Name,(test_name(Name),hardness_of_name(Name,Hard)),All),
  keysort(All,AllK),
  member(_-Name,AllK),\+ fav(Name).

ascending_hard:-
  tell('arc_ascending.pl'),
  forall(test_names_by_hard(Name),
    forall(kaggle_arc(Name,Type,In,Out),format('~q.~n',[kaggle_arc_ord(Name,Type,In,Out)]))),
  told,
  reconsult(arc_ascending).

max_min(A,B,B,B):- var(A),!.
max_min(A,B,A,A):- var(B),!.
max_min(A,B,A,B):- A>B,!.
max_min(A,B,B,A).

hardness_of_name(Name,Hard):-
 %Type=tst+_,
 Type=_,
 findall(Hard,
 (kaggle_arc(Name,Type,In,Out),
  grid_size(In,H0,V0),
  grid_size(Out,H,V),
  max_min(H,V,C,_),
  max_min(H0,V0,C0,_),
  HV is C*C, HV0 is C0*C0,
  max_min(HV,HV0,Max,Min),
  D is Max-Min,
  Hard is Max*10000+D),All),
  sort(All,AllK),last(AllK,Hard).

%tell(s),ignore((nl,nl,test_pairs(Name,Type,In,Out),format('~N~q.~n',[test_pairs_cache(Name,Type,In,Out)]),fail)),told.

is_grid_cell(C):- (integer(C);var(C)),!.
is_grid([[C|H]|R]):- notrace((is_grid_cell(C),is_list(H),is_list(R),
  length([C|H],L),
  maplist(is_row_len(L),R))).

is_row_len(N,L):- length(L,N).

:- dynamic(backfill/1).

current_test_name(Name):- nb_current(test_name,Name),!.
current_test_name([]).

functor_color(pass,green).
functor_color(fail,red).
functor_color(warn,yellow).

arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_color(F,C),wots(S,print(G)),color_print(C,S),!,format('~N').
arcdbg(G):- wdmsg(G).


get_glyph(Point,Glyph):-  
  get_grid_num(Point,N),
  i_sym(N,Code),name(Glyph,[Code]).

get_grid_num(C-Point,N):-
  hv_point(X,Y,Point),
  get_grid_num_xyc(X,Y,C,N).

get_grid_num(Point,N):-
  hv_point(X,Y,Point),
  get_grid_num_xyc(X,Y,_C,N).

get_grid_num_xyc(X,Y,C,N):- fail,
  hv_point(X,Y,Point),
  nb_current(test_name_w_type,NameType),
  grid_nums(NameType,Grids),nth0(N,Grids,E),
  once(member(C-Point,E);member(Point,E);hv_value(E,C,X,Y)).

:- thread_local(grid_nums/2).
:- thread_local(grid_nums/1).
set_grid_nums(Gs):- 
   nb_current(test_name_w_type,NameType),
   asserta(grid_nums(NameType,Gs)).

run_nb(G):- call(G).
%run_nb(G):- setup_call_cleanup(G,true,notrace).

arc:- cls,mmake,
  time((forall(test_names_by_hard(Name),once(arc(Name))))).
arc1:- arc1(v('009d5c81')).
arc2:- arc1(t('25d487eb')).
fav:- cls,mmake, forall(fav(X),arc(X)).
fav1:- cls,mmake, fav(X), arc1(X).

arc(Name):- forall(arc1(Name),true).
arc1(Name):-  
  once(fix_test_name(Name,TName)), 
  retractall(grid_nums(_)),
  retractall(grid_nums(_,_)),
  nb_delete(grid_bgc),
  kaggle_arc(TName,Type,In,Out),
  ignore(run_arc_io(TName,Type,In,Out)).

run_arc_io(Name,Type,In,Out):- fail,
  individuals(In,IndVI),individuals(Out,IndVO),!,
  assert(arc_individuals(Name,Type,IndVI,IndVO)),
  write('.'),flush_output.

run_arc_io(Name,Type,In,Out):-
  run_nb((
  current_test_name(CName),
  nb_delete(grid_bgc),
  nb_setval(test_name,Name),
  nb_setval(test_name_w_type,Name+Type),
  retractall(grid_nums(Name+Type,_)),
  retractall(grid_nums(_)),
  retractall(grid_nums(_,_)),
  ignore(try_arc_io(CName,Name,Type,In,Out)))),!.
%try_arc_io(_,_,_,_,_):- \+ test_config(lmDSL(_)),!.
try_arc_io(CName,Name,Type,In,Out):-
  ignore((CName\==Name,dash_char(60,"A"),dash_char(6,"\n"),nl)),  
     dash_char(60,"V"),nl,
     describe_feature(Name,[test_info]),
    individuals_non_shared(In,IndvS),
    individuals_non_shared(Out,OndvS),!,
     grid_info(Name=in(Type),In), %print_tree_nl(in=IndvS),
     grid_info(Name=out(Type),Out), %print_tree_nl(out=OndvS),
     !,

    \+ \+ ignore(((Type = trn+_), test_config(learn(CProg)),must(training_progs(CProg,In,Out)))),
  compute_diff(IndvS,OndvS,Diffs),!,
  nop(print_tree_nl(diffs=Diffs)),
     confirm(Name,Type,In,Out),nl,!.


% Grid pretty printing
grid_info(Name,Grid):- 
  % dash_char(60,"="),  
  arcdbg(Name),
  describe_feature(Grid,[grid_dim,colors_count_size,colors_count,num_objects]),
  print_grid(Grid),
  /*
  grid_size(Grid,H,V),
  individuals(Grid,IndvS),  
   must_det_l(( when_config(print_grid,print_grid(Grid)),
     print_indiv(H,V,IndvS),
     ignore((debug_indiv,maplist(debug_indiv,IndvS))))),*/
  nop(describe_feature(Grid,[individuals])),!.

confirm(Name,Type,In,Out):- 
   ignore((sols_for(Name,Sol), confirm_sol(Sol,Name,Type,In,Out))).

sols_for(Name,Sol):- test_info(Name,Sols),member(lmDSL(Sol),Sols).

confirm_sol(Prog,Name,Type,In,Out):- 
   (run_dsl(Prog,In,Grid),into_grid(Grid,GridO))
   *->    
   count_difs(Out,GridO,Errors),
   (Errors==0 -> arcdbg(pass(Name,Type,Prog));(arcdbg(fail(Errors,Name,Type,Prog)),
     red_noise,
     once(print_grid(GridO)),
     red_noise))
   ;arcdbg(warn(unrunable(Name,Type,Prog))).


get_selector1(obj(PI),obj(PO)):-
  get_selector_n1(PIN),do_nth(PIN,PI,PO).

do_nth([],_,_):-!.
do_nth([H|T],I,O):-!, do_nth(H,I,O),do_nth(T,I,O).
do_nth(N,I,O):- nth1(N,I,E),nth1(N,O,E).

get_selector_n1([N1,N2,N3,N4],[]):- Top=6,
  between(1,Top,N1), between(N1,Top,N2),N2>N1, between(N2,Top,N3),N3>N2, between(N3,Top,N4),N4>N3.
get_selector_n1([1,2,3]). 
get_selector_n1([2,3]). % Colors Dif
get_selector_n1([1,2]). % Offset Dif
get_selector_n1([1,3]). % Size Dif
get_selector_n1([3]). %Offset
get_selector_n1([2]). %Size
get_selector_n1([1]). %Colors

get_selector(PI,PO):- get_selector1(PI,PO).
get_selector(PI,PO):- get_selector2(PI,PO), \+ (get_selector1(PII,POO), PI=@=PII,PO=@=POO).
get_selector(_,_).

get_selector2(obj(PI),obj(PO)):-
  get_selector_n2(PIN),do_nth(PIN,PI,PO).


get_selector_n2([N1,N2,N3]):- top(Top),
  between(1,Top,N1), between(N1,Top,N2),N2>N1, between(N2,Top,N3),N3>N2.
get_selector_n2([N1,N2]):- top(Top),
  between(1,Top,N1), between(N1,Top,N2),N2>N1.
get_selector_n2([N1]):- top(Top), between(4,Top,N1).

combine_diffs([],D,D):-!.
combine_diffs(D,[],D):-!.
combine_diffs(D,D,D):-!.
combine_diffs(D1,D2,L12):- listify(D1,L1),listify(D2,L2),!,append(L1,L2,L12).


%compute_diff(IPs,OPs,Difs2):- compute_diff(IPs,OPs,Difs2).
number_dif(I,O,0):- I =:= O.
number_dif(I,O,+D):- I<O, D is O -I.
number_dif(I,O,-D):- D is I -O.

dif_lop([],O,left_extra(O)):-!.
dif_lop(O,[],left_over(O)):-!.
dif_lop(I,O,DD):- 
  get_selector(PI,PO),
  select(PI,I,II), 
  select(PO,O,OO),
  compute_diff(PI,PO,D1),
  dif_lop(II,OO,D),
  combine_diffs(D1,D,DD).

include_fav_points(I,II):- include(fav_points,I,II),I=[_,_|_],!.
include_fav_points(I,I).

fav_points(I):- \+ dislike_points(I).

dislike_points(obj(I)):-!,dislike_points(I).
dislike_points(I):- is_list(I),dislike_points1(L),forall(member(E,L),member(E,I)).

%dislike_points1([shape(dot),grid_size(H,V)]):- freeze(H, freeze(V, (HV is H * V, HV > 49))).
dislike_points1([colors_count([color_count(BG, _)]),shape(nonsolid)]):- freeze(BG,is_black_or_bg(BG)).

with_each_indiv(G,I):- individuals(G,I).

move_above_itself(I,M):- move_dir_itself(1,n,I,M). 
move_rightof_itself(I,M):- move_dir_itself(1,e,I,M). 

move_dir_itself(N,D,I,M):- is_object(I),!, object_size(I,SX,SY), move_scale_dir_object(SX,SY,N,D,I,M).
move_dir_itself(N,D,L,LM):- is_list_of_points(L),!,maplist(move_dir_itself(N,D),L,LM).
move_dir_itself(N,D,I,O):- into_sol(I,M),M\=@=I,!,move_dir_itself(N,D,M,O).

move_dir_object(N,D,I,M):- move_scale_dir_object(1,1,N,D,I,M).

move_scale_dir_object(X,Y,N,D,I,M):- is_object(I),!,
 must_det_l((
  object_offset(I,OX,OY),
  move_dir(N,OX,OY,D,X,Y,NX,NY),
  (NY<1 -> M=I ; move_object(NX,NY,I,M)))).
move_scale_dir_object(N,D,L,LM):- is_list_of_points(L),!,maplist(move_scale_dir_object(N,D),L,LM).
move_scale_dir_object(N,D,I,O):- into_sol(I,M),M\=@=I,!,move_scale_dir_object(N,D,M,O).

move_object(NX,NY,I,M):- is_object(I),!,
 must_det_l((
  (NY<1 -> M=I ;
  ( localpoints(I,LPoints),
    offset_points(NX,NY,LPoints,GPoints),
    setq(I,[globalpoints(GPoints),object_offset(NX,NY)],M))))).
move_object(H,V,L,LM):- is_list_of_points(L),!,maplist(move_object(H,V),L,LM).
move_object(H,V,I,O):- into_sol(I,M),M\=@=I,!,move_object(H,V,M,O).


is_black_or_bg(BG):- get_bgc(BG).
is_black_or_bg(black).
is_black_or_bg(0).

compute_diff(I,O,[]):- (var(I);var(O)),!.
compute_diff(obj(I),obj(O),OUT):- !,
  compute_diff(I,O,Diffs),
    (Diffs == [] ->  OUT = same_object(obj(I)) ; OUT = props_diff(obj(I),obj(O),Diffs)).

compute_diff(I,O,DD):- is_list_of_points(I),is_list_of_points(O),!,
  include_fav_points(I,II),
  include_fav_points(O,OO),
  dif_lop(II,OO,DD).

compute_diff(I,O,D):- number(I),number(O),number_dif(I,O,D).
compute_diff(I,O,[]):- O=@=I,!.
compute_diff(I,O,O):- I==[],!.
compute_diff(I,O,I):- O==[],!.
compute_diff([IH,IV],[OH,OV],D):- maplist(integer,[IH,IV,OH,OV]),!,maplist(number_dif,[IH,IV],[OH,OV],D).
compute_diff(I,O,D):- is_list(I),!,is_list(O),sort(I,IS),sort(O,OS),!,list_diff(IS,OS,D).
compute_diff(I,O,D):- is_dict(I),!,findall(D1,(get_dict(K, I, V),compute_diff(K=V,O,D1)),D).
compute_diff(IF=IA,O,IF=D):-!, find_kv(O,IF,OA),!,compute_diff(IA,OA,D).
compute_diff(I,O,D):- compound(I),compound(O),compound_name_arguments(I,IF,IA),compound_name_arguments(O,_OF,OA),!,
  compute_diff(IF=IA,IF=OA,D).
compute_diff(I,O,I->O).

list_diff([],O,O):-!.
list_diff(O,[],O):-!.
list_diff(I,O,D):- select(C-E,I,II),select(C-E,O,OO),!,list_diff(II,OO,D).
list_diff(I,O,D1D):- select(C-E,I,II),select(C2-E,O,OO),!,list_diff(II,OO,D),combine_diffs(change_color(C->C2),D,D1D).
list_diff(I,O,D1D):- select(C-E,I,II),select(C-E2,O,OO),!,list_diff(II,OO,D),combine_diffs(change_point(E->E2),D,D1D).
list_diff(I,O,D):- select(CE,I,II),select(CE,O,OO),!,list_diff(II,OO,D).
list_diff(I,O,D1D):- select(CI,I,II),callable(CI),functor(CI,F,A),functor(CO,F,A),select(CO,O,OO),!,compute_diff(CI,CO,D1),list_diff(II,OO,D),combine_diffs(D1,D,D1D).
list_diff([CE|I],[CE2|O],D1D):- compute_diff(CE,CE2,D1),!, list_diff(I,O,D),combine_diffs(D1,D,D1D).


find_kv(OF=OA,OF,OA):- !.
find_kv(List,OF,OA):- is_list(List),member(E,List),nonvar(E),find_kv(E,OF,OA).
find_kv(Dict,OF,OA):- is_dict(Dict),get_dict(OF,Dict,OA).
find_kv(O,OF,OA):- compound(O),compound_name_arguments(O,OF,[OA]).
find_kv(obj(O),OF,OA):- !, find_kv(O,OF,OA).
find_kv(O,OF,OA):- compound(O),compound_name_arguments(O,OF,OA).


must_intersect_all(Indv,Points,NextScanPoints):-
   globalpoints(Indv,IndvPoints),
   unique_of_each(IndvPoints,Points,[],NextScanPoints),!.

unique_of_each(IndvPoints,Points,UniqueInvO,UniquePointsO):-
  remove_cpoints(Points,IndvPoints,UniquePoints),
  remove_cpoints(IndvPoints,Points,UniqueInv),!,
  UniquePoints=UniquePointsO,
  UniqueInv=UniqueInvO.

remove_cpoints(UniqueInv,[],UniqueInv).
remove_cpoints(IndvPoints,[C-Point|Points],UniqueInv):-
   select(C2-Point,IndvPoints,NextIndvPoints),
   C2==C,!,
   remove_cpoints(NextIndvPoints,Points,UniqueInv).


red_noise:- format('~N'),
  color_print(red,'--------------------------------------------------------------'),nl,
  color_print(red,'--------------------------------------------------------------'),nl,
  color_print(red,'--------------------------------------------------------------'),nl.

into_grid(G,G):- is_grid(G),!.
into_grid(Points,GridO):-
   grid_size(Points,H,V),
   make_unassigned_grid(H,V,Grid),
   maplist(set_on_grid(1,1,Grid),Points),
   get_bgc(BG),
   set_bg(BG,Grid,GridO).

into_grid(P,G):- points_to_grid(P,G),!.

count_difs(A,B,C):- into_grid(A,AA), into_grid(B,BB),!,count_difs0(AA,BB,C).
count_difs0(Out,GridO,0):- Out=@=GridO,!.
count_difs0(Out,GridO,1):- ((\+ compound(Out)) ; \+ compound(GridO)),!.
count_difs0([A|Out],[B|GridO],Errors):- !,
      count_difs0(A,B,Errors1),
      count_difs0(Out,GridO,Errors2),
      Errors is Errors1 + Errors2.
count_difs0(Out,GridO,Errors):-
  compound_name_arguments(Out,F,A),
  compound_name_arguments(GridO,FO,AO),
  count_difs0([F|A],[FO|AO],Errors).
  

describe_feature(Grid,List):- is_list(List),!,maplist(describe_feature(Grid),List).
describe_feature(Grid,Pred):- call(Pred,Grid,Res)->print_equals(Grid,Pred,Res);print_equals(Pred,f),!.

debug_indiv(_):- !.
debug_indiv(A):- format('~N'),print_tree_nl((A)),!.


debug_indiv_info(obj(A)):-  
 object_size(obj(A),H,V),
 make_unassigned_grid(H,V,Grid),
 localpoints(obj(A),Points),
 maplist(set_on_grid(1,1,Grid),Points),
 format('~N'),print_tree_nl(A),
 print_gridl(Grid,H,V),!.

%debug_indiv(Indv):- % trace,
%  pmember(obj(Props),Indv),writeqln(Props+Indv).
writeqln(X):- format('~N~q~N',[X]).

%print_gridl(Grid,_,_):- maplist(writeln,Grid),!.
print_gridl(Grid,H,V):-
 format('~N'),dash_char(H,"__"),format('~N'),
  forall(between(1,V,Y),
   ((format('~N'),forall(between(1,H,X), 
     (hv_value_or(Grid,C,X,Y,0),
        once(print_gridl_c(C))))))),format('~N'),!.
print_gridl_c(C):- var(C),write(' '),color_print(0,' ').
print_gridl_c(C):- write(' '),color_print(C,C).

set_on_grid(OH,OV,Grid,List):- is_list(List),maplist(set_on_grid(OH,OV,Grid),List).
set_on_grid(OH,OV,Grid,O):- is_object(O),globalpoints(O,Ps),!,set_on_grid(OH,OV,Grid,Ps).
set_on_grid(OH,OV,Grid,C-Point):- nonvar(C),
  color_int(C,I),
  hv_point(H,V,Point),
  HH is H - OH + 1, 
  VV is V - OV + 1,
  nth1(VV,Grid,Row),nb_set_nth1(HH,Row,I).


grid_dim(G,grid_size(H,W)):- grid_size(G,H,W).
%grid_size(O,offset_ranges(_,_,_,_,H,V)):- is_grid(O),grid_size(O,H,V).
%grid_size(P,S):- grid_size(P,S).

print_equals(_,N,V):- \+ compound(V),writeqln(N=V).
print_equals(Grid,N,Ps):- is_object(Ps),grid_size(Grid,H,V),print_points(N,H,V,Ps),!.
print_equals(Grid,N,PL):- is_list_of_points(PL), grid_size(Grid,H,V), 
  locally(grid_nums(PL),print_list_of_points(N,H,V,[[]])).
print_equals(_,N,G):- print_equals(N,G).

props_of_points(E,Ns):- findall(obj(Ps),member(obj(Ps),E),Ns).
points_name(E,Name):- 
 props_of_points(E,Ns),
  ignore((grid_nums(Nums),nth0(N,Nums,EE),EE=@=E,i_sym(N,Code),format(atom(Name),' Individual #~w  Code: "~s" ~w',[N,[Code],Ns]))),!.
points_name(E,pop(Ns)):- props_of_points(E,Ns).

print_list_of_points(N,H,V,PL):- 
  length(PL,Len),
  writeqln(N=list_of_points(Len)),!, 
  ignore((nop(debug_indiv),  
    forall(nth1(I,PL,E),print_points(N:I,H,V,E)))).

% print_points(N,H,V,E):- nop(print_points(N,H,V,E)),!.
print_points(N,H,V,E):- 
  dash_char(H,"-"),nl,  
  points_name(E,Name),
  write(Name), write(' from '),writeln(N),
  points_range(E,LoH,LoV,HiH,HiV,H,V),length(E,Len),writeqln(len(Len)->offset_ranges(LoH,LoV,HiH,HiV,H,V)),
  
  print_grid(1,1,LoH,LoV,HiH,HiV,H,V,E),dash_char(H,"-"),!,
  nl.


dash_char(H,C):-forall(between(0,H,_),write(C)).

print_equals(N,V):- \+ compound(V),writeqln(N=V).
print_equals(N,V):- is_grid(V),!,writeqln(N),print_grid(V).
print_equals(N,[G|L]):-
  is_grid(G),is_list(L),maplist(is_grid,L),!,
  length([G|L],Len), 
  grid_size(G,H,_),
  writeqln(N=len(Len)),  
  dash_char(H,"-"),
  forall(member(E,[G|L]),(print_grid(E),dash_char(H,"-"),nl)).
print_equals(N,V):- better_value(V,BV)-> BV\=@=V, !,print_equals(N,BV).
print_equals(N,[S|L]):- string(S),writeq(N),write('= '),write(S),maplist(commawrite,L),nl.
print_equals(Name,json(JSON)):-!, print_equals(Name,JSON).
print_equals(Name,trn=Y):- !, print_equals(Name,Y).
print_equals(Name,X->Y):- !, print_equals(in(Name),X), print_equals(out(Name),Y).
print_equals(Name,X=Y):- !, print_equals(Name=X,Y).
%print_equals(Name,[H|L]):- !, maplist(print_equals(Name),[H|L]).
print_equals(Name,Val):- print_tree_nl(Name=Val).

commawrite(S):- write(','),write(S).


print_indiv(H,V,PS):- print_indiv_0(H,V,PS),!.
print_indiv(H,V,PS):-
  maplist(print_indiv_0(H,V),PS).

% print_indiv_0(H,V,E):- nop(print_points(H,V,E)),!.
print_indiv_0(H,V,Points):- 
 points_to_grid(Points,Grid), % grid_size(Grid,HH,EV),trim_to_rect(Grid,G),grid_size(G,LH,LV),
  forall(between(1,H,_),write('__')),nl,
  %LoH = 1,LoV = 1, HiH = H,HiV = V,
  forall(between(1,V,IV),
   ((format('~N'),forall(between(1,H,IH), 
     (hv_value_or(Grid,C,H,V,_),
        once(print_g(IH,IV,C,1,1,H,V))))))),format('~N'),!,
  forall(between(1,H,_),write('__')),nl,
  !.

%is_list_of_points([G|V]):- is_object(G),is_list(V),maplist(is_object,V).
is_list_of_points([G|V]):- is_cpoint(G),is_list(V),maplist(is_cpoint,V).

as_color(Count-Num,List):- color_name(Num,Name),wots(List,color_print(Num,Name=Count)).
better_value(V,List):- is_list(V), maplist(as_color,V,List).
better_value([G|V],List):- 
  is_list_of_points([G|V]),
  maplist(points_to_grid,[G|V],List),
  [G|V] \=@= List.

black_first(SK,[color_count(Z,NC)|BF]):- is_black(Z), select(color_count(Z,NC),SK,BF),!.
black_first(BF,[color_count(Z,0.0)|BF]):- is_black(Z).

no_black(SK,BF):-select(color_count(Z,_),SK,BF),is_black(Z),!.
no_black(SK,BF):-select(Z,SK,BF),is_black(Z),!.
no_black(BF,BF).

is_color(C):- atom(C), color_int(C,N),integer(N).
is_black(black).
is_black(0).

pixels(G,GL):- is_grid(G),!,flatten(G,GF),include(integer,GF,GL).
pixels(G,GL):- findall(C,(sub_term(CP,G),compound(CP),CP=(C-_)),GL).
unique_colors(G,UC):- pixels(G,GF),sort(GF,UC).
colors_count_size(G,UC):- colors_count_no_black(G,GS),length(GS,UC).

into_cc(SK,BFO):- maplist(into_cc1,SK,BFO).
into_cc1(N-C,color_count(Nm,NC)):- NC is float(N),color_name(C,Nm).
colors_count_black_first(G,BF):- colors_count(G,SK),black_first(SK,BF).
colors_count_no_black(G,BF):- colors_count(G,SK),no_black(SK,BF).

num_objects(G,NO):- individuals(G,GS),length(GS,NO).

count_each([],_,[]).
count_each([C|L],GC,[Len-C|LL]):- include(==(C),GC,Lst),length(Lst,Len),count_each(L,GC,LL).

/*
print_points_grid(Points):- 
  points_range(Points,LoH,LoV,HiH,HiV,H,V),
  writeqln(offset_ranges(LoH,LoV,HiH,HiV,H,V)),
  points_to_grid(Points,Grid),
  print_grid(Grid).
print_points_grid(Grid):-  
  points_range(Grid,LoH,LoV,HiH,HiV,_H,_V),
  print_grid(Grid,LoH,LoV,HiH,HiV,Grid).
*/

print_grid(Grid):- grid_size(Grid,HH,VV), print_grid(1,1,HH,VV,Grid).
print_grid(SH,SV,EH,EV,Grid):- print_grid(SH,SV,SH,SV,EH,EV,EH,EV,Grid).
%print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):- nop(print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid)),!.
print_grid(SH,SV,LoH,LoV,HiH,HiV,EH,EV,Grid):-
  ignore((hv(1,1)\==hv(LoH,LoV),forall(between(SH,EH,_),write('__')),nl)),
  nop(get_bgc(BG)),
  forall(between(SV,EV,V),
   ((format('~N'),forall(between(SH,EH,H), 
     (hv_value_or(Grid,C,H,V,BG)->
        once(print_g(H,V,C,LoH,LoV,HiH,HiV))))))),format('~N'),!,
  ignore((hv(1,1)\==hv(LoH,LoV),forall(between(SH,EH,_),write('__')),nl)),!.
%print_grid(Grid):- is_grid(Grid),!, maplist(print_rows,Grid),nl.
%print_rows(List):- maplist(print_g,List),nl.
%block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
%block_colors([(black),(blue),(red),(green),(yellow),Silver,('#966cb8'),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
block_colors([(black),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
named_colors([(black),(blue),(red),(green),(yellow),(silver),(purple),(orange),(cyan),(brown)]).
named_colors([(black),(blue),(red),(green),(yellow),(grey),(magenta),(orange),(cyan),(brown)]).
named_colors([(black),(blue),(red),(green),(yellow),(grey),(pink),(orange),(cyan),(maroon)]).

silver('#7b7b7b').
silver('#c0c0c0').
silver('#9a9a9a').


color_print(C,W):- atom(C),color_int(C,N),integer(N),!,color_print(N,W).
color_print(C,W):- integer(C),C\==0,block_colors(L),nth0(C,L,Color),ansi_format([bold,fg(Color)],'~w',[W]),!.
color_print(C,W):- var(C),!,ansi_format([underline],'~w',[W]),!.
color_print(C,W):- C==0,!,ansi_format([fg('#444444')],'~w',[W]),!.
color_name(C,W):- var(C),!,W=C.
color_name(C,W):- integer(C),named_colors(L),nth0(C,L,W),!.
color_name(C,W):- atom(C),!,W=C.
%color_int(C,C):- var(C)
color_int(C,W):- integer(C),!,W=C.
color_int(C,W):- atom(C),!,named_colors(L),nth0(W,L,C),!.
color_int(C,C).
color_code(C,W):- color_name(C,W).
is_color_dat(C):- atomic(C),color_code(C,W),!,C==W.

%print_g(H,V,_,LH,LV,HH,HV):- (H<LH;H>HH;V<LV;V>HV),!, write('  ').
print_g(H,V,C,_,_,_,_):- write(' '),print_g1(H,V,C),!.
%get_code_at(_,_,0,_):-!,fail.
i_code_at(_,_,C-N,NC,Code):- nonvar(N),!,i_sym(N,Code),NC=C.
i_code_at(H,V,C,NC,Code):- nonvar(C), get_grid_num_xyc(H,V,Was,N),nonvar(Was),
  C\==Was,i_sym(N,Code),nonvar(Code),NC=Was,!.
i_code_at(H,V,C,NC,Code):- var(C), get_grid_num_xyc(H,V,Was,N),nonvar(Was),
  C\==Was,i_sym(N,Code),nonvar(Code),NC=Was,!.

i_code_at(_,_,C,C,VAR):- var(C),var_dot(VAR),!.
i_code_at(_,_,0,0,BGD):- bg_dot(BGD),!.
i_code_at(_,_,C,C,FGD):- fg_dot(FGD),!.

resrv_dot(Code):-  code_type(Code,white);code_type(Code,punct);code_type(Code,quote);var_dot(Code);bg_dot(Code);fg_dot(Code);
 member(Code,`?.¨«¬­¯°```).

var_dot(63).
/* code=63 ?  code=183 · code=176 ° code=186 º 170 ª */
bg_dot(183).
/* 169	© 248	ø 216	Ø  215 ×  174	® */
fg_dot(174).

print_g1(H,V,C0):- i_code_at(H,V,C0,C,Code),wots(S,format('~s',[[Code]])),!, color_print(C,S),!.
print_g1(_,_,C):- var(C), color_print(C,'?'),!.
print_g1(_,_,C):- trace, write(C).


i_sym(N,Code):- i_syms(Codes),nth0(N,Codes,Code),!.


save_codes(Max):- 
 %stream_property(File,file_no(1)),
 with_output_to(codes(CCC),
 ((forall((between(0,Max,Code),
     code_type(Code,graph),  
  \+ code_type(Code,white),
  \+ between(688,1000,Code),
  \+ between(1350,4600,Code),
  \+ between(4650,5000,Code),
  \+ between(5850,11500,Code),
  %(42600 > Code),
  
  \+ resrv_dot(Code)
   % ignore((0 is Code mod 50, format(File,'\n\n~d:',[Code]), put_code(File,Code))),
  ),put_code(Code))))),
  % format('~N~s~N',[CCC]),
  assert(i_syms(CCC)).

:- save_codes(42600).


%grid_to_id(Grid,ID):- 

erase_grid(GID):- retractall(cmem(GID,_,_)), 
  forall(retract(cindv(GID,_,ID)), erase_grid(ID)).


assert_id_cells(ID,Points):- maplist(assert_id_cell(ID),Points).
assert_id_cell(ID,-(C,HV)):- assert(cmem(ID,HV,C)).
assert_hvc_cell(_,_,_,C):- var(C). % free_cell(C),!.
assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(cmem(ID,HV,C)).


:- dynamic(grid_sz/3).
% Grid to_fast_workspace
assert_id_grid_cells(ID,Grid):-
 throw(all_in_emem(assert_id_grid_cells(ID,Grid))),
   grid_size(Grid,SH,SV),
   retractall(grid_sz(ID,_,_)),
   assert(grid_sz(ID,SH,SV)),
   erase_grid(ID),   
   forall(between(1,SH,H),
    forall(between(1,SV,V),
     ignore((hv_value(Grid,C,H,V),assert_hvc_cell(ID,H,V,C))))).


% Random Non Blk Eles
first_color(Grid1,C1):- sub_term(C1,Grid1),C1 \= 0,is_color(C1).

% Grid object_size/resize
make_lengths(N,L):- length(L,N).




points_range(Points,LoH,LoV,HiH,HiV,H,V):- calc_range(inf,inf,-inf,-inf,-inf,-inf,Points,LoH,LoV,HiH,HiV,H,V).

pmember(E,L):- sub_term(EE,L),nonvar(EE),EE=E,ground(E).
/*pmember(E,L):- is_dict(Points),!,E=grid_size(H,V),!,Points.grid_size=grid_size(H,V).
pmember(E,L):- member(EE,L),(EE=E;(is_list(EE),pmember(E,EE))).
pmember(E,L):- member(obj(EE),L),pmember(E,EE).
*/

%object_size(Points,H,V):- is_dict(Points),!,Points.object_size=object_size(H,V).


points_range(Points,offset_ranges(LoH,LoV,HiH,HiV,H,V)):- calc_range(inf,inf,-inf,-inf,-inf,-inf,Points,LoH,LoV,HiH,HiV,H,V).
% object_size(Points,object_size(H,V)):- points_range(Points,_LoH,_LoV,_HiH,_HiV,H,V).

close_color(brown,orange).
close_color(green,cyan).

%grid_size(Points,H,V):- is_dict(Points),!,Points.grid_size=grid_size(H,V).
grid_size(ID,H,V):- grid_sz(ID,H,V),!.
%grid_size(Grid,H,V):- notrace(is_object(Grid)), !, object_size(Grid,H,V).
grid_size(Points,H,V):- pmember(grid_size(H,V),Points),ground(H-V),!.
grid_size(Grid,H,V):- grid_size_nd(Grid,H,V),!.
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
apv(points_only(num)).  apv(facing(dir)). apv(min(n)). apv(max(n)).  apv(object_size(h,w)). apv(object_offset(h,w)). 
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

when_config(This,Goal):-test_config(This)-> call(Goal) ; true.
test_config(This):- current_test_name(Name),test_info(Name,InfoL),!,contains_nonvar(This,InfoL).

test_cond_or(This,_That):- test_config(This),!.
test_cond_or(This, That):- term_variables(This,[That|_]),!.

check_minsize(Sz,Indv1,Indv):- include(meets_size(Sz),Indv1,Indv).

meets_size(Len,Points):- point_count(Points,L),!,L>=Len.

largest_first(IndvS,IndvO):-  
  findall(Hard-Indv,(member(Indv,IndvS),point_count(Indv,Hard)),All),
  keysort(All,AllK),
  reverse(AllK,AllR),
  findall(Indv,member(_-Indv,AllR),IndvO).

finish_grp(C,Grp,Point2,Dir,Rest,NewGroup,RRest):- 
   is_adjacent_point(Point2,Dir,Point3),
   select([C-Point3],Rest,Rest1),
   finish_grp(C,[C-Point3|Grp],Point3,Dir,Rest1,NewGroup,RRest).
finish_grp(_C,Grp,_From,_Dir,Rest,Grp,Rest).

single_point(C-Point,IndvS,Rest1):-
  select([C-Point],IndvS,Rest1),
  nonvar(C).
single_point(C-Point,IndvS,Rest1):-
  select(C-Point,IndvS,Rest1),
  nonvar(C).
single_point(C-Point,IndvS,Rest1):-
  select(obj(I),IndvS,Rest1),
  globalpoints(obj(I),[C-Point]),
  nonvar(C).

unraw_inds2(Types,IndvS,IndvO):- fail,
   largest_first(IndvS,Indv1),
   reverse(Indv1,IndvR), IndvR\=@=IndvS,
   unraw_inds2(Types,IndvR,IndvO).

unraw_inds2(Types,IndvS,IndvO):-   
  single_point(C-Point1,IndvS,Rest1), \+ free_cell(C),\+ get_bgc(C),
  is_diag(Dir),
  is_adjacent_point(Point1,Dir,Point2),
  single_point(C-Point2,Rest1,Rest2),
  is_adjacent_point(Point2,Dir,Point3),
  single_point(C-Point3,Rest2,Rest),
  finish_grp(C,[C-Point3,C-Point2,C-Point1],Point3,Dir,Rest,NewGroup1,RRest),
  reverse(NewGroup1,NewGroupR),
  reverse_dir(Dir,RevDir),
  finish_grp(C,NewGroupR,Point1,RevDir,RRest,NewGroup,RRestO),
  unraw_inds2(Types,[NewGroup|RRestO],IndvO).

unraw_inds2(Types,IndvS,IndvO):-  fail,
  single_point(C-Point1,IndvS,Rest1), nonvar(C), \+ free_cell(C),
  single_point(C2-Point2,Rest1,Rest),
  findall(C3-Point3,member([C3-Point3],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(Types,IndvM,IndvMO),
  Grp=[C-Point1,C2-Point2|CRest],
  IndvO=[Grp|IndvMO].

unraw_inds2(Types,IndvS,IndvO):- 
  single_point(C-Point1,IndvS,Rest1),
  Grp=[_,C-_,_|_],
  select(Grp,Rest1,Rest),
  is_diag(Dir),
  adjacent_groups(C,[C-Point1],Dir,Grp),
  unraw_inds2(Types,[[C-Point1|Grp]|Rest],IndvO).

/*
unraw_inds2(Types,IndvS,IndvO):-   
  select([C1-Point1],IndvS,Rest),
  nearby_one(Types,C,C1-Point1,Rest),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(Types,IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|C-Rest],
  IndvO=[Grp|IndvMO].
*/
unraw_inds2(_,IndvS,IndvS).


:- thread_local(t_l:id_cells/2).

deoffset_points(1,1,Points,Points):-!.
deoffset_points(OH,OV,Point,LPoint):- pred_subst(if_point_de_offset(OH,OV),Point,LPoint).
if_point_de_offset(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H -OH +1, VV is V - OV +1,hv_point(HH,VV,LPoint).

offset_points(OH,OV,Point,LPoint):- pred_subst(if_point_offset(OH,OV),Point,LPoint).
if_point_offset(OH,OV,Point,LPoint):- atom(Point), hv_point(H,V,Point),HH is H +OH -1, VV is V + OV -1,hv_point(HH,VV,LPoint).

grid_to_individual(Grid,OUT):-
  grid_size(Grid,H,V),
  globalpoints(Grid,Points),
  embue_points(H,V,1,1,H,V,Points,OUT).

remove_color(_-P,P).
remove_color(LPoints,CLPoints):- maplist(remove_color,LPoints,CLPoints).
%embue_points(_,_,I,I):-!.
embue_points(_,_,obj(Ps),obj(Ps)):-!.
embue_points(H,V,Points,OUT):- 
  points_range(Points,LoH,LoV,HiH,HiV,_HO,_VO),
  embue_points(H,V,LoH,LoV,HiH,HiV,Points,OUT).

embue_points(H,V,LoH,LoV,HiH,HiV,C-HV,OBJ):- !, embue_points(H,V,LoH,LoV,HiH,HiV,[C-HV],OBJ).
embue_points(H,V,LoH,LoV,HiH,HiV,Points,obj(Ps)):-
 assertion(ground(Points)),
 must_det_l((
  colors_count(Points,CC),
  length(Points,Len),
  Width is HiH-LoH+1,
  Height is HiV-LoV+1,
  %nb_setval(test_name_w_type,NameType),
  %gensym(id_,IID),
  Area is Width * Height,
  Empty is  Area - Len,
  deoffset_points(LoH,LoV,Points,LPoints),
  findall(shape(Shape),guess_shape(Ps,Empty,Len,Width,Height,CC,Points,Shape),Shapes),
  remove_color(LPoints,CLPoints),
  append([
    points_only(CLPoints),
    colors_count(CC),
    object_size(Width,Height),
    object_offset(LoH,LoV),
    %width(Width), height(Height), area(Area),
    point_count(Len),
    %missing(Empty),
    localpoints(LPoints)|Shapes],[
    globalpoints(Points),
    grid_size(H,V)],Ps))).

point_count(_-P,1):- nonvar(P),!.
point_count(I,X):- indv_props(I,L),member(point_count(X),L),!.
point_count(obj(I),Count):- localpoints(I,Points), length(Points,Count),!.
point_count(Points,Count):- is_list(Points),length(Points,Count),!.

setq(I,[],I):-!.
setq(I,[H|T],II):- !, setq(I,H,M),setq(M,T,II).
setq(I,P,II):- functor(P,F,A),functor(Q,F,A),D=done(nil),pred_subst(setq_1(D,Q,P),I,II).
setq_1(done(t),_,_,I,I):-!.
setq_1(D,Q,P,I,II):- compound(I),I=Q,nb_setarg(1,D,t),II=P.
   
indv_props(obj(L),L):- is_list(L).

localpoints(I,X):- indv_props(I,L),member(localpoints(X),L).

globalpoints(Grid,Points):- is_grid(Grid),!, grid_size(Grid,HH,HV), 
  findall(C-Point,(between(1,HV,V),between(1,HH,H),hv_value(Grid,C,H,V),nonvar(C),hv_point(H,V,Point)),Points),
  assertion(ground(Points)).
%globalpoints(Grid,Points):- is_object(Grid),!,globalpoints(Grid,Points).
globalpoints(Grid,Points):- is_list_of_points(Grid),!,maplist(globalpoints,Grid,MPoints),flatten(MPoints,Points).
globalpoints(I,X):- indv_props(I,L),member(globalpoints(X),L).
/*
globalpoints(ID,Points):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Points).
globalpoints(Grid,Points):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Points).
:- dynamic(is_grid_id/2).
grid_to_id(Grid,ID):- is_grid_id(Grid,ID),!.
grid_to_id(Grid,ID):- gensym('grid_',ID),assert_id_grid_cells(ID,Grid),assert(is_grid_id(Grid,ID)),!.
*/

colors_count(I,X):- indv_props(I,L),!,member(colors_count(X),L).
colors_count(G,BFO):- pixels(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK),into_cc(SK,BFO).

points_only(I,X):- indv_props(I,L),member(points_only(X),L).

object_offset(I,X,Y):- indv_props(I,L),member(object_offset(X,Y),L).
object_size(I,X,Y):- indv_props(I,L),member(object_size(X,Y),L).
object_size(Points,H,V):- pmember(object_size(H,V),Points),!.
object_size(Points,H,V):- points_range(Points,LoH,LoV,HiH,HiV,_,_), H is HiH-LoH+1, V is HiV-LoV+1.

top(8).

:- style_check(-singleton).
guess_shape(I,Empty,N,1,1,Colors,Points,dot):- !.
guess_shape(I,Empty,N,N,1,Colors,Points,hline).
guess_shape(I,Empty,N,1,N,Colors,Points,vline).
guess_shape(I,Empty,N,H,V,[color_count(Zero,_)],Points,background):- (Zero==0;Zero==black).
guess_shape(I,0,N,HV,HV,Colors,Points,square):- HV>1.
guess_shape(I,0,N,H,V,Colors,Points,solid).
guess_shape(I,O,N,H,V,Colors,Points,nonsolid):- O\==0.

guess_shape(I,E,N,H,V,Colors,Points,subI(InvS)):- E>2, fail,
   once((I.object_offset=object_offset(LoH,LoV),
   make_unassigned_grid(H,V,Grid),
   calc_add_points(LoH,LoV,Grid,Points),
   individuals(Grid,InvS))),!,
   InvS=[_,_|_].


   
:- dynamic(individuals_saved/2).
:- dynamic(reuse_grid_nums/1).


:- style_check(+singleton).

individuals(Grid,IndvS):-
  individuals_non_shared(Grid,IndvS),
  set_grid_nums(IndvS).


individuals_non_shared(Grid,IndvS):- is_grid(Grid),!,
 must_det_l((
  notrace((grid_size(Grid,H,V),
  globalpoints(Grid,Points))),
  locally(t_l:id_cells(Grid,Points),
  ( individuals_raw(Grid,Points,Indv_0),    
    % dmsg(is=Indv_0),    
    unraw_inds(Indv_0,Indv1),
    largest_first(Indv1,Indv2))),
  maplist(embue_points(H,V),Indv2,IndvS))).
individuals_non_shared(Grid,I):- is_list_of_points(Grid),!,I=Grid.
individuals_non_shared(obj(Grid),[obj(Grid)]):-!.

individuals_raw(ID,Points,Indv):- 
  individuals_list(squares,ID,Points,Indv).

unraw_inds(IndvS,IndvO):-   
  largest_first(IndvS,Indv1),
  reverse(Indv1,IndvR),
  test_cond_or(indiv(min(SZ)),1),
  check_minsize(SZ,IndvR,Indv),
  unraw_inds2(_,Indv,IndvO),!.

%individuals_list_diamonds(_,Indv,Indv1):-
%  individuals_list(diamonds,ID,Indv,Indv1).


individuals_list(_,_,[],[]):-!.
individuals_list(Types,ID,Points,[Indv|IndvList]):-   
    find_one_individual(Types,Points,Indv,NextScanPoints),!,
    %writeqln(indv(Types)=Indv),    
    individuals_list(Types,ID,NextScanPoints,IndvList).
individuals_list(squares,ID,Points,[Indv|IndvList]):- fail,
   find_one_individual(diamonds,Points,Indv,NextScanPoints),!,
   individuals_list(diamonds,ID,NextScanPoints,IndvList). 
individuals_list(_Types,_,Points,IndvList):- maplist(obj1,Points,IndvList).

   

obj1(X,X).

ok_color_with(C,C2):- /* \+ free_cell(C2), */ C==C2.
find_one_individual(Types,Points,Indv,NextScanPoints):-
    select(C-HV,Points,Rest), \+ free_cell(C), 
    allow_dirs(Types,Dir),
    adjacent_point_allowed(C,HV,Dir,HV2),select(C2-HV2,Rest,ScanPoints),ok_color_with(C,C2),
  /*  nop((\+ filtered_point(C,HV2), \+ filtered_point(C,HV),
     (\+ is_diag(Dir)-> true ; (turn_left_45(Dir,TR),turn_right_45(Dir,TL),color_of(HV,TL,TLC),color_of(HV,TR,TRC),
      \+ (colors_block_diag(C,TLC,TRC,C2)))))),*/
    all_individuals_near(Types,C,[C-HV,C2-HV2],ScanPoints,NextScanPoints,Indv),
    meets_indiv_criteria(Indv).

color_of(HV,TL,TLC):- t_l:id_cells(_ID,Points), is_adjacent_point(HV,TL,TLHV),member(TLC-TLHV,Points).

colors_block_diag(C,TLC,TRC,_C2):- TLC==TRC, TRC\==0, C \== TRC, \+ free_cell(TRC).

filtered_point(C,HV):- t_l:id_cells(_ID,Points),% select(_-HV,Points,Rest), 
  findall(Dir-HV2,(adjacent_point_allowed(C,HV,Dir,HV2),member(C-HV2,Points)),Used),
  findall(Dir-HV2,(adjacent_disallowed(C,HV,Dir,HV2),member(C-HV2,Points)),Unused),
  shape_has_filtered_use(C,Used,Unused),
  %wdmsg(shape_has_filtered_use(C,HV,Used,Unused)),
  !.

shape_has_filtered_use(_,[],_Unused).
shape_has_filtered_use(C,[_],_):- shape_filter(C,squares),!.

adjacent_groups(C,Grp1,Dir,Grp2):- member(_-P1,Grp1),member(C-P2,Grp2),is_adjacent_point(P1,Dir,P2).
adjacent_point(C,HV,HV2):- adjacent_point_allowed(C,HV,_Dir,HV2).
adjacent_point_allowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir(Shape,DirS),member(Dir,DirS).
adjacent_disallowed(C,HV,Dir,HV2):- is_adjacent_point(HV,Dir,HV2), shape_filter(C,Shape),allow_dir(Shape,DirS), \+ member(Dir,DirS).

allow_dirs(Square,X):- allow_dir(Square,List),member(X,List).
allow_dir(horizs,[e,w]). allow_dir(virtzs,[n,s]). allow_dir(diaguz,[ne,sw]). allow_dir(diagdz,[nw,se]). 
allow_dir(squares,[n,s,e,w]). allow_dir(polygs,[n,s,e,w]).
allow_dir(diamonds,[nw,sw,se,ne]).
%circles, dots, , rays, walls

shape_filter(X,squares):- free_cell(X).
shape_filter(X,polygs):- \+ free_cell(X).


ok_dir(s). ok_dir(e). ok_dir(n). ok_dir(w).
ok_dir(_) :- \+ squares.
squares:-true.


meets_indiv_criteria(_).

all_individuals_near(_Types,_C,NewSet,[],[],[],NewSet):-!.
all_individuals_near(Types,C,Indv,ScanPoints,NewScanPoints,NewSet):-
   individuals_near(Types,C,Indv,ScanPoints,New,NextScanPoints),
   (New == [] -> (NewSet = Indv, NewScanPoints = NextScanPoints)
    ; (append(Indv,New,IndvNew),
        all_individuals_near(Types,C,IndvNew,NextScanPoints,NewScanPoints,NewSet))).

individuals_near(_Types,_C,_From,[],[],[]):-!.
individuals_near(Types,C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- nearby_one(Types,C,E,From),!,
  individuals_near(Types,C,[E|From],ScanPoints,Nears,NextScanPoints).

individuals_near(Types,C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- 
      individuals_near(Types,C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(Types,C,C2-E,List):- allow_dirs(Types,Dir), adjacent_point_allowed(C,E2,Dir,E), member(C2-E2,List),ok_color_with(C2,C).

points_to_grid(Points,Grid):- 
  must_det_l((
  notrace(grid_size(Points,H,V)),
  make_unassigned_grid(H,V,Grid),
  calc_add_points(1,1,Grid,Points))).

calc_add_points(OH,OV,_,Obj):- var(Obj),throw(var_calc_add_points(OH,OV,Obj)).
calc_add_points(OH,OV,Grid,Points):- is_list(Points),!,maplist(calc_add_points(OH,OV,Grid),Points).

%calc_add_points(OH,OV,Grid,Obj):- is_grid(Obj),globalpoints(Obj,Points),maplist(calc_add_points(OH,OV,Grid),Points).
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


is_list_of_gridoids([G|V]):- is_gridoid(G), is_list(V), maplist(is_gridoid,V).

is_gridoid(G):- var(G),!, fail.
is_gridoid(G):- is_grid(G),!.
is_gridoid(G):- is_object(G),!.
%is_gridoid(G):- is_cpoint(G),!.
is_gridoid(G):- is_list_of_gridoids(G).

from_gridoid(Points,C,N,H,V):- nth0(N,Points,G),hv_value(G,C,H,V),nonvar(C),C\==black,C\==0,!.
from_gridoid(Points,C,N,H,V):- nth0(N,Points,G),hv_value(G,C,H,V),nonvar(C),!.
from_gridoid(Points,C,N,H,V):- nth0(N,Points,G),hv_value(G,C,H,V).

hv_value_or(Grid,C,H,V,Else):- hv_value(Grid,C,H,V)*->true;C=Else.
hv_value(ID,C,H,V):- cmem(ID,HV,C),hv_point(H,V,HV),!.
hv_value(Points,C-N,H,V):- is_list(Points),is_list_of_gridoids(Points),!,
   from_gridoid(Points,C,N,H,V).
hv_value(O,Color,H,V):- is_object(O),globalpoints(O,Ps),!,hv_value(Ps,Color,H,V).
hv_value(Grid,Color,H,V):- is_grid(Grid),!,nth1(V,Grid,Row),nth1(H,Row,C),color_name(C,Color).

%hv_value(Points,C,H,V):- nonvar(H),!, hv_point(H,V,HV), sub_term(C-HV,Points),atom(HV).
hv_value(Points,C,H,V):- nonvar(H),nonvar(V),hv_point(H,V,HV),!, hv_member(HV,C,Points),!.
hv_value(Points,C,H,V):- member(C-HV,Points),hv_point(H,V,HV).

hv_member(HV,C,O):- is_object(O),globalpoints(O,Ps),!,hv_member(Ps,C,HV).
hv_member(HV,C,Points):- member(C-HV,Points),!.
hv_member(HV,C,Points):- sub_term(C-HV,Points),!.
/*b_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),set_nth1(H,Row,C).
nb_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),nb_set_nth1(H,Row,C).
b_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),rplc_nth1(H,Row,OldC,NewC).
nb_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),nb_rplc_nth1(H,Row,OldC,NewC).

*/
run_dsl(Prog,In,Out):- var(Prog),!,throw(var_solving_progs(Prog,In,Out)).

run_dsl(lmDSL(Prog),In,Out):- !, run_dsl(Prog,In,Out).
run_dsl(call(G),In,Out):-!,call(G),(var(Out)->Out=In; true).
run_dsl([],In,Out):-!, var(Out)->Out=In; true.
run_dsl(same,In,Out):-!, duplicate_term(In,Out).
run_dsl([H|Prog],In,Out):-!, run_dsl(H,In,GridM), run_dsl(Prog,GridM,Out).
run_dsl(Prog,In,In):- missing_arity2(Prog),!,arcdbg(warn(missing(run_dsl(Prog)))).
run_dsl(Prog,In,Out):- call(Prog,In,M)*-> into_sol(M,Out) ; arcdbg(warn(nonworking(run_dsl(Prog)))).

named_test(P,G):- fix_test_name(P,Name),kaggle_arc(Name,tst+0,G,_),!.
named_test(P=in(Type),G):- fix_test_name(P,Name),!,kaggle_arc(Name,Type,G,_),!.
named_test(P=out(Type),G):- fix_test_name(P,Name),!,kaggle_arc(Name,Type,_,G),!.

into_sol(G,G):- var(G),throw(var_into_sol(G)).
into_sol(G,I):- is_grid(G),!,individuals(G,I).
into_sol(P,G):- is_list_of_points(P),!,G=P.
%into_sol(P,G):- is_object(P),!,G=[P].
into_sol(P,G):- is_object(P),points_to_grid(P,M),!,into_sol(M,G).
%into_sol(G,G):- is_grid(G),!.
into_sol(P,G):- named_test(P,M),!,into_sol(M,G).
into_sol(P,G):- dumpST,throw(into_sol(P,G)).

into_sol(P,G):- is_list_of_points(P),set_grid_nums(P),
  maplist(into_sol,P,Gs),!,combine_grids(overlay,Gs,G).
into_sol(P,G):-
  maplist(into_sol,P,Gs),!, 
  set_grid_nums(Gs), arg(1,Gs,G).

into_sol(P,G):- maplist(into_sol,P,Gs),!, set_grid_nums(Gs), combine_grids(overlay,Gs,G).


combine_grids(_,[G],G):-!.
combine_grids(How,[G1,G2|Gs],GO):- combine_grids(How,[G2|Gs],G1,GO).

combine_grids(_How,[],G,G):-!.
combine_grids(How,[H|T],G,GO):- !,
  %in_cmt((writeln(How),print_grid(H))),
  combine_grids(How,H,G,GM),
  combine_grids(How,T,GM,GO).
combine_grids(overlay,H,G,GM):- globalpoints(H,Points),set_point(Points,G,GM),!.
combine_grids(append,H,G,GM):- grid_size(H,W,_),length(Row,W), append(G,[Row|H],GM).
  

training_progs(Prog,In,Out):- var(Prog),!,throw(var_training_progs(Prog,In,Out)).
training_progs(call(G),_In,_Out):-!,call(G).
training_progs([],_In,_Out):-!.
training_progs([H|Prog],In,Out):-!, training_progs(H,In,Out), training_progs(Prog,In,Out).
training_progs(Prog,_,_):- missing_arity2(Prog),!,arcdbg(warn(missing(training_progs(Prog)))).
training_progs(Prog,In,Out):- call(Prog,In,Out)*-> true ; arcdbg(warn(nonworking(training_progs(Prog)))).


missing_arity2(P2):- compound(P2),!,compound_name_arity(P2,F,Am2),A is Am2 + 2, \+ current_predicate(F/A).
missing_arity2(F):- \+ current_predicate(F/2).
% turtle(H,V,Dir,N,H2,V2):- 
prim_ops([
  call_object_grid_size(obj),
  trim_grid_to_size(point,object_size),
  fill_from_point(point,color),
  create_a_ray(point,dir,len),
  object_as_own_grid(obj,gridOps),
  copy_one_object(obj,point),
  rotate_one_object(obj,nsew),
  flatten_one_object(obj),
  sort_by_gravity(nsew),
  flip_grid(hOrv),
  rotate_grid(nsew)]).

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

% make or do plan
do_change(Change,Grid1,Grid2):- \+ is_list(Change),!,one_change(Change,Grid1,Grid2).
do_change(Change,Grid1,Grid2):- do_change_nd(Change,Grid1,Grid2).

do_change_nd([],Grid1,Grid1).
do_change_nd([H|T],Grid1,Grid2):- one_change(H,Grid1,GridM),do_change_nd(T,GridM,Grid2).

one_change(same,Grid1,Grid2):- is_grid(Grid2),Grid1=Grid2,!.
one_change(colorChange(C1,C2),Grid1,Grid2):- 
  first_color(Grid1,C1),ignore((is_grid(Grid2),first_color(Grid2,C2))),
  subst(Grid1,C1,C2,Grid2).
one_change(blank1Color(C1),Grid1,Grid2):- 
  first_color(Grid1,C1),copy_cells(==(C1),free_cell,Grid1,Grid2).
one_change(same_size,Grid1,Grid2):- var(Grid2),grid_size(Grid1,H,V),grid_size(Grid2,H,V),!.


% resize(H,V,Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C2).

