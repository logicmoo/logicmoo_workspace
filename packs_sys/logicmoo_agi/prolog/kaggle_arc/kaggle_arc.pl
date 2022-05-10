
:- dynamic('$exported_op'/3).
:- multifile('$exported_op'/3).
:- system:ensure_loaded(library(logicmoo_common)).
:- system:ensure_loaded(library(pfc_lib)).
:- expects_dialect(pfc).
/*
JSON Conversion
:- use_module(library(http/json_convert)).

test_pairs(Name,Type,In,Out):- 
  kaggle_arc_eval(Name,Stuff), once(atom_json_term(Stuff,json(L),[])),
  json_pairs(L,Type,In,Out).

json_pairs([],_,_,_):- !, fail.
json_pairs(json(T),Type,In,Out):-!,json_pairs(T,Type,In,Out).
json_pairs([input=In,output=Out],_Type,In,Out):-!.
json_pairs(Type=List,Type,In,Out):-!,member(L,List),
   json_pairs(L,Type,In,Out).
json_pairs([H|T],Type,In,Out):-!, 
  (json_pairs(H,Type,In,Out);json_pairs(T,Type,In,Out)).

%print_trainer:- kaggle_arc_train(Name,Stuff), atom_json_term(Stuff,JSON,[]),print_arc(Name,JSON).
%print_evaler:- kaggle_arc_eval(Name,Stuff), atom_json_term(Stuff,JSON,[]),print_arc(Name,JSON).

*/
:- dynamic(cmem/3).
:- ensure_loaded(kaggle_arc_train).
:- ensure_loaded(kaggle_arc_eval).

fix_test_name(X,X):- var(X),!.
fix_test_name(X,X):- kaggle_arc(X,_,_,_).
fix_test_name(X,Y):- compound(X),!,arg(_,X,E),nonvar(E),fix_test_name(E,Y).
fix_test_name(X,t(X)):- kaggle_arc(t(X),_,_,_).
fix_test_name(X,v(X)):- kaggle_arc(v(X),_,_,_).

print_trainer(Name):-  fix_test_name(Name,TName), forall(kaggle_arc(TName,Type,In,Out),print_arc_in_out(TName,Type,In,Out)).
print_trainer:- cls,mmake,forall(test_names_by_hard(Name),print_trainer(Name)).

print_trainer0:- print_trainer(t('25d487eb')).
print_eval0:- print_trainer(v('009d5c81')).

nth_fact(P,I):- clause(P,true,Ref),nth_clause(P,I,Ref).

% Type is tst or trn
kaggle_arc(t(Name),TypeI,In,Out):-   
  nth_fact(kaggle_arc_train(Name,Type,In,Out),This),
  once((nth_fact(kaggle_arc_train(Name,Type,_,_),Start), I is This - Start,TypeI=Type+I)).
kaggle_arc(v(Name),TypeI,In,Out):-   
  member(Type,[trn,tst]),
  nth_fact(kaggle_arc_eval(Name,Type,In,Out),This),
  once((nth_fact(kaggle_arc_eval(Name,Type,_,_),Start), I is This - Start,TypeI=Type+I)).

test_name(Name):- 
  findall(Name,kaggle_arc(Name,_,_,_),All),
  sort(All,AllS),member(Name,AllS).


fav(X,[]):- clause(fav(X),true).
fav(v('94133066'),[sol([largest_indiv,trim_to_used,rot90,flipV])]).
fav(t('5582e5ca'),[sol([compute_max_color(C1),cls_with(C1)])]).
fav(v('e9bb6954'),[indiv(min(8)),e('box of nine draw outward, if you hit a drawn line blacken it')]).
fav(v('762cd429'),[]).
fav(t('3c9b0459'),[sol([rot180])]).
fav(t('6150a2bd'),[sol([rot180])]).
fav(t('a79310a0'),[sol([gravity(1,s),swap_colors(cyan,red)])]).
fav(t('d511f180'),[sol([swap_colors(cyan,grey)])]).
fav(t('d511f180'),[sol([compute_max_color(C1),compute_next_color(C2),swap_colors(C2,C1)])]).
fav(t('73251a56'),[learn([learn_mapping_stateful]),sol([apply_mapping_stateful])]).
fav(t('f76d97a5'),[sol([compute_max_color(C1),compute_next_color(C2),blacken_color(C1),subst_color(C2,C1)])]).
fav(t('6f8cd79b'),[sol([add_borders(cyan)])]).
fav(t('ae4f1146'),[learn([call(set_bgc(cyan))]),sol([largest_indiv,trim_to_used,set_bg(cyan)])]).
fav(t('a48eeaf7'),[sol([largest_indiv(I),tiny_individuals(Is),gravity_to(Is,I)])]).
fav(t('8d5021e8'),[sol([growTimes([[rot180,flipV],
                                   [flipH ,same],
                                   [rot180,flipV]])])]).

fav(t('7b6016b9'),[sol([fillFromBorder(green),subst_color(black,red)])]).
fav(t('810b9b61'),[sol([find_individuals([hollow,boxes],I),indiv_set_color(I,green)])]).
fav(t('44d8ac46'),[sol([find_individuals([hollow,boxes,inside([square])],I),indiv_fill_color(I,red)])]).
fav(t('9aec4887'),[indiv(color_blind),todo_sol([find_individuals([hollow,inside([square])],I),rest_indivdual(Is),put_inside(Is,I),
  if_edge_strong([color=C]),touch(Is,Point),set_point(Point,C)])]).
%fav(t('db3e9e38'),[sol([flipV,C1=orange,C2=blue,[],flipV]).
fav(t('1cf80156'),[sol([trim_to_used])]).
%fav(t(_),[sol([fillFromBorder(none,yellow)])]).
fav(t('47c1f68c'),[sol([compute_max_color(C1),compute_next_color(C2),
 
 blacken_color(C1),subst_color(C2,C1),
 trim_to_used_square,
 grow([[self,rot90],
      [rot270,rot180]])])]).

fav(t('447fd412'),[sol([find_two_color_indivs,find_lesser_block,select_scaled_versions,builds,create_greater_blocks])]).
fav(t('ed36ccf7'),[sol([rot270])]).
fav(t('5521c0d9'),[sol([with_each_indiv,move_above_itself])]).
fav(t('5c2c9af4'),[sol([two_closest_dots_to_edge,make_a_box,grow_box_that_much_bigger,grow_box_that_much_bigger,grow_box_that_much_bigger])]).
fav(t('27a28665'),[learn(shape_to_color),sol([make_box(1),shape_to_color(C),cls_with(C)])]).
fav(t('6cf79266'),[learn(find(nines),remove_them),sol(reverse_learned)]).
fav(t('97999447'),[sol([find_ones,unil_edges(copy_right(grey),copy_right(self))])]).
fav(t('dae9d2b5'),[sol([find_by_color,combine_all,set_all_fg(magenta)])]).
fav(t('3631a71a'),[learn(find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves)]).

fav(t('9ecd008a'),[learn(find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves),
    sol(indiv_is_one_hole,fix_image,selected_indiv,trim_to_used)]).
fav(t('5117e062'),[sol([find_two_color_indivs,selected_indiv,trim_to_used,main_color,paint_landscape])]).
fav(t('7f4411dc'),[sol([shave_away_1s])]).
fav(t('1b60fb0c'),[learn(find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves),
    sol(new_things_are_a_color,fix_image)]).
fav(t('d6ad076f'),[sol(find_smaller,shoot_at_other,wide_beam)]).
fav(t('6d58a25d'),["the blue object is a downward beam maker, each beam must connect to one of its colors "]).
fav(t('23b5c85d'),[sol([smallest_indiv,trim_to_used])]).
%fav(t('23b5c85d'),[b7249182
fav(t('8be77c9e'),[sol([grow([[self],[flipV]])])]).


/*
first i look to see if the grid sizes are purporional, if not i look to see if the output grid can be recognised on the input
if not i look to see if the input grid can be recognised on the output

f35d900a

the input has a small number of points .. the same number on each image
two points are the same color, oh there are two pairs of points
there is a grey dashed box made up from the points
no two grey dots can touch though
the grey dots seem to originate from the four points
teh grey has to come out at least once.. after than if they are going to become two they skip
arround each color point there is a box of the oposting color



 cbded52d

we have a blue background and black TTT board .. to blue openings are made of 2x2s
in each square there can be a color.. each color has a default pos on the 2x2
if like in the game of TTT you can win, but not diagonlly.. place the color on that area


 150deff5.json

  there is a hairything and it is soem color we will paint the entire thing another color
  now we will find the jaggedity bits and paint magenta in dashes of three lonbg until all that is left is 2x2 s





*/
% S=[[1,2,3],[4,x,6],[7,8,0]],grow([[self,self],[self,self]],S, X).

% grow([[self,self]],[[a,b,c]], [[a,b,c,a,b,c]]).
append_left(Grid1,[],Grid1):-!.
append_left(Grid1,Grid2,Grid):- length(Grid1,Len),assertion(length(Grid2,Len)),maplist(append,Grid1,Grid2,Grid).
append_down(Grid1,Grid2,Grid):- append(Grid1,Grid2,Grid).

grow_row([],_,[]).
grow_row([C1],Grid,G1):- !, solving_progs(C1,Grid,G1).
grow_row([C1|Row],Grid,GM):- !, solving_progs(C1,Grid,G1),grow_row(Row,Grid,GR),append_left(G1,GR,GM).
grow([],_,[]).
grow([[Self]],Grid,GridO):- !, solving_progs(Self,Grid,GridO).
grow([Row|Rows],Grid,G1GridO):- grow_row(Row,Grid,G1), grow(Rows,Grid,GridO),append(G1,GridO,G1GridO).

largest_indiv(Grid,GridO):- individuals(Grid,[Points|_]),points_to_grid(Points,GridO).
largest_indiv(GridO,Grid,Grid):- largest_indiv(Grid,GridO).
smallest_indiv(Grid,GridO):- individuals(Grid,Is),last(Is,Points),points_to_grid(Points,GridO).
smallest_indiv(GridO,Grid,Grid):- smallest_indiv(Grid,GridO).


trim_to_used_square(G,G9):- get_bgc(BG),
  trim_unused_vert_square(BG,G,G1),
  trim_unused_vert_square(BG,G1,G2),
  trim_unused_vert_square(BG,G2,G3),
  trim_unused_vert_square(BG,G3,G9).

trim_to_used(G,G9):- get_bgc(BG),trim_unused_vert(BG,G,G1),rot90(G1,G2),trim_unused_vert(BG,G2,G3),rot270(G3,G9).

%:- nb_setval(grid_bgc,8).

set_bg(C0,Grid,GridO):- color_num(C0,C),  nb_setval(grid_bgc,X), get_bgc(X),pred_subst(if_bgc_then(X,C), Grid, GridO),
  wdmsg(set_bg(C,Grid,GridO)).
if_bgc_then(X,C,B,A):- is_bg_or_var(X,B)->A=C; number(B),A=B.

get_bgc(X):- nb_current(grid_bgc,X),!.
get_bgc(X):- X = 0.


set_bgc(C):- color_num(C,N),C\==N,!,set_bgc(N).
set_bgc(C):- nb_setval(grid_bgc,C),!.

is_bg_or_var(_,X):- free_cell(X),!.
is_bg_or_var(BG,X):- X==BG.


free_cell(Var):- var(Var),!.
free_cell(0).
free_cell(8).
free_cell(C):- get_bgc(X),C==X.

trim_unused_vert_square(_,[],[]).
%trim_unused_vert_square(_,_,GridO,GridO):-grid_size(GridO,size(H,W)),H=W,!.
trim_unused_vert_square(BG,[Row|Grid],Grid90):- maplist(is_bg_or_var(BG),Row),rot90(Grid,[Col|Grid90]),
   maplist(is_bg_or_var(BG),Col).
trim_unused_vert_square(_,G1,Grid90):- rot90(G1,Grid90).
/*  
   trim_unused_vert_square(BG,Grid,GridO).
trim_unused_vert_square(BG,GridR,GridO):- append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),trim_unused_vert_square(BG,Grid,GridO).
trim_unused_vert_square(_,G,G).*/

trim_unused_vert([],[]).
trim_unused_vert(BG,[Row|Grid],GridO):- maplist(is_bg_or_var(BG),Row),trim_unused_vert(BG,Grid,GridO).
trim_unused_vert(BG,GridR,GridO):- append(Grid,[Row],GridR),maplist(is_bg_or_var(BG),Row),trim_unused_vert(BG,Grid,GridO).
trim_unused_vert(_,G,G).
%fav(t('25d487eb'),[sol([rocketship])]).
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

compute_max_color(Color1,Grid,Grid):- colors_count_no_black(Grid,[_-Color1|_]).
compute_next_color(Color1,Grid,Grid):- colors_count_no_black(Grid,[_,_-Color1|_]).

subst_color(Color1,Color2,Grid,NewGrid):- notrace((color_num(Color1,Num1),color_num(Color2,Num2),subst(Grid,Num1,Num2,NewGrid))).
blacken_color(Color1,Grid,NewGrid):- subst_color(Color1,0,Grid,NewGrid).
swap_colors(Color1,Color2,Grid,NewGrid):- subst_color(Color1,Swap1,Grid,MGrid),
                                          subst_color(Color2,Color1,MGrid,NewGrid),
                                          color_num(Color2,Swap1).

backfill_vars(GridO):- clause(backfill(GridO),true).

unbind_color(Color1,Grid,GridO):- color_num(Color1,Num1),unbind_color0(Num1,Grid,GridO).
unbind_color0(Num1,Grid,GridO):- is_list(Grid),!,maplist(unbind_color0(Num1),Grid,GridO).
unbind_color0(Num1,Num1,_):-!.
unbind_color0(_,Num1,Num1).

colors_to_vars(Grid,GridO):- unique_colors(Grid,Colors0),include(integer,Colors0,Colors),
 length(Colors,Len),length(Vars,Len),
 subst_list_vars(Colors,Vars,Grid,GridO).

subst_list_vars(B,A,Grid,GridO):- subst_list_vars0(B,A,Grid,GridO).

subst_list_vars0([],_,A,A):-!.
subst_list_vars0([F|FF],[R|RR],S,D):- !,subst(S,F,R,M), subst_list_vars0(FF,RR,M,D).
/*
subst_list_vars0(B,A,Grid,GridO):- is_list(Grid),!,maplist(subst_list_vars0(B,A),Grid,GridO).
subst_list_vars0(F,R,S,D):- nth1(N,F,E),E==S,nth1(N,R,D),!.
subst_list_vars0(_,_,V,V).
*/


add_borders(Color1,Grid,GridO):- color_num(Color1,Num1),!,outline_with0(Num1,Grid,GridO),!.

outline_with0(Num1,Grid,GridO):- 
 grid_size(Grid,size(H,V)),
 must_det_l((
  replace_row_e(1,Num1,Grid,G1),
  replace_row_e(V,Num1,G1,G2),
  replace_col_e(1,Num1,G2,G3),
  replace_col_e(H,Num1,G3,GridO))).

cls_with(Color1,Old,Grid):- color_num(Color1,Num1),cls_with0(Num1,Old,Grid),!.
cls_with0(Color1,Old,Grid):- is_list(Old),!,maplist(cls_with0(Color1),Old,Grid).
cls_with0(Color1,_,Color1).

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

replace_row(N,Row,Grid,NewGrid):- grid_size(Grid,size(H,V)), replace_row(N,Row,Grid,size(H,V),NewGrid).
replace_row(N,Row,Grid,size(H,V),NewGrid):- N<0, NewN is V + N+1,!,replace_row(NewN,Row,Grid,size(H,V),NewGrid).
replace_row(N,Row,Grid,_,NewGrid):- set_nth1(N,Grid,Row,NewGrid).

replace_row_e(N,E,Grid,NewGrid):- grid_size(Grid,size(H,V)), make_list(H,E,Row), replace_row(N,Row,Grid,size(H,V),NewGrid).
replace_col_e(N,E,Grid,NewGrid):- grid_size(Grid,size(H,V)), make_list(V,E,Col), replace_col(N,Col,Grid,size(H,V),NewGrid).

replace_col(N,Col,Grid,NewGrid):- grid_size(Grid,size(H,V)), replace_col(N,Col,Grid,size(H,V),NewGrid).
replace_col(N,Col,Grid,size(H,V),NewGrid):- N<0, NewN is H + N+1,!,replace_col(NewN,Col,Grid,size(H,V),NewGrid).

replace_col(N,Col,Grid,size(_,V),NewGrid):- Nm1 is N - 1, length(Col,V),maplist(replace_col_at0(Nm1),Col,Grid,NewGrid).

replace_col_at0(N,Col,Row,NewRow):- length(Left,N),append(Left,[_|Right],Row),append(Left,[Col|Right],NewRow).


contains_nonvar(N,Info):- sub_term(E,Info),nonvar(E),E=N,!.
debug_indiv:- test_cond(indiv(_)), \+ test_cond(nodebug_indiv),!.
test_info(Name,InfoF):- findall(Inf,fav(Name,Inf),Info),flatten(Info,InfoF).
fav(X):- clause(fav(X,_),true).

test_names_by_hard(Name):- fav(Name).
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

max_min(A,B,A,B):- A>B,!.
max_min(A,B,B,A).

hardness_of_name(Name,Hard):-
 %Type=tst+_,
 Type=_,
 findall(Hard,
 (kaggle_arc(Name,Type,In,Out),
  grid_size(In,size(H0,V0)),
  grid_size(Out,size(H,V)),
  max_min(H,V,C,_),
  max_min(H0,V0,C0,_),
  HV is C*C, HV0 is C0*C0,
  max_min(HV,HV0,Max,Min),
  D is Max-Min,
  Hard is Max*10000+D),All),
  sort(All,AllK),last(AllK,Hard).

%tell(s),ignore((nl,nl,test_pairs(Name,Type,In,Out),format('~N~q.~n',[test_pairs_cache(Name,Type,In,Out)]),fail)),told.

is_grid([[C|H]|R]):- is_list(H),is_list(R),(integer(C);var(C)),
  length([C|H],L),is_row_len(L,[C|H]),maplist(is_row_len(L),R).

is_row_len(N,L):- length(L,N).

:- dynamic(backfill/1).

current_test_name(Name):- nb_current(test_name,Name),!.
current_test_name([]).

functor_color(pass,green).
functor_color(fail,red).
functor_color(warn,yellow).

arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_color(F,C),wots(S,print(G)),color_print(C,S),!,format('~N').
arcdbg(G):- wdmsg(G).

print_arc_in_out(Name,Type,In,Out):-
 setup_call_cleanup((
  current_test_name(CName),
  nb_setval(test_name,Name),
  ignore(maybe_do(CName,Name,Type,In,Out))),true,notrace).


%maybe_do(_,_,_,_,_):- \+ test_cond(sol(_)),!.
maybe_do(CName,Name,Type,In,Out):-
  ignore((CName\==Name,dash_char(60,"A"),dash_char(6,"\n"),nl)),  
     dash_char(60,"V"),nl,
     describe_feature(Name,[test_info]),
     print_grid(Name=in(Type),In),
     print_grid(Name=out(Type),Out),
    \+ \+ ignore(((Type = trn+_), test_cond(learn(CProg)),must(training_progs(CProg,In,Out)))),
     confirm(Name,Type,In,Out),nl,!.


confirm(Name,Type,In,Out):- 
   ignore((sols_for(Name,Sol), confirm_sol(Sol,Name,Type,In,Out))).

sols_for(Name,Sol):- test_info(Name,Sols),member(sol(Sol),Sols).

confirm_sol(Prog,Name,Type,In,Out):- 
   solving_progs(Prog,In,GridO)
   *-> 
   count_difs(Out,GridO,Errors),
   (Errors==0 -> arcdbg(pass(Name,Type,Prog));(arcdbg(fail(Errors,Name,Type,Prog)),
     red_noise,
     once(print_grid(GridO)),
     red_noise))
   ;arcdbg(warn(unrunable(Name,Type,Prog))).

red_noise:- format('~N'),
  color_print(red,'--------------------------------------------------------------'),nl,
  color_print(red,'--------------------------------------------------------------'),nl,
  color_print(red,'--------------------------------------------------------------'),nl.

count_difs(Out,GridO,0):- Out=@=GridO,!.
count_difs(Out,GridO,1):- ((\+ compound(Out)) ; \+ compound(GridO)),!.
count_difs([A|Out],[B|GridO],Errors):- !,
      count_difs(A,B,Errors1),
      count_difs(Out,GridO,Errors2),
      Errors is Errors1 + Errors2.
count_difs(Out,GridO,Errors):-
  compound_name_arguments(Out,F,A),
  compound_name_arguments(GridO,FO,AO),
  count_difs([F|A],[FO|AO],Errors).
  

print_arc(Name,json(JSON)):-!, print_arc(Name,JSON).
print_arc(Name,trn=Y):- !, print_arc(Name,Y).
print_arc(Name,X->Y):- !, print_arc(in(Name),X), print_arc(out(Name),Y).
print_arc(Name,X=Y):- !, print_arc(Name=X,Y).
print_arc(Name,[H|L]):- is_grid([H|L]),!,print_grid(Name,[H|L]).
print_arc(Name,[H|L]):- !, maplist(print_arc(Name),[H|L]).
print_arc(Name,Val):- print_tree_nl(Name=Val).

% Grid pretty printing
print_grid(Name,Grid):- 
  % dash_char(60,"="),  
  arcdbg(Name),
  % assert_id_grid_cells(Name,Grid),!,
  describe_feature(Grid,[grid_size,colors_count_size,colors_count,=,individuals]).
describe_feature(Grid,List):- is_list(List),!,maplist(describe_feature(Grid),List).
describe_feature(Grid,Pred):- call(Pred,Grid,Res)->print_equals(Grid,Pred,Res);print_equals(Pred,f),!.
  
print_equals(Grid,individuals,Res):- grid_size(Grid,size(H,V)),  
   maplist(embue_points(size(H,V)),Res,ResO),
   print_equals(individuals,ResO).
print_equals(_Grid,Pred,Res):- print_equals(Pred,Res).

writeqln(X):- format('~q',[X]),format('~N').


dash_char(H,C):-forall(between(0,H,_),write(C)).
print_equals(N,V):- \+ compound(V),writeqln(N=V).
print_equals(=,V):- is_grid(V),grid_size(V,size(H,_)),!,dash_char(H,"="),print_grid(V).
print_equals(N,V):- is_grid(V),!,writeqln(N),print_grid(V).
print_equals(N,[G|V]):- is_list_of_points([G|V]),
  length([G|V],Len),
  grid_size(G,size(H,_V)),
  writeqln(N=len(Len)),  !,
 ignore((debug_indiv,
  dash_char(H,"-"),nl,
  forall(member(E,[G|V]),(print_points_grid(E),dash_char(H,"-"),nl)))).

print_equals(N,[G|V]):- 
  is_grid(G),is_list(V),maplist(is_grid,V),!,
  length([G|V],Len),
  grid_size(G,size(H,_V)),
  writeqln(N=len(Len)),  
  dash_char(H,"-"),
  forall(member(E,[G|V]),(print_grid(E),dash_char(H,"-"),nl)).
print_equals(N,V):- better_value(V,BV)-> BV\=@=V, !,print_equals(N,BV).
print_equals(N,[S|L]):- string(S),writeq(N),write('= '),write(S),maplist(commawrite,L),nl.
print_equals(N,V):- print_tree(N=V).

commawrite(S):- write(','),write(S).

is_list_of_points([G|V]):- is_points(G),is_list(V),maplist(is_points,V).

print_points_grid(Points):- points_range(Points,LoH,LoV,HiH,HiV),
  writeqln(size_range(LoH,LoV,HiH,HiV)),points_to_grid(Points,Grid),
  print_grid(Grid).

as_color(Count-Num,List):- color_name(Num,Name),wots(List,color_print(Num,Name=Count)).
better_value(V,List):- is_list(V), maplist(as_color,V,List).
better_value([G|V],List):- 
  is_list_of_points([G|V]),
  maplist(points_to_grid,[G|V],List),
  [G|V] \=@= List.

black_first(SK,[N-0|BF]):-select(N-0,SK,BF),!.
black_first(BF,[0-0|BF]).

no_black(SK,BF):-select(_-0,SK,BF),!.
no_black(SK,BF):-select(0,SK,BF),!.
no_black(BF,BF).

pixels(G,GL):- flatten(G,GF),include(integer,GF,GL).
unique_colors(G,UC):- pixels(G,GF),sort(GF,UC).
colors_count_size(G,UC):- colors_count_no_black(G,GS),length(GS,UC).

colors_count(G,SK):- pixels(G,GF),sort(GF,GS),count_each(GS,GF,UC),keysort(UC,KS),reverse(KS,SK).
colors_count_black_first(G,BF):- colors_count(G,SK),black_first(SK,BF).
colors_count_no_black(G,BF):- colors_count(G,SK),no_black(SK,BF).

num_objects(G,NO):- individuals(G,GS),length(GS,NO).

count_each([],_,[]).
count_each([C|L],GC,[Len-C|LL]):- include(==(C),GC,Lst),length(Lst,Len),count_each(L,GC,LL).

%print_grid(Grid):- is_grid(Grid),!, maplist(print_rows,Grid),nl.
print_grid(Grid):- grid_size(Grid,size(HH,VV)),
  forall(between(1,VV,V),
   ((format('~N'),forall(between(1,HH,H), 
     (hv_value_or(Grid,C,H,V,0),
       print_g(C)))))),format('~N').
print_rows(List):- maplist(print_g,List),nl.
%block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
block_colors([(black),(blue),(red),(green),(yellow),Silver,(magenta),'#ff8c00',(cyan),'#8b4513']):- silver(Silver),!.
named_colors([(black),(blue),(red),(green),(yellow),(silver),(purple),(orange),(cyan),(brown)]).
named_colors([(black),(blue),(red),(green),(yellow),(grey),(magenta),(orange),(cyan),(brown)]).

silver('#7b7b7b').
silver('#c0c0c0').
silver('#9a9a9a').


color_print(C,W):- atom(C),color_num(C,N),integer(N),!,color_print(N,W).
color_print(C,W):- integer(C),C\==0,block_colors(L),nth0(C,L,Color),ansi_format(fg(Color),'~w',[W]),!.
color_print(C,W):- (var(C);C=0),!,write(W).
color_name(C,W):-  named_colors(L),nth0(C,L,W).
color_num(C,W):- integer(C),!,W=C.
color_num(C,W):- atom(C),!,named_colors(L),nth0(W,L,C),!.
color_num(C,C).


print_g(V):- var(V),!,write(' ?').
print_g(0):- !,write(' .').
print_g(N):- write(' '),print_g1(N).
print_g1(C):- color_print(C,'o'),!.
print_g1(C):- write(' '),write(C).

arc_test_1(Name):- doall(print_trainer(Name)),nop((individuals(Name=out(tst),O),print_equals(parint_grid,O))).
arc_test_1:- arc_test_1(v('009d5c81')).
arc_test_2:- arc_test_1(t('25d487eb')).
arc_test:- arc_test_2.
arc_test:- arc_test_1.
erase_grid(ID):- retractall(cmem(ID,_HV,_C)).

grid_cells(ID,Cells):- \+ \+ cmem(ID,_,_), findall(-(C,HV),cmem(ID,HV,C),Cells).
grid_cells(Grid,Cells):- grid_to_id(Grid,ID),findall(-(C,HV),cmem(ID,HV,C),Cells).

assert_id_cells(ID,Cells):- maplist(assert_id_cell(ID),Cells).
assert_id_cell(ID,-(C,HV)):- assert(cmem(ID,HV,C)).
assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(cmem(ID,HV,C)).

:- dynamic(is_grid_id/2).
grid_to_id(Grid,ID):- is_grid_id(Grid,ID),!.
grid_to_id(Grid,ID):- gensym('grid_',ID),assert_id_grid_cells(ID,Grid),assert(is_grid_id(Grid,ID)),!.

:- dynamic(grid_sz/3).
% Grid to_fast_workspace
assert_id_grid_cells(ID,Grid):-
   grid_size(Grid,size(SH,SV)),
   retractall(grid_sz(ID,_,_)),
   assert(grid_sz(ID,SH,SV)),
   erase_grid(ID),   
   forall(between(1,SH,H),
    forall(between(1,SV,V),
     ignore((hv_value(Grid,C,H,V),C\==0,assert_hvc_cell(ID,H,V,C))))).


% Random Non Blk Eles
first_color(Grid1,C1):- sub_term(C1,Grid1),C1 \= 0,integer(C1).

% Grid size/resize
make_lengths(N,L):- length(L,N).


grid_size(ID,size(H,V)):- grid_sz(ID,H,V),!.
grid_size(Grid,Size):- notrace( is_points(Grid)), !, points_size(Grid,Size).
grid_size(Grid,Size):- grid_size_nd(Grid,Size),!.
grid_size(_,size(30,30)).

as_hv_point(H,V,C,C-Point):- hv_point(H,V,Point),!.
as_hv_point(H,V,_,Point):- hv_point(H,V,Point),!.
as_hv_point(H,V,_,size(H,V)).
%as_hv_point(inf,inf,size_range(_,_,_,_)).


points_size(Grid,Size):- 
   Size = size(_,_), member(Size,Grid).
points_size(Grid,Size):- 
   size_range(inf,inf,0,0,Grid,_,_,H,V),!,
   Size = size(H,V).


points_range(Points,LoH,LoV,HiH,HiV):-
   size_range(inf,inf,0,0,Points,LoH,LoV,HiH,HiV).

size_range(WLoH,WLoV,WHiH,WHiV,[],WLoH,WLoV,WHiH,WHiV).
size_range(WLoH,WLoV,WHiH,WHiV,[E|L],LoH,LoV,HiH,HiV):- !,
  size_range(WLoH,WLoV,WHiH,WHiV,E,MLoH,MLoV,MHiH,MHiV),
  size_range(MLoH,MLoV,MHiH,MHiV,L,LoH,LoV,HiH,HiV).
size_range(_,_,_,_,size_range(LoH,LoV,HiH,HiV),LoH,LoV,HiH,HiV):-!.
size_range(WLoH,WLoV,WHiH,WHiV,size_range(ILoH,ILoV,IHiH,IHiV),LoH,LoV,HiH,HiV):- 
  max_min(WLoV,ILoV,_,LoV),max_min(WHiV,IHiV,HiV,_),
  max_min(WLoH,ILoH,_,LoH),max_min(WHiH,IHiH,HiH,_).

size_range(WLoH,WLoV,WHiH,WHiV,size(_,_),WLoH,WLoV,WHiH,WHiV).

size_range(WLoH,WLoV,WHiH,WHiV,fsize(IHiH,IHiV),WLoH,WLoV,HiH,HiV):- 
  max_min(WHiV,IHiV,HiV,_), max_min(WHiH,IHiH,HiH,_).


size_range(WLoH,WLoV,WHiH,WHiV,Point,LoH,LoV,HiH,HiV):- as_hv_point(H,V,_,Point),!,
  max_min(WLoV,V,_,LoV),max_min(WHiV,V,HiV,_),
  max_min(WLoH,H,_,LoH),max_min(WHiH,H,HiH,_).


grid_size_nd([C,R|Rows],size(H,V)):- 
   (var(Rows)->between(2,30,V);!), 
   length([C,R|Rows],V),
   (var(R)->between(1,30,H);true), 
   length(R,H),
   (is_list(C)->true;(length(C,H),maplist(make_lengths(H),Rows))).
grid_size_nd([L],size(H,1)):- (var(L)->between(1,30,H);true), length(L,H).

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
one_change(same_size,Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C1),!.
one_change(resize(C1,C2),Grid1,Grid2):- var(Grid2),grid_size(Grid1,C1),grid_size(Grid2,C2).

adjacent_point(HV,HV2):- adjacent_point(HV,_Dir,HV2).

individuals(ID,IndvO):- 
  individuals_raw(ID,Indv),
  unraw_inds(Indv,Indv1),
  largest_first(Indv1,IndvO).

unraw_inds(IndvS,IndvO):-
  largest_first(IndvS,Indv1), 
  %trace,
  test_cond_or(indiv(min(SZ)),2),
  check_minsize(SZ,Indv1,Indv),
  unraw_inds2(_,Indv,IndvO),!.


test_cond(This):- current_test_name(Name),test_info(Name,InfoL),!,contains_nonvar(This,InfoL).

test_cond_or(This,_That):- test_cond(This),!.
test_cond_or(This, That):- term_variables(This,[That|_]),!.

check_minsize(Sz,Indv1,Indv):- include(meets_size(Sz),Indv1,Indv).

meets_size(Len,Points):- length(Points,L),L>=Len.

largest_first(IndvS,IndvO):- 
  findall(Hard-Indv,(member(Indv,IndvS),length(Indv,Hard)),All),
  keysort(All,AllK),
  reverse(AllK,AllR),
  findall(Indv,member(_-Indv,AllR),IndvO).

unraw_inds2(GrpTiny,IndvS,IndvO):-   fail,
  select([C-Point1],IndvS,Rest1),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(GrpTiny,IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|CRest],
  IndvO=[Grp|IndvMO].

unraw_inds2(GrpTiny,IndvS,IndvO):-   
  select([C-Point1],IndvS,Rest1), \+ free_cell(C),
  select([C2-Point2],Rest1,Rest),
  findall(C3-Point3,member([C3-Point3],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(GrpTiny,IndvM,IndvMO),
  Grp=[C-Point1,C2-Point2|CRest],
  IndvO=[Grp|IndvMO].

unraw_inds2(GrpTiny,IndvS,IndvO):-   
  select([C-Point1],IndvS,Rest1),
  select(Grp,Rest1,Rest),
  adjacent_groups([C-Point1],Grp),
  unraw_inds2(GrpTiny,[[C-Point1|Grp]|Rest],IndvO).

/*
unraw_inds2(GrpTiny,IndvS,IndvO):-   
  select([C1-Point1],IndvS,Rest),
  nearby_one(C,C1-Point1,Rest),
  select([C-Point2],Rest1,Rest),
  findall(C-Point,member([C-Point],Rest),CRest),
  subtract(Rest,CRest,IndvM),
  unraw_inds2(GrpTiny,IndvM,IndvMO),
  Grp=[C-Point1,C-Point2|C-Rest],
  IndvO=[Grp|IndvMO].
*/
unraw_inds2(_,IndvS,IndvS).

adjacent_groups(Grp1,Grp2):- member(_-P1,Grp1),member(_-P2,Grp2),adjacent_point(P1,P2).


individuals_raw(ID,Indv):-
  grid_cells(ID,Cells),
  individuals_list(Cells,Indv).

individuals_list([],[]):-!.
individuals_list(Cells,[Indv|IndvList]):- 
    select(C-HV,Cells,Rest), \+ free_cell(C), 
    adjacent_point(HV,HV2),select(C-HV2,Rest,ScanPoints),!,  
    all_individuals_near(C,[C-HV,C-HV2],ScanPoints,NextScanPoints,Indv),!,
    individuals_list(NextScanPoints,IndvList). 
individuals_list(Cells,IndvList):- maplist(obj1,Cells,IndvList).
obj1(X,[X]).

all_individuals_near(_C,NewSet,[],[],[],NewSet):-!.
all_individuals_near(C,Indv,ScanPoints,NewScanPoints,NewSet):-
   individuals_near(C,Indv,ScanPoints,New,NextScanPoints),
   (New == [] -> (NewSet = Indv, NewScanPoints = NextScanPoints)
    ; (append(Indv,New,IndvNew),
        all_individuals_near(C,IndvNew,NextScanPoints,NewScanPoints,NewSet))).

individuals_near(_C,_From,[],[],[]):-!.

individuals_near(C,From,[E|ScanPoints],[E|Nears],NextScanPoints):- nearby_one(C,E,From),!,
  individuals_near(C,[E|From],ScanPoints,Nears,NextScanPoints).

individuals_near(C,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- individuals_near(C,From,ScanPoints,Nears,NextScanPoints).

nearby_one(C,C-E,List):- adjacent_point(E2,E), member(C-E2,List).
/*
nearby_one(C,C-E,List):- adjacent_point(E2,E), member(OC-E2,List),nothing_near(C,E,OC,E2,List).

nothing_near(OrigC,OrigPoint,OC,E,List):- OC\==OrigC,
          adjacent_point(E2,E), member(C2-E2,List),
          E2\==OrigPoint, C2\==OrigC, C2\==OC.
*/

individuals_list([],[],[]).
individuals_list([C-HV|T],[C-HV,How|Adjs],Rest):-
  adjacent_point(HV,How,HV2),
  select(C-HV2,T,RT),RT\==T,!,
  individuals_list([C-HV2|RT],Adjs,Rest).
individuals_list([C-HV|T],[C-HV],T). 
% eventually use 2nd arg as a hueristic

%g(V,H,obj(OV,OH,C,[CellList])).

%individuals_from(Cells,C,HV,[How|Indv]):- adjacent_point(HV,How,HV2),individual_from(ID,C,H2,V2,Indv).
%individuals_from(Cells,_C,_HV,[]):- !.

embue_points(size(H,V),Res,[size(H,V)|Res]). 


points_to_grid(Points,Grid):- 
  must_det_l((
  grid_size(Points,size(H,V)),
  grid_size_nd(Grid,size(H,V)),
  maplist(add_point(Grid),Points))).


insert_row(N,Row,Grid,NewGrid):- grid_size(Grid,size(H,V)), insert_row(N,Row,Grid,size(H,V),NewGrid).
insert_row(N,Row,Grid,size(H,V),NewGrid):- N<0, NewN is V + N+1,!,insert_row(NewN,Row,Grid,size(H,V),NewGrid).
insert_row(N,Row,Grid,size(H,_),NewGrid):- length(Row,H),length(Left,N),append(Left,Right,Grid),append(Left,[Row|Right],NewGrid).

insert_col(N,Col,Grid,NewGrid):- grid_size(Grid,size(H,V)), insert_col(N,Col,Grid,size(H,V),NewGrid).
insert_col(N,Col,Grid,size(H,V),NewGrid):- N<0, NewN is H + N+1,!,insert_col(NewN,Col,Grid,size(H,V),NewGrid).
insert_col(N,Col,Grid,size(_,V),NewGrid):- length(Col,V),maplist(insert_col_at(N),Col,Grid,NewGrid).

insert_col_at(N,Col,Row,NewRow):- length(Left,N),append(Left,Right,Row),append(Left,[Col|Right],NewRow).

insert_ele(N,V,L,NL):- length(Left,N),append(Left,Right,L),append(Left,[V|Right],NL).

delete_row(N,Grid,NewGrid):- N < 0, length(Grid,L), DR is L+N+1,delete_row(DR,Grid,NewGrid).
delete_row(N,Grid,NewGrid):- length(Left,N),append(Left,[_|Right],Grid),append(Left,Right,NewGrid).

delete_col(N,Grid,NewGrid):- maplist(delete_row(N),Grid,NewGrid).

map_nth(P,N,Grid):- nth1(N,Grid,E),call(P,E).
map_row(P,N,Grid):- map_nth(maplist(P),N,Grid).
map_col(P,N,Grid):- maplist(map_nth(P,N),Grid).

add_point(_,size(_,_)).
add_point(Grid,Point):- as_hv_point(H,V,C,Point),add_h_v_c(Grid,H,V,C).
%add_h_v_c(Grid,H,V,C):- var(C),!,nop(add_h_v_c(Grid,H,V,C)).
add_h_v_c(Grid,H,V,C):- hv_value(Grid,Was,H,V),ignore(Was=C).



hv_value_or(Grid,C,H,V,Else):- hv_value(Grid,C,H,V)*->true;C=Else.
hv_value(ID,C,H,V):- cmem(ID,HV,C),hv_point(H,V,HV).
hv_value(Points,C,H,V):- is_list(Points), is_points(Points), !,member(C-HV,Points),hv_point(H,V,HV).
hv_value(Grid,C,H,V):- is_grid(Grid),!,nth1(V,Grid,Row),nth1(H,Row,C).
/*b_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),set_nth1(H,Row,C).
nb_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),nb_set_nth1(H,Row,C).
b_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),rplc_nth1(H,Row,OldC,NewC).
nb_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),nb_rplc_nth1(H,Row,OldC,NewC).

*/
solving_progs(Prog,In,Out):- var(Prog),!,throw(var_solving_progs(Prog,In,Out)).

solving_progs(sol(Prog),In,Out):- !, solving_progs(Prog,In,Out).
solving_progs(call(G),In,Out):-!,call(G),(var(Out)->Out=In; true).
solving_progs([],In,Out):-!, var(Out)->Out=In; true.
solving_progs(self,In,Out):-!, duplicate_term(In,Out).
solving_progs([H|Prog],In,Out):-!, solving_progs(H,In,GridM), solving_progs(Prog,GridM,Out).
solving_progs(Prog,In,In):- missing_arity2(Prog),!,arcdbg(warn(missing(solving_progs(Prog)))).
solving_progs(Prog,In,Out):- call(Prog,In,Out)*-> true ; arcdbg(warn(nonworking(solving_progs(Prog)))).

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
trim_grid_to_size(point,size),
fill_from_point(point,color),
create_a_ray(point,dir,len),
object_as_own_grid(obj,gridOps),
copy_one_object(obj,point),
rotate_one_object(obj,nsew),
flatten_one_object(obj),
sort_by_gravity(nsew),
flip_grid(hOrv),
rotate_grid(nsew)]).

is_points(H):- is_list(H),maplist(is_cpoint,H).
is_cpoint(C):- \+ compound(C),!,fail.
is_cpoint(size_range(C,_,_,_)):- !, nonvar(C).
is_cpoint(prop(_)).
is_cpoint(prop(_,_)).
is_cpoint(size(_,_)).
is_cpoint(C-P):- integer(C),!,atom(P).
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
    assert_if_new(adjacent_point(HV,Dir,HV2)),
    assert_if_new(hv_point(H,V,HV)),
    assert_if_new(adjacent_point(H,V,Dir,H2,V2)))).
  
  



nav(s,0,1). nav(e, 1,0). nav(w,-1,0). nav(n,0,-1).
nav(se, 1,1). nav(sw,-1,1). nav(nw,-1,-1). nav(ne, 1,-1).

rot45(s,sw). rot45(sw,w). rot45(w,nw). rot45(nw,n). rot45(n,ne). rot45(ne,e). rot45(e,se). rot45(se,s).


copy_cells(B,A,H,HH):- call(B,H),!,call(A,HH).
copy_cells(_,_,H,H):- \+ is_list(H),!.
copy_cells(_,_,[],[]):-!. 
copy_cells(B,A,[H|T],[HH|TT]):-!, copy_cells(B,A,H,HH), copy_cells(B,A,T,TT).
  

same_grid(Grid1,Grid1).

  


