test_name(Name):- 
  findall(Name,kaggle_arc(Name,_,_,_),All),
  list_to_set(All,AllS),member(Name,AllS).
test_info(Name,InfoS):- fix_test_name(Name,CName,_),findall([Inf],user:fav(CName,Inf),Info),flatten(Info,InfoF),list_to_set(InfoF,InfoS).
was_fav(X):- nonvar(X), clause(fav(XX,_),true),nonvar(XX),X==XX.
test_names_by_hard(Name):- test_names_ord_favs(FavList),test_names_ord_hard(NamesByHard),append(FavList,NamesByHard,All),
 list_to_set(All,AllS),!,member(Name,AllS).
test_names_ord_favs(FavListS):- findall(Name,fav(Name),FavList),list_to_set(FavList,FavListS).
test_names_ord_hard(NamesByHard):- findall(Hard-Name,(test_name(Name),hardness_of_name(Name,Hard)),All), keysort(All,AllK), maplist(arg(2),AllK,NamesByHard),!.

ascending_hard:-
  tell('arc_ascending.pl'),
  forall(test_names_by_hard(Name),
    forall(kaggle_arc(Name,Type,In,Out),format('~q.~n',[kaggle_arc_ord(Name,Type,In,Out)]))),
  told,
  reconsult(arc_ascending).

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


:- dynamic(fav/2).

fav(A,B):- nonvar(A),nonvar(B),
 asserta(fav(A,B),Ref),!,
 call_cleanup((cls,mmake,arc1(A)),erase(Ref)),!.
fav(X,[]):- clause(fav(X),true).
%fav(t('23b5c85d'),[b7249182
%fav(t('db3e9e38'),[lmDSL([flipV,C1=orange,C2=blue,[],flipV]).
%fav(t(_),[lmDSL([fillFromBorder(none,yellow)])]).
fav(v('e9bb6954'),[print_grid,indiv(min(8)),e('box of nine draw outward, if you hit a drawn line blacken it')]).
fav(v('e41c6fd3'),[debug_indiv]).
fav(v('94133066'),[lmDSL([largest_indiv,trim_to_rect,rot90,flipV])]).
fav(v('762cd429'),[]).
fav(v('1d398264'),[debug_indiv]).
fav(t('f76d97a5'),[lmDSL([compute_max_color(C1),compute_next_color(C2),blacken_color(C1),subst_color(C2,C1)])]).
fav(t('ed36ccf7'),[lmDSL([rot270])]).
fav(t('e9bb6954'),[debug_indiv]).
fav(t('dae9d2b5'),[lmDSL([find_by_color,combine_all,set_all_fg(magenta)])]).
fav(t('d6ad076f'),[lmDSL([find_smaller,shoot_at_other,wide_beam])]).
fav(t('d511f180'),[lmDSL([swap_colors(cyan,grey)])]).
fav(t('ae4f1146'),[learn([call(set_bgc(cyan))]),lmDSL([largest_indiv,trim_to_rect,set_bg(cyan)])]).
fav(t('a79310a0'),[lmDSL([gravity(1,s),swap_colors(cyan,red)])]).
fav(t('a48eeaf7'),[lmDSL([largest_indiv(I),tiny_individuals(Is),gravity_to(Is,I)])]).
fav(t('97999447'),[lmDSL([find_ones,until_edges([copy_right(grey),copy_right(same)])])]).
fav(t('8be77c9e'),[lmDSL([grow([[same],[flipV]])])]).
fav(t('810b9b61'),[lmDSL([find_individuals([hollow,boxes],I),indiv_set_color(I,green)])]).
fav(t('7f4411dc'),[lmDSL([shave_away_1s])]).
fav(t('7b6016b9'),[lmDSL([fillFromBorder(green),subst_color(black,red)])]).
fav(t('73251a56'),[learn([learn_mapping_stateful]),lmDSL([apply_mapping_stateful])]).
fav(t('6f8cd79b'),[lmDSL([add_borders(cyan)])]).
fav(t('6d58a25d'),[debug_indiv,print_grid,"the blue object is a downward beam maker, each beam must connect to one of its colors "]).
fav(t('6cf79266'),[learn([find(nines),remove_them]),lmDSL(reverse_learned)]).
fav(t('6150a2bd'),[lmDSL([rot180])]).
fav(t('5c2c9af4'),[lmDSL([two_closest_dots_to_edge,make_a_box,grow_box_that_much_bigger,grow_box_that_much_bigger,grow_box_that_much_bigger])]).
fav(t('5582e5ca'),[lmDSL([compute_max_color(C1),cls_with(C1)])]).
fav(t('5521c0d9'),[lmDSL([with_each_indiv,move_above_itself])]).
fav(t('5117e062'),[lmDSL([find_two_color_indivs,selected_indiv,trim_to_rect,main_color,paint_landscape])]).
fav(t('44d8ac46'),[lmDSL([find_individuals([hollow,boxes,inside([squares])],I),indiv_fill_color(I,red)])]).
fav(t('447fd412'),[lmDSL([find_two_color_indivs,find_lesser_block,select_scaled_versions,builds,create_greater_blocks])]).
fav(t('3c9b0459'),[lmDSL([rot180])]).
fav(t('3631a71a'),[learn([find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves])]).
fav(t('27a28665'),[learn([shape_to_color]),lmDSL([make_box(1),shape_to_color(C),cls_with(C)])]).
fav(t('23b5c85d'),[lmDSL([smallest_indiv,trim_to_rect])]).
fav(t('1cf80156'),[lmDSL([trim_to_rect])]).
%fav(t('d511f180'),[lmDSL([compute_max_color(C1),compute_next_color(C2),swap_colors(C2,C1)])]).
fav(t('8d5021e8'),[lmDSL([grow([[rot180, flipV],[flipH, same],[rot180, flipV]])])]).
fav(t('47c1f68c'),[lmDSL([compute_max_color(C1),compute_next_color(C2), 
 blacken_color(C1),subst_color(C2,C1),
 trim_to_square,
 grow([[same,rot90],
      [rot270,rot180]])])]).

fav(t('9ecd008a'),[learn([find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves]),
    lmDSL([indiv_is_one_hole,fix_image,selected_indiv,trim_to_rect])]).

fav(t('1b60fb0c'),[learn([find_damage_to_input,find_center,fraction_evenly_to_four,map_slices_upon_themselves]),
    lmDSL([new_things_are_a_color,fix_image])]).

fav(t('9aec4887'),[indiv(color_blind),todo_sol([find_individuals([hollow,inside([squares])],I),rest_indivdual(Is),put_inside(Is,I),
  if_edge_strong([color=C]),touch(Is,Point),set_point(Point,C)])]).


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

current_test_name(Name):- nb_current(test_name,Name),!.
current_test_name([]).


/*  

%fav(t('25d487eb'),[lmDSL([rocketship])]).
%fav(v('20818e16'),[guess_bg,indiv(min(2))]).

%(fav(_,P)/(flatten([P],Flat),member(E,Flat))) ==> fav_trait(E).

*/


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
fix_test_name(X,X,_):- var(X),!.
fix_test_name(X+Type,Y,Type):-!,fix_test_name(X,Y,_).
fix_test_name(X=in(Type),Y,Type):-!,fix_test_name(X,Y,_).
fix_test_name(X=out(Type),Y,Type):-!,fix_test_name(X,Y,_).
fix_test_name(X,X,Type):- kaggle_arc(X,Type,_,_),!.
fix_test_name(X,Y,Type):- compound(X),!,arg(_,X,E),nonvar(E),fix_test_name(E,Y,Type).
fix_test_name(X,t(X),_):- kaggle_arc(t(X),_,_,_),!.
fix_test_name(X,v(X),_):- kaggle_arc(v(X),_,_,_),!.

print_trainer0:- arc(t('25d487eb')).
print_eval0:- arc(v('009d5c81')).

