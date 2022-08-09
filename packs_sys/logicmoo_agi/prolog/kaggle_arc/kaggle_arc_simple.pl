/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


:- export(grid_part/2).
grid_part(Grid,Info):- var(Grid), get_current_test(TestID), ignore(luser_getval(example,ExampleNum)),!,
  kaggle_arc_io(TestID,ExampleNum,_,Grid),
  grid_part(Grid,Info).

%grid_part(Grid,InfoR):- nth1(X,Grid,Info),VInfo=..[v|Info],InfoR=..[row,X,VInfo].
%grid_part(Grid,InfoR):- rot90(Grid,Grid90),nth1(X,Grid90,Info),VInfo=..[v|Info],InfoR=..[col,X,VInfo].
%grid_part(Grid,NObjs):- wno(individuate(complete,Grid,Objs)), maplist_n(1,number_obj,Objs,NObjs).

%cheapest_desc(Grid

number_obj(N,obj(List),obj([ord(N)|List])).
/*
  Obj = obj(List),
  loc(Obj,X,Y),o_i_d(Obj,_,MyID),
 % atomic_list_concat([obj,X,Y],'_',Key),
  localpoints_include_bg(Obj,LocalPoints),
  points_to_grid(X,Y,LocalPoints,Grid),mapgrid(sometimes_assume(=,bg),Grid),
  select(shape(Shape),List,Rest2),mapgrid(sometimes_assume(=,bg),Shape),
  Rest3 = Rest2,
  must_det_ll((remove_too_verbose(MyID,Rest3,TV00))),flatten([TV00],TV0),
  must_det_ll((include(not_too_verbose,TV0,TV1),maplist(fix_iz,TV1,TV))),!,
  member(MrT,[oform(Shape),ogrid(Grid)|TV])*/

%grid_part(Grid,P):- globalpoints(Grid,Points),member(P,Points).

%object_info(obj(List)

%grid(Type,ConstructorData,[rot270]),CacheOfCalls).

%is_graid(T*E*IO,T,E,IO).
is_graid(T*E*IO,G):- kaggle_arc_io(T,E,IO,G).

:- export(is_graid/2).
%grid_aid(ID,T*E*IO):- is_graid(Grid,T,E,IO),format(ID,).

point(Grid,Color,X,Y):- is_graid(Grid,G),nth1(Y,G,R),nth1(X,R,Color).

%g_size(Grid,X,Y):- is_graid(Grid,G),grid_size(G,X,Y).

grid_points(Grid,Points):-  is_graid(Grid,G),globalpoints(G,Points).
grid_point(Grid,point(X,Y,Color)):- point(Grid,Color,X,Y).

grid_object(Grid,mass(1),Point):- grid_point(Grid,Point).
grid_object(Grid,mass(2),point2(Dir,[(HV1)-(HV2)],Color)):- 
  globalpoints(Grid,Ps),select(Color-HV1,Ps,Pss),select(Color-HV2,Pss,_), 
  is_adjacent_point(HV1,Dir,HV2).

grid_object(Grid,mass(N),object(Points,Color)):- 
  is_graid(Grid,G),enum_colors(Color), \+ \+ grid_point(Grid,point(_,_,Color)),
  length(Points,N),Points = [HV1,HV2,HV3|AdjRest],
  globalpoints(G,Ps),select(Color-HV1,Ps,Pss),select(Color-HV2,Pss,Psss),select(Color-HV3,Psss,Rest), 
  HV1 @< HV2,
  is_adjacent_point(HV1,_Dir1,HV2),is_adjacent_point(HV2,_Dir2,HV3),
  ColorHV4 = Color-HV4,
  findall(HV4,(member(ColorHV4,Rest),is_adjacent_point(HV1,_,HV5),is_adjacent_point(HV5,_,HV4)),AdjRest).

:- ensure_loaded('kaggle_arc_fwd.pfc').
:- fixup_exports.

