/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


:- expects_dialect(pfc).

grid_part(Grid,Info):- var(Grid), get_current_test(TestID), ignore(nb_current(example,ExampleNum)),!,
  kaggle_arc_io(TestID,ExampleNum,_,Grid),
  grid_part(Grid,Info).

grid_part(Grid,obj(N,P)):- !,trace,individuate(complete,Grid,Objs),nth1(N,Objs,P).
grid_part(Grid,row(Y,Info)):- nth1(Y,Grid,Info).
grid_part(Grid,col(X,Info)):- rot90(Grid,Grid90),nth1(X,Grid90,Info).
grid_part(Grid,P):- globalpoints(Grid,Points),member(P,Points).
%object_info(obj(List)

%grid(Type,ConstructorData,[rot270]),CacheOfCalls).

%is_graid(T*E*IO,T,E,IO).
is_graid(T*E*IO,G):- kaggle_arc_io(T,E,IO,G).
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


:- fixup_exports.

