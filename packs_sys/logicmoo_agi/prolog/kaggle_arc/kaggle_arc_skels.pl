/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

into_ngrid(Points,NGrid):-  vis2D(Points,H,V),into_ngrid(Points,H,V,NGrid).
into_ngrid(Obj,H,V,NGrid):-
  localpoints_include_bg(Obj,Points),
  neighbor_map(H,V,Points,Points,CountedPoints),!,
  points_to_grid(H,V,CountedPoints,NGrid).


guess_bgc(Grid,BGC):- most_d_colors(Grid,[BGC|_],_).

/*
6=corners
9=edges
D=fill_area
*/
:- decl_pt(most_d_colors(grid,color,grid)).
most_d_colors(Grid,ColorO,GridNM):-
  %trace,
 
  get_fill_points(Grid,Points,GridNM),
  uneib(Points,FPoints),
  % grid_size(GridNM,H,V), pp(fillPoints(H,V) = FPoints),
  sort(FPoints,NPSS),
  %trace,
  % (N2-C)-P1
  maplist(arg(1),NPSS,Colors),
  clumped(Colors,CColors),
  maplist(arg(2),CColors,Set),
  get_black(Black),
  (Set==[]->ColorO=[Black];ColorO=Set),!.

ping_indiv_grid(show_neighbor_map).


get_fill_points2(Grid,FillPoints):-
  findall(C-Point,would_fill(Grid,C,Point),FillPoints).

%:- set_prolog_flag(occurs_check,error).

maybe_make_bg_visible(In,Grid):- make_bg_visible(In,Grid),!,In\=@=Grid.

make_bg_visible(In,Grid):- duplicate_term(In,In0),subst001(In0,blue,'#6666FF',M),make_bg_visible_b(M,Grid).
make_bg_visible_b(In,Grid):- is_grid(In),!,mapgrid(make_bg_visible_c,In,Grid).
make_bg_visible_b(In,Grid):- is_list(In),!,maplist(make_bg_visible_b,In,Grid).
make_bg_visible_b(In,Grid):- var(In),!,Grid=In.
make_bg_visible_b(C-P,CC-P):- !, make_bg_visible_c(C,CC).
make_bg_visible_b(In,Grid):- make_bg_visible_c(In,Grid).
make_bg_visible_c(In,'#104010'):- plain_var(In),!.
make_bg_visible_c(In,In):- var(In),!.
make_bg_visible_c(In,Grid):- get_black(Black),subst001(In,Black,'#301030',Grid).




get_fill_points(In,UNFP,GridO):-
 must_det_ll((
 %grid_size(Grid,H,V),
 make_bg_visible(In,Grid),
 %print(In=Grid),
 neighbor_map(Grid,GridO), 
 localpoints(GridO,NPS),  
 %print(NPS),nl,
 include(p1_or(is_point_type('~'),is_fill_point(NPS)),NPS,FillPoints),
 %%include(is_point_type('wbg'),NPS,NotFillPoints),
 include(is_point_type('wbgzzzzz'),NPS,NotFillPoints),
 subtract(FillPoints,NotFillPoints,RFillPoints),
  %my_partition(is_point_type('.'),NFP,OuterEdges,NonFillPointNonOuterEdges),
 uneib(RFillPoints,UNFP))).
/*
  once(get_fill_points2(Grid,FillPoints2)),
  append([FillPoints2,UNFP],TheFilPoints),
  neighbor_map(H,V,UNFP,UNFP,BUNFP),
  append([FillPoints,BUNFP],FGridO),
  points_to_grid(H,V,FGridO,RGridO),!.
*/
recolor_points(G,Color,Ps):- map_pred(is_color_into(Color),G,Ps).
   
is_color_into(Color,B,Color):- is_color(B).
  
is_fill_point(GridO,Type-Color-Point):- is_fill_point(GridO,Type,Color,Point),!.
is_fill_point(GridO,Type,C,P1):- is_adjacent_point(P1,Dir,P2),select('~'-C-P2,GridO,Rest),
  reverse_nav(Dir,Rev),is_adjacent_point(P1,Rev,PRev),member(Type-C2-PRev,Rest),C2\==C.
is_fill_point(_GridO,5,_C,_P1).
is_fill_point(GridO,Type,C,P1):- 
  findall(P2,(is_adjacent_point(P1,_Dir,P2),member(Type-C-P2,GridO)),IdentGuys),
  length(IdentGuys,N),N=4,!.
is_fill_point(GridO,_,C,P1):- would_fill(GridO,C,P1).
 
is_sedge('.').
 
p1_or(P1A,P1B,X):- call(P1A,X)->true;call(P1B,X).
is_point_type(T,V):- ground(V),ground(T), sub_var(T,V).




% (N2-C)-P1
uneib(S,FB):- is_list(S),!,mapgrid(uneib,S,FB).
uneib(S,FB):- \+ compound(S),!,FB=S.
uneib(U-P1,E-P1):- hv_point(_,_,P1),!,uneib(U,E).
uneib(U-Black,Black):- \+ compound(U),!.
uneib(X,X).



show_call_tf(G):- functor(G,F,_),\+ \+ (call(G)->wdmsg(F=true);wdmsg(F=false)).

 %color,neigbours,glyph
test_neighbor_map:- clsmake, forall(rp_test(G),show_neighbor_map(G)).
show_neighbor_map(G):- 
 grid_to_tid(G,ID),
 most_d_colors(G,C,N),!,print_side_by_side(G,N),nl,writeln(ID=C).

merge_nc(A,B,B):- var(A),!.
merge_nc(A,B,A):- var(B),!.
merge_nc(A,B,G-A):- e_int2glyph(B,G),!.

/*
e_int2glyph(7,'X').
e_int2glyph(5,'-').
e_int2glyph(9,'+').
e_int2glyph(6,'c').

 code=63 ?  code=183 � code=176 � code=186 � 170 �
bg_dot(32).
 169	� 248	� 216	�  215 �  174	� 
*//*
e_int2glyph(1,'�').
e_int2glyph(2,'�').
e_int2glyph(3,'�').
e_int2glyph(4,'�').
e_int2glyph(6,'X').
e_int2glyph(7,'�').
e_int2glyph(8,'�').
e_int2glyph(9,'�').
e_int2glyph(10,'~').
e_int2glyph(11,'.').
e_int2glyph(12,',').
%e_int2glyph(5,'�').
%e_int2glyph(5,'�').
%e_int2glyph(5,'�').
%e_int2glyph(,'�').
e_int2glyph(N,G):- N>9, M is N+ 140,int2glyph(M,G).*/
e_int2glyph(B,G):- atom_number(G,B).


was_adjacent(LPS,SOP,_-P1):- is_adjacent_point(P1,_,P2),member(_-P2,SOP),member(CCC-P1,LPS),member(CCC-P2,LPS),!.

cull_rest(Rest,GridS,GridO):- is_list(GridS),!,maplist(cull_rest(Rest),GridS,GridO).
cull_rest(Rest,GridS,0):- number(GridS),member(GridS,Rest),!.
cull_rest(_Rest,GridS,GridS).

at_least_4([A,B,C,D|_],[A,B,C,D]):-!.
at_least_4(Grid4,Grid4).

%edge_of_grid(_,_,_,_):- !, fail.
%edge_of_grid(H,V,_,_):- (H=<15 ; V=<15),!,fail.
edge_of_grid(_,_,_,1,n).
edge_of_grid(_,_,1,_,w).
edge_of_grid(H,_,H,_,e).
edge_of_grid(_,V,_,V,s).
edge_of_grid(_,_,_,_,c).

neighbor_map(Grid,GridO):-
 must_det_ll((
  globalpoints_maybe_bg(Grid,Points),
  grid_size(Grid,H,V),
  neighbor_map(H,V,Points,Points,CountedPoints),!,
  points_to_grid(H,V,CountedPoints,GridO))).

neighbor_map(_,_,[],_,[]):-!.
neighbor_map(H,V,[NC-P1|Ps],Points,[(N-C)-P1|Ps2]):-
  only_color_data(NC,C),  
  nei_map(H,V,C,P1,Points,N),
  neighbor_map(H,V,Ps,Points,Ps2).

only_color_data(C,_):- var(C),!,fail.
only_color_data(C,C):- is_unreal_color(C),!.
only_color_data(C,C):- is_color(C),!.
only_color_data(NC,NC):- \+ compound(NC),!,fail.
only_color_data(OC,C):- sub_term(C,OC),is_colorish(C),!.
only_color_data(C-P,C):- is_nc_point(P),!.
%only_color_data(_-O,C):- only_color_data(O,C).

only_point_data(NC,NC):- \+ compound(NC),!.
only_point_data(_-C,NC):- only_point_data(C,NC).


is_adjacent_point_m2(P1,Dir,P2):- is_adjacent_point(P1,Dir,P2).
is_adjacent_point_m2(P1,Dir,P2):- is_adjacent_point(P1,Dir,P3),is_adjacent_point(P3,Dir,P2).

would_fill(In,C,P1):-
  localpoints_include_bg(In,Points),
  get_black(Black),
  member(C-P1,Points),
  findall(Dir,(n_s_e_w(Dir),is_adjacent_point(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),dif_color(C,CD),CD\==Black),DirsE),
  findall(Dir,(is_diag(Dir),is_adjacent_point(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),dif_color(C,CD),CD\==Black),DirsF),
  findall(Dir,(n_s_e_w(Dir),once((is_adjacent_point(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),==(C,CD),C\==Black))),DirsC),
  findall(Dir,(is_diag(Dir),once((is_adjacent_point(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),==(C,CD),C\==Black))),DirsD),
  once(would_fill_color(DirsC,DirsD,DirsE,DirsF)).

would_fill_color([_,_,_,_],_,_,_).
would_fill_color(_,[_,_,_,_],_,_).
would_fill_color(_,_,[_,_,_,_],_).
would_fill_color(_,_,_,[_,_,_,_]).
would_fill_color(A,_,C,_):- append([A,C],Len),length(Len,L),L>3.
%would_fill_color(A,B,C,D):- append([B,D],Len),length(Len,L),L>3.
would_fill_color(A,B,C,D):- append([A,B,C,D],Len),length(Len,L),L>7.



nei_map(H,V,C,P1,Points,N):- 
 findall(Dir,(n_s_e_w(Dir),once((is_adjacent_point_m2(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),==(C,CD)))),DirsC),
 findall(Dir,(is_diag(Dir),once((is_adjacent_point(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),==(C,CD)))),DirsD),
 findall(Dir,(is_adjacent_point(P1,Dir,P2),member(NC-P2,Points),only_color_data(NC,CD),dif_color(C,CD)),DirsE),
 hv_point(X,Y,P1), 
 edge_of_grid(H,V,X,Y,Edge),
 map_neib1(C,DirsE,Edge,DirsC,DirsD,N).


only_neib_data(NC,NC):- \+ compound(NC),!.
only_neib_data(C-_,NC):- only_neib_data(C,NC).

map_neib1(C,DirsE,Edge,DirsC,DirsD,N):- 
  append([DirsC,DirsD],AllDirs),
  subtract(AllDirs,DirsE,Has),
  subtract([n,s,e,w,ne,sw,se,nw],Has,Not),!,
  map_neibw9(Has,Not,C,DirsE,Edge,DirsC,DirsD,NSM,PS),!,map_ns(NSM,NS), ((fail,PS=='+')-> N=PS; N=NS),!.

%map_ns('j','+').
map_ns(NS,NS).


%

map_neib([w,sw,se],_,'\\').
map_neib([e,sw,se],_,'/').
map_neib([w,ne],_,'/').
map_neib([e,nw],_,'\\').

map_neib([n,s,e,w],_,'#').
map_neib(_,[n,s,e,w],'X').
map_neib([n,e],_,'+').
map_neib([n,w],_,'+').
map_neib([s,e],_,'+').
map_neib([s,w],_,'+').
map_neib([n],_,'|').
map_neib([s],_,'|').
map_neib([e],_,'-').
map_neib([w],_,'-').

map_neib([n,s],_,'|').
map_neib([e,w],_,'-').

map_neib([n,e,ne,nw],_,'\\').
map_neib([n,w,ne,nw],_,'/').

map_neib([s,e,sw,se],_,'/').
map_neib([s,w,sw,se],_,'\\').

map_neib([nw],_,'\\').
map_neib([se],_,'\\').
map_neib([ne],_,'/').
map_neib([sw],_,'/').

map_neib([s,nw],_,'\\').
map_neib([n,se],_,'\\').
map_neib([s,ne],_,'/').
map_neib([n,sw],_,'/').

map_neib([w,nw],_,'-').
map_neib([e,se],_,'-').
map_neib([e,ne],_,'-').
map_neib([w,sw],_,'-').

/*
map_neib([n,nw],_,'\\').
map_neib([s,se],_,'\\').
map_neib([n,ne],_,'/').
map_neib([s,sw],_,'/').
*/

map_neib([sw,se],_,'^').
map_neib([ne,nw],_,'v').
map_neib([sw,nw],_,'>').
map_neib([ne,se],_,'<').
map_neib([s,sw,se],_,'^').
map_neib([n,ne,nw],_,'v').
map_neib([w,sw,nw],_,'>').
map_neib([e,ne,se],_,'<').

map_neib([s,e,se],_,'+').
map_neib([s,w,sw],_,'+').

map_neib([n,e,ne],_,'+').
map_neib([n,w,nw],_,'+').

map_neib([n,s,sw,se],_,'^').
map_neib([e,w,sw,nw],_,'>').
map_neib([e,w,ne,se],_,'<').
map_neib([s,ne,sw],_,'/').
map_neib([s,nw,se],_,'\\').
map_neib([s,ne],_,'/').
map_neib([s,nw],_,'\\').
map_neib(_,[sw,se],'-').
map_neib(_,[s,sw],'-').
map_neib(_,[s,se],'-').

map_neib(_,[n,ne],'-').
map_neib(_,[n,nw],'-').

map_neib(_,[ne],'\\').
map_neib(_,[nw],'/').
map_neib(_,[se],'/').
map_neib(_,[sw],'\\').
map_neib(_,[n],'v').
map_neib(_,[w],'>').
map_neib(_,[e],'<').
map_neib(_,[s],'^').

map_neib(_,[s,sw,se],'-').
map_neib(_,[n,ne,nw],'-').
map_neib(_,[e,ne,se],'|').
map_neib(_,[w,sw,nw],'|').

map_neib(_,[sw,se],'V').
map_neib(_,[ne,nw],'A').
map_neib(_,[ne,se],'>').
map_neib(_,[sw,nw],'<').

map_neib(_,[n,e],'/').
map_neib(_,[n,w],'\\').
map_neib(_,[s,e],'/').
map_neib(_,[s,w],'\\').


%map_neib([e,w|_],_,'-').

map_neib(_,[e,w,ne,se],'|').
map_neib(_,[e,w,sw,nw],'|').
map_neib(_,[n,s,sw,se],'-').
map_neib(_,[n,s,ne,nw],'-').
map_neib(_,[D],'~'):- is_diag(D),!.
map_neib(_,[D],'*'):- \+ is_diag(D),!.

map_neib(_,[],'~').

map_neib([n,sw,se],_,'A').
map_neib(_,[n,s,e,w,nw],'/').
map_neib(_,[n,s,e,w,se],'/').
map_neib(_,[n,s,e,w,sw],'\\').
map_neib(_,[n,s,e,w,ne],'\\').
/*
map_neib(_,[n,s,e,w,_],'x').
map_neib([_,nw],_,'\\').
map_neib([_,se],_,'\\').
map_neib([_,ne],_,'/').
map_neib([_,sw],_,'/').
*/
map_neib(Has,Not,R):-
  map_neib_u(CHas,CNot,R),
  forall(member(C,CHas),member(C,Has)),
  forall(member(C,CNot),member(C,Not)),!.

map_neib_u([ne,sw],[n,s,e,w],'/').
map_neib_u([nw,se],[n,s,e,w],'\\').

map_neib_u([n,s],[e,w],'|').
map_neib_u([e,w],[n,s],'-').
map_neib_u([ne,sw],[],'/').
map_neib_u([nw,se],[],'\\').


map_neib_u([ne,nw],[n],'V').

map_neib_u([n],[e,w],'!').
map_neib_u([s],[e,w],'!').



map_neib_u([nw],[],'\\').
map_neib_u([se],[],'\\').
map_neib_u([ne],[],'/').
map_neib_u([sw],[],'/').

map_neib_u([e],[n,s],'=').
map_neib_u([w],[n,s],'=').

map_neib_u([_],[nw,sw,se,ne],'+').

map_neib_u([n,w],[nw,sw,se,ne],'/').
map_neib_u([s,e],[nw,sw,se,ne],'/').
map_neib_u([n,e],[nw,sw,se,ne],'\\').
map_neib_u([s,w],[nw,sw,se,ne],'\\').


map_neib2(Has,Not,R):-
  map_neib_u2(CHas,CNot,R),
  forall(member(C,CHas),member(C,Has)),
  forall(member(C,CNot),member(C,Not)),!.
  
map_neib_u2([n,s,e,w],[],'~').


map_neibw9(_,_,_,_,_,[],[],0,0):-!.
%map_neibw9(Has,_Not,C,DirsE,_,DirsC,DirsD,'0','0'):-  length(Has,L),L=1.
map_neibw9(Has,Not,_,_,_,_,_,S,S):- length(Has,L),L=<2, map_neib(Has,Not,S).
map_neibw9(Has,Not,_,_,_,_,_,S,S):- length(Has,L),L=5, map_neib(Has,Not,S), member(S,['-','|']).
map_neibw9(Has,Not,_,_,_,_,_,S,S):- length(Has,L),L=4, member(S,['X','<','v','^','>']), map_neib(Has,Not,S).
map_neibw9(Has,Not,_,_,_,_,_,S,S):- length(Has,L),L=3, member(S,['+','<','v','^','>']),map_neib(Has,Not,S).
map_neibw9(Has,Not,_,_,_,_,_,S,S):- length(Has,L),L=3, map_neib(Has,Not,S),!.
map_neibw9(Has,Not,_,_,_,_,_,S,S):- length(Has,L),L=6,  %member(S,['+','-','|']),
    map_neib(Has,Not,S).
map_neibw9(_Has,_Not,_,_,_,[],[_],0,0):-!.
map_neibw9(_Has,_Not,_,_,_,[_],[],0,0):-!.
map_neibw9(Has,Not,_,_,_,_,_,S,S):-map_neib2(Has,Not,S).
map_neibw9(Has,_Not,_,_,_,_,_,'~','~'):-  length(Has,L),L>6.
map_neibw9(Has,Not,C,DirsE,[Edge],DirsC,DirsD,NSM,PS):-!,map_neibw9(Has,Not,C,DirsE,Edge,DirsC,DirsD,NSM,PS).
%map_neibw9(Has,Not,_,_,_,_,_,S,S):-map_neib(Has,Not,S).
%map_neibw9(_EAll,_Missing,_C,_O,_E,[_,_,_],[],'%','%').
%map_neibw9(_,[],_,_,_,_,_,'~','~').
%map_neibw9(_EAll,_Missing,_,_,_,_,[ne,nw],'V','v').
%map_neibw9(Has,_Not,C,DirsE,Edge,DirsC,DirsD,NSM,PS):- map_neibw(Has,_Not,C,DirsE,Edge,DirsC,DirsD,NSM,PS).
%map_neibw9(EAll,_Not,C,[O],E,[],[],NS,PS):- map_neibw(EAll,_Not,C,[],E,[],[O],NS,PS),!.
map_neibw9(Has,Not,_,_,_,_,_,S,S):- length(Has,L),L=4, map_neib(Has,Not,S),!.
map_neibw9(Has,_Not,_C,_,_,_,_,NSM,PS):- !, length(Has,NSM),PS=NSM.
%map_neibw9(Has,_Not,C,DirsE,_,DirsC,DirsD,NSM,PS):- length(Has,N), NSM is N+1,PS=NSM.

map_neibw(_EAll,[n,s,e,w,sw,se,nw],_,_,_,_,_,'/','/').
map_neibw(_EAll,[n,s,e,w,ne,se,nw],_,_,_,_,_,'/','/').
map_neibw(_EAll,[n,s,e,w,ne,sw,nw],_,_,_,_,_,'\\','\\').
map_neibw(_EAll,[n,s,e,w,ne,sw,se],_,_,_,_,_,'\\','\\').
map_neibw(_EAll,[w,ne,sw,se,nw],_,_,_,_,_,'|','.').
map_neibw(_EAll,[e,ne,sw,se,nw],_,_,_,_,_,'|','.').
map_neibw(_EAll,[w,sw,nw],_,_,_,_,_,'|','.').
map_neibw(_EAll,[e,ne,se],_,_,_,_,_,'|','.').

map_neibw(_EAll,[s,w,sw],_,_,_,_,_,'+','.').


map_neibw(_EAll,[n,ne,sw,se,nw],_,_,_,_,_,'-','.').
map_neibw(_EAll,[s,ne,sw,se,nw],_,_,_,_,_,'-','.').
map_neibw(_EAll,[ne,se],_,_,_,_,_,'|','.').
map_neibw(_EAll,[sw,nw],_,_,_,_,_,'|','.').
map_neibw(_EAll,[ne,nw],_,_,_,_,_,'-','.').
map_neibw(_EAll,[sw,se],_,_,_,_,_,'-','.').
map_neibw(_EAll,[_,_,_],_,_,_,_,_,'+','.').

map_neibw(_EAll,_Missing,_,_,_,[n,s,e,w],[_,_,_],'+','7').

map_neibw(_EAll,_Missing,_,_,_,[n,s,e,w],[_,_,_],'*','7').

%map_neibw(_EAll,_Missing,_,[ne,sw,se,nw],[],_,_,'~','~').
%map_neibw(_EAll,_Missing,_,[n,s,e,w],_,_,_,'~','~').


/*
map_neibw(_EAll,_Missing,_,_,c,[n],[],'!','2').
map_neibw(_EAll,_Missing,_,_,c,[s],[],'!','2').
map_neibw(_EAll,_Missing,_,_,c,[e],[],'=','2').
map_neibw(_EAll,_Missing,_,_,c,[w],[],'=','2').

map_neibw(_EAll,_Missing,_,_,_,[n],[],'|','2').
map_neibw(_EAll,_Missing,_,_,_,[s],[],'|','2').
map_neibw(_EAll,_Missing,_,_,_,[e],[],'-','2').
map_neibw(_EAll,_Missing,_,_,_,[w],[],'-','2').

map_neibw(_EAll,_Missing,_,_,_,[n,s],_,'|','.').
map_neibw(_EAll,_Missing,_,_,_,[e,w],_,'-','.').
map_neibw(_EAll,_Missing,_,_,_,[n,e],[],'C','+').
map_neibw(_EAll,_Missing,_,_,_,[n,w],[],'C','+').
map_neibw(_EAll,_Missing,_,_,_,[s,e],[],'C','+').
map_neibw(_EAll,_Missing,_,_,_,[s,w],[],'C','+').

map_neibw(_EAll,_Missing,_,_,_,[_,_,_,_],[_,_,_],'C','+').
map_neibw(_EAll,_Missing,_,_,_,[_,_,_],[],'T','+').
map_neibw(_EAll,_Missing,_,_,_,[_,_,_],[_],'+','+').

map_neibw(_EAll,_Missing,_,_,_,_,NonNil,'+','.'):- NonNil\==[],!.


map_neibw(_EAll,_Missing,_,_,_,[s,e,w],[sw,se],'-','.').
map_neibw(_EAll,_Missing,_,_,_,[n,e,w],[ne,nw],'-','.').
map_neibw(_EAll,_Missing,_,_,_,[n,s,w],[sw,nw],'tz','.').
map_neibw(_EAll,_Missing,_,_,_,[n,s,e],[ne,se],'yz','.').
map_neibw(_EAll,_Missing,_,_,_,[s,e,w],[ne,sw,se,nw],'-','.').
map_neibw(_EAll,_Missing,_,_,_,[n,e,w],[ne,sw,se,nw],'-','.').
map_neibw(_EAll,_Missing,_,_,_,[n,s,w],[ne,sw,se,nw],'|','.').
map_neibw(_EAll,_Missing,_,_,_,[n,s,e],[ne,sw,se,nw],'|','.').

map_neibw(_EAll,_Missing,_,_,_,[n,s,w],_,'|','.').
map_neibw(_EAll,_Missing,_,_,_,[n,s,e],_,'|','.').

map_neibw(_EAll,_Missing,_,_,_,[s,e,w],_,'-','.').
map_neibw(_EAll,_Missing,_,_,_,[n,e,w],_,'-','.').


map_neibw(_EAll,_Missing,_,_,_,[n,s,e,w],[ne,sw,se,nw],'~','~').
% is_diag(ne). is_diag(sw). is_diag(se). is_diag(nw).
%map_neibw(_EAll,_Missing,_,_,_,[],[ne,nw],'V','v').
map_neibw(_EAll,_Missing,_,_,_,[],[se,nw],'\\','v').
map_neibw(_EAll,_Missing,_,_,_,_,[se,nw],'\\','v').

map_neibw(_EAll,_Missing,_,_,_,[],[_,se,nw],'\\','v').
%map_neibw(_EAll,_Missing,_,_,_,_,[_,se,nw],'\\','v').

map_neibw(_EAll,_Missing,_,_,_,[],[ne,sw],'/','r').
map_neibw(_EAll,_Missing,_,_,_,_,[ne,sw],'/','v').

map_neibw(_EAll,_Missing,_,_,_,[],[ne,sw,_],'/','v').
%map_neibw(_EAll,_Missing,_,_,_,_,[ne,sw,_],'/','v').



map_neibw(_EAll,_Missing,_,_,_,[],[ne,se],'<','v'). 
map_neibw(_EAll,_Missing,_,_,_,_,[ne,se],'<','v').
map_neibw(_EAll,_Missing,_,_,_,[],[sw,nw],'>','v').
map_neibw(_EAll,_Missing,_,_,_,_,[sw,nw],'>','v').
map_neibw(_EAll,_Missing,_,_,_,[],[se,sw],'^','v').
map_neibw(_EAll,_Missing,_,_,_,_,[se,sw],'^','v').
map_neibw(_EAll,_Missing,_,_,_,[],[_,_,_],'Y','v').
map_neibw(_EAll,_Missing,_,_,_,_,[_,_,_],'y','v').


map_neibw(_EAll,_Missing,_,_,_,[_,_,_],[_,_,_,_],7,'X').
map_neibw(_EAll,_Missing,_,_,_,[],[_,_,_,_],'X','X').

%map_neibw(_EAll,_Missing,_,_,c,[],[_],'x','v').

map_neibw(_EAll,_Missing,_,_,_,[],[ne],'/','v').
map_neibw(_EAll,_Missing,_,_,_,[],[nw],'\\','v').
map_neibw(_EAll,_Missing,_,_,_,[],[se],'\\','v').
map_neibw(_EAll,_Missing,_,_,_,[],[sw],'/','v').

map_neibw(_EAll,_Missing,_,_,_,[e],[ne,se],'2','X').
map_neibw(_EAll,_Missing,_,_,_,[w],[sw,nw],'2','X').
map_neibw(_EAll,_Missing,_,_,_,[n],[ne,nw],'2','X').
map_neibw(_EAll,_Missing,_,_,_,[s],[se,sw],'2','X').

map_neibw(_EAll,_Missing,_,_,_,[n,e],[ne],'C','+').
map_neibw(_EAll,_Missing,_,_,_,[n,w],[nw],'C','+').
map_neibw(_EAll,_Missing,_,_,_,[s,e],[se],'C','+').
map_neibw(_EAll,_Missing,_,_,_,[s,w],[sw],'C','+').

map_neibw(_EAll,_Missing,_,_,_,[n,e],[sw],'j','j').
map_neibw(_EAll,_Missing,_,_,_,[n,w],[se],'j','j').
map_neibw(_EAll,_Missing,_,_,_,[s,e],[nw],'j','j').
map_neibw(_EAll,_Missing,_,_,_,[s,w],[ne],'j','j').

map_neibw(_EAll,_Missing,_,_,_,[n,e],[sw],'J','j').
map_neibw(_EAll,_Missing,_,_,_,[n,w],[se],'J','j').
map_neibw(_EAll,_Missing,_,_,_,[s,e],[nw],'J','j').
map_neibw(_EAll,_Missing,_,_,_,[s,w],[ne],'J','j').


map_neibw(_EAll,_Missing,_,_,_,[_,_,_,_],[_],5,'.').
map_neibw(_EAll,_Missing,_,_,_,[_],[_,_,_,_],'X','X').
map_neibw(_EAll,_Missing,_,_,_,[_],[_,_,_,_],4,'X').
map_neibw(_EAll,_Missing,_,_,_,[_],[],2,'*').
map_neibw(_EAll,_Missing,_,_,_,[],[_],1,'*').
%map_neibw(_EAll,_Missing,_,_,_,NonNil1,NonNil2,'a','.'):- NonNil1\==[],NonNil2\==[],!.
map_neibw(_EAll,_Missing,_,_,_,_,NonNil,'A','.'):- NonNil\==[],!.
map_neibw(_EAll,_Missing,_,_,_,NonNil,_,'%','.'):- NonNil\==[],!.
map_neibw(_EAll,_Missing,_,_,_,_,_,'.','.').
*/

sometimes_bg(V):- var(V),!,fail.
sometimes_bg(zzzzzblack).

dir_num(_,_,c,0).
dir_num(C,C,Diag,1):- is_diag(Diag),!.
dir_num(_,Black,Diag,0):- sometimes_bg(Black),is_diag(Diag),!.
dir_num(_,_,Diag,0):- is_diag(Diag),!.
dir_num(C,C,_,2).
dir_num(_,Black,_,0):- sometimes_bg(Black).
dir_num(_,_,_,0).

