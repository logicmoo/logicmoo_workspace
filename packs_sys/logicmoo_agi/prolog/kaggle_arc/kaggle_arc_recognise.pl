

/*
ogs(_,_,_,[]):-!,fail.
ogs(H,V,[FRow],Search):-!,
  append(VPad,[SR|Nexr],Search), 
  append(HPad,FRow,PadFrow),
  append(PadFrow,_,PadFrowO),
  PadFrowO = SR,
  length(VPad,V),
  length(HPad,H),!.
 
ogs(H,V,[FRow|FRGrid],Search):-   
 append(VPad,[SR|Rows],Search), 
 append(HPad,FRow,PadFrow),
 append(PadFrow,_,PadFrowO),
 PadFrowO = SR,
 ogs(H2,V2,FRGrid,Rows),
 length(VPad,V1),
 length(HPad,H1),
 V is V1 + V2, H is H1 + H2.
 % grid_size(FGrid,FH,FV),
% grid_size(Search,GH,GV),
*/
/*
*/
grid_indexer_cache(2,2,3,3,
[_,_,
 [_,_,X1,Y1,Z1|_],
 [_,_,X2,Y2,Z2|_],
 [_,_,X3,Y3,Z3|_]|_],
[[X1,Y1,Z1],
 [X2,Y2,Z2],
 [X3,Y3,Z3]]).


grid_indexer_cache(2,3,3,3,
[_,_,_,
 [_,_,X1,Y1,Z1|_],
 [_,_,X2,Y2,Z2|_],
 [_,_,X3,Y3,Z3|_]|_],
[[X1,Y1,Z1],
 [X2,Y2,Z2],
 [X3,Y3,Z3]]).


:- dynamic(grid_indexer_cache/6).

grid_detect_bg(Grid,GridO):- 
  copy_term(Grid,Grid1),
  term_singletons(Grid1,Background),
  maplist(to_grid_bg(Grid1),Background),
  get_bgc(BG),subst(Grid1,BG,bg,GridO),!.


to_grid_bg(_,E):- has_color_c(E),!.
to_grid_bg(_,BG):- bg_sym(BG),!.
to_grid_bg(_,_).

grid_detect_fg(Grid):- 
  term_singletons(Grid,Background),
  term_variables(Grid,Foreground),!,
  include(not_in(Background),Foreground,Foreground1),
  grid_detect_fg(Grid,Foreground1),!.

grid_detect_fg(_,[]):-!.
grid_detect_fg(Grid,Foreground1):- 
  copy_term(Foreground1,ForegroundCopy),
  numbervars(ForegroundCopy,2021,_,[attvar(skip)]),
  maplist(to_grid_fg(Grid),Foreground1,ForegroundCopy),!.

%maybe_grid_numbervars(Grid,Grid):-!.
maybe_grid_numbervars(GridI,Grid):- grid_numbervars(GridI,Grid),!.
maybe_grid_numbervars(Grid,Grid):-!.

not_in(Background,Foreground):-
  \+ (member(E,Background), E == Foreground). 

to_grid_fg(_,E,_):- has_color_c(E),!.
to_grid_fg(_,N,'$VAR'(N)):-!.
to_grid_fg(_,B,B).

grid_numbervars(Grid,GridO):- 
 must_det_ll((grid_detect_bg(Grid,GridO),grid_detect_fg(GridO))).




color_at(H,V,C,Grid):- nth1(V,Grid,Row),nth1(H,Row,C).

has_color_c(Y):- get_attr(Y,dif,_),!.
has_color_c(Y):- get_attr(Y,cc,_),!.
has_color_c(C,E):- attvar(C), get_attr(C,dif,XX),!, sub_term(E,XX),is_color(E).


is_spec_color(C0,C):- nonvar(C0),\+ is_bg(C0), 
  (is_color(C0);atomic(C0)),!,C=C0.

must_det_ll((X,Y)):- must_det_ll(X),!,must_det_ll(Y).
must_det_ll(X):- call(X),!.

%pad_with_contraints_3(GridO,TODO):-
%  grid_size(GridO,HH,VV),
%  pad_with_contraints_3(GridO,HH,VV,TODO),!.
pad_constraints(O,GridO):- is_object(O),!,object_grid(O,Grid),!,pad_constraints(Grid,GridO).
pad_constraints(Grid1,GridO):-
 must_det_ll((
  grid_size(Grid1,H,V),
  globalpoints(Grid1,Ps),
  %writeln(grid_size(H,V)),
  points_range(Ps,LoH,LoV,HiH,HiV,_,_),  
  create_padding(Grid1,LoH,LoV,HiH,HiV,H,V,HH,VV,Grid2),!,
  grid_detect_bg(Grid2,Grid3),!,
  release_bg(Grid3,GridO),
  pad_with_contraints_3(GridO,HH,VV,TODO),
  maplist(with_grid(GridO),TODO))),!.


release_bg(Grid2,GridO):- must_det_ll((release_bg0(Grid2,GridO))),!.
release_bg0(Grid,GridO):- is_list(Grid), !, maplist(release_bg0,Grid,GridO).
release_bg0(Grid,Grid):- attvar(Grid),!.
release_bg0(Grid,Grid):- var(Grid),!.
%release_bg0(Grid-P,GridO-P):- !, release_bg0(Grid,GridO).
release_bg0(BG,_):- is_bg(BG),!.
release_bg0(C0,C):- is_spec_color(C0,C),!.
release_bg0(C,C).

pad_with_contraints_3(GridO,HH,VV,TODOS):-    
  findall(difc(H2,V2,C),
    (color_at(H,V,C0,GridO),
     H\==HH, V\==VV, H\==1, V\==1,
     is_spec_color(C0,C),
     is_adjacent_hv(H,V,Dir,H2,V2),
     \+ is_diag(Dir),
     color_at(H2,V2,C2,GridO),
     \+ is_spec_color(C2,_)), TODO),
  list_to_set(TODO,TODOS),!.

with_grid(Grid,difc(X,Y,C)):- difc(X,Y,C,Grid).
difc(X,Y,C,Grid):- color_at(X,Y,C2,Grid),dif(C,C2).

  

g666(Y):- in_shape_lib(grid,X),
  object_grid(X,G),
  pad_constraints(G,Y).


make_row(Rows,FV):- functor(P,v,FV), P=..[v|Rows].
% make_grid_indexer(H,V,FH,FV,FindRow,Indexer):- grid_indexer_cache(H,V,FH,FV,FindRow,Indexer),!.
make_grid_indexer(H,_V,FH,_FV,_FindRow,_Indexer):- TH is H+FH, TH>30,!.
make_grid_indexer(_H,V,_FH,FV,_FindRow,_Indexer):- TV is V+FV, TV>30,!.
make_grid_indexer(H,V,FH,FV,FindRow,Indexer):-
  length(Above,V),
  make_row(FV,Rows),
  make_rows(FV,H,Rows,FH,FindRow),
  Before = _,
  append(Above,Rows,AR), append(AR,Before,Indexer),!.
  %grid_indexer_cache(H,V,FH,FV,FindRow,Indexer).

make_rows(FV,H,[Row|RO],VH,[FindRow|FRO]):- length(LPad,H),length(FindRow,H),
 append(LPad,FindRow,LPF), append(LPF,_,Row),!, FV1 is FV -1, make_rows(FV1,H,RO,VH,FRO).
make_rows(0,_,[],_,[]):-!.

%make_grid_indexer:- grid_indexer_cache(27,27,27,27,_,_),!.
make_grid_indexer:- 
 tell('gi_cache'),
 writeln(':- module(gi_cache,[igic/6]). \n'),
 forall(between(0,29,H),
  forall(V=0, %between(0,29,V),
   forall(between(1,30,FH),
    forall(between(1,30,FV),
     (make_grid_indexer(H,V,FH,FV,R,L),writeq(igic(H,V,FH,FV,R,L)),writeln('.')))))),
 told.

%:- ensure_loaded(gi_cache).

test_ogs:-
  forall((f666(F),h666(S),ogs(H,V,F,S)),(writeq(ogs(H,V,F,S)),nl)).

% :- make_grid_indexer.

ogs(H,V,Find,Search):- length(Find,FV),Find=[FR|_],length(FR,FH),!,
   between(0,29,V),make_row(Skip,V),append(Skip,Use,Search),igic(H,0,FH,FV,Use,Find).

ogs(H,V,F,S):- %grid_size(S,GH,GV), %visual_hv
  grid_size(F,FH,FV) -> gi_cache:igic(H,V,FH,FV,F,S).

ogs(H,V,[R|FF],[S,S1|SRCH]):- 
  length(S,GW),
  length(R,FW),
  LPMW is GW-FW,
  %length(LPadRow2O,GW),
  between(1,LPMW,H),
 old_obj_grid_scanner(H,V,[R|FF],GW,[S,S1|SRCH]).

old_obj_grid_scanner(H,V,Find,GW,Search):-
  Find = [R1,R2|Grid],
  length(LPad2,H),
  length(LPadRow2O,GW),
  append(LPad2,R2,LPadAndRow2),
  append(LPadAndRow2,_,LPadRow2O),
  nth1(V,Search,LPadRow2O,Rest),  
  length(LPad1,H),
  append(LPad1,R1,LPadAndRow1),
  append(LPadAndRow1,_,LPadRow1O),
  nth0(V,Search,LPadRow1O,_),
  old_obj_grid_scanner2(H,Grid,Rest).

old_obj_grid_scanner2(_,[],_):-!.
old_obj_grid_scanner2(H,[Row|FindRows],[S|Search]):-
  length(LPad2,H),append(LPad2,Row,LPadRow2),
  append(LPadRow2,_,S),!,
  old_obj_grid_scanner2(H,FindRows,Search).

%open_gridRows(Grid,GridO):- notrace(maplist(append,Grid,_,GridO)).
/*

ogs(SM,SR90,FGrid,Search):- fail,  
 grid_rot90(FGrid,FGrid90),!,
 %grid_size(FGrid,FH,FV),
 SRows=[_,_|_],
 append(VPad,SRows,Search),
 length(VPad,V),
 grid_rot90(SRows,SRows90),
 maplist(append,FGrid90,_,FGrid90O),
 SRows90=[S|SRows90OO],
 FGrid90O=[_|RR],
 append(RR,_,RRO),
 %write(S+FF),
 %append(FGrid90O,_,FGrid90OO),
 %append(HPad,FGrid90OO,Rows90),
  length(SRows90,SR90),
  length(SMore,SM),
  SM<SR90,
  RR = RRO,!,
% length(SMore,SM),
 append(SMore,RRO,SRows90OO).
  */

insert_col_row_pad_open(H0,V0,G,GUU):- 
   insert_col_pad_open(H0,G,GU),
   insert_row_pad_open(V0,GU,GUU).

insert_col_pad_open(V0,GU,GUU):-  rot90(GU,GR), insert_row_pad_open(V0,GR,GRU), rot270(GRU,GUU).
insert_row_pad_open(V0,GU,GridU):- functor(P,v,V0),P=..[v|L],append(L,GU,LGU), append(LGU,_,GridU).



h666(
[[4,5,6,7,8,9,4,5,6,7,8,9],
 [4,5,6,7,8,9,4,5,6,7,8,9],
 [4,5,6,7,8,9,4,5,6,7,8,9],
 [6,5,6,6,8,9,4,5,6,7,8,9],
 [6,6,6,7,8,9,4,5,6,7,8,9],
 [6,6,6,6,8,9,4,5,6,7,8,9],
 [6,6,6,6,8,9,4,6,6,6,8,9],
 [6,6,6,6,8,9,4,5,6,7,8,9],
 [6,6,6,7,8,9,4,5,6,7,8,9],
 [4,5,6,7,8,9,4,5,6,7,8,9]]).

h666(
[[1,2,3,4,5,6,7,8,9],
 [1,2,3,4,5,6,7,8,9],
 [1,2,3,4,5,6,7,8,9]]).

h666(
[[4,5,6,7,8,9],
 [4,5,6,7,8,9],
 [4,5,6,7,8,9],
 [4,5,6,6,8,9],
 [4,6,6,7,8,9],
 [4,6,6,6,8,9],
 [4,6,6,7,8,9],
 [4,5,6,7,8,9]]).



h666(
[[1,2,3,4,5,6,7,8,9],
 [1,2,3,4,5,6,7,8,9],
 [1,2,3,4,5,6,7,8,9],
 [1,2,3,4,5,6,7,8,9],
 [1,2,3,4,6,6,6,8,9],
 [1,2,3,4,5,6,7,8,9],
 [1,2,3,4,5,6,7,8,9]]).

f666(
[[_,6,_],
 [6,6,6],
 [_,6,_]]).

f666(
[[_,_,6,_,_],
 [_,_,6,_,_],
 [6,6,6,6,6],
 [_,_,6,_,_],
 [_,_,6,_,_]]).

f666(
[[_,_,X,_,_],
 [_,_,X,_,_],
 [X,X,X,X,X],
 [_,_,X,_,_],
 [_,_,X,_,_]]).


f666(
[[5,_,7],
 [5,6,7],
 [5,6,7]]).

s666(Y):- f666(X), pad_constraints(X,Y).


create_padding(Grid,LowH,LowV,HiH,HiV,H,V,HH,VV,GridO):- 
   fix_v_range(Grid,LowV,HiV,H,V,VV,Grid1),
   rot90(Grid1,Grid2),
   fix_v_range(Grid2,LowH,HiH,VV,H,HH,Grid3),
   rot270(Grid3,GridO).

fix_v_range(Grid,1,HiV,H,V,VV,GridO):-
  make_row(Row,H), 
  fix_v_range([Row|Grid],2,HiV,H,V,V2,GridO), VV is V2+1.

fix_v_range(Grid,LowV,HiV,H,V,VV,GridO):- HiV==V,!, 
  make_row(Row,H),
  append(Grid,[Row],Grid2),
  HiV2 is HiV+1,
  fix_v_range(Grid2,LowV,HiV2,H,V,V2,GridO),
  VV is V2+1.
fix_v_range(Grid,_LowV,_HiV,_H,V,V,Grid).


  


/*
f666(
[[X],
 [X],
 [X],
 [X],
 [X],
 [X]]).

f666(
[[X,_],
 [X,_],
 [X,_],
 [X,X],
 [X,_]]).

f666(
[[X,_],
 [X,X],
 [X,_],
 [X,_]]).
*/

