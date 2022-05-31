test_ogs:-
  forall((f666(F),h666(S),ogs(H,V,F,S)),(writeq(ogs(H,V,F,S)),nl)).

print_cgrid(F):- nop((\+ \+ ((constrain_grid_f(F,FG),print_grid(FG),nl)))),!.

test_ogs(H,V):- clsmake,
  wqln("searching..."),
  ff666(F),print_cgrid(F),
  %copy_term(F,FC), 
  (sp666(G);(fail,ff666(G0),pad_grid(G0,G))), 
   % print_cgrid(G),
  ogs(H,V,F,G),
  once(show_match(H,V,F,G)).

show_match(H,V,F,G):- 
  wqln("Matched"),
  grid_size(G,GH,GV),
  %make_grid(GH,GV,FDisp),
  localpoints(F,Points),
  H2 is H+1,
  V2 is V+1,
  offset_points(H2,V2,Points,GPoints),
  print_grid(GH,GV,GPoints),nl,
  print_grid(G),
  wqln("Found pair").

% 
% s666(X),text_to_grid(X,G),!,in_shape_lib(hammer,H),object_grid(H,OG),duplicate_term(OG,OGC),ogs(FH,FY,OGC,G),OGC=@=OG.

constrain_type(_CheckType,_Grid,G,G):-!.
constrain_type(CheckType,Grid,G,GG):- is_list(G),!,maplist(constrain_type(CheckType,Grid),G,GG).
%constrain_type(CheckType,Grid,G,GG):- is_bgc(G),freeze(CheckType,\+ is_fg_color(GG)),!.
%constrain_type(CheckType,Grid,G,GG):- is_color(G),!,freeze(CheckType,G==GG).
constrain_type(CheckType,_Grid,G,GG):- is_bgc(G), freeze(CheckType,G=GG),!.
constrain_type(_CheckType,_Grid,G,G):- is_fg_color(G),!.
constrain_type(CheckType,_Grid,G,GG):- freeze(CheckType,G=GG).

  
ogs(H,V,OG,SG):-
  constrain_type(CheckType,OG,OG,OGC),!,
  ogs_0(H,V,OGC,SG),
  nop(CheckType=run).

ogs_0(H,V,OG,SG):-
  %constrain_type(CheckType,Grid,OG,OGC),
  constrain_grid_f(OG,CheckType,OGC),
  constrain_grid_s(SG,CheckType,SGC),!,
  ogs_1(H,V,OGC,SGC),
  CheckType=run,
  %print_grid(OGC),nl,
  %wqnl(found_at(H,V)),
  %print_grid(SGC),nl,
  true.

ogs_1(Hi,Vi,Find,Search):-
  nonvar(Hi),nonvar(Vi), 
  H is Hi - 1, V is Vi - 1,
  length(LPad,H),
  length(VPad,V),!,
  append(VPad,[LPadAndRow|Next],Search),
  Find = [R1|FGrid],
  append(R1,_,Rho),
  append(LPad,Rho,LPadAndRow),
  ogs_pt2(H,FGrid,Next).

ogs_1(H,V,FindI,Search):-
  ogs_2(Hi,Vi,FindI,Search),
  H is Hi + 2,
  V is Vi + 2.

ogs_2(H,V,[R1|FGrid],Search):-
  append(VPad,[LPadAndRow|Next],Search),
  append(R1,_,Rho), append(LPad,Rho,LPadAndRow),
  once((length(LPad,H),
        ogs_pt2(H,FGrid,Next),
        length(VPad,V))).

ogs_pt2(_,[],_):-!.
ogs_pt2(H,[Row|FindRows],[S|Search]):-
  length(LPad2,H),append(LPad2,Row,LPadRow2),
  append(LPadRow2,_,S),!,
  ogs_pt2(H,FindRows,Search).

:- discontiguous h666/1. 
:- discontiguous f666/1. 


grid_detect_bg(Grid1,Background):- 
  term_singletons(Grid1,Background).

grid_label_bg(GridIn,GridO):- 
  copy_term(GridIn,Grid1),
  grid_detect_bg(Grid1,Background),
  maplist(to_grid_bg(Grid1),Background),
  get_bgc(BG),subst(Grid1,BG,bg,GridO),!.


to_grid_bg(_,E):- has_color_c(E),!.
to_grid_bg(_,BG):- bg_sym(BG),!.
to_grid_bg(_,_).

grid_detect_fg(GridIn,Foreground1):- 
  grid_detect_bg(GridIn,Background),
  term_variables(GridIn,Foreground),!,
  include(not_in(Background),Foreground,Foreground1).
  

grid_label_fg(GridIn):- 
  grid_detect_fg(GridIn,Foreground1),
  grid_label_fg(GridIn,Foreground1),!.

grid_label_fg(_,[]):-!.
grid_label_fg(GridIn,Foreground1):- 
  copy_term(Foreground1,ForegroundCopy),
  numbervars(ForegroundCopy,2021,_,[attvar(skip)]),
  maplist(to_grid_fg(GridIn),Foreground1,ForegroundCopy),!.

%maybe_grid_numbervars(GridIn,GridIn):-!.
maybe_grid_numbervars(GridI,GridIn):- grid_numbervars(GridI,GridIn),!.
maybe_grid_numbervars(GridIn,GridIn):-!.

not_in(Background,Foreground):-
  \+ (member(E,Background), E == Foreground). 

to_grid_fg(_,E,_):- has_color_c(E),!.
to_grid_fg(_,N,'$VAR'(N)):-!.
to_grid_fg(_,B,B).

grid_numbervars(GridIn,GridO):- 
 must_det_ll((grid_label_bg(GridIn,GridO),grid_label_fg(GridO))).


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
pad_grid(O,GridO):- is_object(O),!,object_grid(O,GridIn),!,pad_grid(GridIn,GridO).
pad_grid(Grid1,Grid2):-
 must_det_ll((
  grid_size(Grid1,H,V),
  globalpoints(Grid1,Ps),
  %writeln(grid_size(H,V)),
  points_range(Ps,LoH,LoV,HiH,HiV,_,_),  
  create_padding(Grid1,LoH,LoV,HiH,HiV,H,V,_HH,_VV,Grid2))),!.

constrain_grid_f(Grid2,Trig,GridO):-
  pad_grid(Grid2,GridP), 
  constrain_grid_now(f,Trig,GridP,GridO),!.
%constrain_grid_f(Grid2,GridO):- Grid2=GridO.

constrain_grid_s(Grid2,Trig,GridO):- constrain_grid(s,Trig,Grid2,GridO).
constrain_grid(CT,Trig,Grid2,GridO):- 
  grid_label_bg(Grid2,Grid3),
  release_bg(CT,Trig,Grid3,GridO),
  constrain_grid_now(CT,Trig,Grid3,GridO),!.
  


release_bg(CT,Trig,Grid2,GridO):- must_det_ll((release_bg0(CT,Trig,Grid2,GridO))),!.
release_bg0(CT,Trig,GridIn,GridO):- is_list(GridIn), !, maplist(release_bg0(CT,Trig),GridIn,GridO).
release_bg0(_CT,_Trig,GridIn,GridIn):- attvar(GridIn),!.
release_bg0(_CT,_Trig,GridIn,GridIn):- var(GridIn),!.
%release_bg0(CT,Trig,GridIn-P,GridO-P):- !, release_bg0(CT,Trig,GridIn,GridO).
release_bg0(_CT,_Trig,BG,C):- is_bg(BG),!,put_attr(C,ci,bg).
release_bg0(_CT,_Trig,C0,C):- is_spec_color(C0,C),!.
release_bg0(_CT,_Trig,C,C).

constrain_grid_now(CT,Trig,GridIn, GridO):-
  grid_size(GridIn,H,V),
  make_grid(H,V, GridO),
  grid_detect_fg(GridIn,FGUnits),
  ignore((FGUnits\==[],
          maplist(will_be_fg(Trig),FGUnits),
          %FGUnits=[fg|_],
          nop(pt(grid_detect_fg(GridIn,FGUnits))))),
  constrain_grid_now(CT,Trig,GridIn,H,V,H,V,GridO),!.

will_be_fg(Trig,FG):- put_attr(FG,ci,fg),
                      freeze(Trig,is_fg_color_if_nonvar(Trig,FG)),
                      freeze(FG,is_fg_color_if_nonvar(Trig,FG)).

is_fg_color_if_nonvar(Trig,V):- var(V),Trig==run,!,fail,freeze(V,is_fg_color_if_nonvar(Trig,V)).
is_fg_color_if_nonvar(Trig,V):- wqnl(is_fg_color_if_nonvar(Trig,V)),fail.
is_fg_color_if_nonvar(_Trig,C):- is_fg_color(C),!.

constrain_grid_now(_CT,_Trig,_GridIn,_Hi,0,_GH,_GV,_GridO):-!.
constrain_grid_now(CT,Trig,GridIn,Hi,Vi,GH,GV,GridO):-
  get_color_at(Hi,Vi,GridIn,C0),
  constrain_ele(CT,Trig,GridIn,Hi,Vi,C0,GridO),
  (Hi==1 -> (H = GH, V is Vi-1) ; (H is Hi -1, V=Vi)),!,
  constrain_grid_now(CT,Trig,GridIn,H,V,GH,GV,GridO).

constrain_ele(CT,Trig,GridIn,H,V,C0,GridO):- is_spec_color(C0,C),!, 
  get_color_at(H,V,GridO,CO),
  freeze(Trig, C==CO),  
  constrain_dir_ele(CT,Trig,[n,s,w,e],GridIn,H,V,C,GridO).
constrain_ele(CT,Trig,GridIn,H,V,C0,GridO):- is_bgc(C0),!, constrain_bg_ele(CT,Trig,GridIn,H,V,C0,GridO).
constrain_ele(CT,Trig,GridIn,H,V,C0,GridO):- bg_sym(C),C==C0,!, constrain_bg_ele(CT,Trig,GridIn,H,V,C0,GridO).
%constrain_ele(CT,Trig,GridIn,H,V,C0,GridO):- constrain_bg_ele(CT,Trig,GridIn,H,V,C0,GridO),!.
constrain_ele(_CT,_Trig,_GridIn,_H,_V,_C0,_GO):- !.


constrain_bg_ele(_CT,_Trig,_GridIn,H,V,_C0,GridO):-
  %color_at(H,V,C1,GridIn),
  get_color_at(H,V,GridO,C2),
  nop(freeze(C2,\+ is_fg_color(C2))),!.
  %freeze(C2,C1==C2),
  %freeze(C1,C1==C2).

is_fg_color(C):- is_bgc(C),!,fail.
is_fg_color(C):- is_color(C),!.
is_fg_color(C):- C == fg.

count_gneighbors(C,H,V,Count,GridIn):- 
  findall(Dir,
    (is_adjacent_hv(H,V,Dir,H2,V2),
     get_color_at(H2,V2,GridIn,C2),C2=@=C), 
    Count).

ci:attr_unify_hook(_AttVal,Value):-
   Value = _.
  
constrain_dir_ele(_CT,_Trig,[],_GridIn,_,_,_,_GridO).
constrain_dir_ele(CT,Trig,[Dir|SEW],GridIn,H,V,C,GridO):-  
  is_adjacent_hv(H,V,Dir,H2,V2),
  %get_color_at(H,V,GridO,CO),
  ignore((% C=CO,
  \+ is_diag(Dir),
  get_color_at(H2,V2,GridIn,C2),
  \+ is_spec_color(C2,_),
  \+ count_gneighbors(C,H2,V2,[_],GridIn),
  get_color_at(H2,V2,GridO,C2O),
  dif(C2O,C))),!,
  constrain_dir_ele(CT,Trig,SEW,GridIn,H,V,C,GridO).
constrain_dir_ele(CT,Trig,[_|SEW],GridIn,H,V,C,GridO):-
  constrain_dir_ele(CT,Trig,SEW,GridIn,H,V,C,GridO).


g666(Y):- in_shape_lib(grid,X),
  object_grid(X,G),
  pad_grid(G,Y).


make_row(Rows,FV):- functor(P,v,FV), P=..[v|Rows].


f666(G):- in_shape_lib(hammer,Ham),object_grid(Ham,G).
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

h666(
"_________________________________________________
         B B B B B               B B B B B
         B B B B B               B B B B B
             B     B B       R R     B     B B
             B     B B       R R     B     B B
             B B B B B       R R R R B B B B B
             B     B B       R R     B     B B
             B     B B       R R     B     B B
         B B B B B               B B B B B
         B B B B B               B B B B B
 _________________________________________________").

h666(
"_________________________________________________
      B B B                           B B B      
        B B                             B B      
        B       B                       B       B
        B B B B B                 R R   B B B B B
        B B   B B                 R R R B B   B B
          B                       R       B      
        B B                             B B      
        B B B                           B B B     
__________________________________________________").
trans_to_color('R','red').
trans_to_color('B','blue').
trans_to_color('G','green').
trans_to_color('O','orange').
trans_to_color('®','blue').
trans_to_color(' ','black').



ascii_to_grid(Text,G):- atom_contains(Text,'____'),!,
  atom_chars(Text,C),
  make_grid(30,30,G0),
  parse_text_to_grid(0,0,C,G0,G).

ascii_to_grid(Text,G):- 
   ascii_to_growthchart(Text,GrowthChart),
   growthchart_to_grid(GrowthChart,6,5,G).

ascii_to_growthchart(Text,GrowthChart):- 
 replace_in_string([ 
   '\r'='\n','\n\n'='\n','! '='!','!\n'='\n','!'=''],Text,Ascii0),
   atomics_to_string(Rows1,'\n',Ascii0),Rows1=[_|Rows],maplist(atom_chars,Rows,GrowthChart),!.


into_grid_color(L,O):- is_list(L),!,maplist(into_grid_color,L,O).
into_grid_color(L,O):- var(L),!,L=O.
into_grid_color(L,O):- color_code(L,O),!.
into_grid_color(O,O).

into_g666(Text,G):- is_grid(Text),!,maplist(into_grid_color,Text,G).
into_g666(Text,G):- atomic(Text),text_to_grid(Text,TG),into_g666(TG,G).

ss666(G):- h666(S),into_g666(S,G).
sp666(Y):- ss666(X), pad_grid(X,Y).

ff666(G):- f666(F),into_g666(F,G).
fp666(Y):- ff666(X), pad_grid(X,Y).

% ?- h666(X),text_to_grid(X,G).
text_to_grid(Text,GO):- text_to_grid(Text,_HH,_VV,_ObjPoints,GO).
text_to_grid(Text,HH,VV,ObjPoints,GO):-
  ascii_to_grid(Text,G),
  %print_grid(G),
  globalpoints(G,GPs),!,
  %print_grid(GPs),
  points_range(GPs,_LoH_,_LoV_,HiH,HiV,_,_),
  LoH=1,LoV=1,
  deoffset_points(LoH,LoV,GPs,ObjPoints),  
  %print_grid(ObjPoints),
  HH is HiH - LoH + 1,
  VV is HiV - LoV + 1,
  points_to_grid(HH,VV,ObjPoints,GO),!.

parse_text_to_grid(H,V,X,G,GO):- append(LEft,['\r','\n'|Right],X), append(LEft,['\n'|Right],Y),!, parse_text_to_grid(H,V,Y,G,GO).
parse_text_to_grid(0,0,X,G,GO):-
  append(_,['_','\n'|Right],X),
  parse_text_to_grid(1,1,Right,G,GO).
parse_text_to_grid(0,0,X,G,GO):-
  append(_,['\n'|Right],X),
  parse_text_to_grid(1,1,Right,G,GO).

parse_text_to_grid(H,V,[' ',C2|X],G,GO):- C2 \== '\n',
  trans_to_color(C2,NewC),
  replace_global_point(H,V,NewC,_,G,GM),
  H2 is H+1,
  parse_text_to_grid(H2,V,X,GM,GO).
parse_text_to_grid(H,V,[' '|X],G,GO):-
  H2 is H+1,
  parse_text_to_grid(H2,V,X,G,GO).
parse_text_to_grid(_,V,['\n'|X],G,GO):-
  V2 is V+1,
  parse_text_to_grid(1,V2,X,G,GO).
parse_text_to_grid(_,_,[],G,G):-!.
parse_text_to_grid(_,_,['_'|_],G,G):-!.
                                                    
%open_gridRows(GridIn,GridO):- notrace(maplist(append,GridIn,_,GridO)).
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
 [6,6,6,6,8,9,6,6,6,6,6,9],
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



create_padding(GridIn,LowH,LowV,HiH,HiV,H,V,HH,VV,GridO):- 
   fix_v_range(GridIn,LowV,HiV,H,V,VV,Grid1),
   rot90(Grid1,Grid2),
   fix_v_range(Grid2,LowH,HiH,VV,H,HH,Grid3),
   rot270(Grid3,GridO).


fix_v_range(GridIn,1,HiV,H,V,VV,GridO):-
  make_row(Row,H), 
  fix_v_range([Row|GridIn],2,HiV,H,V,V2,GridO), VV is V2+1.

fix_v_range(GridIn,LowV,HiV,H,V,VV,GridO):- HiV==V,!, 
  make_row(Row,H),
  append(GridIn,[Row],Grid2),
  HiV2 is HiV+1,
  fix_v_range(Grid2,LowV,HiV2,H,V,V2,GridO),
  VV is V2+1.
fix_v_range(GridIn,_LowV,_HiV,_H,V,V,GridIn).


  

