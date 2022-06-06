/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- discontiguous h666/2. 
:- discontiguous f666/2. 

test_ogs:- forall(test_ogs(_,_),true).


grid_minus_grid(B,A,OI):- grid_size(B,BH,BV),grid_size(A,AH,AV),(BH\==AH;BV\==AV),!,OI=B.
grid_minus_grid(B,A,OI):- remove_global_points(A,B,OI),!.
%grid_minus_grid(B,A,OI):- is_list(B),maplist(grid_minus_grid,B,A,OI).
%grid_minus_grid(B,A,C):- ignore(grid_minus_grid0(B,A,C)).
%grid_minus_grid(B,_,B):- !.
%grid_minus_grid0(B,A,OI):- B==A,!, OI=black.
%grid_minus_grid0(B,A,OI):- nonvar(B),nonvar(A),IO=black.
%grid_minus_grid0(B,A,OI):- nonvar(B),IO=B.
%grid_minus_grid0(B,A,B):- !.
/*

grid_minus_grid(B,A,OI):- 
  pred_global_points(point_minus_point(A,B),A,B,OI).

point_minus_point(A,B,H,V,C,Old,Grid,Grid):- nonvar(Old),nonvar(C),Old\==C,nop(point_minus_point(A,B,H,V,C,Old,Grid,Grid)).
point_minus_point(A,B,H,V,C,Old,Grid,Grid):-  nth1(V,Grid,Row),nb_set_nth1(H,Row,black),!, nop(point_minus_point(A,B,H,V,C,Old,Grid,Grid)).
*/

h666(_,G):- fail,ff666(_,G0),
  flipV(G0,GV),
  flipH(G0,GH),
  append([GH,GV],G1),
  pad_grid(var,G1,G). 

%f666(_Ham,G0):-  clause(f666(_Ham,F),true),into_g666(F,G),all_rotations(G,G0).

test_ogs(H,V):- clsmake,
  wqln("searching..."),
  ss666(T,SG),
  ff666(T,FG),
  copy_term(FG,FGX),
  copy_term(SG,SGX),

  once((constrain_grid_f(FG,CheckType,FGC), constrain_grid_s(SG,CheckType,SGC))),

  ((ogs_1(H,V,FGC,SGC),CheckType=run) *-> 
     (once(show_match(H,V,FGX,SGX)),nop(once(show_match(H,V,FGC,SGC))))
     ; (show_mismatch(FGX,SGX),nop(show_mismatch(FGC,SGC)),fail)).
  


test_ogs0(H,V):- clsmake,
  wqln("searching..."),
  ss666(T,SG),
  ff666(T,FG), 
  (ogs(H,V,FG,SG) *-> once(show_match(H,V,FG,SG)) ; (show_mismatch(FG,SG),fail)).

show_mismatch(F,G):- % fail, 
  nl,dash_char,
  show_m_pair(color_print(red,"Mismatched"),1,1,F,G),
  nl,dash_char,!.

show_match(H,V,F,G):- 
  nl,dash_char,
  show_m_pair(color_print(green,"Matched"),H,V,F,G),
  nl,dash_char,!.

show_m_pair(S,H,V,F,G):- 
  grid_size(G,GH,GV),
  %make_grid(GH,GV,FDisp),
  H2 is H-3, V2 is V-3,
  offset_grid(H2,V2,F,OF),
  constrain_grid_f(OF,_,FF),!,
  print_grid(GH,GV,FF),
  nl,
  dash_char(60,' '),wqs(S),
  print_grid(G),!.

print_fgrid(GH,GV,F):- ((\+ \+ ((constrain_grid_f(F,_Trig,_FG),print_grid(GH,GV,F),nl)))),!.
print_sgrid(F):- ((\+ \+ ((constrain_grid_s(F,_Trig,_FG),print_grid(F),nl)))),!.


constrain_type(_CheckType,_Grid,G,G):-!.
constrain_type(CheckType,Grid,G,GG):- is_list(G),!,maplist(constrain_type(CheckType,Grid),G,GG).
%constrain_type(CheckType,Grid,G,GG):- is_bg_color(G),freeze(CheckType,\+ is_fg_color(GG)),!.
%constrain_type(CheckType,Grid,G,GG):- is_color(G),!,freeze(CheckType,G==GG).
constrain_type(CheckType,_Grid,G,GG):- is_bg_color(G), freeze(CheckType,G=GG),!.
constrain_type(_CheckType,_Grid,G,G):- is_fg_color(G),!.
constrain_type(CheckType,_Grid,G,GG):- freeze(CheckType,G=GG).

  
ogs(H,V,FG,SG):-
  %constrain_type(CheckType,FG,FG,FGC),!,
  ogs_0(CheckType,H,V,FG,SG),
  CheckType=run.

ogs_0(CheckType,H,V,FG,SG):-
  %constrain_type(CheckType,Grid,FG,FGC),
  once((constrain_grid_f(FG,CheckType,FGC),
        constrain_grid_s(SG,CheckType,SGC))),
  ogs_1(H,V,FGC,SGC),
  CheckType=run.
  %CheckType=run,
  %print_grid(FGC),nl,
  %wqnl(found_at(H,V)),
  %print_grid(SGC),nl,
  %true.

ogs_1(Hi,Vi,Find,Search):-
  nonvar(Hi),nonvar(Vi), 
  H is Hi - 1, V is Vi - 1,  
  length(LPad,H),
  length(VPad,V),!,
  append(VPad,[LPadAndRow|Next],Search),
  Find = [R1|FGrid],
  append(R1,_,Rho),
  append(LPad,Rho,LPadAndRow),
  ogs_pt2(H,FGrid,Next),!.

ogs_1(H,V,FindI,Search):-
  grid_size(Search,SH,SV),
  grid_size(FindI,FH,FV),
  MH is SH - FH,MV is SV - FV,
  ogs_2(Hi,Vi,MH,MV,FindI,Search),
  H is Hi + 1,
  V is Vi + 1.

ogs_2(H,V,MH,MV,[R1|FGrid],Search):-  
  append(R1,_,Rho),!,
  append(VPad,[LPadAndRow|Next],Search),
  length(VPad,V),
  %between(0,MV,V),
  (V> MV -> (!, fail) ; true),
  between(0,MH,H),
  once((length(LPad,H),
  append(LPad,Rho,LPadAndRow),
  once((ogs_pt2(H,FGrid,Next),
        length(VPad,V))))).

ogs_pt2(_,[],_):-!.
ogs_pt2(H,[Row|FindRows],[S|Search]):-
  length(LPad2,H),append(LPad2,Row,LPadRow2),
  append(LPadRow2,_,S),!,
  ogs_pt2(H,FindRows,Search),!.


grid_detect_bg(Grid1,Background):- 
  term_singletons(Grid1,Background).

grid_label_bg(GridIn,GridO):- 
  copy_term(GridIn,Grid1),
  grid_detect_bg(Grid1,Background),
  maplist(to_grid_bg(Grid1),Background),
  get_bgc(BG),subst_w_attv(Grid1,BG,bg,GridO),!.


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



must_det_ll((X,Y)):- must_det_ll(X),!,must_det_ll(Y).
must_det_ll(X):- must_det_l(X),!.


offset_grid(H2,V2,FF,OF):-
  offset_v_grid(V2,FF,FF0),
  rot90(FF0,FF1),
  offset_v_grid(H2,FF1,FF2),
  rot270(FF2,OF),!.

offset_v_grid(V2,FF,OF):-  
  grid_size(FF,GW,_), 
  offset_v_grid_row(GW,V2,FF,OF).

offset_v_grid_row(_,V2,FF,FF):- V2<0,!.
offset_v_grid_row(GW,V2,FF,[Row|OF]):- V1 is V2-1,
   length(Row,GW), offset_v_grid_row(GW,V1,FF,OF).
   
  


%pad_with_contraints_3(GridO,TODO):-
%  grid_size(GridO,HH,VV),
%  pad_with_contraints_3(GridO,HH,VV,TODO),!.
pad_grid(Before,After):-  pad_grid(=(bg),Before,After).
pad_grid(P1,O,GridO):- is_object(O),!,object_grid(O,GridIn),!,pad_grid(P1,GridIn,GridO).
pad_grid(P1,Grid,Grid2):-
  grid_size(Grid,H,_), H2 is H +2,
  length(T,H2),maplist(P1,T),
  length(B,H2),maplist(P1,B),
  maplist(pad_sides(P1),Grid,FillRows),
  append([T|FillRows],[B],Grid2).


%constrain_grid_f(Grid2,GridO):- Grid2=GridO.
constrain_grid_f(Obj,Trig,GridO):- \+ is_grid(Obj), object_grid(Obj,Grid2),constrain_grid_f(Grid2,Trig,GridO),!.
constrain_grid_f(Grid2,Trig,GridO):- constrain_grid(f,Trig,Grid2,GridO),!.
constrain_grid_s(Grid2,Trig,GridO):- constrain_grid(s,Trig,Grid2,GridO),!.
constrain_grid(CT,Trig,Grid1,GridO):- 
  pad_grid(Grid1,Grid2),
  grid_label_bg(Grid2,Grid3),
  must_det_l(constrain_grid_now(CT,Trig,Grid3,GridO)),!.
  


release_bg(CT,Trig,Grid2,GridO):- must_det_ll((release_bg0(CT,Trig,Grid2,GridO))),!.
release_bg0(CT,Trig,GridIn,GridO):- is_list(GridIn), !, maplist(release_bg0(CT,Trig),GridIn,GridO).
release_bg0(_CT,_Trig,GridIn,GridIn):- attvar(GridIn),!.
release_bg0(_CT,_Trig,GridIn,GridIn):- plain_var(GridIn),!.
%release_bg0(CT,Trig,GridIn-P,GridO-P):- !, release_bg0(CT,Trig,GridIn,GridO).
release_bg0(_CT,_Trig,BG,C):- is_bg_color(BG),!,put_attr(C,ci,bg).
release_bg0(_CT,_Trig,C1I,C):- is_spec_color(C1I,C),!.
release_bg0(_CT,_Trig,C,C).

%constrain_grid_now(CT,Trig,GridIn, GridO):- GridIn= GridO,!.
constrain_grid_now(CT,Trig,GridIn, GridO):-
  grid_size(GridIn,H,V), make_grid(H,V, GridO),
  maybe_constrain_fg(Trig,GridIn),
  constrain_grid_now(CT,Trig,GridIn,H,V,H,V,GridO),!.

maybe_constrain_fg(_Trig,GridIn):- 
  grid_detect_fg(GridIn,FGUnits),
  ignore((FGUnits\==[],
          set_fg_vars(FGUnits))),!.


set_fg_vars(Vars):-
  copy_term(Vars,CVars), numbervars(CVars,1,_,[attvar(skip),functor_name('fg')]), 
  maplist(set_as_fg,Vars,CVars),!.

set_as_fg(V,fg(N)):- atomic(N), put_attr(V,ci,fg(N)),!,atom_concat(fg,N,Lookup),nb_linkval(Lookup,V).
set_as_fg(V,Sym):- put_attr(V,ci,Sym).

is_fg_color_if_nonvar(Trig,V):- plain_var(V),Trig==run,!,fail,freeze(V,is_fg_color_if_nonvar(Trig,V)).
is_fg_color_if_nonvar(Trig,V):- nop(wqnl(is_fg_color_if_nonvar(Trig,V))),fail.
is_fg_color_if_nonvar(_Trig,C):- is_fg_color(C),!.
is_bg(C):- is_bg_color(C).
is_bgc(C):- is_bg_color(C).

constrain_grid_now(_CT,_Trig,_GridIn,_Hi,0,_GH,_GV,_GridO):-!.
constrain_grid_now(CT,Trig,GridIn,Hi,Vi,GH,GV,GridO):-
  get_color_at(Hi,Vi,GridIn,C1I),
  get_color_at(Hi,Vi,GridO,C1O),
  constrain_ele(CT,GH,GV,Trig,GridIn,Hi,Vi,C1I,C1O,GridO),
  (Hi==1 -> (H = GH, V is Vi-1) ; (H is Hi -1, V=Vi)),!,
  constrain_grid_now(CT,Trig,GridIn,H,V,GH,GV,GridO).

% Out of bounds on Source Canvas
constrain_ele(s,GH,GV,_Trig,_GridIn,H,V,_C1I,_C1O,_GridO):- (H==1;V==1;V==GV;H==GH),!.
% FG Source Canvas
constrain_ele(s,_GH,_GV,_Trig,_GridIn,_H,_V,C1I,C1O,_GridO):- is_spec_color(C1I,_),!, 
  %freeze(C1O, \+ is_bg_color(C1O)), 
  C1I=C1O.
% BG Source Canvas
constrain_ele(s,_GH,_GV,Trig,_GridIn,_H,_V,C1I,C1O,_GridO):- is_bg_color(C1I),!,
  %freeze(C1O, \+ is_fg_color(C1O)),
  freeze(Trig, (\+ is_fg_color(C1O))).

% BG Find On Canvas
constrain_ele(f,_GH,_GV,_Trig,_GridIn,_H,_V,C1I,_C1O,_GridO):- is_bg_color(C1I),!.
% Find FG On Canvas
constrain_ele(f,_GH,_GV,Trig,GridIn,H,V,C1I,C1O,GridO):- is_spec_color(C1I,_),!, 
  (C1O==C1I -> true ; must_det_l((freeze(Trig,C1I=C1O), attach_ci(C1O,C1I)))), 
  constrain_dir_ele(f,Trig,[n,s,e,w],GridIn,H,V,C1I,C1O,GridO).
% UNKOWN
constrain_ele(_CT,_GH,_GV,_Trig,_GridIn,_H,_V,_C1I,_C1O,_GridO).

same_colors(_Trig,C1I,_C1O):- \+ is_spec_color(C1I,_),!.
same_colors(_Trig,C1I,C1O):- nonvar(C1O),!,C1I=C1O.
same_colors(Trig,C1I,C1O):- freeze(C1O,same_colors(Trig,C1I,C1O)).
%same_colors(C1I,C1O):- is_spec_color(C1O,_),!,C1I=C1O.

attach_ci(CO,_C):- nonvar_or_ci(CO),!.
attach_ci(CO,C) :- put_attr(CO,ci,fg(C)).



count_o_neighbors(C,H,V,N,GridIn):- 
  findall(Dir,
    (is_adjacent_hv(H,V,Dir,H2,V2),
     get_color_at(H2,V2,GridIn,C1O),
       is_spec_color(C1O,_),C1O\=@=C), 
    Count),
  length(Count,N).

count_c_neighbors(C,H,V,N,GridIn):- 
  findall(Dir,
    (is_adjacent_hv(H,V,Dir,H2,V2),
     get_color_at(H2,V2,GridIn,C1O),
       C1O=@=C), 
    Count),
  length(Count,N).
  
%ci:attr_unify_hook(fg(_),Value):- !, is_fg_color(Value).
%ci:attr_unify_hook(bg,Value):- !, is_bg_color(Value).
ci:attr_unify_hook(_,_Value).
  
%constrain_dir_ele(CT,Trig,[_|SEW],GridIn,H,V,GridO):- constrain_dir_ele(CT,Trig,SEW,GridIn,H,V,GridO).

mfreeze(Trig,_CDE):- nonvar(Trig),!.
mfreeze(Trig,CDE):- freeze(Trig,CDE).

constrain_dir_ele(_CT,_Trig,[],_GridIn,_H,_V,_C1I,_C1O,_GridO).
constrain_dir_ele(CT, Trig,[Dir|SEW],GridIn,H,V,C1I,C1O,GridO):-
  ignore((
  is_adjacent_hv(H,V,Dir,H2,V2),
  get_color_at(H2,V2,GridIn,C2I),
  get_color_at(H2,V2,GridO,C2O),
     \+ is_spec_color(C2I,_),
     count_c_neighbors(C1I,H2,V2,N,GridIn),
     count_o_neighbors(C1I,H2,V2,N2,GridIn),
     o_c_n(CT,N2,N),     
     dif(C2O,C1O))),!,
  constrain_dir_ele(CT, Trig,SEW,GridIn,H,V,C1I,C1O,GridO).


o_c_n(f,Tw,_):- Tw>=2,!. %has at least two neighbours we care about
o_c_n(f,0,1):-!,fail. % no neighbours and just self
o_c_n(f,_,_).
o_c_n(s,_,_).

g666(_,Y):- in_shape_lib(l_shape,X),
  once((object_grid(X,G),pad_grid(G,Y))).


make_row(Rows,FV):- functor(P,v,FV), P=..[v|Rows].


f666(_Color,
[[4,5,1],
 [4,5,1],
 [4,5,1]]).

f666(_Ham,G):- 
S="
 _________
 B B B B B 
 B B B B B 
     B    
     B    
 _________",
 into_g666(S,G1),Color=_,once(subst(G1,blue,Color,G)).

f666(_Ham,G):-
S="
 _________
 B B B B B 
 B B B B B 
     B    
     B    
 _________",
 into_g666(S,G1),Color=blue,once(subst(G1,blue,Color,G)).



f666(_Ham,G):- in_shape_lib(hammer,Ham),object_grid(Ham,G0),all_rotations(G0,G).

h666(_Ham,
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

h666(_Ham,
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
into_grid_color(L,O):- plain_var(L),!,L=O.
into_grid_color(L,O):- color_code(L,O),!.
into_grid_color(O,O).

into_g666(Text,G):- is_grid(Text),!,maplist(into_grid_color,Text,G),!.
into_g666(Text,G):- atomic(Text),text_to_grid(Text,TG),into_g666(TG,G),!.

ss666(T,G):- h666(T,S),into_g666(S,G).
sp666(T,Y):- ss666(T,X), pad_grid(X,Y).

ff666(T,G):- f666(T,F),into_g666(F,G).
fp666(T,Y):- ff666(T,X), pad_grid(X,Y).

% ?- h666(X),text_to_grid(X,G).
text_to_grid(Text,GO):- text_to_grid(Text,_HH,_VV,_ObjPoints,GO).
text_to_grid(Text,HH,VV,ObjPoints,GO):-
  ascii_to_grid(Text,G),
  % grid_to_id(G,ID),
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

parse_text_to_grid(H,V,[' ',C1O|X],G,GO):- C1O \== '\n',
  trans_to_color(C1O,NewC),
  replace_global_point(H,V,NewC,_,G,GM),
  H2 is H+1,
  parse_text_to_grid(H2,V,X,GM,GO).
parse_text_to_grid(H,V,[' '|X],G,GO):-
  H2 is H+1,
  parse_text_to_grid(H2,V,X,G,GO).
parse_text_to_grid(H,V,[C1O|X],G,GO):-
  trans_to_color(C1O,NewC),
  replace_global_point(H,V,NewC,_,G,GM),
  H2 is H+1,
  parse_text_to_grid(H2,V,X,GM,GO).
parse_text_to_grid(_,V,['\n'|X],G,GO):-
  V2 is V+1,
  parse_text_to_grid(1,V2,X,G,GO).
parse_text_to_grid(_,_,[],G,G):-!.
parse_text_to_grid(_,_,['_'|_],G,G):-!.

insert_col_row_pad_open(H0,V0,G,GUU):- 
   insert_col_pad_open(H0,G,GU),
   insert_row_pad_open(V0,GU,GUU).

insert_col_pad_open(V0,GU,GUU):-  rot90(GU,GR), insert_row_pad_open(V0,GR,GRU), rot270(GRU,GUU).
insert_row_pad_open(V0,GU,GridU):- functor(P,v,V0),P=..[v|L],append(L,GU,LGU), append(LGU,_,GridU).



h666(colors,
[[4,5,1,7,8,9,4,5,2,7,8,9],
 [4,5,1,7,8,9,4,5,1,7,8,9],
 [4,5,1,7,8,9,4,5,1,7,8,9],
 [1,5,1,1,8,9,4,5,1,7,8,9],
 [1,1,1,7,8,9,4,5,1,7,8,9],
 [1,1,1,1,8,9,4,5,1,7,8,9],
 [1,1,1,1,8,9,1,1,1,1,1,9],
 [1,1,1,1,8,9,4,5,1,7,8,9],
 [1,1,1,7,8,9,4,5,1,7,8,9],
 [4,5,1,7,8,9,4,5,1,7,8,9]]).

h666(colors,
[[1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9]]).

h666(colors,
[[4,5,1,7,8,9],
 [4,5,1,7,8,9],
 [4,5,1,7,8,9],
 [4,5,1,1,8,9],
 [4,1,1,7,8,9],
 [4,1,1,1,8,9],
 [4,1,1,7,8,9],
 [4,5,1,7,8,9]]).



h666(colors,
[[1,2,3,4,5,2,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,1,1,1,8,9],
 [1,2,3,4,5,1,7,8,9],
 [1,2,3,4,5,2,7,8,9]]).

f666(_,
[[_,1,_],
 [1,1,1],
 [_,1,_]]).

f666(_,
[[_,_,1,_,_],
 [_,_,1,_,_],
 [1,1,1,1,1],
 [_,_,1,_,_],
 [_,_,1,_,_]]).

f666(_,
[[_,_,X,_,_],
 [_,_,X,_,_],
 [X,X,X,X,X],
 [_,_,X,_,_],
 [_,_,X,_,_]]).


f666(colors,
[[5,_,7],
 [5,1,7],
 [5,1,7]]).

f666(T,
[[X],
 [X],
 [X],
 [X],
 [X],
 [X]]):- T =  _.

f666(T,
[[X,_],
 [X,_],
 [X,_],
 [X,X],
 [X,_]]):- T =  _.

f666(T,
[[X,_],
 [X,X],
 [X,_],
 [X,_]]):- T =  _.




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


  

