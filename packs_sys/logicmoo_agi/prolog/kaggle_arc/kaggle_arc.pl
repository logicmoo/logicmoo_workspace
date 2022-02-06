
:- dynamic('$exported_op'/3).
:- multifile('$exported_op'/3).
:- system:ensure_loaded(library(logicmoo_common)).
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


print_trainer(Name):- kaggle_arc(Name,Type,In,Out),print_arc(Name,Type,In,Out).
print_trainer:- kaggle_arc(Name,Type,In,Out),print_arc(Name,Type,In,Out).

print_trainer0:- print_trainer('25d487eb').
print_eval0:- print_trainer('009d5c81').

print_arc(Name,Type,In,Out):- print_arc(Name=in(Type),In),print_arc(Name=out(Type),Out).

% Type is test or train
kaggle_arc(Name,Type,In,Out):- kaggle_arc_train(Name,Type,In,Out).
kaggle_arc(Name,Type,In,Out):- kaggle_arc_eval(Name,Type,In,Out).

/*
% data looks like

kaggle_arc_train('007bbfb7',train,[[0,7,7],[7,7,7],[0,7,7]],[[0,0,0,0,7,7,0,7,7],[0,0,0,7,7,7,7,7,7],[0,0,0,0,7,7,0,7,7],[0,7,7,0,7,7,0,7,7],[7,7,7,7,7,7,7,7,7],[0,7,7,0,7,7,0,7,7],[0,0,0,0,7,7,0,7,7],[0,0,0,7,7,7,7,7,7],[0,0,0,0,7,7,0,7,7]]).
kaggle_arc_train('007bbfb7',train,[[4,0,4],[0,0,0],[0,4,0]],[[4,0,4,0,0,0,4,0,4],[0,0,0,0,0,0,0,0,0],[0,4,0,0,0,0,0,4,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,4,0,4,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,4,0,0,0,0]]).
kaggle_arc_train('007bbfb7',train,[[0,0,0],[0,0,2],[2,0,2]],[[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,2],[0,0,0,0,0,0,2,0,2],[0,0,0,0,0,0,0,0,0],[0,0,2,0,0,0,0,0,2],[2,0,2,0,0,0,2,0,2]]).
kaggle_arc_train('007bbfb7',train,[[6,6,0],[6,0,0],[0,6,6]],[[6,6,0,6,6,0,0,0,0],[6,0,0,6,0,0,0,0,0],[0,6,6,0,6,6,0,0,0],[6,6,0,0,0,0,0,0,0],[6,0,0,0,0,0,0,0,0],[0,6,6,0,0,0,0,0,0],[0,0,0,6,6,0,6,6,0],[0,0,0,6,0,0,6,0,0],[0,0,0,0,6,6,0,6,6]]).
kaggle_arc_train('007bbfb7',train,[[2,2,2],[0,0,0],[0,2,2]],[[2,2,2,2,2,2,2,2,2],[0,0,0,0,0,0,0,0,0],[0,2,2,0,2,2,0,2,2],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,2,2,2,2,2,2],[0,0,0,0,0,0,0,0,0],[0,0,0,0,2,2,0,2,2]]).
kaggle_arc_train('007bbfb7',test,[[7,0,7],[7,0,7],[7,7,0]],[[7,0,7,0,0,0,7,0,7],[7,0,7,0,0,0,7,0,7],[7,7,0,0,0,0,7,7,0],[7,0,7,0,0,0,7,0,7],[7,0,7,0,0,0,7,0,7],[7,7,0,0,0,0,7,7,0],[7,0,7,7,0,7,0,0,0],[7,0,7,7,0,7,0,0,0],[7,7,0,7,7,0,0,0,0]]).

kaggle_arc_train('00d62c1b',train,[[0,0,0,0,0,0],[0,0,3,0,0,0],[0,3,0,3,0,0],[0,0,3,0,3,0],[0,0,0,3,0,0],[0,0,0,0,0,0]],[[0,0,0,0,0,0],[0,0,3,0,0,0],[0,3,4,3,0,0],[0,0,3,4,3,0],[0,0,0,3,0,0],[0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',train,[[0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,0,0,0,0,0],[0,0,0,3,0,3,0,0,0,0],[0,0,3,0,0,0,3,0,0,0],[0,0,0,0,0,3,0,3,0,0],[0,0,0,3,0,3,3,0,0,0],[0,0,3,3,3,0,0,0,0,0],[0,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0]],[[0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,0,0,0,0,0],[0,0,0,3,0,3,0,0,0,0],[0,0,3,0,0,0,3,0,0,0],[0,0,0,0,0,3,4,3,0,0],[0,0,0,3,0,3,3,0,0,0],[0,0,3,3,3,0,0,0,0,0],[0,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',train,[[0,0,0,0,0,3,0,0,0,0],[0,0,0,0,3,0,0,0,0,0],[0,3,3,0,3,3,0,3,0,0],[3,0,0,3,0,0,3,0,3,0],[0,0,0,3,0,0,3,3,0,0],[0,0,0,3,0,0,3,0,0,0],[0,0,0,3,0,0,3,0,0,0],[0,0,0,0,3,3,0,3,0,0],[0,0,0,0,0,0,0,0,3,0],[0,0,0,0,0,0,0,0,0,0]],[[0,0,0,0,0,3,0,0,0,0],[0,0,0,0,3,0,0,0,0,0],[0,3,3,0,3,3,0,3,0,0],[3,0,0,3,4,4,3,4,3,0],[0,0,0,3,4,4,3,3,0,0],[0,0,0,3,4,4,3,0,0,0],[0,0,0,3,4,4,3,0,0,0],[0,0,0,0,3,3,0,3,0,0],[0,0,0,0,0,0,0,0,3,0],[0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',train,[[0,0,0,0,0,0,0,0,0,0],[0,0,3,3,3,3,0,0,0,0],[0,0,3,0,0,3,0,0,0,0],[0,0,3,0,0,3,0,3,0,0],[0,0,3,3,3,3,3,3,3,0],[0,0,0,3,0,0,0,0,3,0],[0,0,0,3,0,0,0,3,3,0],[0,0,0,3,3,0,0,3,0,3],[0,0,0,3,0,3,0,0,3,0],[0,0,0,0,3,0,0,0,0,0]],[[0,0,0,0,0,0,0,0,0,0],[0,0,3,3,3,3,0,0,0,0],[0,0,3,4,4,3,0,0,0,0],[0,0,3,4,4,3,0,3,0,0],[0,0,3,3,3,3,3,3,3,0],[0,0,0,3,0,0,0,0,3,0],[0,0,0,3,0,0,0,3,3,0],[0,0,0,3,3,0,0,3,4,3],[0,0,0,3,4,3,0,0,3,0],[0,0,0,0,3,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',train,[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,3,3,3,3,0,3,3,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,3,0,0,0,0,0,0,0,3,0],[0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0],[0,0,0,0,3,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,3,0,0,0,0],[0,0,3,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,3,3,0,0,0,0,3,0,3,0,0],[0,0,0,0,0,0,3,3,0,0,3,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,0,0,3,3,0,0,3,0,0,3,0,0],[0,0,0,0,0,0,0,3,3,3,3,0,3,0,0,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,3,0,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,3,3,3,3,4,3,3,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,4,3,0,0,0,0,0,0,0,3,0],[0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,4,4,4,4,4,4,3,0,0,0,0],[0,0,0,0,3,0,0,0,3,4,4,4,4,4,4,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,4,4,4,4,4,4,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,4,4,4,4,4,4,3,0,0,0,0],[0,0,3,0,0,0,0,0,3,3,3,3,3,3,3,3,0,0,0,0],[0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,3,3,3,0,0,0,0,3,0,3,0,0],[0,0,0,0,0,0,3,3,4,4,3,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,4,4,3,3,0,0,3,0,0,3,0,0],[0,0,0,0,0,0,0,3,3,3,3,0,3,0,0,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,3,4,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b',test,[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,3,0,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,3,3,3,3,0,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,3,0,0,0,0,3,0,0,3,0,0,0,0,0,0,0],[0,0,0,0,3,3,3,3,3,0,3,3,3,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,3,0,0],[0,0,0,0,0,0,0,0,0,3,3,3,3,3,0,0,0,3,0,0],[0,0,0,0,0,0,0,0,0,3,0,0,0,3,0,0,0,3,0,0],[0,0,0,0,0,0,0,0,3,3,3,3,3,3,0,0,0,3,0,0],[0,0,0,0,0,0,3,3,0,3,0,0,0,3,3,3,3,3,0,0],[0,0,3,0,0,0,0,0,3,3,0,0,0,0,0,0,0,0,0,0],[0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,0,3,3,3,3,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,0,0,0,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,0,0,0,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,3,3,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,3,4,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,3,3,3,3,0,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,3,4,4,4,4,3,4,4,3,0,0,0,0,0,0,0],[0,0,0,0,3,3,3,3,3,0,3,3,3,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,3,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,4,4,4,3,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,3,4,4,4,3,0,0],[0,0,0,0,0,0,0,0,0,3,3,3,3,3,4,4,4,3,0,0],[0,0,0,0,0,0,0,0,0,3,4,4,4,3,4,4,4,3,0,0],[0,0,0,0,0,0,0,0,3,3,3,3,3,3,4,4,4,3,0,0],[0,0,0,0,0,0,3,3,4,3,0,0,0,3,3,3,3,3,0,0],[0,0,3,0,0,0,0,0,3,3,0,0,0,0,0,0,0,0,0,0],[0,3,4,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,3,0,3,0,3,3,3,3,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,4,4,4,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,4,4,4,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,3,3,3,3,3,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
*/
%tell(s),ignore((nl,nl,test_pairs(Name,Type,In,Out),format('~N~q.~n',[test_pairs_cache(Name,Type,In,Out)]),fail)),told.

is_grid([H|L]):- nonvar(L), is_list(H),member(E,H),!,\+ is_list(E).

print_arc(Name,json(JSON)):-!, print_arc(Name,JSON).
print_arc(Name,train=Y):- !, print_arc(Name,Y).
print_arc(Name,X->Y):- !, print_arc(in(Name),X), print_arc(out(Name),Y).
print_arc(Name,X=Y):- !, print_arc(Name=X,Y).
print_arc(Name,[H|L]):- is_grid([H|L]),!,print_grid(Name,[H|L]).
print_arc(Name,[H|L]):- !, maplist(print_arc(Name),[H|L]).
print_arc(Name,Val):- print_tree_nl(Name=Val).

% Grid pretty printing
print_grid(Name,Grid):- wdmsg(Name),print_grid(Grid),assert_grid(Name,Grid),!.

%print_grid(Grid):- is_grid(Grid),!, maplist(print_rows,Grid),nl.
print_grid(Grid):- nl,grid_size(Grid,size(HH,VV)),
  forall(between(1,VV,V),
   (nl,(forall(between(1,HH,H), 
     (hv_value_or(Grid,C,H,V,0),
       print_g(C)))))).
print_rows(List):- maplist(print_g,List),nl.
block_colors([(black),(blue),(red),(green),(yellow),'#c0c0c0',(magenta),'#ff8c00',(cyan),'#8b4513']).
print_g(V):- var(V),!,write(' ?').
print_g(0):- !,write(' .').
print_g(N):- write(' '),print_g1(N).
print_g1(C):- integer(C),block_colors(L),nth0(C,L,Color),ansi_format(fg(Color),'~w',[C]),!.
print_g1(C):- write(' '),write(C).

arc_test_1(Name):- doall(print_trainer(Name)),individuals(Name=out(test),O),!,maplist(print_grid,O).
arc_test_1:- arc_test_1('009d5c81').
arc_test_1:- arc_test_1('25d487eb').

erase_grid(ID):- retractall(cmem(ID,_HV,_C)).
grid_cells(ID,Cells):- findall(-(C,HV),cmem(ID,HV,C),Cells).
assert_grid_cells(ID,Cells):- maplist(assert_cell(ID),Cells).

:- dynamic(grid_sz/3).
% Grid to_fast_workspace
assert_grid(ID,Grid):-
   grid_size(Grid,size(SH,SV)),
   retractall(grid_sz(ID,_,_)),
   assert(grid_sz(ID,SH,SV)),
   erase_grid(ID),   
   forall(between(1,SH,H),
    forall(between(1,SV,V),
     ignore((hv_value(Grid,C,H,V),C\==0,assert_cell(ID,H,V,C))))).

assert_cell(ID,H,V,C):- hv_point(H,V,HV),assert(cmem(ID,HV,C)).
assert_cell(ID,-(C,HV)):- assert(cmem(ID,HV,C)).

% Random Non Blk Eles
first_color(Grid1,C1):- sub_term(C1,Grid1),C1 \= 0,integer(C1).

is_points([_-A|_]):- atom(A).
% Grid size/resize
make_lengths(N,L):- length(L,N).
grid_size(ID,size(H,V)):- grid_sz(ID,H,V),!.
grid_size(Grid,Size):- \+ is_points(Grid),!,grid_size_nd(Grid,Size),!.
grid_size(Grid,size(H,V)):- maplist(arg(2),Grid,Points),sort(Points,Ordered),last(Ordered,Point),!,hv_point(H,V,Point).
grid_size(_,size(30,30)).

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
individuals(ID,Indv):- grid_cells(ID,Cells),
  individuals_list(Cells,Indv).

individuals_list([],[]):-!.
individuals_list(Cells,[Indv|IndvList]):- 
    select(C-HV,Cells,Rest), adjacent_point(HV,HV2),select(C-HV2,Rest,ScanPoints),!,  
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

hv_value_or(Grid,C,H,V,Else):- hv_value(Grid,C,H,V)*->true;C=Else.
hv_value(ID,C,H,V):- cmem(ID,HV,C),hv_point(H,V,HV).
hv_value(Points,C,H,V):- is_list(Points), \+ \+ member(_-_,Points), !,member(C-HV,Points),hv_point(H,V,HV).
hv_value(Grid,C,H,V):- is_grid(Grid),!,nth1(V,Grid,Row),nth1(H,Row,C).
/*b_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),set_nth1(H,Row,C).
nb_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),nb_set_nth1(H,Row,C).
b_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),rplc_nth1(H,Row,OldC,NewC).
nb_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),nb_rplc_nth1(H,Row,OldC,NewC).

*/
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


free_cell(Var):- var(Var),!.
free_cell(0).

copy_cells(B,A,H,HH):- call(B,H),!,call(A,HH).
copy_cells(_,_,H,H):- \+ is_list(H),!.
copy_cells(_,_,[],[]):-!. 
copy_cells(B,A,[H|T],[HH|TT]):-!, copy_cells(B,A,H,HH), copy_cells(B,A,T,TT).
  

same_grid(Grid1,Grid1).

  


