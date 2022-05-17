


:- dynamic(is_gridname/2).

set_gridname(Grid,Name):- nb_setval(grid_name,Name),asserta_new(is_gridname(Grid,Name)).

get_gridname(Grid,Name):- is_gridname(Grid,Name)*->true; nb_getval(grid_name,Name).

% Grid pretty printing
grid_info(Name,IO,Grid):- 
  GridName = (Name->IO),
  test_info(Name,InfoF),
  wqnl(fav(GridName,InfoF)),
  set_gridname(Grid,GridName),
  describe_feature(Grid,[grid_dim,colors_count_size,colors_count,num_objects]),

  individuals(Grid,IndvS),
  append(IndvS,[Grid],Print),
  colors_count_size(Grid,CCS),
  grid_size(Grid,H,V),
  ignore((sub_var(in,IO),(CCS>4;debug_indiv;true), debug_indiv(IndvS))),
  print_grid(1,1,1,1,H,V,H,V,Print),
  ignore((sub_var(out,IO),(CCS>4;debug_indiv;true), debug_indiv(IndvS))),
  nop(describe_feature(Grid,[individuals])),!.

maybe_confirm_sol(Name,Type,In,Out):- 
   ignore((sols_for(Name,Sol), confirm_sol(Sol,Name,Type,In,Out))),!.

sols_for(Name,Sol):- test_info(Name,Sols),member(lmDSL(Sol),Sols).

confirm_sol(Prog,Name,Type,In,Out):- 
   (run_dsl(Prog,In,Grid),into_grid(Grid,GridO))
   *->    
   count_difs(Out,GridO,Errors),
   (Errors==0 -> arcdbg(pass(Name,Type,Prog));(arcdbg(fail(Errors,Name,Type,Prog)),
     test_info(Name,InfoF),
     wqnl(fav(Name->Type,InfoF)),
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

compute_diff(I,O,[]):- (var(I);var(O)),!.
compute_diff(obj(I),obj(O),OUT):- !,
  compute_diff(I,O,Diffs),
    (Diffs == [] ->  OUT = same_object(obj(I)) ; OUT = props_diff(obj(I),obj(O),Diffs)).

compute_diff(I,O,DD):- is_objectlist(I),is_objectlist(O),!,
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


/*
print_points_grid(Points):- 
  points_range(Points,LoH,LoV,HiH,HiV,H,V),
  wqnl(offset_ranges(LoH,LoV,HiH,HiV,H,V)),
  points_to_grid(Points,Grid),
  print_grid(Grid).
print_points_grid(Grid):-  
  points_range(Grid,LoH,LoV,HiH,HiV,_H,_V),
  print_grid(Grid,LoH,LoV,HiH,HiV,Grid).
*/



%object_size(Points,H,V):- is_dict(Points),!,Points.object_size=object_size(H,V).



combine_grids(_,[G],G):-!.
combine_grids(How,[G1,G2|Gs],GO):- combine_grids(How,[G2|Gs],G1,GO).

combine_grids(_How,[],G,G):-!.
combine_grids(How,[H|T],G,GO):- !,
  %in_cmt((writeln(How),print_grid(H))),
  combine_grids(How,H,G,GM),
  combine_grids(How,T,GM,GO).
combine_grids(overlay,H,G,GM):- globalpoints(H,Points),set_point(Points,G,GM),!.
combine_grids(append,H,G,GM):- grid_size(H,W,_),length(Row,W), append(G,[Row|H],GM).
  


