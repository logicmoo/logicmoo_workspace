

showdiff(A,B):- is_group(A), is_group(B), showdiff_groups(A,B),!.
showdiff(A,B):- is_object(A), is_object(B), showdiff_objects(A,B),!.
showdiff(A,B):- must(diff_terms(A,B,D)) -> D\==[],!,writeln('SOME DIFFERNCE'),ptt(D).
showdiff(_,_):- writeln('NO DIFFERNCE').


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

get_selector(PI,PO):- get_selector2(PI,PO).
/*
get_selector(PI,PO):- get_selector1(PI,PO).
get_selector(PI,PO):- get_selector2(PI,PO), \+ (get_selector1(PII,POO), PI=@=PII,PO=@=POO).
*/
get_selector(_,_).
get_selector2(obj(PI),obj(PO)):- 
  get_selector_n2(PIN),do_nth(PIN,PI,PO).


get_selector_n2([N1,N2,N3,N4]):- top(Top),
  between(1,Top,N1), between(N1,Top,N2),N2>N1, between(N2,Top,N3),N3>N2,between(N3,Top,N4),N4>N3.
get_selector_n2([N1,N2,N3]):- top(Top),
  between(1,Top,N1), between(N1,Top,N2),N2>N1, between(N2,Top,N3),N3>N2.
get_selector_n2([N1,N2]):- top(Top),
  between(1,Top,N1), between(N1,Top,N2),N2>N1.
get_selector_n2([N1]):- top(Top), between(4,Top,N1).

% generalize(CI,CO):- freeze(CO,(CI),functor(CI,F,A),functor(CO,F,A).
%generalize(CI,CO):- callable(CI),functor(CI,F,A),functor(CO,F,A).

generalize_atomic(I,I).
generalize_atomic(I,O):- number(I),!, freeze(O, number(O)).
generalize_atomic(I,O):- atom(I),!, freeze(O, atom(O)).
generalize_atomic(I,O):- string(I),!, freeze(O, string(O)).

generalize_term(I,O):- \+ compound(I),!, I = O.
generalize_term(I,O):- generalize(I,O).

generalize_once(I,O):- var(O), !, freeze(O,once(generalize(I,O))).
generalize_once(I,O):- generalize(I,O).
%generalize(I,O):- var(I), !, freeze(I, generalize(I,O)).
generalize(I,O):- var(I), !, (O=I ; O = _).
generalize(I,O):- ( \+ compound(I) ),!, generalize_atomic(I, O).
generalize([A|B],[AA|BB]):- !, generalize(A,AA),generalize_cons(B,BB).
generalize(_-P,_-P):-!.
generalize(I,O):- compound(I),
  compound_name_arguments(I,F,IA),
  generalize_list(IA,OA),
  compound_name_arguments(O,F,OA).
generalize(_,_).

generalize_list([],[]). 
generalize_list([I],[O]):- !, generalize(I,O).
generalize_list([A,B],[AA,BB]):- !, generalize(A,AA),generalize(B,BB).
generalize_list([A,B,C],[AA,BB,CC]):- !, generalize(A,AA), generalize(B,BB),generalize(C,CC).
generalize_list([A,B,C,D],[AA,BB,CC,DD]):- !, generalize(A,AA), generalize(B,BB),generalize(C,CC),generalize(D,DD).
generalize_list([A|B],[AA|BB]):- generalize(A,AA),generalize_list(B,BB).

generalize_cons(I,O):- var(I), !, (O=[];O=_).
generalize_cons([],O):- !, (O=[];O=_).
generalize_cons([A|B],[AA|BB]):- !, generalize(A,AA),generalize_cons(B,BB).

/*
make_list_perms2(0,IA,IO):- !, duplicate_term(IA,IO),IA=IO.
make_list_perms2(L,IA,IO):- duplicate_term(IA,IO),IA=IO,between(1,L,N),once(nb_set_nth1(N,IO,_)).
make_list_perms2(L,IA,IO):- duplicate_term(IA,IO),IA=IO, L2 is L - 1,make_list_perms2(L2,IA,IO).
[A,B,C,D],[A,B,C,D]
[A,B,C,D],[A,B,_,D]
[A,B,C,D],[A,_,C,D]
[A,B,C,D],[_,B,C,D]
[A,B,C,D],[A,B,_,_]
[A,B,C,D],[A,_,C,_]
[A,B,C,D],[A,_,_,D]
[A,B,C,D],[_,B,_,D]
[A,B,C,D],[_,_,C,D]
[A,B,C,D],[_,B,C,_]
[A,B,C,D],[A,_,_,_]
[A,B,C,D],[_,B,_,_]
[A,B,C,D],[_,_,C,_]
[A,B,C,D],[_,_,_,D]


*/
combine_diffs([],D,D):-!.
combine_diffs(D,[],D):-!.
combine_diffs(D,D,D):-!.
combine_diffs(D1,D2,L12):- listify(D1,L1),listify(D2,L2),!,append(L1,L2,L12).



  

showdiff_objects(O1,O2):-  showdiff_objects(sameness,O1,O2).
showdiff_objects(N,O1,O2):-  
  diff_objects(O1,O2,Diffs),
  print_list_of(diffs,Diffs),
  print_list_of(N,[O1,O2]),
  dash_char.

print_list_of(_,[]):-!.
print_list_of(N,O):-
 (N\=[] -> pt(N); true),
  maplist(print_info,O).

print_info(A):- is_grid(A),print_grid(A).
print_info(A):- is_object(A),debug_indiv(A).
print_info(A):- is_group(A),debug_indiv(A).
print_info([]):-!.
print_info(A):- pt(A).

showdiff_groups(I0,O0):-
  maplist(obj_grp_comparable,I0,I1),
  maplist(obj_grp_comparable,O0,O1),
  showdiff_groups(I1,O1,compare_objs1(perfect),I2,O2),
  showdiff_groups(I2,O2,compare_objs1(moved),I3,O3),
  showdiff_groups(I3,O3,compare_objs1(same_shape),I4,O4),
  (I0==I4 -> true ; print_list_of(groupA,I4), dash_char),
  (O0==O4 -> true ; print_list_of(groupB,O4), dash_char),
  !.
showdiff_groups(I,O,Pred,IIR,OOR):- 
  pred_intersection(Pred,I,O,IntersectA,IntersectB,IIR,OOR),
  ignore((IntersectA\==[], maplist(showdiff_objects(Pred),IntersectA,IntersectB))).


diff_groups(I0,O0,DD):- 
  maplist(obj_grp_comparable,I0,I),
  maplist(obj_grp_comparable,O0,O),
  pred_intersection(overlap_same_obj_no_diff,I,O,_IntersectA,_IntersectB,IIR,OOR),
  diff_groups0(IIR,OOR,DD).

diff_groups0([],[],[]):-!.
diff_groups0([],O,left_extra(O)):-!.
diff_groups0(O,[],left_over(O)):-!.
diff_groups0(IIR,OOR,DD):-
  %make_comparable(O0,O),
  /*
  ,*/
  %pred_intersection(overlap_same_obj,I,O,Intersect,IIR,OOR),
  %Intersect == [],
  select(PI,IIR,II),
  get_selector(PI,PO),
  select(PO,OOR,OO),
  diff_objects(PI,PO,D),
  diff_groups0(II,OO,D1),
  combine_diffs(D1, D , DD).

diff_groups0(IIR,OOR,DD):-
  %make_comparable(O0,O),
  /*
  get_selector(PI,PO),*/
  %pred_intersection(overlap_same_obj,I,O,Intersect,IIR,OOR),
  %Intersect == [],
  select(PI,IIR,II),
  select(PO,OOR,OO),
  same_colorless_points(PI,PO,D1),
  diff_groups0(II,OO,D),
  combine_diffs(D1,D , DD).

obj_grp_comparable(I,obj(O)):- obj_make_comparable(I,M),
  exclude(uncomparable(group),M,O).

include_fav_points(I,II):- include(fav_points,I,II),II=[_,_|_],!.
include_fav_points(I,I).

fav_points(I):- \+ dislike_points(I).

dislike_points(obj(I)):-!,dislike_points(I).
dislike_points(I):- is_list(I),dislike_points1(L),forall(member(E,L),member(E,I)).

%dislike_points1([object_shape(dot),grid_size(H,V)]):- freeze(H, freeze(V, (HV is H * V, HV > 49))).
dislike_points1([colors_count([color_count(BG, _)]),object_shape(nonsolid)]):- freeze(BG,is_black_or_bg(BG)).

usefull_compare(P):- compound(P),functor(P,F,_),!,usefull_compare(F).
usefull_compare(P):- changed_by(P,_).

changed_by(localpoints_nc,rotate).
changed_by(object_offset,move).
changed_by(point_count,grow).
changed_by(colors_count,repaint).
changed_by(object_size,copy).

uncomparable(term,grid).
uncomparable(term,globalpoints).

uncomparable(object,object_shape).
uncomparable(shape,localpoints).
uncomparable(group,grid_size).
uncomparable(group,object_indv_id).
uncomparable(P):- compound(P),functor(P,F,_),uncomparable(F).

make_comparable(I,I):- var(I).
make_comparable(I,I):- \+ compound(I),!.
make_comparable(I,II):- is_list(I),!,maplist(make_comparable,I,II).
make_comparable(obj(L),obj(LL)):- !,make_comparable(L,LL).
make_comparable(L,L):- usefull_compare(L),!.
make_comparable(I,II):- functor(I,II,_).


no_diff(in,out).
simular([],_,_,[]):- !.
simular(object_offset=Where,I,O,object_has_moved(Where)):-  
  \+ (point_count(O,OC), OC < 6) ,
  \+ (colors_count(O,[color_count(BG, _)|_]),is_black_or_bg(BG)),
  object_indv_id(I,Tst,_Id1), \+ object_indv_id(O,Tst,_Id2).



maye_sort(L,S):- is_list(L),\+ is_grid(L), !,sort(L,S).
maye_sort(S,S).
obj_make_comparable(obj(I),O):- maplist(obj_make_comparable_e,I,O).
obj_make_comparable(I,O):- is_list(I),maplist(obj_make_comparable_e,I,O).
obj_make_comparable_e(I,O):- is_list(I),sort(I,O).
obj_make_comparable_e(I,O):- I=..[F,L],maye_sort(L,S),O=..[F,S].
obj_make_comparable_e(I,I).
%obj_make_comparable(I,SI):- exclude(uncomparable,I,II), sort(II,SI).

%diff_objects(I,O,OUT):- !, fail, locally(set_prolog_lfag(gc,false),compute_diff_objs1(I,O,OUT)).
:- style_check(-singleton).

diff_objects(I,O,Diffs):-  
  obj_make_comparable(I,II), obj_make_comparable(O,OO),!,
  intersection(II,OO,Intersect,IIR,OOR),!,
  findall(Diff, 
    (member(P,IIR),
      generalize(P,Q),member(Q,OOR),
      diff_terms(P,Q,Diff)),Diffs).

  %(simular(Diffs,obj(I),obj(O),Sameness) ->  OUT = same_object(Sameness) ; OUT = props_diff(obj(I),obj(O),Diffs)).

same_colorless_points(I,O,OUT):-  
  obj_make_comparable(I,II), obj_make_comparable(O,OO),!,
  intersection(II,OO,SL,IIR,OOR),!,
  %member(point_count(_),SL),
  member(localpoints_nc(_),SL),
  diff_objects(I,O,OUT).

combine_perfects(IndvS,[IO|IndvSO]):- 
  select(I,IndvS,IndvS1),select(O,IndvS1,IndvS2),
  compare_objs1(perfect,I,O),
  override_list(I,O,IO),
  combine_perfects(IndvS2,IndvSO).
combine_perfects(IndvSO,IndvSO).


combine_objects(IndvS,[obj(IO)|IndvSO]):- 
  select(obj(I),IndvS,IndvS1),select(obj(O),IndvS1,IndvS2),
  compare_objs2(I,O,perfect),
  override_list(I,O,IO),
  combine_objects(IndvS2,IndvSO).
combine_objects(IndvSO,IndvSO).


overlap_same_obj_no_diff(I,O):- diff_objects(I,O,Diff),Diff==[]. 
overlap_same_obj(I,O):- compare_objs1(I,O,Diff),Diff==[]. 

compare_objs1(How,obj(I),obj(O)):- !, compare_objs2(How,I,O).
compare_objs1(How,I,O):- compare_objs2(How,I,O).
compare_objs2(How,I,O):- intersection(I,O,SL,UI,UO), !, compare_objs1(How,I,O,SL,UI,UO).

compare_objs1(perfect,I,O,SL,UI,UO):-
  e1_member(point_count(_),SL),
  e1_member(localpoints_nc(_),SL), 
  e1_member(globalpoints(_),SL),
  e1_member(object_offset(_,_),SL), 
  e1_member(object_size(_),SL),
  e1_member(localpoints(_),SL),
  e1_member(color_counts(_),SL),
  trace,
  !.

compare_objs1(moved,I,O,SL,UI,UO):-
  e1_member(point_count(_),SL),
  e1_member(localpoints_nc(_),SL),
  e1_member(object_size(_),SL),
  e1_member(localpoints(_),SL),
  e1_member(color_counts(_),SL).

compare_objs1(same_shape,I,O,SL,UI,UO):- e1_member(localpoints_nc(_),SL).

e1_member(E,L):- \+ \+ member(E,L).

:- style_check(+singleton).
%compute_diff_objs2(I,IIR,O,OOR,[]):-  diff_terms(IIR,OOR,[]),


  %pt(remove_sames(II,OO)),
%  select(C,IIR,IIR2),compound(C),generalize(C,M),select(M,OOR,OOR2),
  %trace,diff_terms(IIR,OOR,OUT),!.
  
/*
  append(L,[A|R],II),append(L,[B|R],OO),
  diff_lists(A,B,Diffs),
  (simular(Diffs,obj(I),obj(O),Sameness) ->  OUT = same_object(Sameness) ; OUT = props_diff(obj(I),obj(O),Diffs)).
*/

needs_indivs(I,_):- is_object(I),!,fail.
%needs_indivs(I,O):- is_grid(I),_unshared_indivs(I,O),!.
needs_indivs(I,O):- is_gridoid(I), \+ is_group(I),compute_unshared_indivs(I,O),!.

%diff_terms(IPs,OPs,Difs2):- diff_terms(IPs,OPs,Difs2).
diff_numbers(I,O,0):- I =:= O,!.
diff_numbers(I,O,diff(-(D))):- I<O,!, D is O -I.
diff_numbers(I,O,diff(+(D))):- D is I -O.

diff_terms(I,O,D):- nonvar(D),!,diff_terms(I,O,DD),!,D==DD.
diff_terms(I,O,D):- number(I),number(O),!,diff_numbers(I,O,D).
diff_terms(I,O,[]):- O==I,!.
diff_terms(I,O, [] ):- (never_diff(I);never_diff(O)),!.
diff_terms(I,O,[]):- var(I),var(O),!.
diff_terms(I,O,O):- var(I),!.
diff_terms(O,I,O):- var(I),!.
diff_terms(I,O,[]):- O=@=I,!.
diff_terms(I,O,O):- I==[],!.
diff_terms(I,O,I):- O==[],!.
%diff_terms(I,O, (O \== I)):- O=@=I,!.
diff_terms(I,O,[]):- no_diff(I,O),!.
diff_terms(O,I,[]):- no_diff(I,O),!.
diff_terms(Grid,Other,OUT):- needs_indivs(Grid,I),!,diff_terms(I,Other,OUT).
diff_terms(Other,Grid,OUT):- needs_indivs(Grid,I),!,diff_terms(Other,I,OUT).
% diff_terms(I,O,DD):-  is_group(I),is_group(O), !,  include_fav_points(I,II), include_fav_points(O,OO), diff_groups(I,O,DD).
diff_terms(I,O,DD):-  is_group(I), is_group(O), !, diff_groups(I,O,DD).
diff_terms(obj(I),obj(O),OUT):- !, diff_objects(I,O,OUT).
diff_terms([IH,IV],[OH,OV],D):- maplist(number,[IH,IV,OH,OV]),!,maplist(diff_numbers,[IH,IV],[OH,OV],D).
diff_terms(I,O, (diff_lists=@= D)):- is_list(I),is_list(O),!,diff_lists(I,O,D).
diff_terms(I,O,D):- is_dict(I),!,findall(D1,(get_dict(K, I, V),diff_terms(K=V,O,D1)),D).
diff_terms(IF=IA,O,IF=D):- find_kv(O,IF,OA),!,diff_terms(IA,OA,D).
diff_terms(I,O,D):- compound(I),compound(O),!,diff_compounds(I,O,D).
diff_terms(I,O,diff(I->O)).

never_diff(object_indv_id(_,_)).
diff_compounds(I,O, [] ):- (never_diff(I);never_diff(O)),!.
diff_compounds(I,O,D):- compound_name_arguments(I,IF,IA),compound_name_arguments(O,OF,OA),
  maplist(compute_diff_or_same,IA,OA,DA),
  diff_compounds(I,IF,O,OF,DA,D).

diff_compounds(_I, F,_O, F, DA, D):- !, compound_name_arguments(D,F,DA).
diff_compounds(I,_IF,O,_OF,_DA, diff(I->O)).


compute_diff_or_same(I,O,IO):- diff_terms(I,O,D),
  maybe_show_diff(I,O,D,IO).

maybe_show_diff(_,O,[],O):-!.
maybe_show_diff(_,_,D,D).


diff_lists(AA,BB,D):- is_list(A), is_list(B), sort(AA,A),sort(BB,B), length(A,AL), length(B,BL), !, 
  (AL>BL -> list_diff_recurse(B,A,D) ; list_diff_recurse(A,B,D)).

list_diff_recurse(I,O,[]):- I==O,!.
list_diff_recurse(I,O,[]):- I=@=O,!.
list_diff_recurse([],O,diff([]->O)):-!.
list_diff_recurse(O,[],diff(O->[])):-!.
list_diff_recurse(I,O,D1D):- select(CI,I,II),generalize_term(CI,CO),select(CO,O,OO), diff_terms(CI,CO,D1), !, 
       list_diff_recurse(II,OO,D), combine_diffs(D1,D,D1D).
list_diff_recurse([CE|I],[CE2|O],D1D):- diff_terms(CE,CE2,D1),!, list_diff_recurse(I,O,D),combine_diffs(D1,D,D1D).

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
  remove_global_points(IndvPoints,Points,UniquePoints),
  remove_global_points(Points,IndvPoints,UniqueInv),!,
  UniquePoints=UniquePointsO,
  UniqueInv=UniqueInvO.


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
