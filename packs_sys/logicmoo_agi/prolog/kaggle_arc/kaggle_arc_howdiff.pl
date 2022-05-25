

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

generalize_once(I,O):- var(O), !, freeze(O,once(generalize(I,O))).
generalize_once(I,O):- generalize(I,O).
%generalize(I,O):- var(I), !, freeze(I, generalize(I,O)).
generalize(I,O):- var(I), !, (O=I ; O = _).
generalize(I,O):- ( \+ compound(I) ),!, (generalize_atomic(I, O)).
generalize([A|B],[AA|BB]):- !, generalize(A,AA),generalize_cons(B,BB).
generalize(I,O):- 
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



  
%compute_diff(IPs,OPs,Difs2):- compute_diff(IPs,OPs,Difs2).
number_dif(I,O,0):- I =:= O,!.
number_dif(I,O,diff(-(D))):- I<O,!, D is O -I.
number_dif(I,O,diff(+(D))):- D is I -O.

usefull_compare(P):- compound(P),functor(P,F,_),!,usefull_compare(F).
usefull_compare(P):- changed_by(P,_).

changed_by(points_only,rotate).
changed_by(object_offset,move).
changed_by(point_count,grow).
changed_by(colors_count,repaint).
changed_by(object_size,copy).
changed_by(object_shape,rotate).

uncomparable(grid_size).
uncomparable(object_indv_id).

make_comparable(I,I):- var(I).
make_comparable(I,I):- \+ compound(I),!.
make_comparable(I,II):- is_list(I),!,maplist(make_comparable,I,II).
make_comparable(obj(L),obj(LL)):- !,make_comparable(L,LL).
make_comparable(L,L):- usefull_compare(L),!.
make_comparable(I,II):- functor(I,II,_).

group_diff([],O,left_extra(O)):-!.
group_diff(O,[],left_over(O)):-!.

group_diff(I0,O0,DD):-  
  make_comparable(I0,I),
  make_comparable(O0,O),
  get_selector(PI,PO),
  select(PI,I,II),
  select(PO,O,OO),
  compute_diff(PI,PO,D1),
  group_diff(II,OO,D),
  combine_diffs(D1,D,DD).

include_fav_points(I,II):- include(fav_points,I,II),II=[_,_|_],!.
include_fav_points(I,I).

fav_points(I):- \+ dislike_points(I).

dislike_points(obj(I)):-!,dislike_points(I).
dislike_points(I):- is_list(I),dislike_points1(L),forall(member(E,L),member(E,I)).

%dislike_points1([object_shape(dot),grid_size(H,V)]):- freeze(H, freeze(V, (HV is H * V, HV > 49))).
dislike_points1([colors_count([color_count(BG, _)]),object_shape(nonsolid)]):- freeze(BG,is_black_or_bg(BG)).

no_diff(in,out).
simular([],_,_,[]):- !.
simular(object_offset=Where,I,O,object_has_moved(Where)):-  fail,
  \+ (point_count(O,OC), OC < 6) ,
  \+ (colors_count(O,[color_count(BG, _)|_]),is_black_or_bg(BG)),
  object_indv_id(I,Tst,_Id1), \+ object_indv_id(O,Tst,_Id2).


%compute_diff_objs

compute_diff_objs(obj(I),II,obj(O),OO,OUT):- append(L,[A|R],II),append(L,[B|R],OO),
  compute_diff(A,B,Diffs),
    (simular(Diffs,obj(I),obj(O),Sameness) ->  OUT = same_object(Sameness) ; OUT = props_diff(obj(I),obj(O),Diffs)).

needs_indivs(I,_):- is_object(I),!,fail.
%needs_indivs(I,O):- is_grid(I),_unshared_indivs(I,O),!.
needs_indivs(I,O):- is_gridoid(I), \+ is_group(I),compute_unshared_indivs(I,O),!.


showdiff(A,B):- compute_diff(A,B,D),ptt(D).


compute_diff(I,O,D):- number(I),number(O),!,number_dif(I,O,D).
compute_diff(I,O,[]):- O==I,!.
compute_diff(I,O,[]):- (var(I);var(O)),!.
compute_diff(Grid,Other,OUT):- needs_indivs(Grid,I),!,compute_diff(I,Other,OUT).
compute_diff(Other,Grid,OUT):- needs_indivs(Grid,I),!,compute_diff(Other,I,OUT).
compute_diff(obj(I),obj(O),OUT):- fail,!,
  make_comparable(I,II), make_comparable(O,OO), compute_diff_objs(obj(I),II,obj(O),OO,OUT).
compute_diff(I,O,DD):- is_group(I),is_group(O), fail, !, include_fav_points(I,II),
  include_fav_points(O,OO),
  group_diff(II,OO,DD).
compute_diff(I,O,[O \== I]):- O=@=I,!.
compute_diff(I,O,[]):- no_diff(I,O),!.
compute_diff(O,I,[]):- no_diff(I,O),!.
compute_diff(I,O,O):- I==[],!.
compute_diff(I,O,I):- O==[],!.
compute_diff([IH,IV],[OH,OV],D):- maplist(number,[IH,IV,OH,OV]),!,maplist(number_dif,[IH,IV],[OH,OV],D).
compute_diff(I,O,list_diff=@=D):- is_list(I),is_list(O),!,list_diff(I,O,D).
compute_diff(I,O,D):- is_dict(I),!,findall(D1,(get_dict(K, I, V),compute_diff(K=V,O,D1)),D).
compute_diff(IF=IA,O,IF=D):-!, find_kv(O,IF,OA),!,compute_diff(IA,OA,D).
compute_diff(I,O,D):- compound(I),compound(O),
  compound_name_arguments(I,IF,IA),
  compound_name_arguments(O,OF,OA),!,
  maplist(compute_diff_or_same,[IF|IA],[OF|OA],[DF|DA]),
  compound_name_arguments(D,DF,DA).
compute_diff(I,O,diff(I->O)).

compute_diff_or_same(I,O,IO):- compute_diff(I,O,D),
  maybe_show_diff(I,O,D,IO).

maybe_show_diff(_,O,[],O):-!.
maybe_show_diff(_,_,D,D).



list_diff([],O,diff([]->O)):-!.
list_diff(O,[],diff(O->[])):-!.
list_diff(I,O,[]):- I==O,!.
list_diff(I,O,[]):- I=@=O,!.

list_diff(A,B,D):- length(A,AL),length(B,BL),!,AL>BL,list_diff(B,A,D).
%list_diff(I,O,D):- select(CE,I,II),select(CE2,O,OO),CE==CE2,!,list_diff(II,OO,D).
%list_diff(I,O,D):- select(CE,I,II),select(CE2,O,OO),compute_diff(CE,CE2,D2),D2==[],!,list_diff(II,OO,D).
%list_diff(I,O,D1D):- select(C-E,I,II),select(C2-E,O,OO),!,list_diff(II,OO,D),combine_diffs(change_color(C->C2),D,D1D).
%list_diff(I,O,D1D):- select(C-E,I,II),select(C-E2,O,OO),!,list_diff(II,OO,D),combine_diffs(change_point(E->E2),D,D1D).
list_diff(I,O,D1D):- select(CI,I,II),generalize(CI,CO),select(CO,O,OO),!,compute_diff(CI,CO,D1),list_diff(II,OO,D),combine_diffs(D1,D,D1D).
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
