/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

showdiff(A,B):- is_group(A), is_group(B), showdiff_groups(A,B),!.
showdiff(A,B):- is_object(A), is_object(B), showdiff_objects(A,B),!.
showdiff(A,B):- must(diff_terms(A,B,D)) -> D\==[],!,writeln('SOME DIFFERNCE'),pt(D).
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

%get_selector(PI,PO):- get_selector2(PI,PO).
/*
get_selector(PI,PO):- get_selector1(PI,PO).
get_selector(PI,PO):- get_selector2(PI,PO), \+ (get_selector1(PII,POO), PI=@=PII,PO=@=POO).
*/
%get_selector(_,_).
%get_selector2(obj(PI),obj(PO)):- 
%  get_selector_n2(PIN),do_nth(PIN,PI,PO).

get_selector(obj(A),obj(B)):- 
   s4(A1,B1),s4(A2,B2),
   my_append(A1,A2,A12),my_append(A12,_,A),
   my_append(B1,B2,B12),my_append(B12,_,B).

:- style_check(-singleton).
s4([A,B,C,D],[A,B,C,D]).
s4([A,B,C,D],[A,B,C,_]).
s4([A,B,C,D],[A,B,_,D]).
s4([A,B,C,D],[A,_,C,D]).
s4([A,B,C,D],[_,B,C,D]).
s4([A,B,C,D],[A,B,_,_]).
s4([A,B,C,D],[A,_,C,_]).
s4([A,B,C,D],[A,_,_,D]).
s4([A,B,C,D],[_,B,_,D]).
s4([A,B,C,D],[_,B,C,_]).
s4([A,B,C,D],[_,_,C,D]).
s4(3,[A,B,C,D],[A,_,_,_]).
s4(3,[A,B,C,D],[_,B,_,_]).
s4(3,[A,B,C,D],[_,_,C,_]).
s4(3,[A,B,C,D],[_,_,_,D]).
s4(4,[A,B,C,D],[_,_,_,_]).
:- style_check(+singleton).

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

generalize_once(I,O):- plain_var(O), !, freeze(O,once(generalize(I,O))).
generalize_once(I,O):- generalize(I,O).
%generalize(I,O):- plain_var(I), !, freeze(I, generalize(I,O)).
generalize(I,O):- plain_var(I), !, (O=I ; O = _).
generalize(I,O):- ( \+ compound(I) ), !, (O=I ; O = _).
%generalize(I,O):- ( \+ compound(I) ),!, generalize_atomic(I, O).
generalize([A|B],[AA|BB]):- !, generalize(A,AA),generalize_cons(B,BB).
generalize(_-P,_-P):-!.
generalize(I,O):- compound(I),!,
  compound_name_arguments(I,F,IA),
  generalize_list(IA,OA),
  compound_name_arguments(O,F,OA).
generalize(_,_).

generalize_arg(I,O):- O=I ; O = _.

generalize_list([],[]). 
generalize_list([I],[O]):- !, generalize_arg(I,O).
generalize_list([A,B],[AA,BB]):- !, generalize_arg(A,AA),generalize_arg(B,BB).
generalize_list([A,B,C],[AA,BB,CC]):- !, generalize_arg(A,AA), generalize_arg(B,BB),generalize_arg(C,CC).
generalize_list([A,B,C,D],[AA,BB,CC,DD]):- !, generalize_arg(A,AA), generalize_arg(B,BB),generalize_arg(C,CC),generalize_arg(D,DD).
generalize_list([A|B],[AA|BB]):- generalize_arg(A,AA),generalize_list(B,BB).

generalize_cons(I,O):- plain_var(I), !, (O=[];O=_).
generalize_cons([],O):- !, (O=[];O=_).
generalize_cons([A|B],[AA|BB]):- !, generalize(A,AA),generalize_list(B,BB).


combine_diffs([],D,D):-!.
combine_diffs(D,[],D):-!.
combine_diffs(D,D,D):-!.
combine_diffs(D1,D2,L12):- listify(D1,L1),listify(D2,L2),!,my_append(L1,L2,L12).



showdiff_groups(AG,BG):- not_list(AG),into_list(AG,AGL),!,showdiff_groups(AGL,BG).
showdiff_groups(AG,BG):- not_list(BG),into_list(BG,BGL),!,showdiff_groups(AG,BGL).
showdiff_groups(AG,BG):- once((proportional_how(AG,BG,DD), pt(cyan,proportional(DD)),
  maplist(showdiff_objects,AG,BG))),fail.
  
showdiff_groups(AG,BG):-
  maplist(obj_grp_comparable,AG,A1),
  maplist(obj_grp_comparable,BG,B1),
  
  showdiff_groups(A1,B1,
      [compare_objs1([perfect]),
       compare_objs1([turned,+loc]),
       compare_objs1([turned,-loc]),
       compare_objs1([moved]),
       compare_objs1([same])],
                  A4,B4),  
  ((AG==A4, fail) -> true ; length(A4,LenA),ignore((LenA>0,dash_chars)),print_list_of(inputUniqs=LenA,A4), dash_chars),
  ((BG==B4, fail) -> true ; length(B4,LenB),print_list_of(outputUniqs=LenB,B4), dash_chars),
  diff_groups(A4,B4,Diff),
  pt(Diff),
  !.

showdiff_groups(A,B,[],A,B):-!.
showdiff_groups(A,B,[H|T],AAR,BBR):- !,
  showdiff_groups(A,B,H,A1,B1),
  showdiff_groups(A1,B1,T,AAR,BBR).
showdiff_groups(A,B,Pred,AAR,BBR):- 
  pred_intersection(Pred,A,B,IntersectA,IntersectB,AAR,BBR),
  ignore((IntersectA\==[], maplist(showdiff_objects(Pred),IntersectA,IntersectB))).


diff_groups(A0,B0,DD):- 
  sort(A0,A1),
  sort(B0,B1),
  maplist(obj_grp_comparable,A1,A2),
  maplist(obj_grp_comparable,B1,B2),
  sort(A2,A3),
  sort(B2,B3),
  diff_groups1(A3,B3,DD).

diff_groups2(AAR,BBR,proportional(DD,Diffs)):- proportional(AAR,BBR,DD),
  maplist(diff_objects,AAR,BBR,Diffs).
diff_groups0(A3,B3,DD):- diff_groups2(A3,B3,DD).
diff_groups0(A3,B3,DD):- diff_groups1(A3,B3,DD).
diff_groups1([],[],[]):-!.
diff_groups1([],B,left_extra(BO)):- maplist(object_dglyph,B,BO).
diff_groups1(B,[],left_over(BO)):- maplist(object_dglyph,B,BO).

diff_groups1(AAR,BBR,DD):-
  %make_comparable(B0,B),
  /*
  ,*/
  %pred_intersection(overlap_same_obj,A,B,Antersect,AAR,BBR),
  %Antersect == [],
  select(PA,AAR,AA),
  get_selector(PA,PB),
  select(PB,BBR,BB),
  diff_objects(PA,PB,DAB),
  (DAB == [] -> D = [] ;  
    showdiff(PA,PB), 
      object_dglyph(PA,GA),
      object_dglyph(PB,GB),
      D = change_obj(GA,GB,DAB)),
  diff_groups1(AA,BB,D1),
  combine_diffs(D1, D , DD),!.

diff_groups1(A,B,disjointed(SharedT,AOnlyT,BOnlyT,AO,BO)):- 
  intersection(A,B,Shared,AOnly,BOnly),
  tersify(Shared,SharedT),
  tersify(AOnly,AOnlyT),
  tersify(BOnly,BOnlyT),
  maplist(object_dglyph,A,AO),maplist(object_dglyph,B,BO),!.

unused_diff_groups0(AAR,BBR,DD):-
  %make_comparable(B0,B),
  /*
  get_selector(PA,PB),*/
  %pred_intersection(overlap_same_obj,A,B,Antersect,AAR,BBR),
  %Antersect == [],
  select(PA,AAR,AA),
  select(PB,BBR,BB),
  same_colorless_points(PA,PB,D1),
  diff_groups1(AA,BB,D),
  combine_diffs(D1,D , DD).

%object_dglyph(O,G):- object_cglyph(O,G). % want this
object_dglyph(O,D):- object_glyph(O,G), atom_concat(' ',G,D),!.
obj_grp_comparable(I,obj(O)):- obj_make_comparable(I,M),
  my_partition(uncomparable(group),M,_,O).

include_fav_points(I,II):- include(fav_points,I,II),II=[_,_|_],!.
include_fav_points(I,I).

fav_points(I):- \+ dislike_points(I).

dislike_points(obj(I)):-!,dislike_points(I).
dislike_points(I):- is_list(I),dislike_points1(L),forall(member(E,L),member(E,I)).

%dislike_points1([iz(dot),grid_size(H,V)]):- freeze(H, freeze(V, (HV is H * V, HV > 49))).
dislike_points1([colors([cc(BG, _)]),iz(polygon)]):- freeze(BG,is_black_or_bg(BG)).


uncomparable(_,Var):- var(Var),!.
uncomparable(group,W):- good_overlap(W),!,fail.
uncomparable(group,W):- too_non_unique(W).
uncomparable(group,W):- too_unique(W).
uncomparable(H,P):- compound(P),!,functor(P,F,_),uncomparable2(H,F).
uncomparable(H,F):- uncomparable2(H,F).

uncomparable2(group,grid).
uncomparable2(group,globalpoints).
uncomparable2(group,grid_size).
%uncomparable2(group,o_i_d).
uncomparable2(group,link).
uncomparable2(object,iz).
uncomparable2(shape,localpoints).

never_diff(iz(_)).
never_diff(o_i_d(_,_)).

make_comparable(I,I):- plain_var(I).
make_comparable(I,I):- \+ compound(I),!.
make_comparable(I,II):- is_list(I),!,maplist(make_comparable,I,II).
make_comparable(obj(L),obj(LL)):- !,make_comparable(L,LL).
make_comparable(L,L):- usefull_compare(L),!.
make_comparable(I,II):- functor(I,II,_).


no_diff(in,out).
simular([],_,_,[]):- !.
simular(loc=Where,I,O,object_has_moved(Where)):-  
  \+ (mass(O,OC), OC < 6) ,
  \+ (colors(O,[cc(BG, _)|_]),is_black_or_bg(BG)),
  o_i_d(I,Tst,_Id1), \+ o_i_d(O,Tst,_Id2).



maye_sort(L,S):- is_list(L),\+ is_grid(L), !,sort(L,S).
maye_sort(S,S).
obj_make_comparable(I,_):- plain_var(I),!,fail.
obj_make_comparable(obj(I),O):- maplist(obj_make_comparable_e,I,O).
obj_make_comparable(I,O):- is_list(I),sort_obj_props(I,II),maplist(obj_make_comparable_e,II,O).
obj_make_comparable(I,O):- into_obj(I,M),obj_make_comparable(M,O).
%obj_make_comparable_e(I,O):- is_list(I),sort(I,O).
%obj_make_comparable_e(Comp,F):- compound(Comp),functor(Comp,F,_),f_uncomparable_e(F).
%obj_make_comparable_e(grid_size(_,_),grid([])).
%obj_make_comparable_e(I,O):- I=..[F,L],maye_sort(L,S),O=..[F,S].
obj_make_comparable_e(I,I).
%obj_make_comparable(I,SI):- exclude(uncomparable,I,II), sort(II,SI).

% f_uncomparable_e(F).
f_uncomparable_e(grid).
f_uncomparable_e(birth):- writeq(b).
f_uncomparable_e(grid_size).
f_uncomparable_e(iz).
%diff_objects(I,O,OUT):- !, fail, locally(set_prolog_lfag(gc,false),compute_diff_objs1(I,O,OUT)).
:- style_check(-singleton).

diff_objects(I,O,DiffsS):-  
  obj_make_comparable(I,II), obj_make_comparable(O,OO),!,
  intersection(II,OO,Intersect,IIR,OOR),!,
  findall(Diff, 
    (member(P,IIR),
      once((generalize(P,Q), member(Q,OOR))),
      once(diff_terms(P,Q,Diff))),Diffs),flatten(Diffs,DiffsF),list_to_set(DiffsF,DiffsS).

  %(simular(Diffs,obj(I),obj(O),Sameness) ->  OUT = same_object(Sameness) ; OUT = props_diff(obj(I),obj(O),Diffs)).

same_colorless_points(I,O,OUT):-  
  obj_make_comparable(I,II), obj_make_comparable(O,OO),!,
  intersection(II,OO,SL,IIR,OOR),!,
  %member(mass(_),SL),
  member(shape(_),SL),
  diff_objects(I,O,OUT).


combine_duplicates_old(IndvS,[IO|IndvSO]):- 
  select(I,IndvS,IndvS1),select(O,IndvS1,IndvS2),
  compare_objs1(perfect,I,O),
  override_object(O,I,IO),
  combine_duplicates_old(IndvS2,IndvSO).
combine_duplicates_old(IndvSO,IndvSO).


is_fti_step(combine_duplicates).
%combine_duplicates(_VM):-!.
combine_duplicates(VM):- combine_duplicates(VM.objs,set(VM.objs)).

combine_duplicates(IndvS,IndvSO):- combine_duplicates1(IndvS,IndvSO),!.
combine_duplicates(IndvS,IndvSO):-
  combine_duplicates1(IndvS,IndvSM),
  combine_duplicates1(IndvSM,IndvSO),!.

combine_duplicates1(IndvS,IndvSO):- 
  append(NoDupes,[I|Rest],IndvS),
  select(O,Rest,IndvS2),
  overlap_same_obj_no_diff(I,O),
  %merge_2objs(VM,I,O,[],IO),
  must_det_ll(indv_props(I,Props)),
  must_det_ll(override_object(Props,O,IO)),
  must_det_ll(combine_duplicates1([IO|IndvS2],NoMoreDupes)),
  must_det_ll(append(NoDupes,NoMoreDupes,IndvSO)),!.
combine_duplicates1(IndvSO,IndvSO).


overlap_same_obj_no_diff(I,O):- !, globalpoints(I,II),globalpoints(O,OO),!,pred_intersection((=@=),II,OO,_,_,[],[]),!.
overlap_same_obj_no_diff(I,O):- compare_objs1(perfect,I,O). %diff_objects(I,O,Diff),Diff==[]. 

overlap_same_obj(I,O):- compare_objs1(same,I,O).

%fti(VM,[combine_objects|set(VM.program_i)]):- combine_objects(VM),!.
is_fti_step(combine_objects).
combine_objects(VM):- combine_objects(VM.objs,set(VM.objs)).
combine_objects(I,I):-!.
combine_objects(IndvS,[obj(IO)|IndvSO]):- 
  select(obj([A,B,C,D|I]),IndvS,IndvS1),
  select(obj([A,B,C,D|O]),IndvS1,IndvS2),
  compare_objprops(perfect,I,O),
  override_object(O,I,IO),
  combine_objects(IndvS2,IndvSO).
combine_objects(IndvSO,IndvSO).

is_fti_step(combine_same_globalpoints).
%combine_same_globalpoints(_VM):-!.
combine_same_globalpoints(VM):- combine_same_globalpoints(VM.objs,set(VM.objs)).

combine_same_globalpoints(IndvS,IndvSO):-   
  append(NoDupes,[I|Rest],IndvS),
  select(O,Rest,IndvS2),
  same_globalpoints(I,O),
  %merge_2objs(VM,I,O,[],IO),
  must_det_ll(indv_props(I,Props)),
  must_det_ll(override_object(Props,O,IO)),
  must_det_ll(combine_same_globalpoints([IO|IndvS2],NoMoreDupes)),
  must_det_ll(append(NoDupes,NoMoreDupes,IndvSO)),!.
combine_same_globalpoints(IndvSO,IndvSO).

%overlap_same_obj_no_diff(I,O):- compare_objs1(perfect,I,O). %diff_objects(I,O,Diff),Diff==[]. 
%overlap_same_obj(I,O):- compare_objs1(same,I,O).

showdiff_objects(A,B):- into_obj(A,A1),into_obj(B,B1), !, showdiff_objects(sameness,A1,B1),!.
showdiff_objects(N,O1,O2):- diff_objects(O1,O2,Diffs), showdiff_objects(N,O1,O2,Diffs).
showdiff_objects(N,O1,O2,[]):- print_list_of(N,[O1,O2]),!.
showdiff_objects(N,O1,O2,Diffs):- 
  print_list_of(diffs,Diffs),
  print_list_of(N,[O1,O2]),
  findall(E,compare_objs1(E,O1,O2),L),
  pt(compare_objs1=L), dash_chars.


compare_objs1(How,I,O):- is_list(I), is_list(O), compare_objprops(How,I,O).
compare_objs1(How,obj(I),obj(O)):- !, compare_objprops(How,I,O).
compare_objs1(How,I,O):- into_obj(I,I2),into_obj(O,O2),!,compare_objs1(How,I2,O2).

compare_objprops(How,I,O):- intersection(I,O,SL,UI,UO), !, compare_objs1(How,I,O,SL,UI,UO).

compare_objs1(-X,I,O,SL,UI,UO):- sprop(X), \+ compare_objs1(X,I,O,SL,UI,UO).
compare_objs1(+X,I,O,SL,UI,UO):- plain_var(X),!, sprop(X), \+ compare_objs1(-X,I,O,SL,UI,UO).

compare_objs1( X,I,O,SL,UI,UO):- is_list(X),!,forall(member(E,X),compare_objs1(E,I,O,SL,UI,UO)),!.
compare_objs1( X,I,O,SL,UI,UO):- \+ \+ sprop_of(X,_),!, forall(sprop_of(X,E), compare_objs1(E,I,O,SL,UI,UO)),!.
compare_objs1( X,I,O,SL,UI,UO):- \+ \+  prop_of(X,_), !, prop_of(X,E),e1_member(E,SL),!.
compare_objs1(perfect,I,O,SL,UI,UO):- forall(prop_of(_,E), compare_objs1(E,I,O,SL,UI,UO)).
compare_objs1(perfect,I,O,SL,UI,UO):-  e1_member(globalpoints(_),SL).

sprop(same).
sprop(moved).
sprop(turned).
sprop(X):- prop_of(X,_).
sprop(perfect).


%sprop(perfect).

sprop_of(same,visually).
sprop_of(same,size).
sprop_of(same,shape).
sprop_of(same,colors).

sprop_of(moved,same).
sprop_of(moved,loc).

sprop_of(turned,rotate).

sprop_of(reshape_and_recolor,localpoints).




%prop_of(visual_impact,globalpoints(_)).
usefull_compare(P):- compound(P),functor(P,F,_),!,usefull_compare(F).
usefull_compare(P):- changed_by(P,_).

changed_by(shape,reshape).
changed_by(loc,move).
changed_by(mass,grow).
changed_by(localpoints,reshape_and_recolor).
changed_by(rotation,rotate).
changed_by(colors,repaint).
changed_by(v_hv,copy).


:- style_check(+singleton).
%compute_diff_objs2(I,IIR,O,OOR,[]):-  diff_terms(IIR,OOR,[]),


  %pt(remove_sames(II,OO)),
%  select(C,IIR,IIR2),compound(C),generalize(C,M),select(M,OOR,OOR2),
  %trace,diff_terms(IIR,OOR,OUT),!.
  
/*
  my_append(L,[A|R],II),my_append(L,[B|R],OO),
  diff_lists(A,B,Diffs),
  (simular(Diffs,obj(I),obj(O),Sameness) ->  OUT = same_object(Sameness) ; OUT = props_diff(obj(I),obj(O),Diffs)).
*/

needs_indivs(I,_):- is_object(I),!,fail.
%needs_indivs(I,O):- is_grid(I),_unshared_indivs(I,O),!.
needs_indivs(I,O):- is_gridoid(I), \+ is_group(I), dumpST, trace, compute_unshared_indivs(I,O),!.

%diff_terms(IPs,OPs,Difs2):- diff_terms(IPs,OPs,Difs2).
diff_numbers(I,O,0):- I =:= O,!.
diff_numbers(I,O,diff(-(D))):- I<O,!, D is O -I.
diff_numbers(I,O,diff(+(D))):- D is I -O.

diff_terms(I,O,D):- nonvar_or_ci(D),!,diff_terms(I,O,DD),!,D==DD.
diff_terms(I,O,D):- number(I),number(O),!,diff_numbers(I,O,D).
diff_terms(I,O,[]):- O==I,!.
diff_terms(I,O, [] ):- (never_diff(I);never_diff(O)),!.
diff_terms(I,O,[]):- plain_var(I),plain_var(O),!.
diff_terms(I,O,O):- plain_var(I),!.
diff_terms(O,I,O):- plain_var(I),!.
diff_terms(I,O,[]):- O=@=I,!.
diff_terms(I,O,O):- I==[],!.
diff_terms(I,O,I):- O==[],!.
%diff_terms(I,O, (O \== I)):- O=@=I,!.
diff_terms(I,O,[]):- no_diff(I,O),!.
diff_terms(O,I,[]):- no_diff(I,O),!.
% diff_terms(I,O,DD):-  is_group(I),is_group(O), !,  include_fav_points(I,II), include_fav_points(O,OO), diff_groups(I,O,DD).
diff_terms(I,O,DD):-  is_group(I), is_group(O), !, diff_groups(I,O,DD).
diff_terms(obj(I),obj(O),OUT):- !, diff_objects(I,O,OUT).
diff_terms([IH,IV],[OH,OV],D):- maplist(number,[IH,IV,OH,OV]),!,maplist(diff_numbers,[IH,IV],[OH,OV],D).
diff_terms(I,O, (diff_lists=@= D)):- is_list(I),is_list(O),!,diff_lists(I,O,D).
diff_terms(I,O,D):- is_map(I),!,findall(D1,(get_kov(K, I, V),diff_terms(K=V,O,D1)),D).
diff_terms(Grid,Other,OUT):- needs_indivs(Grid,I),!,diff_terms(I,Other,OUT).
diff_terms(Other,Grid,OUT):- needs_indivs(Grid,I),!,diff_terms(Other,I,OUT).
diff_terms(IF=IA,O,IF=D):- find_kv(O,IF,OA),!,diff_terms(IA,OA,D).
diff_terms(I,O,D):- compound(I),compound(O),!,diff_compounds(I,O,D).
diff_terms(I,O,diff(I->O)).

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
find_kv(List,OF,OA):- is_list(List),member(E,List),nonvar_or_ci(E),find_kv(E,OF,OA).
find_kv(Dict,OF,OA):- is_map(Dict),get_kov(OF,Dict,OA).
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
count_difs(A,B,C):- count_difs0(A,B,C).
count_difs0(Out,GridO,0):- Out=@=GridO,!.
count_difs0(Out,GridO,1):- ((\+ compound(Out)) ; \+ compound(GridO)),!.
count_difs0([A|Out],[B|GridO],Errors):- 
      count_difs0(A,B,Errors1),
      count_difs0(Out,GridO,Errors2),!,
      Errors is Errors1 + Errors2.
count_difs0(Out,GridO,Errors):-
  compound_name_arguments(Out,F,A),
  compound_name_arguments(GridO,FO,AO),
  count_difs0([F|A],[FO|AO],Errors),!.
count_difs0(_Out,_GridO,1).

:- fixup_exports.

