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
generalize_term(iz(I),iz(O)):-!,generalize_term(I,O).
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
combine_diffs(nc([]),D,D):-!.
combine_diffs(D,nc([]),D):-!.
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


diff_groups2(AAR,BBR,proportional(DD,Diffs)):- proportional(AAR,BBR,DD), maplist(diff_objects,AAR,BBR,Diffs).
diff_groups0(A3,B3,DD):- diff_groups2(A3,B3,DD).
diff_groups0(A3,B3,DD):- diff_groups1(A3,B3,DD).

diff_groups(A0,B0,DD):- 
  maplist(obj_grp_comparable,A0,A2),
  maplist(obj_grp_comparable,B0,B2),
  diff_groups1(A2,B2,DD).


select_obj_pair(AAR,BBR,PA,PB):-
  select_obj_pair1(AAR,BBR,PA,PB),
  select_obj_pair2(AAR,BBR,PA2,PB2),
  ignore((fail, ([PA,PB] == [PA2,PB2]) , wdmsg([a,PA,b,PB]))),!.
select_obj_pair(AAR,BBR,PA,PB):-
  select_obj_pair1(AAR,BBR,PA,PB).
select_obj_pair(AAR,BBR,PA2,PB2):-
  select_obj_pair2(AAR,BBR,PA2,PB2).


select_obj_pair1(AAR,BBR,PA,PB):- 
 findall(pair(L,PA,PB),
  (member(PA,AAR), obj_atoms(PA,PAP), 
   member(PB,BBR), obj_atoms(PB,PBP),
     intersection(PAP,PBP,Shared,_,_),length(Shared,L)),
   Pairs),sort(Pairs,SPairs),last(SPairs,pair(L,PA,PB)),
 nop(wdmsg(pair(L,PA,PB))).

select_obj_pair2(AAR,BBR,PA,PB):- member(PA,AAR), get_selector(PA,PB), member(PB,BBR).


obj_atoms(PA,PAP):-nonvar(PA),obj_atoms1(PA,M),findall(E,(member(SE,M),sub_obj_atom(E,SE)),PAP),!.

sub_obj_atom(_,M):- \+ compound(M),!,fail.
sub_obj_atom(A,M):- M = localpoints(_),!,A=M.
sub_obj_atom(A,A).
sub_obj_atom(A,M):- arg(1,M,A),is_list(A).
sub_obj_atom(E,M):- sub_term(E,M),ground(E),compound(E),arg(1,E,A),\+ compound(A).

obj_atoms1(PA,PAP):- is_list(PA),!,PAP=PA.
obj_atoms1(obj(PA),PAP):- is_list(PA),!,PAP=PA.
%obj_atoms1(obj(PA,PAP):- indv_props(PA,PAP),!.

never_pair(PA,PB):- nop(never_pair(PA,PB)),!,fail.

reorder_group_objs(AAR,BBR,[PA|G1],[PB|G2]):-
   select_obj_pair(AAR,BBR,PA,PB),
  \+ never_pair(PA,PB),
  select(PA,AAR,AA), select(PB,BBR,BB),!,
  reorder_group_objs(AA,BB,G1,G2).
reorder_group_objs(AA,BB,AA,BB).

diff_groups1([],[],[]):-!.
diff_groups1([],B,left_extra(BO)):- maplist(object_dglyph,B,BO).
diff_groups1(B,[],left_over(BO)):- maplist(object_dglyph,B,BO).
diff_groups1(AAR,BBR,DD):-
  select_obj_pair(AAR,BBR,PA,PB),
  \+ never_pair(PA,PB),
  select(PA,AAR,AA), select(PB,BBR,BB),
  diff_objects(PA,PB,DAB),
  (DAB == [] -> D = [] ;  
     (nop(showdiff(PA,PB)),
      object_dglyph(PA,GA), object_dglyph(PB,GB),
      D = change_obj(GA,GB,DAB))),
  diff_groups1(AA,BB,D1),
  combine_diffs(D1, nc( D) , DD),!.
diff_groups1(A,B,disjointed(SharedT,AOnlyT,BOnlyT,AO,BO)):- 
  intersection(A,B,Shared,AOnly,BOnly),
  tersify_cheap(Shared,SharedT),
  tersify_cheap(AOnly,AOnlyT),
  tersify_cheap(BOnly,BOnlyT),
  maplist(object_dglyph,A,AO),maplist(object_dglyph,B,BO),!.

tersify_cheap(I,O):- tersify(I,O),!.

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
%uncomparable2(group,obj_to_oid).
uncomparable2(group,link).
uncomparable2(object,iz).
uncomparable2(shape,localpoints).

never_show_diff(V):- var(V),!,fail.
never_show_diff(_):- nb_current(diff_porportional,t),!,fail.
never_show_diff(o).
never_show_diff(link).
never_show_diff(iz(A)):- atomic(A).
never_show_diff(obj_to_oid).
never_show_diff(change).
never_show_diff(birth).
never_show_diff(V):- compound(V),functor(V,F,_),!,never_show_diff(F).

never_do_diff(V):- never_show_diff(V).

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
  object_glyph(I,G), \+ object_glyph(O,G).



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
  %member(amass(_),SL),
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
  same_globalpoints(I,O),!,
  %merge_2objs(VM,I,O,[],IO),
  must_det_ll(indv_props(I,Props)),
  my_partition(props_not_for_merge,Props,_Exclude,Include),
  must_det_ll(override_object([iz(merged(cgp))|Include],O,IO)),
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
changed_by(amass,grow).
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
needs_indivs(I,O):- is_gridoid(I), \+ is_group(I), arcST, trace, compute_unshared_indivs(I,O),!.

%diff_terms(IPs,OPs,Difs2):- diff_terms(IPs,OPs,Difs2).

diff_terms(I,O,D):- nonvar_or_ci(D),!,diff_terms(I,O,DD),!,D==DD.
diff_terms(I,O,D):- maybe_number(I,N1),maybe_number(O,N2),!,proportional_size(N1,N2,D).
diff_terms(I,O,[]):- O==I,!.
diff_terms(I,O, [] ):- (never_do_diff(I);never_do_diff(O)),!.
diff_terms(I,O,[]):- plain_var(I),plain_var(O),!.
diff_terms(I,O,O):- plain_var(I),!.
diff_terms(O,I,O):- plain_var(I),!.
diff_terms(I,O,[]):- O=@=I,!.
diff_terms(I,O,O):- I==[],!.
diff_terms(I,O,I):- O==[],!.
%diff_terms(I,O, (O \== I)):- O=@=I,!.
diff_terms(group_o(I),group_o(O),group_o(DD)):- !, must_det_ll(diff_groups(I,O,DD)).
diff_terms(I,O,DD):-  is_group(I), is_group(O), !, must_det_ll(diff_groups(I,O,DD)).
diff_terms(I,O,[]):- no_diff(I,O),!.
diff_terms(O,I,[]):- no_diff(I,O),!.
% diff_terms(I,O,DD):-  is_group(I),is_group(O), !,  include_fav_points(I,II), include_fav_points(O,OO), diff_groups(I,O,DD).
diff_terms(obj(I),obj(O),OUT):- !, diff_objects(I,O,OUT).
diff_terms([IH,IV],[OH,OV],D):- maplist(number,[IH,IV,OH,OV]),!,maplist(diff_numbers,[IH,IV],[OH,OV],D).
diff_terms(I,O, (diff_lists=@= D)):- non_grid_list(I),non_grid_list(O),!,diff_lists(I,O,D).
diff_terms(I,O,D):- is_map(I),!,findall(D1,(get_kov(K, I, V),diff_terms(K=V,O,D1)),D).
diff_terms(Grid,Other,OUT):- needs_indivs(Grid,I),!,diff_terms(I,Other,OUT).
diff_terms(Other,Grid,OUT):- needs_indivs(Grid,I),!,diff_terms(Other,I,OUT).
diff_terms(IF=IA,O,IF=D):- find_kv(O,IF,OA),!,diff_terms(IA,OA,D).
diff_terms(I,O,D):- compound(I),compound(O),!,diff_compounds(I,O,D).
diff_terms(I,O,diff(I->O)).

diff_compounds(I,O, [] ):- (never_show_diff(I);never_show_diff(O)),!.
diff_compounds(I,O,D):- compound_name_arguments(I,IF,IA),compound_name_arguments(O,OF,OA),
  maplist(compute_diff_or_same,IA,OA,DA),
  diff_compounds(I,IF,O,OF,DA,D).

diff_compounds(_I, F,_O, F, DA, D):- !, compound_name_arguments(D,F,DA).
diff_compounds(I,_IF,O,_OF,_DA, diff(I->O)).


compute_diff_or_same(I,O,IO):- diff_terms(I,O,D),
  maybe_show_diff(I,O,D,IO).

maybe_show_diff(_,O,[],O):-!.
maybe_show_diff(_,_,D,D).


diff_lists(AA,BB,D):- must_det_ll((non_grid_list(AA), non_grid_list(BB), sort(AA,A),sort(BB,B), length(A,AL), length(B,BL))),
 must_det_ll(AL>BL -> list_diff_recurse(B,A,D) ; list_diff_recurse(A,B,D)).

list_diff_recurse(I,O,[]):- I==O,!.
list_diff_recurse(I,O,[]):- I=@=O,!.
list_diff_recurse([],O,diff([]->O)):-!.
list_diff_recurse(O,[],diff(O->[])):-!.
list_diff_recurse(I,O,D1D):- select(CI,I,II),generalize_term(CI,CO),select(CO,O,OO), diff_terms(CI,CO,D1),  
       list_diff_recurse(II,OO,D), combine_diffs(D1,D,D1D),!.
list_diff_recurse(I,O,D1D):- select(CI,O,II),generalize_term(CI,CO),select(CO,I,OO), diff_terms(CI,CO,D1),  
       list_diff_recurse(II,OO,D), combine_diffs(D1,D,D1D),!.
list_diff_recurse([CE|I],[CE2|O],D1D):- diff_terms(CE,CE2,D1), list_diff_recurse(I,O,D),combine_diffs(D1,D,D1D),!.

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


proportional_how(AG,BG,Set):- into_list(AG,AGL),into_list(BG,BGL), proportional_how_l(AGL,BGL,Set). %findall(DD,proportionate(AGL,BGL,DD),List),list_to_set(List,Set).

proportional_how_l(AG,BG,Set):-  my_permutation(AG,AGL), my_permutation(BG,BGL), proportionate(not_very_simular,AGL,BGL,Set).

my_permutation(BG,BG):-!.
my_permutation(BG,BGL):- permutation(BG,BGL).
%proportionate(List1,List2):- proportionate(List1,List2,_),!.
proportionate(_Nvm,[],[],_).
proportionate(NVM,[HV1|List1],[HV2|List2],N):-
   proportional(HV1,HV2,N),
   nop(call(NVM, N)),
   proportionate(NVM,List1,List2,N).

not_very_simular(X):- \+ not_very_different(X).

not_very_different(vis_hv_term(size(A,B))):- !, not_very_different_t(A),not_very_different_t(B).
not_very_different(vis_hv_term(area(A))):-   !, not_very_different_t(A).
not_very_different(loc_term(loc(A,B))):-  !, not_very_different_t(A),not_very_different_t(B).
not_very_different(center_term(loc(A,B))):-  !, not_very_different_t(A),not_very_different_t(B).

not_very_different(mass(A)):- !, not_very_different_t(A).
not_very_different(amass(A)):- !, not_very_different_t(A).
not_very_different_t(difference(0)). not_very_different_t(ratio(1)). not_very_different_t(moved(0)).


proportional_types(list,A,B,D):- !, proportional_lists(A,B,D).
proportional_types(_How,A,B,D):- proportional(A,B,D).

maybe_label_colors(G,L):- is_grid(G),!,mapgrid(color_name,G,L),!,G\==L.

non_grid_list(X):- is_list(X), \+ is_grid(X).

unused_proportion(_,_,_):- \+ nb_current(allow_unused_proportion,t),!,fail.
unused_proportion(Obj1,Obj2,Obj3):- unused_proportion1(Obj1,Obj2,Obj3),!.

unused_proportion1( Obj1,Obj2,Obj1):- Obj1=@=Obj2.
unused_proportion1(Obj1,_Obj2,Obj1):- var(Obj1),!.
unused_proportion1(_Obj1,Obj2,Obj2):- var(Obj2),!.

maybe_reorder_pair(A2,B2,A3,B3):- is_group(A2),is_group(B2),
  once(reorder_group_objs(A2,B2,A3,B3)),(A2\==A3;B2\==B3),!.

proportional(Obj1,Obj2,Obj3):- unused_proportion1(Obj1,Obj2,Obj3),!.
proportional(A2,B2,List):- maybe_reorder_pair(A2,B2,A3,B3), !, proportional(A3,B3,List).
proportional(L1,L2,List):- non_grid_list(L1),non_grid_list(L2),!,must_det_ll(proportional_lists(L1,L2,List)).
proportional(size(H1,V1),size(H2,V2),size(H,V)):- proportional_size(H1,H2,H),proportional_size(V1,V2,V).
proportional(size(V1,H1),size(H2,V2),size_inv(H,V)):- proportional_size(H1,H2,H),proportional_size(V1,V2,V).
proportional(size(H1,V1),size(H2,V2),area(HV)):- !, HV1 is H1*V1, HV2 is H2*V2, proportional_size(HV1,HV2,HV).
proportional(loc(H1,V1),loc(H2,V2),loc(H,V)):- !, proportional_loc(H1,H2,H),proportional_loc(V1,V2,V).
proportional(colors(H1),colors(H2),color_changes(H)):- !, proportional_lists(H1,H2,H).
%proportional(cc(N1,C),cc(N2,C),cc(H,C)):- !, proportional_size(N1,N2,H).
proportional(N1,N2,N):- number(N1),number(N2),!,proportional_size(N1,N2,N).

proportional(G1,G2,Out):- maybe_label_colors(G1,L1),!, proportional(L1,G2,Out).
proportional(G1,G2,Out):- maybe_label_colors(G2,L2),!, proportional(G1,L2,Out).

proportional(A,B,C):- maybe_extract_values(B,BB), compound(A), \+ maybe_extract_values(A,_), proportional(A,BB,AABB),proportional(AABB,B,C),!.
proportional(B,A,C):- maybe_extract_values(B,BB), compound(A), \+ maybe_extract_values(A,_), proportional(A,BB,AABB),proportional(AABB,B,C),!.

proportional(G1,G2,Out):- unused_proportion(G1,G2,Out),!.
%proportional(E1,E2,E1):- E1=@=E2,!.
proportional(Obj1,Obj2,Out):- 
  decl_pt(prop_h,P1P2), P1P2=..[P2,P1|Lst],
  once((once((on_x_log_and_fail(call(P1,Obj1)),
              on_x_log_and_fail(call(P1,Obj2)))),
  length(Lst,Len), length(NewLst1,Len),length(NewLst2,Len),
  once((on_x_log_and_fail(apply(P2,[Obj1|NewLst1])),
        on_x_log_and_fail(apply(P2,[Obj2|NewLst2])))))), 
  maplist(proportional_types,Lst,NewLst1,NewLst2,OutL), Out =.. [P2|OutL].

proportional(L1,L2,_List):- is_grid(L1),is_grid(L2),!,fail.
proportional(N1,N2,N):- compound(N1),compound_name_arguments(N1,F,A1),compound_name_arguments(N2,F,A2),
  maplist(proportional_or_same,A1,A2,AR),compound_name_arguments(N,F,AR).
proportional(L1,L2,Diff):- locally(nb_setval(diff_porportional,t),diff_terms(L1,L2,Diff)),!.

proportional_or_same(G1,G2,Out):- unused_proportion(G1,G2,Out),!.
proportional_or_same(A1,A2,A1):- A1==A2,!.
proportional_or_same(L1,L2,LR):- proportional(L1,L2,LR).

on_x_log_and_fail(G):- catch(G,E,(wdmsg(red((E -> G))),trace,G,fail)).

%proportional(N1,N2,N):- is_object(N1),is_object(N2),!,proportional_objs(N1,N2,N).
%proportional(N1,N2,N):- is_grid(N1),is_grid(N2),!,proportional_grids(N1,N2,N).

maybe_number(N,N):- \+ compound(N),!,number(N).
maybe_number(M,N):- extract_vals(M,[N|_]),!,number(N),!.

into_vals(V1,V2,Vals):- extract_vals(V1,VV1),extract_vals(V2,VV2),append(VV1,VV2,Vs),sort(Vs,Vals).

%extract_vals(M,Vals):- var(M),!,Vals=M.
%extract_vals(M,Vals):- \+ compound(M),!,Vals=[M].
extract_vals(M,Vals):- maybe_extract_values(M,Vals),!.
extract_vals(M,N):- M=..[_,A],maybe_extract_values(A,N).
extract_vals(V2,[V2]).

maybe_extract_values(Color,Values):- compound(Color), Color=..[DF,vals(Values)|_],!,diff_f(DF),is_list(Values),!.
maybe_extract_value(Color,Value):- maybe_extract_values(Color,Values),!,member(Value,Values).

proportional_size(N1,N2,P):- unused_proportion(N1,N2,P),!.
proportional_size(M1,M2, num(vals([M1,M2]),+N,r(R))):- maybe_number(M1,N1),maybe_number(M2,N2), N is N2-N1, catch(R is rationalize(N1/N2),_,true).


diff_f(lst).
diff_f(num).


proportional_loc(G1,G2,Out):- unused_proportion(G1,G2,Out),!.
proportional_loc(N1,N2,moved(N1,N,N2)):- diff_numbers(N1,N2,N).
diff_numbers(I,O,0):- I =:= O,!.
diff_numbers(I,O,diff(-(D))):- I<O,!, D is O -I.
diff_numbers(I,O,diff(+(D))):- D is I -O.

is_vset(Colors):- sort(Colors,ColorsS),!,Colors=ColorsS.
is_lset(Colors):- list_to_set(Colors,ColorsS),!,Colors=ColorsS.


proportional_lists(L1,L2,L1):- unused_proportion(L1,L2,_Out).
%proportional_lists(L1,L2,OUT):- is_vset(L1),is_vset(L2),!,proportional_sets(L1,L2,OUT).
proportional_lists(L1,L2,Out):- maybe_extract_value(L1,V),V\==L2,!,proportional_lists(V,L2,Out).
proportional_lists(L1,L2,Out):- maybe_extract_value(L2,V),V\==L1,!,proportional_lists(L1,V,Out).
proportional_lists(L1,L2,OUT):- is_group(L1),is_group(L2),must_det_ll(diff_groups(L1,L2,OUT)),!.
proportional_lists(L1,L2,OUT):- 
 must_det_ll((
  length(L1,N1),length(L2,N2), proportional_size(N1,N2,N),
  intersection(L1,L2,Shared,IOnlyC,OOnlyC), diff_lists(IOnlyC,OOnlyC,Diff),
  maplist(length,[L1,L2,IOnlyC,Shared,OOnlyC],Lens),
  into_vals(L1,L2,Vals),
  list_to_set(Shared,SharedS),
  OUT=..[lst,vals(Vals),len(N),s(SharedS),dif(Diff),l(IOnlyC),r(OOnlyC)|Lens])),!.

proportional_lists(L1,L2,p(L1,L2)):-!.

map_overlap(P,L1,L2,[R|R13]):- select(E1,L1,R1),select(E2,L2,R2),call(P,E1,E2,R),map_overlap(P,R1,R2,R13),!.
map_overlap(_P,[],L2,L2):-!.
map_overlap(_P,L1,[],L1).

%proportional_grids(Obj1,Obj2,vis_hv_term(N)):- once((vis_hv_term(Obj1,N1),vis_hv_term(Obj2,N2))),proportional(N1,N2,N).
%proportional_grids(Obj1,Obj2,loc_term(N)):- once((loc_term(Obj1,N1),loc_term(Obj2,N2))),proportional(N1,N2,N).
%proportional_grids(Obj1,Obj2,center_term(N)):- center_term(Obj1,N1),center_term(Obj2,N2),proportional(N1,N2,N).
%proportional_grids(Obj1,Obj2,amass(N)):- once((amass(Obj1,N1),amass(Obj2,N2))),proportional_size(N1,N2,N).

/*
The IEEE floating-point standard, supported by almost all modern floating-point units, specifies that every floating 
 point arithmetic operation, including division by zero, has a well-defined result. 
  The standard supports signed zero, as well as infinity and NaN (not a number). 
   There are two zeroes: +0 (positive zero) and -0 (negative zero) and this removes any ambiguity when dividing. 
   In IEEE 754 arithmetic, a ÷ +0 is positive infinity when a is positive, negative infinity when a is negative, 
   and NaN when a = ±0. The infinity signs change when dividing by -0 instead.
*/
ratio_for(Ratio,_/_=Out,In):- nonvar(Out), !, ratio_for(Ratio,Out,In).
ratio_for(Ratio,Out,_/_=In):- nonvar(In), !, ratio_for(Ratio,Out,In).
ratio_for(Out/In=Ratio,Out,In):- ratio_for0(Ratio,Out,In).
ratio_for0(1.0,Out,In):- 0 is In, 0 is Out,!.
ratio_for0(Ratio,_Out,In):- 0 is In, !, Ratio is -0.0.
ratio_for0(1,Out,In):- Out =:= In.
ratio_for0(Ratio,Out,_In):- 0 is Out, !, Ratio is +0.0.
ratio_for0(Ratio,Out,In):- catch(Ratio is rationalize(Out/In),error(evaluation_error(_Zero_divisor),_),fail),!.
ratio_for0(Ratio,Out,In):- catch(NRatio is rationalize(In/Out),error(evaluation_error(_Zero_divisor),_),fail),!, Ratio is -NRatio.

:- decl_pt(prop_h,each_object(is_grid, list)).
each_object(Grid,ListO):- 
 print_collapsed(100,individuate(complete,Grid,List)),!,
 simplify_objs(List,ListO).

simplify_objs(I,O):-is_list(I),!,maplist(simplify_objs,I,O).
simplify_objs(obj(I),obj(O)):-!,simplify_objs(I,M),include(compound,M,M1),sort(M1,M2),reverse(M2,M3),M3=O.
%simplify_objs(iz(g(_)),iz(g(_))).
%simplify_objs(Comp,F):- compound(Comp),functor(Comp,F,_),uncomparable(group,Comp),!.
simplify_objs(F,F).

prefer_grid(G):- is_object_or_grid(G).

:- decl_pt(prop_h,unique_colors(prefer_grid, list)).
:- decl_pt(prop_h,mass(is_object_or_grid,number)).

:- decl_pt(prop_h,center_term(is_object,loc)).
:- decl_pt(prop_h,loc_term(is_object,loc)).

:- decl_pt(prop_h,has_y_rows(is_grid,colcount,color,list(rownums))).
:- decl_pt(prop_h,has_x_columns(is_grid,rowcount,color,list(colnums))).


:- fixup_exports.

