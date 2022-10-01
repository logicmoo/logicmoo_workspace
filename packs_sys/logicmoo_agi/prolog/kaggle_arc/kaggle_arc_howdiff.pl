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
showdiff(A,B):- must(diff_terms(A,B,D)) -> D\==[],!,writeln('SOME DIFFERNCE'),pp(D).
showdiff(_,_):- writeln('NO DIFFERNCE').



do_nth([],_,_):-!.
do_nth([H|T],I,O):-!, do_nth(H,I,O),do_nth(T,I,O).
do_nth(N,I,O):- nth1(N,I,E),nth1(N,O,E).


get_selector(obj(A),obj(B)):- s4(A1,B1), append(A1,_,A), append(B1,_,B).

s4(I,O):- s4(N,I,O), N=<1.
:- style_check(-singleton).
s4(0,[A,B,C,D],[A,B,C,D]).
s4(1,[A,B,C,D],[A,B,C,_]).
s4(1,[A,B,C,D],[A,B,_,D]).
s4(1,[A,B,C,D],[A,_,C,D]).
s4(2,[A,B,C,D],[A,B,_,_]).
s4(1,[A,B,C,D],[_,B,C,D]).
s4(2,[A,B,C,D],[A,_,C,_]).
s4(2,[A,B,C,D],[A,_,_,D]).
s4(2,[A,B,C,D],[_,B,_,D]).
s4(2,[A,B,C,D],[_,B,C,_]).
s4(2,[A,B,C,D],[_,_,C,D]).
s4(3,[A,B,C,D],[A,_,_,_]).
s4(3,[A,B,C,D],[_,B,_,_]).
s4(3,[A,B,C,D],[_,_,C,_]).
%s4(3,[A,B,C,D],[_,_,_,D]).
%s4(4,[A,B,C,D],[_,_,_,_]).
:- style_check(+singleton).

get_selector1(obj(PI),obj(PO)):- get_selector_n1(PIN),do_nth(PIN,PI,PO).
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
generalize_atomic(I,O):- atomic_type(P1), call(P1,I),!, freeze(O, call(P1,O)).
generalize_atomic(I,O):- atom(I),!, freeze(O, atom(O)).

number_or_ratio(NR):- (number(NR);rational(NR)),!.
atomic_type(number_or_ratio).
atomic_type(string).
atomic_type(is_fg_color). atomic_type(is_bg_color).
atomic_type(is_nc_point). atomic_type(is_cpoint).
atomic_type(is_color).
atomic_type(atom).
atomic_type(is_list).

generalize_term(I,O):- nonvar(O),!,generalize_term(I,OO),O=OO.
generalize_term(I,O):- compound_not_list(I), !, generalize_compound(I,O),nonvar(O).
generalize_term(I,O):- \+ compound(I),!, generalize_atomic(I,O).
generalize_term([H|T],O):- !, generalize_cons([H|T],O).

generalize_once(I,O):- plain_var(O), !, freeze(O,once(generalize(I,O))).
generalize_once(I,O):- generalize(I,O).
%generalize(I,O):- plain_var(I), !, freeze(I, generalize(I,O)).

generalize(I,O):- plain_var(I), !, (O=I ; O = _).
%generalize(I,O):- ( \+ compound(I) ), !, (O=I ; O = _).
%generalize(I,O):- ( \+ compound(I) ),!, generalize_atomic(I, O).
generalize([A|B],[AA|BB]):- !, generalize(A,AA),generalize_cons(B,BB).
generalize(_-P,_-P):-!.
generalize(I,O):- compound_not_list(I),!,generalize_compound(I,O). 
generalize(_,_).

compound_not_list(I):- compound(I), !, \+ is_list(I).

generalize_compound(iz(I),iz(O)):-!,generalize_term(I,O), nonvar(O).
generalize_compound(I,O):- term_variables(I,Vs), 
  functor(I,F,A),functor(O,F,A),
  compound_name_arguments(I,F,IA),
  compound_name_arguments(O,F,OA),!,
  generalize_arglist(IA,OA),
  term_variables(O,VsO), length(Vs,IL),length(VsO,OL), OL =< IL+1.

generalize_arg(I,O):- compound_not_list(I),!,(generalize_compound(I,O);I=_).
generalize_arg(I,O):- O=I ; O = _.

generalize_arglist([],[]). 
generalize_arglist([I],[O]):- !, generalize_arg(I,O).
generalize_arglist([A,B],[AA,BB]):- !, generalize_arg(A,AA),generalize_arg(B,BB).

generalize_arglist(List,Loose):- findall(E,generalize_arglist3(List,E),Pos),
  sort_looseness(Pos,Gens),!,member(Loose,Gens).

%sort_looseness(Pos,Pos):-!.
sort_looseness(Pos,Gens):- predsort(using_compare(var_count),Pos,Gens).

sub_term_or_e(E,List):- is_list(List),!,member(EE,List),sub_term_or_e(E,EE).
sub_term_or_e(E,List):- sub_term(E,List).

count_of(A,P1, NEG):- nonvar(NEG), NEG = -N, !, count_of(A,P1,NN), N is -NN.
count_of(A,P1,N):- findall(E,(sub_term_or_e(E,A),call(P1,E)),L),length(L,N).
count_sum_of(A,P2,N):- findall(EN,(sub_term_or_e(E,A),call(P2,E,EN)),L),sum_list(L,N).

var_count(Term,N):- compound_not_list(Term),compound_name_arguments(Term,_,List),!,var_count(List,N).
var_count(Term,N):- var(Term),!,N = term(Term,Term,Term).
var_count(Term,N):- \+ is_list(Term), N = Term,!.
var_count(Term,N):- 
 count_of(Term,is_colorish,-NCC), count_of(Term,atom,-AC), count_of(Term,compound,-CC),
 count_of(Term,number_or_point,-NC), count_of(Term,ground,-GC), count_of(Term,var,VC),
 include(plain_var,Term,AVM),length(AVM,FA),
 term_variables(Term,Vs),length(Vs,VsL),
 copy_term(Term,GTerm,_), numbervars(GTerm,0,_,[attvars(bind),singletons(true)]), 
 N = (FA+NCC+AC+VsL+NC+VC+GC+CC+GTerm).

number_or_point(O):- number(O).
number_or_point(O):- is_nc_point(O).

generalize_arglist3([A,B,C],[AA,BB,CC]):- generalize_arg(A,AA), generalize_arg(B,BB),generalize_arg(C,CC).
generalize_arglist3([A,B,C,D],[AA,BB,CC,DD]):- generalize_arg(A,AA), generalize_arg(B,BB),generalize_arg(C,CC),generalize_arg(D,DD).
generalize_arglist3([A|B],[AA|BB]):- generalize_arg(A,AA),generalize_arglist(B,BB).
generalize_arglist3(L1,L2):- length(L1,N),length(L2,N).

generalize_cons(I,O):- plain_var(I), !, (O=[];O=_).
generalize_cons([],O):- !, (O=[];O=_).
generalize_cons([A|B],[AA|BB]):- !, generalize(A,AA),generalize_cons(B,BB).


combine_diffs([],D,D):-!.
combine_diffs(D,[],D):-!.
combine_diffs(nc([]),D,D):-!.
combine_diffs(D,nc([]),D):-!.
combine_diffs(D,D,D):-!.
combine_diffs(D1,D2,L12):- listify(D1,L1),listify(D2,L2),!,append(L1,L2,L12).


maybe_reorder_pair(A2,B2,A3,B3):- 
  (A2\==[];B2\==[]),
  is_group(A2),is_group(B2),
  once(final_alignment(A2,B2,A2,B2,A3,B3)),(A2\==A3;B2\==B3),!.

final_alignment(_AR,_BR,[],[],[],[]):-!.
final_alignment(AR,BR,AAR,BBR,[PA|G1],[PB|G2]):- %fail,
  select_obj_pair_1_3(AAR,BBR,PA,PB,_),
  select(PA,AAR,AA), select(PB,BBR,BB),
  !, final_alignment(AR,BR,AA,BB,G1,G2).
%final_alignment([],[],AA,BB,AA,BB):-!.
final_alignment(AR,BR,AAA,BBR,[PA|G1],[PB|G2]):- BBR\==[],
  select_obj_pair_1_3(BBR,AR,PB,PA,_),  
  %select(PA,AR,AA), 
  select(PB,BBR,BB),
  !, final_alignment(AR,BR,AAA,BB,G1,G2).

final_alignment(AR,BR,AAR,[],[PA|G1],[PB|G2]):- AAR\==[],
  select_obj_pair_1_3(AAR,BR,PA,PB,_),
  select(PA,AAR,AA), 
  %select(PB,BR,BB),
  !, final_alignment(AR,BR,AA,[],G1,G2).

final_alignment(_,_,AA,BB,AA,BB):-!.




showdiff_groups(AG,BG):- \+ is_group(AG),into_group(AG,AGL),!,showdiff_groups(AGL,BG).
showdiff_groups(AG,BG):- \+ is_group(BG),into_group(BG,BGL),!,showdiff_groups(AG,BGL).
showdiff_groups(AG,BG):- not_list(AG),into_list(AG,AGL),!,showdiff_groups(AGL,BG).
showdiff_groups(AG,BG):- not_list(BG),into_list(BG,BGL),!,showdiff_groups(AG,BGL).
showdiff_groups(AG,BG):- ignore((once((proportional_how(AG,BG,DD), pp(cyan,proportional(DD)))))),fail.
showdiff_groups(AG,BG):-
  maplist(obj_grp_comparable,AG,A3),
  maplist(obj_grp_comparable,BG,B3),
  final_alignment(AG,BG,A3,B3,A4,B4),
  length(A3,LenA3),length(A4,LenA4),
  length(B3,LenB3),length(B4,LenB4),
  ignore((LenA3>0,dash_chars, print_list_of( inputUniqs=LenA3/LenA4,A3))),
  ignore((LenB3>0,dash_chars, print_list_of(outputUniqs=LenB3/LenB4,B3))),
  dash_chars,
  diff_groups1a(A3,B3,A4,B4,Diffs),
  print_list_of(showdiff_objects,showdiff_objects,Diffs),
  dash_chars,
  !.

pair_up(F,A,B,G):- G=..[F,A,B].

showdiff_groups5(A,B,[],A,B):-!.
showdiff_groups5(A,B,[H|T],AAR,BBR):- !,
  showdiff_groups5(A,B,H,A1,B1),
  showdiff_groups5(A1,B1,T,AAR,BBR).
showdiff_groups5(A,B,Pred,AAR,BBR):- 
  pred_intersection(Pred,A,B,IntersectA,IntersectB,AAR,BBR),
  ignore((IntersectA\==[], collapsible_section(info,"Object Differences",true,
   maplist(showdiff_objects_vis(Pred),IntersectA,IntersectB)))).


diff_groups2(AAR,BBR,proportional(DD,Diffs)):- proportional(AAR,BBR,DD), maplist(diff_objects,AAR,BBR,Diffs).
diff_groups0(A3,B3,DD):- diff_groups2(A3,B3,DD).
diff_groups0(A3,B3,DD):- diff_groups1(A3,B3,DD).

diff_groups(A0,B0,DD):- 
  maplist(obj_grp_comparable,A0,A2),
  maplist(obj_grp_comparable,B0,B2),
  diff_groups1(A2,B2,DD).


obj_atoms(PA,PAP):- must_det_ll((nonvar(PA),obj_plist(PA,M),M\==[],
  findall(E,(member(SE,M),sub_obj_atom(E,SE)),PAP),PAP\==[])),!.

obj_plist(PA,PAP):- is_list(PA),!,PAP=PA.
obj_plist(obj(PA),PAP):- is_list(PA),!,PAP=PA.
%obj_plist(obj(PA,PAP):- indv_props(PA,PAP),!.

sub_obj_atom(M,M):- var(M),!,fail.
%sub_obj_atom(M,M):- attvar(M),!.
%sub_obj_atom(A,A).
sub_obj_atom(M,M):- \+ compound(M),!.
sub_obj_atom(M,M).
%sub_obj_atom(A,M):- M = localpoints(_),!,A=M.
%sub_obj_atom(iz(A),iz(A)):-!. % sub_obj_atom(A,M).
sub_obj_atom(A,M):- M=..[F,List],is_list(List),!,member(E,List),A=..[F,E].
sub_obj_atom(E,M):- sub_term(E,M),E\==M,compound(E),once((arg(_,E,A), atomic(A))).

select_obj_pair_2(AAR,BBR,PA,PB,Why):- 
 AAR\==[],
 BBR\==[],
 prop_atoms(H/O,Best,PA,PB) = Why,
 findall(Why,
  (member(PA,AAR), 
   member(PB,BBR), 
     must_det_ll((
   % maybe_allow_pair(PA,PB), allow_pair(PA,PB),
     obj_atoms(PA,PAP), 
     obj_atoms(PB,PBP),
     intersection(PAP,PBP,Joins,_,Other),
     length(Joins,H),length(Other,O)))),
   Pairs), 
 
 sort(Pairs,SPairs),reverse(SPairs,RPairs),!,last(RPairs,pair(Best,_,_,_)),
   member(Why,RPairs).

select_obj_pair_1(AAR,BBR,PA,PB,Why):- fail,
 member(PA,AAR), get_selector(PA,PB), 
 copy_term(PB,Why),
 member(PB,BBR). %, maybe_allow_pair(PA,PB).

select_obj_pair_1_2(AAR,BBR,PA,PB,Why):- 
 pair(PA,PB,Why) = Pair,
 findall(Pair,select_obj_pair_1(AAR,BBR,PA,PB,Why),PairList1),
 findall(Pair,select_obj_pair_2(AAR,BBR,PA,PB,Why),PairList2),
 pred_intersection(same_pairs,PairList1,PairList2,Ret1a,_Ret1b,Ret2,Ret3),
 append([Ret1a,Ret2,Ret3],Rets),
 member(Pair,Rets).

same_pairs(AB,pair(A,B,Y2)):- AB = pair(A,B,Y1), nb_setarg(3,AB,Y1+Y2),!.

compare_objs_mask([perfect]).
compare_objs_mask([turned,+loc]).
compare_objs_mask([turned,-loc]).
compare_objs_mask([moved]).
%compare_objs_mask([sameO]).

select_obj_pair_1_3(AAR,BBR,PA,PB,compare_objs1(How)):- fail, compare_objs_mask(How), member(PA,AAR), member(PB,BBR), compare_objs1(How,PA,PB).
select_obj_pair_1_3(AAR,BBR,PA,PB,Why):-   select_obj_pair_1_2(AAR,BBR,PA,PB,Why), nop(allow_pair(PA,PB)).
select_obj_pair_1_3(AAR,BBR,PA,PB,maybe_good_prop(PAP,PBP)):-    maybe_good_prop(PAP,PBP), member(PA,AAR), has_prop(PA,PAP), member(PB,BBR),  allow_pair(PA,PB), has_prop(PB,PBP),!.
%select_obj_pair_1_3(AAR,BBR,PA,PB):-   select_obj_pair_1_2(AAR,BBR,PA,PB), \+ allow_pair(PA,PB),!.
%select_obj_pair_1_3(AAR,BBR,PA,PB):-   member(PA,AAR), member(PB,BBR), allow_pair(PA,PB),!.
%select_obj_pair_1_3(AAR,BBR,PA,PB):-   maybe_good_prop(PA,PB), member(PA,AAR), member(PB,BBR), \+ allow_pair(PA,PB),!.
%select_obj_pair_1_3(AAR,BBR,PA,PB):-   member(PA,AAR), member(PB,BBR), \+ allow_pair(PA,PB),!.

maybe_good_prop(o(How,LFN,O),o(How,LFN,O)).
maybe_good_prop(o(How,LFN,_),o(How,LFN,_)).
maybe_good_prop(pen([Color1|_]),pen([Color2|_])):- prop_color(Color1,Color2).
maybe_good_prop(pen([_,Color1|_]),pen([_,Color2|_])):- prop_color(Color1,Color2).
maybe_good_prop(pen([_,Color1|_]),pen([Color2|_])):- prop_color(Color1,Color2).
maybe_good_prop(pen([Color1|_]),pen([_,Color2|_])):- prop_color(Color1,Color2).
maybe_good_prop(A,A):- maybe_good_prop1(A).
maybe_good_prop(o(How,_,_),o(How,_,_)).
maybe_good_prop1(v_hv(_,_)).
maybe_good_prop1(loc(_,_)).
maybe_good_prop1(iz(poly(_))).
maybe_good_prop1(birth(_)).
maybe_good_prop1(iz(locY(_))).
maybe_good_prop1(iz(locX(_))).
prop_color(A,B):- var(B),!,freeze(B,prop_color(A,B)).
prop_color(C,C).

%maybe_allow_pair(PA,PB):- PA=@=PB,!,fail.
maybe_allow_pair(PA,PB):- never_pair(PA,PB),!,fail.
%maybe_allow_pair(PA,PB):- ((has_prop(PA,mass(1));has_prop(PB,mass(1)))),!,fail.
allow_pair(PA,PB):-  \+ never_pair(PA,PB).

never_pair(PA,PB):- PA=@=PB.
never_pair(PA,PB):- never_pair_r(PA,PB),!.
never_pair(PA,PB):- never_pair_r(PB,PA),!.
never_pair_r(PA,PB):- has_prop(pen([_-black]),PA),has_prop(pen([_,_|_]),PB).
%never_pair_r(PA,PB):- has_prop(chromatic(0),PA), \+ has_prop(chromatic(0),PB).


diff_groups1(AAR,BBR,DD):- maybe_reorder_pair(AAR,BBR,AAR2,BBR2),!,  diff_groups1a(AAR,BBR,AAR2,BBR2,DD).
diff_groups1(AAR,BBR,DD):- diff_groups1a(AAR,BBR,AAR,BBR,DD).

diff_groups1a(_OA,_OB,[],[],[]):-!.
diff_groups1a(_OA,_OB,[],B,right_over(BO)):- maplist(object_dglyphH,B,BO).
diff_groups1a(_OA,_OB,B,[],left_over(BO)):- maplist(object_dglyphH,B,BO).
diff_groups1a(OA,OB,AAR,BBR,DD):-
  select_obj_pair_1_3(AAR,BBR,PA,PB,Why),
  diff_objects(PA,PB,DAB,Same),
  (DAB == [] -> D = [] ;  
     (nop(showdiff(PA,PB)),
      object_dglyphH(PA,GA), 
      object_dglyphH(PB,GB),
      D = change_obj(Why,GA,GB,Same,DAB))),
  select(PA,AAR,AA), select(PB,BBR,BB),
  diff_groups1a(OA,OB,AA,BB,D1),
  combine_diffs(D1, D , DD),!.

diff_groups1a(OA,OB,[PA|AA],[PB|BB],DD):-
  diff_objects(PA,PB,DAB,Same),
  (DAB == [] -> D = [] ;  
     (%(showdiff(PA,PB)),
      object_dglyphH(PA,GA), 
      object_dglyphH(PB,GB),
      D = change_obj(list_ordered,GA,GB,Same,DAB))),  
  diff_groups1a(OA,OB,AA,BB,D1),
  combine_diffs(D1, D , DD),!.

diff_groups1a(_OA,_OB,A,B,[disjointed(SharedT,AOnlyT,BOnlyT)]):- 
  intersection(A,B,Shared,AOnly,BOnly),
  tersify_cheap(Shared,SharedT),
  tersify_cheap(AOnly,AOnlyT),
  tersify_cheap(BOnly,BOnlyT).

tersify_cheap(I,O):- tersify(I,O),!.


object_dglyphH(PA,objFn(GA,loc(X,Y),ROT,Pen,ShapeID)):- 
  obj_to_oid(PA,GA),% mass(PA,Mass),
  shape(PA,Shape),pen(PA,Pen),loc(PA,X,Y), rotation(PA,ROT),
  shape_id(Shape,ShapeID).
 

object_dglyphH(PA,GA):- object_dglyph(PA,GA).

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
%uncomparable2(group,grid_size).
uncomparable2(group,obj_to_oid).
%uncomparable2(group,link).
uncomparable2(object,iz).
uncomparable2(shape,localpoints).

never_show_diff(V):- var(V),!,fail.
never_show_diff(_):- nb_current(diff_porportional,t),!,fail.
%never_show_diff(o).
never_show_diff(link).
%never_show_diff(iz(A)):- atomic(A).
never_show_diff(iz(g(_))).
never_show_diff(obj_to_oid).
never_show_diff(change).
%never_show_diff(birth).
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
obj_make_comparable(obj(I),O):- !, obj_make_comparable(I,O).
obj_make_comparable(I,O):- is_list(I),maplist(obj_make_comparable_e,I,M),sort_obj_props(M,O),
 nop(pp(sort_obj_props(O))).
obj_make_comparable(I,O):- into_obj(I,M),obj_make_comparable(M,O).
%obj_make_comparable_e(I,O):- is_list(I),sort(I,O).
%obj_make_comparable_e(Comp,F):- compound(Comp),functor(Comp,F,_),f_uncomparable_e(F).
%obj_make_comparable_e(grid_size(_,_),grid([])).
%obj_make_comparable_e(I,O):- I=..[F,L],maye_sort(L,S),O=..[F,S].
obj_make_comparable_e(I,I).
%obj_make_comparable(I,SI):- exclude(uncomparable,I,II), sort(II,SI).

% f_uncomparable_e(F).
f_uncomparable_e(grid).
%f_uncomparable_e(birth):- writeq(b).
%f_uncomparable_e(grid_size).
%f_uncomparable_e(iz).
%diff_objects(I,O,OUT):- !, fail, locally(set_prolog_lfag(gc,false),compute_diff_objs1(I,O,OUT)).
:- style_check(-singleton).

diff_objects(I,O,DiffsS):-  diff_objects(I,O,DiffsS,_Intersect).
diff_objects(I,O,DiffsS,Intersect):-    
  obj_make_comparable(I,II), obj_make_comparable(O,OO),!,
  intersection(II,OO,Intersect,IIR,OOR),!,
  findall(Diff, 
    (member(P,IIR),
      once((generalize(P,Q), member(Q,OOR))),
      once(diff_terms(P,Q,Diff))),Diffs),
   flatten([Diffs],DiffsF),list_to_set(DiffsF,DiffsS).

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

overlap_same_obj(I,O):- compare_objs1(sameO,I,O).

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
%overlap_same_obj(I,O):- compare_objs1(sameO,I,O).

showdiff_objects(A,B):- into_obj(A,A1),into_obj(B,B1), !, showdiff_objects_n(sameness,A1,B1),!.

showdiff_objects_vis(N,O1,O2):- showdiff_objects_n(vis(N),O1,O2).


showdiff_objects_n(N,O1,O2):- 
  diff_objects(O1,O2,Diffs,Sames), 
  showdiff_objects5(N,O1,O2,Sames,Diffs).

%showdiff_objects_n(N,O1,O2,[]):- print_list_of(N,[O1,O2]),!.
showdiff_objects(change_obj(N,O1,O2,Sames,Diffs)):- 
 showdiff_objects5(N,O1,O2,Sames,Diffs),!.
showdiff_objects(XY):- pp(showdiff_objects(XY)),!.

showdiff_objects5(Why,OO1,OO2,Sames,Diffs):- 
 must_det_ll((
  into_obj(OO1,O1),into_obj(OO2,O2),
  dash_chars,dash_chars,
  object_grid_to_str(O1,Str1,T1),
  object_grid_to_str(O2,Str2,T2),
  ppt(Why),
  ((global_grid(O1,OG1),global_grid(O2,OG2),print_side_by_side(teal,OG1,global_grid(T1),_,OG2,diff(T2)))-> true;
   (print_side_by_side(yellow,O1,no_global_grid(T1),_,O2,diff(T2)))),
  ignore((into_ngrid(O1,NO1),into_ngrid(O2,NO2), print_side_by_side(silver,NO1,ngrid(T1),_,NO2,ngrid(T2)))),
  dash_chars,
  writeln(Str1), 
  debug_indiv(O1),
  writeln(Str2),
  debug_indiv(O2),
  %print_list_of(debug_indiv,showdiff_objects_n(Why),[O1,O2]), 
 % print_list_of(debug_as_grid,showdiff_objects_n(Why),[O1,O2]),  
  findall(E,compare_objs1(E,O1,O2),L), pp(compare_objs1(showdiff_objects)=L),  
  dash_chars,dash_chars,
  noisey_debug(print_list_of(print_sames,sames,Sames)),
  include(fun_diff,Diffs,FunDiffs),
  print_list_of(print_diffs,diffs,FunDiffs),
  dash_chars,dash_chars)).

print_sames(N):- format('\t'), pp_no_nl(N).
print_diffs(N):- format('\t'), pp_no_nl(N).

excl_diff(C):- var(C),!,fail.
excl_diff(diff(A->_)):- !, excl_diff(Ap).
excl_diff(C):- compound(C),!, compound_name_arity(C,F,_),!,excl_diff(F).
excl_diff(localpoints).
excl_diff(shape).
fun_diff(P):- \+ excl_diff(P).

compare_objs1(_,I,O):- I==O,!,fail.
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

sprop(sameO).
sprop(moved).
sprop(turned).
sprop(X):- prop_of(X,_).
sprop(perfect).


%sprop(perfect).

sprop_of(sameO,visually).
sprop_of(sameO,size).
sprop_of(sameO,shape).
sprop_of(sameO,colors).

sprop_of(moved,sameO).
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


  %pp(remove_sames(II,OO)),
%  select(C,IIR,IIR2),compound(C),generalize(C,M),select(M,OOR,OOR2),
  %trace,diff_terms(IIR,OOR,OUT),!.
  
/*
  append(L,[A|R],II),append(L,[B|R],OO),
  diff_lists(A,B,Diffs),
  (simular(Diffs,obj(I),obj(O),Sameness) ->  OUT = same_object(Sameness) ; OUT = props_diff(obj(I),obj(O),Diffs)).
*/

needs_indivs(I,_):- is_object(I),!,fail.
needs_indivs(globalpoints(O),O):- !.
needs_indivs(I,O):- is_gridoid(I), \+ is_group(I),!, globalpoints_maybe_bg(I,O),!,I\==O.
%needs_indivs(I,O):- is_grid(I),_unshared_indivs(I,O),!.
needs_indivs(I,O):- is_gridoid(I), \+ is_group(I), arcST, trace, compute_unshared_indivs(I,O),!.

%diff_terms(IPs,OPs,Difs2):- diff_terms(IPs,OPs,Difs2).

diff_terms(I,O,D):- diff_termz(I,O,D),!.
diff_terms(I,O,diff(I->O)).

diff_termz(I,O,D):- nonvar_or_ci(D),!,diff_terms(I,O,DD),D=DD.
diff_termz(I,O,[]):- O=@=I,!.
diff_termz(I,O,[]):- O==I,!.
diff_termz(I,O,[]):- plain_var(I),plain_var(O),!.
diff_termz(I,O,O):- plain_var(I),!.
diff_termz(O,I,O):- plain_var(I),!.
diff_termz(I,O,D):- maybe_number(I,N1),maybe_number(O,N2),!,proportional_size(N1,N2,D).
diff_termz(I,O,O):- I==[],!.
diff_termz(I,O,I):- O==[],!.

diff_termz([IH,IV],[OH,OV],D):- maplist(number,[IH,IV,OH,OV]),!,maplist(diff_numbers,[IH,IV],[OH,OV],D).

%diff_termz(I,O, [] ):- (never_do_diff(I);never_do_diff(O)),!.
diff_termz(shape(I),shape(O),[]):- !,sort(I,II),sort(O,OO),II=@=OO,!.
diff_termz(shape(I),shape(O),shape(diff(I->O))):-!.
%diff_termz(I,O, (O \== I)):- O=@=I,!.
diff_termz(group_o(I),group_o(O),group_o(DD)):- !, must_det_ll(diff_groups(I,O,DD)).
diff_termz(I,O,DD):-  is_group(I), is_group(O), !, must_det_ll(diff_groups(I,O,DD)).
diff_termz(I,O,[]):- no_diff(I,O),!.
diff_termz(O,I,[]):- no_diff(I,O),!.
% diff_termz(I,O,DD):-  is_group(I),is_group(O), !,  include_fav_points(I,II), include_fav_points(O,OO), diff_groups(I,O,DD).

%diff_termz(obj(I),obj(O),OUT):- !, diff_objects(I,O,OUT).

diff_termz(I,O, D):- non_grid_list(I),non_grid_list(O),!,diff_lists(I,O,D).

diff_termz(I,O,D):- is_map(I),!,findall(D1,(get_kov(K, I, V),diff_terms(K=V,O,D1)),D).
diff_termz(IF=IA,O,IF=D):- find_kval(O,IF,OA),!,diff_terms(IA,OA,D).


diff_termz(Grid,Other,OUT):- needs_indivs(Grid,I),!,diff_termz(I,Other,OUT).
diff_termz(Other,Grid,OUT):- needs_indivs(Grid,I),!,diff_termz(Other,I,OUT).
diff_termz(I,O,D):- compound(I),compound(O),!,diff_compounds(I,O,D).



diff_compounds(I,O, [] ):- (never_show_diff(I);never_show_diff(O)),!.

diff_compounds(I,O,D):- compound_name_arguments(I,IF,IA),compound_name_arguments(O,OF,OA),
  maplist(compute_diff_or_same,IA,OA,DA),
  diff_compounds(I,IF,O,OF,DA,D).

diff_compounds( I, F,_O, F, DA, D):- I=@=DA, D=[].
diff_compounds(_I, F,_O, F, DA, D):- !, compound_name_arguments(D,F,DA).
diff_compounds(I,_IF,O,_OF,_DA, diff(I->O)).


compute_diff_or_same(I,O,IO):- 
  diff_terms(I,O,D),
  maybe_no_diff(I,O,D,IO).

maybe_no_diff(I,_,[],I):-!.
maybe_no_diff(_,_,D,D).

is_object_props(O):- is_list(O),member(E,O),compound(E),shape(_)=E,!.
diff_lists(AA,BB,D):- AA=@=BB,!,D=[].
diff_lists(AA,BB,diff(AA=@=BB)):- sort(AA,A),sort(BB,B), A=@=B,!.
diff_lists(I,O,D1D):- is_kv_list(I),is_kv_list(O),!,kv_list_diff(_Sytle,I,O,D1D).
diff_lists(I,O,D1D):- is_object_props(I),is_object_props(O),!,object_props_diff(_Sytle,I,O,D1D).
diff_lists(AA,BB,D):- must_det_ll((non_grid_list(AA), non_grid_list(BB), =(AA,A),=(BB,B), length(A,AL), length(B,BL))),
 must_det_ll((AL>BL,fail) -> list_diff_recurse(B,A,D) ; list_diff_recurse(A,B,D)).

select_two_0(I,O,CI,CO,II,OO):- select_two_any(I,O,CI,CO,II,OO), CO==CI.
select_two_0(I,O,CI,CO,II,OO):- select_two_any(I,O,CI,CO,II,OO), CO=@=CI.
select_two_1(I,O,CI,CO,II,OO):- select(CI,I,II), compound_not_list(CI), generalize_compound(CI,CO), \+ plain_var(CO), select(CO,O,OO),be_comparable(CI,CO).
select_two_1(I,O,CI,CO,II,OO):- select(CI,I,II), generalize_term(CI,CO), \+ plain_var(CO), select(CO,O,OO),be_comparable(CI,CO).
select_two_2(I,O,CI,CO,II,OO):- select(CI,I,II), generalize_term(CI,CO),  select(CO,O,OO), be_comparable(CI,CO).
select_two_3(I,O,CI,CO,II,OO):- select_two_any(I,O,CI,CO,II,OO), be_comparable(CI,CO),!.
select_two_any(I,O,CI,CO,II,OO):- select(CI,I,II), select(CO,O,OO).

is_kv_list([C|_]):- compound(C),functor(C,(-),2).
select_two(I,O,CI,CO,II,OO):- select_two_0(I,O,CI,CO,II,OO),!.
select_two(I,O,CI,CO,II,OO):- select_two_1(I,O,CI,CO,II,OO),!.
select_two(I,O,CI,CO,II,OO):- select_two_1(O,I,CO,CI,OO,II),!.
select_two(I,O,CI,CO,II,OO):- select_two_2(I,O,CI,CO,II,OO).
select_two(I,O,CI,CO,II,OO):- select_two_2(O,I,CO,CI,OO,II),!.
select_two(I,O,CI,CO,II,OO):- select_two_3(I,O,CI,CO,II,OO).
select_two(I,O,CI,CO,II,OO):- select_two_3(O,I,CO,CI,OO,II),!.

be_comparable(CI,CO):- data_type(CI,TI),data_type(CO,TO),TI==TO,!.
be_comparable(CI,CO):- compound(CI),compound(CO),!,functor(CI,F,A),functor(CO,F,A).

is_nil(Nil):- Nil == [].

list_diff_recurse_nil(I,O,[]):- I==O,!.
list_diff_recurse_nil(I,O,[]):- I=@=O,!.
list_diff_recurse_nil(Nil,O,diff(Nil->O)):- is_nil(Nil),!.
list_diff_recurse_nil(O,Nil,diff(O->Nil)):- is_nil(Nil),!.

list_diff_recurse(I,O,D1D):- list_diff_recurse_nil(I,O,D1D),!.

list_diff_recurse(I,O,D1D):- is_object_props(I),is_object_props(O),!,object_props_diff(_Sytle,I,O,D1D).
list_diff_recurse(I,O,D1D):- select_two(I,O,CI,CO,II,OO),!,diff_terms(CI,CO,D1),!,
       list_diff_recurse(II,OO,D),!, combine_diffs(D1,D,D1D),!.
list_diff_recurse([CI|II],[CO|OO],D1D):- must_det_ll(diff_terms(CI,CO,D1)),!,
       list_diff_recurse(II,OO,D),!, combine_diffs(D1,D,D1D),!.

%kv_list_diff(Style,I,O,D1D):- select_two_0(I,O,CI,CO,II,OO),!,kv_list_diff(Style,II,OO,D1D).

select_two_kv(key,I,O,CI,CO,II,OO):- CI=C-_,CO=C-_, select_two_any(I,O,CI,CO,II,OO).
select_two_kv(value,I,O,CI,CO,II,OO):- CI=_-P1,CO=_-P1, select_two_any(I,O,CI,CO,II,OO).

select_two_props(_Style,I,O,CI,CO,II,OO):- select_two_any(I,O,CI,CO,II,OO),compound(I),functor(I,F,A),functor(O,F,A).
object_props_diff(_Sytle,I,O,D1D):- list_diff_recurse_nil(I,O,D1D),!.
object_props_diff(Style,I,O,D1D):- select_two_any(I,O,CI,CO,II,OO),CI=@=CO,!,object_props_diff(Style,II,OO,D1D).
object_props_diff(Style,I,O,D1D):- select_two_any(I,O,CI,CO,II,OO),CI=CO,!,object_props_diff(Style,II,OO,D1D).
object_props_diff(Style,I,O,D1D):- select_two_props(Style,I,O,CI,CO,II,OO),diff_terms(CI,CO,D1),!,
       object_props_diff(Style,II,OO,D),!, combine_diffs(D1,D,D1D),!.
object_props_diff(Style,[CI|II],[CO|OO],D1D):- must_det_ll(diff_terms(CI,CO,D1)),!,
       object_props_diff(Style,II,OO,D),!, combine_diffs(D1,D,D1D),!.


kv_list_diff(_Sytle,I,O,D1D):- list_diff_recurse_nil(I,O,D1D),!.
kv_list_diff(Style,I,O,D1D):- select_two_any(I,O,CI,CO,II,OO),CI=@=CO,!,kv_list_diff(Style,II,OO,D1D).
kv_list_diff(Style,I,O,D1D):- select_two_any(I,O,CI,CO,II,OO),CI=CO,!,kv_list_diff(Style,II,OO,D1D).
kv_list_diff(Style,I,O,D1D):- select_two_kv(Style,I,O,CI,CO,II,OO),diff_terms(CI,CO,D1),!,
       kv_list_diff(Style,II,OO,D),!, combine_diffs(D1,D,D1D),!.
kv_list_diff(Style,[CI|II],[CO|OO],D1D):- must_det_ll(diff_terms(CI,CO,D1)),!,
       kv_list_diff(Style,II,OO,D),!, combine_diffs(D1,D,D1D),!.

find_kval(OF=OA,OF,OA):- !.
find_kval(List,OF,OA):- is_list(List),member(E,List),nonvar_or_ci(E),find_kval(E,OF,OA).
find_kval(Dict,OF,OA):- is_map(Dict),get_kov(OF,Dict,OA).
find_kval(O,OF,OA):- compound(O),compound_name_arguments(O,OF,[OA]).
find_kval(obj(O),OF,OA):- !, find_kval(O,OF,OA).
find_kval(O,OF,OA):- compound(O),compound_name_arguments(O,OF,OA).


must_intersect_all(Indv,Points,NextScanPoints):-
   globalpoints(Indv,IndvPoints),
   unique_of_each(IndvPoints,Points,[],NextScanPoints),!.

unique_of_each(IndvPoints,Points,UniqueInvO,UniquePointsO):-
  remove_global_points(IndvPoints,Points,UniquePoints),
  remove_global_points(Points,IndvPoints,UniqueInv),!,
  UniquePoints=UniquePointsO,
  UniqueInv=UniqueInvO.


count_difs(A,B,C):- is_grid(A),into_grid(B,BB), !, count_difs0(A,BB,C).
count_difs(B,A,C):- is_grid(A),into_grid(B,BB), !, count_difs0(A,BB,C).
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


proportional_types(set,A,B,D):- !, proportional_lists(A,B,D).
proportional_types(_How,A,B,D):- proportional(A,B,D).

proportional_type_list([],[],[],[]):-!.
proportional_type_list([L|Lst],[N1|NewLst1],[N2|NewLst2],[O|OutL]):- 
   proportional_types(L,N1,N2,O),!, 
   proportional_type_list(Lst,NewLst1,NewLst2,OutL).


maybe_label_colors(G,L):- is_grid(G),!,mapgrid(color_name,G,L),!,G\==L.

non_grid_list(X):- is_list(X), \+ is_grid(X).

unused_proportion(_,_,_):- \+ nb_current(allow_unused_proportion,t),!,fail.
unused_proportion(Obj1,Obj2,Obj3):- unused_proportion1(Obj1,Obj2,Obj3),!.

unused_proportion1( Obj1,Obj2,Obj1):- Obj1=@=Obj2.
unused_proportion1(Obj1,_Obj2,Obj1):- var(Obj1),!.
unused_proportion1(_Obj1,Obj2,Obj2):- var(Obj2),!.


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
  (decl_pt(prop_g,P1P2);decl_pt(prop_o,P1P2)), 
  P1P2=..[P2,P1|Lst],
  once((
  once((once((on_x_log_and_fail(call(P1,Obj1)),
              on_x_log_and_fail(call(P1,Obj2)))),
  length(Lst,Len), length(NewLst1,Len),length(NewLst2,Len),
  once((on_x_log_and_fail(apply(P2,[Obj1|NewLst1])),
        on_x_log_and_fail(apply(P2,[Obj2|NewLst2])))))),
  proportional_type_list(Lst,NewLst1,NewLst2,OutL))), 
  Out =.. [P2|OutL].

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
maybe_number(M,N):- extract_vals(M,Vals),!,last(Vals,N),number(N),!.

into_vals(V1,V2,Vals):- extract_vals(V1,VV1),extract_vals(V2,VV2),append(VV1,VV2,Vs),sort(Vs,Vals).

%extract_vals(M,Vals):- var(M),!,Vals=M.
%extract_vals(M,Vals):- \+ compound(M),!,Vals=[M].
extract_vals(M,Vals):- maybe_extract_values(M,Vals),!.
extract_vals(M,N):- M=..[_,A],maybe_extract_values(A,N).
extract_vals(V2,[V2]).

maybe_extract_values(Color,Values):- must_be_free(Values), compound(Color), Color=..[DF,vals(Values)|_],diff_f(DF),!,is_list(Values),!.
maybe_extract_value(Color,Value):- maybe_extract_values(Color,Values),!,member(Value,Values).

proportional_size(N1,N2,P):- unused_proportion(N1,N2,P),!.
proportional_size(M1,M2, num(vals([M1,M2]),+N,r(R))):- maybe_number(M1,N1),maybe_number(M2,N2), N is N2-N1, 
  catch(R is rationalize(N1/N2),_,true).


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
  intersection(L1,L2,Shared,IOnlyC,OOnlyC), 
  maplist(length,[L1,L2,IOnlyC,Shared,OOnlyC],Lens),
  into_vals(L1,L2,Vals),
  list_to_set(Shared,SharedS),
  diff_lists(IOnlyC,OOnlyC,Diff),
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
   In IEEE 754 arithmetic, a div +0 is positive infinity when a is positive, negative infinity when a is negative, 
   and NaN when a = +/-0. The infinity signs change when dividing by -0 instead.
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

%:- decl_pt(prop_g,each_object(is_grid, set)).

%each_object(_Grid,[]):-!.
each_object(Grid,ListO):- arc_memoized(individuate(complete,Grid,List)),!, simplify_objs(List,ListO).
%each_object(Grid,ListO):- print_collapsed(100,memoized(individuate(complete,Grid,List))),!, simplify_objs(List,ListO).

simplify_objs(I,O):-is_list(I),!,maplist(simplify_objs,I,O).
simplify_objs(obj(I),obj(O)):-!,simplify_objs(I,M),include(compound,M,M1),sort_obj_props(M1,M2),M2=O.
%simplify_objs(iz(g(_)),iz(g(_))).
%simplify_objs(Comp,F):- compound(Comp),functor(Comp,F,_),uncomparable(group,Comp),!.
simplify_objs(F,F).

prefer_grid(G):- is_object_or_grid(G).

:- decl_pt(prop_g,unique_colors(prefer_grid, set)).
:- decl_pt(prop_g,mass(is_object_or_grid,number)).

:- decl_pt(prop_g,center_term(is_object,loc)).
:- decl_pt(prop_g,loc_term(is_object,loc)).

:- decl_pt(prop_g,has_y_rows(is_grid,colcount,color,set(rownums))).
:- decl_pt(prop_g,has_x_columns(is_grid,rowcount,color,set(colnums))).


:- fixup_exports.

