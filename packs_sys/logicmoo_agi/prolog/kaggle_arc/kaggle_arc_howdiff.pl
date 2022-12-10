/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

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
generalize_atomic(I,O):- atomic_type(P1), is_type_call(P1,I),!, freeze(O, is_type_call(P1,O)).
generalize_atomic(I,O):- atom(I),!, freeze(O, atom(O)).

number_or_ratio(NR):- (number(NR);rational(NR)),!.
atomic_type(number_or_ratio).
atomic_type(string).
atomic_type(is_fg_color). atomic_type(is_bg_color).
atomic_type(is_nc_point). atomic_type(is_cpoint).
atomic_type(is_color).
atomic_type(atom).
atomic_type(is_list).

generalize_term(I,O):- var(I),!,O=I.
generalize_term(iz(I),iz(O)):-!,generalize_term(I,O), nonvar(O).
generalize_term(g(I),g(O)):-!,generalize_term(I,O), nonvar(O).
generalize_term(I,O):- nonvar(O),!,generalize_term(I,OO),O=OO.
generalize_term(I,O):- compound_not_list(I), !, generalize_compound(I,O),nonvar(O).
generalize_term(I,O):- \+ compound(I),!, generalize_atomic(I,O).
generalize_term([H|T],O):- !, generalize_cons([H|T],O).

generalize_once(I,O):- plain_var(O), !, freeze(O,once(generalize(I,O))).
generalize_once(I,O):- generalize(I,O).
%generalize(I,O):- plain_var(I), !, freeze(I, generalize(I,O)).

/*
generalize(I,O):- plain_var(I), !, (O=I ; O = _).
%generalize(I,O):- ( \+ compound(I) ), !, (O=I ; O = _).
%generalize(I,O):- ( \+ compound(I) ),!, generalize_atomic(I, O).
generalize([A|B],[AA|BB]):- !, generalize(A,AA),generalize_cons(B,BB).
generalize(_-P,_-P):-!.
generalize(I,O):- compound_not_list(I),!,generalize_compound(I,O). 
generalize(_,_).
*/
generalize(I,O):- generalize_term(I,O).

compound_not_list(I):- compound(I), !, \+ is_list(I).

generalize_compound(iz(I),iz(O)):-!,generalize_term(I,O), nonvar(O).
generalize_compound(g(I),g(O)):-!,generalize_term(I,O), nonvar(O).
generalize_compound(I,O):- term_variables(I,Vs), 
  functor(I,F,A),functor(O,F,A),
  compound_name_arguments(I,F,IA),
  compound_name_arguments(O,F,OA),!,
  generalize_arglist(IA,OA),
  term_variables(O,VsO), length(Vs,IL),length(VsO,OL), OL =< IL+1.

%generalize_arg(I,_):- compound_not_list(I), I=..[F|Args],diff_f(F),!.
%generalize_arg(I,O):- compound_not_list(I),!,(generalize_compound(I,O);I=_).
generalize_arg(I,O):- generalize_term(O,I).

generalize_arglist([],[]):-!. 
generalize_arglist([I],[O]):- !, generalize_arg(I,O).
%generalize_arglist([A,B,C],[AA,BB,CC]):- !, generalize_arg(A,AA),generalize_arg(B,BB),generalize_arg(C,CC).
generalize_arglist(List,Loose):- 
 findall(E,generalize_arglist2(List,E),Pos),
  sort_looseness(Pos,Gens),!,member(Loose,Gens).

%sort_looseness(Pos,Pos):-!.
sort_looseness(Pos,Gens):- predsort_using_only(var_count,Pos,Gens),!.

sub_term_or_e(E,List):- is_list(List),!,member(EE,List),sub_term_or_e(E,EE).
sub_term_or_e(E,List):- sub_term(E,List).

count_of(A,P1, NEG):- nonvar(NEG), NEG = -N, !, count_of(A,P1,NN), N is -NN.
count_of(A,P1,N):- findall(E,(sub_term_or_e(E,A),is_type_call(P1,E)),L),length(L,N).
count_sum_of(A,P2,N):- findall(EN,(sub_term_or_e(E,A),call(P2,E,EN)),L),sum_list(L,N).

var_count(Term,N):- term_variables(Term,Vs),length(Vs,N),!.
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

generalize_arglist2([A,B],[AA,BB]):- generalize_arg(A,AA),generalize_arg(B,BB).
generalize_arglist2(I,O):- generalize_arglist3(I,O).

generalize_arglist3([A,B,C],[AA,BB,CC]):- generalize_arg(A,AA), generalize_arg(B,BB),generalize_arg(C,CC).
generalize_arglist3([A,B,C,D|E],[AA,BB,CC,DD|EE]):- generalize_arg(A,AA), generalize_arg(B,BB),generalize_arg(C,CC),generalize_arg(D,DD),generalize_arglist(E,EE).
%generalize_arglist3(L1,L2):- length(L1,N),length(L2,N).

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

obj_grp_atoms(A,[A,PA|Atoms]):- obj_grp_comparable(A,PA),obj_atoms(PA,Atoms).

find_obj_mappings(A,BG,OO):-
  obj_grp_atoms(A,AINFO),
  maplist(obj_grp_atoms,BG,BBR),
  find_obj_mappings2(AINFO,BBR,OO).

find_obj_mappings2([A,PA|PAP],BBR,pair4(A,PA,B,PB)):- 
   ord(NJ/O+JO+Joins,[PA,A],[PB,B]) = Why,
   findall(Why,
    (      
     member([B,PB|PBP],BBR),
      must_det_ll((
     % maybe_allow_pair(PA,PB), allow_pair(PA,PB),  
       intersection(PAP,PBP,Joins,OtherA,OtherB),     
       flatten([OtherA,OtherB],Other),
       length(Joins,J),length(Other,O),
       NJ is -J,
       JO is - rationalize(J/(O+1))))),
     Pairs), 
   sort(Pairs,RPairs),!,
   %maplist(writeln,Pairs),
   %last(RPairs,prop_atoms(Best,_,_,_)),
   % Best = pair(PA,PB,_),
   member(Why,RPairs).


showdiff_groups(AG,BG):- \+ is_group(AG),into_group(AG,AGL),!,showdiff_groups(AGL,BG).
showdiff_groups(AG,BG):- \+ is_group(BG),into_group(BG,BGL),!,showdiff_groups(AG,BGL).
showdiff_groups(AG,BG):- not_list(AG),into_list(AG,AGL),!,showdiff_groups(AGL,BG).
showdiff_groups(AG,BG):- not_list(BG),into_list(BG,BGL),!,showdiff_groups(AG,BGL).

showdiff_groups(AG,BG):- ignore((once((proportional_how(AG,BG,DD), pp(cyan,proportional(DD)))))),fail.

showdiff_groups(AG,BG):- showdiff_groups_new(AG,BG),!.   
showdiff_groups(AG,BG):- showdiff_groups_old(AG,BG),!.   

showdiff_groups_old(AG,BG):-
  maplist(obj_grp_comparable,AG,A3),
  maplist(obj_grp_comparable,BG,B3),
  final_alignment(AG,BG,A3,B3,A4,B4),
  length(A3,LenA3),length(A4,LenA4),
  length(B3,LenB3),length(B4,LenB4),
  Shown = shown([]),
  
  ignore((LenA3>0,dash_chars, print_list_of(indiv_show_pairs_input(AG,Shown,BG), inputUniqs=LenA3/LenA4,A3))),
  smallest_first(AG,SFA),print_grid(inputUniqs,SFA),

    
  ignore((LenB3>0,dash_chars, print_list_of(indiv_show_pairs_output(BG,Shown,AG),outputUniqs=LenB3/LenB4,B3))),
  smallest_first(BG,SFB),print_grid(outputUniqs,SFB),
  dash_chars,
  ignore((diff_groups1(A4,B4,Diffs),
 % print_list_of(showdiff_objects,showdiff_objects,Diffs),
  pp(Diffs))),
  dash_chars,
  !.

pair_up(F,A,B,G):- G=..[F,A,B].

showdiff_groups5(A,B,[],A,B):-!.
showdiff_groups5(A,B,[H|T],AAR,BBR):- !,
  showdiff_groups5(A,B,H,A1,B1),
  showdiff_groups5(A1,B1,T,AAR,BBR).
showdiff_groups5(A,B,Pred,AAR,BBR):- 
  pred_intersection(Pred,A,B,IntersectA,IntersectB,AAR,BBR),
  ignore((IntersectA\==[], collapsible_section(info,"Object Differences",false,
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

never_matom(localpoints(_)).
never_matom(shape(_)).
never_matom(o(_,_,_)).
never_matom(giz(_)).
never_matom(globalpoints(_)).
sub_obj_atom(_,M):- var(M),!,fail.
sub_obj_atom(_,M):- never_matom(M),!,fail.
%sub_obj_atom(M,M):- attvar(M),!.
%sub_obj_atom(A,A).
sub_obj_atom(M,M):- \+ compound(M),!.
sub_obj_atom(M,M).
%sub_obj_atom(A,M):- M = localpoints(_),!,A=M.
%sub_obj_atom(iz(A),iz(A)):-!. % sub_obj_atom(A,M).
sub_obj_atom(A,M):- M=..[F,List],is_list(List),!,member(E,List),A=..[F,E].
sub_obj_atom(E,M):- sub_term(E,M),E\==M,compound(E),once((arg(_,E,A), number(A))).

select_obj_pair_2(AAR,BBR,PA,PB,(J/O)):- 
 AAR\==[],
 BBR\==[],
 ord(NJ/O+JO+Joins,PA,PB) = Why,
 findall(Why,
  (member(PA,AAR), 
   member(PB,BBR), 
     must_det_ll((
   % maybe_allow_pair(PA,PB), allow_pair(PA,PB),
     obj_atoms(PA,PAP), 
     obj_atoms(PB,PBP),
     intersection(PAP,PBP,Joins,OtherA,OtherB),     
     flatten([OtherA,OtherB],Other),
     length(Joins,J),length(Other,O),
     NJ is -J,
     JO is - rationalize(J/(O+1))))),
   Pairs), 
 sort(Pairs,RPairs),!,
 %maplist(writeln,Pairs),
 %last(RPairs,prop_atoms(Best,_,_,_)),
 % Best = pair(PA,PB,_),
   member(Why,RPairs).

select_obj_pair_1(AAR,BBR,PA,PB,Why):- % fail,
 member(PA,AAR), get_selector(PA,PB), 
 copy_term(PB,Why),
 member(PB,BBR). %, maybe_allow_pair(PA,PB).

select_obj_pair_1_2(AAR,BBR,PA,PB,Why):- 
 pair(PA,PB,Why) = Pair,
 findall(Pair,select_obj_pair_1(AAR,BBR,PA,PB,Why),PairList1),
 findall(Pair,select_obj_pair_2(AAR,BBR,PA,PB,Why),PairList2),
 pred_intersection(same_pairs,PairList1,PairList2,Ret1a,_Ret1b,Ret2,Ret3),
 append([Ret1a,Ret3,Ret2],Rets),
 member(Pair,Rets).

same_pairs(AB,pair(A,B,Y2)):- AB = pair(A,B,Y1), nb_setarg(3,AB,Y1+Y2),!.

uniqueness_prop(symmetry(_)).
uniqueness_prop(mass(_)).


indiv_show_pairs_input(_Peers,_Shown,_List,Indv):- nb_current(menu_key,'o'),!, dg(Indv).
indiv_show_pairs_input(_Peers,_Shown,_List,Indv):- get_current_test(TestID), print_info(Indv), ignore(what_unique(TestID,Indv)).

indiv_show_pairs_output(_Peers,_Shown,_List,Indv):- nb_current(menu_key,'o'),!, dg(Indv).
%indiv_show_pairs_output(_Peers,_Shown,_List,Indv):- has_prop(pen([cc('black',_)]),Indv),!, dash_chars, nop(debug_as_grid(Indv)).
indiv_show_pairs_output(Peers,_Shown,List,Indv):-
  dash_chars,
  (best_mates(Indv,List,Mate)->showdiff_arg1("I<>O",Peers,Indv,List,Mate);dg(Indv)).

showdiff_groups_new(AG,BG):- 
 must_det_ll((
  maplist(obj_grp_atoms,AG,AGG),
  maplist(obj_grp_atoms,BG,BGG),
   print_list_of(show_mappings("IN -> OUT",AG,BG,BGG), inputMap,AGG),
   print_list_of(show_mappings("IN <- OUT",BG,AG,AGG),outputMap,BGG))).

show_mappings(TITLE,AG,BG,BGG,APA):-
 must_det_ll((
  dash_chars(100),nl,nl,nl,
  APA = [A,PA|_Atoms],
  find_obj_mappings2(APA,BGG,Pair),  
  Pair = pair4(A,PA,B,PB),
  % must_det_ll(A\==B),
  nop(PA\==PB),
  %%debug_as_grid('show_mappings',A),
  (TITLE == "IN <- OUT" 
    -> showdiff_arg1(TITLE,BG,B,AG,A)
     ; showdiff_arg1(TITLE,AG,A,BG,B)))).
  %showdiff_objects(PA,PB),!.


showdiff_arg1(TITLE,Peers1,Obj1,Peers2,Obj2):- 
 must_det_ll((
  findall(Peer,(nop(has_prop(o(X,_,Y),Obj1)),member(Peer,Peers1),has_prop(o(X,_,Y),Peer),Peer\==Obj1),Peers11),
  findall(Peer,(nop(has_prop(o(X,_,Y),Obj2)),member(Peer,Peers2),has_prop(o(X,_,Y),Peer),Peer\==Obj2),Peers22),
  objs_to_io(Obj1,Obj2,I1,O1),
  ((Obj1==I1) 
       -> (PeersI = Peers11,PeersO = Peers22) ; (PeersI = Peers22,PeersO = Peers11)))),
 must_det_ll((
 %link_prop_types(loc2D,I1,O1,_LOCS),
 show_pair_now(TITLE,I1,O1),
  %what_unique(TestID,O1),

 if_t(nb_current(menu_key,'u'),
 (
  indv_props(I1,S1),indv_props(O1,S2),
  get_current_test(TestID), ignore(what_unique(TestID,I1)),
  remove_giz(S1,T1),remove_giz(S2,T2),
  indv_u_props(I1,IU),indv_u_props(O1,OU),
  intersection(T1,T2,Sames,IA,OA),maplist(refunctor,Sames,NewSames),
  object_props_diff(IA,OA,Diffs), listify(Diffs,DiffL),maplist(print_nl,DiffL),      
  undiff(IA,OA,IZ,OZ),
  subst_2L(Sames,NewSames, T1+ T2+IU +OU,
                          _U1+_U2+IU2+OU2),
  try_omember(PeersI,T1,TT1),
  nop(PeersO=PeersO),
  flatten_sets([IU2,TT1],LHSSet),
  flatten_sets([OU2],RHSSet),
  %peerless_props(O1,PeersO,Props2),
  %print([x=[in_i(S1),in_o(Props1),out_i(S2),out_o(Props2)]]),
  SETS = RHSSet+LHSSet,
  save_learnt_rule(test_solved(i_o,obj(NewSames,LHSSet,IZ),obj(NewSames,RHSSet,OZ)),1+2+3+4+5+6+SETS,SETS))))),!.  


%dg(I1):-  print_grid(I1),!, print_info(I1).
dg(I1):- debug_as_grid(I1) -> true ; (print_grid(I1), print_info(I1)).
%print_object_pair(I1,O1):- dg(I1),!, dg(O1),!.

show_pair_now(TITLE,OO1,OO2):-  
 must_det_ll((
  dash_chars,dash_chars,format("~N~n\t\t",[]),ppt(TITLE),
  into_obj(OO1,O1),into_obj(OO2,O2),
  object_grid_to_str(O1,Str1,T1),
  object_grid_to_str(O2,Str2,T2),
  print_side_by_side(yellow,Str1,T1,_,Str2,T2),  
  format('~N~n'),
  nop(ignore((into_ngrid(O1,NO1),into_ngrid(O2,NO2), print_side_by_side(silver,NO1,ngrid(T1),_,NO2,ngrid(T2))))),
  debug_indiv_obj(O1),
  format('~N~n'),
  debug_indiv_obj(O2),  
  if_t(nb_current(menu_key,'o'),
   collapsible_section(info,compare_objs1(TITLE),false,
   (findall(E,compare_objs1(E,O1,O2),L), pp(compare_objs1(showdiff_objects)=L),
    indv_props(O1,S1),indv_props(O2,S2),
    intersection(S1,S2,Sames,SS1,SS2),
    proportional(SS1,SS2,lst(vals(_),len(_),PDiffs)),
    show_sames_diffs_now(Sames,PDiffs)))))),
  dash_chars, dash_chars.


show_sames_diffs_now(Sames,PDiffs):- 
   D is  2,
    az_ansi(noisey_debug(print_list_of(print_sames,sames,Sames))),    
    my_partition('\\='(o(_,_,_)),PDiffs,NonFunDiffs,FunDiffs),
    print_list_of(print_diffs(D + 1),diffs,NonFunDiffs),
    print_list_of(print_diffs(D + 1),oDiffs2,FunDiffs),  !.


undiff(I,O,(MI,IZ),(MO,OZ)):- select_two_props(_Style,I,O,CI,CO,II,OO),two_ok(CI,CO),manage_diff(CI,CO,MI,MO),undiff(II,OO,IZ,OZ).
undiff(IA,OA,IA,OA).

manage_diff(CI,CO,convert(CI,CO,How),accept(How)).

refunctor_args([A],[O]):- compound(A),!,refunctor(A,O).
refunctor_args([_],[_]):- !.
refunctor_args([H|T],[H|TT]):- refunctor_args(T,TT).
refunctor(I,O):- \+ compound(I),!,O=I.
refunctor(iz(C),iz(O)):-!, refunctor(C,O).
refunctor(pen([cc(Silver,_)]),pen([cc(Silver,_)])).
refunctor(edge(CI1,CI2),edge(CO1,CO2)):-!,(CI2=CO2;CI1=CO1).
refunctor(o(NS1,WE,_),o(NS2,WE,_)):-!,NS1=NS2.
refunctor([H|T],[HH|TT]):- !, refunctor(H,HH),refunctor(T,TT).
%refunctor(C,O):- is_list(C),!,maplist(refunctor,C,O).
refunctor(C,O):- compound_name_arguments(C,F,A),!,refunctor_args(A,B),compound_name_arguments(O,F,B).

objs_to_io(O2,O1,O1,O2):- (has_prop(giz(g(in)),O1);has_prop(giz(g(out)),O2)),!.
objs_to_io(O1,O2,O1,O2).

try_omember(_,S1,[O]):- O = o(  LF,N, A), member(O,S1),number(N),member(o(LF,N,B),S1),B\==A,!.
try_omember(_,S1,[O]):- O = o(_LF,N,_A), member(O,S1),number(N),!.
try_omember(Peers,S1,Props1):- include(not_peerless_prop(Peers),S1,Props1).

  

best_mates(PA,BBR,PB):- select_obj_pair_2([PA],BBR,PA,PB,_Why).


compare_objs_mask([perfect]).
compare_objs_mask([turned,+loc2D]).
compare_objs_mask([turned,-loc2D]).
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

maybe_good_prop1(vis2D(_,_)).
maybe_good_prop1(loc2D(_,_)).
maybe_good_prop1(iz(poly(_))).
maybe_good_prop1(birth(_)).
maybe_good_prop1(iz(locY(_))).
maybe_good_prop1(iz(locX(_))).
maybe_good_prop1(symmetry(X)):- !, nonvar(X).
prop_color(A,B):- var(B),!,freeze(B,prop_color(A,B)).
prop_color(C,C).

%prop_of(visual_impact,globalpoints(_)).
usefull_compare(P):- compound(P),functor(P,F,_),!,usefull_compare(F).
usefull_compare(P):- changed_by(P,_).

remove_giz(L,O):- include(not_giz,L,M),list_to_set(M,O).
not_giz(giz(_)):-!,fail.
not_giz(o(_,_,_)):-!,fail.
not_giz(merged(_)):-!,fail.
not_giz(birth(_)):-!,fail.
not_giz(changes(_)):-!,fail.
not_giz(P):- prop_type(_,P),!.
not_giz(iz(_)):-!.
not_giz(_):-!,fail.

prop_type(loc2D,loc2D(_,_)).
prop_type(loc2D,center2G(_,_)).
prop_type(loc2D,iz(locX(_))).
prop_type(loc2D,iz(cenX(_))).
prop_type(loc2D,iz(locY(_))).
prop_type(loc2D,iz(cenY(_))).
prop_type(scale,rotOffset(_,_)).
prop_type(scale,vis2D(_,_)).
prop_type(scale,iz(sizeX(_))).
prop_type(scale,iz(sizeY(_))).
prop_type(order,o(_Peers,_Ord,_Type)).
prop_type(shape,shape(_)).
prop_type(rotate,rot2L(_)).
prop_type(repaint,pen(_)).
prop_type(repaint,colors(_)).
prop_type(loc2D,edge(_,_)).

changed_by(shape,reshape).
changed_by(loc2D,move).
changed_by(amass,grow).
changed_by(localpoints,reshape_and_recolor).
changed_by(rot2L,rotate).
changed_by(colors,repaint).
changed_by(vis2D,copy).

link_prop_types(Loc,O1,O2,Ps):-
  findall(P,(prop_type(Loc,P), has_prop(O1,P),has_prop(O2,P)),Ps).


%maybe_allow_pair(PA,PB):- PA=@=PB,!,fail.
maybe_allow_pair(PA,PB):- never_pair(PA,PB),!,fail.
%maybe_allow_pair(PA,PB):- ((has_prop(PA,mass(1));has_prop(PB,mass(1)))),!,fail.
allow_pair(PA,PB):-  \+ never_pair(PA,PB).

never_pair(PA,PB):- PA=@=PB.
never_pair(PA,PB):- never_pair_r(PA,PB),!.
never_pair(PA,PB):- never_pair_r(PB,PA),!.
never_pair_r(PA,PB):- get_black(Black),has_prop(pen([_-Black]),PA),has_prop(pen([_,_|_]),PB).
%never_pair_r(PA,PB):- has_prop(chromatic(0,_),PA), \+ has_prop(chromatic(0,_),PB).


diff_groups1(AAR,BBR,DD):- maybe_reorder_pair(AAR,BBR,AAR2,BBR2),!,  diff_groups1a(AAR,BBR,AAR2,BBR2,DD).
diff_groups1(AAR,BBR,DD):- diff_groups1a(AAR,BBR,AAR,BBR,DD).

diff_groups1a(_OA,_OB,[],[],[]):-!.
diff_groups1a(_OA,_OB,[],B,right_over(BO)):- maplist(object_ref_desc,B,BO).
diff_groups1a(_OA,_OB,B,[],left_over(BO)):- maplist(object_ref_desc,B,BO).
diff_groups1a(OA,OB,AAR,BBR,DD):-
  select_obj_pair_1_3(AAR,BBR,PA,PB,Why),
  diff_objects(PA,PB,DAB,Same),
  (DAB == [] -> D = [] ;  
     (nop(showdiff(PA,PB)),
      object_ref_desc(PA,GA), 
      object_ref_desc(PB,GB),
      D = change_obj(Why,GA,GB,Same,DAB))),
  select(PA,AAR,AA), select(PB,BBR,BB),
  diff_groups1a(OA,OB,AA,BB,D1),
  combine_diffs(D1, D , DD),!.

diff_groups1a(OA,OB,[PA|AA],[PB|BB],DD):-
  diff_objects(PA,PB,DAB,Same),
  (DAB == [] -> D = [] ;  
     (%(showdiff(PA,PB)),
      object_ref_desc(PA,GA), 
      object_ref_desc(PB,GB),
      D = change_obj(list_ordered,GA,GB,Same,DAB))),  
  diff_groups1a(OA,OB,AA,BB,D1),
  combine_diffs(D1, D , DD),!.

diff_groups1a(_OA,_OB,A,B,[disjointed(SharedT,AOnlyT,BOnlyT)]):- 
  intersection(A,B,Shared,AOnly,BOnly),
  tersify_cheap(Shared,SharedT),
  tersify_cheap(AOnly,AOnlyT),
  tersify_cheap(BOnly,BOnlyT).

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
uncomparable(group,W):- too_non_unique(W).
uncomparable(group,W):- too_unique(W).
uncomparable(H,P):- compound(P),!,functor(P,F,_),uncomparable2(H,F).
uncomparable(group,W):- good_overlap(W),!,fail.
uncomparable(H,F):- uncomparable2(H,F).

uncomparable2(group,grid).
uncomparable2(group,globalpoints).
uncomparable2(group,giz).
uncomparable2(group,o).
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
simular(loc2D=Where,I,O,object_has_moved(Where)):-  
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
  %merge_2objs(VM,I,O,[],IO),
  %must_det_ll(indv_props(O,OProps)),
  must_det_ll(indv_props(I,IProps)),
  same_globalpoints_and_window(I,O),
  my_partition(props_not_for_merge,IProps,_Exclude,Include),
  % iz(merged(cgp))
  must_det_ll(override_object(Include,O,IO)),
  must_det_ll(combine_same_globalpoints([IO|IndvS2],NoMoreDupes)),
  must_det_ll(append(NoDupes,NoMoreDupes,IndvSO)),!.
combine_same_globalpoints(IndvSO,IndvSO).


%overlap_same_obj_no_diff(I,O):- compare_objs1(perfect,I,O). %diff_objects(I,O,Diff),Diff==[]. 
%overlap_same_obj(I,O):- compare_objs1(sameO,I,O).

showdiff_objects(A,B):- into_obj(A,A1),into_obj(B,B1), !, showdiff_objects_n(sameness,A1,B1),!.

showdiff_objects_vis(N,O1,O2):- showdiff_objects_n(vis(N),O1,O2).


%showdiff_objects_n(N,O1,O2,[]):- print_list_of(N,[O1,O2]),!.
showdiff_objects_n(N,O1,O2):-  showdiff_objects(change_obj(N,O1,O2)).

maybe_add_long_web_message(SS):- current_predicate(add_long_web_message/1),!,call(call,add_long_web_message,SS).
maybe_add_long_web_message(SS):- write(SS).

showdiff_objects(Info):- in_pp(bfly),!, wots(SS,showdiff_objects1(Info)),
  format('~N'), wots(S,maybe_add_long_web_message(SS)),
  format('~N'),!,bfly_in_out(write_expandable3(false,S,bfly_in_out(write(SS)))),format('~N').
showdiff_objects(Info):- showdiff_objects1(Info).

showdiff_objects1(change_obj(N,O1,O2,Sames,Diffs)):- 
  show_pair_now(N,O1,O2),
  show_sames_diffs_now(Sames,Diffs).
showdiff_objects1(change_obj(N,O1,O2)):- 
  show_pair_now(N,O1,O2),!.
showdiff_objects1(XY):- pp(showdiff_objects(XY)),!.

print_sames(N):- is_list(N),!, maplist(print_sames,N).
print_sames(N):- format('\t'), pp_no_nl(N),probably_nl.

print_diffs(D,N):- is_list(N),!, maplist(print_diffs(D + 1),N).
print_diffs(D,diff(List1->List2)):- \+ is_points_list(List1), \+ is_points_list(List2), !,  
     n_tabs(D-1), print_list_of(print_diffs(D + 2),uniqLeft,List1),print_list_of(print_diffs(D + 1),uniqRight,List2).
print_diffs(D,N):- n_tabs(D), pp_no_nl(N),probably_nl.

n_tabs(D):- DD is D, forall(between(1,DD,_),format('\t')).

print_hints(N):- is_list(N),!, maplist(print_hints,N).
print_hints(N):- format('\t'), pp_no_nl(N),probably_nl.

excl_diff(C):- var(C),!,fail.
excl_diff(diff(A->_)):- !, excl_diff(A).
excl_diff(C):- compound(C),!, compound_name_arity(C,F,_),!,excl_diff(F).
excl_diff(localpoints).
excl_diff(shape).
leftover_diffs(P):- \+ excl_diff(P).

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
sprop_of(sameO,size2D).
sprop_of(sameO,shape).
sprop_of(sameO,colors).

sprop_of(moved,sameO).
sprop_of(moved,loc2D).

sprop_of(turned,rotate).

sprop_of(reshape_and_recolor,localpoints).




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


%diff2_terms(I,O,same(I)):- I=@=O.
%diff2_terms(I,O,diff(I->O)).


diff_terms(I,O,D):- diff_termz(I,O,D),!.
%diff_terms(I,O,O):- is_real_color(I),is_real_color(O). % diff_tracked(I->O)
diff_terms(I,O,I):- atom(I), I=@=O.
diff_terms(I,O,same(I)):- I=@=O.
diff_terms(I,O,diff(I->O)).

diff_termz(I,O,D):- nonvar_or_ci(D),!,diff_terms(I,O,DD),D=DD.
diff_termz(I,O,[]):- plain_var(I),plain_var(O),!.
diff_termz(I,O,O):- plain_var(I),!.
diff_termz(O,I,O):- plain_var(I),!.
diff_termz(I,O,D):- maybe_number(I,N1),maybe_number(O,N2),!,proportional_size(N1,N2,D).
/*
diff_termz(I,O,[]):- O=@=I,!.
diff_termz(I,O,[]):- O==I,!.
diff_termz(I,O,O):- I==[],!.
diff_termz(I,O,I):- O==[],!.
*/
diff_termz([IH,IV],[OH,OV],D):- maplist(number,[IH,IV,OH,OV]),!,maplist(diff_numbers,[IH,IV],[OH,OV],D).

%diff_termz(I,O, [] ):- (never_do_diff(I);never_do_diff(O)),!.
diff_termz(shape(I),shape(O),[]):- !,sort(I,II),sort(O,OO),II=@=OO,!.
diff_termz(shape(I),shape(O),shape(diff(I->O))):-!.
%diff_termz(I,O, (O \== I)):- O=@=I,!.
diff_termz(group_o(I),group_o(O),group_o(DD)):- !, must_det_ll(diff_groups(I,O,DD)).
diff_termz(I,O,DD):-  is_group(I), is_group(O), !, must_det_ll(diff_groups(I,O,DD)).
diff_termz(I,O, D):- non_grid_list(I),non_grid_list(O),!,diff_lists(I,O,D).
diff_termz(I,O,[]):- no_diff(I,O),!.
diff_termz(O,I,[]):- no_diff(I,O),!.

% diff_termz(I,O,DD):-  is_group(I),is_group(O), !,  include_fav_points(I,II), include_fav_points(O,OO), diff_groups(I,O,DD).

%diff_termz(obj(I),obj(O),OUT):- !, diff_objects(I,O,OUT).


diff_termz(I,O,D):- is_map(I),!,findall(D1,(get_kov(K, I, V),diff_terms(K=V,O,D1)),D).
diff_termz(IF=IA,O,IF=D):- find_kval(O,IF,OA),!,diff_terms(IA,OA,D).


diff_termz(Grid,Other,OUT):- needs_indivs(Grid,I),!,diff_termz(I,Other,OUT).
diff_termz(Other,Grid,OUT):- needs_indivs(Grid,I),!,diff_termz(Other,I,OUT).
diff_termz(I,O,D):- compound(I),compound(O),!,diff_compounds(I,O,D).



diff_compounds(I,O, [] ):- fail, (never_show_diff(I);never_show_diff(O)),!.

diff_compounds(I,O,D):- compound_name_arguments(I,IF,IA),compound_name_arguments(O,OF,OA),
  maplist(compute_diff_or_same,IA,OA,DA),
  diff_compounds(I,IF,O,OF,DA,D).

diff_compounds( I, F,_O, F, DA, D):- I=@=DA, D=[].
diff_compounds(_I, F,_O, F, DA, D):- !, compound_name_arguments(D,F,DA).
diff_compounds(I,_IF,O,_OF,_DA, diff(I->O)).


compute_diff_or_same(I,O,D):- maybe_number(I,N1),maybe_number(O,N2),!,proportional_size(N1,N2,D).
compute_diff_or_same(I,O,IO):- 
  diff_terms(I,O,D),
  maybe_no_diff(I,O,D,IO).

maybe_no_diff(I,_,[],I):-!.
maybe_no_diff(_,_,D,D).

is_object_props(O):- is_list(O),member(E,O),compound(E),shape(_)=E,!.
diff_lists(AA,BB,D):- AA=@=BB,!,D=[].
diff_lists(AA,BB,diff(AA=@=BB)):- sort(AA,A),sort(BB,B), A=@=B,!.
diff_lists(I,O,D1D):- is_kv_list(I),is_kv_list(O),!,kv_list_diff(_Sytle,I,O,D1D).
diff_lists(I,O,D1D):- is_object_props(I),is_object_props(O),!,object_props_diff(I,O,D1D).
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

select_two_simple(A,B,E1,E2,AA,BB):- select_two(A,B,E1,E2,AA,BB),!.

select_two(I,O,CI,CO,II,OO):- var(CI), prop_type(_,CI),copy_term(CI,CO),select_two(I,O,CI,CO,II,OO).
select_two(I,O,CI,CO,II,OO):- select_two0(I,O,CI,CO,II,OO), two_ok(CI,CO),!.
select_two(I,O,CI,CO,II,OO):- select_two0(I,O,CI,CO,II,OO), refunctor(CI,CII),CO=CII,!.

select_two0(I,O,CI,CO,II,OO):- select_two_0(I,O,CI,CO,II,OO).
select_two0(I,O,CI,CO,II,OO):- select_two_0(O,I,CO,CI,OO,II).
select_two0(I,O,CI,CO,II,OO):- select_two_1(I,O,CI,CO,II,OO).
select_two0(I,O,CI,CO,II,OO):- select_two_1(O,I,CO,CI,OO,II).
select_two0(I,O,CI,CO,II,OO):- select_two_2(I,O,CI,CO,II,OO).
select_two0(I,O,CI,CO,II,OO):- select_two_2(O,I,CO,CI,OO,II).
select_two0(I,O,CI,CO,II,OO):- select_two_3(I,O,CI,CO,II,OO).
select_two0(I,O,CI,CO,II,OO):- select_two_3(O,I,CO,CI,OO,II).

be_comparable(CI,CO):- \+ compound(CI); \+ compound(CO),!, data_type(CI,TI),data_type(CO,TO),TI=@=TO,!.
be_comparable(iz(CI),iz(CO)):-!, be_comparable(CI,CO).
be_comparable(giz(CI),giz(CO)):-!, be_comparable(CI,CO).
be_comparable(CI,CO):- compound(CI),compound(CO),functor(CI,F,A),functor(CO,F,A),!.


diff2_terms(A,B,D):- two_ok(A,B),!,must_det_ll(diff_terms(A,B,DD)),!,D=DD.


reduce_required(norm(IO),IO).
reduce_required(iz(IO),IO).
reduce_required(obj(IO),IO).
reduce_required(giz(IO),IO).
reduce_required(pen(IO),IO).
reduce_required(birth(IO),IO).
reduce_required(shape(IO),IO).
reduce_required(g(IO),IO).
reduce_required(i(IO),IO).


two_ok(I,O):- I=@=O,!.
two_ok(I,O):- ( is_list(I);is_list(O); \+ compound(I); \+ compound(O)),!, two_ok_dt(I,O).
two_ok(o(A,_,B),o(A,_,B)):-!.
two_ok(I,O):- reduce_required(I,II),!,functor(I,F,A),functor(O,F,A),arg(1,O,OO),!,two_ok(II,OO).
two_ok(cc(_,N),cc(_,N)).
two_ok(cc(N,_),cc(N,_)).
two_ok(-(_,N),-(_,N)).
two_ok(-(N,_),-(N,_)).
two_ok(edge(CI1,CI2),edge(CO1,CO2)):-!,(CI1==CO1;CI2==CO2).
%two_ok(giz(CI),CO):- !, CO=giz(COO), two_ok(CI,COO).
two_ok(A,B):- maybe_good_prop(A,B).
two_ok(CI,CO):- compound(CI),compound(CO),functor(CI,F,A),functor(CO,F,A),!,
  compound_name_arguments(CI,F,A1),compound_name_arguments(CO,F,A2),!,
  nop(args_ok(F,A,A1,A2)).  
  %maplist(two_ok,A1,A2).
two_ok(CI,CO):- two_ok_dt(CI,CO).

args_ok(_,_,[],[]).
args_ok(_F,A,A1,A2):- A>2,!,between(1,A,Nth),between(Nth,A,Nth2),Nth2>Nth,
  nth1(Nth,A1,I1),nth1(Nth2,A2,O1), I1==O1,
  nth1(Nth,A1,I2),nth1(Nth2,A2,O2), I2==O2,!.
args_ok(_F,_A,[A1],[A2]):- compound(A1),!,two_ok(A1,A2).
args_ok(F,A,[A1|T],[A2|TT]):- compound(A1),!,two_ok(A1,A2),args_ok(F,A,T,TT).
args_ok(F,A,[A1|T],[A1|TT]):- args_ok(F,A,T,TT).

%two_ok_dt(I,O):- atom(I),!,atom(O),!.
two_ok_dt(I,O):- (var(I);var(O)),!.
two_ok_dt(I,O):- number(I),!,number(O),!.
two_ok_dt(CI,CO):- data_type(CI,TI),data_type(CO,TO),TI==TO,!.



is_nil(Nil):- Nil == [].

list_diff_recurse_nil(I,O,[]):- I==O,!.
list_diff_recurse_nil(I,O,[]):- I=@=O,!.
list_diff_recurse_nil(Nil,O,diff(Nil->O)):- is_nil(Nil),!.
list_diff_recurse_nil(O,Nil,diff(O->Nil)):- is_nil(Nil),!.

list_diff_recurse(I,O,D1D):- list_diff_recurse_nil(I,O,D1D),!.

list_diff_recurse(I,O,D1D):- select_two(I,O,CI,CO,II,OO),diff2_terms(CI,CO,D1),!,
       list_diff_recurse(II,OO,D),!, combine_diffs(D1,D,D1D),!.
list_diff_recurse(I,O,D1D):- sort(I,III),sort(O,OOO),select_two_any(III,OOO,CI,CO,II,OO),diff2_terms(CI,CO,D1),!,
       list_diff_recurse(II,OO,D),!, combine_diffs(D1,D,D1D),!.
list_diff_recurse(I,O,[diff(I->O)]):- !.

list_diff_recurse([CI|II],[CO|OO],D1D):- must_det_ll(diff2_terms(CI,CO,D1)),!,
       list_diff_recurse(II,OO,D),!, combine_diffs(D1,D,D1D),!.

object_props_diff(I,O,D):- simplify_objs_l(I,II),!,simplify_objs_l(O,OO),!, list_diff_recurse(II,OO,D).

%kv_list_diff(Style,I,O,D1D):- select_two_0(I,O,CI,CO,II,OO),!,kv_list_diff(Style,II,OO,D1D).

select_two_kv(key,I,O,CI,CO,II,OO):- CI=C-_,CO=C-_, select_two_any(I,O,CI,CO,II,OO).
select_two_kv(value,I,O,CI,CO,II,OO):- CI=_-P1,CO=_-P1, select_two_any(I,O,CI,CO,II,OO).

select_two_props(_Style,I,O,CI,CO,II,OO):- select_two_any(I,O,CI,CO,II,OO),CI==CO.
select_two_props(_Style,I,O,CI,CO,II,OO):- select_two_any(I,O,CI,CO,II,OO),CI=@=CO.
select_two_props(_Style,I,O,CI,CO,II,OO):- select_two(I,O,CI,CO,II,OO),compound(I),compound(O),functor(I,F,A),functor(O,F,A).
select_two_props(_Style,I,O,CI,CO,II,OO):- select_two(I,O,CI,CO,II,OO).



kv_list_diff(_Sytle,I,O,D1D):- list_diff_recurse_nil(I,O,D1D),!.
kv_list_diff(Style,I,O,D1D):- select_two_any(I,O,CI,CO,II,OO),CI=@=CO,!,kv_list_diff(Style,II,OO,D1D).
kv_list_diff(Style,I,O,D1D):- select_two_any(I,O,CI,CO,II,OO),CI=CO,!,kv_list_diff(Style,II,OO,D1D).
kv_list_diff(Style,I,O,D1D):- select_two_kv(Style,I,O,CI,CO,II,OO),diff2_terms(CI,CO,D1),!,
       kv_list_diff(Style,II,OO,D),!, combine_diffs(D1,D,D1D),!.
kv_list_diff(Style,[CI|II],[CO|OO],D1D):- must_det_ll(diff2_terms(CI,CO,D1)),!,
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
   nop(is_type_call(NVM, N)),
   proportionate(NVM,List1,List2,N).

not_very_simular(X):- \+ not_very_different(X).

not_very_different(vis_hv_term(size2D(A,B))):- !, not_very_different_t(A),not_very_different_t(B).
not_very_different(vis_hv_term(area(A))):-   !, not_very_different_t(A).
not_very_different(loc_term(loc2D(A,B))):-  !, not_very_different_t(A),not_very_different_t(B).
not_very_different(center_term(loc2D(A,B))):-  !, not_very_different_t(A),not_very_different_t(B).

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

unused_proportion(_,_,_):- nb_current(allow_unused_proportion,t),!,fail.
unused_proportion(Obj1,Obj2,Obj3):- unused_proportion1(Obj1,Obj2,Obj3),!.

unused_proportion1( Obj1,Obj2,Obj1):- Obj1=@=Obj2.
unused_proportion1(Obj1,_Obj2,Obj1):- var(Obj1),!.
unused_proportion1(_Obj1,Obj2,Obj2):- var(Obj2),!.


%proportional(Obj1,Obj2,Obj3):- unused_proportion1(Obj1,Obj2,Obj3),!.
proportional(A2,B2,List):- maybe_reorder_pair(A2,B2,A3,B3), !, proportional(A3,B3,List).
proportional(L1,L2,List):- non_grid_list(L1),non_grid_list(L2),!,must_det_ll(proportional_lists(L1,L2,List)).
proportional(loc2D(H1,V1),loc2D(H2,V2),loc2D(H,V)):- !, proportional_loc(H1,H2,H),proportional_loc(V1,V2,V).
proportional(loc2G(H1,V1),loc2G(H2,V2),loc2G(H,V)):- !, proportional_loc(H1,H2,H),proportional_loc(V1,V2,V).
proportional(center2G(H1,V1),center2G(H2,V2),center2G(H,V)):- !, proportional_loc(H1,H2,H),proportional_loc(V1,V2,V).
proportional(colors(H1),colors(H2),color_changes(H)):- !, proportional_lists(H1,H2,H).
%proportional(cc(N1,C),cc(N2,C),cc(H,C)):- !, proportional_size(N1,N2,H).
proportional(N1,N2,N):- number(N1),number(N2),!,proportional_size(N1,N2,N).

proportional(G1,G2,Out):- maybe_label_colors(G1,L1),!, proportional(L1,G2,Out).
proportional(G1,G2,Out):- maybe_label_colors(G2,L2),!, proportional(G1,L2,Out).

proportional(size2D(H1,V1),size2D(H2,V2),HV):- proportional_size2D(H1,V1,H2,V2,HV).
proportional(A,B,C):- maybe_extract_values(B,BB), compound(A), \+ maybe_extract_values(A,_), proportional(A,BB,AABB),proportional(AABB,B,C),!.
proportional(B,A,C):- maybe_extract_values(B,BB), compound(A), \+ maybe_extract_values(A,_), proportional(A,BB,AABB),proportional(AABB,B,C),!.

proportional(G1,G2,Out):- unused_proportion(G1,G2,Out),!.
%proportional(E1,E2,E1):- E1=@=E2,!.


proportional(Obj2,Obj1,Out):- 
  (is_object(Obj1) -> enum_obj_props(P1P2) ; enum_grid_props(P1P2)),
  P1P2=..[P2,P1|Lst],
  once((
  once((once(((is_type_call(P1,Obj1)),
              on_x_log_and_fail(is_type_call(P1,Obj2)))),
  length(Lst,Len), length(NewLst1,Len),length(NewLst2,Len),
  once((on_x_log_and_fail(apply(P2,[Obj1|NewLst1])),
        on_x_log_and_fail(apply(P2,[Obj2|NewLst2])))))),
  proportional_type_list(Lst,NewLst1,NewLst2,OutL))), 
  Out =.. [P2|OutL].

proportional(Obj1,Obj2,Out):- compound(Obj1), compound(Obj2),  fail,
  is_grid(Obj1),is_grid(Obj2),
  once((grid_props(Obj1,Out1), grid_props(Obj2,Out2))),
  proportional_lists(Out1,Out2,Out),!.

proportional(L1,L2,_List):- is_grid(L1),is_grid(L2),!,fail.

proportional(N1,N2,N):- compound(N1),compound_name_arguments(N1,F,A1),compound_name_arguments(N2,F,A2),
  maplist(proportional_or_same,A1,A2,AR),compound_name_arguments(N,F,AR).
  
proportional(L1,L2,Diff):- locally(nb_setval(diff_porportional,t),diff2_terms(L1,L2,Diff)),!.

:- multifile(gvs:dot_overload_hook/4).
:- dynamic(gvs:dot_overload_hook/4).
:- module_transparent(gvs:dot_overload_hook/4).
gvs:dot_overload_hook(_M,_NewName, _Memb, _Value):- fail.


grid_props(Obj1,OOO):- \+ is_grid(Obj1),!,into_grid(Obj1,G),print_grid(G),grid_props(G,OOO).
grid_props(Obj1,OOO):- % \+ arc_option(grid_size_only), 
 %to_assertable_grid(Obj1,AG),data_type(Obj1,DT),
 % wots(S,print_grid(Obj1)),
 findall(Prop, ((
  enum_grid_props(P1P2),
  P1P2=..[P2,P1|Lst], once((on_x_log_and_fail(is_type_call(P1,Obj1)))),
    length(Lst,Len), length(NewLst1,Len), 
    once((on_x_log_and_fail(apply(P2,[Obj1|NewLst1])))),
   Prop =.. [P2|NewLst1])), ListO),
   %OOO = [toStr(S)|ListO],
   OOO = ListO,
   !.

enum_grid_props(P1P2):- no_repeats(P1P2,(is_decl_pt(prop_g,P1P2))).
enum_obj_props(P1P2):- no_repeats(P1P2,(is_decl_pt(prop_o,P1P2))).





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


diff_f(lst).
diff_f(num).


proportional_loc(G1,G2,Out):- unused_proportion(G1,G2,Out),!.
proportional_loc(N1,N2,moved(N1,N,N2)):- diff_numbers(N1,N2,N).


diff_numbers(M1,N2,P):- maybe_number(M1,N1),M1\==N1, !, diff_numbers(N1,N2,P).
diff_numbers(N1,M2,P):- maybe_number(M2,N2),M2\==N2, !, diff_numbers(N1,N2,P).
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
  into_vals(L1,L2,Vals), diff_lists(L1,L2,Diff),
  OUT=..[lst,vals(Vals),len(N),(Diff)])),!.

/*
proportional_lists(L1,L2,OUT):- 
 must_det_ll((
  length(L1,N1),length(L2,N2), proportional_size(N1,N2,N),
  intersection(L1,L2,Shared,IOnlyC,OOnlyC), 
  maplist(length,[L1,L2,IOnlyC,Shared,OOnlyC],Lens),
  into_vals(L1,L2,Vals),
  list_to_set(Shared,SharedS),
  diff_lists(IOnlyC,OOnlyC,Diff),
  OUT=..[lst,vals(Vals),len(N),d(Diff),lsr(IOnlyC,SharedS,OOnlyC)|Lens])),!.
*/

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
ratio_for(Out/In=Ratio,Out,In):- calc_ratio(Ratio,Out,In).
calc_ratio(1.0,Out,In):- 0 is In, 0 is Out,!.
calc_ratio(1,Out,In):- Out =:= In.
calc_ratio(Ratio,Out,_In):- 0 is Out, !, Ratio is +0.0.
calc_ratio(Ratio,_Out,In):- 0 is In, !, Ratio is -0.0.
calc_ratio(Ratio,Out,In):- catch(Ratio is rationalize(Out/In),error(evaluation_error(_Zero_divisor),_),fail),!.
calc_ratio(Ratio,Out,In):- catch(NRatio is rationalize(In/Out),error(evaluation_error(_Zero_divisor),_),fail),!, Ratio is -NRatio.

%:- decl_pt(prop_g,each_object(is_grid, set)).

%each_object(_Grid,[]):-!.
each_object(Grid,ListO):- \+ arc_option(grid_size_only), arc_memoized(individuate(complete,Grid,List)),!, simplify_objs(List,ListO).
%each_object(Grid,ListO):- print_collapsed(100,memoized(individuate(complete,Grid,List))),!, simplify_objs(List,ListO).

simplify_objs(I,O):-is_list(I),!,maplist(simplify_objs,I,O).
simplify_objs(obj(I),obj(O)):-!,simplify_objs_l(I,O).
simplify_objs(F,F).

simplify_objs_l(obj(I),O):- is_list(I),!,simplify_objs_l(I,O).
simplify_objs_l(I,O):- include(compound,I,M1),sort_obj_props(M1,M2),maplist(simplify_objs_e,M2,O).
%simplify_objs(iz(g(_)),iz(g(_))).
%simplify_objs(Comp,F):- compound(Comp),functor(Comp,F,_),uncomparable(group,Comp),!.


simplify_objs_e(iz(X),O):- compound(X),!, simplify_objs_e(X,O).
simplify_objs_e(X,X).

prefer_grid(G):- is_object_or_grid(G).

:- decl_pt(prop_g,mass(prefer_grid,number)).
:- decl_pt(prop_g,unique_colors(prefer_grid, set)).
:- decl_pt(prop_g,has_y_rows(grid,colcount,color,set(rownums))).
:- decl_pt(prop_g,has_x_columns(grid,rowcount,color,set(colnums))).
:- decl_pt(prop_g,x_columns(grid,set)).
:- decl_pt(prop_g,y_rows(grid,set)).
:- decl_pt(prop_g,colors(prefer_grid, set)).
:- decl_pt(prop_g,symmetric_types(prefer_grid, set)).


:- decl_pt(prop_o,center_term(object,loc2D)).
:- decl_pt(prop_o,loc_term(object,loc2D)).

:- assertz_if_new((is_decl_pt(prop_o, P):- is_decl_pt(prop_g, P))).



:- include(kaggle_arc_footer).

