/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

showdiff(A,B):- is_group(A), is_group(B), showdiff_groups(A,B),!.
showdiff(A,B):- is_object(A), is_object(B), dmsg((showdiff_objects(A,B))),!.
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
atomic_type(is_ncpoint). atomic_type(is_cpoint).
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

call_count(G,N):- ground(G),findall(G,G,Gs),length(Gs,N).
call_count(G,N):- term_variables(G,Vs),findall(Vs,G,Gs),list_to_set(Gs,Ss),length(Ss,N).

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
number_or_point(O):- is_ncpoint(O).

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

:- abolish(arc_cache:object_atomslist/5).
:- dynamic(arc_cache:object_atomslist/5).
%obj_grp_atoms(IO,A,[A,PA|Atoms]):- nonvar(IO),!,into_gid(IO,GID),obj_grp_atoms0(GID,A,[A,PA|Atoms]).
obj_grp_atoms(IO,A,[A,PA|Atoms]):- obj_grp_atomslist(IO,A,PA,Atoms).


obj_grp_atomslist(IO,A,PA,Atoms):- \+ \+ see_object_atomslist(IO,A,PA,Atoms), !, see_object_atomslist(IO,A,PA,Atoms).
obj_grp_atomslist(IO,A,PA,Atoms):- 
  obj_grp_atoms_deep(A,PA,Atoms),
  assert_in_testid(arc_cache:object_atomslist(IO,A,PA,Atoms)).

dref_match(Var,_):- var(Var),u_dmsg(dref_match(Var)),!,fail.
dref_match(rhs(LHS),PA):- !, dref_match(LHS,PA).
dref_match(lhs(LHS),PA):- !, dref_match(LHS,PA).
dref_match([obj(LHS)],PA):- !, dref_match(LHS,PA).
dref_match([LHS],PA):- !, dref_match(LHS,PA).
dref_match(object_to_object(_TestID,_Name,LHS,_Create,_DebugInfo,_),PA):- !, dref_match(LHS,PA).
dref_match(obj(LHS),PA):- !, dref_match(LHS,PA).
dref_match(List,PA):- is_list(List), flatten(List,ListF),List\=@=ListF,!,dref_match(ListF,PA).
dref_match(PA,PA).

obj_grp_atoms_deep(A,PA,Atoms):- A=obj(_),is_object(A),!,obj_grp_comparable(A,PA),obj_atoms(PA,Atoms).
obj_grp_atoms_deep(A,PA,Atoms):- dref_match(A,DA),A\=@=DA,!,obj_grp_atoms_deep(DA,PA,Atoms).
obj_grp_atoms_deep(A,PA,Atoms):- PA=A,obj_atoms(PA,Atoms).

see_object_atomslist(IO,A,PA,Atoms):- call_in_testid(arc_cache:object_atomslist(IO,A,PA,Atoms)).

other_io(in,out).
other_io(out,in).
other_io(I,O):- 
  cast_to_grid(I,Grid,UC),
  other_grid(Grid,Other),
  uncast(_,UC,Other,O).

find_obj_mappings(A,BG,OO):-
  obj_grp_atoms(IO,A,AINFO),
  other_io(IO,OI),
  my_maplist(obj_grp_atoms(OI),BG,BBR),
  find_obj_mappings2(AINFO,BBR,OO).


find_obj_mappings2([A,PA|PAP],BBR,pair4(A,PA,B,PB)):- !,
   ord(NJ/O+JO+Joins,[PA,A],[PB,B]) = Why,
   findall(Why,
    (      
     member([B,PB|PBP],BBR),
     PA\==PB,
     B\==A,
     \+ is_whole_grid(B),
      must_det_ll((
     % maybe_allow_pair(PA,PB), allow_pair(PA,PB),  
       jaccard(PA,PB,PAP,PBP,NJ,O,JO,_J,Joins)))),
     Pairs), 
   sort_safe(Pairs,RPairs),!,
   %my_maplist(writeln,Pairs),
   %last(RPairs,prop_atoms(Best,_,_,_)),
   % Best = pair(PA,PB,_),
   member(Why,RPairs).
find_obj_mappings2([A,PA|PAP],BBR,pair4(A,PA,B,PB)):- 
   ord(NJ/O+JO+Joins,[PA,A],[PB,B]) = Why,
   findall(Why,
    (      
     member([B,PB|PBP],BBR),
     PA\==PB,
     B\==A,
     \+ is_whole_grid(B),
      must_det_ll((
     % maybe_allow_pair(PA,PB), allow_pair(PA,PB),  
       intersection(PAP,PBP,Joins,OtherA,OtherB),     
       flatten([OtherA,OtherB],Other),
       length(Joins,J),length(Other,O),
       NJ is -J,
       JO is - rationalize(J/(O+1))))),
     Pairs), 
   sort_safe(Pairs,RPairs),!,
   %my_maplist(writeln,Pairs),
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
  my_maplist(obj_grp_comparable,AG,A3),
  my_maplist(obj_grp_comparable,BG,B3),
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
  ignore(((IntersectA\==[], 
   w_section("Object Differences",
     my_maplist(showdiff_objects_vis(Pred),IntersectA,IntersectB))))).


diff_groups2(AAR,BBR,proportional(DD,Diffs)):- proportional(AAR,BBR,DD), my_maplist(diff_objects,AAR,BBR,Diffs).
diff_groups0(A3,B3,DD):- diff_groups2(A3,B3,DD).
diff_groups0(A3,B3,DD):- diff_groups1(A3,B3,DD).

diff_groups(A0,B0,DD):- 
  my_maplist(obj_grp_comparable,A0,A2),
  my_maplist(obj_grp_comparable,B0,B2),
  diff_groups1(A2,B2,DD).

obj_atoms(PA,PAP):- PA==[],!,PAP=[].
obj_atoms(PA,PAP):- sub_term(E,PA),compound(E),E=obj_atoms(UU),!,subobj_atoms(UU,PAP).
obj_atoms(PA,PAP):- must_det_ll((nonvar(PA))),is_grid(PA),globalpoints(PA,GP),!,subobj_atoms(points(GP),PAP).
obj_atoms(PA,PAP):- is_list(PA),maplist(obj_atoms,PA,LPA),append(LPA,PAP),!.
obj_atoms(PA,PAP):- is_object(PA),must_det_ll((indv_props_list(PA,MF),subobj_atoms(MF,PAP),PAP\==[])).
obj_atoms(PA,PAP):- into_obj_props1(PA,MF),must_subobj_atoms(MF,PAP),!.
obj_atoms(PA,PAP):- must_subobj_atoms(PA,PAP),!.

%never_matom(localpoints(_)).
%never_matom(shape_rep(grav,_)).
%never_matom(pg(_OG,_,_,_)).
%never_matom(giz(_)).
never_matom(edit(_)).
%never_matom(globalpoints(_)).
verbatum_matom(pg(_,_,_,_)).
relaxed_matom(pg(_,A,B,C),pg(r,A,B,C)).
relaxed_matom(link(A,r),link(A,r)).

must_subobj_atoms(PA,PAP):- subobj_atoms(PA,PAP),PAP\==[],!.
must_subobj_atoms(PA,PAP):- findall(E,(all_sub_terms(E,PA),nonvar(E)),PAP),!.

all_sub_terms(S,T):- is_list(T),!,member(E,T),all_sub_terms(S,E).
all_sub_terms(S,T):- T=S.
all_sub_terms(S,T):- compound(T),functor(T,F,A),((arg(_,T,E),all_sub_terms(S,E));S=F/A).

subobj_atoms(PA,PAP):- PA==[],!,PAP=[].
%subobj_atoms(PA,PAP):- is_grid(PA),globalpoints(PA,GP),!,subobj_atoms(GP,PAP).
subobj_atoms(PA,PAP):- is_grid(PA),globalpoints(PA,GP),!,sub_obj_atom(points(GP),PAP).
subobj_atoms(PA,PAP):- must_det_ll((nonvar(PA),flatten([PA],M), 
  findall(E,(member(SE,M),sub_obj_atom(E,SE)),PAP))),!.

sub_obj_atom(_,E):- var(E),!,fail.
%sub_obj_atom(M,M):- attvar(M),!.
sub_obj_atom(E,E):- \+ compound(E),!.
%sub_obj_atom(E,shape_rep(grav,CP)):- !, is_list(CP),member(E,CP).
sub_obj_atom(_,E):- never_matom(E),!,fail.
sub_obj_atom(E,E):- verbatum_matom(E).
sub_obj_atom(E,L):- is_list(L),!,member(EM,L),sub_obj_atom(E,EM).

sub_obj_atom(R,E):- relaxed_matom(E,R),E\=@=R.
sub_obj_atom(NO,M):- remove_oids(M,MM,EL),EL\==[], !,sub_obj_atom(NO,MM).
sub_obj_atom(M,M).
sub_obj_atom(M,pg(T,P1,R,I)):- !, ((M = extra(R,I,T));(M = extra(R,T,P1)),(M = extra(R,I))). %, \+ (arg(_,M,V),var(V)).


%sub_obj_atom(globalpoints(E),globalpoints(CP)):- !, my_maplist(arg(2),CP,EL),!, (member(E,EL); (E=EL)).
%sub_obj_atom(A,M):- M = localpoints(_),!,A=M.
sub_obj_atom(iz(A),iz(A)):-!. % sub_obj_atom(A,M).
sub_obj_atom(A,M):- M=..[F,List],is_list(List),length(List,Len),!,
  (A=len(F,Len) ; (interesting_sub_atoms(List,E),A=..[F,E])).

sub_obj_atom(M,M):- functor(link,M,_),!.
sub_obj_atom(E,M):- interesting_sub_atoms(M,E).
%sub_obj_atom(S,M):- special_properties(M,L),!,member(S,L).

interesting_sub_atoms(PA,PAP):- is_grid(PA),globalpoints(PA,GP),!,sub_obj_atom(points(GP),PAP).

interesting_sub_atoms(List,E) :- is_list(List),!,member(EM,List),interesting_sub_atoms(EM,E).
interesting_sub_atoms(E,_):- var(E),!,fail.
interesting_sub_atoms(E,E) :- atomic(E),!.
interesting_sub_atoms(EM,E) :- sub_term(E,EM),compound(E), \+ \+ (arg(_,E,A),atomic(A)).

select_obj_pair_2(AAR,BBR,PA,PB,(J/O)):- 
 AAR\==[],
 BBR\==[],
 ord(NJ/O+JO+J+Joins,PA,PB) = Why,
 findall(Why,
  (member(PA,AAR), 
   member(PB,BBR), 
     must_det_ll((
   % maybe_allow_pair(PA,PB), allow_pair(PA,PB),
     jaccard(PA,PB,_PAP,_PBP,NJ,O,JO,J,Joins)))),
   Pairs), 
 sort_safe(Pairs,RPairs),!,
 %my_maplist(writeln,Pairs),
 %last(RPairs,prop_atoms(Best,_,_,_)),
 % Best = pair(PA,PB,_),
 member(Why,RPairs).

is_io(PB,IOB):- has_prop(giz(g(IOB)),PB).

jaccard_focus(I,O,touch):- I=@=O,!.
jaccard_focus(I,O,sameness):- I\=@=O,!.

focus_type(sameness,link(_,_)):-!,fail.
focus_type(sameness,center2G(_,_)):-!.
focus_type(sameness,_). 
%focus_type(touch,link(sees(_),_)).
focus_type(touch,link(_,_)).
focus_type(touch,center2G(_,_)).

focus_type_include(Touch,Mask):- \+ \+ focus_type(Touch,Mask).
include_focus(Touch,PBP0,PBP):-
   my_include(focus_type_include(Touch),PBP0,PBP), PBP\==[].

jaccard(PA,PB,PAP,PBP,NJ,O,JO,J,Joins):-
  is_io(PA,IOA),is_io(PB,IOB),
  jaccard_focus(IOA,IOB,Focus),
  jaccard(Focus,PA,PB,PAP,PBP,NJ,O,JO,J,Joins).
jaccard(Touch,PA,PB,PAP,PBP,NJ,O,JO,J,Joins):-
     obj_atoms(PA,PAP0), include_focus(Touch,PAP0,PAP),
     obj_atoms(PB,PBP0), include_focus(Touch,PBP0,PBP),
     !,
     intersection(PAP,PBP,Joins,OtherA,OtherB),     
     flatten([OtherA,OtherB],Other),
     length(Joins,J),length(Other,O),
     NJ is -J,
     JO is - rationalize(J/(O+1)),!.



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

uniqueness_prop(symmetry_type(_)).
uniqueness_prop(mass(_)).


indiv_show_pairs_input(_Peers,_Shown,_List,Indv):- nb_current(menu_key,'o'),!, dg(Indv).
indiv_show_pairs_input(_Peers,_Shown,_List,Indv):- get_current_test(TestID), print_info(Indv), 
  nop(ignore(what_unique(TestID,Indv))).

indiv_show_pairs_output(_Peers,_Shown,_List,Indv):- nb_current(menu_key,'o'),!, dg(Indv).
%indiv_show_pairs_output(_Peers,_Shown,_List,Indv):- has_prop(pen([cc('black',_)]),Indv),!, dash_chars, nop(show_indiv(Indv)).
indiv_show_pairs_output(Peers,_Shown,List,Indv):-
  dash_chars,
  (best_mates(Indv,List,Mate)->showdiff_arg1("I<>O",Peers,Indv,List,Mate);dg(Indv)).


showdiff_groups_new(AG,BG):- learn_group_mapping(AG,BG),!.
showdiff_groups_new(AG,BG):- 
 must_det_ll((
  other_io(IO,OI),
  retractall_in_testid(arc_cache:did_map(_,_,_,_)),   
  retractall_in_testid(arc_cache:object_atomslist(IO,_,_,_)),
  retractall_in_testid(arc_cache:object_atomslist(OI,_,_,_)),
  my_maplist(obj_grp_atoms(IO),AG,_AGG),
  my_maplist(obj_grp_atoms(OI),BG,_BGG),  
   print_list_of(xfer_mappings("IN  -> OUT",AG,BG,BGG),  inputOutputMap,AGG),
   %print_list_of(prox_mappings("OUT -> OUT",BG,BG,BGG), outputOutputMap,BGG),   
   print_list_of(xfer_mappings("IN  <- OUT",BG,AG,AGG),  outputInputMap,BGG),   
   %print_list_of(prox_mappings("IN  -> IN", AG,AG,AGG),   inputInputMap,AGG),
 true)).


xfer_mappings(TITLE,AG,BG,BGG,APA):-
 ignore(( 
  dash_chars(100),nl,nl,nl,
  member(E,APA),E=obj(_),!,  
  \+ is_whole_grid(E),
  APA = [A,PA|_Atoms],
  find_obj_mappings2(APA,BGG,Pair),  
  Pair = pair4(A,PA,B,PB),
  % must_det_ll(A\==B),
  nop(PA\==PB),!,
  %%show_indiv('xfer_mappings',A),  
  (TITLE == "IN <- OUT" 
    -> showdiff_arg1(TITLE,BG,B,AG,A)
     ; showdiff_arg1(TITLE,AG,A,BG,B)))).
  %showdiff_objects(PA,PB),!.

nearest_by_not_simular(A,B,R):- A == B,!, R = inf.
nearest_by_not_simular(_,B,R):- is_whole_grid(B), !, R = inf.
nearest_by_not_simular(A,B,R):- distance(A,B,R).

distance(A,B,R):- loc2D(A,X1,Y1),loc2D(B,X2,Y2), R is sqrt(abs(X1-X2)*abs(Y1-Y2)).

sort_by_closeness(In,Objs,List):- sorted_by_closeness(In,_Sorted,Objs,List).
:- dynamic(saved_sorted_by_closeness/4).
sorted_by_closeness(In,Sorted,Objs,List):- once(var(In);var(Objs)),!,enum_in_objs(In,Objs), sorted_by_closeness(In,Sorted,Objs,List),List\==[].
sorted_by_closeness(In,Sorted,Objs,List):- var(Sorted), my_maplist(obj_to_oid,Objs,OIDS), sort_safe(OIDS,Sorted),!,sorted_by_closeness(In,Sorted,Objs,List).
sorted_by_closeness(In,Sorted,Objs,List):- saved_sorted_by_closeness(In,Sorted,Objs,List),!.
sorted_by_closeness(In,Sorted,Objs,List):- 
  sort_by_jaccard(In,_,Objs,List),
  asserta(saved_sorted_by_closeness(In,Sorted,Objs,List)),!.



%find_prox_mappings(A,Candidates,Objs):- sort_by_jaccard(A,Candidates,Objs).
sort_by_jaccard(A,Candidates,Objs):- bonus_sort_by_jaccard([],A,sort_by_jaccard,Candidates,Objs).

find_prox_mappings(A,GroupID,Candidates,Objs):- sort_by_jaccard(A,GroupID,Candidates,Objs).
sort_by_jaccard(A,GroupID,Candidates,Objs):- bonus_sort_by_jaccard([],A,GroupID,Candidates,Objs).

bonus_sort_by_jaccard(Bonus,A,Candidates,Objs):-
  bonus_sort_by_jaccard(Bonus,A,sort_by_jaccard,Candidates,Objs).

find_prox_mappings(Bonus,A,GroupID,Candidates,Objs):- bonus_sort_by_jaccard(Bonus,A,GroupID,Candidates,Objs).

points_by_distance_to_center(I,GGP):- center2D(I,X,Y), globalpoints(I,GP), predsort_on(dist_to(X,Y),GP,GGP).

dist_to(X1,Y1,HVP,Dist):- center2D(HVP,X2,Y2),dist(X1,Y1,X2,Y2,Dist).
dist_to(P1,P2,Dist):- center2D(P1,X1,Y1), center2D(P2,X2,Y2),dist(X1,Y1,X2,Y2,Dist).


sort_by_distance(Bonus,A,GroupID,Candidates,Objs):-
     center2D(A,X1,Y1),
     globalpoints(A,AP),
     maplist(distance(A,AP,X1,Y1),Candidates,Results),
     sort(Results,SortedR),sort(SortedR,Sorted),
     tie_break_sbd(Bonus,A,GroupID,Sorted,Objs).

tie_break_sbd(Bonus,A,GroupID,[W1,W2|Sorted],[S1,S2|Sorted]):-
     arg(1,W1,W1a),arg(1,W2,W2a), arg(2,W2,W1b),arg(2,W2,W2b), W1a=:=W2a,W1b=:=W2b,
     bonus_sort_by_jaccard0(Bonus,A,GroupID,[W1,W2],[S1,S2]),!.
tie_break_sbd(_,_,_,Sorted,Sorted).     


dist(X1,Y1,X2,Y2,Dist):-
  DiffX is X1 - X2,
  DiffY is Y1 - Y2,
  Dist is sqrt(DiffX * DiffX + DiffY * DiffY).

object_points(R1,R2,P1,P2):- globalpoints(R1,CP1),globalpoints(R2,CP2),!,member(C-P1,CP1), member(C-P2,CP2).

% TODO Directional shooter
is_adjacent_same_color(R1,R2,0):- object_points(R1,R2,P,P).
is_adjacent_same_color(R1,R2,1):- object_points(R1,R2,P1,P2), is_adjacent_point(P1,Dir,P2), \+ is_diag(Dir).
is_adjacent_same_color(R1,R2,2):- object_points(R1,R2,P1,P2), is_adjacent_point(P1,Dir,PM), \+ is_diag(Dir), is_adjacent_point(PM,Dir,P2).
is_adjacent_same_color(R1,R2,2):- object_points(R1,R2,P1,P2), is_adjacent_point(P1,Dir,P2), is_diag(Dir).

distance(A,_AP,_X1,_Y1,B,Res):- A=@=B,!, Res=inf.
distance(_A,AP,_X1,_Y1,B,Res):- 
 must_det_ll((
  Res = dist(Close,R,B),
  %center2D(B,X2,Y2),
  %dist(X1,Y1,X2,Y2,R),
  R = 1,
  globalpoints(B,BP),
  maplist(is_adjacent_closeness(AP),BP,N),
  sumlist(N,Sum),
  Close is R*Sum)).

is_adjacent_closeness(AP,B,Sum):- maplist(is_adjacent_close(B),AP,N), sumlist(N,Sum).

is_adjacent_close(A,B,Diff):- is_adjacent_point(A,Dir,B),!, (\+ is_diag(Dir) ->  Diff = -2 ; Diff = -1).
is_adjacent_close(A,B,Diff):- center2D(A,X1,Y1),center2D(B,X2,Y2), dist(X1,Y1,X2,Y2,Dist),Diff is Dist+1.


bonus_sort_by_jaccard(_,_,_,[Obj],[Obj]):-!.
bonus_sort_by_jaccard(Bonus,A,GroupID,Candidates,Objs):- \+ \+ member(A,Candidates),!, 
  sort_by_distance(Bonus,A,GroupID,Candidates,Objs).
bonus_sort_by_jaccard(Bonus,A,GroupID,Candidates,Objs):- bonus_sort_by_jaccard0(Bonus,A,GroupID,Candidates,Objs).

simularity(B,A,Number):- 
  obj_grp_atomslist(simularity,B,PB,PBP),
  obj_grp_atomslist(simularity,A,PA,PAP),
  memo_op(PAP,PBP,O,Joins,_J,NJ,JO),
  ord(NJ/O+JO+Joins,[PA,A],[PB,B],B) = Number.


bonus_sort_by_jaccard0(Bonus,A,GroupID,Candidates,ObjsO):-
 must_det_ll((
    obj_grp_atomslist(GroupID,A,PA,PAP0),
    obj_atoms(Bonus,BonusAtoms),
    append(PAP0,BonusAtoms,PAP),
    (ord((NJ/O+JO+Joins),[PA,A],[PB,B],B) = Why),
    !,
    findall(Why,
     (member(B,Candidates),
        B\==A,
        \+ is_whole_grid(B),
        obj_grp_atomslist(GroupID,B,PB,PBP),
       % PA\==PB,
        memo_op(PAP,PBP,O,Joins,_J,NJ,JO)),
     % maybe_allow_pair(PA,PB), allow_pair(PA,PB),  
     Pairs), 
   sort_safe(Pairs,RPairs),
   %list_upto(3,RPairs,Some),
   my_maplist(arg(4),RPairs,Objs))),!,
 Objs=ObjsO.


memo_op(PAP,PBP,O,Joins,J,NJ,JO):- PAP@>PBP->memo_op_1(PBP,PAP,O,Joins,J,NJ,JO);memo_op_1(PAP,PBP,O,Joins,J,NJ,JO).

:- abolish(memo_op_then/7).
:- dynamic(memo_op_then/7).
memo_op_1(PAP,PBP,O,Joins,J,NJ,JO):- memo_op_then(PAP,PBP,O,Joins,J,NJ,JO),!.
memo_op_1(PAP,PBP,O,Joins,J,NJ,JO):- memo_op_now(PAP,PBP,O,Joins,J,NJ,JO), asserta(memo_op_then(PAP,PBP,O,Joins,J,NJ,JO)),!.

memo_op_now(PAP,PBP,O,Joins,J,NJ,JO):-
       intersection(PAP,PBP,Joins,OtherA,OtherB),!,
       %append([OtherA,OtherB],Other),
       length(Joins,J),length(OtherA,OA),length(OtherB,OB),
       O is OA+OB,
       NJ is -J,
       JO is - rationalize(J/(O+1)),!.

prox_mappings(TITLE,AG,BG,_BGG,APA):-
 ignore((  
  member(E,APA),E=obj(_),!,
  \+ is_whole_grid(E),
  \+ use_did_map(E),
  APA = [A,_PA|_Atoms],
  predsort(sort_on(nearest_by_not_simular(A)),BG,BGS),
  BGS=[B|_],
  % must_det_ll(A\==B),
  %%show_indiv('xfer_mappings',A),  
  dash_chars(100),nl,nl,nl,
  (TITLE == "IN <- OUT" 
    -> showdiff_arg1(TITLE,BG,B,AG,A)
     ; showdiff_arg1(TITLE,AG,A,BG,B)))).
  %showdiff_objects(PA,PB),!.

:- dynamic(arc_cache:did_map/5).
use_did_map(O1):- call_in_testid(arc_cache:did_map(_,O1,_,_)),!.
use_did_map(O2):- call_in_testid(arc_cache:did_map(_,_,_,O2)),!.
get_did_map(PeersI,O1,PeersO,O2):- call_in_testid(arc_cache:did_map(PeersI,O1,PeersO,O2)).
get_did_map(PeersO,O2,PeersI,O1):- call_in_testid(arc_cache:did_map(PeersI,O1,PeersO,O2)).

showdiff_arg1(TITLE,Peers1,Obj1,Peers2,Obj2):- 
 must_det_ll((
  findall(Peer,(nop(has_prop(pg(OG,X,_,Y),Obj1)),member(Peer,Peers1),has_prop(pg(OG,X,_,Y),Peer),Peer\==Obj1),Peers11),
  findall(Peer,(nop(has_prop(pg(OG,X,_,Y),Obj2)),member(Peer,Peers2),has_prop(pg(OG,X,_,Y),Peer),Peer\==Obj2),Peers22),
  objs_to_io(Obj1,Obj2,O1,O2),
  ((Obj1==O1) 
       -> (PeersI = Peers11,PeersO = Peers22) ; (PeersI = Peers22,PeersO = Peers11)),
 % map_objects_now(TITLE,PeersI,O1,PeersO,O2))).
/*
map_objects(TITLE,PeersI,O1,PeersO,O2):- 
 get_did_map(PeersI,O1,PeersM,OM),
 map_objects_now(m(TITLE),PeersM,OM,PeersO,O2).

map_objects(TITLE,PeersI,O1,PeersO,O2):- 
 get_did_map(PeersO,O2,PeersM,OM),
 map_objects_now(m(TITLE),PeersI,O1,PeersM,OM).

map_objects(_TITLE,_PeersI,O1,_PeersO,O2):-
 (did_map(O1);is_bg_object(O1)),
 (did_map(O2);is_bg_object(O2)),!.

map_objects(TITLE,PeersI,O2,PeersO,O2):-
 map_objects_now(TITLE,PeersI,O2,PeersO,O2).

*/
%map_objects_now(TITLE,PeersI,O2,PeersO,O2):-
 %must_det_ll((

 assertz_in_testid(arc_cache:did_map(PeersI,O1,PeersO,O2)),
 %link_prop_types(loc2D,O1,O2,_LOCS),
 show_pair_now(TITLE,O1,O2),
  %what_unique(TestID,O2),
 if_t(nb_current(menu_key,'u'),
 (
  indv_props_list(O1,S1),indv_props_list(O2,S2),
  get_current_test(TestID), nop(ignore(what_unique(TestID,O1))),
  remove_giz(S1,T1),remove_giz(S2,T2),
  indv_u_props(O1,IU),indv_u_props(O2,OU),
  intersection(T1,T2,Sames,IA,OA),my_maplist(refunctor,Sames,NewSames),
  object_props_diff(IA,OA,Diffs), listify(Diffs,DiffL),my_maplist(print_nl,DiffL),      
  undiff(IA,OA,IZ,OZ),
  subst_2L(Sames,NewSames, T1+ T2+IU +OU,
                          _U1+_U2+IU2+OU2),
  try_omember(PeersI,T1,TT1),
  nop(PeersO=PeersO),
  flatten_sets([IU2,TT1],LHSSet),
  flatten_sets([OU2],RHSSet),
  %peerless_props(O2,PeersO,Props2),
  %print([x=[in_i(S1),in_o(Props1),out_i(S2),out_o(Props2)]]),
  SETS = RHSSet+LHSSet,
  save_learnt_rule(arc_cache:object_to_object(i_to_o,obj(NewSames,LHSSet,IZ),obj(NewSames,RHSSet,OZ)),1+2+3+4+5+6+SETS,SETS))))),!.  


%dg(I1):-  print_grid(I1),!, print_info(I1).
dg(I1):- show_indiv(I1) -> true ; (print_grid(I1), print_info(I1)).
%print_object_pair(I1,O1):- dg(I1),!, dg(O1),!.

show_pair_now(TITLE,OO1,OO2):-  
 must_det_ll((
  dash_chars,dash_chars,format("~N~n\t\t",[]),ppt(TITLE),
  into_obj(OO1,O1),into_obj(OO2,O2),
  object_grid_to_str(O1,Str1,T1),
  object_grid_to_str(O2,Str2,T2),
  print_side_by_side(yellow,Str1,T1,_,Str2,T2),  
  format('~N~n'),

  if_t((true),
    learn_rule_in_out(TITLE,O1,O2)),

  nop(ignore((into_ngrid(O1,NO1),into_ngrid(O2,NO2), print_side_by_side(silver,NO1,ngrid(T1),_,NO2,ngrid(T2))))),

  if_t(\+ nb_current(menu_key,'u'),
    (dash_chars,show_indiv_textinfo(O1), format('~N~n'),dash_chars, show_indiv_textinfo(O2))),  

  if_t(nb_current(menu_key,'o'),
    nop((w_section(compare_objs1(TITLE),
     (findall(E,compare_objs1(E,O1,O2),L), pp(compare_objs1(showdiff_objects)=L),
      indv_props_list(O1,S1),indv_props_list(O2,S2),
      %pp(s1=S1),pp(s2=S2),
      intersection(S1,S2,Sames,SS1,SS2),
      proportional(SS1,SS2,lst(vals(_),len(_),PDiffs)),
      show_sames_diffs_now(Sames,PDiffs)),info,false)))))),
  dash_chars, dash_chars.


show_sames_diffs_now(Sames,PDiffs):- 
   D is  2,
    az_ansi(noisey_debug(print_list_of(print_sames,sames,Sames))),    
    my_partition('\\='(pg(_,_,_,_)),PDiffs,NonFunDiffs,FunDiffs),
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
refunctor(pg(OG,NS1,WE,_),pg(OG,NS2,WE,_)):-!,NS1=NS2.
refunctor([H|T],[HH|TT]):- !, refunctor(H,HH),refunctor(T,TT).
%refunctor(C,O):- is_list(C),!,my_maplist(refunctor,C,O).
refunctor(C,O):- compound_name_arguments(C,F,A),!,refunctor_args(A,B),compound_name_arguments(O,F,B).

objs_to_io(O2,O1,O1,O2):- (has_prop(giz(g(in)),O1);has_prop(giz(g(out)),O2)),!.
objs_to_io(O1,O2,O1,O2).

try_omember(_,S1,[O]):- O = pg(OG, LF,N, A), member(O,S1),number(N),member(pg(OG,LF,N,B),S1),B\==A,!.
try_omember(_,S1,[O]):- O = pg(_OG,_LF,N,_A), member(O,S1),number(N),!.
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

maybe_good_prop(pg(OG,How,LFN,O),pg(OG,How,LFN,O)).
maybe_good_prop(pg(OG,How,LFN,_),pg(OG,How,LFN,_)).
maybe_good_prop(pen([Color1|_]),pen([Color2|_])):- prop_color(Color1,Color2).
maybe_good_prop(pen([_,Color1|_]),pen([_,Color2|_])):- prop_color(Color1,Color2).
maybe_good_prop(pen([_,Color1|_]),pen([Color2|_])):- prop_color(Color1,Color2).
maybe_good_prop(pen([Color1|_]),pen([_,Color2|_])):- prop_color(Color1,Color2).
maybe_good_prop(A,A):- maybe_good_prop1(A).
maybe_good_prop(pg(OG,How,_,_),pg(OG,How,_,_)).

maybe_good_prop1(vis2D(_,_)).
maybe_good_prop1(loc2D(_,_)).
maybe_good_prop1(iz(type(_))).
maybe_good_prop1(/*b*/iz(_)).
maybe_good_prop1(iz(locY(_))).
maybe_good_prop1(iz(locX(_))).
maybe_good_prop1(symmetry_type(X)):- !, nonvar(X).
prop_color(A,B):- var(B),!,freeze(B,prop_color(A,B)).
prop_color(C,C).

%prop_of(visual_impact,globalpoints(_)).
usefull_compare(P):- compound(P),functor(P,F,_),!,usefull_compare(F).
usefull_compare(P):- changed_by(P,_).

remove_giz(L,O):- include(not_giz,L,M),list_to_set(M,O).
not_giz(giz(_)):-!,fail.
not_giz(merged(_)):-!,fail.
not_giz(changes(_)):-!,fail.
not_giz(/*b*/iz(i_o(_))):-!,fail.
not_giz(oid(_)):-!,fail.
%not_giz(pg(_,_,_,_)):-!,fail.
not_giz(P):- type_prop(_,P),!.
not_giz(iz(_)):-!.
not_giz(_).
%not_giz(_):-!,fail.


prop_type(P,T):- type_prop(T,P).

prop_type_type(reposition).
prop_type_type(rotate).
prop_type_type(reshape).
prop_type_type(rescale).
prop_type_type(repaint).
prop_type_type(reorder).


type_prop(reposition,loc2D(_,_)).
type_prop(reposition,center2G(_,_)).
%type_prop(reposition,loc2G(_,_)).
type_prop(reposition,iz(locX(_))).
type_prop(reposition,iz(cenGX(_))).
type_prop(reposition,iz(locY(_))).
type_prop(reposition,iz(cenGY(_))).
type_prop(reposition,edge(_,_)).
type_prop(reposition,link(_,_)).
type_prop(rotate,rotG(_)).
type_prop(rotate,rot2D(_)).
type_prop(rotate,rotSize2D(_,_,_)).
type_prop(rescale,rotSize2D(grav,_,_)).
type_prop(rescale,vis2D(_,_)).
type_prop(rescale,iz(sizeGX(_))).
type_prop(rescale,iz(sizeGY(_))).
type_prop(rescale,rotSize2D(_,_,_)).
type_prop(rescale,mass(_)).
%type_prop(rescale,cc(fg,_)).
type_prop(rescale,grid_rep(norm,_)).
type_prop(reshape,shape_rep(grav,_)).
type_prop(reshape,iz(sid(_))).
type_prop(reshape,iz(stype(_))).
type_prop(reshape,iz(algo_sid(_, _))).
type_prop(reshape,iz(filltype(_))).
type_prop(reshape,iz(symmetry_type(_,_))).
type_prop(reshape,grid_rep(norm,_)).
type_prop(reorder,pg(_Peers,_OG,_Type,_Ord)).
type_prop(reorder,link_count(_,_)).
type_prop(reorder,occurs_in_links(_,_)).
type_prop(repaint,colors_cc(_)).
type_prop(repaint,pen(_)).
type_prop(repaint,cc(_,_)).
type_prop(repaint,grid_rep(norm,_)).


changed_by(colorlesspoints,reshape).
changed_by(loc2D,move).
changed_by(mass,grow).
changed_by(localpoints,reshape_and_recolor).
changed_by(rot2D,rotate).
changed_by(colors_cc,repaint).
changed_by(vis2D,copy).

link_prop_types(Loc,O1,O2,Ps):-
  findall(P,(type_prop(Loc,P), has_prop(O1,P),has_prop(O2,P)),Ps).


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
diff_groups1a(_OA,_OB,[],B,right_over(BO)):- my_maplist(object_ref_desc,B,BO).
diff_groups1a(_OA,_OB,B,[],left_over(BO)):- my_maplist(object_ref_desc,B,BO).
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

%dislike_points1([iz(type(dot)),grid_size(H,V)]):- freeze(H, freeze(V, (HV is H * V, HV > 49))).
dislike_points1([colors_cc([cc(BG, _)]),iz(reshape(polygon))]):- freeze(BG,is_black_or_bg(BG)).


uncomparable(_,Var):- var(Var),!.
uncomparable(group,W):- too_non_unique(W).
uncomparable(group,W):- too_unique(W).
uncomparable(H,P):- compound(P),!,functor(P,F,_),uncomparable2(H,F).
uncomparable(group,W):- good_overlap(W),!,fail.
uncomparable(H,F):- uncomparable2(H,F).

uncomparable2(group,grid).
uncomparable2(group,globalpoints).
uncomparable2(group,giz).
uncomparable2(group,pg).
%uncomparable2(group,grid_size).
uncomparable2(group,obj_to_oid).
%uncomparable2(group,link).
uncomparable2(object,iz).
uncomparable2(colorlesspoints,localpoints).

never_show_diff(V):- var(V),!,fail.
never_show_diff(_):- nb_current(diff_porportional,t),!,fail.
%never_show_diff(o).
never_show_diff(link).
%never_show_diff(iz(A)):- atomic(A).
never_show_diff(iz(g(_))).
never_show_diff(obj_to_oid).
never_show_diff(change).
%never_show_diff(/*b*/iz).
never_show_diff(V):- compound(V),functor(V,F,_),!,never_show_diff(F).

never_do_diff(V):- never_show_diff(V).

make_comparable(I,I):- plain_var(I).
make_comparable(I,I):- \+ compound(I),!.
make_comparable(I,II):- is_list(I),!,my_maplist(make_comparable,I,II).
make_comparable(obj(L),obj(LL)):- !,make_comparable(L,LL).
make_comparable(L,L):- usefull_compare(L),!.
make_comparable(I,II):- functor(I,II,_).


no_diff(in,out).
simular([],_,_,[]):- !.
simular(loc2D=Where,I,O,object_has_moved(Where)):-  
  \+ (mass(O,OC), OC < 6) ,
  \+ (colors_cc(O,[cc(BG, _)|_]),is_black_or_bg(BG)),
  object_glyph(I,G), \+ object_glyph(O,G).



maye_sort(L,S):- is_list(L),\+ is_grid(L), !,sort_safe(L,S).
maye_sort(S,S).
obj_make_comparable(I,_):- plain_var(I),!,fail.
obj_make_comparable(obj(I),O):- !, obj_make_comparable(I,O).
obj_make_comparable(I,O):- is_list(I),my_maplist(obj_make_comparable_e,I,M),sort_obj_props(M,O),
 nop(pp(sort_obj_props(O))).
obj_make_comparable(I,O):- into_obj(I,M),obj_make_comparable(M,O).
%obj_make_comparable_e(I,O):- is_list(I),sort_safe(I,O).
%obj_make_comparable_e(Comp,F):- compound(Comp),functor(Comp,F,_),f_uncomparable_e(F).
%obj_make_comparable_e(grid_size(_,_),grid([])).
%obj_make_comparable_e(I,O):- I=..[F,L],maye_sort(L,S),O=..[F,S].
obj_make_comparable_e(I,I).
%obj_make_comparable(I,SI):- exclude(uncomparable,I,II), sort_safe(II,SI).

% f_uncomparable_e(F).
f_uncomparable_e(grid).
%f_uncomparable_e(/*b*/iz):- writeq(b).
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
  %member(mass(_),SL),
  member(shape_rep(grav,_),SL),
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
  must_det_ll(indv_props_list(I,Props)),
  must_det_ll(override_object(Props,O,IO)),
  must_det_ll(combine_duplicates1([IO|IndvS2],NoMoreDupes)),
  must_det_ll(append(NoDupes,NoMoreDupes,IndvSO)),!.
combine_duplicates1(IndvSO,IndvSO).


overlap_same_obj_no_diff(I,O):- !, globalpoints(I,II),globalpoints(O,OO),!,pred_intersection((=@=),II,OO,_,_,[],[]),!.
overlap_same_obj_no_diff(I,O):- compare_objs1(perfect,I,O). %diff_objects(I,O,Diff),Diff==[]. 

overlap_same_obj(I,O):- compare_objs1(sameO,I,O).

%fti(VM,[combine_objects|set(VM.lo_program)]):- combine_objects(VM),!.
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
  select(O,Rest,IndvS2),  \+ is_whole_grid(O),
  %merge_2objs(VM,I,O,[],IO),
  %must_det_ll(indv_props_list(O,OProps)),
  must_det_ll(indv_props_list(I,IProps)),
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

print_sames(N):- is_list(N),!, my_maplist(print_sames,N).
print_sames(N):- format('\t'), pp_no_nl(N),nl_if_needed_ansi.

print_diffs(D,N):- is_list(N),!, my_maplist(print_diffs(D + 1),N).
print_diffs(D,diff(List1->List2)):- \+ is_points_list(List1), \+ is_points_list(List2), !,  
     n_tabs(D-1), print_list_of(print_diffs(D + 2),uniqLeft,List1),print_list_of(print_diffs(D + 1),uniqRight,List2).
print_diffs(D,N):- n_tabs(D), pp_no_nl(N),nl_if_needed_ansi.

n_tabs(D):- DD is D, forall(between(1,DD,_),format('\t')).

print_hints(N):- is_list(N),!, my_maplist(print_hints,N).
print_hints(N):- format('\t'), pp_no_nl(N),nl_if_needed_ansi.

excl_diff(C):- var(C),!,fail.
excl_diff(diff(A->_)):- !, excl_diff(A).
excl_diff(C):- compound(C),!, compound_name_arity(C,F,_),!,excl_diff(F).
excl_diff(localpoints).
excl_diff(colorlesspoints).
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
sprop_of(sameO,colorlesspoints).
sprop_of(sameO,colors_cc).

sprop_of(moved,sameO).
sprop_of(moved,loc2D).

sprop_of(turned,rotate).

sprop_of(reshape_and_recolor,localpoints).


% which objects in I are simply-copied?
% which objects in I are moved?
% which objects in I are recolored?
% which objects in I are rotated?
% which objects in I are resized?
% which objects deleted in I have no O counterpart
% which objects are added in O are closest to other objects in I
% which objects are added in O are closest to other objects in O
related_formula(simply_copied,i_to_o,[loc2D,colorlesspoints,pen,rot2D]).
related_formula(simply_moved,i_to_o,[pen,differ(loc2D),rot2D,colorlesspoints]).
related_formula(simply_recolored,i_to_o,[differ(pen),loc2D,rot2D,colorlesspoints]).
related_formula(simply_rotated,i_to_o,[pen,near(loc2D),differ(rot2D),colorlesspoints]).
related_formula(simply_resized,i_to_o,[pen,near(loc2D),rot2D,norm_grid,differ(norm_ops)]).
related_formula(i_triggers_o,i_to_o,[near(loc2D)]).
related_formula(o_triggers_o,o_to_o,[near(loc2D)]).


not_in_sub(P2CallList,Obj):- member(E,P2CallList),(arg(3,E,O);arg(4,E,O)),Obj=O,!,fail.
not_in_sub(_,_).

remove_some(P,_,P):-!.
remove_some(PAIRS,P2CallList,NewPAIRS):-
  from_pairs_i(PAIRS,InC),
  from_pairs_o(PAIRS,OutC),
  include(not_in_sub(P2CallList),InC,NewInC),
  include(not_in_sub(P2CallList),OutC,NewOutC),!,
  into_pairs(NewInC,NewOutC,NewPAIRS).

from_pairs_i(i_to_o(I,_),I).
from_pairs_o(i_to_o(_,O),O).
one_pair(i_to_o,PAIRS,X,Y):- from_pairs_i(PAIRS,IObjs),from_pairs_o(PAIRS,OObjs),!,member(X,IObjs),member(Y,OObjs).
one_pair(o_to_o,PAIRS,X,Y):- from_pairs_o(PAIRS,Objs),!,select_two_objs(X,Y,Objs).
one_pair(i_to_i,PAIRS,X,Y):- from_pairs_i(PAIRS,Objs),!,select_two_objs(X,Y,Objs).

select_two_objs(X,Y,Objs):- select(X,Objs,RestObjs),member(Y,RestObjs).

into_pairs(I,O,i_to_o(I,O)).

/*
__|i1, i2, i3, i4
o1| x   y   x  xy
o2|         y   
o3|
o4|         x
*/

%  
into_th(ColOrRow,Nth,Obj,TH):- obj_to_oid(Obj,OID),into_gui_item(Obj,S),sformat(TH,'<th id="th_~w" onclick="top.sortTable~ws(this,~w);" class="th-sm selectable" style="max-width: 100; max-height: 100">~w</th>',[OID,ColOrRow,Nth,S]).

write_o_i_data(DIR,O,I):- 
 must_det_ll((
   id_between(DIR,I,O,ID),
   format('<td id="~w" class="tiptext selectable" style="color: white; max-width: 100; max-height: 100"',[ID]),
   ignore(write_data_between(DIR,I,O)),
   write('</td>'))),!.


:- dynamic(relation_between/4).

write_data_between(_DIR,I,O):- I==O,  !,
  Title = I,
  write_ta(Title), 
  write(' onclick="top.showDesc(this);">'),  write(0),
  print_gui_item(I),!.
  
write_data_between(DIR,I,O):- 
 must_det_ll((
  relation_value(I,O,Val,RelsC,MissedRel),
  wots_ansi(ToolTips, (pp(RelsCL/MissedC), nl, write_gec(same=RelsC), nl, write_gec(missed=MissedRel))),
  write_ta(ToolTips), write(' onclick="top.showDesc(this);">'),
  obj_to_oid(I,OID1),
  obj_to_oid(O,OID2),
  assert_if_new(relation_between(DIR,OID1,OID2,Val)),
  write(Val),
  print_gui_item(I))),!.
  %nop(add_tool_tips(ID,Title)))),!.

relation_value(II,OO,Val,RelsC,MissedRel):- 
 into_obj(II,I),into_obj(OO,O),
 must_det_ll((
  indv_props_list(I,IL),indv_props_list(O,OL),
  include(cmpable_value,IL,ILC),
  include(cmpable_value,OL,OLC),
  intersection(IL,OL,Rels,LO,RO),!,
  intersection(ILC,OLC,RelsC,LOC,ROC),!,
  my_maplist(length,[Rels,LO,RO,RelsC,LOC,ROC],[RelsL,LOL,ROL,RelsCL,LOCL,ROCL]),
  max_min(LOL,ROL,_,Missed),
  max_min(LOCL,ROCL,_,MissedC),
  (MissedC==LOCL->MissedRel=LOC;MissedRel=ROC),
  Val is (RelsCL*100+ (100-(MissedC*2))))),!.
relation_value(I,O,Val):-
  relation_value(I,O,Val,_RelsC,_MissedRel).

write_ta(Title):- write(' title="'), write_ea(Title), write('"'),!.

write_gec(N=V):- nl_if_needed,nonvar(N), !, pp_no_nl(N),writeln(' = '), !, wots(S,write_gec(V)),print_w_pad(2,S).
write_gec(Val):- is_list(Val), !, wots(S,my_maplist(write_gec,Val)), print_w_pad(2,S).
write_gec(Val):- writeg(Val).


write_ea(Title):-  string(Title),!, into_attribute(Title,TitleSA),write(TitleSA).
write_ea(Title):-  wots_ansi(TitleS,wqs_c((Title))), into_attribute(TitleS,TitleSA),write(TitleSA).
write_eh(List):- is_list(List),!,my_maplist(write_eh,List).
write_eh(String):- atomic(String),!,write(String).
write_eh(Compound):- wqs_c(Compound),!.

id_between(DIR,I,O,ID):-  obj_to_oid(I,OID1),obj_to_oid(O,OID2),sformat(ID,'~w_~w_~w',[DIR,OID1,OID2]).
   
sort_some_relations(IL,OL):- 
 write('<div id="sort_some_relations">'),
  sort_some_relations(i_to_o,OL,IL),
  sort_some_relations(o_to_i,IL,OL),
  sort_some_relations(i_to_i,OL,OL),
  sort_some_relations(o_to_o,IL,IL),
  write('</div>').

sort_some_relations(DIR,IL,OL):- 
 upcase_atom(DIR,Title),
  sformat(IDDIR,'~w_sort_some_relations',[DIR]),
 w_section(title(Title),
  (
    format('<div id="~w_panel" class="sort_some_relations resizable" style="min-height: 200; min-width: 80%"></div>',[IDDIR]),
    get_relation_between(DIR,Nodes,Links),
    u_dmsg(nodes=Nodes),
    u_dmsg(links=Links),
    sformat(S,'<script>top.runD3Sim("~w_panel",~@,~@)</script>',
                            [IDDIR,
                             json_write(current_output,Nodes),
                             json_write(current_output,Links)]),
    write(S))),
  !.

dir_to_oidset(Dir,OIDSet):-
 findall_set(OID,(relation_between(DIR,OID1,OID2,_),member(OID,[OID1,OID2])),OIDSet).

% relation_between(DIR,OID1,OID2,Val)
get_relation_between(DIR,Nodes,Links):-
  dir_to_oidset(Dir,OIDSet),
  u_dmsg(tOIDSet=OIDSet),
  my_maplist(oid_to_node,OIDSet,Nodes),
  findall(_{source: OID1, target: OID2, value: Val100}, 
   (dir_to_oidset(Dir,OIDSet),
    member(OID1,OIDSet),
    good_relation_between(DIR,OID1,OID2,Val),Val100 is Val/100+10),Links).

best_relation_between(DIR,OID1,OID2,Val):-
  dir_to_oidset(Dir,OIDSet),member(OID1,OIDSet),
  findall(Val-OID2,(relation_between(DIR,OID1,OID2,Val);relation_between(DIR,OID2,OID1,Val)),List),
  sort(List,SList),last(SList,Val-OID2).


good_relation_between(DIR,OID1,OID2,Val):-
  best_relation_between(DIR,OID1,OID2,Val).

good_relation_between(DIR,OID1,OID2,Val):-
  findall_set(Val,relation_between(DIR,_,_,Val),VList),
  sort(VList,SVList),  
  length(SVList,Len), Len>=4,
  [_,_|UseList] = SVList,!,
  relation_between(DIR,OID1,OID2,Val),
  \+ \+ member(Val,UseList).
good_relation_between(DIR,OID1,OID2,Val):- relation_between(DIR,OID1,OID2,Val).

oid_to_node(OID,_{oid:OID,name:OID,desc:Glyph,color:HTMLColor,reshape:circle,size:10}):- 
   oid_to_obj(OID,Obj),
   object_glyph(Obj,Glyph),object_glyph_colorz(Obj,[Color|_]),into_html_color(Color,HTMLColor),!.
oid_to_node(OID,_{oid:OID,name:OID,reshape:circle,size:10}).


cmpable_value(V):- \+ compound(V),!,fail.
cmpable_value(V):- sub_term(S,V),number(S),S\==0,!.
cmpable_value(V):- arg(1,V,E), is_points_list(E),!,fail.
cmpable_value(V):- sub_term(S,V),is_color(S),!, \+ has_zero(V).
cmpable_value(V):- sub_term(S,V),compound(S),cmpable_cmpd(S),!.
cmpable_cmpd(sid(_)).
cmpable_cmpd(rot2D(_)).
has_zero(V):- sub_var(0,V).


guess_some_relations(IL,OL):- 
 write('<div id="guess_some_relations">'),
  guess_some_relations(i_to_o,OL,IL),
  guess_some_relations(o_to_i,IL,OL),
  guess_some_relations(i_to_i,OL,OL),
  guess_some_relations(o_to_o,IL,IL),
  write('</div>').

%sort_il(A,BG,BGS):- predsort(sort_on(nearest_by_not_simular(A)),BG,BGS).
sort_il(A,BG,BGSR):- predsort(sort_on(relation_value(A)),BG,BGS),reverse(BGS,BGSR).



guess_some_relations(DIR,IL,OL):- 
 upcase_atom(DIR,Title),
 w_section(title(Title),
  ( sformat(IDDIR,'~w_guess_some_relations',[DIR]),
    retractall(relation_between(DIR,_OID1,_OID2,_Val)),
    format_s(`<table id="~w" class="compare_objects resizable sorttable sortable searchable display table table-bordered table-sm">`,[IDDIR]),
    format_s(`<tr><th><h3>~w</h3></th>`,[Title]), 
      forall(nth1(M,IL,I),must_det_ll(((into_th('Row',M,I,ITH),write_eh(ITH))))), write('</tr>'),
    forall(nth1(N,OL,O),
      must_det_ll(((write('<tr>'),into_th('Col',N,O,OTH),write_eh(OTH),
       sort_il(O,IL,SIL),
       forall(member(I,SIL),ignore(write_o_i_data(DIR,O,I))),
       write('</tr>'))))),
  format_s(`</table>`,[]))),!.


guess_some_relations(Title,IL,OL):-
  guess_some_relations([],IL,OL,RelationsList),
  w_section(title(Title),pp(RelationsList)).

guess_some_relations(ExceptFor,I,O,RelationsList):-
  into_pairs(I,O,PAIRS),
  guess_some_pair_relations(ExceptFor,PAIRS, 
       [simply_copied,simply_moved,simply_rotated,simply_recolored,simply_resized,
          i_triggers_o,simply_deleted,o_triggers_o],  RelationsList).

guess_some_pair_relations(ExceptFor,PAIRS,P2L,RelationsList):-
   try_some_pair_relations(ExceptFor,PAIRS,P2L,P2CallList), P2CallList\==[],!,
   append(P2CallList,ExceptFor,NewExceptFor),
   guess_some_pair_relations(NewExceptFor,PAIRS,P2L,RelationsList).

try_some_pair_relations(ExceptFor,PAIRS,P2L,P2CallList):- 
   (P2L\==[]->member(P2,P2L);true),
   related_formula(P2,Sel,Tests),
   Save = pair(P2,Sel,O1,O2),   
   findall(Save,
     (one_pair(Sel,PAIRS,O1,O2), \+ member(Save,ExceptFor), 
      once(equiv_props(Tests,O1,O2))),P2CallList),
   P2CallList\==[].

io_is_correct(i_to_o,O1,O2):- indv_props(O1,iz(input)),indv_props(O1,iz(output)).
io_is_correct(i_to_i,O1,O2):- indv_props(O1,iz(input)),indv_props(O1,iz(input)).
io_is_correct(o_to_o,O1,O2):- indv_props(O1,iz(output)),indv_props(O1,iz(output)).
  

pair4(P2,Sel,O1,O2):-
  related_formula(P2,Sel,Tests),
  into_obj(_,O1),into_obj(_,O2),
  io_is_correct(Sel,O1,O2),
  one_pair(Sel,PAIRS,O1,O2),   
  equiv_props(Tests,O1,O2).

get_prop(Test,O1,Prop):- 
  nonvar(Test),!,prop_specifier(Test,Prop),
  indv_props(O1,Prop).
get_prop(Test,O1,Prop):- 
  indv_props_list(O1,PropList),
  member(Prop,PropList),
  prop_specifier(Test,Prop).
    
prop_specifier(F,Prop):- Prop=..[F,_].
prop_specifier(F,Prop):- Prop=..[F,_,_].
prop_specifier(Spec,Prop):- Spec=..[F,A],Prop=..[F,A,_].

equiv_props(Prop1,Prop2):- not_differ_props(Prop1,Prop2).

equiv_props(Nil,_,_):- Nil==[],!.
equiv_props([H|T],O1,O2):- is_list(T),!,equiv_props(H,O1,O2),equiv_props(T,O1,O2).
equiv_props(Test,O1,O2):- var(Test),!,
  get_prop(Test,O1,Prop1),
  get_prop(Test,O2,Prop2),
  equiv_props(Prop1,Prop2).
equiv_props(differ(Test),O1,O2):- !,
  get_prop(Test,O1,Prop1),
  get_prop(Test,O2,Prop2),
  differ_props(Prop1,Prop2).
equiv_props(near(loc2D),O1,O2):-
  indv_props(O1,loc2D(X1,Y1)),
  indv_props(O2,loc2D(X2,Y2)),!,
  R is sqrt(abs(X1-X2)*abs(Y1-Y2)),
  R<3.
equiv_props(near(Test),O1,O2):- !,
  get_prop(Test,O1,Prop1),
  get_prop(Test,O2,Prop2),
  near_props(Prop1,Prop2).
equiv_props(loc2D,O1,O2):- !, 
  indv_props(O1,loc2D(X,Y)),!,
  indv_props(O2,loc2D(X,Y)).

equiv_props(Test,O1,O2):- related_formula(Test,_,Expanded),!, equiv_props(Expanded,O1,O2).
equiv_props(Test,O1,O2):- 
  get_prop(Test,O1,Prop1),!,
  get_prop(Test,O2,Prop2),
  not_differ_props(Prop1,Prop2).
/*equiv_props(Tests,O1,O2):-
    indv_props_list(O1,L1),
    indv_props_list(O2,L2),
    intersection(L1,L2,Tests,_D1,_D2).*/


near_props(Prop1,Prop2):-  number(Prop1),!,number(Prop2),!,PropDif is abs(Prop1-Prop2),PropDif=<1.
near_props(Prop1,Prop2):-  atom(Prop1),!,atom(Prop2),!.
near_props(Prop1,Prop2):-  compound(Prop1),
  compound_name_arguments(Prop1,_,Args1),
  compound_name_arguments(Prop2,_,Args2),
  my_maplist(near_props,Args1,Args2).
differ_props(Prop1,Prop2):- \+ not_differ_props(Prop1,Prop2).
not_differ_props(Prop1,Prop2):- Prop1=Prop2,!.
not_differ_props(Prop1,Prop2):- fail, compound(Prop1),
  compound_name_arguments(Prop1,_,Args1),
  compound_name_arguments(Prop2,_,Args2),
  my_maplist(not_differ_props,Args1,Args2).
  


:- style_check(+singleton).
%compute_diff_objs2(I,IIR,O,OOR,[]):-  diff_terms(IIR,OOR,[]),


  %pp(remove_sames(II,OO)),
%  select(C,IIR,IIR2),compound(C),generalize(C,M),select(M,OOR,OOR2),
  %atrace,diff_terms(IIR,OOR,OUT),!.
  
/*
  append(L,[A|R],II),append(L,[B|R],OO),
  diff_lists(A,B,Diffs),
  (simular(Diffs,obj(I),obj(O),Sameness) ->  OUT = same_object(Sameness) ; OUT = props_diff(obj(I),obj(O),Diffs)).
*/

needs_indivs(I,_):- is_object(I),!,fail.
needs_indivs(globalpoints(O),O):- !.
needs_indivs(I,O):- is_gridoid(I), \+ is_group(I),!, globalpoints_maybe_bg(I,O),!,I\==O.
%needs_indivs(I,O):- is_grid(I),_unshared_indivs(I,O),!.
needs_indivs(I,O):- is_gridoid(I), \+ is_group(I), arcST, atrace, compute_unshared_indivs(I,O),!.

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
diff_termz([IH,IV],[OH,OV],D):- my_maplist(number,[IH,IV,OH,OV]),!,my_maplist(diff_numbers,[IH,IV],[OH,OV],D).

%diff_termz(I,O, [] ):- (never_do_diff(I);never_do_diff(O)),!.
diff_termz(shape_rep(grav,I),shape_rep(grav,O),[]):- !,sort_safe(I,II),sort_safe(O,OO),II=@=OO,!.
diff_termz(shape_rep(grav,I),shape_rep(grav,O),shape_rep(grav,diff(I->O))):-!.
%diff_termz(I,O, (O \== I)):- O=@=I,!.
diff_termz(group_o(I),group_o(O),group_o(DD)):- !, must_det_ll(diff_groups(I,O,DD)).
diff_termz(I,O,DD):-  is_group(I), is_group(O), !, must_det_ll(diff_groups(I,O,DD)).
diff_termz(I,O, D):- non_grid_list(I),non_grid_list(O),!,diff_lists(I,O,D).
diff_termz(I,O,[]):- no_diff(I,O),!.
diff_termz(O,I,[]):- no_diff(I,O),!.

% diff_termz(I,O,DD):-  is_group(I),is_group(O), !,  include_fav_points(I,II), include_fav_points(O,OO), diff_groups(I,O,DD).

%diff_termz(obj(I),obj(O),OUT):- !, diff_objects(I,O,OUT).


diff_termz(I,O,D):- is_vm_map(I),!,findall(D1,(get_kov(K, I, V),diff_terms(K=V,O,D1)),D).
diff_termz(IF=IA,O,IF=D):- find_kval(O,IF,OA),!,diff_terms(IA,OA,D).


diff_termz(Grid,Other,OUT):- needs_indivs(Grid,I),!,diff_termz(I,Other,OUT).
diff_termz(Other,Grid,OUT):- needs_indivs(Grid,I),!,diff_termz(Other,I,OUT).
diff_termz(I,O,D):- compound(I),compound(O),!,diff_compounds(I,O,D).



diff_compounds(I,O, [] ):- fail, (never_show_diff(I);never_show_diff(O)),!.

diff_compounds(I,O,D):- compound_name_arguments(I,IF,IA),compound_name_arguments(O,OF,OA),
  my_maplist(compute_diff_or_same,IA,OA,DA),
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

is_object_props(O):- is_list(O),member(E,O),compound(E),shape_rep(grav,_)=E,!.
diff_lists(AA,BB,D):- AA=@=BB,!,D=[].
diff_lists(AA,BB,diff(AA=@=BB)):- sort_safe(AA,A),sort_safe(BB,B), A=@=B,!.
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

select_two(I,O,CI,CO,II,OO):- var(CI), type_prop(_,CI),copy_term(CI,CO),select_two(I,O,CI,CO,II,OO).
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


%reduce_required(norm(IO),IO).
reduce_required(iz(IO),IO).
reduce_required(obj(IO),IO).
reduce_required(giz(IO),IO).
%reduce_required(pen(IO),IO).
%reduce_required(/*b*/iz(IO),IO).
reduce_required(shape_rep(grav,IO),IO).
reduce_required(g(IO),IO).
reduce_required(i(IO),IO).


two_ok(I,O):- I=@=O,!.
two_ok(I,O):- ( is_list(I);is_list(O); \+ compound(I); \+ compound(O)),!, two_ok_dt(I,O).
two_ok(pg(OG,A,_,B),pg(OG,A,_,B)):-!.
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
  %my_maplist(two_ok,A1,A2).
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
list_diff_recurse(I,O,D1D):- sort_safe(I,III),sort_safe(O,OOO),select_two_any(III,OOO,CI,CO,II,OO),diff2_terms(CI,CO,D1),!,
       list_diff_recurse(II,OO,D),!, combine_diffs(D1,D,D1D),!.
list_diff_recurse(I,O,[diff(I->O)]):- !.

list_diff_recurse([CI|II],[CO|OO],D1D):- must_det_ll(diff2_terms(CI,CO,D1)),!,
       list_diff_recurse(II,OO,D),!, combine_diffs(D1,D,D1D),!.

%object_props_diff(I,O,diff(_,_)):- atrace,!.
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
find_kval(Dict,OF,OA):- is_vm_map(Dict),get_kov(OF,Dict,OA).
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


count_difs(A,B,C):- into_grid(A,AA),into_grid(B,BB), !, count_difs0(AA,BB,C).
count_difs0(Out,GridO,0):- Out=@=GridO,!.
count_difs0(Out,GridO,1):- \+ compound(Out), \+ compound(GridO),  is_bg_color(Out); is_bg_color(Out).
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
not_very_different(mass(A)):- !, not_very_different_t(A).
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
proportional(colors_cc(H1),colors_cc(H2),color_changes(H)):- !, proportional_lists(H1,H2,H).
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
  my_maplist(proportional_or_same,A1,A2,AR),compound_name_arguments(N,F,AR).
  
proportional(L1,L2,Diff):- locally(nb_setval(diff_porportional,t),diff2_terms(L1,L2,Diff)),!.

:- multifile(dictoo:dot_overload_hook/4).
:- dynamic(dictoo:dot_overload_hook/4).
:- module_transparent(dictoo:dot_overload_hook/4).
dictoo:dot_overload_hook(_M,_NewName, _Memb, _Value):- fail.


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

on_x_log_and_fail(G):- catch(G,E,(u_dmsg(red((E -> G))),rrtrace(G),fail)).

%proportional(N1,N2,N):- is_object(N1),is_object(N2),!,proportional_objs(N1,N2,N).
%proportional(N1,N2,N):- is_grid(N1),is_grid(N2),!,proportional_grids(N1,N2,N).

maybe_number(N,N):- \+ compound(N),!,number(N).
maybe_number(M,N):- extract_vals(M,Vals),!,last(Vals,N),number(N),!.

into_vals(V1,V2,Vals):- extract_vals(V1,VV1),extract_vals(V2,VV2),append(VV1,VV2,Vs),sort_safe(Vs,Vals).

%extract_vals(M,Vals):- var(M),!,Vals=M.
%extract_vals(M,Vals):- \+ compound(M),!,Vals=[M].
extract_vals(M,Vals):- maybe_extract_values(M,Vals),!.
extract_vals(M,N):- M=..[_,A],maybe_extract_values(A,N),!.
extract_vals(V2,[V2]).

maybe_extract_values(Color,Values):- must_be_free(Values), compound(Color), Color=..[DF,vals(Values)|_],diff_f(DF), fail,!,is_list(Values),!.
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

is_vset(Colors):- sort_safe(Colors,ColorsS),!,Colors=ColorsS.
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
  my_maplist(length,[L1,L2,IOnlyC,Shared,OOnlyC],Lens),
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
%proportional_grids(Obj1,Obj2,mass(N)):- once((mass(Obj1,N1),mass(Obj2,N2))),proportional_size(N1,N2,N).

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

simplify_objs(I,O):-is_list(I),!,my_maplist(simplify_objs,I,O).
simplify_objs(obj(I),obj(O)):-!,simplify_objs_l(I,O).
simplify_objs(F,F).

simplify_objs_l(obj(I),O):- is_list(I),!,simplify_objs_l(I,O).
simplify_objs_l(I,O):- include(compound,I,M1),sort_obj_props(M1,M2),my_maplist(simplify_objs_e,M2,O).
%simplify_objs(giz(g(_)),giz(g(_))).
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
:- decl_pt(prop_g,colors_cc(prefer_grid, set)).
:- decl_pt(prop_g,symmetric_types(prefer_grid, set)).


:- decl_pt(prop_o,center_term(object,loc2D)).
:- decl_pt(prop_o,loc_term(object,loc2D)).

:- assertz_if_new((is_decl_pt(prop_o, P):- is_decl_pt(prop_g, P))).



:- include(kaggle_arc_footer).




