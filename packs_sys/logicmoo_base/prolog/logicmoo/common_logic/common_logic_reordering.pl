:- module(common_logic_reordering,[]).


:- op(800,xfx,'=<>=').



combine_clauses_with_disjuncts(SET,OUT):-
  sort(SET,SORTED),combine_clauses_with_disjuncts_0(SORTED,OUT).  

combine_clauses_with_disjuncts_0([],[]).
combine_clauses_with_disjuncts_0([(H1:-B1),(H2:-B2)|SORTED],OUT):- 
  H2=@=H1,(WAS = H1:B2),
  copy_term(WAS,NOW),H1=H2,WAS=@=NOW,!,
  combine_clauses_with_disjuncts_0([(H1:- (B2 ; B1))|SORTED],OUT).
combine_clauses_with_disjuncts_0([S1|SORTED],[S1|OUT]):- 
 combine_clauses_with_disjuncts_0(SORTED,OUT).


dedupe_clauses(List,ListO):- sanity(is_list(List)),
  sort(List,ListM),
  dedupe_clauses_pass2(ListM,ListO).

dedupe_clauses_pass2([],[]).
dedupe_clauses_pass2([X,Y|ListM],ListO):- show_call(same_clauses(X,Y)),!,
  dedupe_clauses_pass2([X|ListM],ListO).
dedupe_clauses_pass2([X|ListM],[X|ListO]):-
  dedupe_clauses_pass2(ListM,ListO).
 

same_cl_test0 :-
 same_clauses(
  proven_neg(dudes(X)) ,
        ((nesc(dudes(Dudes0)),
        dif_objs(X, Dudes0))),
  proven_neg(dudes(Dudes0)),
        (nesc(dudes(X)),
        dif_objs(X, Dudes0))).

same_cl_test1:- 
    same_clauses(
   (proven_neg(different(Dudes1, Dudes3)) :-
           nesc(dudes(Dudes3)),
           nesc(dudes(Dudes1)),
           dif_objs(Dudes1, Dudes2),
           nesc(dudes(Dudes2))),
   (proven_neg(different(Dudes1, Dudes2)) :-
           dif_objs(Dudes1, Dudes3),
           nesc(dudes(Dudes1)),
           nesc(dudes(Dudes2)),
           nesc(dudes(Dudes3)))).


X =<>= Y :- X==Y,!.
X =<>= Y :- (\+compound(X);\+compound(Y)),!,fail.
L =<>= R :- L=..[F,X,Y], R=..[F,YY,XX], is_symetric_lr(F), v(X,Y) == v(XX,YY),v(X,Y) = v(XX,YY).
L =<>= R :- L=..[F,X|Y], R=..[F,XX|YY],maplist( =<>= ,[X|Y],[XX|YY]).
% dif_objs(X,Y) =<>= dif_objs(YY,XX):-  v(X,Y) == v(XX,YY).

is_symetric_lr(sameObjects).
is_symetric_lr(different).
is_symetric_lr(equal).
is_symetric_lr(equals).
is_symetric_lr(dif_objs).

same_clauses(HB1,HB2):- HB1 =<>= HB2,!.
same_clauses(HB1,HB2):- \+ \+ same_clauses0(HB1,HB2).

same_clauses0(H1B1,H2B2):-
  del_term_attr(vn,H1B1),
  del_term_attr(vn,H2B2),
  expand_to_hb(H1B1,H1,B1),
  expand_to_hb(H2B2,H2,B2),!,
  same_clauses(H1,B1,H2,B2).

same_clauses(H1,B1,H2,B2):- H1 =@= H2, H1 = H2,
  body_to_sorted_dumb(B1,BB1),
  body_to_sorted_dumb(B2,BB2),!,
   term_variables(H1:BB1,BV1),
   term_variables(H2:BB2,BV2),
   BV1=BV2,!,
  maplist(=<>=,BB1,BB2),!.

del_term_attr(Attr,Term):-attvar(Term),!,del_attr(Term,Attr).
del_term_attr(Attr,Term):-term_attvars(Term,AVs),maplist(del_term_attr(Attr),AVs).

body_to_sorted_dumb(B1,BB1):-
   conjuncts_to_list_det(B1,List),
   sort(List,BB1),!.






test_sort_body_better(Head,SET,SSET):- 
  SET=[A,B],
  body_rating(Head,A,AR),writeln(AR-A),
  body_rating(Head,B,BR),writeln(BR-B),
  predsort(nearest_to_head(Head,SET),SET,SSET),!.


sort_body_list_better(Head,SET,SSET):- 
  predsort(nearest_to_head(Head,SET),SET,SSET),!.

vbody_sort((H:-B),(H:-BO)):- !, must(sort_body_better(H,B,BO)).
vbody_sort(H,H).

sort_body_better(Head,(A,B),BodyOut):- nonvar(A), 
   conjuncts_to_list_det((A,B),List),
   list_to_set(List,SET),
   sort_body_list_better(Head,SET,SSET),
   list_to_conjuncts_det(SSET,BodyOut).
sort_body_better(_,Body,Body).


nearest_to_head(Head,_SET,Order,A,B):- 
   body_rating(Head,A,AR),
   body_rating(Head,B,BR),
   compare_along(Order,BR,AR),
   Order \== (=),!.
nearest_to_head(_Head,SET,Order,A,B):-
   nth1_eq(AR,SET,A),
   nth1_eq(BR,SET,B),
   compare(Order,AR,BR).

compare_along(Order,[A|List1],[B|List2]):-   
   ((compare(Order,A,B), Order \== ( = ) )
      -> true ; compare_along(Order,List1,List2)).
 
nth1_eq(AR,SET,A):- nth1(AR,SET,E),E==A.

body_rating(Head,A,[SC,UCR,AR,AC]):-
  term_variables(A,BV),length(BV,BC),
  term_variables(Head,HV),length(BV,HC),   
  '$expand':intersection_eq(HV,BV,Shared),length(Shared,SC),
  subtract_eq(BV,Shared,Uniq),length(Uniq,UC),UCR is - UC,
  atomics_count(A,AC),!,
   nop(AR is SC*3 - UC*2 + AC + HC +BC),
   AR is ((SC*3 - UC + AC*2 ))/(BC+HC+1).

atomics_count(A,AC):- findall(Sub,(sub_term(Sub,A),atomic(Sub)),Atoms),length(Atoms,AC).




%% sort_body( ?HBINFO, ?BB, ?BBB) is det.
%
% Sort Body.
%
sort_body(HBINFO,BB,BBB):-sort_body_0(HBINFO,BB,BBB),(BBB=@=BB->true; (expand_to_hb(HBINFO,H,_),nop(dmsg([(H:-BB),'=>',(H:-BBB)])))).




%% sort_body_0( ?VALUE1, ?SORTED, ?SORTED) is det.
%
% sort body  Primary Helper.
%
sort_body_0(_,SORTED,SORTED):-leave_as_is_logically(SORTED).
sort_body_0(HBINFO,(A,B),SORTED):-!,conjuncts_to_list_det((A,B),List),
   must_maplist_det(sort_body_0(HBINFO),List,ListIn),
   predsort(litcost_compare(HBINFO),ListIn,SortedL),
   list_to_conjuncts_det(SortedL,SORTED).
sort_body_0(HBINFO,(A;B),SORTED):-!,disjuncts_to_list((A;B),List),
   must_maplist_det(sort_body_0(HBINFO),List,ListIn),
   predsort(litcost_compare(HBINFO),ListIn,SortedL),
   list_to_conjuncts_det((;),SortedL,SORTED).
sort_body_0(_,SORTED,SORTED).




%% litcost_compare( ?HBINFO, ?Comp, ?A, ?B) is det.
%
% Litcost Compare.
%
litcost_compare(_,=,A,B):- A=@=B,!.
litcost_compare(HBINFO,Comp,A,B):-lit_cost(HBINFO,A,AC),lit_cost(HBINFO,B,BC),compare(CompC,AC,BC),
  (CompC\== (=) -> CompC = Comp ; Comp = (<)).




%% lit_cost( ?HBINFO, ?A, :GoalAC) is det.
%
% Literal Cost.
%
lit_cost(_,A,9):-isSlot(A).
lit_cost(_,A,0):- \+ compound(A),!.
lit_cost(HBINFO,A,AC):- A=..[F,ARG], is_log_op(F),!,lit_cost(HBINFO,ARG,AC0),!,
 % this removes the headvar bonus
  term_slots(A,Slots),length(Slots,SC),
  AC is AC0+SC.
lit_cost(HBINFO,A,AC):- expand_to_hb(HBINFO,H,B),
  var_count_num(A,H,SH,UH),
  var_count_num(A,B,VC,Singles),
  AC is Singles*3 + VC + UH - SH.



:- fixup_exports.

