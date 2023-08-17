must_min_unifier(A,B,D):- must_det_ll(min_unifier_e(A,B,D)).
%min_unifier_e(A,B,C):- compound(B),maybe_extract_value(B,BB), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),min_unifier(AABB,B,C),!.
%min_unifier_e(B,A,C):- compound(B),maybe_extract_value(B,BB), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),min_unifier(AABB,B,C),!.
min_unifier_e(A,B,C):- maybe_extract_value(B,BB), \+ maybe_extract_value(A,_), min_unifier(A,BB,C).
min_unifier_e(B,A,C):- maybe_extract_value(B,BB), \+ maybe_extract_value(A,_), min_unifier(BB,A,C).
min_unifier_e(A,B,C):- min_unifier(A,B,C),nonvar(C),!.
%min_unifier_e(A,B,C):- compound(B),maybe_extract_values(B,BB), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),must_min_unifier(AABB,B,C),!.
%min_unifier_e(B,A,C):- compound(B),maybe_extract_values(B,BB), \+ maybe_extract_values(A,_), c_proportional(A,BB,AABB),must_min_unifier(AABB,B,C),!.
min_unifier_e(_,_,_).

some_min_unifier(X,X):- \+ compound(X),!.
some_min_unifier([A|List],Term):- some_min_unifier_3(A,List,Term).

can_unfy_already(A,B):- \+ \+ A = B.
some_min_unifier_3(A,[B|List],O):- min_unifier(B,A,C),nonvar(C),some_min_unifier_3(C,List,O),!.
some_min_unifier_3(A,[B|List],O):- relax_hint(A,AA),nonvar(AA),maplist(can_unfy_already(AA),[B|List]), some_min_unifier_3(AA,List,O),!.
some_min_unifier_3(A,List,A):- my_maplist(can_unfy_already(A),List),!.


is_a_min_unifier(A,B,C):- B==strict,A==loose,!,C=A.
is_a_min_unifier(A,B,C):- A==fg,B\==unkC,B\==wbg,!,C=A.
is_a_min_unifier(A,_,C):- plain_var(A),!,C=A.
is_a_min_unifier(A,B,C):- compound(A),A=trim(BB),B==BB,!,C=A.

min_unifier(A,B,C):- A=@=B,!,C=A.
min_unifier(A,B,C):- is_a_min_unifier(A,B,C),!.
min_unifier(B,A,C):- is_a_min_unifier(A,B,C),!.
min_unifier(A,B,C):- min_unifier_u(A,B,C),!.
/*

min_unifier(A,B,A):- plain_var(B),!.
min_unifier(A,B,B):- plain_var(A),!.
*/

min_unifier_n(A,B,D):- number(A),number(B),!,c_proportional(A,B,D).
min_unifier_n(A,B,D):- min_unifier(A,B,D).



min_unifier_u(A,B,_):- (\+ compound(A); \+ compound(B)),!.
min_unifier_u(A,B,AA):- is_grid(A),is_grid(B),!,min_grid_unifier(A,B,AA),!.
min_unifier_u(A,B,AA):- is_list(A),is_list(B),!,min_list_unifier(A,B,AA),
  ignore((length(A,AL),length(B,AL),length(AA,AL))).
min_unifier_u(A,B,AA):- is_cons(A),is_cons(B),!,min_list_unifier(A,B,AA),!.
%min_unifier(A,B,C):- is_list(A),sort_safe(A,AA),A\==AA,!,min_unifier(B,AA,C).
min_unifier_u(A,B,R):- compound(A),compound(B),
 compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
 my_maplist(min_unifier,AA,BB,RR),compound_name_arguments(R,F,RR).

min_unifier_u(A,B,R):- relax_hint(A,R),\+ (B \= R),!.



 
relax_hint(G,G):- (\+ compound(G)) -> !; true.
relax_hint(X1,X2):- verbatum_unifiable(X1), !, X2=X1.
%relax_hint(rev(G),rev(GG)):- !, relax_hint(G,GG).
%relax_hint(mono(G),mono(GG)):- !, relax_hint(G,GG).
%relax_hint(iz(G),iz(GG)):- relax_hint(G,GG).
%relax_hint(info(G),info(GG)):- relax_hint(G,GG).
relax_hint(P,PP):- compound_name_arguments(P,F,[G]),compound_name_arguments(PP,F,[GG]),relax_hint(G,GG).
relax_hint(cg(W,G),cg(W,GG)):- !, relax_hint(G,GG).
relax_hint(G,GG):- compound(G), duplicate_term(G,GG),arg(N,G,E),relax_arg(E,Hints),nb_setarg(N,GG,Hints).
%relax_hint(G,GG):- functor(G,F,A),functor(GG,F,A).

relax_arg(E,C):- is_color(E),!,relax_color_arg(E,C).
%relax_arg(E,_):- var(E),!,fail.
relax_arg(E,E):- var(E) -> !; true.
relax_arg((G),(GG)):- relax_hint(G,GG).
relax_arg(E,len(L)):- is_list(E),length(E,L).
relax_arg(_,_).


min_grid_unifier(A,B,_):- (\+ is_list(A) ; \+ is_list(B)),!.
min_grid_unifier(A,B,[E1|C]):- select(E1,A,AA),select(E2,B,BB), E1=@=E2 ,!,min_grid_unifier(AA,BB,C).
min_grid_unifier(A,B,[E |C]):- select(E1,A,AA),select(E2,B,BB),min_unifier(E1,E2,E),!,min_grid_unifier(AA,BB,C).
min_grid_unifier(_,_,_).


min_list_unifier(A,B,A):- A=@=B,!.
min_list_unifier(A,B,_):- ( \+ compound(A); \+ compound(B) ),!.
min_list_unifier(A,B,A):- is_list(A),is_list(B), sort_safe(A,AA),sort_safe(B,BB),BB=@=AA,!.
min_list_unifier([A|AA],[B|BB],[A|CC]):- A=@=B,min_list_unifier(AA,BB,CC).
min_list_unifier([A|AA],[B|BB],[C|CC]):- A\=@=B,min_list_unifier(AA,BB,CC),min_unifier(A,B,C),!.
min_list_unifier(A,B,[EC|C]):- is_list(A),is_list(B),  select_two(A,B,E1,E2,AA,BB), min_unifier(E1,E2,EC) ,!,min_list_unifier(AA,BB,C).


min_list_unifier(A,_,_):- (\+ is_list(A), \+ is_cons(A)),!.
min_list_unifier(_,A,_):- (\+ is_list(A), \+ is_cons(A)),!.
min_list_unifier(A,B,_):- (\+ is_list(A) ; \+ is_list(B)),!.

%min_list_unifier([E1|AA],[E2|BB],[EC|C]):- min_unifier(E1,E2,EC) ,!,min_unifier(AA,BB,C).
min_list_unifier(A,B,[E1|C]):- nonvar(A),nonvar(B), select(E1,A,AA),nonvar(E1),select(E2,B,BB),nonvar(E2), E1=@=E2 ,!,min_list_unifier(AA,BB,C).
%min_list_unifier([_|A],[_|B],[_|C]):- !,min_list_unifier(A,B,C).
%min_list_unifier([_],[_|B],[_|B]):-!.
%min_list_unifier([_|B],[_],[_|B]):-!.
min_list_unifier(_,_,_):-!.
%min_unifier(A,B,C):- is_list(B), is_list(A), length(B,L), length(A,L), length(C,L).




some_min_unifier_allow_nil(MUL,Out):-
  include(\==([]),MUL,MULL),
  some_min_unifier(MULL,Out).


make_unifiable(A1,A2):- make_unifiable0(A1,O),!,A2=O.

make_unifiable0(C1,_):- \+ compound(C1),fail.
make_unifiable0(A1,A2):- var(A1),!,A2=A1.
make_unifiable0(pg(A,B,C,_),pg(A,B,C,_)):-!.
make_unifiable0(cc(C,_),cc(C,_)):-!.
make_unifiable0(iz(C1),iz(C2)):- !, make_unifiable(C1,C2).
make_unifiable0(giz(C1),giz(C2)):- !, make_unifiable(C1,C2).
make_unifiable0(Cmp,CmpU):-  Cmp=..[F|List1], 
  append(Left1,[C1],List1),append(Left2,[C2],List2), CmpU=..[F|List2],
  my_maplist(unifiable_cmpd_else_keep,Left1,Left2),
  unifiable_cmpd_else_var(C1,C2),!.
make_unifiable0(C1,C2):- functor(C1,F,A),functor(C2,F,A).

%unifiable_cmpd_else_keep(A1,A2):- unifiable_cmpd_keep(A1,A2).
unifiable_cmpd_else_keep(Num,_):- number(Num),!.
unifiable_cmpd_else_keep(A1,A1).

unifiable_cmpd_else_var(A1,A2):- unifiable_cmpd_keep(A1,A2), \+ ground(A2).
unifiable_cmpd_else_var(_,_).


unifiable_cmpd_keep(A1,A2):- var(A1),!,A2=A1.
unifiable_cmpd_keep(cc(C,_),cc(C,_)):-!.
unifiable_cmpd_keep(A1,A2):- compound(A1), \+ is_list(A1), make_unifiable(A1,A2),!.


make_unifiable_with_ftvars(C1,C2):- functor(C1,F,A),functor(C2,F,A),numbervars(C2).




% Helper predicate to create a unifiable version of a term
make_unifiable_u(X1,X2):- verbatum_unifiable(X1), !, X2=X1.
make_unifiable_u(X1,X2):- maybe_deref_value(X1,E1), !, make_unifiable_u(E1,X2).
make_unifiable_u(P,U):- copy_term(P,PP),make_unifiable_u1(PP,U),!.
make_unifiable_u1(Atom,U):- is_ftVar(Atom),!,Atom=U.
make_unifiable_u1(X1, X2):- verbatum_unifiable(X1), !, X2=X1.
make_unifiable_u1(Atom,U):- atomic(Atom),!,freeze(U,atomic(U)).
make_unifiable_u1(link(sees(L),A),link(sees(U),B)):- !, maplist(make_unifiable_u,[A|L],[B|U]),!.

make_unifiable_u1(P,U):- assume_prop(P),!,P=U.
make_unifiable_u1(X1,U1):- make_unifiable_cc(X1,U1),!.
make_unifiable_u1(X1,X1).

make_unifiable_ov(I,O):- make_unifiable_u(I,O),!.

make_unifiable_f(I,O):- make_unifiable_ov(I,O).
make_unifiable_f(I,O):- same_functor(I,O),!.


make_unifiable_cc(WP1,WP2):- \+ compound(WP1),!,WP2=WP1.
make_unifiable_cc(N-WP1,N-_):- \+ is_list(WP1),!.
make_unifiable_cc(N-WP1,N-WP2):- !, make_unifiable_cc(WP1,WP2).
make_unifiable_cc([H|T],[HH|TT]):- !, make_unifiable_cc(H,HH),make_unifiable_cc(T,TT).
make_unifiable_cc(cc(C,N),cc(_,N)):- integer(N), is_real_color(C),!.
make_unifiable_cc(cc(N,_),cc(N,_)):-!.
make_unifiable_cc(oid(_),oid(_)):-!.
make_unifiable_cc(recolor(N,_),recolor(N,_)):-!.
make_unifiable_cc(rotSize2D(grav,_,_),rotSize2D(grav,_,_)):-!.
%make_unifiable_cc(grid_ops(comp,_),grid_ops(comp,_)):-!.
make_unifiable_cc(iz(symmetry_type(N,_)),iz(symmetry_type(N,_))):-!.
make_unifiable_cc(pg(_,G,M,_),pg(_,UG,M,_)):-!,make_unifiable_cc(G,UG).
make_unifiable_cc(O,U):- make_unifiable(O,U).

%m_unifiers(Mode,In,Out):- \+ is_list(In),Out=In.

m_unifiers(_Mode,In, Out):- (\+ compound(In);\+ is_list(In)),!,Out=In.
m_unifiers(Mode,In,Out):- my_partition(is_debug_info,In,Skip,DontSkip), Skip\==[],!,
  m_unifiers1(Mode,DontSkip, Mid), append_sets([Mid, Skip], Out), !.
%m_unifiers(Mode,In,Out):- my_partition(assume_prop,In,Skip,DontSkip), Skip\==[],
%  m_unifiers(Mode,DontSkip, Mid), append_sets([Mid, Skip], Out), !.
m_unifiers(Mode,In, Out):-m_unifiers1(Mode,In, Out),!.

%m_unifiers(Mode,In,Out):- is_list(In), select(E,In,More),is_prop1(E),is_unbound_prop(E),make_unifiable_u(E,U),select(U,More,UMore), 
%  min_unifier(U,E,S),!,m_unifiers(Mode,[S|UMore],Out),!.
%m_unifiers(Mode,In, Out):-m_unifiers1(Mode,In, Out),!.

is_rhs_esc(Var):- \+compound(Var),!,fail.
is_rhs_esc(rhs(_)).
is_rhs_esc(group(_)).
is_rhs_esc(obj(_)).
is_rhs_esc(CL):- compound(CL), \+ is_prop1(CL),CL=..[_,L],is_list(L),!.

is_skip_over_esc(Var):- \+ compound(Var),!,fail.
is_skip_over_esc(always(_)).
is_skip_over_esc(CL):- is_rhs_esc(CL),!,fail.
is_skip_over_esc(CL):- verbatum_unifiable(CL),!.
is_skip_over_esc(Debug):- is_debug_info(Debug),!.
is_skip_over_esc(CL):- \+ is_prop1(CL), compound(CL),CL=..[_,L],is_list(L),!.

verbatum_unifiable(Var):- \+ compound(Var), !, fail.
verbatum_unifiable(always(_)).
verbatum_unifiable(CL):- is_rhs_esc(CL),!.
verbatum_unifiable(P):- functor(P,of_obj_use,_).
%verbatum_unifiable(C):- sub_cmpd('$VAR'(_), C), , !.


m_unifiers1(_Mode,In, Out):- (\+ compound(In);\+ is_list(In)),!,Out=In.

m_unifiers1(Mode,In, [E|Out]):- select(E, In, More), is_skip_over_esc(E), 
  m_unifiers1(Mode, More, Out).
m_unifiers1(Mode,In, [E|Out]):- Mode==lhs,select(E, In, More), is_unbound_prop(E), 
  my_partition(same_prop_names(E),More,_Sames,Others),!,
  m_unifiers1(Mode,Others, Out).
m_unifiers1(Mode,In, [S|Out]):- Mode==lhs,select(E, In, More), is_prop1(E),
  my_partition(same_prop_names(E),More,Sames,Others),
  some_min_unifier([E|Sames],S),!,
  m_unifiers1(Mode,Others, Out).
m_unifiers1(Mode,In, Out):- select(E, In, More), is_rhs_esc(E),
  my_partition(same_functor(E),More,Sames,Others),
  some_min_unifier([E|Sames],S),!,
  m_unifiers1(Mode,[S|Others], Out).
m_unifiers1(_Mode,IO, IO).


  %m_unifiers(Mode,[S|Others], Out),  warn_and_fail_if_empty(Out).
%m_unifiers(Mode,In,Out):- select(E,In,More),is_prop1(E),make_unifiable_u(E,U),select(U,More,UMore),other_val(E,U),merge_props(Mode,U,E,S),!,m_unifiers(Mode,[S|UMore],Out).
/*

% ALSO m_unifiers(Mode,In, Out):- In=Out.

% ALSO m_unifiers1(Mode,In,Out):- \+ compound(In),!,In=Out.
m_unifiers1(Mode,In,Out):-  once(( is_list(In), In\==[], select(E,In,More),is_prop1(E),make_unifiable_u(E,U),select(U,More,UMore),other_val(E,U),
  min_unifier(U,E,S))), (E\=@=S;U\=@=S),!,m_unifiers1(Mode,[S|UMore],Out).
% ALSO m_unifiers1(Mode,In, Out):- once(( is_list(In), select(E, In, More), is_prop1(E), make_unifiable_u(E, U), select(U, More, UMore),
% ALSO   min_unifier(U, E, S))), (E\=@=S;U\=@=S), !, m_unifiers1(Mode,[S|UMore], Out), !, warn_and_fail_if_empty(Out).

m_unifiers1(Mode,In, Out):- is_list(In), In\==[], select(E, In, More), is_prop1(E), select(U, More, UMore), is_prop1(U),
  other_val(E,U), same_prop_names(E, U), must_det_ll((min_unifier(E,U, S), m_unifiers1(Mode,[S|UMore], Out), !, warn_and_fail_if_empty(Out))).
m_unifiers1(Mode,[S|In],[S|Out]):-  m_unifiers(Mode,In, Out).
*/


%o_unifiers(In,Out):- select(E,In,More),is_prop1(E),make_unifiable(E,U),select(U,More,UMore),other_val(E,U),the_or_unifier(U,E,S),!,o_unifiers([S|UMore],Out).
o_unifiers(IO,IO). 
the_or_unifier(U,E,(U;E)).


merge_props(Mode,S1,S2,S):- my_partition(is_debug_info,S1,SP1,SO1),my_partition(is_debug_info,S2,SP2,SO2),
  the_min_unifier0(Mode,SO1,SO2,SO),append_vsets([SO,SP1,SP2],S).

  

some_rhs_unifier(_Mode,[E],E):-!.
some_rhs_unifier(_Mode,[],[]):-!.
some_rhs_unifier(Mode,[SF1,SF2|More],E):- 
 must_det_ll((functor(SF1,F,1), 
  maplist(arg(1),[SF1,SF2],[List1,List2]),
  merge_props(Mode,List1,List2,List12),!,
  SF12 =..[ F| List12],
  some_rhs_unifier(Mode,[SF12|More],E))).
    

the_min_unifier0(Mode,S1,S2,S):- 
  the_min_unifier1(S1,S2,SA),
  m_unifiers(Mode,SA,SB),!,
  variant_list_to_set(SB,S).

the_min_unifier1(S1,S2,S):- \+ is_list(S1), \+ is_list(S2),!,min_unifier(S1,S2,S).
the_min_unifier1(S1,S2,[E|S]):- 
   select(E1,S1,S1R),
   same_named_functor(E1,E2Pos),
   also_neg(E2Pos,E2),
   select(E2,S2,S2R),
   %make_unifiable_u(E1,E2),
   other_val(E1,E2),%min_unifier(E1,E2,E),!,
   min_unifier(E1,E2,E),
   the_min_unifier1(S1R,S2R,S).
the_min_unifier1(S1,S2,S):- append(S1,S2,S),!.



same_named_functor(\+ E1,E2):- !, compound(E1),same_functor(E1,E2),!.
same_named_functor(E1,E2):- same_functor(E1,E2).

also_neg(E1, E1).
also_neg(E1,\+ E2):- same_functor(E1,E2).



merge_vals(A,B,C):- atom(A),!,A==B,C=A.
merge_vals(A,B,C):- A=@=B,!,C=A.
merge_vals(A, A, A) :- !.
merge_vals(A,B,C):- A==[],!,B=C.
merge_vals(A,B,C):- B==[],!,A=C.

merge_vals(A,B,C):- is_obj_props(A),is_obj_props(B),!,merge_props(lhs,A,B,C).
merge_vals([A1,A2],[B],[C1,C2]):-  !, merge_vals(A1,B,C1),merge_vals(A2,B,C2).
merge_vals([A|AA],[B|BB],[C|CC]):- !, merge_vals(A,B,C), merge_vals(AA,BB,CC).

merge_vals(prop(Name,A),prop(Name,B),prop(Name,C)):- !, merge_vals(A, B, C).
merge_vals(prop(Name, A1, A2),prop(Name, B1, B2), prop(Name, C1, C2)):- !,
  merge_vals(A1, B1, C1),merge_vals(A2, B2, C2).

merge_vals(A,B,C):- ( \+ compound(A) ; \+ compound(B)),!, flatten_sets([A,B],C),!. 
merge_vals(T+A,T+B,C):-!,must_det_ll((C=(T+A+B))).

merge_vals(A,B,A):- functor(A,F,_),must_be_identical(F),!,A=@=B.

merge_vals(info(A),info(B),info(C)):- !, merge_list_vals(A,B,C).

merge_vals(A,B,C):- is_valid_testname(A),!,A=B,A=C.
%merge_vals(A,B,C):- good__rhs(A),!,same_rhs_operation(A,B),A=C.
%info([step(Step),is_swapped_lr(IsSwapped),ctx(Ctx),why(TypeO),testid(TestID),example(ExampleNum)])
merge_vals(A,B,C):-
  A =  ac_unit(TestID, IO, P1, PSame1),
  B =  ac_unit(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(lhs,PSame1,PSame2,PSame),!,
  C =  ac_unit(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_db_unit(TestID, IO, P1, PSame1),
  B =  ac_db_unit(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(lhs,PSame1,PSame2,PSame),!,
  C =  ac_db_unit(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_rules(TestID, IO, P1, PSame1),
  B =  ac_rules(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(lhs,PSame1,PSame2,PSame),!,
  C =  ac_rules(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_listing(TestID, IO, P1, PSame1),
  B =  ac_listing(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(lhs,PSame1,PSame2,PSame),!,
  C =  ac_listing(TestID, IO, P1, PSame).

/*
merge_vals(Rule1,Rule2,NewRule):-
  r(Type1,LHS1,RHS1,S1,L1,R1, Ex1, Step1) = Rule1,
  r(Type2,LHS2,RHS2,S2,L2,R2, Ex2, Step2) = Rule2,
  combine_rule(do_requires,
              Step1,Type1,LHS1,RHS1,S1,L1,R1, 
              Step2,Type2,LHS2,RHS2,S2,L2,R2,    
              Step, Type, LHS, RHS, S ,L ,R  ),!,
  r(Type,LHS,RHS,S ,L ,R ,Ex1+Ex2,Step) = NewRule.
*/

merge_vals(rhs(A),rhs(B),rhs(C)):-  same_rhs_operation(A,B),!,merge_vals(A,B,C).
merge_vals(always(A),always(B),always(C)):- same_rhs_operation(A,B),!,merge_vals(A,B,C).
merge_vals(rhs(A),rhs(B),rhs(C)):-  merge_vals(A,B,C).
merge_vals(always(A),always(B),always(C)):- merge_vals(A,B,C).

merge_vals(A,B,C):- compound(A),compound(B),var(C),
  compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
  maplist(merge_vals,AA,BB,CC),!, compound_name_arguments(C,F,CC).
%merge_vals(obj(A),obj(B),obj(C)):- is_list(A),is_list(B),!,merge_props(Mode,A,B,C).
merge_vals(A,B,C):-  flatten_sets([A,B],C),!. 

same_rhs_operation(A,B):- is_list(A),is_list(B),!.
same_rhs_operation(A,B):- (\+ compound(A) ; \+ compound(B)),!, A=@=B.
same_rhs_operation(A,B):-
  compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
  maplist(same_rhs_operation,AA,BB),!.







% Check if two values have the same property names but are not equal

%into_rhs(edit(_,_,_,R),P):- !, into_rhs(R,P).
maybe_deref_value(X1,_):- \+ compound(X1),!,fail.
maybe_deref_value(Term,_):- is_rhs_prop(Term),!,fail.
maybe_deref_value(Term,Prop):- maybe_deref_value1(Term,Prop),!, Term\=@=Prop.

maybe_deref_value1(X1,_):- \+ compound(X1),!,fail.
maybe_deref_value1(Term,_):- is_grid(Term),!, fail.
maybe_deref_value1(Term,_):- is_object(Term),!, fail.
maybe_deref_value1(Term,Prop):- is_list(Term),!, reverse(Term,RArgs),member(CProp,RArgs),maybe_deref_value1(CProp,Prop).
maybe_deref_value1(Term,Out):- is_rhs_prop(Term),!,Out = Term,!.
maybe_deref_value1(edit(_,_,E1),Y1):- ensure_deref_value(E1,Y1).
maybe_deref_value1(edit(_,_,_,E1),Y1):- ensure_deref_value(E1,Y1).
maybe_deref_value1(\+ Term, \+ Out):-  !,Out = Term,!.
maybe_deref_value1(\+ Term, \+ Prop):- !, ensure_deref_value(Term,Prop).
maybe_deref_value1(Term,Prop):- compound_name_arguments(Term,_,Args),maybe_deref_value1(Args,Prop),!.
maybe_deref_value1(Term,Prop):- sub_term(Prop,Term),Prop\==Term, compound(Prop),is_rhs_prop(Prop).

maybe_deref_name(Term,_):- is_rhs_prop(Term),!,fail.
maybe_deref_name(\+ Term,Prop):- !, ensure_deref_value(Prop, Term). 
maybe_deref_name(Term,Prop):- !, maybe_deref_value(Term,Prop). 


%maybe_deref_value(X1,Y1):- compound(X1), once(into_rhs(X1,E1)), E1\=@=X1,!,ensure_deref_value(E1,Y1).

ensure_deref_value(X1,E1):- maybe_deref_value(X1,E1),!.
ensure_deref_value(X1,X1).

at_least_one_overlap(DSame,PSame):-
  member(DS,DSame),member(S,PSame),
  about_same_property(DS,S),!.

about_same_property(DS,S):- \+ \+ (same_name_and_value(DS,S);( \+ DS\=S )).

same_name_and_value(P,DP):- maybe_deref_value(P,DR),!,same_name_and_value(DP,DR).
same_name_and_value(P,DP):- \+ \+ (DP=@=P;other_val(P,DP)).

competing_rhs_property(P,DP):- maybe_deref_name(P,DR),!,competing_rhs_property(DP,DR).
competing_rhs_property(P,DP):- about_same_property(P,DP),!.
competing_rhs_property(P,DP):- compound_name_arguments(P,F,Args),compound_name_arguments(DP,DF,DArgs),
  F==DF,!,last(Args,LA),last(DArgs,DLA),LA=@=DLA.
competing_rhs_property(X2,X1):- is_copy_or_delete_object(X1),!, \+  is_copy_or_delete_object(X2).
competing_rhs_property(_ ,X1):- is_copy_or_delete_object(X1),!.


make_unifiable_maybe_neg(S, D):- 
  freeze(D, about_same_property(S,D)).

make_unifiable_rhs(S, D):- 
  freeze(D, about_same_property(S,D)).

different_rhs_simular_lhs_ele(TestID, IN_OUT, P, S, U, DSame, D):-
  make_unifiable_rhs(P, U),
  ac_rules(TestID, IN_OUT, U, DSame),P\=@=U,other_val(P,U),
  make_unifiable_maybe_neg(S, D), 
  member(D,DSame).



is_rhs_prop(Prop):- type_prop(_,Prop),!.
is_rhs_prop(Prop):- is_copy_or_delete_object(Prop),!.

is_copy_or_delete_object(P):- compound(P),!,compound_name_arity(P,F,_),(F==perfect_copy;F==delete_object),!.
is_1st_perfect_copy(P):- compound(P), P= perfect_copy(_,H),H==1.
is_perfect_copy(P):- compound(P), P= perfect_copy(_,_).

has_a_value(P):- make_unifiable_u(P, U), P\=@=U.


other_val(X1,X2):- X1=@=X2,!,fail.
other_val(X1,X2):- maybe_deref_value(X1,E1), !, other_val(E1,X2).
other_val(X2,X1):- maybe_deref_value(X1,E1), !, other_val(X2,E1).

other_val(X1,X2):- negated_s_lit(X1,P1), 
  ( negated_s_lit(X2,P2) -> other_val(P1,P2) ; other_val(X2,P1)).

other_val(X1,X2):- \+ same_prop_names(X1,X2),!,fail.
other_val(X1,X2):- once((selfless_type(X1,V1),selfless_type(X2,V2))), \=@=(V1,V2),!,fail.
other_val(X1,X2):- is_color_prop(X1),is_color_prop(X2),once((specific_value(X1,V1),specific_value(X2,V2))), \+ other_val_same_types(V1,V2),!,fail.
other_val(_,_).

is_color_prop(X1):- fail,compound(X1),X1=pen(_).

other_val_same_types(X1,X2):- once((selfless_type(X1,V1),selfless_type(X2,V2))), \=@=(V1,V2),!,fail.
other_val_same_types(X1,X2):- X1=@=X2,!,fail.
other_val_same_types(_,_).

selfless_type(V1,T1):- data_type(V1,T),subst(T,V1,v,T1).


specific_value(X,V):- sub_term(V,X),V\=X,comparable_value(V).


same_prop_names(X1,X2):- maybe_deref_name(X1,E1), !, same_prop_names(X2,E1).
%same_prop_names(X2,X1):- maybe_deref_name(X1,E1), !, same_prop_names(X2,E1).
same_prop_names(X1,X2):- 
  compound(X1),compound(X2), same_functor(X1,X2),!,
  make_unifiable_u(X1,U1), make_unifiable_u(X2,U2),!,  U1 =@= U2.

is_unbound_prop(S):- var(S), !, fail.
is_unbound_prop(S):- verbatum_unifiable(S), !, fail.
is_unbound_prop(S):- make_unifiable(S,DS), S=@=DS,!.

prop_value(Prop,Value):- nonvar(Value),prop_value(Prop,Var),!,Value=Var.
prop_value(List,Value):- is_list(List),!,maplist(prop_value,List,Value),!.
prop_value(Prop,Value):- prop_first_value(Prop,Value).

prop_first_value(Prop,Value):- nonvar(Value),prop_first_value(Prop,Var),!,Value=Var.
prop_first_value(Prop,Value):- \+ compound(Prop),!,Value=Prop.
prop_first_value(\+ Prop,Value):- nonvar(Prop),!,prop_first_value(Prop,Value).
prop_first_value(_ - Prop,Value):- nonvar(Prop),!,prop_first_value(Prop,Value).
prop_first_value(List,Value):- is_list(List),!,member(Prop,List),prop_first_value(Prop,Value),!.
prop_first_value(Value,Value).

prop_name(Prop,Named):- nonvar(Named),prop_name(Prop,Var),!,Named=Var.
prop_name(Prop,Named):- make_prop_name(Prop,Named),!.
prop_name(Prop,Named):- prop_first_value(Prop,Value), value_to_name(Value,Named),!.


make_prop_name(X,Y):- \+ compound(X),!,X=Y.
make_prop_name(samez(Prop, _),samez(Named)):- !, make_prop_name(Prop,Named).
make_prop_name(pg(_,X,R,_),pg(X,R)):-!.
make_prop_name(iz(Prop),iz(Named)):- !, make_prop_name(Prop,Named).
make_prop_name(giz(Prop),giz(Named)):- !, make_prop_name(Prop,Named).
make_prop_name(Prop,Free):- make_unifiable_cc(Prop,Named),remove_free_args(Named,Free),!.
make_prop_name(Prop,Named):- functor(Prop,Named,_).
remove_free_args(Named,Free):-  copy_term(Named,Free),term_variables(Free,Vs),numbervars(Vs,666,_,[singletons(true)]).
%remove_free_args(Named,Free):- Named=..[F|Args],include(nonvar,Args,NArgs),Free=..[F|NArgs].




value_to_name(Value,Named):- nonvar(Named),value_to_name(Value,Named2),!,Named2=Named.
value_to_name(Value,Named):- \+ compound(Value),!,Value=Named.
value_to_name(Value,Named):- make_unifiable_cc(Value,Named),!.
value_to_name(Value,Named):- make_unifiable_cc(Value,UProp),
   compound_name_arguments(UProp,F,Args),
   include(nonvar,Args,OArgs),
   (OArgs ==[] -> Named=F ; compound_name_arguments(Named,F,OArgs)).
