:- module(logicmoo_ocl_and_pddl,[

  solve_files_w_ocl/2,
  test_blocks/0,test_domain/1,test_all/0,test_rest/0,test_sas/0,test_dir_files_sas/1,test_dir_files_sas/3]).



solve_files_w_ocl(DomainFile, ProblemFile):-
   must_det_l(( 
      format('~q.~n',[solve_files(DomainFile, ProblemFile)]))),
   parseDomain(DomainFile, DD),
    parseProblem(ProblemFile, PP),
    solve_files_w_ocl_pt2(DD, PP).

solve_files_w_ocl_pt2(DD, PP):-
   must_det_l(( 
    term_to_ord_term(DD, D),prop_get(domain_name,D,DName),save_type_named(domain,DName,D),
    term_to_ord_term(PP, P),prop_get(problem_name,P,PName),save_type_named(problem,PName,P),    
    reset_statistic)),
    !,
   locally(t_l:other_planner(hyhtn_solve), record_time(try_solve(PName, D,P,S),SolverTime)),
    flag(time_used_other,X,X + SolverTime),
    show_statistic(P, S),
    !.



compile_problem(Pu,P):-
 must_det_l((
 replc_structure_vars(Pu,Pu0),
  term_to_ord_term(Pu0,P),
  prop_get(init,P, UCI),
  prop_get(goal,P, UCG),
  term_to_ord_term((UCI,UCG),OT),
  copy_term_for_assert(OT,(I,G)),
  prop_set(init,P, I),
  prop_set(goal,P, G),
  prop_get(problem_name,P,PName),
  save_type_named(problem,PName,P))).

compile_domain(D,Dc):-
 must_det_l((
    prop_get(domain_name,D, DName),
    prop_get(actions,D,A),
    must_maplist(copy_term_spec,A,AC),
    prop_set(actions,D,AC),
    must_maplist(to_action5,AC,A5),
    prop_set(actions5,D,A5),
    must_maplist(save_action(DName), A5),    
    D = Dc,  % replc_structure_vars2(D,Dc),
    save_type_named(domain,DName,Dc))).

save_varnames_in_action(A,CA):-varnames_for_assert(A,CA,Vars),prop_set(varnames,CA,Vars).

% DMILES
to_action5(action(_S, _L, P, PE, NE, Assign, ActionDef,_,_),action5(ActionDef, P, PE, NE,Assign)):-!.
to_action5(Struct,Term):-
  prop_get_nvlist(Struct,[parameters=UT,preconditions=Precon,positiv_effect=Pos,negativ_effect=Neg,assign_effect=Af]),
  Term=action5(UT, Precon, Pos, Neg, Af).
     
save_action(Mt,A):- ain(actn(Mt,A)).



record_time(G,TimeUsed):- record_time(G,runtime,TimeUsed).
record_time(G,Runtime,TimeUsed):- statistics(Runtime, [B,_]),G,statistics(Runtime, [E,_]),TimeUsed is E - B.


try_solve(PN,D,P,S):- t_l:loading_files,!,pmsg((loading_files(PN):-try_solve(D,P,S))),!.
% try_solve(PN,D,P,S):- once(time_out(solve(PN,D, P, S), 3000, Result)), Result == time_out, portray_clause(hard_working:-try_solve(PN,D,P,S)),fail.
try_solve(PN,D,P,S):- gripe_time(14,time_out((solve(PN,D, P, S)), 30000, Result)),!, % time limit for a planner (was 500000)  30000
   ((\+ is_list(S)
     -> portray_clause('failed'(Result):-try_solve(PN,D,P,S)) ;
       ((Result=time_out)->portray_clause('failed'(Result):-try_solve(PN,D,P,S));true))),fail.

try_solve(PN,D,P,S):- solve(PN,D, P, S),fail.

try_solve(PN,D,P,S):-dmsg('utter_failed'(warn):-try_solve(PN,D,P,S)),!.

sdmsg(B,B):- \+ compound(B),!.
sdmsg((D:-B),SS):-B==true,!,sdmsg(D,SS).
sdmsg(B,SS):-B=..[F|A],must_maplist(sdmsg,A,AA),SS=..[F|AA].

pmsg(D):- sdmsg(D,SS),D \=@= SS, !,pmsg(SS).
pmsg(D):- compound(D),functor(D,(:-),_),!,subst(D,=,'k_===_v',SS),wdmsg(SS).
pmsg(D):- subst(D,=,'k_===_v',SS),wdmsg(SS),wdmsg((:-SS)).




hyhtn_solve(_,D, P, Solution):-
  must_det_l((
    env_clear_doms_and_tasks,
    clean_problem,
    bb_put(currentProblem, P),
    bb_put(currentDomain, D),
    prop_get(init,P, UCI),
    prop_get(goal,P, UCG),


    copy_term_for_assert((UCI,UCG),(I,G)),
    prop_get(domain_name,D,Mt),    
    must(prop_get(domain_name,P,Mt)),
    must(prop_get(types,D,Types)),
    must(prop_get(predicates,D,Preds)),
    must(prop_get(objects,P,Objects)),
    must_maplist(save_ocl_objects,Objects),
    must_maplist(save_ocl_predicates,Preds),
    must_maplist(save_ocl_types,Types),
    wdmsg(dtpo(Mt,Types,Preds,Objects)),
    prop_get(actions,D, A),
    must_maplist(save_ocl_operators,A),
    bb_put(goalState, G),        
    bb_put(fictiveGoal, G))),
    ignore(init_locol_planner_interface0(G,I,Solution)).

save_ocl_operators(A):-dmsg(save_ocl_operators(A)), % varnames_for_assert(A,CA,Vars),
   must(( 
      prop_get_nvlist(A,
         [(preconditions)=Precon,positiv_effect=Pos,negativ_effect=Neg, assign_effect=Af, (parameters)= UT, 
                 parameter_types=SPT,parameters_decl=_PDs]),
     UT=..[_|ARGS],     
     SPT=..[_|PTs], 
     nop(must_maplist(record_var_names,Vars)),
     must_maplist(create_hint,PTs,ARGS,ARGHints),
     %conjuncts_to_list(Call,MORE),
     append(ARGHints,Precon,M0),
     append(M0,Af,PrecondHints),
     append(Pos,Neg,POSNEG),
     append(PrecondHints,POSNEG,ALLHINTS),
     append(POSNEG,PrecondHints,REVALLHINTS),
     to_ssify(ALLHINTS,se,PrecondHints,SE),
     get_one_isa(S,X,REVALLHINTS),
     SC = sc(S,X,Neg=>Pos),
     OP = operator(UT,SE,SC,[]),
     varnames_for_assert(OP,COP,Vars),
     env_aif(COP))).

env_aif(G):-functor(G,F,_),wdmsg(F:-G), assertz_new(ocl:G).

save_ocl_predicates(Decl):-dmsg(save_ocl_predicates(Decl)),prop_get(parameter_types,Decl,PTDecl),   
   env_aif(predicates([PTDecl])),PTDecl=..[_F|PTypes],must_maplist(save_ocl_types,PTypes).

save_ocl_types(Atom):- atom( Atom ),!, save_ocl_types([Atom]-type).
save_ocl_types(Obj):- Obj=..[Type,List],!,save_ocl_types(List-Type).
save_ocl_types(Type-Type):-!.
save_ocl_types([Type]-Type):-!.
save_ocl_types(List-Type):- atom(List),!,save_ocl_types([List]-Type).
save_ocl_types(List-Type):- Type==type-> save_ocl_objects(type(List))->save_ocl_types(List-top)->save_ocl_types(List-primitive_sorts).
save_ocl_types(List-Type):-  env_aif(sorts(Type,List)),  
    (Type==non_primitive_sorts;true;save_ocl_types(Type-non_primitive_sorts)).

save_ocl_objects(Atom):- atom( Atom ),!, save_ocl_objects([Atom]-top).
save_ocl_objects(Type-Type):-!.
save_ocl_objects(_-top):-!.
save_ocl_objects(_-type):-!.
save_ocl_objects([Type]-Type):-!.
save_ocl_objects(List-Type):- atom(List),!,save_ocl_objects([List]-Type).
% save_ocl_objects(List-Type):- Type==top-> !.
save_ocl_objects(Obj):- Obj=..[Type,List],!,save_ocl_objects(List-Type).
save_ocl_objects(List-Type):- env_aif(objects(Type,List)).
% FILENAME:  domain.pl 
% :- module(pdd_domain, [pdd_domain/2, pdd_domain/3, pddl_not_equal/2, pddl_bind/1]).

% :- expects_dialect(sicstus).

:- use_module(library(when)).
% :- use_module(library(atts)).
:- use_module(library(ordsets), [
        ord_intersection/3,
        ord_intersect/2,
        ord_union/3,
        ord_union/2,
        %ord_member/2,
        %ord_nonmember/2,
        ord_add_element/3,
        ord_del_element/3,
        list_to_ord_set/2
   ]).

% :- use_module(library(sets), [ del_element/3 ]).

sicstus_get_atts(V,As):- get_attrs(V,As).
sicstus_put_atts(V,As):- put_attrs(V,As).
%:- attribute(pddl_dom/2).
%:- attribute forbidden/1.

domain3:verify_attributes(Var, Other, Goals) :-
    sicstus_get_atts(Var, pddl_dom(Da, Fa)),             % are we involved?
    !,
    (   var(Other) ->                       % must be attributed then
        (   sicstus_get_atts(Other, pddl_dom(Db, Fb)) -> %   has a pdd_domain?
            (my_del_element2(Var, Fb, _) -> 
                 !, 
                 fail
                 ;
                 true
            ),
            ord_intersection(Da, Db, Dc),
            ord_union(Fa, Fb, Fc),
            Dc = [El|Els],              % at least one element
            (   Els = [] ->             % exactly one element
                Goals = [Other=El]      % implied binding
                ;   
                Goals = [],
                sicstus_put_atts(Other, pddl_dom(Dc, Fc))% rescue intersection
            )
            ;
            Goals = [],
            sicstus_put_atts(Other, pddl_dom(Da, Fa))    % rescue the pdd_domain
        )
        ;
        Goals = [],
        ord_intersect([Other], Da),      % value in pdd_domain?
        delete_from(Fa, Var, Other),
        sicstus_put_atts(Var, pddl_dom([Other], Fa)),
%        my_del_element(Var, Fa, NewFa),
        bind_all(Fa)
    ).
domain3:verify_attributes(_, _, []).                % unification triggered
                                            % because of attributes
                                            % in other modules

attribute_goal(Var, pdd_domain(Var,Dom, F)) :-     % interpretation as goal
    sicstus_get_atts(Var, pddl_dom(Dom, F)).

pdd_domain(X, Dom) :-
    pdd_domain(X, Dom, _).
pdd_domain(X, List) :-
    list_to_ord_set(List, Set),
    Set = [El|Els],                     % at least one element
    (   Els = [] ->                     % exactly one element
        X = El                          % implied binding
        ;
        sicstus_put_atts(Fresh, pddl_dom(Set, [])),
        X = Fresh                       % may call
                                        % domain3:verify_attributes/3
    ).

pdd_domain(X, Dom, F) :-
    var(Dom),
    !,
    sicstus_get_atts(X, pddl_dom(Dom, F)).

delete_from([], _, _).
delete_from([A|T], V, Value):-
    (A==V ->
        true
        ;
        sicstus_get_atts(A, pddl_dom(Ad, Af)),
        my_del_element(Value, Ad, NewAd),
        my_del_element(V, Af, NewAf),
        sicstus_put_atts(A, pddl_dom(NewAd, NewAf))
    ),
    delete_from(T, V, Value).

my_del_element(_, [], []).
my_del_element(E, [H|T], R):-
    E==H,
    !,
    my_del_element(E, T, R).
my_del_element(E, [H|T], [H|R]):-
    my_del_element(E, T, R).

my_del_element2(E, [H|T], R):-
    E==H,
    !,
    my_del_element(E, T, R).
my_del_element2(E, [H|T], [H|R]):-
    my_del_element2(E, T, R).


pddl_not_equal([], []).
pddl_not_equal(A, B):-
    ground(A), ground(B),
    !,
    A\=B.

pddl_not_equal(A, B):-
    ground(A),
    !,
    pddl_not_equal(B, A).
pddl_not_equal(A, B):-
    var(A), ground(B),
    !,
    sicstus_get_atts(A, pddl_dom(Ad, Fa)),
    ord_del_element(Ad, B, NewAd),
    sicstus_put_atts(A, pddl_dom(NewAd, Fa)),
    pddl_bind(Fa).
pddl_not_equal(A, B):-
    A==B,
    !,
    fail.
pddl_not_equal(A, B):-
    var(A), var(B),
    sicstus_get_atts(A, pddl_dom(Da, Fa)),
    sicstus_get_atts(B, pddl_dom(Db, Fb)),
%    ord_union([Fa,Fb,[A,B]], F),
    ord_union([[B],Fa], Faa),
    ord_union([[A],Fb], Fbb),
    sicstus_put_atts(A, pddl_dom(Da, Faa)),
    sicstus_put_atts(B, pddl_dom(Db, Fbb)),
    ord_union([[A],Fa], Faaaa),
    ord_union([[B],Fb], Fbbbb),
    pddl_bind(Faaaa),
    pddl_bind(Fbbbb).
pddl_not_equal([Ha|Ta], [Hb|Tb]):-
    !,
    pddl_not_equal(Ha, Hb),
    pddl_not_equal(Ta, Tb).
pddl_not_equal(A, B):-
    compound(A), compound(B),
    !,
    A =.. [Fa|Pa], B=..[Fb|Pb],
    (Fa=Fb ->
        pddl_not_equal(Pa, Pb)
        ;
        true
    ).


set_forbidden([], _).
set_forbidden([H|T], F):-
    sicstus_get_atts(H, pddl_dom(D, _)),
    sicstus_put_atts(H, pddl_dom(D, F)),
%    write(H-D-F),nl,
    set_forbidden(T, F).

bind_all([]).
bind_all([H|T]):-
  (var(H) ->
       pddl_bind([H])
       ;
       true
  ),
  bind_all(T).

pddl_bind(F):-
    setof(B, solvable(F, [], B), Bs),
    rotate(Bs, RB),
    bind_value(F, RB).

bind_value([], _).
bind_value([H|T], [B|Bs]):-
    (var(H) ->
        list_to_ord_set(B, OB),
        (OB=[VB] ->
            H=VB
            ;
            sicstus_get_atts(H, pddl_dom(_, Hf)),
            sicstus_put_atts(H, pddl_dom(OB, Hf))
        )
        ;
        true
   ),
   bind_value(T, Bs).

rotate([[]|_], []).
rotate(S, [F|Fs]):-
    first(S, F, R),
    rotate(R, Fs).

first([], [], []).
first([[F|T]|T2], [F|Fs], [T|R]):-
    first(T2, Fs, R).

solvable([], _, []).
solvable([H|T], FV, [M|S]):-
    (var(H) ->
        sicstus_get_atts(H, pddl_dom(Hd, _)),
        member(M, Hd),
        ordsets:ord_nonmember(M, FV),
        ord_add_element(FV, M, NewFV)
        ;
        NewFV=FV
    ),
    solvable(T, NewFV, S).
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 


prop_merge_svo(Struct,Name,Value):-prop_merge(Name,Struct,Value).


domain_name_for_ocl(Name):- use_local_pddl, no_repeats(domain_name0(Name)).
domain_name0(Name):- bb_get(currentDomain, D),prop_get(domain_name,D,Name).
domain_name0(Name):- bb_get(currentProblem, P),prop_get(domain_name,P,Name).
domain_name0(Name):- user:is_saved_type(domain,Name,_).
domain_name0(Name):- user:is_saved_type(problem,_,P),prop_get(domain_name,P,Name).

:- export(problem_name/1).
problem_name(Name):- use_local_pddl, no_repeats(problem_name0(Name)).
problem_name0(Name):- bb_get(currentProblem, P),prop_get(problem_name,P,Name).
problem_name0(Name):- user:is_saved_type(problem,Name,_).


/* EXAMPLE OCLh

% Sorts
sorts(chameleonWorld,primitive_sorts,[door,flexarium,chameleon,box,substrate]).
*/ 

sorts_for_ocl(DName,primitive_sorts,TypesList):-  use_local_pddl, pddl_sorts(DName,primitive_sorts,TypesList).
pddl_sorts(DName,primitive_sorts,TypesList):- bb_get(currentDomain, D),prop_get(domain_name,D,DName),!,prop_get(types,D,TypesList).
pddl_sorts(DName,primitive_sorts,List):-nonvar(DName),findall(S,is_a_type(DName,S),List).


is_a_type(D,S):- use_local_pddl, loop_check(is_a_type0(D,S)).
is_a_type0(D,S):-sorts(D,_,L),member(S,L).
is_a_type0(Name,S):-bb_get(currentProblem,P),prop_get(domain_name,P,Name),objects_3(P,S,_).


pname_to_dname(t7,chameleonWorld).
pname_to_dname(PName,DName):-name_to_problem_struct(PName,P),!,prop_get(domain_name,P,DName).
pname_to_dname(_,DName):-domain_name(DName).

% Objects
objects_for_ocl(Name,Type,List):- use_local_pddl, name_to_problem_struct(Name,P),
   prop_get(objects,P,ObjsDef),
   member(Objs,ObjsDef),Objs=..[Type,List].

/* EXAMPLE OCLh
objects(t7,door,[door1]).
objects(t7,flexarium,[flexarium1]).
objects(t7,chameleon,[veiledChameleon]).
objects(t7,box,[box1,box2]).
objects(t7,substrate,[newsPaper1,newsPaper2]).
*/

% Predicates
predicates_for_ocl(Name,List):- use_local_pddl, name_to_domain_struct(Name,D),   
   prop_get(predicates,D,List).


/* EXAMPLE OCLh
predicates(chameleonWorld,[
    doorOpen(door),
    doorClosed(door),
    dirty(flexarium),
    clean(flexarium),
    inBox(chameleon,box),
    inHands(chameleon),
    inFlexarium(chameleon),
    boxOpen(box),
    boxClosed(box),
    insideFlexarium(substrate),
    outsideFlexarium(substrate)]).

% Object Class Definitions
substate_classes(chameleonWorld,door,Door,[
    [doorOpen(Door)],
    [doorClosed(Door)]]).
substate_classes(chameleonWorld,flexarium,Flexarium,[
    [dirty(Flexarium)],
    [clean(Flexarium)]]).
substate_classes(chameleonWorld,chameleon,Chameleon,[
    [inBox(Chameleon,_Box)],
    [inHands(Chameleon)],
    [inFlexarium(Chameleon)]]).
substate_classes(chameleonWorld,box,Box,[
    [boxOpen(Box)],
    [boxClosed(Box)]]).
substate_classes(chameleonWorld,substrate,Substrate,[
    [insideFlexarium(Substrate)],
    [outsideFlexarium(Substrate)]]).

% Atomic Invariants

% Implied Invariants

% Inconsistent Constraints

% Operators
operator(chameleonWorld,takeOutFlex(Door,Chameleon),
    % prevail
    [     se(door,Door,[doorOpen(Door)])],
    % necessary
    [     sc(chameleon,Chameleon,[inFlexarium(Chameleon)]=>[inHands(Chameleon)])],
    % conditional
    []).
operator(chameleonWorld,putInBox(Box,Chameleon),
    % prevail
    [     se(box,Box,[boxOpen(Box)])],
    % necessary
    [     sc(chameleon,Chameleon,[inHands(Chameleon)]=>[inBox(Chameleon,Box)])],
    % conditional
    []).
operator(chameleonWorld,takeOutBox(Box,Chameleon),
    % prevail
    [     se(box,Box,[boxOpen(Box)])],
    % necessary
    [     sc(chameleon,Chameleon,[inBox(Chameleon,Box)]=>[inHands(Chameleon)])],
    % conditional
    []).
operator(chameleonWorld,putInFlex(Door,Substrate,Flexarium,Chameleon),
    % prevail
    [     se(door,Door,[doorOpen(Door)]),
     se(substrate,Substrate,[insideFlexarium(Substrate)]),
     se(flexarium,Flexarium,[clean(Flexarium)])],
    % necessary
    [     sc(chameleon,Chameleon,[inHands(Chameleon)]=>[inFlexarium(Chameleon)])],
    % conditional
    []).
operator(chameleonWorld,openDoor(Door),
    % prevail
    [],
    % necessary
    [     sc(door,Door,[doorClosed(Door)]=>[doorOpen(Door)])],
    % conditional
    []).
operator(chameleonWorld,closeDoor(Door),
    % prevail
    [],
    % necessary
    [     sc(door,Door,[doorOpen(Door)]=>[doorClosed(Door)])],
    % conditional
    []).
operator(chameleonWorld,time(Flexarium),
    % prevail
    [],
    % necessary
    [     sc(flexarium,Flexarium,[clean(Flexarium)]=>[dirty(Flexarium)])],
    % conditional
    []).
operator(chameleonWorld,wash(Chameleon,Box,Door,Substrate,Flexarium),
    % prevail
    [     se(chameleon,Chameleon,[inBox(Chameleon,Box)]),
     se(door,Door,[doorOpen(Door)]),
     se(substrate,Substrate,[outsideFlexarium(Substrate)])],
    % necessary
    [     sc(flexarium,Flexarium,[dirty(Flexarium)]=>[clean(Flexarium)])],
    % conditional
    []).
operator(chameleonWorld,addCleanNewspaper(Flexarium,Door,Chameleon,Box,Substrate),
    % prevail
    [     se(flexarium,Flexarium,[clean(Flexarium)]),
     se(door,Door,[doorOpen(Door)]),
     se(chameleon,Chameleon,[inBox(Chameleon,Box)])],
    % necessary
    [     sc(substrate,Substrate,[outsideFlexarium(Substrate)]=>[insideFlexarium(Substrate)])],
    % conditional
    []).
operator(chameleonWorld,removeDirtyNewspaper(Flexarium,Door,Chameleon,Box,Substrate),
    % prevail
    [     se(flexarium,Flexarium,[dirty(Flexarium)]),
     se(door,Door,[doorOpen(Door)]),
     se(chameleon,Chameleon,[inBox(Chameleon,Box)])],
    % necessary
    [     sc(substrate,Substrate,[insideFlexarium(Substrate)]=>[outsideFlexarium(Substrate)])],
    % conditional
    []).
operator(chameleonWorld,openBox(Box),
    % prevail
    [],
    % necessary
    [     sc(box,Box,[boxClosed(Box)]=>[boxOpen(Box)])],
    % conditional
    []).
operator(chameleonWorld,closeBox(Box),
    % prevail
    [],
    % necessary
    [     sc(box,Box,[boxOpen(Box)]=>[boxClosed(Box)])],
    % conditional
    []).


operator(chameleonWorld,removeDirtyNewspaper(Flexarium,Door,Chameleon,Box,Substrate),
    % prevail
    [     se(flexarium,Flexarium,[dirty(Flexarium)]),
     se(door,Door,[doorOpen(Door)]),
     se(chameleon,Chameleon,[inBox(Chameleon,Box)])],
    % necessary
    [     sc(substrate,Substrate,[insideFlexarium(Substrate)]=>[outsideFlexarium(Substrate)])],
    % conditional
    []).
*/

:- style_check(+singleton).

op_action(Mt, S, PTs, NPrecon, Pos, Neg, Af, UT , True):- use_local_pddl,
   loop_check(env_call(operator(Mt,UT,SE,SC,SS))),
   must_det_l((
      True = true,
      UT=..[S|ARGS],
      must_maplist(lock_var,ARGS),
      HintsIn=[],
      unss_ify(=,HintsIn,ss,SS,Af,Hints1),
      unss_ify(=,Hints1,se,SE,Precon,Hints2),
      unss_ify(=,Hints2,sc,SC,NEGPOS,Hints),
      divide_neg_pos(NEGPOS,[],[],Neg,Pos),   
      append(Precon,Neg,NPrecon),
      must_maplist(get_type_of(Mt,top,Hints),ARGS,PTs))).

lock_var(X):-when((?=(X,Y);nonvar(X)),X==Y).
unlock_var(V):-del_attrs(V).


add_wrapper(W,In , Out):-Out=..[W,In].

actn_operator(Mt,UT,SE,SC,SS):- use_local_pddl, actn(Mt,A),
 must_det_l(( 
    prop_get_nvlist(A,
       [(preconditions)=Precon,positiv_effect=Pos,negativ_effect=Neg, assign_effect=Af, (parameters)= UT, 
               parameter_types=PTs,
               (constraints)=Call,
               (varnames)=Vars]),
   UT=..[_|ARGS],
   show_call(Call),   
   must_maplist(record_var_names,Vars),
   must_maplist(create_hint,PTs,ARGS,ARGHints),
   conjuncts_to_list(Call,MORE),
   append(ARGHints,MORE,PrecondHints),
   unss_ify(=,[],PrecondHints,Precon,se,SEPs,HintsSE),
   unss_ify(=,[],HintsSE,Af,ss,ASEPs,HintsSS),
   unss_ify(add_wrapper(del),[],HintsSS,Neg,ss,NOTS,HintsNEG),
   unss_ify(add_wrapper(add),NOTS,HintsNEG,Pos,ss,NOTSPOSC,HintsPOS),
   mylist_to_set(HintsPOS,Hints),
   must_maplist(ress_ify(Mt,Def,Hints,se),SEPs,SE),
   must_maplist(ress_ify(Mt,Def,Hints,ss),ASEPs,SS),   
   must_maplist(make_rem_adds(Mt,Hints),NOTSPOSC,SC))).
  

make_rem_adds(Mt,Hints,A1-LIST,sc(Type,A1,(NEG=>POS))):-findall(N,member(del(N),LIST),NEG),findall(N,member(add(N),LIST),POS),
  must(get_type_of(Mt,top,Hints,A1,Type)).

create_hint(S,X,is_of_sort(X,S)).

mylist_to_set(HintsIn,HintsM):-must(list_to_set(HintsIn,HintsM)).

ghints(HintsIn,[],HintsIn).
ghints(HintsIn,[G|More],HintsOut):-
   ghints(HintsIn,G,HintsM),
   ghints(HintsM,More,HintsOut).
ghints(HintsIn,G,[kt(F,A),p(F)|HintsIn]):-G=..[F,A],!.
ghints(HintsIn,G,[kta(F,1,A),p(F)|Hints]):-G=..[F,A|ARGS],garg_hints(HintsIn,F,2,ARGS,Hints).

ss_other(ss).
ss_other(sc).
ss_other(se).

garg_hints(HintsIn,_,_,[],HintsIn).
garg_hints(HintsIn,F,N,[A|ARGS],[kta(F,N,A)|Hints]):- 
  N2 is N + 1, garg_hints(HintsIn,F,N2,ARGS,Hints).

unss_ify(_ ,WAS,Hints,G,_,WAS,Hints):- (nonvar(G);G==['Uninitialized']),!.
unss_ify(GT,WAS,HintsIn,G,SS,OUTS,Hints):-
  must((dess_ify(GT,WAS,HintsIn,G,SS,OUT,HintsM),!,mygroup_pairs_by_key(OUT,OUTS),!,mylist_to_set(HintsM,Hints))).

dess_ify(GT,WAS,HintsIn,G,SS,OUT,Hints):-mylist_to_set(HintsIn,HintsM),HintsIn\=@=HintsM,!,
  dess_ify(GT,WAS,HintsM,G,SS,OUT,Hints).

dess_ify( _,WAS,HintsIn,[],_SS,WAS,HintsIn).

dess_ify(GT,WAS,HintsIn,NC,_SS,OUT,HintsIn):- ( \+ compound(NC)),!,must((call(GT,NC,NCC),append(WAS,[NCC],OUT))).

dess_ify(GT,WAS,HintsIn,[G|GG],SS,OUT,HintsOut):- !,
  dess_ify(GT,WAS,HintsIn,G,SS,MID,Hints1),dess_ify(GT,MID,Hints1,GG,SS,OUT,HintsOut).

dess_ify(GT,WAS,HintsIn,A1-[GLs|GG],SS,OUT,HintsOut):- 
  dess_ify(GT,WAS,HintsIn,A1-GLs,SS,SEs,Hints1),dess_ify(GT,SEs,Hints1,A1-GG,SS,OUT,HintsOut).

dess_ify(GT,WAS,HintsIn,SSG,SS,OUT,[kt(Type,A1)|HintsOut]):- SSG=..[SS,Type,A1,GLs],
  dess_ify(GT,WAS,HintsIn,A1-GLs,SS,OUT,HintsOut).

dess_ify(GT,WAS,HintsIn,SSG,SS,OUT,[kt(Type,A1)|HintsOut]):- SSG=..[SOTHER,Type,A1,GLs],ss_other(SOTHER),
  dess_ify(GT,WAS,HintsIn,A1-GLs,SS,OUT,HintsOut).

dess_ify(GT,WAS,HintsIn,A1-G,_ ,[A1-GO|WAS],HintsOut):- ghints(HintsIn,G,HintsOut),call(GT,G,GO).

dess_ify(GT,WAS,HintsIn,G,SS,OUT,Hints):- arg(1,G,A1), dess_ify(GT,WAS,HintsIn,A1-G,SS,OUT,Hints).

ress_ify(Mt, Def, Hints,SS,A1-Gs,GO):-GO=..[SS,Type,A1,Gs],must(get_type_of(Mt,Def,Hints,A1,Type)),!.
ress_ify(_Mt,Def,_Hints,SS,A1-Gs,GO):-GO=..[SS,Def,A1,Gs],!.




get_type_of(_ , Def, Hints,A1,Type):-member(kt(K,V),Hints),A1==V,K\==Def,!,Type=K,record_var_type(A1,Type).
get_type_of(Mt, Def, Hints,A1,Type):-atom(A1),get_type_of_atom(Mt,Def,Hints,A1,Type),!.
get_type_of(Mt,_Def,_Hints,A1,Type):-nonvar(A1),loop_check(objects_3(Mt,Type,List)),member(A1,List).

:- style_check(-singleton).
get_type_of_atom(Mt,Def,Hints,veiledChameleon,chameleon).
get_type_of_atom(Mt,Def,Hints,veiledchameleon,chameleon).
get_type_of_atom(Mt,Def,Hints,A1,Type):-pname_to_dname(P,D),is_a_type(D,Type),atom_concat(Type,Num,A1),Num=_.
get_type_of_atom(Mt,Def,Hints,A1,Type):-pname_to_dname(P,D),is_a_type(D,Type),atom_concat(_,Type,A1).
:- style_check(+singleton).



divide_neg_pos([],Neg,Pos,Neg,Pos).
divide_neg_pos([A|MORE],NL2,PL2,Neg,Pos):-divide_neg_pos(A,NL2,PL2,NegM,PosM),divide_neg_pos(MORE,NegM,PosM,Neg,Pos).
divide_neg_pos(NL1=>PL1,NL2,PL2,Neg,Pos):-append(NL1,NL2,Neg),append(PL1,PL2,Pos).

/* EXAMPLE OCLh

% Methods

% Domain Tasks
planner_task(chameleonWorld,t7,
    % Goals
    [
     se(door,door1,[doorClosed(door1)]),
     se(flexarium,flexarium1,[clean(flexarium1)]),
     se(chameleon,veiledChameleon,[inHands(veiledChameleon)]),
     se(box,box1,[boxClosed(box1)]),
     se(box,box2,[boxClosed(box2)]),
     se(substrate,newsPaper1,[outsideFlexarium(newsPaper1)]),
     se(substrate,newsPaper2,[insideFlexarium(newsPaper2)])],
    % INIT States
    [
     ss(door,door1,[doorClosed(door1)]),
     ss(flexarium,flexarium1,[dirty(flexarium1)]),
     ss(chameleon,veiledChameleon,[inFlexarium(veiledChameleon)]),
     ss(box,box1,[boxOpen(box1)]),
     ss(box,box2,[boxOpen(box2)]),
     ss(substrate,newsPaper1,[insideFlexarium(newsPaper1)]),
     ss(substrate,newsPaper2,[outsideFlexarium(newsPaper2)])]).
*/

planner_task_for_ocl(Domain,PName,
  % Goals
    SEs,SSs):-  use_local_pddl,
     name_to_problem_struct(PName,P),prop_get(domain_name,P,Domain),
     must_det_l((
        prop_get(init,P, UCI),
        prop_get(goal,P, UCG),    
        copy_term_for_assert((UCI,UCG),(I,G)),       
        unss_ify(=,[],[],I,ss,SSK,HintsSS),
        unss_ify(=,[],HintsSS,G,se,SEK,Hints),
        must_maplist(ress_ify(Mt,Def,Hints,ss),SSK,SSs),
        must_maplist(ress_ify(Mt,Def,Hints,se),SEK,SEs))).

name_to_problem_struct(Name,P):- use_local_pddl, problem_name(Name),name_to_problem_struct0(Name,P).
name_to_problem_struct0(Name,P):-Name==current,!,bb_get(currentProblem,P).
name_to_problem_struct0(Name,P):-is_saved_type(problem,Name,P).
name_to_problem_struct0(Name,P):-bb_get(currentProblem,P),prop_get(problem_name,P,NameO),Name=NameO.
name_to_problem_struct0(Name,P):-loop_check(ocl_problem_struct(Name,P)).

kv_to_pddl([], []).
kv_to_pddl([_-N|T0], OUT) :- 
 	kv_to_pddl(T0,VALUES),
        append(N,VALUES,OUT).

%TODO work on later        
ocl_problem_struct(Name,P):- use_local_pddl, no_repeats(Name,planner_task(DomainName,Name,SE,SS)),
   must_det_l((
      unss_ify(=,[],[],ss,SS,OUTI,HintsM),mygroup_pairs_by_key(OUTI,IOUT),kv_to_pddl(IOUT,I),
      unss_ify(=,[],HintsM,se,SE,OUTG,Hints),mygroup_pairs_by_key(OUTG,GOUT),kv_to_pddl(GOUT,G),
      P = problem(Name, DomainName, [ocl], [], I, G, Hints, /*MS*/ [], /*LS*/ []),
      ignore(loop_check(domain_name(DomainName))))).


name_to_domain_struct(Name,P):-Name==current,!,bb_get(currentDomain,P).
name_to_domain_struct(Name,P):-bb_get(currentDomain,P),prop_get(domain_name,P,NameO),Name=NameO.
name_to_domain_struct(Name,P):-is_saved_type(domain,Name,P).
name_to_domain_struct(Name,P):-loop_check(ocl_domain_struct(Name,P)).

%TODO work on later
% sorts % consts % preds
ocl_domain_struct(Name,D):- use_local_pddl, no_repeats(Name,loop_check(predicates(Name,PredsList))),
   % once((not((bb_get(currentDomain,D),once(Name==current;prop_get(domain_name,D,Name)))))),   
   must_det_l((
      findall(S,is_a_type(Name,S),Types),
      D = domain(Name, [ocl], Types, /*Consts*/ [] , PredsList, /*Fuents*/[]  ,/*Constrs*/ [], /*Dconstraints*/[], Actions),
      Mt=Name,
      findall(A,actn(Mt,A),Actions))).

mygroup_pairs_by_key([], []).
mygroup_pairs_by_key([M-N|T0], [M-ORDERED|T]) :-
 	mysame_key(M, T0, TN, T1),
 	mygroup_pairs_by_key(T1, T),
        mylist_to_set([N|TN],ORDERED).


mysame_key(M0, [M-N|T0], [N|TN], T) :-
 	M0 == M, !,
 	mysame_key(M, T0, TN, T).
mysame_key(_, L, [], L).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handling mutexes

 make_mutex(M):-
		bagof(R1, forbiden_pair(R1), MA),
		bagof(R2, forbiden_pair(MA, R2), MB),
%		writel(MA),nl,
%		writel(MB),nl,
		union(MA, MB, M0),
%		list_to_set(M0_, M0),
%		write('Cistim:'),nl,	
		clear_mutex1(M0, M1),
		clear_mutex2(M1, M2),
		clear_duplicates(M2, M).
		%write('Ocistene:'),nl,writel(M),nl, length(M, L), write('Pocet: '), write(L),nl.

clear_duplicates([], []).
clear_duplicates([H|T], R):-
    member(M, T),
    identical_but_for_variables(H, M),
    !,
    clear_duplicates(T, R).
clear_duplicates([H|T], [H|R]):-
    clear_duplicates(T, R).

forbiden_pair(R):-
		get_action(A),
		get_positiv_effect(A, PE),
		get_negativ_effect(A, NE),
		member(P, PE),
		member(Q, NE),
		copy_term_spec(P-Q, R).
forbiden_pair(MA, NR):-
		member(P-Q, MA),
		get_action(A),
		get_precondition(A, Precond),
		get_positiv_effect(A, PE),
		member(R, Precond),
		member(P, PE),
		copy_term_spec(R-Q, NR).

clear_mutex1([], []):-!.
clear_mutex1([PP-QQ|T], M):-
		(P-Q = PP-QQ ; P-Q = QQ-PP),
		get_init(I),
		select_20_faster(P, I, R),
		member(Q, R),
%		write('Rule1: '), write(PP-QQ),nl,
		clear_mutex1(T, M), !.
clear_mutex1([P-Q|R], [P-Q|M]):-
		clear_mutex1(R, M).

clear_mutex2(M0, M):-
		(select_20_faster(P-Q, M0, R) ; select_20_faster(Q-P, M0, R)),
		get_action(A, _Def), get_precondition(A, Precond), get_positiv_effect(A, PE), get_negativ_effect(A, NE),
		select_20_faster(P, PE, RPE),
		\+ member(Q, NE),
		(
			member(Q, RPE)%, write('prva cast')
			;
			all_not_in(Precond, P, Q, M0)%, write('druha cast')
		),
%		write('Rule2: '), write(P-Q-_Def),nl,

		clear_mutex2(R, M), !.
clear_mutex2(M0, M0).

all_not_in([], _, _, _).
all_not_in([P|T], P, Q, M):-
	all_not_in(T, P, Q, M).
all_not_in([R|T], P, Q, M):-
		\+ (member(R-Q, M) ; member(Q-R, M)),
		%write(precon-R),nl,
		all_not_in(T, P, Q, M).



%check_mutex(+State).
check_mutex(S):-
		bb_get(mutex, M),
		pairfrom(S, P, Q, _),
		(member(P-Q, M) ; member(Q-P, M)),
%		write('Mutex pair.'), write(P-Q), nl,
		!, fail.
check_mutex(_).




identical_but_for_variables(X, Y) :-
		\+ \+ (
			copy_term(X, Z),
			numbervars(Z, 0, N),
			numbervars(Y, 0, N),
			Z = Y
		).% Dostupne veci:


% FILENAME:  test_validate_input_2009.pl 
% :- expects_dialect(sicstus).
% :-[parseProblem, parseDomain].

%nop(_).

%parse_file(+File).
test_parse_file(F,O):- must(read_file(F, L, _Filename)),!,((domainBNF(O, L, _R1); must(((problem(O, L, _R2)))))),!.

test_parse_file(F):- test_parse_file(F,L),arg(2,L,List),!,
 (not(member('(',List))->true;
    ((absolute_file_name(F,A),
	write('Parsing file failed. '), write('('), write(F:A), write(')'), nl))),!.


test_dir_sas(DirIn):-forall(must_filematch(DirIn,DirInM),test_dir_m(DirInM)).
test_dir_m(DIR):-
  working_directory(WAS,WAS),
     call_cleanup(( 
        cd(DIR),
	write('Testing ':DIR), nl,
	test_dir_files_sas(DIR)),
        cd(WAS)).

test_sas:- 
      test_dir_sas('ipc2008-no-cybersec/seq-sat/elevators-strips/'),!, % NO FIRST ANSWER
      !.

test_sas_sanity:- 
      test_dir_sas('ipc2008-no-cybersec/seq-opt/openstacks-strips/'), %PASSES BUT RUNS SLOW
       test_dir_sas('ipc2008-no-cybersec/seq-opt/transport-strips/'), %PASSES BUT RUNS
      test_dir_sas('ipc2008-no-cybersec/netben-opt/elevators-strips/'), % FAIL ALL
      !.

pddl_dir(PDDLDir,Dir):- atom(PDDLDir),absolute_file_name(PDDLDir,Dir0),expand_file_name(Dir0,DirS),DirS\==[],!,member(Dir,DirS).
pddl_dir(PDDLDir,Dir):- atom(PDDLDir),absolute_file_name(pddl(PDDLDir),Dir0),expand_file_name(Dir0,DirS),DirS\==[],!,member(Dir,DirS).
pddl_dir(PDDLDir,Dir):- absolute_file_name(PDDLDir,Dir0),expand_file_name(Dir0,DirS),member(Dir,DirS).

test_rest:-	
	test_dir_sas('ipc2008-no-cybersec/seq-opt/parcprinter-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/pegsol-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/scanalyzer-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/sokoban-strips/'),  % NO FIRST ANSWER
       
	test_dir_sas('ipc2008-no-cybersec/seq-opt/woodworking-strips/'),
	
        
        expand_file_name('../pddl/ipc2008-no-cybersec/?*?/*/',O),
        forall(member(E,O),test_dir_sas(E)).

/*
test_dir_files_sas(PDDLDir,D,P):- pddl_dir(PDDLDir,Dir), directory_file_path(Dir,D,DF), directory_file_path(Dir,P,PF),
        test_parse_file(DF),test_parse_file(PF),
        solve_files(DF,PF),!.

test_dir_files_sas(Dir):-   
	test_dir_files_sas(Dir,'p01-domain.pddl','p01.pddl'),
	test_dir_files_sas(Dir,'p02-domain.pddl','p02.pddl'),
	test_dir_files_sas(Dir,'p03-domain.pddl','p03.pddl'),
	test_dir_files_sas(Dir,'p04-domain.pddl','p04.pddl'),
	test_dir_files_sas(Dir,'p05-domain.pddl','p05.pddl'),
	test_dir_files_sas(Dir,'p06-domain.pddl','p06.pddl'),
	test_dir_files_sas(Dir,'p07-domain.pddl','p07.pddl'),
	test_dir_files_sas(Dir,'p08-domain.pddl','p08.pddl'),
	test_dir_files_sas(Dir,'p09-domain.pddl','p09.pddl'),
	test_dir_files_sas(Dir,'p10-domain.pddl','p10.pddl'),
	test_dir_files_sas(Dir,'p11-domain.pddl','p11.pddl'),
	test_dir_files_sas(Dir,'p12-domain.pddl','p12.pddl'),
	test_dir_files_sas(Dir,'p13-domain.pddl','p13.pddl'),
	test_dir_files_sas(Dir,'p14-domain.pddl','p14.pddl'),
	test_dir_files_sas(Dir,'p15-domain.pddl','p15.pddl'),
	test_dir_files_sas(Dir,'p16-domain.pddl','p16.pddl'),
	test_dir_files_sas(Dir,'p17-domain.pddl','p17.pddl'),
	test_dir_files_sas(Dir,'p18-domain.pddl','p18.pddl'),
	test_dir_files_sas(Dir,'p19-domain.pddl','p19.pddl'),
	test_dir_files_sas(Dir,'p20-domain.pddl','p20.pddl'),
	test_dir_files_sas(Dir,'p21-domain.pddl','p21.pddl'),
	test_dir_files_sas(Dir,'p22-domain.pddl','p22.pddl'),
	test_dir_files_sas(Dir,'p23-domain.pddl','p23.pddl'),
	test_dir_files_sas(Dir,'p24-domain.pddl','p24.pddl'),
	test_dir_files_sas(Dir,'p25-domain.pddl','p25.pddl'),
	test_dir_files_sas(Dir,'p26-domain.pddl','p26.pddl'),
	test_dir_files_sas(Dir,'p27-domain.pddl','p27.pddl'),
	test_dir_files_sas(Dir,'p28-domain.pddl','p28.pddl'),
	test_dir_files_sas(Dir,'p29-domain.pddl','p29.pddl'),
	test_dir_files_sas(Dir,'p30-domain.pddl','p30.pddl').



*/
