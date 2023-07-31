/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

kaggle_arc_pred(M,P):- 
  predicate_property(M:P,file(F)), 
  \+ \+ atom_contains(F,'arc_'),
  \+ predicate_property(M:P,imported_from(_)),
  \+ atom_contains(F,'_pfc'),
  \+ atom_contains(F,'_afc'),
  \+ atom_contains(F,'_ui_'),
  true.

/*  
any
+-- atomic ; constant
¦   +-- number
¦   ¦   +-- integer
¦   ¦   ¦   +-- nonneg            % Integer >= 0 
¦   ¦   ¦   +-- positive_integer  % Integer > 0 
¦   ¦   ¦   +-- negative_integer  % Integer < 0
¦   ¦   ¦   +-- between(U, L)     % Integer >= L, Integer =< U 
¦   ¦   +-- float
¦   ¦       +-- rational          % includes integers
¦   ¦       +-- between(U, L)     % Float >= L, Float =< U 
¦   +-- text
¦   ¦   +-- atom 
¦   ¦   ¦   +-- symbol            % with or without single quotes
¦   ¦   ¦   +-- char              % Atom of length 1
¦   ¦   ¦   +-- code              % Unicode point
¦   ¦   ¦   +-- blob
¦   ¦   ¦       +-- stream 
¦   ¦   +-- string                % double or single quoted
¦   +-- boolean 
+-- compound
    +-- callable                  % Can also be functor/0 symbol
    +-- list ; proper_list
    ¦   +-- list_or_partial_list
    ¦   +-- list(Type)
    ¦   +-- chars
    ¦   +-- codes
    +-- tree ; graph            % Not defined as a formal type
    ¦   +-- cyclic
    ¦   +-- acyclic
    +-- dict
*/

kaggle_arc_pred_types(M,P):-
  findall(M:P,kaggle_arc_pred(M,P),L),
  member(M:P,L),
  copy_term(P,PP),once(ded_argtypes(M,P)),P\=@=PP.


ded_argtypes(M,P):- var(P),!,kaggle_arc_pred(M,P),ded_argtypes(M,P).
ded_argtypes(_,P):- ground(P); \+ compound(P),!.
ded_argtypes(M,P):- term_variables(P,Vars), ded_argtypes_until(10,M,P,Vars), !.


test_learn_decls:- do_learn_decls.

do_learn_decls:- nb_setval(learned_decls,[]), do_learn_decls_one_pass.

do_learn_decls_one_pass:- 
 once(nb_current(learned_decls,Was);Was=[]),
 findall(P,(kaggle_arc_pred_types(_,P), ignore(( \+ (member(E,Was),E==P),pp(:-decl_pt(P))))),List),
 (Was==List-> ! ; (nb_setval(learned_decls,List),nop(do_learn_decls_one_pass))).

learnable_pt(P):- 
 compound(P), functor(P,F,A),current_predicate(F/A), 
  upcase_atom(F,UC), \+ downcase_atom(F,UC).

maybe_learn_decl(P):- ignore(( learnable_pt(P), 
  functor(P,F,A), functor(PP,F,A),P\=@=PP, 
   \+ \+ (clause(is_decl_pt(_,PP),true), P=@=PP ), 
   fail, pp('?-'(decl_pt(P))),decl_pt(P))).

skip_mod(M):- M\==user,!.
is_type_ch(F,1,Rest):- atom(F), atom_concat('is_',Rest,F), \+ atom_contains(Rest,'_').

is_decl_pt(helper,P):- compound(P),\+ \+ clause(is_decl_pt(_,P),true),
  compound_name_arguments(P,F,[Type]),is_type_ch(F,1,T),T=Type.


/*ded_argtypes_until(Depth,M,(A,B),Vars):- !,
  ded_argtypes_until(Depth,M,A,Vars),
  ded_argtypes_until(Depth,M,B,Vars).*/
%ded_argtypes_until(Depth,M,P,Vars):- copy_term(P,PP), is_decl_pt(_,P),PP\=@=P,!,ignore(ded_argtypes_until(Depth,M,P,Vars)).
ded_argtypes_until(Depth,M,P,Vars):- (Depth<0; ground(P) ; ground(Vars); \+ compound(P); skip_mod(M)),!, 
  ignore((Depth<10,maybe_learn_decl(P))).
ded_argtypes_until(Depth,M,forall(A,B),Vars):-!, ded_argtypes_until(Depth,M,A,Vars), ded_argtypes_until(Depth,M,B,Vars). 
ded_argtypes_until(Depth,M,(A,B),Vars):-!, ded_argtypes_until(Depth,M,A,Vars), ded_argtypes_until(Depth,M,B,Vars). 
ded_argtypes_until(Depth,M,(A;B),Vars):-!, ded_argtypes_until(Depth,M,A,Vars), ded_argtypes_until(Depth,M,B,Vars).
ded_argtypes_until(Depth,M,(A->B),Vars):-!, ded_argtypes_until(Depth,M,A,Vars), ded_argtypes_until(Depth,M,B,Vars).
ded_argtypes_until(Depth,M,(A*->B),Vars):-!, ded_argtypes_until(Depth,M,A,Vars), ded_argtypes_until(Depth,M,B,Vars).
ded_argtypes_until(Depth,M,call(A),Vars):-!, ded_argtypes_until(Depth,M,A,Vars).
ded_argtypes_until(Depth,M,\+ (A),Vars):-!, ded_argtypes_until(Depth,M,A,Vars).
ded_argtypes_until(Depth,M,must_det_ll(A),Vars):-!, ded_argtypes_until(Depth,M,A,Vars).

ded_argtypes_until(Depth,M,P,Vars):- copy_term(P,PP), narrow_using(_Whateva,Depth,M,P,Vars),PP\=@=P,!,
  nop(ded_argtypes_until(Depth,M,P,Vars)),maybe_learn_decl(P).
ded_argtypes_until(_Depth,_M,_P,_Vars).


narrow_using(_,_,_M,P,_Vars):- is_decl_pt(_,P).
narrow_using(_SS,Depth,M,(A,B),Vars):-
  narrow_using(_,Depth,M,A,Vars),
  narrow_using(_,Depth,M,B,Vars).

narrow_using(clauses,Depth,M,P,Vars):-  compound(P), \+ ground(P),
 functor(P,F,A), functor(PP,F,A),functor(SPP,F,A),
 predicate_property(M:PP,number_of_rules(N)),N>0,!,
  clause(M:PP,BodyC),
  compound(BodyC),
  \+ \+ ((sub_term(BC,BodyC),compound(BC),\+ \+ is_decl_pt(_,BC))),
  ignore((
    once(( 
      arg_to_argtypes((PP,BodyC),NVArgs),
      include(nonvar,NVArgs,Args),
      length(Args,L),length(Vs,L),
      subst_2L(Args,Vs,PP+BodyC,SPP+SBodyC),
      compound_name_arguments(SPP,_,SPPArgs),  
      nop(pp((clause(M:PP,BodyC), (SPP:-SBodyC)))),
      Depth2 is Depth-1,      
      ignore(ded_argtypes_until(Depth2,M,SBodyC,Vars)),
      maplist(var,SPPArgs),  P = SPP)))).

narrow_using(clauses,Depth,M,P,Vars):- Depth>0,  compound(P), \+ ground(P), P\= run_fti(_,_), 
 functor(P,F,A), functor(PP,F,A), \+ (is_decl_pt(_,PP),ground(PP)), 
 predicate_property(M:PP,number_of_rules(N)),N>0,!,
  clause(M:PP,BodyC),
  compound(BodyC),
  copy_term(Vars,VarsC),
  ((sub_term(BC,BodyC),compound(BC),\+ \+ is_decl_pt(_,BC))),Vars\=@=VarsC,!.

%narrow_using(decls,_,_M,P,_Vars):- ignore((is_decl_pt(_,P))).

narrow_using(metapreds,Depth,M,P,Vars):- Depth<10,
  predicate_property(M:P,meta_predicate(MP)),
 ignore((  
  compound_name_arguments(MP,_,MArgs),
  compound_name_arguments(P,_,PArgs),
  Depth2 is Depth-1,
  maplist(maybe_label_args(Depth2,Vars,M),MArgs,PArgs))).

/*
functor(SPP,F,A),
  ignore((
    once(( 
      arg_to_argtypes((PP,BodyC),NVArgs),
      include(nonvar,NVArgs,Args),
      length(Args,L),length(Vs,L),
      subst_2L(Args,Vs,PP+BodyC,SPP+SBodyC),
      compound_name_arguments(SPP,_,SPPArgs),  
      (pp(((SPP:-SBodyC)))),
      Depth2 is Depth-1,      
      ignore(ded_argtypes_until(Depth2,M,SBodyC,Vars)),
      maplist(var,SPPArgs),  P = SPP)))).
*/
narrow_using(metapreds,Depth,M,P,Vars):- Depth<10,
  predicate_property(M:P,meta_predicate(MP)),
 ignore((  
  compound_name_arguments(MP,_,MArgs),
  compound_name_arguments(P,_,PArgs),
  Depth2 is Depth-1,
  maplist(maybe_label_args(Depth2,Vars,M),MArgs,PArgs))).


maybe_label_args(_Depth,_Vars,_M,(+),_).
maybe_label_args(_Depth,_Vars,_M,(-),_).
maybe_label_args(_Depth,_Vars,_M,(?),_).
maybe_label_args( Depth, Vars, M, _,P):- ignore(ded_argtypes_until(Depth,M,P,Vars)).


arg_to_argtypes((A,B),(AA,BB)):- arg_to_argtypes(A,AA), arg_to_argtypes(B,BB).
arg_to_argtypes(S,DT):- \+ compound(S),!,data_type(S,DT).
arg_to_argtypes(P,Args):- 
  predicate_property(P,meta_predicate(MP)),
  compound_name_arguments(MP,_,MArgs),
  compound_name_arguments(P,_,PArgs),
  maplist(should_replace_arg,MArgs,PArgs,Vars),append(Vars,Args).
arg_to_argtypes(PP,NVArgs):- compound_name_arguments(PP,_,NVArgs).

should_replace_arg((+),A,[A]).
should_replace_arg((-),A,[A]).
should_replace_arg((?),A,[A]).
should_replace_arg(_,A,Args):- arg_to_argtypes(A,Args),!.

%decl_pt(_):- fail.

check_args(P,MC):- functor(P,F,A),functor(T,F,A),functor(C,F,A),is_decl_pt(_,T),check_args(P,1,A,A,T,C,MC),!.
check_args(P,P):- !. 

check_args(P,Arity,Arity,1,T,C,MC):- fail,
 call(C), arg(An,T,ArgType),arg(An,C,Result),
 MC = t,
 arg(An,P,Return),into_type(ArgType,Result,Return).

check_args(P,Arity,An,2,T,C,MC):- fail,
 arg(An,P,ArgIn),arg(An,T,ArgType),arg(An,C,CallArg),
 is_group(ArgIn), ArgType = object,!,
 arg(Arity,P,Result),arg(Arity,C,Return),
 findall(Return,(member(CallArg,ArgIn),check_args(P,Arity,Arity,1,T,C,MC)),Result).

check_args(P,Arity,An,Left,T,C,MC):-  
 arg(An,P,ArgIn),arg(An,T,ArgType),arg(An,C,CallArg),into_type(ArgType,ArgIn,CallArg),
 AnM1 is An+1,LeftP1 is Left-1, check_args(P,Arity,AnM1,LeftP1,T,C,MC),!.
check_args(_P,_Arity,_An,_Left,_T,C,C).

is_type_call(P1,Term):- var(P1),!,throw(is_type_call(P1,Term)).
is_type_call(list,Term):- is_list(Term),!.
is_type_call(P1,Term):- current_predicate(P1/1),!,on_x_log_and_fail(call(P1,Term)),!.
is_type_call( F,Term):- atom(F),atom_concat('is_',F,P1),current_predicate(P1/1),on_x_log_and_fail(call(P1,Term)),!.

into_type(Type,G,O):- is_type_call(Type,G),!,G=O.
into_type(Type,G,O):- nonvar_or_ci(O),!,into_type(Type,G,M),!,M=O.
into_type(_Type,G,O):- plain_var(G),O=G,!.
into_type(Type,G,O):- plain_var(G),throw(var_into_type(Type,G)),O=fake(Type,G).
into_type(+,X,X).
into_type(oid,X,ID):- into_oid(X,ID).
into_type(num,X,X):- assertion(number(X)).
into_type(dir,X,X):- assertion(nav(X,_,_)).
into_type(grid,X,O):- into_grid(X,O).
into_type(object,X,O):- is_object(X)-> X=O ; into_obj(X,O).
into_type(group,X,O):- into_group(X,O).



