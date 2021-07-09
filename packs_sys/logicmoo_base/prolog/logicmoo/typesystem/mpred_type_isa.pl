/* 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == ain(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_type_isa,
          [ 

          was_isa0/3,
          to_isa_out/3,

            assert_isa/2,
            assert_isa_rev/2,
            assert_isa_safe/2,
            assert_compound_isa/3,
            assert_p_safe/3,
            assert_subclass/2,
            assert_subclass_safe/2,
            asserted_subclass/2,
            atom_prefix_other/3,
            atom_type_prefix_other/4,
            is_known_trew/1,
            callOr/3,
            chk_ft/1,
            compound_isa/3,
            decl_type/1,
            decl_type_safe/1,
            disjointWith0/2,
            disjointWithT/2,
            isa_mpred_provide_storage_clauses/3,
            dont_call_type_arity_one/1,
            get_mpred_arg/3,
            shouldnt_be_set/1,
          guess_supertypes/1,
          guess_supertypes_0/1,
          guess_types/1,
          guess_types/2,
          guess_types_0/2,
          guess_typetypes/1,
          guess_typetypes_0/1,
            has_free_args/1,
            into_single_class/2,
            is_Template/1,
            is_known_false/1,
            is_known_false0/1,
            % is_known_true0/1,
            is_known_true/1,
            is_never_type/1,
            is_non_skolem/1,
            is_non_unit/1,
            is_sk_functor/1,
            is_sk_unit/1,
            isa_asserted/2,
            % isa_asserted_0/2,            isa_asserted_compound/2,
            isa_complete/2,
          % cached_isa/2,isa_w_type_atom/2,isa_atom_call/2,isa_atom_call_ilc/2,
            isa_from_morphology/2,
            
            ain_guess/1,
            mpred_types_loaded/0,            
            never_type_f/1,
            never_type_why/2,
            noncol_type/1,
            not_ft/1,
            not_ft_quick/1,
            not_mud_isa/2,
            not_mud_isa/3,
            not_mud_isa0/2,
            %onLoadPfcRule/1,
            pfcNeverTrue/1,
            col_gen/2,
            tSetOrdered/1,
            
            transitive_P/4,
            transitive_P_l_r/4,
            transitive_P_r_l/4,
            transitive_subclass_or_same/2,
            type_deduced/2,
            type_isa/2,
            
         as_reflexive/3 , % (still imported into mpred_type_wff)
         assert_hasInstance/2 , % (still imported into mpred_type_wff)
         call_u_t/2 , % (still imported into mpred_type_wff)
         call_u_t/3 , % (still imported into mpred_type_wff)
         call_u_t/4 , % (still imported into mpred_type_wff)
         call_u_t/5 , % (still imported into mpred_type_wff)
         if_bound/2 , % (still imported into mpred_type_wff)
         is_typef/1 , % (still imported into mpred_type_wff)
         isa_asserted_0/2 , % (still imported into mpred_type_wff)
         isa_asserted_compound/2 , % (still imported into mpred_type_wff)
         isa_backchaing_0/2 , % (still imported into mpred_type_wff)
         isa_backchaing_1/2 , % (still imported into mpred_type_wff)
         isa_w_type_atom/2 , % (still imported into mpred_type_wff)
         map_list_conj/2 , % (still imported into mpred_type_wff)
         subcache/2 , % (still imported into mpred_type_wff)
         transitive_subclass_not_same/2 , % (still imported into mpred_type_wff)
         transitive_superclass_not_same/2 , % (still imported into mpred_type_wff)


         %    decided_not_was_isa/2,
         % did_learn_from_name/1,
         % isa_pred_now_locked/0,
         
         % type_prefix/2,
         % type_suffix/2, 
          mpred_type_isa_file/0
          ]).

:- kb_global(baseKB:type_prefix/2).
:- kb_global(baseKB:type_suffix/2).
 
%:- include('mpred_header.pi').

:- meta_predicate as_reflexive(2,?,?).

:- thread_local(t_l:disable_px/0).

%:- endif.


:-
            op(1150,fx,(was_dynamic)),
            op(1150,fx,(was_multifile)),
            op(1150,fy,(was_module_transparent)),
            op(1150,fx,(was_export)).


:- dynamic(is_never_type/1).
% autoloading user:portray_clause_pi/2 from /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_first
:- meta_predicate callOr(1,?,?),
transitive_P(3,?,?,?),
transitive_P_l_r(3,?,?,?),
transitive_P_r_l(3,?,?,?),
        assert_isa(+, +).



:- module_transparent((
            guess_supertypes/1,
            guess_supertypes_0/1,
            guess_types/1,
            guess_types/2,
            guess_types_0/2,
            guess_typetypes/1,
            guess_typetypes_0/1)).

:- set_prolog_flag(mpred_te,true).
:- set_how_virtualize_file(bodies).

:- multifile(baseKB:use_cyc_database/0).
:- thread_local(baseKB:use_cyc_database/0).

:- multifile baseKB:prologBuiltin/1.
:- discontiguous baseKB:prologBuiltin/1.
:- dynamic baseKB:prologBuiltin/1.

%% assert_hasInstance( ?T, ?I) is nondet.
%
% Assert Has Instance.
%
assert_hasInstance(T,I):-  sanity(ground(T:I)),ain_expanded(isa(I,T)),!,expire_tabled_list(all).

% ========================================
% is_typef(F).
% Checks F for isa(F,tCol).
% ========================================

%= 	 	 

%% is_typef( ?C) is nondet.
%
% If Is A Typef.
%
is_typef(C):-is_ftVar(C),!,fail.
is_typef(prologSingleValued):-!.
is_typef(prologSideEffects):-!.
is_typef(F):- isa_backchaing_0(F,tCol),!.
is_typef(F):- (a(functorDeclares,F);a(tCol,F);clause(isa(F,tCol),true)),!.
is_typef(F):- atom(F),current_predicate(isa_from_morphology/2),isa_from_morphology(F,TT),!,atom_concat(_,'Type',TT).

% ========================================
% is_never_type(F).
% Checks F for not_mudIsa(F,tCol).
% ========================================
:- set_prolog_flag(expect_pfc_file,some_preds).
baseKB:prologBuiltin(is_never_type/1).
:- set_prolog_flag(expect_pfc_file,never).

%= 	 	 

%% is_never_type( ?V) is nondet.
%
% If Is A Never Type.
%
is_never_type(V):-nonvar(V),never_type_why(V,_),!.



%= 	 	 

%% never_type_f( ?Var) is nondet.
%
% Never Type False.
%
never_type_f(Var):-is_ftVar(Var),!,trace_or_throw(var_never_type(Var)).
never_type_f(F):- call_u(tCol(F)),!,fail.
never_type_f(_:W):-!,never_type_f(W).
never_type_f(':-').
never_type_f('include').
never_type_f('tCol'):-!,fail.
never_type_f('clause_asserted').
never_type_f('onSpawn').
never_type_f('ensure_loaded').
never_type_f('declare_load_dbase').
never_type_f('use_module').
never_type_f('meta_predicate').
never_type_f('Area1000').
never_type_f(iPlayer2).
never_type_f(genls).
never_type_f(must).
% never_type_f(mpred_isa).
never_type_f(defnSufficient).
never_type_f(backchainForbidden).


%= 	 	 

%% noncol_type( ?VALUE1) is nondet.
%
% Noncol Type.
%
noncol_type('LogicalConnective').


%= 	 	 

%% never_type_why( ?C, ?VALUE2) is nondet.
%
% Never Type Generation Of Proof.
%

never_type_why(V,ftVar(isThis)):-is_ftVar(V),!.
never_type_why(C,fn):-atom(C),atom_concat(_,'Fn',C),!.
never_type_why(cheaply_u,cheaply_u(isThis)):-!.
never_type_why(C,_):-a(tCol,C),!,fail. % already declared to be a type
never_type_why(_,_):- \+ flag_call(runtime_debug == true) ,!,fail.
never_type_why(C,noncol_type(T)):- noncol_type(T),a(T,C),!.
never_type_why(F,decided_not_was_isa(F,W)):-call_u(decided_not_was_isa(F,W)),!.
%never_type_why(C):- is_ftCompound(C),functor(C,F,1),isa_asserted(F,tCol).
never_type_why(F,Why):-atom(F),functor(G,F,1),real_builtin_predicate(G),!,Why=(whynot( real_builtin_predicate(G) )).
% never_type_why(F):-dmsg(never_type_why(F)),!,asserta_if_new(isa(F,prologDynamic)).
never_type_why(F,Why):-never_type_f(F),Why=is_never_type(F).
never_type_why(F,Why):-a(prologSideEffects,F),Why=prologSideEffects(F).
never_type_why(F,Why):- atom(F), call_u(arity(F,A)),!,F\==isa, call_u(isa(F,_)), A > 1,Why=(whynot( arity(F,A) )).
never_type_why(M:C,Why):-atomic(M),!,never_type_why(C,Why).



% ========================================
% was_isa(Goal,I,C) recognises isa/2 and its many alternative forms
% ========================================
:- kb_global(baseKB:decided_not_was_isa/2).

%= 	 	 

%% was_isa( +G, -INST, -COL) is semidet.
%
% was  (isa/2) syntax.
%
was_isa(_,_,_):-!,fail.
was_isa(G,_,_):- \+ compound(G),!,fail.
was_isa('$VAR'(_),_,_):-!,fail.
was_isa(isa(I,C),I,C):-!.
was_isa(proved_isa(I,C),I,C):-!.
was_isa(ttNotTemporalType(I),I,ttNotTemporalType).
was_isa(tChannel(I),I,tChannel).
was_isa(tAgent(I),I,tAgent).
was_isa(is_typef(_),_,_):-!,fail.
was_isa(quietly(_),_,_):-!,fail.
was_isa(call(_),_,_):-!,fail.
was_isa(dtrace(_),_,_):-!,fail.
was_isa( \+ (_),_,_):-!,fail.
was_isa( ~(_),_,_):-!,fail.
was_isa( not(_),_,_):-!,fail.
was_isa(M:G,I,C):-atom(M),!,was_isa(G,I,C).
was_isa(a(C,I),I,C):-!.
was_isa(t(P,I,C),I,C):-!,P==isa.
was_isa(t(C,_),_,_):- never_type_why(C,_),!,fail.
was_isa(t(C,I),I,C):- nonvar(C),!,call_u(tSet(C)).
was_isa(CI,I,C):- CI=..[C,I],!, \+ atom_concat(_,'Fn',C), call_u(tSet(C)).
was_isa(G,I,C):- \+(current_predicate(_,G)),
  is_ftCompound(G),functor(G,F,_),quietly((( \+ call_u(decided_not_was_isa(F,_)),once(was_isa0(G,I,C)-> true;((functor(G,F,1),
  get_source_ref1(When),asserta_if_new(decided_not_was_isa(F,When)),!,fail)))))).

%% was_isa0( ?G, ?I, ?C) is nondet.
%
% was  (isa/2) Primary Helper.
%
% was_isa0(a(tCol,I),I,tCol).
was_isa0(G,I,C):-G=..[C,I],!,is_typef(C),!,\+ (is_never_type(C)).
% was_isa0(t(C,I),I,C):- new_was_isa, atom(C),!.


:- set_prolog_flag(expect_pfc_file,some_preds).
baseKB:prologBuiltin(to_isa_out/2).

%% to_isa_out( ?I, ?C, ?OUT) is nondet.
%
% Converted To  (isa/2) out.
%
to_isa_out(I,C,isa(I,C)).
to_isa_out(I,C,t(C,I)).
to_isa_out(I,C,OUT):- atom(C)->OUT=..[C,I].



% ========================================
% isa_from_morphology(F).
% Checks F's name for isa(F,*).
% ========================================


%= 	 	 

%% isa_from_morphology( ?Inst, ?Type) is nondet.
%
%  (isa/2) Converted From morphology.
%
isa_from_morphology(Inst,Type):-atom(Inst),type_suffix(Suffix,Type),atom_concat(Base,Suffix,Inst),!,atom_length(Base,BL),BL>2.
isa_from_morphology(Inst,Type):-atom(Inst),type_prefix(Prefix,Type),atom_concat(Prefix,Other,Inst),capitalized(Other),!.

%:- add_import_module(mpred_type_isa,baseKB,start).

:- multifile(baseKB:type_suffix/2).
:- export(baseKB:type_suffix/2).
:- dynamic(baseKB:type_suffix/2).
:- mpred_type_isa:import(baseKB:type_suffix/2).

%% type_suffix( ?VALUE1, ?VALUE2) is nondet.
%
% Type Suffix.
%
baseKB:type_suffix('Fn',ftFunctional).
baseKB:type_suffix('Type',ttTypeType).
baseKB:type_suffix('Able',ttTypeByAction).



:- multifile(baseKB:type_prefix/2).
:- export(baseKB:type_prefix/2).
:- dynamic(baseKB:type_prefix/2).
:- mpred_type_isa:import(baseKB:type_prefix/2).

%% type_prefix( ?VALUE1, ?VALUE2) is nondet.
%
% Type Prefix.
%
baseKB:type_prefix(A,B):-typ_prfx(A,B).

typ_prfx(vt,ttValueType).
typ_prfx(tt,ttTypeType).
typ_prfx(t,tSet).
typ_prfx(pred,tPred).

typ_prfx(format,tPred).
typ_prfx(is,ftSyntaxOperator).
typ_prfx(dcg,ftSyntaxOperator).
typ_prfx(dcg,ftTextType).
typ_prfx(txt,ftTextType).
typ_prfx(sk,ftSkolemFunction).
%typ_prfx(is,tPred).
%typ_prfx(a,tFunction).
%typ_prfx(t,tCol).
typ_prfx(fn,tFunction).
typ_prfx(mud,rtMudPred).
typ_prfx(mud,tPred).
typ_prfx(prop,tPred).
typ_prfx(ft,ttExpressionType).


typ_prfx(prolog,ttRelationType).
typ_prfx(pfc,ttRelationType).
typ_prfx(rt,ttRelationType).
typ_prfx(pt,ttRelationType).

typ_prfx(v,vtValue).
typ_prfx(i,tIndividual).

/*
typ_prfx(i,ftID).
typ_prfx(act,ftAction).
typ_prfx(mob,ttAgentType).
typ_prfx(mt,ttModuleType).
typ_prfx(role,ttKnowledgeType).
typ_prfx(item,ttItemType).
typ_prfx(macro,ttMacroType).
*/

% ========================================
% was_isa(Goal,I,C) recognises isa/2 and its many alternative forms
% ========================================
:- kb_global(baseKB:decided_not_was_isa/2).

%= 	 	 




:- export(shouldnt_be_set/1).
shouldnt_be_set(C):- a(ttExpressionType,C).
shouldnt_be_set(C):- atom(C), atom_concat(ft,_,C).

%= 	 	 

%% not_ft( ?T) is nondet.
%
% Not Format Type.
%
not_ft(T):-nonvar(T),not_ft_quick(T), \+ (a(ttExpressionType,T)).


%= 	 	 

%% not_ft_quick( ?T) is nondet.
%
% Not Format Type Incomplete, But Fast, Version.
%
not_ft_quick(T):-nonvar(T),(T=tItem;T=tRegion;T=tCol;T=completelyAssertedCollection;transitive_subclass_or_same(T,tTemporalThing)).

baseKB:prologBuiltin(asserted_subclass/2).

%= 	 	 

%% asserted_subclass( ?I, ?T) is nondet.
%
% Asserted Subclass.
%

% asserted_subclass(I,T):- ((t_l:useOnlyExternalDBs,!);baseKB:use_cyc_database),(kbp_t([genls,I,T])).
asserted_subclass(T,ST):- call_u(t(genls,T,ST)).


%= 	 	 

%% chk_ft( ?T) is nondet.
%
% Checking Format Type.
%
chk_ft(T):- not_ft_quick(T),!,fail.
%chk_ft(I):- t_l:infForward, a(defnSufficient,I,_),!.
%chk_ft(I):- t_l:infForward, asserted_subclass(I,FT),I\=FT,chk_ft(FT),!.
chk_ft(I):- t_l:infForward, !,a(ttExpressionType,I).



%% into_single_class( ?Var, ?VV) is nondet.
%
% Convert Two Cols into a Single Class.
%
into_single_class(Var,VV):-is_ftVar(Var),!, (nonvar(VV)->into_single_class(VV,Var);Var=VV).
into_single_class(A,B):- is_ftCompound(B),!, (is_ftCompound(A) -> (into_single_class(A,AB),into_single_class(B,AB)) ; into_single_class(B,A) ).
into_single_class('&'(A,Var),VV):-is_ftVar(Var),!,into_single_class(A,VV).
into_single_class('&'(A,B),VV):-!, into_single_class((B),VV);into_single_class((A),VV).
into_single_class(A,A).


% ==========================
% tran_by_trans(isa,genls)
% ==========================

genls_by_trans(C,S):- var(S),!,nonvar(C),genls_upward(C,S),not_already_genls(C,S).
genls_by_trans(C,S):- var(C),!,genls_downward(C,S),not_already_genls(C,S).
genls_by_trans(C,S):- genls_inward(C,S),not_already_genls(C,S).

not_already_genls(C,P):- C\=P, \+ clause_b(genls(C,P)).

genls_upward(C,S):-clause_b(genls(C,C0)),clause_b(genls(C0,SP)),(SP=S;clause_b(genls(SP,S))).
genls_downward(C,S):-clause_b(genls(S0,S)),clause_b(genls(SP,S0)),(SP=C;clause_b(genls(C,SP))).
genls_inward(C,S):- genls_upward(C,M),(M=S ; genls_downward(M,S)).

tran_by_trans(_,C,S):- C==S,!.
tran_by_trans(genls,C,S):- !,genls_by_trans(C,S), \+ call_u(disjointWith(C,S)).
tran_by_trans(R,C,S):- var(S),!,nonvar(C),tran_upward(R,C,S),not_already_tran(R,C,S).
tran_by_trans(R,C,S):- var(C),!,tran_downward(R,C,S),not_already_tran(R,C,S).
tran_by_trans(R,C,S):- tran_inward(R,C,S),not_already_tran(R,C,S).

not_already_tran(R,C,P):- C\=P, \+ clause_r(R,C,P).

tran_upward(R,C,S):-clause_r(R,C,C0),clause_r(R,C0,SP),(SP=S;clause_r(R,SP,S)).
tran_downward(R,C,S):-clause_r(R,S0,S),clause_r(R,SP,S0),(SP=C;clause_r(R,C,SP)).
tran_inward(R,C,S):- tran_upward(R,C,M),(M=S ; tran_downward(R,M,S)).

clause_r(R,C,P):- P=..[R,C,P],clause_b(P).


% ==========================
% taxonomicPair(isa,genls)
% ==========================

baseKB:prologBuiltin((transitive_subclass_or_same/2)).

%= 	 	 

%% transitive_subclass_or_same( ?A, ?B) is nondet.
%
% Transitive Subclass Or Same.
%
transitive_subclass_or_same(A,B):- (is_ftVar(A),is_ftVar(B)),!,A=B.
transitive_subclass_or_same(A,A):-nonvar(A).
transitive_subclass_or_same(A,B):-cheaply_u(genls(A,B)).

transitive_superclass_not_same(A,O):- no_repeats(O,call_u((genls(A,B),(O=B;(genls(B,B2),(O=B2;genls(B2,O))))))).

transitive_subclass_not_same(A,O):- no_repeats(O,call_u((genls(B,A),(O=B;(genls(B2,B),(O=B2;genls(O,B2))))))).


%= 	 	 

%% transitive_P( :PRED3DB, ?P, ?L, ?R) is nondet.
%
% Transitive P.
%
transitive_P(DB,P,L,R):-call_u_t(DB,P,L,R).
transitive_P(DB,P,L,R):-is_ftVar(L),!,transitive_P_r_l(DB,P,L,R).
transitive_P(DB,P,L,R):-transitive_P_l_r(DB,P,L,R).


%= 	 	 

%% transitive_P_l_r( :PRED3DB, ?P, ?L, ?R) is nondet.
%
% Transitive P (list Version) R.
%
transitive_P_l_r(DB,P,L,R):-call_u_t(DB,P,L,A1),(call_u_t(DB,P,A1,R);call_u_t(DB,P,A1,A2),call_u_t(DB,P,A2,R)).
transitive_P_l_r(DB,P,L,R):-nonvar(R),call_u_t(DB,P,L,A1),call_u_t(DB,P,A1,A2),call_u_t(DB,P,A2,A3),call_u_t(DB,P,A3,R).
transitive_P_l_r(DB,P,L,R):-ground(L:R),call_u_t(DB,P,L,A1),call_u_t(DB,P,A1,A2),call_u_t(DB,P,A2,A3),call_u_t(DB,P,A3,A4),call_u_t(DB,P,A4,R).


%= 	 	 

%% transitive_P_r_l( :PRED3DB, ?P, ?L, ?R) is nondet.
%
% Transitive P R (list Version).
%
transitive_P_r_l(DB,P,L,R):-nonvar(R),(call_u_t(DB,P,A1,R),(call_u_t(DB,P,L,A1);call_u_t(DB,P,A2,A1),call_u_t(DB,P,L,A2))).
transitive_P_r_l(DB,P,L,R):-nonvar(R),call_u_t(DB,P,A3,R),call_u_t(DB,P,A2,A3),call_u_t(DB,P,A1,A2),call_u_t(DB,P,L,A1).
transitive_P_r_l(DB,P,L,R):-ground(L:R),call_u_t(DB,P,A3,R),call_u_t(DB,P,A2,A3),call_u_t(DB,P,A1,A2),call_u_t(DB,P,A0,A1),call_u_t(DB,P,L,A0).



%= 	 	 

%% is_known_true( ?C) is nondet.
%
% If Is A Known True.
%
is_known_true(C):-has_free_args(C),!,trace_or_throw(has_free_args(is_known_true,C)).
is_known_true(isa(tPred, ttRelationType)).
is_known_true(F):-is_known_false0(F),!,fail.
is_known_true(F):-is_known_trew(F),!.
%is_known_true(isa(G,tTemporalThing)):- a(_,G),not_mud_isa(G,tCol),not_mud_isa(G,tPred).
is_known_true(genls(G,G)).
is_known_true(isa(apathFn(_,_),tPathway)).

%is_known_true(isa(_,ftTerm)).
%is_known_true(isa(_,ftID)).


:- dynamic(is_known_trew/1).

%= 	 	 

%% is_known_trew( :TermF) is nondet.
%
% If Is A Known Trew.
%
is_known_trew(genls(tRegion,tChannel)).
% is_known_trew(genls('tMaleAnimal',tAgent)).
is_known_trew(genls(prologSingleValued, extentDecidable)).
is_known_trew(genls(tAgent,tChannel)).
is_known_trew(genls(completelyAssertedCollection, extentDecidable)).
is_known_trew(genls(ttExpressionType,tCol)).
is_known_trew(genls(ttExpressionType,ttNotTemporalType)).
is_known_trew(genls(meta_argtypes,tRelation)).
is_known_trew(genls(tFunction,tRelation)).
is_known_trew(genls(F,tPred)):-a(ttRelationType,F).
is_known_trew(isa(_,_)):-fail.
is_known_trew(disjointWith(A,B)):-disjointWithT(A,B).


%= 	 	 

%% pfcNeverTrue( ?P) is nondet.
%
% Prolog Forward Chaining Never True.
%
pfcNeverTrue(P):-compound(P),is_known_false(P).


%= 	 	 

%% is_known_false( ?C) is nondet.
%
% If Is A Known False.
%
is_known_false(C):-has_free_args(C),!,fail.
is_known_false(F):-is_known_trew(F),!,fail.
is_known_false(F):-is_known_false0(F),!.

baseKB:prologBuiltin(is_known_false0/1).


%% is_known_false0( :TermG) is nondet.
%
% If Is A Known False Primary Helper.
%
is_known_false0(isa(I,C)):-catch(not_mud_isa(I,C),_,fail).
is_known_false0(isa(G,Why)):-!,catch(not_mud_isa(G,Why),_,fail).
is_known_false0(genls(A,B)):- as_reflexive(disjointWith,A,B).

% is_known_false0(genls(Type,_)):-arg(_,vv(tCol,tRelation,ttExpressionType),Type).

%% as_reflexive(+P,?A,?B) is nondet.
%
% Call as reflexive Predicate
%
as_reflexive(P,A,B):- t(P,A,B);t(P,B,A).


%% is_non_unit( ?C) is nondet.
%
% If Is A Not Unit.
%
is_non_unit(C):- \+ is_unit(C).

%= 	 	 

%% is_non_skolem( ?C) is nondet.
%
% If Is A Not Skolem.
%
is_non_skolem(C):- \+ is_sk_unit(C).


%= 	 	 

%% is_sk_unit( ?C) is nondet.
%
% If Is A Skolem Unit.
%
is_sk_unit(C):-is_ftCompound(C), C\=(_:-_),get_functor(C,F),is_sk_functor(F),!.

%= 	 	 

%% is_sk_functor( ?F) is nondet.
%
% If Is A Skolem Functor.
%
is_sk_functor(F):- (\+ atom(F)),!,fail.
is_sk_functor(F):-atom_concat('sk',_,F).



baseKB:prologBuiltin(has_free_args/1).

%= 	 	 

%% has_free_args( ?C) is nondet.
%
% Has Free Arguments.
%
has_free_args(C):- sanity(is_ftCompound(C)),arg(_,C,E),is_ftVar(E),!.

% is_known_false(genls(A,B)):-disjointWith(A,B).


%= 	 	 

%% disjointWith0( ?VALUE1, ?VALUE2) is nondet.
%
% Disjoint Using Primary Helper.
%
disjointWith0(tAgent,tItem).
disjointWith0(tRegion,tObj).
disjointWith0(ttExpressionType,tItem).
disjointWith0(ttExpressionType,tObj).
disjointWith0(ttExpressionType,tRegion).
disjointWith0(ttTemporalType,ttNotTemporalType).


%= 	 	 

%% disjointWithT( ?A, ?B) is nondet.
%
% Disjoint Using True Stucture.
%
disjointWithT(A,B):-disjointWith0(A,B).
disjointWithT(B,A):-disjointWith0(A,B).

baseKB:prologBuiltin(not_mud_isa/3).
%mud_isa0(tObj, completelyAssertedCollection).
%mud_isa0(tObj, ttTemporalType).
%mud_isa0(tTemporalThing,ttTemporalType).

%= 	 	 

%% not_mud_isa0( ?I, ?T) is semidet.
%
%  \+  Application  (isa/2) Primary Helper.
%
not_mud_isa0(I,T):-(is_ftVar(I);is_ftVar(T)),trace_or_throw(var_not_mud_isa(I,T)).
not_mud_isa0(I,_):- is_sk_unit(I),!,fail.
not_mud_isa0(ttTypeByAction,ttTypeByAction).
not_mud_isa0(meta_argtypes,ttRelationType).
not_mud_isa0(isa,ttRelationType).
not_mud_isa0(props,ttRelationType).
not_mud_isa0(F, functorDeclares):- \+ (clause_asserted(functorDeclares(F))).
not_mud_isa0(actGossup,tChannel).
not_mud_isa0(_, blah):-!.
not_mud_isa0(I,meta_argtypes):- \+ (is_ftCompound(I)).
%not_mud_isa0(_,prologHybrid):-!,fail.
not_mud_isa0(functorIsMacro, ttExpressionType).
not_mud_isa0(tAgent,ttExpressionType).
not_mud_isa0(tCol,ttExpressionType).
not_mud_isa0(tItem,ttExpressionType).
not_mud_isa0(tObj, ttExpressionType).
not_mud_isa0(tTemporalThing, tTemporalThing).
not_mud_isa0(completelyAssertedCollection,ttTemporalType).
not_mud_isa0(ttExpressionType,ttExpressionType).
not_mud_isa0(ttTemporalType,tTemporalThing).
%not_mud_isa0(I,tCol):- is_ftCompound(I),!, \+ a(tCol,I).
%not_mud_isa0(I,C):- is_ftCompound(I),!, \+ a(C,I).


%= 	 	 

%% not_mud_isa( ?I, ?C) is semidet.
%
%  \+  Application  (isa/2).
%
not_mud_isa(I,C):- nonvar(I),nonvar(C),loop_check(not_mud_isa(I,C,_)).


%= 	 	 

%% not_mud_isa( ?F, ?CAC, ?Why) is nondet.
%
%  \+  Application  (isa/2).
%
not_mud_isa(F, CAC,Why):- fail, baseKB:( cheaply_u(completelyAssertedCollection(CAC)),!,atom(CAC),quietly(current_predicate(_:CAC/1)),
   G=..[CAC,F],\+(call_u(G)),!,Why=completelyAssertedCollection(CAC)).
not_mud_isa(I,C,Why):-not_mud_isa0(I,C),Why=not_mud_isa0(I,C).
not_mud_isa(G,tTemporalThing,Why):- baseKB:call_u((a(tCol,G),Why=a(tCol,G));(tPred(G),Why=tPred(G))).
not_mud_isa(G,tCol,Why):-never_type_why(G,Why).



%= 	 	 

%% tSetOrdered( ?T) is nondet.
%
% True Structure Col Gen.
%
tSetOrdered(T):-nonvar(T),!.
tSetOrdered(T):- no_repeats(T,(clause_b(completelyAssertedCollection(T));clause_b(tSet(T));clause_b(ttExpressionType(T)))).


col_gen(_,T):- nonvar(T),!.
col_gen(S,T):- var(S),!,trace_or_throw(col_gen(S,T)).
col_gen(tCol,T):- !, no_repeats(T,col_type(T)).
col_gen(tRelation,T):- !, no_repeats(T,rel_type(T)).
col_gen(tTemporalThing,T):- !, no_repeats(T,(inst_type(T))).
col_gen(vtValue,T):- !, no_repeats(T,val_type(T)).
col_gen(S,T):- trace_or_throw(col_gen(S,T)).


:- export(col_type/1).
col_type(T):- clause_b(ttTypeType(T)).
:- export(rel_type/1).
rel_type(T):- clause_b(ttRelationType(T)).
:- export(inst_type/1).
inst_type(T):- clause_b(ttTemporalType(T)).
inst_type(T):- clause_b(ttTimeDependentCollection(T)).
:- export(val_type/1).
val_type(T):- clause_b(tCol(T)), \+ col_type(T),\+ inst_type(T),\+ rel_type(T).

/*
  main types

collections (tSet,tCol,collection types)
relations (predciates/function)
spatial/temporals (people,places)
symbols/values (string/numbers/shapes)

*/

:- export(main_type/2).
main_type(C,vtValue):- (string(C);number(C)),!.
main_type(C,_):- \+ callable(C),!,fail.
main_type(C,tCol):- clause_b(tCol(C)),!.
main_type(C,tTemporalThing):- clause_b(tTemporalThing(C)),!.
main_type(C,tRelation):- clause_b(tRelation(C)),!.

% main_type(C,vtValue):- clause_b(vValue(C)),!.

main_type2(C,_):- \+ atom(C),!,fail.
% * GUESSES
main_type2(C,tRelation):- (arity(C,_);downcase_atom(C,C)),!.
main_type2(C,tTemporalThing):- atom_concat(i,_,C),!.
% main_type2(C,vtValue):- atom_concat(v,_,C),!.
main_type2(_,vtValue).


% ==========================
% isa_backchaing_0(i,c)
% ==========================

% isa_module_local_init(+ABox,+TBox) is semidet.
%
% Hook To [isa_module_local_init/2] For Module Mpred_type_isa.
% Module Local Init.
%

:- discontiguous isa_module_local_init/2.
isa_module_local_init(_UserModule,_SystemModule):- ain((isa(I,T):- cwc,isa_backchaing_0(I,T))).
%a(P,F):-loop_check(isa(F,P)).
%a(T,I):- baseKB:pfcManageHybrids,clause_safe(isa(I,T),true).
baseKB:prologBuiltin(isa_asserted/2).


% ============================================
% isa_asserted/1
% ============================================



%% type_isa( ?Type, ?VALUE2) is nondet.
%
% type  (isa/2).
%
type_isa(Type,ttTemporalType):-arg(_,vv(tAgent,tItem,tObj,tRegion),Type),!.
type_isa(ArgIsa,ttRelationType):-a(ttRelationType,ArgIsa),!.
type_isa(ftString,ttExpressionType):-!.
type_isa(Type,ttExpressionType):-chk_ft(Type),!. % text
%  from name


%= 	 	 

%% atom_prefix_other( ?Inst, ?Prefix, ?Other) is nondet.
%
% Atom Prefix Other.
%
atom_prefix_other(Inst,Prefix,Other):-atom_type_prefix_other(Inst,_,Prefix,Other).

%= 	 	 

%% atom_type_prefix_other( ?Inst, ?Type, ?Prefix, ?Other) is nondet.
%
% Atom Type Prefix Other.
%
atom_type_prefix_other(Inst,Type,Prefix,Other):-atom(Inst),type_prefix(Prefix,Type),atom_concat(Prefix,Other,Inst),capitalized(Other).
atom_type_prefix_other(Inst,Type,Suffix,Other):-atom(Inst),type_suffix(Suffix,Type),atom_concat(Other,Suffix,Inst),!.



%= 	 	 




%= 	 	 

%% callOr( :PRED1Pred, ?I, ?T) is nondet.
%
% Call Or.
%
callOr(Pred,I,T):- call_u(call(Pred,I);call(Pred,T)),!.

% type_deduced(I,T):-atom(T),i_name(mud,T,P),!,clause(a(P,_,I),true).

%= 	 	 

%% type_deduced( ?I, ?T) is nondet.
%
% Type Deduced.
%
type_deduced(I,T):-nonvar(I), \+ (number(I)),clause(a(P,_,I),true),(argIsa_known(P,2,AT)->T=AT;i_name(vt,P,T)).

%= 	 	 

%% compound_isa( ?F, ?VALUE2, ?T) is nondet.
%
% compound  (isa/2).
%
compound_isa(F,_,T):- cheaply_u(resultIsa(F,T)).
compound_isa(_,I,T):- cheaply_u(formatted_resultIsa(I,T)).
compound_isa(_,I,T):- cheaply_u(isa_asserted(I,T)).

if_bound(C,G):-var(C)->true;call_u(G).

%:- table(subcache/2).

:-dynamic(subcache/2).

subcache(X,Y):-isa(X,Y).
subcache(X,Z):-nonvar(Z)-> (genls(Y,Z),isa(X,Y)) ; isa(X,Y),genls(Y,Z).

%= 	 	 

%% isa_backchaing_0( ?I, ?T) is nondet.
%
%  (isa/2) backchaing.
%
isa_backchaing(I,C):- C==ftVar,!,is_ftVar(I).
isa_backchaing(I,C):- nonvar(I),is_ftVar(I),!,C=ftVar.
isa_backchaing(_,C):- C==ftProlog,!.
isa_backchaing(I,C):- no_repeats(isa_complete_0(I,C)).
%isa_backchaing_0(I,C):- nonvar(I),var(C),!,tSetOrdered(C),isa_backchaing_0(I,C).

isa_complete_0(I,C):- nonvar(I),var(C),!,tSetOrdered(C),isa_complete_0(I,C).
isa_complete_0(I,C):- nonvar(I),nonvar(C),isa_backchaing_0(I,C).
isa_complete_0(I,C):- callable(C),C=..[P|ARGS],G=..[P,I|ARGS],quietly(current_predicate(P,G)),!,on_x_fail(call_u(G)).
isa_complete_0(I,C):- compound(I),is_non_unit(I),is_non_skolem(I),!,get_functor(I,F),compound_isa(F,I,C).

isa_complete(I,C):- no_repeats(isa_complete_0(I,C)).

isa_backchaing_0(I,C):- no_repeats((isa_asserted(I,C)*->true;(var(C),tSet(C),isa_asserted(I,C)))).

%:- table(isa_backchaing_1/2).
isa_backchaing_1(I,C):- fail,
  nonvar(C)
   -> (transitive_subclass_not_same(T,C),isa_asserted(I,T)) 
   ; (nonvar(I),isa_asserted(I,T),transitive_superclass_not_same(T,C)).

%% isa_asserted( ?I, ?C) is nondet.
%
%  (isa/2) asserted.
%

%isa_asserted(I,C):-  compound(I),!,no_repeats(loop_check(isa_asserted_0(I,C))).
%isa_asserted(I,C):-  ground(I:C),!,no_loop_check(no_repeats(loop_check(isa_asserted_0(I,C)))).
%isa_asserted(I,C):-  no_repeats(loop_check(isa_asserted_0(I,C))).
isa_asserted(I,C):- isa_asserted_0(I,C).
% isa_asserted(I,C):- !, call_u(isa(I,C)).
%isa_asserted(I,C):- ((lc_tcall(isa(I,C),no_repeats(loop_check(isa_asserted_0(I,C)))))).
%isa_asserted(I,CC):-no_repeats((isa_asserted_0(I,C),call_u(genls(C,CC)))).



%% isa_asserted_0( ?I, ?T) is nondet.
%
%  (isa/2) asserted  Primary Helper.
%
%isa_asserted_0(I,C):-is_known_trew(isa(I,C)).
%isa_asserted_0(F,tCol):-isa_from_morphology(F,Col),atom_concat(_,'Type',Col),call_u(arity(F,1)).
%isa_asserted_0([I],C):-nonvar(I),!,isa_asserted_0(I,C).

isa_asserted_0(isInstFn(I),C):-nonvar(I),dtrace,!,C=I.
isa_asserted_0(aRelatedFn(C,_),I):-nonvar(C),!,C=I.
isa_asserted_0(aRelatedFn(C,_,_),I):-nonvar(C),!,C=I.
isa_asserted_0(I,C):-  clause_b(mudIsa(I,C)).
%isa_asserted_0(I,C):- ((t_l:useOnlyExternalDBs,!);baseKB:use_cyc_database),(kbp_t([isa,I,C]);kbp_t([C,I])).

isa_asserted_0(I,C):- quietly((atom(C),G=..[C,I],current_predicate(C,M:G))),!,on_x_fail(M:G).
isa_asserted_0(I,C):- nonvar(I),nonvar(C),not_mud_isa(I,C),!,fail.
isa_asserted_0(_,C):- nonvar(C),sanity(\+ is_ftVar(C)), clause_b(completelyAssertedCollection(C)),!,fail.
% isa_asserted_0(I,_):- sanity(\+ is_ftVar(I)), clause_b(completeIsaAsserted(I)),!,fail.
isa_asserted_0(I,C):- var(I),!,tSetOrdered(C),isa_asserted_0(I,C).

isa_asserted_0(I,C):- isa_asserted_1(I,C)*->true;isa_asserted_2(I,C).

isa_asserted_1(I,C):- var(C),main_type(I,SubType),!,isa_asserted_3(I,SubType,C).
isa_asserted_1(I,C):- var(C),main_type2(I,SubType),!,isa_asserted_3(I,SubType,C),C\==SubType.
isa_asserted_1(I,C):- is_ftCompound(I),is_non_unit(I),is_non_skolem(I),!,get_functor(I,F),compound_isa(F,I,C).
isa_asserted_1(I,C):- isa_asserted_compound(I,C).


isa_asserted_2(I,C):- atom(I),isa_from_morphology(I,C).


% :- kb_local(genls/2).

% isa_asserted_3(I,tCol,C):- (atom(I);atom(C)),type_isa(I,C).
isa_asserted_3(_,C,C).

:- on_f_rtrace((prolog_load_context(file,File),is_file_virtualize_allowed(File))).
%:- rtrace.
isa_asserted_3(I,SType,C):- vwc, var(C),genls(C,SType),nonvar(C),SType\==C,isa_asserted_0(I,C).
/*
:- listing(isa_asserted_3).
:- rtrace(virtualize_source(ge,genls(_A,_B),_O)).
:- b r e a k.
*/

% isa_asserted_3(I,SType,C):- var(C),!,col_gen(SType,C),nonvar(C),SType\==C,isa_asserted_0(I,C).
% isa_asserted_3(_,C,_):- clause_b(ttExpressionType(C)),!,fail.

% isa_asserted_0(I,C):- I == ttTypeByAction, C=ttTypeByAction,!,fail.
% isa_asserted_0(I,C):- HEAD= isa(I, C),ruleBackward(HEAD,BODY),dtrace,call_mpred_body(HEAD,BODY).
% isa_asserted_0(I,C):- ( ((is_ftVar(C);chk_ft(C)),if_defined(term_is_ft(I,C)))*->true;type_deduced(I,C) ).



%% isa_asserted_compound( ?I, :TermT) is nondet.
%
%  (isa/2) asserted  Secondary Helper.
%

%isa_asserted_compound(I,T):- a(ttRelationType,T),!,cheaply_u(isa(I,T)).
isa_asserted_compound(I,T):- \+ compound(T),!, isa_w_type_atom(I,T).
%isa_asserted_compound(_,T):- a(completelyAssertedCollection,T),!,fail.
%isa_asserted_compound(I,T):- append_term(T,I,HEAD),ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).
isa_asserted_compound(I,'&'(T1 , T2)):-!,nonvar(T1),is_ftVar(T2),!,dif:dif(T1,T2),isa_backchaing_0(I,T1),call_u(genls(T1,T2)),isa_backchaing_0(I,T2).
isa_asserted_compound(I,'&'(T1 , T2)):-!,nonvar(T1),!,dif:dif(T1,T2),isa_backchaing_0(I,T1),isa_backchaing_0(I,T2).
isa_asserted_compound(I,(T1 ; T2)):-!,nonvar(T1),!,dif:dif(T1,T2),isa_backchaing_0(I,T1),isa_backchaing_0(I,T2).


%= 	 	 

%% isa_w_type_atom( ?I, ?T) is nondet.
%
%  (isa/2) w type atom.
%
isa_w_type_atom(I,T):- 
  atom(T),
  \+ dont_call_type_arity_one(T),
  G=..[T,I],!,
  locally(set_prolog_flag(retry_undefined, none),
     loop_check(find_and_call(G))).


%% dont_call_type_arity_one( ?VALUE1) is nondet.
%
% Dont Call Type Arity One.
%
% dont_call_type_arity_one(_):-!,fail.
dont_call_type_arity_one(tCol).
dont_call_type_arity_one(ttExpressionType).
dont_call_type_arity_one(ttAgentType).
% dont_call_type_arity_one(F):-a(prologHybrid,F),!.




map_list_conj(Pred,Var):-is_ftVar(Var),!,trace_or_throw(var_with_each(Pred,Var)).
map_list_conj(Pred,All):-is_list(All),!,must_maplist(Pred,All).
map_list_conj(Pred,(A,B)):-!,map_list_conj(Pred,A),map_list_conj(Pred,B).
% allows code to move on





baseKB:prologBuiltin(assert_subclass/2).

%= 	 	 

%% assert_subclass( ?O, ?T) is nondet.
%
% Assert Subclass.
%
assert_subclass(O,T):-assert_subclass_safe(O,T).

baseKB:prologBuiltin(assert_p_safe/3).

%= 	 	 

%% assert_p_safe( ?P, ?O, ?T) is nondet.
%
% Assert Pred Safely Paying Attention To Corner Cases.
%
assert_p_safe(P,O,T):-
  ignore((nonvar(O),nonvar(T),nonvar(P),nop(( \+ (chk_ft(O)), \+ (chk_ft(T)))),ain_guess(t(P,O,T)))).

baseKB:prologBuiltin(assert_subclass_safe/2).

%= 	 	 

%% assert_subclass_safe( ?O, ?T) is nondet.
%
% Assert Subclass Safely Paying Attention To Corner Cases.
%
assert_subclass_safe(O,T):-
  ignore((nonvar(O),decl_type_safe(O),nonvar(T),decl_type_safe(T),nonvar(O),nop(( \+ (chk_ft(O)), \+ (chk_ft(T)))),ain_guess(genls(O,T)))).

%OLD baseKB:decl_database_hook(change(assert,_A_or_Z),genls(S,C)):-decl_type_safe(S),decl_type_safe(C).


%OLD baseKB:decl_database_hook(change(assert,_A_or_Z),isa(W,ttTemporalType)):-decl_type_safe(W). %,call_after_mpred_load(forall(isa(I,W),create_instance(I,W))).
%OLD baseKB:decl_database_hook(change(assert,_A_or_Z),isa(W,tCol)):- (test_tl(infSupertypeName);true),guess_supertypes(W).

%:- was_dynamic(tried_guess_types_from_name/1).
:- was_dynamic(did_learn_from_name/1).


%= 	 	 

%% guess_types( ?W) is nondet.
%
% guess  Types.
%
guess_types(_):- !. % DONT GUESS
guess_types(W):- cheaply_u(tried_guess_types_from_name(W)),!.
guess_types(W):- isa_from_morphology(W,What),ignore(guess_types(W,What)).


%= 	 	 

%% guess_types( ?W, ?What) is nondet.
%
% guess  Types.
%
guess_types(W,tCol):- !, guess_supertypes(W).
guess_types(W,What):- ain(tried_guess_types_from_name(W)),ignore((atom(W),guess_types_0(W,What))).

%= 	 	 

%% guess_types_0( ?VALUE1, ?VALUE2) is nondet.
%
% guess  Types  Primary Helper.
%
guess_types_0(W,ftID):-quietly((atom(W),atom_concat(i,T,W), 
   atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catch(number_codes(_,Digits),_,fail),atom_codes(CC,Type),!,i_name(t,CC,NewType))),
   decl_type_safe(NewType),(a(tCol,NewType)->(assert_isa_safe(W,NewType),ain(did_learn_from_name(W)),guess_supertypes(NewType));true).



%= 	 	 

%% guess_supertypes( ?W) is nondet.
%
% Guess Super Types.
%
guess_supertypes(_):- !. % DONT GUESS
guess_supertypes(W):- cheaply_u(tried_guess_types_from_name(W)),!.
guess_supertypes(W):- ain(tried_guess_types_from_name(W)),ignore((atom(W),guess_supertypes_0(W))).


%= 	 	 

%% guess_supertypes_0( ?W) is nondet.
%
% guess super Types  Primary Helper.
%

guess_supertypes_0(_):- !. % DONT GUESS
guess_supertypes_0(W):-atom(W),atomic_list_concat(List,'_',W),length(List,S),S>2,!, append(FirstPart,[Last],List),atom_length(Last,AL),AL>3, \+ (member(flagged,FirstPart)),
            atomic_list_concat(FirstPart,'_',_NewCol),ain_guess(genls(W,Last)),ain(did_learn_from_name(W)).
guess_supertypes_0(W):-T=t,to_first_break(W,lower,T,All,upper),to_first_break(All,upper,_UnusedSuper,Rest,_),
   atom_length(Rest,L),!,L>2,i_name(tt,Rest,NewSuper),atom_concat(NewSuper,'Type',SuperTT),ain_guess(isa(SuperTT,ttTypeType)),
  ain_guess(isa(W,SuperTT)),ain(did_learn_from_name(W)),!,guess_typetypes(SuperTT).


%= 	 	 

%% ain_guess( ?G) is nondet.
%
% Assert If New Guess.
%
ain_guess(G):-show_call(ain_guess,mpred_ain(G)).


%= 	 	 

%% guess_typetypes( ?W) is nondet.
%
% Guess Type Types.
%
guess_typetypes(_):- !. % DONT GUESS
guess_typetypes(W):- cheaply_u(tried_guess_types_from_name(W)),!.
guess_typetypes(W):- ain(tried_guess_types_from_name(W)),ignore((atom(W),guess_typetypes_0(W))).


%= 	 	 

%% guess_typetypes_0( ?TtTypeType) is nondet.
%
% guess type Types  Primary Helper.
%
guess_typetypes_0(TtTypeType):-
   atom_concat(tt,TypeType,TtTypeType),
   atom_concat(Type,'Type',TypeType),
 atom_concat(t,Type,TType),ain((isa(T,TtTypeType)==>genls(T,TType))).

/*
system:term_expansion(isa(Compound,PredArgTypes),
  (:-dmsg(ain(wizza(Compound,PredArgTypes))))):-
  baseKB:isa_pred_now_locked,
   ground(Compound:PredArgTypes),show_call(why,ain_expanded(isa(Compound,PredArgTypes))),!.
*/


%= 	 	 

%% mpred_provide_storage_op( :TermOp, ?G) is nondet.
%
% Hook To [isa_lmconf:mpred_provide_storage_op/2] For Module Mpred_type_isa.
% Managed Predicate Provide Storage Oper..
%
isa_lmconf:mpred_provide_storage_op(_,_):-!,fail.
% ISA MODIFY
isa_lmconf:mpred_provide_storage_op(change(assert,_),G):- was_isa(G,I,C),!,dmsg(assert_isa_from_op(I,C)),!, assert_isa(I,C).
% ISA MODIFY
isa_lmconf:mpred_provide_storage_op(change(retract,How),G):- was_isa(G,I,C),!,show_call(why,(with_assert_op_override(change(retract,How),((dmsg(retract_isa(G,I,C)),!, assert_isa(I,C)))))).
% ISA CALL
isa_lmconf:mpred_provide_storage_op(call(_),G):- was_isa(G,I,C),!, (isa_backchaing_0(I,C);a(C,I)).
% ISA CLAUSES

%= 	 	 

%% isa_mpred_provide_storage_clauses( ?H, ?B, ?What) is nondet.
%
% Hook To [baseKB:mpred_provide_storage_clauses/3] For Module Mpred_type_isa.
% Managed Predicate Provide Storage Clauses.
%
:- multifile(baseKB:mpred_provide_storage_clauses/3).
baseKB:mpred_provide_storage_clauses(H,B,(What)):-fail,isa_mpred_provide_storage_clauses(H,B,What).

isa_mpred_provide_storage_clauses(isa(I,C),true,hasInstanceIC):-a(C,I).
isa_mpred_provide_storage_clauses(H,true,hasInstanceCI):-
   (is_ftCompound(H)-> 
      ((functor(H,C,1)-> H=..[C,I]; H=isa(I,C)), a(C,I)) ; 
      (a(C,I),(nonvar(C)->append_term(C,I,H);H=isa(I,C)))).

%isa_mpred_provide_storage_clauses(isa(I,C),B,W):-mpred_t_mpred_storage_clauses_rules(isa(I,C),B,W).
%isa_mpred_provide_storage_clauses(isa(I,C),B,W):-nonvar(C),append_term(C,I,H),mpred_t_mpred_storage_clauses_rules(H,B,W).







% ============================================
% decl_type/1
% ============================================
baseKB:prologBuiltin(decl_type_safe/1).

%% decl_type_safe( ?T) is nondet.
%
% Declare Type Safely Paying Attention To Corner Cases.
%
decl_type_safe(All):- map_list_conj(decl_type_safe,All),!.
% decl_type_safe(T):- is_ftCompound(T),!.
decl_type_safe(T):- ignore((atom(T), \+ (never_type_why(T,_)), \+ (number(T)),decl_type(T))).


baseKB:prologBuiltin(decl_type/1).

%% decl_type( :TermVar) is nondet.
%
% Declare Type.
%

decl_type(_):-!.
decl_type(All):- map_list_conj(decl_type,All),!.
decl_type(Spec):- show_call(why,ain(tCol(Spec))),!,guess_supertypes(Spec).
decl_type(Spec):- never_type_why(Spec,Why),!,trace_or_throw(never_type_why(Spec,Why)).



% ============================================
% assert_isa/2
% ============================================

baseKB:prologBuiltin(assert_isa_safe/2).
baseKB:prologBuiltin(assert_isa/2).
  

:- set_prolog_flag(expect_pfc_file,never).

%% assert_isa_safe( ?O, ?T) is nondet.
%
% assert  (isa/2) Safely Paying Attention To Corner Cases.
%
assert_isa_safe(O,T):- ignore((nonvar(O),nonvar(T),decl_type_safe(T),isa_asserted(T,tCol),assert_isa(O,T))).

%% assert_isa( ?I, ?T) is nondet.
%
% assert  (isa/2) Inside Of Loop Checking.
%
assert_isa(I,T):- assert_isa_rev(T,I).

%% assert_isa_rev( ?T, ?I) is nondet.
%
% Assert Has Instance.
% assert (isa/2) reversed.
%
assert_isa_rev(T,I):- sanity(nonvar(I)),sanity(nonvar(T)),fail.
assert_isa_rev(T,I):- sanity( \+ (singletons_throw_else_fail(assert_isa(I,T)))),fail.

% this more about "never"
assert_isa_rev(T,I):- not_mud_isa(I,T,Why)->dmsg(trace_or_throw(not_mud_isa(I,T,Why))),fail.
% skip formatter cols
assert_isa_rev(ftTerm,_):-!.
assert_isa_rev(ftString,_):-!.
assert_isa_rev(_,isKappaFn(_,_)):-!.
% break conjuncts
assert_isa_rev(T,I):- map_list_conj(assert_isa_rev(T),I).
assert_isa_rev(T,I):- map_list_conj(assert_isa(I),T).
assert_isa_rev(T,I):- stack_depth(Level),Level>1650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(isa_asserted(I,T)))),!,fail.
assert_isa_rev(T,I):- isa_asserted(I,TT),T==TT,!.
assert_isa_rev(T,I):- once(decl_type(T)), \+(a(tCol,I)),\+(a(ttTypeType,T)), ain(isa(I,tIndividual)), fail.
assert_isa_rev(T,I):- is_ftCompound(I),is_non_skolem(I),!,must((get_functor(I,F),assert_compound_isa(I,T,F))),!.
assert_isa_rev(T,I):- quietly(chk_ft(T)),(is_ftCompound(I)->dmsg(once(dont_assert_c_is_ft(I,T)));dmsg(once(dont_assert_is_ft(I,T)))),rtrace((chk_ft(T))).

%OLD baseKB:decl_database_hook(change(assert,_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),quietly(doall(isa_asserted_sanely(I,T))).
%OLD baseKB:decl_database_hook(change(assert,_),isa(I,T)):- assert_isa_rev(T,I),fail.
%OLD baseKB:decl_database_hook(change( retract,_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).
% assert_isa_hooked(I,T):- motel:defconcept(I,isAnd([lexicon,T])).
% assert_isa_hooked(I,T):- motel:defprimconcept(I,T).
% assert_isa_hooked(I,T):-dmsg((told(assert_isa(I,T)))).
% TODO assert_isa(F,T):-a(ttRelationType,T),!,decl_mpred(F,T).
% TODO assert_isa_rev(T,I):- ttTemporalType(T),!,assert_isa(I,T).
% TODO assert_isa_rev(T,I):- assert_isa(I,T).
% one of 4 special cols
% assert_isa_rev(T,I):- ttTemporalType(T),!,call_after_mpred_load((create_instance(I,T,[]))).
% sublass of 4 special cols
% assert_isa_rev(T,I):- doall((ttTemporalType(ST),impliedSubClass(T,ST),call_after_mpred_load((create_instance(I,ST,[isa(T)]))))).

assert_isa_rev(T,I):- 
  must_det_l(( 
   %expire_tabled_list(isa(I,T)),
   sanity(ignore(show_failure(ground(T:I)))),
   %nop(expire_tabled_list(all)),
   ain(isa(I,T)),
   sanity(call_u(isa(I,T))),
   sanity(isa_asserted(I,T)),
   % sanity(call_u(installedMotelHook) -> must(call_motel(isa(I,T))) ; true),
   % sanity(call_u(mudIsa(I,T))),
   expire_tabled_list(all))).


%% assert_compound_isa( ?I, ?VALUE2, ?VALUE3) is nondet.
%
% assert compound  (isa/2).
%

%assert_compound_isa(I,_,_):- is_ftCompound(I), I\=resultIsaFn(_),glean_pred_props_maybe(I),fail.
assert_compound_isa(I,T,_):- quietly(chk_ft(T)),dmsg(once(dont_assert_is_ft(I,T))),rtrace((chk_ft(T))).
%assert_compound_isa(I,T,F):- is_Template(I),!,assert_isa_rev(T,I),show_call(why,ain(resultIsa(I,T))),assert_isa_rev(T,resultIsaFn(F)).
assert_compound_isa(I,T,F):- ignore((is_Template(I),locally(infConfidence(vWeak),assert_predArgTypes(I)))),
   assert_isa_rev(T,I),show_call(why,ain_expanded(resultIsa(F,T))),assert_isa_rev(T,resultIsaFn(F)),!.
   




%% get_mpred_arg( ?N, ?C, ?E) is nondet.
%
% Get Managed Predicate Argument.
%
get_mpred_arg(N,MC,E):-strip_module(MC,_,C),is_ftCompound(C),arg(N,C,E).


%% is_Template( ?I) is nondet.
%
% If Is A Template.
%
is_Template(I):- \+ (get_mpred_arg(_,I,Arg1), \+ a(tCol,Arg1)).



%% mpred_types_loaded is nondet.
%
% Managed Predicate  Types loaded.
%
mpred_types_loaded.

% ISA QUERY
isa_module_local_init(_UserModule,_SystemModule):- 
  asserta_if_new((system:goal_expansion(ISA,GO) :- \+ t_l:disable_px, \+current_predicate(_,ISA),
  once((is_ftCompound(ISA),was_isa(ISA,I,C))),t_l:is_calling,show_call(why,GO=no_repeats(isa(I,C))))).
% ISA GOAL
% mpred_system_goal_expansion(G,GO):-G\=isa(_,_),was_isa(G,I,C),GO=isa(I,C).
% ISA EVER
%mpred_term_expansion(G,GO):-  \+ t_l:disable_px,was_isa(G,I,C),GO=isa(I,C).

call_u_t(DB,P,L,A1,A2):-call_u(call(DB,P,L,A1,A2)).
call_u_t(DB,P,L,A1):-call_u(call(DB,P,L,A1)).
call_u_t(DB,P,L):-call_u(call(DB,P,L)).
call_u_t(DB,P):-call_u(call(DB,P)).


mpred_type_isa_file.

:- fixup_exports.


end_of_file.




















end_of_file.




















end_of_file.




















end_of_file.




















end_of_file.




















end_of_file.
















:- module_transparent(all_source_file_predicates_are_shared/0).
all_source_file_predicates_are_shared:-
 source_location(S,_), prolog_load_context(module,LC),
 all_source_file_predicates_are_shared(S,LC).
:- module_transparent(all_source_file_predicates_are_shared/2).
all_source_file_predicates_are_shared(S,LC):-
 forall(source_file(M:H,S),
 ignore((functor(H,F,A), \+ atom_concat('$',_,F), kb_shared(M:F/A)))).

% :- all_source_file_predicates_are_shared.



% ================================================
% assert_isa HOOKS
% ================================================
%OLD baseKB:decl_database_hook(_,genls(_,_)):-retractall(a(_,isa,_,_)),retractall(a(_,genls,_,_)).
%OLD baseKB:decl_database_hook(change(assert,_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),quietly(doall(isa_asserted_sanely(I,T))).
%OLD baseKB:decl_database_hook(change(assert,_),isa(I,T)):- assert_hasInstance(T,I),fail.

%OLD baseKB:decl_database_hook(change( retract,_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).


%= 	 	 

%% assert_isa_hooked( ?A, ?VALUE2) is nondet.
%
% assert  (isa/2) hooked.
%
:- meta_predicate assert_isa_hooked(?,1).

assert_isa_hooked(A,_):-retractall(a(cache_I_L,isa,A,_)),fail.
assert_isa_hooked(F,T):- a(ttRelationType,T),decl_mpred(F,T),fail.
assert_isa_hooked(I,T):- assert_isa(I,T).
assert_isa_hooked(I,T):-  \+ (ground(assert_isa(T))),!, trace_or_throw( \+ (ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- assert_hasInstance(T,I),fail.

assert_isa_hooked(T,tCol):-!,decl_type(T),!.
assert_isa_hooked(T,ttExpressionType):-!,define_ft(T),!.
assert_isa_hooked(Term,tPred):-!,decl_mpred(Term).
assert_isa_hooked(Term,prologHybrid):-!,kb_shared(Term).
% assert_isa_hooked(Term,prologDynamic):-!,export(Term).
assert_isa_hooked(Term,prologPTTP):-!,kb_shared(Term).
assert_isa_hooked(Term,prologKIF):-!,kb_shared(Term).
assert_isa_hooked(I,_):- I\=prologHybrid(_),glean_pred_props_maybe(I),fail.
assert_isa_hooked(food5,tWeapon):-trace_or_throw(assert_isa(food5,tWeapon)).

% assert_isa_hooked(I,T):- motel:defconcept(I,isAnd([lexicon,T])).
% assert_isa_hooked(I,T):- motel:defprimconcept(I,T).
% assert_isa_hooked(I,T):-dmsg((told(assert_isa(I,T)))).


%OLD baseKB:decl_database_hook(change(assert,_),isa(I,T)):- doall(assert_isa_hooked_after(I,T)).


%= 	 	 

%% assert_isa_hooked_after( ?F, ?T) is nondet.
%
% assert  (isa/2) hooked after.
%
assert_isa_hooked_after(F,T):-a(ttRelationType,T),!,decl_mpred(F,T).
assert_isa_hooked_after(_,tCol):-!.
assert_isa_hooked_after(_,ttExpressionType):-!.
%assert_isa_hooked_after(I,T):- ttTemporalType(T),!,assert_isa_hooked_creation(I,T).

%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

/*
assert_isa_hooked_after(I,T):-  \+ (completelyAssertedCollection(T)),impliedSubClass(T,ST),completelyAssertedCollection(ST),assert_isa(I,ST).
baseKB:prologBuiltin(impliedSubClass/2).
impliedSubClass(T,ST):-ground(T:ST),is_known_false(genls(T,ST)),!,fail.
impliedSubClass(T,ST):-predicate_property(transitive_subclass(T,ST),_),!,lc_tcall((isa(T,ST),transitive_subclass(T,ST))).
*/

% one of 4 special cols
% assert_isa_hooked_creation(I,T):- ttTemporalType(T),!,call_after_mpred_load((create_instance(I,T,[]))).
% sublass of 4 special cols
% assert_isa_hooked_creation(I,T):- doall((ttTemporalType(ST),impliedSubClass(T,ST),call_after_mpred_load((create_instance(I,ST,[isa(T)]))))).


% :- ain((baseKB:isa(I,C):-loop_check(isa_backchaing_0(I,C)))).
% isa_module_local_init:- ain(('$toplevel':isa(I,C):-baseKB:isa(I,C))).


%= 	 	 

%% mpred_types_loaded is nondet.
%
% Managed Predicate  Types loaded.
%
mpred_types_loaded.

% ISA QUERY
isa_module_local_init(_UserModule,_SystemModule):- 
  asserta_if_new((system:goal_expansion(ISA,GO) :- \+ (t_l:disable_px, false ), \+current_predicate(_,ISA),
  once((is_ftCompound(ISA),was_isa(ISA,I,C))),t_l:is_calling,show_call(why,GO=no_repeats(isa(I,C))))).
% ISA GOAL
% mpred_system_goal_expansion(G,GO):-G\=isa(_,_),was_isa(G,I,C),GO=isa(I,C).
% ISA EVER
%mpred_term_expansion(G,GO):-  \+ t_l:disable_px,was_isa(G,I,C),GO=isa(I,C).

isa_module_local_init(_UserModule,SystemModule):-ain(SystemModule:tCol(tCol)).
isa_module_local_init(_UserModule,SystemModule):-ain(SystemModule:tCol(ttRelationType)).

call_u_t(DB,P,L,A1,A2):-call_u(call(DB,P,L,A1,A2)).
call_u_t(DB,P,L,A1):-call_u(call(DB,P,L,A1)).
call_u_t(DB,P,L):-call_u(call(DB,P,L)).
call_u_t(DB,P):-call_u(call(DB,P)).

%% mpred_univ( ?C, ?I, ?Head) is det.
%
% Managed Predicate Univ.
%
% TODO decide if still needed 
mpred_univ(C,I,Head):- atom(C),!,Head=..[C,I],predicate_property(Head,number_of_clauses(_)).

onLoadPfcRule('==>'(a(tCol,Inst), {isa_from_morphology(Inst,Type)} , isa(Inst,Type))).
:-ain(baseKB:prologBuiltin(was_isa/3)).
:-ain(baseKB:prologBuiltin(is_typef/1)).
:- fixup_exports.

/*                       
%% onLoadPfcRule( :TermInst) is nondet.
%
% Whenever Load Prolog Forward Chaining Rule.
%

*/
