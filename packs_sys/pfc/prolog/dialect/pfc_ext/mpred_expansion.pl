/*  
% ===================================================================
% File 'dbase_c_term_expansion'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == holds(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
% when deciding the setting for a pred in file foof.pl
%
%  foom:foo(1):-bar(2).
%
%      we search in this order:  SOURCE:LOADTYPE:PRED
%
% SOURCETYPE
%                  source_file('/dir/foof.pl')
%                  source_module(foom)
%                  source_user(ax)
%                  source_filetype(pl)
%                  source_caller(user)   % module it's being loaded for
%                  (missing)*
% LOADTYPE
%                  consult
%                  assert
%                  (missing)*
%                  
% CLAUSETYPE
%                  rule
%                  fact
%                  directive
%                  (missing)*
%                  
% PRED INDICATOR
%                  
%                  foo(int),
%                  foo/1
%                  foo,
%                  (:-)/2  % neck used
%                  (missing)*
%
%
%
% clause types: (:-)/1, (:-)/2, (=>)/1,  (=>)/2,  (==>)/1,  (==>)/2, (<-)/1,  (<-)/2, (<==>)/2, fact/1
%
*/
:- if(current_prolog_flag(xref,true)).
:- module(mpred_expansion,
          [ a/2,
            acceptable_xform/2,
            additiveOp/1,
            alt_calls/1,
            any_op_to_call_op/2,
            as_is_term/1,as_is_term/1,
            as_list/2,
            cheaply_u/1,
            cheaply_u/1,
            maybe_prepend_mt/3,
            compare_op/4,
            comparitiveOp/1,
            compound_all_open/1,
            conjoin_l/3,
            try_expand_head/3,
            db_expand_0/3,
            db_expand_chain/3,
            db_expand_final/3,
            db_expand_maplist/5,
            db_op_sentence/4,
            db_op_simpler/3,
            db_quf/4,
            db_quf_l/5,
            db_quf_l_0/5,
            default_te/3,
            demodulize/3,
            remodulize/3,
            replaced_module/3,
            fully_expand_into_cache/3,
            do_expand_args/3,
            do_expand_args_l/3,
            do_expand_args_pa/4,
            ex_argIsa/3,
            exact_args/1,
            exact_args0/1,
            expand_isEach_or_fail/2,
            expand_goal_correct_argIsa/2,
            expand_props/3,
            expand_props/4,
            expanded_different/2,
            expanded_different_1/2,
            expanded_different_ic/2,
            %expands_on/2,
            fix_negations/2,
            fixed_negations/2,
            fixed_syntax/2,
            foreach_arg/7,
            from_univ/4,
            fully_expand/2,            
            fully_expand/3,
            fully_expand_clause/3,
            fully_expand_goal/3,
            fully_expand_head/3,
            fully_expand_into_cache/3,
            fully_expand_real/3,
            fully_expand_into_cache/3,
            %full_transform_warn_if_changed_UNUSED/3,
            functor_declares_collectiontype/2,
            functor_declares_instance/2,
            functor_declares_instance_0/2,
            holds_args/2,
            %if_expands_on/3,
            infix_op/2,
            instTypePropsToType/2,
            into_functor_form/3,
            into_functor_form/5,
            into_mpred_form/2,
            into_mpred_form6/6,
            into_mpred_form_ilc/2,
            is_arity_pred/1,
            is_meta_functor/3,
            is_pred_declarer/1,
            is_relation_type/1,
            is_stripped_module/1,
            is_unit/1,
			is_unit_like/1,
                        post_expansion/3,
            is_unit_functor/1,
            listToE/2,
            map_f/2,
            mpred_expand_rule/2,
            should_expand/1,
            must_remodulize/3,
            recommify/2,
            recommify/3,
            reduce_clause/3,
            reduce_clause_from_fwd/3,
            show_doall/1,
            string_to_mws/2,
            simply_functors/3,
            to_reduced_hb/4,
            transform_functor_holds/5,
            transform_holds/3,
            transform_holds_3/3,
            transitive_lc_nr/3,
            translate_args/9,
            translateListOps/8,
            translateOneArg/8,
            was_isa_ex/3,

          mpred_expansion_file/0,
          expand_kif_string/2,
         temp_comp/4,
         get_ruleRewrite/2,
         expand_kif_string_or_fail/3,
         to_predicate_isas/2,
         append_as_first_arg/3,
         try_expand_head/3,
         expand_isEach_or_fail/2,
         % expand_kif_string/3,
         is_elist_functor/1
          
          ]).
:- endif.

:- include('mpred_header.pi').



:- meta_predicate 
   % mpred_expansion
   cheaply_u(+),
   cheaply_u(+),
   db_expand_maplist(2,*,*,*,*),
   % mpred_expansion
   transitive_lc_nr(2,*,*),
   simply_functors(2,*,*).
          

:- thread_local(t_l:disable_px/0).


:- use_module(library(apply)).
:- use_module(library(logicmoo/attvar_serializer)).

%= :- kb_shared(was_chain_rule/1).
%= :- kb_shared(baseKB:rtReformulatorDirectivePredicate/1).
%= :- kb_shared(props/2).

:- dynamic(baseKB:col_as_isa/1).
:- dynamic(baseKB:col_as_unary/1).

%:- rtrace.
%:- kb_shared(baseKB:wid/3).

:- style_check(+singleton).

%% default_te( ?IF, ?VAR, ?VAL) is semidet.
%
% Default Te.
%
default_te(IF,VAR,VAL):-assertz(te_setting(IF,VAR,VAL)).

:- default_te([source_filetype(pl) ],use_te,file_prolog).
:- default_te([source_filetype(pfc) ],use_te,file_pfc).
:- default_te([source_filetype(console) ],use_te,file_prolog).

:- default_te(file_prolog,proccess_directive, proccess_directive).
:- default_te(file_prolog,compile_clause, compile_clause).
:- default_te(file_prolog,rule_neck, (head :- body)).
:- default_te(file_prolog,fact_neck, (head :- true)).

:- default_te(file_pfc, compile_clause, ain).
:- default_te(file_pfc, expand_clause, fully_expand_clause).
:- default_te(file_pfc, proccess_directive, proccess_directive).
:- default_te(file_pfc, fact_neck, (clause <- true)).
:- default_te(file_pfc, rule_neck, (head :- body)).

:- default_te(file_syspreds,isa_detector, always_fail(i,c)).
:- default_te(file_syspreds,isa_holder, c(i)).
:- default_te(file_syspreds,isa_varholder, (t(c,i))).  % was isa(i,c).
:- default_te(file_syspreds,pred_holder, head).  % was isa(i,c).
:- default_te(file_syspreds,pred_varholder,  univ_safe(newhead , [t,pred|args])).
:- default_te(file_syspreds,proccess_directive, proccess_directive).
:- default_te(file_syspreds,compile_clause, compile_clause).
:- default_te(file_syspreds,rule_neck, (head :- body)).
:- default_te(file_syspreds,fact_neck, (clause :- true)).
:- default_te(file_syspreds, expand_clause, (=)).

:- default_te(file_syspreds:pred(*), neck_override, (cwc)).
:- default_te(file_pfc:pred(*), neck_override, (hybrid)).
:- default_te(file_prolog:pred(*), neck_override, (hybrid)).

:- default_te((:-)/1, compile_clause, proccess_directive).
:- default_te((:-)/2, rule_neck, clause).
:- default_te((=>),use_te, file_pfc).
:- default_te((<==>),use_te, file_pfc).
:- default_te((<-),use_te, file_pfc).

/*
% :- directive:  process_directive, call
% fact:  fwc(pfc), bwc(pfc), *cwc(prolog), bwc(pttp), implies(kif), other
% :- rule:  fwc(pfc), bwc(pfc), *cwc(prolog), bwc(pttp), implies(kif), other
% <- rule:   fwc(pfc), *bwc(pfc), cwc(prolog), bwc(pttp), implies(kif), other
% <= rule:   *fwc(pfc), bwc(pfc), cwc(prolog), bwc(pttp), implies(kif), other
% <- fact:   fwc(pfc), *bwc(pfc), cwc(prolog), bwc(pttp), implies(kif), other
% => fact:   *fwc(pfc), bwc(pfc), cwc(prolog), bwc(pttp), implies(kif), other
% loading:  compile_clause, process_directive, assertz, 
% head types: code, *hybrid, safe_functor(outer), holds(outer)
% body types: code, *hybrid, safe_functor(outer), holds(outer)
% isa holder:   isa(i,c), t(c,i),  *c(i).
% isa holder is_ftVar c:   isa(i,c), *t(c,i).
% varpred_head:  *t(P,A,B).
% varpred_body:  *t(P,A,B).
% body types: code, *hybrid, safe_functor(outer), holds(outer)

Interestingly there are three main components I have finally admit to needing despite the fact that using Prolog was to provide these exact components.  
First of all a defaulting system using to shadow (hidden) behind assertions
Axiomatic semantics define the meaning of a command in a program by describing its effect on assertions about the program state.
The assertions are logical statements - predicates with variables, where the variables define the state of the program.
Predicate transformer semantics to combine programming concepts in a compact way, before logic is realized.   
This simplicity makes proving the correctness of programs easier, using Hoare logic.

Axiomatic semantics
Writing in Prolog is actually really easy for a MUD is when X is chosen

%
% Dec 13, 2035
% Douglas Miles
*/

%:-use_module(pfc_lib).
%:-use_module(mpred_type_wff).


% ============================================
% inital a/2 database
% ============================================

% baseKB:hasInstance_dyn(W,SS):-nonvar(W),nonvar(SS),SS=isKappaFn(_,S),nonvar(S),!.


/*
disabled a(T,I):- not(current_predicate(deduce_M/1)),!,baseKB:hasInstance_dyn(T,I).
disabled a(T,I):- !, (mudIsa_motel(I,T) *-> true ; (((atom(I),must(not(baseKB:hasInstance_dyn(T,I)))),fail))).
disabled a(T,I):- rdf_x(I,rdf:type,T).
*/
:- system:op(700,xfx,('univ_safe')).
%% a( ?C, ?I) is nondet.
%
% A.
%

:- meta_predicate a(+,?).
% WANT (but will loop) a(C,I):- !, quietly((atom(C),G  univ_safe  [C,I], no_repeats(call_u(G)))).
a(C,I):- quietly((atom(C),current_predicate(C/1), G  univ_safe  [C,I], no_repeats(lookup_u(G)))).


%=  :- was_export(alt_calls/1).

%= 	 	 

%% alt_calls( +Op) is semidet.
%
% Alt Calls.
%
alt_calls(call).
alt_calls(call_u).
alt_calls(clause_u).
alt_calls(lookup_u).
alt_calls(clause_asserted_i).
alt_calls(t).
alt_calls(is_entailed_u).
alt_calls(call_u).
alt_calls(ireq).



:- meta_predicate compare_op(*,2,?,?).


:- meta_predicate show_doall(0).

%= 	 	 

%% show_doall( :Goal) is semidet.
%
% Show Doall.
%
show_doall(Call):- doall(show_call(why,Call)).


/*
Name               Meaning                            

speed              speed of the runtime code
safety             run-time error checking            

correct         run-time error correction            

compilation-speed  speed of the compilation process   
debug              ease of debugging     


cheaply_u(G):- quickly(quietly(Goal)).

*/

% lookup_u/cheaply_u/call_u/clause_b

cheaply_u(rtArgsVerbatum(G)):- !, clause_b(rtArgsVerbatum(G)).
cheaply_u(functorDeclares(F)):-!, clause_b(functorDeclares(F)).
cheaply_u(prologBuiltin(G)):- !,clause_b(prologBuiltin(G)).
cheaply_u(call(ereq,G)):- !,sanity(callable(G)),cheaply_u(G).
% cheaply_u(G):-!,call(G).
cheaply_u(G):- quietly(lookup_u(G)).

%cheaply_u(G):- need_speed,!, (ground(G)->(quietly(baseKB:G),!);quietly(lookup_u(G))).
%cheaply_u(G):- loop_check(cheaply_u(G),loop_check_term(cheaply_u(G),ilc2(G),fail)).
%cheaply_u(G):- predicate_property(G,number_of_rules(N)),N=0,!,lookup_u(G).
%cheaply_u(G):- strip_module(G,_,C),G\==C,!,cheaply_u(C).


was_isa_ex(ISA,I,C):- if_defined(was_isa(ISA,I,C),fail).
%= 	 	 

%% is_pred_declarer( ?P) is semidet.
%
% If Is A Predicate Declarer.
%
is_pred_declarer(P):-functor_declares_instance(P,tPred).

%= 	 	 

%% is_relation_type( ?P) is semidet.
%
% If Is A Relation Type.
%
is_relation_type(tRelation).
is_relation_type(tFunction).
is_relation_type(tPred).
is_relation_type(P):-is_pred_declarer(P).


%= 	 	 

%% functor_declares_instance( ?F, ?C) is semidet.
%
% Functor Declares Instance.
%
functor_declares_instance(F,C):- fail, functor_declares_instance_0(F,C0),!,C=C0. % , nop(sanity(F\=C0)).

%= 	 	 

%% functor_declares_instance_0( ?P, ?P) is semidet.
%
% safe_functor declares instance  Primary Helper.
%


functor_declares_instance_0(isa,_):-!,fail.
functor_declares_instance_0(props,_):-!,fail.
functor_declares_instance_0(F,F):- cheaply_u(functorDeclares(F)).
functor_declares_instance_0(F,F):- a(ttRelationType,F).

:- if(false).
functor_declares_instance_0(P,P):- arg(_,s(ttExpressionType,ttModuleType,tSet,ttTypeType,tFunction),P).

functor_declares_instance_0(P,P):- arg(_,s(tCol,ftSpec),P).
functor_declares_instance_0(P,P):- 
  arg(_,s(tPred,prologMultiValued, prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,prologSideEffects,
       predIsFlag,prologBuiltin,prologKIF,prologDynamic,prologListValued,prologSingleValued),P).
functor_declares_instance_0(P,P):- arg(_,s(predCanHaveSingletons,functorIsMacro),P).
% functor_declares_instance_0(P,P):- arg(_,s(mpred_isa),P),!,fail.
functor_declares_instance_0(col_as_isa,col_as_isa).

functor_declares_instance_0(F,F):- between(2,5,A),arity_no_bc(F,A),!,fail.
functor_declares_instance_0(F,F):- arity_no_bc(F,A),A>5,!,fail.

% functor_declares_instance_0(F,F):- arity_no_bc(F,1).

functor_declares_instance_0(_,_):- !, fail.  % @TODO COMMENT THIS OUT

% functor_declares_instance_0(COL,COL):- call_u(isa(COL,ttTypeType)).
functor_declares_instance_0(COL,COL):- call_u(isa(COL,tSet)).
functor_declares_instance_0(P,P):- isa_asserted(P,ttRelationType),!.

functor_declares_instance_0(_,_):- !, fail.  % @TODO COMMENT THIS OUT

% @TODO CONFIRM THIS IS WRONG functor_declares_instance_0(functorIsMacro,tRelation).
functor_declares_instance_0(P,ttRelationType):-isa_from_morphology(P,ttRelationType).
functor_declares_instance_0(P,tFunction):-isa_from_morphology(P,ftFunctional).
functor_declares_instance_0(P,tFunction):-isa_from_morphology(P,O)->O=tFunction.
%functor_declares_instance_0(COL,COL):- call_u(isa(COL,tCol)).
%functor_declares_instance_0(P,tCol):-isa_asserted(P,functorDeclares),\+functor_declares_instance_0(P,tPred).



functor_adds_instance_0(decl_mpred,tPred).
functor_adds_instance_0(kb_shared,prologHybrid).
functor_adds_instance_0(kb_shared,prologHybrid).
functor_adds_instance_0(decl_mpred_prolog,prologBuiltin).
functor_adds_instance_0(decl_mpred_prolog,prologDynamic).

% functor_adds_instance_0(meta_argtypes,tRelation).

:- endif.


% ========================================
% Logic Preds Shared
% ========================================

%= %= :- was_export(is_svo_functor/1).



%% is_svo_functor( ?Prop) is semidet.
%
% If Is A Svo Functor.
%
is_svo_functor(Prop):- quietly((atom(Prop),arg(_,svo(svo,prop,valueOf,rdf),Prop))).

%= %= :- was_export(hilog_functor/1).



%% hilog_functor( ?VALUE1) is semidet.
%
% Hilog Functor.
%
hilog_functor(true_t).

%= %= :- was_export(is_holds_true_not_hilog/1).



%% is_holds_true_not_hilog( ?HOFDS) is semidet.
%
% If Is A Holds True Not Hilog.
%
is_holds_true_not_hilog(HOFDS):-is_holds_true(HOFDS),\+ hilog_functor(HOFDS).

%= %= :- was_export(is_holds_true/1).



%% is_holds_true( ?Prop) is semidet.
%
% If Is A Holds True.
%
is_holds_true(Prop):- quietly((atom(Prop),is_holds_true0(Prop))),!.

% k,p,..


is_holds_functor(F):- atom(F),is_holds_functor0(F),!, \+ isBodyConnective(F).
is_holds_functor0(F):- atom_concat('proven_',_,F).
is_holds_functor0(F):- atom_concat('ex_',_,F).
is_holds_functor0(F):- atom_concat(_,'_t',F).
is_holds_functor0(F):- is_2nd_order_holds(F).

must_be_unqualified(_):-!.
must_be_unqualified(Var):- \+ compound(Var),!.
must_be_unqualified(Var):-strip_module(Var,_,O),Var\==O,!,break_ex.
must_be_unqualified(Var):-forall(arg(_,Var,E),must_be_unqualified(E)).


:- dynamic(isBodyConnective/1).


%% isBodyConnective( ?Funct) is semidet.
%
% If Is A Body Connective.
%
isBodyConnective(Funct):-atom_concat(_,'_',Funct),!.
isBodyConnective(Funct):-atom_concat('t~',_,Funct),!.
isBodyConnective(Funct):-atom_concat('f~',_,Funct),!.
isBodyConnective(Funct):-member(Funct,[and,or,until,',',';',':-',unless,xor,holdsDuring]). % Other Propositional Wrhtml_appers




%% is_holds_true0( ?Prop) is semidet.
%
% If Is A Holds True Primary Helper.
%
is_holds_true0(Prop):-arg(_,vvv(holds,holds_t,t,t,asserted_mpred_t,assertion_t,true_t,assertion,secondOrder,firstOrder),Prop).


% is_holds_true0(Prop):-atom_concat(_,'_t',Prop).

%= %= :- was_export(is_2nd_order_holds/1).



%% is_2nd_order_holds( ?Prop) is semidet.
%
% If Is A 2nd Order Holds.
%
is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

%= %= :- was_export(is_holds_false/1).



%% is_holds_false( ?Prop) is semidet.
%
% If Is A Holds False.
%
is_holds_false(Prop):-quietly((atom(Prop),is_holds_false0(Prop))).




%% is_holds_false0( ?Prop) is semidet.
%
% If Is A Holds False Primary Helper.
%
is_holds_false0(Prop):-member(Prop,[not,nholds,holds_f,mpred_f,aint,assertion_f,not_true_t,asserted_mpred_f,retraction,not_secondOrder,not_firstOrder]).
%is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
%is_holds_false0(Prop,Stem):-atom_concat('int_not_',Stem,Prop).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).
%is_holds_false0(Prop):-is_holds_false0(Prop,Stem),is_holds_true0(Stem).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).


%= 	 	 

%% with_assert_op_override( ?Op, ?Call) is semidet.
%
% Using Assert Oper. Override.
%
with_assert_op_override(Op,Call):-locally_tl(assert_op_override(Op),Call).



%= 	 	 

%% functor_declares_collectiontype( +Op, ?VALUE2) is semidet.
%
% Functor Declares Collectiontype.
%
functor_declares_collectiontype(typeProps,ttTemporalType).


%= 	 	 

%% instTypePropsToType( +Op, ?VALUE2) is semidet.
%
% Inst Type Props Converted To Type.
%
instTypePropsToType(instTypeProps,ttSpatialType222).


:- thread_local(t_l:into_goal_code/0).


%= 	 	 

%% reduce_clause( ?Op, ?C, ?HB) is semidet.
%
% Reduce Clause.
%
%reduce_clause(Op,C,HB):-must(nonvar(C)),quietly_must(demodulize(Op,C,CB)),CB\=@=C,!,reduce_clause(Op,CB,HB).
reduce_clause(_,C,C):- t_l:into_goal_code,!.
% reduce_clause(Op,clause(C, B),HB):-!,reduce_clause(Op,(C :- B),HB).
%reduce_clause(Op,(C:- B),HB):- is_src_true(B),!,reduce_clause(Op,C,HB).
reduce_clause(_,C,C).


%% demodulize( ?Op, ?H, ?HH) is semidet.
%
% Demodulize.
%

demodulize(_Op,H,H):-!.
demodulize(_Op,H,HH):- not_ftCompound(H),!,HH=H.
demodulize(Op,H,HHH):- strip_module(H,M,HH),H\==HH,old_is_stripped_module(M),!,demodulize(Op,HH,HHH).
demodulize(Op,[I|HL],[O|HHL]):- \+ is_list(HL), !, demodulize(Op,I,O),demodulize(Op,HL,HHL).
demodulize(Op,H,HH):- is_list(H),must_maplist(demodulize(Op),H,HH),!.
demodulize(Op,H,HH):- H  univ_safe  [F|HL],must_maplist(demodulize(Op),HL,HHL),HH  univ_safe  [F|HHL],!.
% lmcache:completely_expanded


old_is_stripped_module(_):-!,fail.
old_is_stripped_module(user).
old_is_stripped_module(baseKB).
%= 	 	 

%% to_reduced_hb( ?Op, ?HB, ?HH, ?BB) is semidet.
%
% Converted To Reduced Head+body.
%
to_reduced_hb(Op,HB,HH,BB):-reduce_clause(Op,HB,HHBB),expand_to_hb(HHBB,HH,BB).


/*
dbase_head_expansion(_,V,V ):-is_ftVar(V),!.
dbase_head_expansion(Op,H,GG):-correct_negations(Op,H,GG),!.
dbase_head_expansion(_,V,V).
*/

% ================================================
% db_expand_maplist/3
% ================================================


%= 	 	 

%% any_op_to_call_op( +Op, ?VALUE2) is semidet.
%
% Any Oper. Converted To Call Oper..
%
any_op_to_call_op(_,call(conjecture)).


%= 	 	 

%% db_expand_maplist( :PRED2FE, ?List, ?T, ?G, ?O) is semidet.
%
% Database Expand Maplist.
%
db_expand_maplist(FE,[E],E,G,O):- !,call(FE,G,O).
db_expand_maplist(FE,[E|List],T,G,O):- copy_term(T+G,CT+CG),E=CT,!,call(FE,CG,O1),db_expand_maplist(FE,List,T,G,O2),conjoin_l(O1,O2,O).
db_expand_maplist(FE,List,T,G,O):-bagof(M, (member(T,List),call(FE,G,M)), ML),list_to_conjuncts(ML,O).


% ================================================
% fully_expand/3
%   SIMPLISTIC REWRITE (this is not the PRECANONICALIZER)
% ================================================


%= 	 	 

%% should_expand( :TermG) is semidet.
%
% Must Be Successfull Expand.
%
%TODO Maybe later? should_expand(G):- \+ skip_expand(G), arg(_,G,E),compound(E).
should_expand(G):- \+ compound(G),!,fail.
should_expand(_:G):- !,should_expand(G).
should_expand((G:-_)):- !,should_expand(G).
should_expand(G):- safe_functor(G,F,_),should_expand_f(F),!.
should_expand(G):- safe_functor(G,F,_),exact_args_f(F),!,fail.  % Will expand these only after evaluation
should_expand(G):- arg(A,G,C),(string(C);(compound(C),A==2)),!.


should_expand_f(kif).
should_expand_f(pkif).
should_expand_f('==>').
should_expand_f('~').

should_expand_f(props).
should_expand_f(iprops).
should_expand_f(upprop).
should_expand_f(typeProps).
should_expand_f(mudLabelTypeProps).
should_expand_f(iprops).
should_expand_f(isa).
should_expand_f(t).
should_expand_f(isEach).

% Collecton Hooks
should_expand_f(tPred).
should_expand_f(tFunction).
should_expand_f(tRelation).
should_expand_f(tCol).
should_expand_f(tSet).
should_expand_f(F):-atom_concat('tt',_,F).
should_expand_f(F):-atom_concat('rt',_,F).

% Pred Impl Hooks
should_expand_f(singleValuedHybrid).
%should_expand_f(prologHybrid).
%should_expand_f(prologBuiltin).
%should_expand_f(prologDynamic).
should_expand_f(F):-atom_concat('prolog',_,F).
should_expand_f(F):-atom_concat('pddl',_,F).
should_expand_f(F):-atom_concat('pfc',_,F).
should_expand_f(F):-atom_concat('mpred_',_,F).


%= 	 	 

%% full_transform_warn_if_changed_UNUSED( ?A, ?B, ?O) is semidet.
%
% Fully Expand Warn.
%
full_transform_warn_if_changed_UNUSED(A,B,O):-
  must(fully_expand_real(A,B,C)),!,
  sanity(ignore(show_failure(why,same_terms(B,C)))),(O=C;must(sanity(ignore(show_failure(why,same_terms(O,C)))))),!.




:- export(fully_expand/3).


:- export(fully_expand/2).

%= 	 	 

%% fully_expand( ^X, --Y) is det.
%
% Fully Expand.
%
fully_expand(X,Y):- must((fully_expand(clause(unknown,cuz),X,Y))).

%:- mpred_trace_nochilds(fully_expand/3).



%% fully_expand( ++Op, ^Sent, --SentO) is det.
%
% Fully Expand.
%
%  Op = the type of operation we are expanding for.. currently there are
%  change(_,_) - for inclusion and testing of present in plain prolog
%  query(_,_) - for call/ask that is dirrectly runnable
%  pfc(_,_) - for salient language based analysis at a human level
%


%fully_expand(_,Var,Var):- \+ compound(Var),!.
%fully_expand(Op,Sent,SentO):- safe_functor(Sent,F,A),should_fully_expand(F,A),!,must(fully_expand_real(Op,Sent,SentO)),!.
fully_expand(Op,Sent,SentO):- fully_expand_real(Op,Sent,SentO),!.
% fully_expand(Op,Sent,Sent):- sanity((ignore((fully_expand_real(Op,Sent,SentO)->sanity((Sent=@=SentO)))))).

/*
fully_expand(Op,Sent,SentO):- must(fully_expand_real(Op,Sent,SentO)),!,
   fully_expand_check(Op,Sent,SentO).

fully_expand_check(_Op,Sent,SentO):- Sent=@=SentO.
fully_expand_check(Op,Sent,SentO):- break,throw(fully_expand_real(Op,Sent,SentO)).

*/
/*
should_fully_expand(~,1).
should_fully_expand(==>,_).
should_fully_expand(props,2).
should_fully_expand(t,_).
should_fully_expand(ereq,_).
should_fully_expand(arity,2).
should_fully_expand(F,_):-clause_b(functorIsMacro(F)).
should_fully_expand(F,_):-clause_b(functorDeclares(F)).
*/

:- meta_predicate memoize_on_local(*,*,0).
memoize_on_local(_Why,_,Goal):- call(Goal),!.
% memoize_on_local(_Why,Sent->SentO,Goal):- memoize_on(fully_expand_real,Sent->SentO,Goal).

has_skolem_attrvars(Sent):- quietly((term_attvars(Sent,Attvars),member(Var,Attvars),get_attr(Var,skk,_))),!.


% for trace testing
fully_expand_real(X,Y):- must((fully_expand_real(clause(unknown,cuz),X,Y))).



fully_expand_real(_Op,Sent,SentO):- \+ compound(Sent),!,Sent=SentO.
fully_expand_real(Op,isa(I,C),SentO):- !,fully_expand_real_2(Op,isa(I,C),SentO).
fully_expand_real(Op,==>(Sent),SentO):- !,fully_expand_real_2(Op,Sent,SentO).
fully_expand_real(Op,==>(SentA,SentB),SentOO):- !,fully_expand_real_2(Op,==>(SentA,SentB),SentOO).
fully_expand_real(Op,mudKeyword(SentA,SentB),SentOO):- !,fully_expand_real_2(Op,mudKeyword(SentA,SentB),SentOO).
fully_expand_real(Op,<==>(SentA,SentB),SentOO):- !,fully_expand_real_2(Op,<==>(SentA,SentB),SentOO).
fully_expand_real(Op,(Sent/I),(SentO/O)):- !,fully_expand_real_2(Op,Sent,SentO),fully_expand_goal(Op,I,O).
fully_expand_real(_Op,{}(IC),{}(IC)):- !.
fully_expand_real(Op,Sent,SentO):- safe_functor(Sent,F,A),always_quite_expand_fa(F,A),!,fully_expand_real_2(Op,Sent,SentO).
fully_expand_real(Op,(SentA,SentB),(SentAA,SentBB)):- !,
  fully_expand_real(Op,SentA,SentAA),fully_expand_real(Op,SentB,SentBB).
fully_expand_real(Op,SentI,SentO):- maybe_expand_reduce(Op,SentI,Sent),!,
   fully_expand_real_2(Op,Sent,SentO),!,
   (Sent=@=SentO-> true ;            
     (SentI \=@= Sent -> true ; 
       show_expansion("~N-----++",Op,Sent,SentO))).
fully_expand_real(_Op,Sent,Sent):- current_prolog_flag(runtime_speed,3),!.
fully_expand_real(_Op,Sent,Sent):- current_prolog_flag(runtime_safety,0),!.
fully_expand_real(_Op,(H:-B),(H:-B)):-!.

fully_expand_real(Op,Sent,SentO):- !, fully_expand_real_2(Op,Sent,SentO),!.
fully_expand_real(Op,Sent,SentO):-
  fully_expand_real_2(Op,Sent,SentO),!,
  (Sent=@=SentO-> true ; 
    (dumpST,show_expansion("~N<!--BREAK-ERROR--->",Op,Sent,SentO),nop(break))).

show_expansion(Prefix,Op,Sent,SentO):-dmsg_pretty(Prefix),dmsg_pretty(-->(Op)),dmsg_pretty(Sent),dmsg_pretty(-->),dmsg_pretty(SentO),!.

fully_expand_real_2(Op,Sent,SentO):- has_skolem_attrvars(Sent),!,
   gripe_time(0.2,
    (must_det(quietly(serialize_attvars(Sent,SentI))),
      sanity(\+ has_skolem_attrvars(SentI)),
     must_det(fully_expand_real_3(Op,SentI,SentO)),!)),!.
fully_expand_real_2(Op,Sent,SentO):- fully_expand_real_3(Op,Sent,SentO).

fully_expand_real_3(Op,Sent,SentO):-
   gripe_time(0.2,
    must_det(locally_tl(disable_px,
       (locally(local_override(no_kif_var_coroutines,true),
       (must_det(fully_expand_into_cache(Op,Sent,SentIO)),
                   must_det(quietly(maybe_deserialize_attvars(SentIO,SentO))))))))).


maybe_expand(_Op,C,_):- \+ compound(C),!,fail.
maybe_expand(Op,M:P,M:PP):-!,maybe_expand(Op,P,PP).
maybe_expand(_Op,C,_):- compound_name_arity(C,_,0),!,fail.
maybe_expand(_Op,P,_):- var(P),!,fail.
maybe_expand(Op,P,P):-maybe_expand_p(Op,P),!.


maybe_expand_reduce(_Op,==>(P),P).
maybe_expand_reduce(_Op,expand(P),P).
maybe_expand_reduce(_Op,Sent,Sent):- safe_functor(Sent,F,_), clause_b(rtSymmetricBinaryPredicate(F)).


maybe_expand_p( Op, H:-_ ):- !, maybe_expand_p(Op,H).
maybe_expand_p(_Op,==>(_,_)).
maybe_expand_p(_Op,mudKeyword(_,_)).
maybe_expand_p(_Op,isa(_,_)).
maybe_expand_p(_Op,(_/_)).
maybe_expand_p(_Op,(_,_)).
maybe_expand_p(_Op,P):- safe_functor(P,F,A),!,always_quite_expand_fa(F,A).

always_quite_expand_fa(F,1):- maybe_expand_f(F).
always_quite_expand_fa(F,2):- clause_b(rtSymmetricBinaryPredicate(F)).
always_quite_expand_fa(t,_).
%always_quite_expand_fa(F,2):- should_expand_f(F).

maybe_expand_f(meta_argtypes).
maybe_expand_f(functorIsMacro).
maybe_expand_f(tPred).
maybe_expand_f(t).
maybe_expand_f(ttExpressionType).
maybe_expand_f(prologHybrid).
maybe_expand_f(prologBuiltin).
maybe_expand_f(prologSingleValued).
maybe_expand_f(prologHybrid).
maybe_expand_f(singleValuedHybrid).
maybe_expand_f(prologSideEffects).
maybe_expand_f(prologMultiValued).
%maybe_expand_f(F):- should_expand_f(F).


maybe_deserialize_attvars(X,Y):- current_prolog_flag(expand_attvars,true) 
  -> deserialize_attvars(X,Y) ; X=Y.
%maybe_deserialize_attvars(X,X):-!.


/*
fully_expand_real(Op,Sent,SentO):-
   gripe_time(0.2,
    (quietly(maybe_deserialize_attvars(Sent,SentI)),
     locally_tl(disable_px,
       locally(local_override(no_kif_var_coroutines,true),
       fully_expand_into_cache(Op,SentI,SentO))))),!.
*/    

%% is_stripped_module( +Op) is semidet.
%
%  Is a stripped Module (Meaning it will be found via inheritance)
%
is_stripped_module(_):-!,fail.
is_stripped_module(A):-var(A),!,fail.
is_stripped_module(Mt):- call_u(mtExact(Mt)),!,fail.
%is_stripped_module(Inherited):-'$current_source_module'(E), default_module(E,Inherited).
%is_stripped_module(Inherited):-'$current_typein_module'(E), default_module(E,Inherited).
is_stripped_module(abox).
% is_stripped_module(_):-!,fail.
% is_stripped_module(baseKB).
% is_stripped_module(A):- defaultAssertMt(AB),!,AB=A.



%% expand_isEach_or_fail(^Sent, --SentO) is semidet.
%
% Expand isEach/Ns.

expand_isEach_or_fail(==>(Sent),SentO):- expand_isEach_or_fail_real(Sent,SentO),!.
expand_isEach_or_fail(Sent,SentO):- expand_isEach_or_fail_real(Sent,SentO),!,throw(expand_isEach_or_fail(Sent)).

expand_isEach_or_fail_real(Sent,SentO):- compound(Sent),
    \+ (Sent  univ_safe  [_,I],atomic(I)), bagof(O,do_expand_args(isEach,Sent,O),L),!,L\=@=[Sent],SentO=L,!.
expand_isEach_or_fail_conj(Sent,SentO):- expand_isEach_or_fail_real(Sent,SentM),pfc_list_to_conj(SentM,SentO).

%% expand_kif_string_or_fail( ++Op, ++Sent, --SentO) is semidet.
%
% Expand if String of KIF.
expand_kif_string_or_fail(_Why,I,O):- string(I), 
   locally(t_l:sreader_options(logicmoo_read_kif,true),
     ((input_to_forms(string(I),Wff,Vs)->
   put_variable_names(Vs) ->
   if_defined(sexpr_sterm_to_pterm(Wff,PTerm),Wff=PTerm)->
   PTerm\=@=I -> 
   O=PTerm))).


expand_kif_string(I,O):- any_to_string(I,S), string(S),
  locally(t_l:sreader_options(logicmoo_read_kif,true),input_to_forms(string(S),O,Vs))->
  put_variable_names(Vs).
  

%% fully_expand_clause( ++Op, :TermSent, -- SentO) is det.
%
% Fully Expand Clause.
%

:- dynamic(lmcache:completely_expanded/2).

%% fully_expand_into_cache( ++Op, ^Sent, --SentO) is det.
%
% Fully Expand Now.
fully_expand_into_cache(Op,Sent,SentO):- \+ ground(Sent),!,fully_expand_clause_catch_each(Op,Sent,SentO),!.
fully_expand_into_cache(_,Sent,SentO):- lmcache:completely_expanded(_,Sent),!,SentO=Sent.
fully_expand_into_cache(_,Sent,SentO):- lmcache:completely_expanded(Sent,SentO),!.
fully_expand_into_cache(Op,Sent,SentO):- 
 fully_expand_clause_catch_each(Op,Sent,SentO),!,
         asserta(lmcache:completely_expanded(Sent,SentO)),!.
fully_expand_into_cache(Op,Sent,SentO):- 
 trace,break,
  (fully_expand_clause_catch_each(Op,Sent,SentO)),
         asserta(lmcache:completely_expanded(Sent,SentO)),!.
% fully_expand_clause_catch_each(change(assert, ain), arity(functorDeclares, 1), _32410)


fully_expand_clause_catch_each(Op,Sent,SentO):-
  catch(fully_expand_clause(Op,Sent,SentO),
       hasEach,
      (must(expand_isEach_or_fail_conj(Sent,SentM)),
       must(fully_expand_real(Op,SentM,SentO)))),!.
/*

fully_expand_into_cache(Op,Sent,SentO):- term_variables(Sent,SentV),copy_term(Sent-SentV,SentI-SentIV),
                             numbervars(SentI,311,_),fully_expand_clause_now1a(Op,SentI,SentV-SentIV,Sent,SentO),!.

:- dynamic(completely_expanded_v/3).
subst_All(B,[],_R,B):-!.
subst_All(B,[F|L],R,A):-subst(B,F,R,M),subst_All(M,L,R,A).

fully_expand_clause_now1a(_Op,SentI,_,Sent,SentO):- completely_expanded_v(_,SentI),!,SentO=Sent.

%  p(A,B).  p(1,2).   ==>  q(2,1).   q(B,A)      SentV-SentIV,   [1,2],[A,B]  % substAll(p(1,2),[1,2],[A,B],O).
fully_expand_clause_now1a(Op,SentI,SentV-SentIV,_Sent,SentO):- lmcache:completely_expanded(SentI,SentOM),!,subst_All(SentOM,SentIV,SentV,SentO).
fully_expand_clause_now1a(Op,SentI,_,_Sent,SentO):- fully_expand_into_cache(Op,SentI,SentO),
         asserta(lmcache:completely_expanded(SentI,SentO)).

% fully_expand_into_cache(Op,Sent,SentO):- expand_isEach_or_fail(Sent,SentM)->SentM\=@=Sent,!,must(fully_expand_clause(Op,SentM,SentO)).
% fully_expand_into_cache(Op,Sent,SentO):- fully_expand_clause(Op,Sent,SentO),!.
fully_expand_into_cache(Op,Sent,SentO):- memoize_on_local(fully_expand_clause,Sent->SentO,(fully_expand_clause(Op,Sent,SentM),
  % post_expansion(Op,SentM,SentO)
  SentM=SentO
  )),!.
*/


post_expansion(Op,Sent,SentO):- 
   do_renames_expansion(Sent,SentM),!,
   maybe_correctArgsIsa(Op,SentM,SentO),!.

% 
do_renames_expansion(Sent,Sent):- \+ current_prolog_flag(do_renames,mpred_expansion),!.
do_renames_expansion(Sent,SentM):- if_defined(do_renames(Sent,SentM),=(Sent,SentM)).

maybe_correctArgsIsa(_ ,SentO,SentO):-!.
maybe_correctArgsIsa(Op,SentM,SentO):- locally_tl(infMustArgIsa,correctArgsIsa(Op,SentM,SentO)),!.

fully_expand_clause(Op,':-'(Sent),Out):-!,fully_expand_goal(Op,Sent,SentO),!,must(Out=':-'(SentO)).
fully_expand_clause(Op,Sent,SentO):- sanity(is_ftNonvar(Op)),sanity(var(SentO)),var(Sent),!,Sent=SentO.
fully_expand_clause(Op,'==>'(Sent),(SentO)):-!,fully_expand_clause(Op,Sent,SentO),!.
fully_expand_clause(Op,'=>'(Sent),(SentO)):-!,fully_expand_clause(Op,Sent,SentO),!.
fully_expand_clause(Op,(B,H),Out):- !,must((fully_expand_clause(Op,H,HH),fully_expand_clause(Op,B,BB))),!,must(Out=(BB,HH)).
fully_expand_clause(Op,Sent,SentO):- is_list(Sent),!,must_maplist(fully_expand_clause(Op),Sent,SentO).
% fully_expand_clause(_,(:-(Sent)),(:-(Sent))):-!.

fully_expand_clause(_,Sent,SentO):- t_l:infSkipFullExpand,!,must(Sent=SentO).

% fully_expand_clause(Op,Sent,SentO):- \+ compound(Sent),!,must(fully_expand_head(Op,Sent,SentO)).
fully_expand_clause(_,aNoExpansionFn(Sent),Sent):- !.
fully_expand_clause(Op,aExpansionFn(Sent),SentO):- fully_expand_clause(Op,Sent,SentO).
fully_expand_clause(Op,M:Sent,SentO):- is_stripped_module(M),!,fully_expand_clause(Op,Sent,SentO).

fully_expand_clause(Op,(B/H),Out):- !,fully_expand_head(Op,H,HH),fully_expand_goal(Op,B,BB),!,must(Out=(BB/HH)).

% prolog_clause fully_expand_clause
fully_expand_clause(Op, H :- B, HH :- B):- is_ftVar(B),!,fully_expand_head(Op,H,HH).

fully_expand_clause(Op,Sent,SentO):- string(Sent),expand_kif_string_or_fail(Op,Sent,SentO),!.
%covered fully_expand_clause(Op ,NC,NCO):- db_expand_final(Op,NC,NCO),!.
fully_expand_clause(Op, HB, OUT):- 
  to_reduced_hb(Op,HB,H,B) ->
  (fully_expand_head(Op,H,HH) ->
  (is_src_true(B) -> HH = OUT ;
    ( must(fully_expand_goal(Op,B,BB)),
      ((HH \= (_,_)) -> reduce_clause(Op,(HH:-BB),OUT) ;
         reduce_clause(Op,(H:-BB),OUT))))).


:- thread_local(t_l:into_form_code/0).

%% fully_expand_goal( ?Op, ^ Sent, -- SentO) is det.
%
% Fully Expand Goal.
%
fully_expand_goal(change(assert,_),Sent,SentO):- var(Sent),!,SentO=call_u(Sent).
fully_expand_goal(Op,Sent,SentO):- 
 must((
  locally_tl(into_goal_code,locally(t_l:into_form_code,fully_expand_head(Op,Sent,SentM))),
    recommify(SentM,SentO))).

/*

?- recommify((a,{((b,c),d)},e),O).
O =  (a, {b, c, d}, e).

?- recommify((a,{((b,c),d)},e),O).
O =  (a, {b, c, d}, e).

?- recommify((a,(b,c,d),e),O).
O =  (a, b, c, d, e).

?- recommify((a,(b,c),(d,e)),O).
O =  (a, b, c, d, e).

?- recommify((a,(b,c),(true,e)),O).
O =  (a, b, c, e).

?- recommify((((a0,a),(b,c)),(true,d,e)),O),portray_clause((h:-O)).
O =  (a0, a, b, c, d, e).

?- recommify((a,(b,c),call((true,e)),true),O).
O =  (a, b, c, call(e)).

*/

recommify(A,AA):- \+ compound(A),!,AA=A.
% recommify(A,A):-!.
recommify(A,B):- recommify(true,A,B),!.

recommify(A,B,C):- \+ compound(B),!,conjoin(A,B,C).
recommify(A,(B,C),D):- \+ compound(B),!, conjoin(A,B,AB), recommify(AB,C,D).
recommify(A,((X,B),C),D):- !, recommify(A,X,AX),recommify(AX,(B,C),D).
recommify(A,(B,C),D):- !, conjoin(A,B,AB), recommify(AB,C,D).
recommify(A,PredArgs,C):- PredArgs  univ_safe  [P|Args],maplist(recommify,Args,AArgs),B  univ_safe  [P|AArgs],conjoin(A,B,C),!.

pfc_list_to_conj([], true).
pfc_list_to_conj([C], C) :- !.
pfc_list_to_conj([H|T], (H,C)) :-
    pfc_list_to_conj(T, C).

const_or_var(I):- \+ atom(I), (var(I);I='$VAR'(_);(atomic(I),\+ string(I))),!.

:- export(as_is_term/1).

%% as_is_term( :TermNC) is semidet.
%
% Converted To If Is A Term Primary Helper.
%
as_is_term(NC):- cyclic_break(NC), const_or_var(NC),!.
as_is_term(PARSE):-is_parse_type(PARSE),!,fail.
as_is_term(NC):- compound(NC),!,NC  univ_safe  [F,A|R],as_is_term(F,A,R),!.

as_is_term(F,_,_):- exact_args_f(F).
as_is_term(F,I,[]):- !, (F==I;const_or_var(I)).
% as_is_term(_,I,[C]):- C==I. % const_or_var(I),const_or_var(C),!.
% covered  above as_is_term(CI):- CI  univ_safe  [C,I],C==I,!.

% covered  as_is_term(P):-safe_functor(P,F,A),safe_functor(C,F,A),C=@=P,!. % all vars
% covered  as_is_term(meta_argtypes(_)):-!.
% covered  as_is_term(meta_argtypes_guessed(_)):-!.
% covered  as_is_term(rtArgsVerbatum(Atom)):- !, \+ compound(Atom).
as_is_term(ftListFn,_,[]):-!.
as_is_term(arity,F,_):- atom(F).
% covered  as_is_term(functorIsMacro(Atom)):- !, \+ compound(Atom).
% covered  as_is_term(functorDeclares(Atom)):- !, \+ compound(Atom).
% covered  above as_is_term('$VAR'(_)).
% covered  above as_is_term(NC):-exact_args(NC),!.
% covered  above as_is_term(NC):-loop_check(is_unit(NC)),!.
% as_is_term(isa(I,C)):- \+ compound(I),atom(C), clause_asserted(baseKB:col_as_isa(C)),!.

/*
as_is_term((:),_,[NC]):-!,as_is_term(NC). 
as_is_term(Op,_,[_]):- infix_op(Op,_).
*/

:- mpred_trace_none(as_is_term(_)).
:- '$set_predicate_attribute'(as_is_term(_), hide_childs, 1).
%:- lock_predicate(as_is_term(_)).

%=  :- was_export(infix_op/2).

%= 	 	 

%% infix_op( ?Op, ?VALUE2) is semidet.
%
% Infix Oper..
%
infix_op(Op,_):-comparitiveOp(Op).
infix_op(Op,_):-additiveOp(Op).

%=  :- was_export(comparitiveOp/1).

%= 	 	 

%% comparitiveOp( +Op) is semidet.
%
% Comparitive Oper..
%
comparitiveOp((\=)).
comparitiveOp((\==)).
% comparitiveOp((=)).
comparitiveOp((=:=)).
comparitiveOp((==)).
comparitiveOp((<)).
comparitiveOp((>)).
comparitiveOp((=<)).
comparitiveOp((>=)).

%=  :- was_export(additiveOp/1).

%= 	 	 

%% additiveOp( +Op) is semidet.
%
% Additive Oper..
%
additiveOp((is)).
additiveOp((*)).
additiveOp(+).
additiveOp(-).
additiveOp((/)).



%= 	 	 

%% is_unit( ?C) is semidet.
%
% If Is A Unit.
%
is_unit(A):-quietly(is_unit_like(A)).

is_unit_like(A):- atomic(A),!.
is_unit_like(C):-is_unit_like0(C).

is_unit_like0(C):- var(C),!, dictoo:oo_get_attr(C,skk,_),!.
is_unit_like0(C):- \+ compound(C),!.
is_unit_like0(C):- C\='VAR'(_),C\='$VAR'(_),C\=(_:-_),C\=ftRest(_),C\=ftListFn(_),get_functor(C,F),is_unit_functor(F).



%= 	 	 

%% is_unit_functor( ?F) is semidet.
%
% If Is A Unit Functor.
%
is_unit_functor(F):- (\+ atom(F)),!,fail.
is_unit_functor(F):-atom_concat('sk',_,F).
is_unit_functor(F):-atom_concat(_,'Fn',F).


%= 	 	 

%% get_ruleRewrite( ^ Sent, ?SentM) is semidet.
%
% Get Rule Rewrite.
%
% TODO - remove the fail (added just to speed up testing and initial debugging)
get_ruleRewrite(Sent,SentM):- fail, cheaply_u(ruleRewrite(Sent,SentM)).



transitive_lc_nr(P,A,B):- call(P,A,B),!.
transitive_lc_nr(_,A,A).
%= 	 	 

renamed_atom(F,FO):-atom(F),if_defined(best_rename(F,FO),fail),!.

%% mpred_expand_rule( ?PfcRule, ?Out) is det.
%
% Managed Predicate Expand.
%
mpred_expand_rule(PfcRule,Out):- 
   is_ftCompound(PfcRule),
   safe_functor(PfcRule,F,A),
   clause_b(mpred_database_term(F,A,_)),
   PfcRule  univ_safe  [F|Args],maplist(fully_expand_goal(assert),Args,ArgsO),!,Out  univ_safe  [F|ArgsO].

is_parse_type(Var):- \+ compound(Var),!,fail.
is_parse_type('kif'(NV)):-nonvar(NV).
is_parse_type('pkif'(NV)):-nonvar(NV).

%% db_expand_final( +Op, +TermNC, ?NC) is semidet.
%
% Database Expand Final.
%


string_to_mws_2(NC,NG):- \+ ground(NC),!, NG=NC.
string_to_mws_2([String,A|B],OUT):- is_list(B),!,OUT  univ_safe  [s,String,A|B].
string_to_mws_2([String],String):-!.
string_to_mws_2(String,String).


string_to_mws(NC,NCO):- string(NC),!,convert_to_cycString(NC,NCM),string_to_mws_2(NCM,NCO).
string_to_mws(NC,_):- \+ compound(NC),!,fail.
string_to_mws(NC,NO):- safe_functor(NC,s,_),!,NO=NC.
string_to_mws([String],NCO):-string(String),!,must((convert_to_cycString(String,NCM),string_to_mws_2(NCM,NCO))).

string_to_mws([String,A|B],OUT):- (string(String);string(A)),!,must((string_to_mws_2([String,A|B],OUT))).
%MAYBE string_to_mws([String|Rest],O):-string(String),!,(Rest==[]->O=String;O=[String|Rest]).


db_expand_final(_ ,NC,NC):-  is_ftVar(NC),!.
%db_expand_final(Op,t(EL),O):- !, db_expand_final(Op,EL,O).
db_expand_final(change(assert,_),props(_Obj,List),true):-  List==[],dumpST,!.
db_expand_final(_,props(Obj,List),{nonvar(Obj)}):- (List==[] ; List==true).
db_expand_final(_ ,sumo_rule(NC),sumo_rule(NC)):- !.

db_expand_final(Op, CMP,    O  ):- compound(CMP),meta_argtypes(Args)=CMP,
  is_ftCompound(Args),safe_functor(Args,Pred,A),
    (Pred=t->  (fully_expand_head(Op, Args,ArgsO),O=meta_argtypes(ArgsO)) ; 
      (assert_arity(Pred,A),O=meta_argtypes(Args))).

% db_expand_final(_,NC,NCO):- string_to_mws(NC,NCO),!.

%db_expand_final(_ ,NC,NCO):- atom(NC),do_renames_expansion(NC,NCO),!.
db_expand_final(_ ,NC,NC):- atomic(NC),!.
db_expand_final(_,PARSE,_):- is_parse_type(PARSE),!,fail.
db_expand_final(Op,M:Sent,SentO):- atom(M),is_stripped_module(M),!,db_expand_final(Op,Sent,SentO).
db_expand_final(_,Sent,_):- arg(_,Sent,E),compound(E),safe_functor(E,isEach,_),throw(hasEach).
db_expand_final(_,[_|_],_):- !,fail.
db_expand_final(_,Arg,Arg):- safe_functor(Arg,s,_),!.
db_expand_final(_,Sent,Sent):- Sent  univ_safe  [_,A],(atom(A);var(A);number(A);is_ftVar(A)),!.

db_expand_final(_ ,isa(Args,Meta_argtypes),  meta_argtypes(Args)):-Meta_argtypes==meta_argtypes,!,is_ftCompound(Args),!,safe_functor(Args,Pred,A),assert_arity(Pred,A).
% covered db_expand_final(Op,Sent,Sent):- Sent  univ_safe  [_,A],atom(A),!.
%db_expand_final(_,PARSE,ISA):- PARSE  univ_safe  [t,C,I],atom(C),atom(I),ISA  univ_safe  [C,I],!.
% covered db_expand_final(_ ,NC,NC):-safe_functor(NC,_,1),arg(1,NC,T),(not_ftCompound(T)),!.
db_expand_final(_, Sent,Sent):-is_src_true(Sent).
% covered db_expand_final(_,Term,Term):- is_ftCompound(Term),safe_functor(Term,F,_),(cheaply_u(prologBuiltin(F));cheaply_u(rtArgsVerbatum(F))).
% covered db_expand_final(_, arity(F,A),arity(F,A)):- not_ftCompound(F),not_ftCompound(A),!, (maybe_ain_arity(F,A)).
%unused db_expand_final(_, tPred(V),tPred(V)):-!,fail, not_ftCompound(V),!.
%db_expand_final(_ ,NC,NC):-safe_functor(NC,_,1),arg(1,NC,T),db_expand_final(_,T,_),!.

db_expand_final(_ ,IN,OUT):- IN  univ_safe  [F,A,B], \+ is_ftVar(A), \+ is_ftVar(B), clause_b(rtSymmetricBinaryPredicate(F)), (A@<B -> OUT=IN ; OUT  univ_safe  [F,B,A]).

db_expand_final(_ ,isa(Atom,PredArgTypes), tRelation(Atom)):-PredArgTypes==meta_argtypes,atom(Atom),!.
db_expand_final(_ ,meta_argtypes(F,Args),    meta_argtypes(Args)):-atom(F),!,safe_functor(Args,Pred,A),assert_arity(Pred,A).
%covered db_expand_final(_ ,meta_argtypes(Args),      meta_argtypes(Args)):-!.
%covered db_expand_final(_ ,meta_argtypes_guessed(Args),      meta_argtypes_guessed(Args)):-!.
%covered db_expand_final(Op,(A,B),(AA,BB)):-  !,db_expand_final(Op,A,AA),db_expand_final(Op,B,BB).
%db_expand_final(Op,props(A,B),PROPS):- (is_ftNonvar(A);is_ftNonvar(B)),!,expand_props(_,Op,props(A,B),Props),!,Props\=props(_,_),fully_expand_head(Op,Props,PROPS).
db_expand_final(_ ,NCO,NCO):- NCO   univ_safe  [F,A|R],as_is_term(F,A,R),!.
/*
db_expand_final(_, MArg1User, NewMArg1User):- is_ftCompound(MArg1User), fail,
   MArg1User  univ_safe  [M,Arg1,Arg2|User],
   compound_all_open(Arg1),
   get_functor(Arg1,F,A),F\==(t),F\==(/),
   member(F,[arity,predicateConventionMt]),
   NewMArg1User  univ_safe  [M,F/A,Arg2|User],!.
*/



%= 	 	 

%% is_elist_functor( +Op) is semidet.
%
% If Is A Elist Functor.
%
is_elist_functor(isList).
is_elist_functor(ftListfn).
is_elist_functor(isEach).
is_elist_functor(isAnd).


%= 	 	 

%% as_list( ?EC, ?AL) is det.
%
% Converted To List.
%
as_list(ftListFn(Atom),[Atom]):- atom(Atom),!.
as_list(Atom,[]):-is_elist_functor(Atom),!.
as_list(_,Atom):- \+ var(Atom), \+ is_list(Atom),!,fail.
as_list(EC,AL):-compound(EC),EC  univ_safe  [IsEach,A|List],is_elist_functor(IsEach),!,((List==[],is_list(A))->AL=A;AL=[A|List]).
as_list(List,AL):-sanity(is_list(List)),AL=List.



%= 	 	 

%% listToE( ?EL, ?E) is det.
%
% List Converted To E.
%
listToE(EL,E):-nonvar(EL),!,must(as_list(EL,List)),sanity(is_list(List)),E  univ_safe  [isEach|List].



%= 	 	 

%% db_expand_chain( +Op, ?M, ?PO) is det.
%
% Database Expand Chain.
%
db_expand_chain(_,V,_):-var(V),!,fail.
db_expand_chain(_,M:PO,PO) :- atom(M),!.
db_expand_chain(_,isa(I,Not),INot):-Not==not,!,INot   univ_safe   [Not,I].
db_expand_chain(_,_,_):- t_l:into_goal_code,!,fail.
db_expand_chain(_,(P:-B),P) :-is_src_true(B),!.
db_expand_chain(_,B=>P,P) :-is_src_true(B),!.
db_expand_chain(_,<=(P,B),P) :-is_src_true(B),!.
db_expand_chain(_,P<==>B,P) :-is_src_true(B),!.
db_expand_chain(_,B<==>P,P) :-is_src_true(B),!.
db_expand_chain(_,P<-B,P) :-is_src_true(B),!.
%db_expand_chain(_,P,PE):-fail,cyc_to_clif_entry(P,PE).
%db_expand_chain(_,('nesc'(P)),P) :- !.




%% fully_expand_head( ?A, ?B, ?C) is semidet.
%
% Database Expand A Noloop.
%

% covered fully_expand_head(_,Sent,SentO):- as_is_term(Sent),!,SentO=Sent,!.

fully_expand_head(Why,Before,After):-
  % quietly(subst(Before,mpred_isa,isa,Before1)),
  into_mpred_form(Before,Before2),
  must(try_expand_head(Why,Before2,After1)),
  must(post_expansion(Why,After1,After)).


try_expand_head(_,A,B):- t_l:infSkipFullExpand,!,A=B.
% try_expand_head(Op,Sent,SentO):- transitive_lc(db_expand_0(Op),Sent,OO),!,SentO=OO.


try_expand_head(Op,~Sent,~SentO):- nonvar(Sent),!,try_expand_head(Op,Sent,SentO).
try_expand_head(Op,Sent,SentO):- db_expand_0(Op,Sent,M)->( M==Sent->SentO=M;db_expand_0(Op,M,SentO)),!.


:- meta_predicate temp_comp(*,*,2,?).

% prolog_clause fully_expand_clause
temp_comp(H,B,PRED,OUT):- nonvar(H),term_variables(B,Vs1),Vs1\==[], term_attvars(B,AVs1), AVs1==[],   
   quietly((asserta(('$temp_comp123'(H,B):- B),Ref),clause('$temp_comp123'(H,_),BO,Ref),erase(Ref))),
   B\=@=BO,!,
   must((term_variables(BO,Vs2),!,must_maplist(=,Vs1,Vs2),call(PRED,(H:-BO),OUT))).


%:- discontiguous db_expand_0/3.
%:- discontiguous(pfc_lib:db_expand_0/3).
%% db_expand_0( ?Op, ^ Sent, -- SentO) is semidet.
%
% Database expand  Primary Helper.
%
% :- meta_predicate(term_expansion(':'(:-),(:-))).
% :- mode(term_expansion(+,--)).

db_expand_0(_Op,Sent,Sent):- var(Sent),!.
db_expand_0(Op,not(Sent),not(SentO)):- !, db_expand_0(Op,Sent,SentO).
db_expand_0(Op,\+(Sent),\+(SentO)):- !, db_expand_0(Op,Sent,SentO).
db_expand_0(Op,~(Sent),~(SentO)):- !, db_expand_0(Op,Sent,SentO).
db_expand_0(Op,poss(Sent),poss(SentO)):- !, db_expand_0(Op,Sent,SentO).
db_expand_0(Op,nesc(Sent),nesc(SentO)):- !, db_expand_0(Op,Sent,SentO).
db_expand_0(Op,Sent,SentO):- cyclic_break(Sent),db_expand_final(Op ,Sent,SentO),!.

db_expand_0(_,Sent,Sent):- \+ compound(Sent),!.

db_expand_0(_Op,GG,SentO):-ground(GG),GG  univ_safe  [_,G],(G= -kif(_);G= -pkif(_)),!,SentO=G.
db_expand_0(Op,pkif(SentI),SentO):- nonvar(SentI),!,must((any_to_string(SentI,Sent),must(expand_kif_string_or_fail(Op,Sent,SentM)),SentM\=@=Sent,!,db_expand_0(Op,SentM,SentO))).
db_expand_0(_Op,kif(Sent),SentO):- nonvar(Sent),!, must(expand_kif_string(Sent,SentM)),if_defined(sexpr_sterm_to_pterm(SentM,SentO),SentM=SentO).

%TODO DONT RUIN 
db_expand_0(Op,==>(EL),O):- !, db_expand_0(Op,EL,O).
db_expand_0(_,t(Sent),t(Sent)):- ftVar(Sent),!.
%TODO DONT RUIN   
db_expand_0(Op,t(EL),O):- !, db_expand_0(Op,EL,O).

db_expand_0(Op,[G|B],[GG|BB]):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB).
db_expand_0(_Op,=>(G,B),=>(G,B)):-!.
db_expand_0(Op,(G,B),(GG,BB)):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB).
db_expand_0(Op,G:B,GG:BB):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB).

db_expand_0(Op,{Sent},{SentO}):- !,fully_expand_goal(Op,Sent,SentO),!.

db_expand_0(Op,SentI,SentO):- SentI  univ_safe  [NOT,Sent],arg(_,v( ( \+ ), '{}' , (~) , ( :- )  ),NOT),
  db_expand_0(Op,Sent,SentM)-> 
  (Sent\=@=SentM -> (SentMM  univ_safe  [NOT,SentM],fully_expand_goal(Op,SentMM,SentO)) ; SentO   univ_safe  [NOT,SentM]),!.


db_expand_0(_,Sent,SentO):- copy_term(Sent,NoVary),get_ruleRewrite(Sent,SentO),must(Sent\=@=NoVary),SentO \=@= Sent.
db_expand_0(call(Op),Sent,SentO):-  mreq(quasiQuote(QQuote)),subst(Sent,QQuote,isEach,MID),Sent\=@=MID,!,must(db_expand_0(call(Op),MID,SentO)).

db_expand_0(Op,Sent,SentO):- transitive_lc(db_expand_chain(Op),Sent,SentO)-> SentO \=@= Sent.


db_expand_0(_Op,P,PO):-db_expand_argIsa(P,PO),!.

db_expand_0(_Op,P,PO):- fail,
  compound(P),
  P  univ_safe  [_TYPE,UNIT],
  is_unit(UNIT),!,
  PO=P.

%:- kb_shared(is_svo_functor/1).

% prolog_clause db_expand_0
% db_expand_0(_Op,(H:-B),(H:-B)):- !.
db_expand_0(Op,(H:-B),OUT):- fully_expand_clause(Op,(H:-B),OUT),!,  
                         (((H:-B)=@=OUT)->true;dmsg_pretty(warn(db_expand_0(Op,(H:-B),OUT)))).
% prolog_clause db_expand_0
% db_expand_0(Op,(H:-B),OUT):- temp_comp(H,B,db_expand_0(Op),OUT),!.
db_expand_0(Op,(:-(CALL)),(:-(CALLO))):-with_assert_op_override(Op,db_expand_0(Op,CALL,CALLO)).
db_expand_0(Op,isa(I,O),INot):-Not==not,!,INot   univ_safe   [Not,I],!,db_expand_0(Op,INot,O).
db_expand_0(Op,isa(I,O),INot):-Not== ( \+ ) ,!,INot   univ_safe   [Not,I],!,db_expand_0(Op,INot,O).

db_expand_0(Op,THOLDS,OUT):- THOLDS  univ_safe  [t,P|ARGS],atom(P),!,HOLDS  univ_safe  [P|ARGS],db_expand_0(Op,HOLDS,OUT).
db_expand_0(Op,RDF,OUT):- RDF  univ_safe  [SVO,S,V,O],if_defined(is_svo_functor(SVO)),!,must_det(from_univ(_,Op,[V,S,O],OUT)).
db_expand_0(Op,G,OUT):- G  univ_safe  [Pred,InstFn,VO],compound(InstFn),InstFn=isInstFn(Type),is_ftNonvar(Type),from_univ(relationMostInstance,Op,[Pred,Type,VO],OUT).
db_expand_0(Op,G,OUT):- G  univ_safe  [Pred,InstFn|VO],compound(InstFn),InstFn=isInstFn(Type),is_ftNonvar(Type),GO  univ_safe  [Pred,Type|VO],db_expand_0(Op,GO,OUT).

db_expand_0(Op,props(A,F),OO):-!,expand_props(_Prefix,Op,props(A,F),OO),!.
db_expand_0(Op,iprops(A,F),OO):-!,expand_props(_Prefix,Op,props(A,F),OO),!.
db_expand_0(Op,upprop(A,F),ain(OO)):-!,expand_props(_Prefix,Op,props(A,F),OO),!.
db_expand_0(Op,padd(A,F),ain(OO)):-!,expand_props(_Prefix,Op,props(A,F),OO),!.


db_expand_0(Op,(call_u(CALL)),(call_u(CALLO))):- with_assert_op_override(Op,db_expand_0(Op,CALL,CALLO)).
%db_expand_0(_ ,include(CALL),(load_data_file_now(CALL))):- dtrace, !.
db_expand_0(_ ,include(CALL),(include(CALL))):- !.

db_expand_0(Op,=>(G),(GG)):-!,db_expand_0(Op,(G),(GG)).
db_expand_0(Op,(G,B),(GGBB)):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB),conjoin_l(GG,BB,GGBB).

db_expand_0(Op,(G==>B),(GG==>BB)):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB).


db_expand_0(Op,(G;B),(GG;BB)):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB).
db_expand_0(Op,(G:-B),(GG:-BB)):-!,db_expand_0(Op,G,GG),fully_expand_goal(Op,B,BB).

db_expand_0(Op,M:Sent,SentO):- atom(M),is_stripped_module(M),!,db_expand_0(Op,Sent,SentO).
db_expand_0(Op,M:Sent,R:SentO):- replaced_module(Op,M,R),!,db_expand_0(Op,Sent,SentO).

:- if(false).
db_expand_0(_Op,pddlSomethingIsa(I,EL),(isa(I,IC),O)):- icn_tcn(I,IC), listToE(EL,E),expand_isEach_or_fail(==>genls(IC,E),O),!.
:- endif.

db_expand_0(_Op,pddlSomethingIsa(I,EL),O):- listToE(EL,E),expand_isEach_or_fail(==>isa(I,E),O).
db_expand_0(_Op,pddlDescription(I,EL),O):- listToE(EL,E),expand_isEach_or_fail(==>mudDescription(I,E),O).
db_expand_0(_Op,pddlObjects(I,EL),O):- listToE(EL,E),expand_isEach_or_fail(==>isa(E,I),O).
db_expand_0(_Op,pddlSorts(I,EL),O):- listToE(EL,E),expand_isEach_or_fail(==>genls(E,I),O).
db_expand_0(_Op,pddlTypes(EL),O):- listToE(EL,E),expand_isEach_or_fail(==>isa(E,tCol),O).
db_expand_0(_Op,pddlPredicates(EL),O):- listToE(EL,E),expand_isEach_or_fail(==>prologHybrid(E),O).

db_expand_0(_,prop_mpred(M,RT,F,A),mpred_prop(M,F,A,RT)).

db_expand_0(Op,DECL,OUT):- 
    is_ftCompound(DECL)->
    DECL  univ_safe  [D,FA|Args0] ->
    functor_declares_instance_not_ft(D,DT)->
    flat_list(Args0,Args)->   
    maplist(nonvar,[FA|Args]) ->
    db_expand_set(Op,[DT,FA|Args],OUT).

db_expand_0(_,Sent,mpred_prop(M,F,A,RT)):- Sent  univ_safe  [RT,MFA],a(ttRelationType,RT),nonvar(MFA),get_mfa(MFA,M,F,A),atom(F),!.

% @TODO uncomment IMMEDIATELY
db_expand_0(Op,ClassTemplate,OUT):- \+ t_l:no_db_expand_props, db_expand_props(Op,ClassTemplate,OUT),!.
% db_expand_0(Op,C,F/A):-compound_all_open(C),get_functor(C,F,A).
db_expand_0(Op,IN,OUT):- IN  univ_safe  [F|Args],F==t,!,must(from_univ(_,Op,Args,OUT)).
db_expand_0(Op,isa(A,F),OO):-atom(F),O  univ_safe  [F,A],!,db_expand_0(Op,O,OO).
db_expand_0(Op,isa(A,F),OO):-is_ftNonvar(A),is_ftNonvar(F),expand_props(_Prefix,Op,props(A,F),OO),!.
db_expand_0(_Op,isa(A,F),isa(A,F)):-!.
db_expand_0(Op,props(A,F),OO):-expand_props(_Prefix,Op,props(A,F),OO),!.
db_expand_0(Op,typeProps(A,F),EXP):-expand_props(_Prefix,Op,props(I,F),OO),!,fully_expand(Op,(isa(I,A)==>OO),EXP).

% covered db_expand_0(_,arity(F,A),arity(F,A)):-atom(F),!.
db_expand_0(Op,IN,OUT):- 
   cnas(IN,F,Args),
   % wdmsg_pfc(db_expand_0(Op,IN)),
   sanity(F \== isa),
   must_maplist(db_expand_0(Op),Args,ArgsO),
   map_f(F,FO),OUT  univ_safe  [FO|ArgsO].


/*
db_expand_0(Op,Mt:Term,Mt:O):- is_kb_module(Mt),!,locally_tl(caller_module(baseKB,Mt),db_expand_0(Op,Term,O)).
db_expand_0(Op,DB:Term,DB:O):- defaultAssertMt(DB),!,locally_tl(caller_module(db,DB),db_expand_0(Op,Term,O)).
db_expand_0(Op,KB:Term,KB:O):- atom(KB),!,locally_tl(caller_module(prolog,KB),db_expand_0(Op,Term,O)).
*/

% db_expand_0(query(HLDS,Must),props(Obj,Props)):- is_ftNonvar(Obj),is_ftVar(Props),!,gather_props_for(query(HLDS,Must),Obj,Props).

get_mfa(M:FA,M,F,A):- !, get_fa(FA,F,A).
get_mfa(FA,M,F,A):- get_fa(FA,F,A),must(current_assertion_module(M)).


flat_list([Args],Args):-is_list(Args),!.
flat_list(Args,Args).

functor_declares_instance_not_ft(F,C):- functor_declares_instance_0(F,C0),!,C=C0. % , nop(sanity(F\=C0)).

% tSet(tMySet,comment("this was my set"))
db_expand_set(Op,[TPRED,F,A|Args],OUT):- atom(F),number(A),!,db_expand_set(Op,[TPRED,F/A|Args],OUT),!, (maybe_ain_arity(F,A)).
db_expand_set(Op,[TPRED,F/A|Args],OUT):- is_ftNameArity(F,A),
   db_expand_set(Op,[TPRED,F| Args],OUT),!,
    (maybe_ain_arity(F,A)).
db_expand_set(Op,[TPRED,F|Args],OUT):- atom(F),!,db_expand_0(Op,props(F,[TPRED|Args]),OUT).
db_expand_set(Op,[TPRED,FARGS|Args],(meta_argtypes(FARGS),OUT)):- 
   compound(FARGS), \+ ((arg(_,FARGS,E),is_ftVar(E))),safe_functor(FARGS,F,A),!,
   db_expand_set(Op,[TPRED,F/A|Args],OUT).


:- thread_local(t_l:no_db_expand_props/0).


db_expand_props(Op,DECL,O):- fail, arg(_,DECL,S),string(S),DECL  univ_safe  [F|Args],maplist(destringify,Args,ArgsO),
  ArgsO\=@=Args,!,DECLM  univ_safe  [F|ArgsO],db_expand_0(Op,DECLM,O).


db_expand_props(Op,DECL,((isa(F,TPRED),O))):-DECL  univ_safe  [D,FA|Args],compound(FA),FA= (F/A),
  is_ftNameArity(F,A),functor_declares_instance(D,TPRED),
  is_ftNonvar(TPRED),is_relation_type(TPRED),expand_props(_Prefix,Op,props(F,[D|Args]),O),!,
   (maybe_ain_arity(F,A)).

db_expand_props(Op,DECL,(isa(F,TPRED),O)):-DECL  univ_safe  [D,F,A|Args],is_ftNameArity(F,A),functor_declares_instance(D,TPRED),
  arity_zor(D,1),
  is_ftNonvar(TPRED),is_relation_type(TPRED),expand_props(_Prefix,Op,props(F,[D|Args]),O),!,
   (maybe_ain_arity(F,A)).

:- if(false).
db_expand_props(Op,DECL,(isa(F,TPRED),O)):-DECL  univ_safe    [D,C|Args],is_ftCompound(C),functor_declares_instance(D,TPRED),
  \+ is_ftVar(C),
  \+ \+ is_non_unit(C),
  get_functor(C,F,A),  
  arity_zor(D,1),
  is_ftNonvar(TPRED),expand_props(_Prefix,Op,props(F,[D|Args]),M),!,
  (\+((arg(_,C,Arg),is_ftVar(Arg))) -> O = (meta_argtypes(C),M) ; (O= (M))),
   (maybe_ain_arity(F,A)).
:- endif.

db_expand_props(Op,DECL,O):-DECL  univ_safe  [D,F,A1|Args], functor_declares_instance(D,DType),
   arity_zor(D,1),
   %\+ is_relation_type(DType),
   expand_props(_Prefix,Op,props(F,[DType,D,A1|Args]),O),!.

db_expand_props(Op,DECL,O):-DECL  univ_safe  [D,F|Args],functor_declares_instance(D,DType),
   %\+ is_relation_type(DType),
   arity_zor(D,1)->
   expand_props(_Prefix,Op,props(F,[DType,D|Args]),O),!.


% shift/1 reset/3
%  room_template(iLivingRoom7,.....).
db_expand_props(Op,ClassTemplate,(tCol(PropsIsa),isa(Inst,PropsIsa),OUT)):- 
   ClassTemplate  univ_safe  [TypePropsFunctor,Inst|Props],
   functor_declares_instance(TypePropsFunctor,PropsIsa),
   arity_zor(TypePropsFunctor,1)->
   \+ compound_all_open(ClassTemplate),
   %ain(isa(PropsIsa,tCol)),
   %ain(isa(Inst,PropsIsa)),
   expand_props(t,Op,props(Inst,[PropsIsa|Props]),OUT),!.

% typeProps(tCrackers,.....).
db_expand_props(Op,ClassTemplate,(tCol(PropsIsa),isa(Type,PropsIsa),OUT)):-
   ClassTemplate  univ_safe  [TypeTypePropsFunctor,Type|Props],
   functor_declares_collectiontype(TypeTypePropsFunctor,PropsIsa),
   arity_zor(TypeTypePropsFunctor,1),
   \+ compound_all_open(ClassTemplate),
   %ain(isa(Type,tCol)),
   %ain(isa(Type,PropsIsa)),
   expand_props(relationMostInstance,Op,props(Type,Props),OUT),!.

% tRegion_inst_template(X, tLivingRoom,.....).
db_expand_props(Op,ClassTemplate,(isa(TypePropsIsa,Type),ONEPROP)):- isa_one_prop(NewInst,Type,OUT,ONEPROP),
  ClassTemplate  univ_safe  [FunctorTypePropsIsa,NewInst,Type|Props],
  instTypePropsToType(FunctorTypePropsIsa,TypePropsIsa),
  arity_zor(FunctorTypePropsIsa,2),
   \+ compound_all_open(ClassTemplate),
  expand_props(Op,props(NewInst,Props),OUT),!.

/*

% tRegion_template(tLivingRoom,.....).
db_expand_props(Op,typeProps(C,Props),(isa(I,C)==>mdefault(OOUT))):- (is_ftNonvar(C);is_ftNonvar(Props)), expand_props(Prefix,Op,props(I,Props),OUT),dtrace,list_to_conjuncts(OUT,OUTC),conjuncts_to_list(OUTC,OUTL),
   ISEACH  univ_safe  [isEach|OUTL],
  db_expand_0(Op,mdefault(ISEACH),OOUT).

*/

db_expand_props(Op,ClassTemplate,OUT):- ClassTemplate  univ_safe  [props,Inst,Second,Third|Props],!,
   must(expand_props(_Prefix,Op,props(Inst,[Second,Third|Props]),OUT)),!.
db_expand_props(Op,arity(F,A),O):-expand_props(_Prefix,Op,props(F,arity(A)),O),!.

maybe_ain_arity(F,A):- ignore((atom(F),integer(A),ain(arity(F,A)))).


isa_one_prop(NewInst,Type,OUT,ONEPROP):- ONEPROP = (isa(NewInst,Type)==>OUT).

%= 	 	 

%% is_arity_pred( +Op) is semidet.
%
% If Is A Arity Predicate.
%
is_arity_pred(argIsa).
is_arity_pred(arity).

arity_zor(D,ZOR) :- atom(D),D\==isa, \+ (arity_no_bc(D,N),!,N>ZOR).

%= 	 	 

%% map_f( ?F, ?F) is semidet.
%
% Map False.
%
map_f(M:F,M:FO):-atom(M),map_f(F,FO).
% map_f(mpred_isa,isa).
% map_f(props,isa).
map_f(F,F):-!.


%= 	 	 

%% ex_argIsa( ?P, ?N, ?C) is semidet.
%
% ex Argument  (isa/2).
%
ex_argIsa(P,N,C):- clause(_:argIsa(P,N,C),true).

db_expand_argIsa(P,_):- \+ compound(P),!,fail.
db_expand_argIsa(P,_):- is_dict(P),!,fail.
db_expand_argIsa(P,PO):- 
  P  univ_safe  [ARE,FF,AA],
   atom_concat('arg',REST,ARE),
   member(E,['Genl','Isa','SometimesIsa','Format','QuotedIsa']),atom_concat(N,E,REST),
   atom_number(N,NN),
   atom_concat('arg',E,AE),
  PO  univ_safe  [AE,FF,NN,AA],!.

db_expand_argIsa(P,PO):- 
  P  univ_safe  [ARE,FF,C1,C2],
   atom_concat('interArg',REST,ARE),
   member(E,['Isa','Genl','Format','QuotedIsa','GenlQuantity','NotIsa','SometimesIsa','NotQuotedIsa']),
   atom_concat(E,Nums,REST),
   (atomic_list_concat([A1,A2],'-',Nums);atomic_list_concat([A1,A2],'_',Nums)),!,
   atom_number(A1,N1),
   atom_number(A2,N2),
   atomic_list_concat(['interArg',E],AE),
  PO  univ_safe  [AE,FF,N1,C1,N2,C2],!.

db_expand_argIsa(P,PO):- 
  P  univ_safe  [ARE,FF,AA,RESULT],
   atom_concat('interArg',REST,ARE),
   member(E,['ResultGenl','ResultIsa','ResultNotIsa','ResultSometimesIsa','ResultFormat','ResultQuotedIsa','ResultNotQuotedIsa']),
   atom_concat(N,E,REST),
   atom_number(N,NN),
   atom_concat('interArg',E,AE),
  PO  univ_safe  [AE,FF,NN,AA,RESULT],!.


%= 	 	 

%% compound_all_open( ?C) is semidet.
%
% Compound All Open.
%
compound_all_open(C):-compound(C),safe_functor(C,_,A),A>1,\+((arg(_,C,Arg),is_ftNonvar(Arg))),!.

replaced_module(_,_,_):-!, fail.
replaced_module(_,V,_):- \+ atom(V),!,fail.
replaced_module(_,umt,ABox):-defaultAssertMt(ABox).
replaced_module(_,abox,ABox):-defaultAssertMt(ABox).
replaced_module(_,tbox,TBox):-get_current_default_tbox(TBox).

:- thread_local(t_l:current_defaultAssertMt/1).

maybe_prepend_mt(MT,I,O):- t_l:current_defaultAssertMt(ABOX)->ABOX==MT,!,maybe_prepend_mt(abox,I,O).
maybe_prepend_mt(abox,H,HH):-nonvar(HH),dtrace,maybe_prepend_mt(abox,H,HHH),must(HHH=HH),!.
maybe_prepend_mt(abox,H,HH):-var(H),must(HH=H),!.
maybe_prepend_mt(_,CL,CL):- compound(CL),CL=(_,_),!.
maybe_prepend_mt(_,H,HH):-predicateSystemCode(H,HH),!.
maybe_prepend_mt(abox,_:HH,HH):-!.
maybe_prepend_mt(abox,HH,HH):-!.
maybe_prepend_mt(Mt,Mt:HH,Mt:HH):-!.
maybe_prepend_mt(_,Mt:HH,Mt:HH):-!.
maybe_prepend_mt(Mt,HH,Mt:HH):-!.

predicateSystemCode(P,PP):-strip_module(P,_,PP),predicate_property(system:PP,defined),
  \+ predicate_property(system:PP,imported_from(baseKB)).

%% remodulize( ?Op, ?H, ?HH) is det.
%
% Re-Modulize.
%

remodulize(_, H,H):-!.

remodulize(_, H,H):- is_ftVar(H),!.
remodulize(_, H,H):- \+ compound(H),!. % this disables the two next rules
remodulize(Op, H,HH):- atom(H),strip_module(H,FROM,_HHH),convention_to_symbolic_mt(FROM,Op,H,0,M),maybe_prepend_mt(M,H,HH).
remodulize(call(Op),M,R):-atom(M),replaced_module(Op,M,R),!.
remodulize(Op,M:H,M:HHH):-is_ftVar(M),!,must_remodulize(mvar(Op),H,HHH).
remodulize(Op,H,HH):-is_list(H),!,must_maplist(remodulize(Op),H,HH),!.
remodulize(Op,':-'(G),':-'(GG)):-!,must_remodulize(call(Op),G,GG).
remodulize(Op,(H:-G),(HH:-GG)):-!,must_remodulize(clause(Op,(':-')),H,HH),must_remodulize(call(Op),G,GG).
remodulize(Op,(H,G),(HH,GG)):-!,must_remodulize(call(Op),H,HH),must_remodulize(call(Op),G,GG).
remodulize(Op,(H;G),(HH;GG)):-!,must_remodulize(call(Op),H,HH),must_remodulize(call(Op),G,GG).

remodulize(Op,M:H,R:HHH):- replaced_module(Op,M,R),!,must_remodulize(Op,H,HHH).
remodulize(Op,M:H,HHH):- is_stripped_module(M),!,must_remodulize(Op,H,HHH).

remodulize(Op,Mt:H,HHHH):- is_ftCompound(H),H  univ_safe  [F|HL],!,must_maplist(remodulize(Op),HL,HHL),HH  univ_safe  [F|HHL],!,
  must((remodulize_pass2(Op,HH,HHH),maybe_prepend_mt(Mt,HHH,HHHH))).

remodulize(Op,H,HHH):-is_ftCompound(H),H  univ_safe  [F|HL],!,must_maplist(remodulize(Op),HL,HHL),HH  univ_safe  [F|HHL],!,
  must(remodulize_pass2(Op,HH,HHH)).

remodulize_pass2(Op,MHH,HHH):- strip_module(MHH,FROM,HH),safe_functor(HH,F,A),convention_to_symbolic_mt(FROM,Op,F,A,Mt),maybe_prepend_mt(Mt,HH,HHH).
% remodulize_pass2(Op,HH,HHH):- fix_mp(Op,HH,HHH),!. % this is overzealous
remodulize_pass2(_Why,HH,HH):- !.

%:- kb_shared(is_sentence_functor/1).

must_remodulize(Op,H,HHH):-must(demodulize(Op,H,HHH)),!.
%= 	 	 

%% is_meta_functor( ^ Sent, ?F, ?List) is semidet.
%
% If Is A Meta Functor.
%
is_meta_functor(Sent,F,List):-is_ftCompound(Sent),Sent  univ_safe  [F|List],
 (predicate_property(Sent,meta_predicate(_));
   is_sentence_functor(F);F==pfcDefault),!.






%% is_sentence_functor( ?And) is semidet.
%
% If Is A Sentence Functor.
%
is_sentence_functor(And):-quietly(is_logical_functor0(And)).



%% is_logical_functor0( ?X) is semidet.
%
% If Is A Logical Functor Primary Helper.
%
is_logical_functor0(&).
is_logical_functor0(v).
is_logical_functor0(exists).
is_logical_functor0(all).
is_logical_functor0(X):-atom(X),member(X,[',',';',xor,'\\+',~]).
is_logical_functor0(X):- a(logical_functor_pttp,X).
is_logical_functor0(X):- a(is_quantifier,X).
is_logical_functor0(And):-member(And,[(,),(;),('<-'),('=>'),('<=>'),(':-'),(and),nop]).



%= 	 	 

%% from_univ( ?Prefix, ?Op, :TermMORE, ?Out) is semidet.
%
% Converted From Univ.
%
from_univ(Prefix,Op,[T|MORE],Out):-T==t,!,from_univ(Prefix,Op,MORE,Out).
% MAYBE from_univ(Prefix,Op,[C,I],Out):- is_tspec(C),!,to_isa_form(I,C,Out).

from_univ(Prefix,Op,[PROP,Obj|MORE],Out):-PROP==props,!,expand_props(Prefix,Op,props(Obj,MORE),Out).
% from_univ(Prefix,Op,MORE,Out):-atom(Prefix),!,from_univ(_,Op,[Prefix|MORE],Out).
from_univ(_Prefix,_Op,[PROP|MORE],Out):-atom(PROP),!,Out  univ_safe  [PROP|MORE]. % ,db_expand_up(Prefix,Op,Mid,Out).
from_univ(_Prefix,_Op,In,Out):- Out  univ_safe  [t|In],!.


%= 	 	 

%% db_expand_up( ?Prefix, ?Op, ?Mid, ?OOUT) is semidet.
%
% Database Expand Up.
%
db_expand_up(Prefix,Op,Mid,OOUT):- fully_expand_head(Op,Mid,Out), 
  is_ftCompound(Prefix),subst(Prefix,value,Out,OOUT).
db_expand_up(_,Op,Mid,Out):- fully_expand_head(Op,Mid,Out).



%% expand_props( ?Op, ?Term, ?OUT) is semidet.
%
% Expand Props.
%

expand_props(Op,Term,OUT):-expand_props(_,Op,Term,OUT).


%= 	 	 

%% expand_props( ?Prefix, ?VALUE2, ^ Sent, ^ Sent) is semidet.
%
% Expand Props.
%
expand_props(_Prefix,_,Sent,OUT):- t_l:no_db_expand_props, (not_ftCompound(Sent)),!,OUT=Sent.
%expand_props(Prefix,Op,Term,OUT):- stack_check,(is_ftVar(OpeR);is_ftVar(Term)),!,trace_or_throw_ex(var_expand_units(OpeR,Term,OUT)).
expand_props(Prefix,Op,Sent,OUT):-  Sent  univ_safe  [And|C12],is_sentence_functor(And),!,maplist(expand_props(Prefix,Op),C12,O12),OUT  univ_safe  [And|O12].
expand_props(_Prefix,_ ,props(Obj,Open),props(Obj,Open)):- is_ftVar(Open),!. % ,trace_or_throw_ex(expand_props(Prefix,Op,props(Obj,Open))->OUT).
expand_props(_Prefix,change(assert,_),props(_Obj,List),true):- List==[],!.
expand_props(_Prefix,_,props(Obj,List),{nonvar(Obj)}):- List==[],!.
% expand_props(_Prefix,_ ,props(_Obj,List),true):- List==[],!.
expand_props(Prefix,Op,props(Obj,[P|List]),OUT):- List==[],expand_props(Prefix,Op,props(Obj,P),OUT),!.
% expand_props(Prefix,Op,props(Obj,[P]),OUT):- is_ftNonvar(P),!,expand_props(Prefix,Op,props(Obj,P),OUT).
expand_props(Prefix,Op,props(Obj,[P|ROPS]),OUT):- !,expand_props(Prefix,Op,props(Obj,P),OUT1),
   expand_props(Prefix,Op,props(Obj,ROPS),OUT2),
   conjoin_l(OUT1,OUT2,OUT).
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- atom(PropVal),!,from_univ(Prefix,Op,[PropVal,Obj],OUT).

expand_props(_Prefix,_Op,props(Obj,PropVal),(PropVal2,{OPVAL})):- PropVal  univ_safe  [OpeR,Pred|Val],comparitiveOp(OpeR),
   not(comparitiveOp(Pred)),!,OPVAL  univ_safe  [OpeR,NewVar|Val],PropVal2  univ_safe  [Pred,Obj,NewVar],!.    

expand_props(_,_,props(Obj,PropVal),OUT):- var(Obj),atomic(PropVal), \+ atom(PropVal),OUT=[PropVal],!.
expand_props(_,_,props(Obj,PropValS),OUT):- var(Obj),member(PropVal,PropValS),atomic(PropVal), \+ atom(PropVal),OUT=PropValS,!.
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,from_univ(Prefix,Op,[Prop,Obj|Val],OUT).
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- 
   PropVal  univ_safe  [OpeR,Pred|Val],comparitiveOp(OpeR),
   not(comparitiveOp(Pred)),!,OPVAL  univ_safe  [OpeR|Val],PropVal2  univ_safe  [Pred,OPVAL],
    expand_props(Prefix,Op,props(Obj,PropVal2),OUT),!.
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- PropVal  univ_safe  [Prop|Val], \+ (infix_op(Prop,_)),!,from_univ(Prefix,Op,[Prop,Obj|Val],OUT).
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- PropVal  univ_safe  [Prop|Val],!,dtrace(from_univ(Prefix,Op,[Prop,Obj|Val],OUT)).
expand_props(Prefix,Op,props(Obj,Open),props(Obj,Open)):- trace_or_throw_ex(unk_expand_props(Prefix,Op,props(Obj,Open))).

expand_props(Prefix,OpeR,ClassTemplate,OUT):- ClassTemplate  univ_safe  [props,Inst,Second,Third|Props],!,
   expand_props(Prefix,OpeR,props(Inst,[Second,Third|Props]),OUT),!.

expand_props(_Prefix,_,Sent,Sent).


%= 	 	 

%% conjoin_l( ?A, :TermAA, ?C) is semidet.
%
% Conjoin (list Version).
%
conjoin_l(A,AA,C):-A==AA,!,C=A.
conjoin_l(A,AAB,C):- compound(AAB),AAB=(B,AA), A==AA,!,conjoin_l(A,B,C).
conjoin_l(A,AAB,C):- compound(AAB),AAB=(AA,B), A==AA,!,conjoin_l(A,B,C).
conjoin_l(A,B,C):-conjoin(A,B,C).



% ========================================
% into_mpred_form/2 (removes a second order functors until the common mpred form is left)
% ========================================
%=  :- was_export(into_mpred_form/2).

%= 	 	 

%% into_mpred_form( :TermV, ?VO) is semidet.
%
% Converted To Managed Predicate Form.
%

% into_mpred_form(Var,MPRED):- is_ftVar(Var), trace_or_throw_ex(var_into_mpred_form(Var,MPRED)).
into_mpred_form(V,VO):- (not_ftCompound(V)),!,VO=V.
into_mpred_form(M:X,M:O):- atom(M),!,into_mpred_form(X,O),!.
% convered into_mpred_form(Sent,SentO):-is_ftNonvar(Sent),get_ruleRewrite(Sent,SentM),!,into_mpred_form(SentM,SentO).
into_mpred_form((H:-B),(HH:-BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H:-B),(HH:-BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H,B),(HH,BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H;B),(HH;BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H/B),(HH/BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form(WAS,isa(I,C)):- was_isa_ex(WAS,I,C),!.
into_mpred_form(t(P),O):-is_ftNonvar(P),!,into_mpred_form(P,O).
into_mpred_form(t(P,A),O):-atom(P),!,O  univ_safe  [P,A].
into_mpred_form(t(P,A,B),O):-atom(P),!,O  univ_safe  [P,A,B].
into_mpred_form(t(P,A,B,C),O):-atom(P),!,O  univ_safe  [P,A,B,C].
into_mpred_form(IN,OUT):- 
   cnas(IN,F,Args),
   must_maplist(into_mpred_form,Args,ArgsO),!,
   map_f(F,FO),
   cnas(OUT,FO,ArgsO).


% into_mpred_form(I,O):- /*quietly*/(loop_check(into_mpred_form_ilc(I,O),O=I)). % trace_or_throw_ex(into_mpred_form(I,O).

%:- mpred_trace_nochilds(into_mpred_form/2).


%= 	 	 

%% into_mpred_form_ilc( ?G, ?O) is semidet.
%
% Converted To Managed Predicate Form Inside Of Loop Checking.
%
into_mpred_form_ilc([F|Fist],O):- is_list([F|Fist]),!,G  univ_safe  [t|[F|Fist]], into_mpred_form(G,O).
into_mpred_form_ilc(G,O):- safe_functor(G,F,A),G  univ_safe  [F,P|ARGS],!,into_mpred_form6(G,F,P,A,ARGS,O),!.

% TODO confirm negations

:- expire_tabled_list(all).


%% into_mpred_form6( ?X, ?H, ?P, ?N, ?A, ?O) is semidet.
%
% Converted To Managed Predicate Form6.
%
into_mpred_form6(C,_,_,2,_,C):-!.
% into_mpred_form6(H,_,_,_,_,G0):- once(locally(t_l:into_form_code,(expand_term( (H :- true) , C ), reduce_clause(assert,C,G)))),expanded_different(H,G),!,into_mpred_form(G,G0),!.
into_mpred_form6(_,F,_,1,[C],O):-alt_calls(F),!,into_mpred_form(C,O),!.
into_mpred_form6(_,':-',C,1,_,':-'(O)):-!,into_mpred_form_ilc(C,O).
into_mpred_form6(_,not,C,1,_,not(O)):-into_mpred_form(C,O),!.
into_mpred_form6(C,isa,_,2,_,C):-!.
into_mpred_form6(C,_,_,_,_,isa(I,T)):-was_isa_ex(C,I,T),!.
into_mpred_form6(_X,t,P,_N,A,O):-!,(atom(P)->O  univ_safe  [P|A];O  univ_safe  [t,P|A]).
into_mpred_form6(G,_,_,1,_,G):-predicate_property(G,number_of_rules(N)),N >0, !.
into_mpred_form6(G,F,C,1,_,O):-real_builtin_predicate(G),!,into_mpred_form(C,OO),O  univ_safe  [F,OO].
into_mpred_form6(_X,H,P,_N,A,O):-a(is_holds_false,H),(atom(P)->(G  univ_safe  [P|A],O=not(G));O  univ_safe  [holds_f,P|A]).
into_mpred_form6(_X,H,P,_N,A,O):-a(is_holds_true,H),(atom(P)->O  univ_safe  [P|A];O  univ_safe  [t,P|A]).
into_mpred_form6(G,F,_,_,_,G):-a(prologHybrid,F),!.
into_mpred_form6(G,F,_,_,_,G):-a(prologDynamic,F),!.
into_mpred_form6(G,F,_,_,_,G):-nop(dmsg_pretty(warn(unknown_mpred_type(F,G)))).

% ========================================
% acceptable_xform/2 (when the form is a isa/2, do a validity check)
% ========================================

%= 	 	 

%% acceptable_xform( ?From, ?To) is semidet.
%
% Acceptable Xform.
%
acceptable_xform(From,To):- From \=@= To,  (To = isa(I,C) -> was_isa_ex(From,I,C); true).

% ========================================
% transform_holds(Functor,In,Out)
% ========================================

%= 	 	 

%% transform_holds( ?H, ?In, ?Out) is semidet.
%
% Transform Holds.
%
transform_holds(H,In,Out):- once(transform_holds_3(H,In,Out)),!,ignore((In\=Out,fail,dmsg_pretty(transform_holds(H,In,Out)))).


% foreach_arg/7 
%  is a maping predicate

%= 	 	 

%% foreach_arg( :TermARGS, ?N, ?ArgIn, ?ArgN, ?ArgOut, ?Call, :TermARGS) is semidet.
%
% Foreach Argument.
%
foreach_arg(ARGS,_N,_ArgIn,_ArgN,_ArgOut,_Call,ARGS):- (not_ftCompound(ARGS)),!.
foreach_arg([ArgIn1|ARGS],ArgN1,ArgIn,ArgN,ArgOut,Call1,[ArgOut1|ARGSO]):-
     copy_term( a(ArgIn1,ArgOut1,ArgN1,Call1), a(ArgIn,ArgOut,ArgN,Call) ),
      call(Call),
      ArgN2 is ArgN + 1,
      foreach_arg(ARGS,ArgN2,ArgIn,ArgN,ArgOut,Call,ARGSO).


%= 	 	 

%% transform_functor_holds( +Op, ?F, ?ArgInOut, ?N, ?ArgInOut) is semidet.
%
% Transform Functor Holds.
%
transform_functor_holds(_,F,ArgInOut,N,ArgInOut):- once(call_u(argQuotedIsa(F,N,FT))),FT=ftTerm,!.
transform_functor_holds(Op,_,ArgIn,_,ArgOut):- transform_holds(Op,ArgIn,ArgOut),!.


%= 	 	 

%% transform_holds_3( +Op, :TermA, ?A) is semidet.
%
% Transform Holds Helper Number 3..
%
transform_holds_3(_,A,A):- (not_ftCompound(A)),!.
transform_holds_3(_,props(Obj,Props),props(Obj,Props)):-!.
%transform_holds_3(Op,Sent,OUT):-Sent  univ_safe  [And|C12],is_sentence_functor(And),!,maplist(transform_holds_3(Op),C12,O12),OUT  univ_safe  [And|O12].
transform_holds_3(_,A,A):-compound(A),safe_functor(A,F,N), predicate_property(A,_),arity_no_bc(F,N),!.
transform_holds_3(HFDS,M:Term,M:OUT):-atom(M),!,transform_holds_3(HFDS,Term,OUT).
transform_holds_3(HFDS,[P,A|ARGS],DBASE):- is_ftVar(P),!,DBASE  univ_safe  [HFDS,P,A|ARGS].
transform_holds_3(HFDS, ['[|]'|ARGS],DBASE):- trace_or_throw_ex(list_transform_holds_3(HFDS,['[|]'|ARGS],DBASE)).
transform_holds_3(Op,[SVOFunctor,Obj,Prop|ARGS],OUT):- if_defined(is_svo_functor(SVOFunctor)),!,transform_holds_3(Op,[Prop,Obj|ARGS],OUT).
transform_holds_3(Op,[P|ARGS],[P|ARGS]):- not(atom(P)),!,dmsg_pretty(transform_holds_3),trace_or_throw_ex(transform_holds_3(Op,[P|ARGS],[P|ARGS])).
transform_holds_3(HFDS,[HOFDS,P,A|ARGS],OUT):- a(is_holds_true,HOFDS),!,transform_holds_3(HFDS,[P,A|ARGS],OUT).
transform_holds_3(HFDS,[HOFDS,P,A|ARGS],OUT):- HFDS==HOFDS, !, transform_holds_3(HFDS,[P,A|ARGS],OUT).
transform_holds_3(_,HOFDS,isa(I,C)) :- was_isa_ex(HOFDS,I,C),!.
transform_holds_3(_,[Type,Inst],isa(Inst,Type)):-is_ftNonvar(Type),a(tCol,Type),!.
transform_holds_3(_,HOFDS,isa(I,C)):- holds_args(HOFDS,[ISA,I,C]),ISA==isa,!.

transform_holds_3(Op,[Fogical|ARGS],OUT):-  
         call(call,is_sentence_functor(Fogical)),!,sanity( \+ (a(is_svo_functor,Fogical))),
         must_det(foreach_arg(ARGS,1,ArgIn,ArgN,ArgOut,transform_functor_holds(Op,Fogical,ArgIn,ArgN,ArgOut),FARGS)),
         OUT  univ_safe  [Fogical|FARGS].

transform_holds_3(_,[props,Obj,Props],props(Obj,Props)).
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[isa(Type)|PROPS])):- 
                  is_ftNonvar(Inst), not(Type=props), (cheaply_u(tCol(Type));a(functorDeclares,Type)), 
                  must_det(\+(if_defined(is_never_type(Type)))),!.

transform_holds_3(_,[P,A|ARGS],DBASE):- atom(P),!,DBASE  univ_safe  [P,A|ARGS].
transform_holds_3(Op,[P,A|ARGS],DBASE):- !, is_ftNonvar(P),dumpST,trace_or_throw_ex(transform_holds_3(Op,[P,A|ARGS],DBASE)), DBASE  univ_safe  [P,A|ARGS].
transform_holds_3(Op,DBASE_T,OUT):- DBASE_T  univ_safe  [P,A|ARGS],!,transform_holds_3(Op,[P,A|ARGS],OUT).



%= 	 	 

%% holds_args( ?HOFDS, ?FIST) is semidet.
%
% Holds Arguments.
%
holds_args([H|FIST],FISTO):- !, a(is_holds_true,H),!,FIST=FISTO.
holds_args(HOFDS,FIST):- is_ftCompound(HOFDS),HOFDS  univ_safe  [H|FIST],a(is_holds_true,H),!.


%% do_expand_args( ?Op, ?Term, ?Term) is semidet.
%
% Do Expand Arguments.
%
do_expand_args(_,Term,TermO):- \+ compound(Term),!,must(Term=TermO).
do_expand_args(Exp,M:Sent,M:SentO):- atom(M),!,do_expand_args(Exp,Sent,SentO).
do_expand_args(_,Term,Term):- safe_functor(Term,F,_),cheaply_u(rtArgsVerbatum(F)),!.
do_expand_args(Exp,[L|IST],Out):- !,must(do_expand_args_l(Exp,[L|IST],Out)).
do_expand_args(Exp,Term,Out):- Term  univ_safe  [P|ARGS],do_expand_args_pa(Exp,P,ARGS,Out).


%= 	 	 

%% do_expand_args_pa( ?Exp, ?P, ?ARGS, ?Out) is semidet.
%
% Do Expand Arguments Pa.
%

% allows ?- fully_expand(arity(isEach([X,TY,OO]),4),O).
do_expand_args_pa(Exp,Exp,[ARGS|Some],Out):- (Some==[]),is_list(ARGS),!,member(Out,ARGS).
% allows ?- fully_expand(arity(isEach(X,TY,OO),4),O).
do_expand_args_pa(Exp,Exp,ARGS,Out):- !,member(Out,ARGS).
do_expand_args_pa(Exp,P,ARGS,Out):- do_expand_args_l(Exp,ARGS,EARGS), Out  univ_safe  [P|EARGS].


%= 	 	 

%% do_expand_args_l( +Op, :TermA, :TermA) is semidet.
%
% Do Expand Arguments (list Version).
%

% do_expand_args_l(Exp,ARGS,EARGS):- do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).

do_expand_args_l(_,A,A):- is_ftVar(A),!.
do_expand_args_l(Exp,[A|RGS],[E|ARGS]):- is_list(RGS),!,do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).
do_expand_args_l(_,A,A).



% :- mpred_trace_nochilds(functor_safe/2).
% :- mpred_trace_nochilds(functor_safe/3).


% ================================================
%  expand_goal_correct_argIsa/2
% ================================================

%= 	 	 

%% expands_on( ?EachOf, ?Term) is semidet.
%
% Expands Whenever.
%
%expands_on(EachOf,Term):-subst(Term,EachOf,foooz,Term2),!,Term2\=Term, \+ ((do_expand_args(EachOf,Term,O),O = Term)).

%= 	 	 

%% if_expands_on( ?EachOf, ?Term, ?Call) is semidet.
%
% If Expands Whenever.
%
%if_expands_on(EachOf,Term,Call):- expands_on(EachOf,Term),subst(Call,Term,O,OCall),!, forall(do_expand_args(EachOf,Term,O),OCall).

/*
%db_reop(WhatNot,Call) :- into_mpred_form(Call,NewCall),NewCall\=@=Call,!,db_reop(WhatNot,NewCall).
db_reop(Op,Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),db_reop_l(Op,O)).
db_reop(Op,Term):-db_reop_l(Op,Term).

db_reop_l(query(_HLDS,Must),Call) :- !,preq(Must,Call).
db_reop_l(Op,DATA):-no_loop_check(db_op0(Op,DATA)).

 dm sg_hook(transform_holds(t,_What,props(ttSpatialType,[isa(isa),isa]))):-trace_or_throw_ex(dtrace).

*/


% expand_goal_correct_argIsa(A,A):-simple_code,!.

%= 	 	 

%% expand_goal_correct_argIsa( ?A, ?B) is semidet.
%
% expand goal correct Argument  (isa/2).
%
expand_goal_correct_argIsa(A,B):- expand_goal(A,B).

% db_op_simpler(query(HLDS,_),MODULE:C0,call_u(call,MODULE:C0)):- atom(MODULE), is_ftNonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor_catch(C0,F,A), dmsg_pretty(todo(unmodulize(F/A))), %trace_or_throw_ex(module_form(MODULE:C0)), %   db_op(Op,C0).

%= 	 	 

%% db_op_simpler( +Op, ?VALUE2, :TermARG3) is semidet.
%
% Database Oper. Simpler.
%
db_op_simpler(Op,Sent,SentO):- call_last_is_var(db_op_simpler(Op,Sent,SentO)).

db_op_simpler(_,TypeTerm,props(Inst,[isa(Type)|PROPS])):- TypeTerm  univ_safe  [Type,Inst|PROPS],is_ftNonvar(Inst),a(functorDeclares,Type),!.



%= 	 	 

%% db_op_sentence( ?Op, ?Prop, ?ARGS, ?C0) is semidet.
%
% Database Oper. Sentence.
%
db_op_sentence(_Op,Prop,ARGS,C0):- atom(Prop),!, C0  univ_safe  [Prop|ARGS].
db_op_sentence(_Op,Prop,ARGS,C0):- C0  univ_safe  [t,Prop|ARGS].


%=  :- was_export(simply_functors/3).

%= 	 	 

%% simply_functors( :PRED2Db_pred, ?Op, ?Wild) is semidet.
%
% Simply Functors.
%
simply_functors(Db_pred,query(HLDS,Must),Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,query(HLDS,Must),Simpler).
simply_functors(Db_pred,Op,Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,Op,Simpler).


% -  dmsg_hook(db_op(query(HLDS,call),holds_t(ft_info,tCol,'$VAR'(_)))):-trace_or_throw_ex(dtrace).

lin_visits(P,Visits):-attvar(P),get_attr(P,linv,num(Visits)),!.
lin_visits(_,0).

set_lin_visits(P,Visits):-attvar(P),get_attr(P,linv,NumVisits),!,setarg(1,NumVisits,Visits).
set_lin_visits(P,Visits):-put_attr(P,linv,num(Visits)).

linearize_headvar_dupes(In,Out,How):- 
  term_variables(In,Vs),
  linearize_headvar_dupes((=),In,Out,How),
  maplist(del_attr_rl(linv),Vs).

del_attr_rl(Attr,Vs):- del_attr(Vs,Attr).

linearize_headvar_dupes(Equ,In,Out,How):- linearize_headvar_dupes(Equ,In,Out,true,How).
linearize_headvar_dupes(_Equ,P,PO,Left,Connector):- 
  (var(P),lin_visits(P,Visits)->Visits==0),!,
  set_lin_visits(P,1),PO=P,Left=Connector.
linearize_headvar_dupes(Equ,P,PO,Left,Connector):- var(P),!,PO=_,conjoin(Left,call(Equ,P,PO),Connector),!.
linearize_headvar_dupes(_Equ,P,PO,Left,Connector):- \+ compound(P),PO=P,Connector=Left,!.
linearize_headvar_dupes(Equ,[P1|M],[PO1|PL2],Left,Connector):-!, 
  linearize_headvar_dupes(Equ,P1,PO1,Left,MID),
  linearize_headvar_dupes(Equ,M,PL2,MID,Connector).
linearize_headvar_dupes(Equ,P,PO,Left,Connector):-P  univ_safe  [F|M],
 linearize_headvar_dupes(Equ,M,POL,Left,Connector),PO  univ_safe  [F|POL].


fixed_syntax(I,O):- notrace((compound(I), with_vars_locked(I,fix_syntax(I,O))))->I\=@=O.

fix_syntax(P0,P0):- \+ compound(P0),!.
fix_syntax(I,O):-sub_compound_of(I,~(P/Cond)), !,O= preventedWhen(P,{Cond}).
fix_syntax(I,O):- sub_compound_of(I, (~P/Cond)), !,fix_syntax(~(P/Cond),O).
fix_syntax(~I,O):- compound(I),linearize_headvar_dupes(I,M,Cond)->Cond\==true,!,O= preventedWhen(M,{Cond}).
%fix_syntax(~I,O):- compound(I),linearize_headvar_dupes(I,M,Cond),!,O= preventedWhen(M,{Cond}).
fix_syntax(I,O):- fixed_negations(I,M),fix_syntax(M,O).
%fix_syntax(~P/Cond,O):-  !,O=(((P/Cond)==> ~P)).
%fix_syntax((~P)/Cond,O):- !,O=((~P <- {Cond} )).
%fix_syntax((~P)/Cond,O):- !,O=(((P/Cond)==> ~P)).
%fix_syntax((~P)/Cond,O):- !,O=(((P/Cond)==> ~P)).
%fix_syntax((~P)/Cond,O):- !,O=(((P/ (\+Cond)) ==> \+ ~P)).
%fix_syntax(P/Cond,O):- mpred_literal_nonvar(P),!,O=((P <- { Cond } )).
fix_syntax(P/Cond,O):- !,O=((P <- {Cond} )).
fix_syntax(I, O):- sub_compound_of(I,((P/Cond):-B)),!,O=(P :- (B, Cond)).
fix_syntax(P:-B,PP:-B):-!, fix_syntax(P,PP).
% fix_syntax(I,O):- compound(I),linearize_headvar_dupes(I,PL,Cond)->Cond\==true,!,O= enabledWhen(PL,{Cond}).
fix_syntax(P,P).

sub_compound_of(I,Of):- compound(I),compound(Of),compound_name_arity(I,IN,IA),compound_name_arity(Of,ON,OA),
   (IA\==OA ; IN\==ON),!,fail.  
sub_compound_of(I,Of):- \+ \+ (numbervars(I,99,_,[attvar(bind)]),I=Of ), I = Of.

fixed_negations(I,O):- notrace(( compound(I), with_some_vars_locked(I,fix_negations(I,O))))->I\=@=O.

fix_negations(P0,P0):- not_ftCompound(P0),!.
fix_negations(~(P0),~(P0)):- not_ftCompound(P0),!.
fix_negations(\+(P0),\+(P0)):- not_ftCompound(P0),!.
fix_negations(\+ \+ (P0), (P0)):- not_ftCompound(P0),!.

fix_negations(\+ \+ (I), (O)):- !, fix_negations((I), (O)).

fix_negations(P==>Q, PP==>QQ):-!,
  fix_negations(P,PP),
  fix_negations(Q,QQ),!.

fix_negations(~(~I),O):- !, fix_negations(\+(~I),O).
fix_negations(~not(I),O):- !, fix_negations(\+(~I),O).
fix_negations(~~(I),O):- safe_functor(~~(I),~~,1),!, fix_negations(\+(~I),O).
fix_negations(not(I),O):- !, fix_negations(\+(I),O).
fix_negations(~(I),~(O)):- !, fix_negations(I,O).
fix_negations(\+(I),\+(O)):- !, fix_negations(I,O).
fix_negations(C,C):- if_defined(exact_args(C),fail),!.
fix_negations([H|T],[HH|TT]):-!,fix_negations(H,HH),fix_negations(T,TT),!.
fix_negations(C,CO):-C  univ_safe  [F|CL],must_maplist(fix_negations,CL,CLO),!,CO  univ_safe  [F|CLO].


%% reduce_clause_from_fwd(Op, +H, ?H) is semidet.
%
% Reduce Clause Converted From Forward Repropigated.
%
reduce_clause_from_fwd(_Op,H,H):- quietly(\+is_ftCompound(H)),!.
reduce_clause_from_fwd(Op,(H:-B),HH):-B==true,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,(B==>H),HH):-B==true,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,I,O):- quietly(fixed_negations(I,M)),!,reduce_clause_from_fwd(Op,M,O).
reduce_clause_from_fwd(Op,(==>H),HH):-!,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,(H<- B),HH):-B==true,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,(B<==> H),HH):-B==true,reduce_clause_from_fwd(Op,'==>'(H),HH).
reduce_clause_from_fwd(Op,(H<==> B),HH):-B==true,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,(H,B),(HH,BB)):-!,reduce_clause_from_fwd(Op,H,HH),reduce_clause_from_fwd(Op,B,BB).
reduce_clause_from_fwd(_Op,H,H).
        


%% append_as_first_arg( +C, ?I, ?V) is semidet.
%
% Append Converted To First Argument.
%
append_as_first_arg(C,I,V):-C  univ_safe  [F|ARGS],V  univ_safe  [F,I|ARGS].



%% to_predicate_isas( :TermV, :TermV) is semidet.
%
% Converted To Predicate Isas.
%
to_predicate_isas(V,V):- (\+is_ftCompound(V)),!.
to_predicate_isas({V},{V}):-!.
% to_predicate_isas(eXact(V),V):-!.
to_predicate_isas([H|T],[HH|TT]):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
to_predicate_isas((H,T),(HH,TT)):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
%to_predicate_isas(I,I):-contains_term(S,I),is_ftNonvar(S),exact_args(S),!.
to_predicate_isas(t(C,I),V):-atom(C)->V  univ_safe  [C,I];(is_ftVar(C)->V=t(C,I);append_as_first_arg(C,I,V)).
to_predicate_isas(isa(I,C),V):-!,(atom(C)->V  univ_safe  [C,I];(is_ftVar(C)->V=isa(I,C);append_as_first_arg(C,I,V))).
to_predicate_isas(C,C):- exact_args(C),!.
to_predicate_isas(C,CO):-C  univ_safe  [F|CL],must_maplist(to_predicate_isas,CL,CLO),!,CO  univ_safe  [F|CLO].


%% exact_args( +Q) is semidet.
%
% Exact Arguments.
%
exact_args(Q):- is_ftVar(Q),!,fail.
exact_args(Q):- \+ compound(Q), !.
exact_args(isEach):-!,fail.
%exact_args(_:Q):- !,(exact_args0(Q),fail).
exact_args(_:Q):- !,(exact_args0(Q)).
exact_args(Q):- exact_args0(Q),!.



exact_args0(Q):- \+ compound(Q), !.
exact_args0((A/B)):- (is_ftVar(A);(number(B);is_ftVar(B))),!.
exact_args0(==>(_,_)):-!,fail.
% exact_args0(Q):- Q  univ_safe  [_,A],atomic(A),!.
exact_args0(Q):- compound_name_arity(Q,F,A),A>0,!,exact_args_f(F),!.

exact_args_f(-->).
exact_args_f(if_defined).
exact_args_f(txtConcatFn).
exact_args_f(dif).

exact_args_f(maplist).
exact_args_f(action_info).
exact_args_f(never_retract_u).
exact_args_f(install_converter).
exact_args_f(installed_converter).
exact_args_f(actn).
exact_args_f(wid).
exact_args_f(wdmsg_pfc).
exact_args_f(fol_to_pkif).
exact_args_f(ftListFn).
exact_args_f(vtActionTemplate).
exact_args_f(txtConcatFn).
exact_args_f('$spft').
exact_args_f(skip_expand_fa).
exact_args_f(sformat).
exact_args_f(second_order).
exact_args_f(retract_eq_quitely).
exact_args_f(not_undoable).
exact_args_f(mtExact).
exact_args_f(vQuotientFn).
exact_args_f(uSubLQuoteFn).
exact_args_f(mpred_prop).
exact_args_f(mpred_ain).
exact_args_f(meta_argtypes_guessed).
exact_args_f(meta_argtypes).
exact_args_f(ignore).
exact_args_f(format).
exact_args_f(dynamic).
exact_args_f(dmsg_pretty).
exact_args_f(call_u).
exact_args_f(say).
exact_args_f(call).
exact_args_f(assertz_if_new).
exact_args_f(asserts_eq_quitely).
exact_args_f(asserted).
exact_args_f(rtArgsVerbatum).
exact_args_f((  univ_safe  )).
exact_args_f((=)).
exact_args_f('$was_imported_kb_content$'):-!. %dtrace.
exact_args_f(F):-clause_b(rtArgsVerbatum(F)),!.
exact_args_f(F):-cheaply_u(prologBuiltin(F)),!.

:- source_location(F,_),asserta(absolute_source_location_pfc(F)).
% exact_args((_:-_)).
% exact_args((:-( _))).
% exact_args(C):-source_file(C,I),absolute_source_location_pfc(I).


:- module_transparent(is_stripped_module/1).


%= 	 	 

%% db_quf_l( ?Op, ?And, ?C12, ?Pre2, ?Templ2) is semidet.
%
% Database Quf (list Version).
%
db_quf_l(Op,And,[C],D2,D4):- !, db_quf(Op,C,D2,D3),!,D4  univ_safe  [And,D3].
db_quf_l(Op,And,C12,Pre2,Templ2):-db_quf_l_0(Op,And,C12,Pre2,Templ2).


%= 	 	 

%% db_quf_l_0( ?Op, ?And, :TermC, ?D2, ?D3) is semidet.
%
% Database quf (List version)  Primary Helper.
%
db_quf_l_0(Op,_And,[C],D2,D3):- db_quf(Op,C,D2,D3),!.
db_quf_l_0(Op, And,[C|C12],PreO,TemplO):-
  db_quf(Op,C,Pre,Next),
  db_quf_l_0(Op,And,C12,Pre2,Templ2),
  conjoin_l(Pre,Pre2,PreO),
  conjoin_op(And,Next,Templ2,TemplO).

%=  :- was_export(db_quf/4).

%= 	 	 

%% db_quf( +Op, ?C, ?Pretest, ?Template) is semidet.
%
% Database Quf.
%

db_quf(Op,C,Template):- db_quf(Op,C,true,Template),!.

db_quf(_ ,C,Pretest,Template):- (not_ftCompound(C)),!,must(Pretest=true),must(Template=C).
db_quf(Op,C,Pretest,Template):-is_ftVar(C),!,trace_or_throw_ex(var_db_quf(Op,C,Pretest,Template)).
db_quf(_ ,C,Pretest,Template):-as_is_term(C),!,must(Pretest=true),must(Template=C),!.

db_quf(Op,M:C,Pretest,M:Template):-atom(M),!,must(db_quf(Op,C,Pretest,Template)).

db_quf(Op,C,Pretest,Template):- C  univ_safe  [Holds,OBJ|ARGS],a(is_holds_true,Holds),atom(OBJ),!,C1  univ_safe  [OBJ|ARGS],must(db_quf(Op,C1,Pretest,Template)).
db_quf(_Op,C,true,C):- C  univ_safe  [Holds,OBJ|_],a(is_holds_true,Holds),is_ftVar(OBJ),!.
db_quf(Op,Sent,D2,D3):- Sent  univ_safe  [And|C12],C12=[_|_],is_sentence_functor(And),!, db_quf_l(Op,And,C12,D2,D3).
db_quf(Op,C,Pretest,Template):- C  univ_safe  [Prop,OBJ|ARGS],
      safe_functor(C,Prop,A),
      show_failure(why,translate_args(Op,Prop,A,OBJ,2,ARGS,NEWARGS,true,Pretest)),
      Template   univ_safe   [Prop,OBJ|NEWARGS],!.
db_quf(_Op,C,true,C).


%= 	 	 

%% translate_args( ?O, ?Prop, ?A, ?OBJ, ?N, :TermARG6, :TermARG7, ?GIN, ?GIN) is semidet.
%
% Translate Arguments.
%
translate_args(_O,_Prop,_A,_OBJ,_N,[],[],GIN,GIN).
translate_args(Op,Prop,A,OBJ,N1,[ARG|S],[NEW|ARGS],GIN,GOALS):-
   Type = argIsaFn(Prop,N1),
   translateOneArg(Op,Prop,OBJ,Type,ARG,NEW,GIN,GMID),
   N2 is N1 +1,
   translate_args(Op,Prop,A,OBJ,N2,S,ARGS,GMID,GOALS).


% ftVar

%= 	 	 

%% translateOneArg( ?Op, ?Prop, ?Obj, ?Type, ?VAR, ?VAR, ?G, ?G) is semidet.
%
% Translate One Argument.
%
translateOneArg(_Op,_Prop,_Obj,_Type,VAR,VAR,G,G):-is_ftVar(VAR),!.

% not an expression
translateOneArg(_O,_Prop,_Obj,_Type,ATOMIC,ATOMIC,G,G):-atomic(ATOMIC),!.
% translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,same_arg(tCol(Type),ATOMIC,ATOMICUSE))):-atomic(ATOMIC),!.

% translateOneArg(_O,_Prop,_Obj,Type,VAR,VAR,G,G):-ignore(isa(VAR,Type)),!.

% props(Obj,size < 2).
translateOneArg(_O,Prop,Obj,Type,ARG,OLD,G,(GETTER,COMPARE,G)):-
       safe_functor(ARG,F,2), comparitiveOp(F),!,
       ARG  univ_safe  [F,Prop,VAL],
       GETTER  univ_safe  [Prop,Obj,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,isOneOf(Sz,[size+1,2])).
translateOneArg(Op,Prop,O,Type,isOneOf(VAL,LIST),VAL,G,(G0,G)):-
   translateListOps(Op,Prop,O,Type,VAL,LIST,G,G0).

% db_op(Op, Obj,size + 2).
translateOneArg(_O,Prop,Obj,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       ground(ARG),
       safe_functor(ARG,F,2), additiveOp(F),!,
       ARG  univ_safe  [F,Prop,VAL],
       GETTER  univ_safe  [Prop,Obj,OLD],
       STORE= update_value(OLD,VAL,NEW),!.

translateOneArg(_O,_Prop,_Obj,_Type,NART,NART,G,G):-!. % <- makes us skip the next bit of code
translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,ignore(same_arg(tCol(Type),ATOMIC,ATOMICUSE)))).


%= 	 	 

%% translateListOps( ?O, ?Prop, ?Obj, ?Type, ?VAL, :TermARG6, ?G, ?G) is semidet.
%
% Translate List Oper.s.
%
translateListOps(_O,_Prop,_Obj,_Type,_VAL,[],G,G).
translateListOps(Op,Prop,Obj,Type,VAL,[L|LIST],G,GO2):-
   translateOneArg(Op,Prop,Obj,Type,L,VAL,G,G0),
   translateListOps(Op,Prop,Obj,Type,VAL,LIST,G0,GO2).


%= 	 	 

%% compare_op( ?Type, :PRED2F, ?OLD, ?VAL) is semidet.
%
% Compare Oper..
%
compare_op(Type,F,OLD,VAL):-nop(Type),show_call(why,(call(F,OLD,VAL))),!.


% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.

% ========================================
% expanded_different compares fact terms to see if they are different
% ========================================

:- '$hide'(expanded_different/2).
%=  :- was_export(expanded_different/2).


%= 	 	 

%% expanded_different( ?G0, ?G1) is semidet.
%
% Expanded Different.
%
expanded_different(G0,G1):-call(expanded_different_ic(G0,G1)).


%= 	 	 

%% expanded_different_ic( ?G0, ?G1) is semidet.
%
% Expanded Different Ic.
%
expanded_different_ic(G0,G1):-G0==G1,!,fail.
expanded_different_ic(G0,G1):-expanded_different_1(G0,G1),!.
expanded_different_ic(G0,G1):- G0\==G1.


%= 	 	 

%% expanded_different_1( ?G0, :TermG1) is semidet.
%
% expanded different  Secondary Helper.
%
expanded_different_1(NV:G0,G1):-is_ftNonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,NV:G1):-is_ftNonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,G1):- (is_ftVar(G0);is_ftVar(G1)),!,trace_or_throw_ex(expanded_different(G0,G1)).
expanded_different_1(G0,G1):- G0 \= G1,!.


% ========================================
% into_functor_form/3 (adds a second order safe_functor onto most predicates)
% ========================================
%=  :- was_export(into_functor_form/3).

%= 	 	 

%% into_functor_form( ?HFDS, ?X, ?O) is semidet.
%
% Converted To Functor Form.
%
into_functor_form(DBase_t,P,PO):- into_functor_form(call,=,DBase_t,P,PO).

into_functor_form(call,V,T,M:P,M:PO):- atom(M),! ,into_functor_form(call,V,T,P,PO),!.
into_functor_form(call,V,T,P,PO):- var(P),O univ_safe [T,P], call(V,O,PO).
into_functor_form(_,_,T,P,PO):- var(P),PO univ_safe [T,P].
into_functor_form(call,V,T,P,PO):- is_list(P),O univ_safe [T|P], call(V,O,PO).
into_functor_form(C,V,T,P,PO):- is_list(P),maplist(into_functor_form(C,V,T),P,PO).

into_functor_form(call,V,_,P,PO):- atomic(P),callable(P),O=P,call(V,O,PO).
into_functor_form(_,_,_,P,PO):- \+ callable(P),P=PO.
into_functor_form(C,V,T,P,PO):-must((( P  univ_safe [F|Args],into_functor_form(C,V,T,P,F,Args,PO)))),!.

% TODO finish negations

%= 	 	 

%% into_functor_form( ?T, ?X, ?T, ?A, ?X) is semidet.
%
% Converted To Functor Form.
%
into_functor_form(_,_,T,P,F,_Args,P):- F==T,!.
into_functor_form(_,V,T,_,F,Args,PO):- isBodyConnective(F),
  maplist(into_functor_form(call,V,T),Args,OArgs), PO univ_safe [F|OArgs].
into_functor_form(_,V,T,_,F,Args,PO):-is_holds_true0(F), O univ_safe  [T|Args], call(V,O,PO).
% into_functor_form(T,_P,T,Args,Call):- a(is_holds_true,T), Call  univ_safe  [T|Args].
into_functor_form(_,V,T,_,F,Args,PO):- O univ_safe [T,F|Args], call(V,O,PO).


% these do not get defined!?%= :- kb_shared user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.


%:- fixup_exports.

:- export(mpred_expansion_file/0).
mpred_expansion_file.

