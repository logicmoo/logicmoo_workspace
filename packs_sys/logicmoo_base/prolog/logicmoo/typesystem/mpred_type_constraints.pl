/*  
% ===================================================================
% File 'mpred_type_constraints.pl'
% Purpose: For Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
%
% Dec 13, 2035
% Douglas Miles
*/

% File: /opt/PrologMUD/pack/logicmmtc_base/prolog/logicmoo/mpred/mpred_type_constraints.pl
%:- if(( ( \+ ((current_prolog_flag(logicmmtc_include,Call),Call))) )).
:- module(mpred_type_constraints,
          [ add_cond/2,           
            arg_to_var/3,
            attempt_attribute_args/3,
            attempt_attribute_args/5,
            attempt_attribute_one_arg/4,
            attribs_to_atoms/2,
            attribs_to_atoms0/2,
            cmp_memberchk_0/2,
            cmp_memberchk_00/2,
            comp_type/3,
            iz/2,
            extend_iz/2,
            extend_iz_member/2,
            init_iz/2,
            inst_cond/2,
            isa_pred_l/3,
            isa_pred_l/4,
            chk_cond/2,
            call_cond/2,
            condz_to_isa/2,
            map_subterms/3,
            max_cond/3,
            max_cond_l/2,
            dif_objs/2,
            min_cond/3,
            min_cond_l/2,
            promp_yn/2,
            same/2,
            same_arg/3,
            samef/2,
            to_functor/2,
            type_size/2,
            extract_conditions/2,
            
            unrelax/1, iz_member/1,

            lazy/1,lazy/2,

            constrain/1,enforce/1,
            

            relax/1,relax_goal/2,thaw/1,
            mpred_type_constraints_file/0
          ]).

%:- set_prolog_flag(generate_debug_info, true).
:- user:use_module(library(logicmoo_common)).
:- user:use_module(library(gvar_globals_api)).
% :- use_module(library(logicmoo/common_logic/common_logic_snark)).



:- meta_predicate my_when(+,0).
:- meta_predicate nrlc(0).
:- meta_predicate prolog_current_choice(1,*).
:- meta_predicate prolog_current_choice(1,*,*).
:- meta_predicate xnr(0).
:- meta_predicate xnr(*,0).
%:- include('mpred_header.pi').

%:- rtrace.
:- kb_shared(baseKB:admittedArgument/3).
:- kb_shared(baseKB:argIsa/3).
:- kb_shared(baseKB:genls/2).

:- kb_global(baseKB:nesc/1).
:- kb_global(baseKB:proven_tru/1).

:- export_everywhere(mpred_hooks,holds_t,2).

:- kb_global(mpred_hooks:holds_t/3).
:- kb_global(mpred_storage:equals_call/2).

:- kb_global(baseKB:call_e_tru/2).
:- kb_global(baseKB:is_fort/1).
:- kb_global(common_logic_snark:kif_option_value/2).
:- kb_global(baseKB:member_eqz/2).

:- op(300,fx,('~')).

% :- endif.

:- module_transparent((
            add_cond/2,           
            arg_to_var/3,
            attempt_attribute_args/3,
            attempt_attribute_args/5,
            attempt_attribute_one_arg/4,
            attribs_to_atoms/2,
            attribs_to_atoms0/2,
            cmp_memberchk_0/2,
            cmp_memberchk_00/2,
            comp_type/3,
            iz/2,
            extend_iz/2,
            extend_iz_member/2,
            init_iz/2,
            inst_cond/2,
            isa_pred_l/3,
            isa_pred_l/4,
            chk_cond/2,
            call_cond/2,
            condz_to_isa/2,
            map_subterms/3,
            max_cond/3,
            max_cond_l/2,
            dif_objs/2,
            min_cond/3,
            min_cond_l/2,
            promp_yn/2,
            same/2,
            same_arg/3,
            samef/2,
            to_functor/2,
            type_size/2,
            extract_conditions/2,
            
            unrelax/1, iz_member/1,

            lazy/1,lazy/2,

            constrain/1,enforce/1,

            relax/1,relax_goal/2,thaw/1,
            mpred_type_constraints_file/0)).

:- if(exists_source(library(multivar))).
%:- use_module(library(multivar)).
:- endif.

%:- rtrace.
:- if(exists_source(library(vhar))).
%:- use_module(library(vhar)).
:- endif.

:- if(exists_source(library(vprox))).
%:- use_module(library(vprox)).
:- endif.


:- meta_predicate 
   isa_pred_l(+,*,*),
   isa_pred_l(+,*,*,*),
   map_subterms(+,?,?),
   iz_member(*),
   constrain(*),
   map_lits(1,+),
   boxlog_goal_expansion(*,*),
   map_each_argnum(?,4,?,?,*),
   map_argnums(?,4,*),
   thaw(?),
   lazy(*),
   unrelax(*),
   relax_goal(*,+),
   lazy(+,*).

:- meta_predicate relax(*),relaxing(*).

:- kb_local(baseKB:admittedArgument/3).

:- thread_local(t_l:no_kif_var_coroutines/1).

:- meta_predicate relaxed_call(*).

% ?- G=(loves(X,Y),~knows(Y,tHuman(X))),relax_goal(G,Out),writeq(Out).

:- meta_predicate map_plits(1,*).
map_lits(P1,Lit):- 
 locally($('$outer_stack')=[],once(map_plits(P1,Lit))),!.

map_plits(P1,Lit):- \+ compound(Lit),!,call(P1,Lit).
map_plits(P1,[Lit1 |  Lit2]):- !,map_plits(P1,Lit1),map_plits(P1,Lit2).
map_plits(P1,(Lit1 ,  Lit2)):- !,map_plits(P1,Lit1),map_plits(P1,Lit2).
map_plits(P1,(Lit1 ;  Lit2)):- !,map_plits(P1,Lit1),map_plits(P1,Lit2).
map_plits(P1,(Lit1 :- Lit2)):- !,map_lits(P1,Lit1),with_outer(Lit1,0,map_plits(P1,Lit2)).
map_plits(P1, Expr) :- demodalfy_outermost(+,Expr,MExpr,_Outer),!,
   with_outer(Expr,1,map_plits(P1, MExpr)).
map_plits(P1, Expr) :- Expr=..[C,I], tCol(C),!,map_plits(P1, isa(I,C)).
map_plits(P1, Expr) :- functor(Expr,F,A),mappable_sentence_functor(F,A), !, Expr =.. [F|Args],
  map_meta_lit(F,1,P1,Args).
map_plits(P1,Lit):- call(P1,Lit).

map_meta_lit(F,N,P1,[Arg|Args]):- !,
  with_outer(F,N,map_plits(P1, Arg)),
  N2 is N + 1,
  map_meta_lit(F,N2,P1,Args).
map_meta_lit(_,_,_,[]):-!.

:- nb_setval('$outer_stack',[]).

with_outer(ExprF,N,Goal):- nb_current('$outer_stack',Was),
  locally($('$outer_stack')=[ExprF-N|Was],Goal).

closure_push(Closure,Data):- var(Closure),!,add_cond(Closure,Data).
closure_push(Closure,Data):- Closure=[true|_Rest],!,setarg(1,Closure,Data).
closure_push(Closure,Data):- Closure=[_First|Rest],!,setarg(2,Closure,[Data|Rest]).

constrain_arg_var(Closure,Arg,FA):- closure_push(Closure,add_cond(Arg,FA)).

%push_modal(neg(_)):- nb_current('$modal_stack',[neg(_)|Was]),!, b_setval('$modal_stack',Was).
%push_modal(Modal):- nb_current('$modal_stack',Was)->b_setval('$modal_stack',[Modal|Was]);b_setval('$modal_stack',[Modal,call]).
%last_modal(Modal):- nb_current('$modal_stack',[Modal|_])-> true; Modal=call.

map_argnums(_,_,Lit):- \+ compound(Lit), !.
map_argnums(Modal,P4,[Lit1 |  Lit2]):- !,map_argnums(Modal,P4,Lit1),map_argnums(Modal,P4,Lit2).
map_argnums(Modal,P4,isa(I,C)):- !,call(P4,Modal,C,0,I).
map_argnums(Modal,P4,Expr) :- demodalfy_outermost(Modal,Expr,MExpr,ModalValue),!,map_argnums(ModalValue,P4, MExpr).
map_argnums(Modal,P4,Expr) :- Expr=..[C,I], \+ (clause_b(argIsa(C,1,CC)),CC==C), clause_b(tCol(C)), !,map_argnums(Modal,P4,isa(I,C)).
map_argnums(Modal,P4,Expr) :- compound_name_arguments(Expr,F,Args),functor(Expr,F,A),
   (mappable_sentence_functor(F,A) -> map_argnums(Modal,P4,Args); map_each_argnum(Modal,P4,F,1,Args)).


map_each_argnum(Modal,P4,F,N,[Arg|Args]):- !,
   call(P4,Modal,F,N,Arg),
   N2 is N + 1,
   map_each_argnum(Modal,P4,F,N2,Args).
map_each_argnum(_Modal,_,_,_,_).


% non-backtracking attribute updates
 

demodalfy_outermost(ModalIn,MExpr, Expr, ModalValue):-  MExpr=..[Modal,Expr], modal_value(ModalIn,Modal,ModalValue).
modal_value(neg(_), Neg , true):- arg(_,v( ( \+ ),'~','-','not'),Neg).
modal_value(_, Neg , neg(Neg)):- arg(_,v( ( \+ ),'~','-','not'),Neg).

mappable_sentence_functor(call,1).
mappable_sentence_functor(=,2):-!,fail.
mappable_sentence_functor(call_u,1).
mappable_sentence_functor(F,_):- downcase_atom(F,DC),upcase_atom(F,DC).
%mappable_sentence_functor(F,1):- \+ tCol(F).
%mappable_sentence_functor(F,A):- \+ argIsa(F,A,_).

%mtc_put_iza(X,Z):- Z=[id(ID)|_],nonvar(ID),!,put_attr(X,iza,Z).
%mtc_put_iza(X,Z):- get_attr(X,iza,[id(ID)|_]),put_attr(X,iza,[id(ID)|Z]).
%mtc_put_iza(X,Z):- gensym(id_,ID),!,put_attr(X,iza,[id(ID)|Z]).


mtc_put_iza(X,Z):- put_attr(X,iza,Z).

mtc_put_attr(X,iza,Z):- mtc_get_attr(X,iza,_Prev),!, mtc_put_iza(X,Z).
mtc_put_attr(X,iza,Z):- !, mtc_put_iza(X,[iza_id(X)|Z]).
mtc_put_attr(X,Y,Z):- var(X),!,oo_put_attr(X,Y,Z).
mtc_put_attr(X,Y,Z):- oo_put_attr(X,Y,Z),nop(dmsg(warn(need_to_error(oo_put_attr(X,Y,Z))))).

mtc_get_attr(X,Y,Z):- var(X),!,oo_get_attr(X,Y,Z).
mtc_get_attr(X,Y,Z):- oo_get_attr(X,Y,Z),nop(dmsg(warn(need_to_fail(oo_get_attr(X,Y,Z))))),!,fail.


mtc_get_attvar(Dom1,X):-memberchk(iza_id(X),Dom1).

compound_lit(Arg):- compound(Arg).

% ========================================================================
% enforce_bound(G)  = check constraints
% ========================================================================
:- export(enforce_bound/1).
enforce_bound(G):-args_enforce_bound(G,Closure),maplist(call,Closure).

:- during_boot(add_history(( 
  G=(loves(X,Y),~(knows(Y,tHuman(X)))),must(args_enforce_bound(G,Out)),writeq(Out)
   ))).

:- export(args_enforce_bound/2).
args_enforce_bound(G,Closure):- ignore(Closure=[true]),map_argnums(pos(_),args_enforce_bound3(Closure),G).

args_enforce_bound3(Closure,Modal,C,0,I):- !, ignore(( nonvar(I),
   (Modal\=pos(_)  -> closure_push(Closure,modal_isa(I,C)) ; closure_push(Closure,isa(I,C))))).
args_enforce_bound3(Closure,Modal,_F,_A,Arg):- compound_lit(Arg),!,map_argnums(Modal,args_enforce_bound3(Closure),Arg).
args_enforce_bound3(_Closure,_Modal,_F,_A,Arg):- var(Arg),!.
args_enforce_bound3(Closure,Modal,F,A,Arg):-args_enforce_nonvar(Closure,Modal,F,A,Arg).


% ========================================================================
% constrain(G)  = add constraints to free args
% ========================================================================
:- export(constrain/1).
constrain(G):-ground(G),!.
constrain(G):-args_constrain(G,Closure),maplist(call,Closure).

:- export(args_constrain/2).
:- during_boot(add_history(( 
  G=(loves(X,Y),~knows(Y,tHuman(X))),must(args_constrain(G,Out)),writeq(Out)
   ))).

args_constrain(G,Closure):- ignore(Closure=[true]),map_argnums(pos(_),args_constrains3(Closure),G).


args_constrains3(Closure,Modal,C,0,I):- !,
   (Modal\=pos(_)  -> constrain_arg_var(Closure,I,does_exist(I)) ; constrain_arg_var(Closure,I,isa(I,C))).
args_constrains3(Closure,Modal,_F,_A,Arg):- compound_lit(Arg),!,map_argnums(Modal,args_constrains3(Closure),Arg).
args_constrains3(_Closure,_Modal,_F,_A,Arg):- nonvar(Arg),!.
args_constrains3(Closure,Modal,F,A,Arg):-args_constrain_var(Closure,Modal,F,A,Arg).
   
:- export(does_exist/1).
does_exist(_).
modal_admittedArgument(F,1,V):-!,admittedArgument(F,1,V).
modal_admittedArgument(_,_,_).
% ========================================================================
% enforce(G)  = enforce_bound/1 + constrain/1
% ========================================================================
:- export(enforce/1).
enforce(G):-args_enforce(G,Closure),maplist(call,Closure).


:- during_boot(add_history(( 
  G=(loves(X,Y),~knows(Y,tHuman(X))),must(args_enforce(G,Out)),writeq(Out)
   ))).

:- export(args_enforce/2).
args_enforce(G,Closure):- ignore(Closure=[true]),map_argnums(pos(_),args_enforces3(Closure),G).

args_enforces3(Closure,Modal,C,0,I):- !,
   (Modal\=pos(_)  -> constrain_arg_var(Closure,I,does_exist(I)) ; constrain_arg_var(Closure,I,isa(I,C))).
args_enforces3(Closure,Modal,_F,_A,Arg):- compound_lit(Arg),!,map_argnums(Modal,args_enforces3(Closure),Arg).
args_enforces3(Closure,Modal,F,A,Arg):- var(Arg),!, args_constrain_var(Closure,Modal,F,A,Arg).
args_enforces3(Closure,Modal,F,A,Arg):- args_enforce_nonvar(Closure,Modal,F,A,Arg).
 


% ========================================================================
% remove_constraints(G)  = remove constraints 
% ========================================================================
remove_constraints(G):-args_remove_constraints(G,Closures),maplist(ignore,Closures).

:- export(args_remove_constraints/2).
:- during_boot(add_history(( 
                            G=(loves(X,Y),~knows(Y,tHuman(X))),args_enforce(G,Out),writeq(Out),
                            args_remove_constraints(G,Out2),writeq(Out2)
  
   ))).

args_remove_constraints(G,Closure):- ignore(Closure=[true]),map_argnums(pos(_),args_remove_constraints3(Closure),G).

args_remove_constraints3(Closure,_Modal,C,0,I):- !, transfer_constraints(Closure,I),transfer_constraints(Closure,C).
args_remove_constraints3(Closure,Modal,_F,_A,Arg):- compound_lit(Arg),!,map_argnums(Modal,args_remove_constraints3(Closure),Arg).
args_remove_constraints3(Closure,_Modal,_F,_A,Arg):- transfer_constraints(Arg,Closure).

transfer_constraints(Arg,Closure):- ignore((var(Arg),mtc_get_attr(Arg,iza,ToDo),del_attr(Arg,iza),
   maplist(constrain_arg_var(Closure,Arg),ToDo))).

%:- module_transparent(apply:maplist/2).
%:- module_transparent(apply:maplist/3).


%% args_constrain_var(?Closure, +Modal, +F, +A, +Arg) is det.
%
% Datalog Preconditional Expansion.
%
args_constrain_var(Closure,Modal,F,A,Arg):- (A==1 ; Modal=pos(_)),
    argIsa(F,A,Type),!,constrain_arg_var(Closure,Arg,isa(Arg,Type)).

args_constrain_var(Closure,Modal,F,A,Arg):- 
   (Modal\=pos(_)  ->
       constrain_arg_var(Closure,Arg,modal_admittedArgument(F,A,Arg)) ;
       constrain_arg_var(Closure,Arg,    admittedArgument(F,A,Arg))).

%% args_enforce_nonvar(?Closure, +Modal, +F, +A, +Arg) is det.
%
% Datalog Preconditional Expansion.
%
args_enforce_nonvar(Closure,Modal,F,A,Arg):-
   (Modal\=pos(_)  ->
       closure_push(Closure,modal_admittedArgument(F,A,Arg)) ;
       closure_push(Closure,    admittedArgument(F,A,Arg))).


%% extract_conditions( +PFCSentence, -Conds) is semidet.
%
% Datalog Preconditional Expansion.
%
extract_conditions(Sentence,Conds):- 
 copy_term(Sentence,Sentence,Goals),
 list_to_set(Goals,GoalSet),
 (Goals\==GoalSet-> dmsg(cons_odd) ; true),
 list_to_conjuncts(GoalSet,Conds),!.

%% boxlog_goal_expansion( ?G, ?GG) is semidet.
%
% Datalog Goal Expansion.
%
boxlog_goal_expansion(relax(G),GG):-!,relax_goal(G,GG).
%boxlog_goal_expansion(G,GG):-!,relax_goal(G,GG).
/* 
boxlog_goal_expansion(G,_):- % \+ source_location(_,_),
  wdmsg(g_s(G)),fail.
*/


is_iz_or_iza(Var):- zotrace((mtc_get_attr(Var,iz,_);mtc_get_attr(Var,iza,_))).

%% relax( :GoalG) is det.
%
% Relaxen.
%
relax(G):- map_lits(relax_lit,G).

relaxing(G):- term_attvars(G,Gs),quietly(relax(G)),term_attvars(G,Gs0),!,Gs0\==Gs.

relax_lit(G):- var(G),!.
relax_lit(_:G):-!,relax_lit(G).
relax_lit(G):- G=..[_|ARGS],relax_args(G,1,ARGS).


%% relaxed_call( :GoalG) is nondet.
%
%
relaxed_call(G):- relax(G), (G *-> unrelax(G) ; (unrelax(G),!,fail)).


%% relax_goal( :GoalG ) is det.
%
% Relaxen Goal.
%

relax_goal(G,GG):- copy_term(G,GG),relax(GG).


relax_goal_alt_old(G,GGG):-
  copy_term(G,GG,Gs),G=GG,G=..[_|ARGS],relax_args(GG,1,ARGS),   
  GGG=(GG,maplist(iz_member,Gs)).


%  ?- G=loves(a,b),relax_lit(G).
  




%% relax_N( ?G, ?N, ?A) is semidet.
%
% Relaxen Argument.
%
% % relax_N(G,N,Val):- var(Val),!,setarg(N,G,Val).
% % relax_N(G,N,Val):- iz(AA,[Val]),!,nb_setarg(N,G,AA).
relax_N(_,_,Val):- var(Val),!, ((mtc_get_attr(Val,iz,_);mtc_get_attr(Val,iza,_))->true;mtc_put_attr(Val,iz,[_])).
relax_N(G,N,Val):- dont_relax(Val)->true;(nb_setarg(N,G,NewVar),put_value(NewVar,Val)).

:- if(exists_source(library(multivar))).
% put_value(Var,Value):- multivar(Var),iz(Var,[Value]),mv_set1(Var,Value).

% put_value(Var,Value):- Var==Value,!.
put_value(Var,Value):- is_dict(Value,Tag),!,
     (Tag==Var->true;put_value(Var,Tag)),
     dict_pairs(Value,_Tag2,Pairs),
     maplist(put_value_attr(Var),Pairs).
put_value(Var,Value):- iz(Var,[Value]).

put_value_attr(Var,N-V):- put_attr_value(Var,N,V).
put_attr_value(Var,iza,V):- !, add_cond(Var,V).
put_attr_value(Var,iz,V):- !, iz(Var,V).
put_attr_value(Arg,Name,FA):- as_constraint_for(Arg,FA,Constraint),!,put_attr_value0(Arg,Name,Constraint).

put_attr_value0(Var,Name,HintE):- 
  (mtc_get_attr(Var,Name,HintL) -> min_cond(HintE,HintL,Hint); Hint=[HintE]), !,
   mtc_put_attr(Var,Name,Hint).



:- else.
 put_value(Var,Value):- iz(Var,[Value]).
:- endif.

dont_relax(A):- var(A),!,is_iz_or_iza(A).
dont_relax(A):- \+ compound(A), \+ atom(A), \+ string(A).

%% relax_args( ?G, ?N, :TermA) is semidet.
%
% Relaxen Arguments.
%
relax_args(G,N,[A|RGS]):-relax_N(G,N,A),!,N2 is N + 1,relax_args(G,N2,RGS).
relax_args(_,_,[]).

%:- set_prolog_flag(verbose_file_search,true).
% @TODO DMILES RE-ADD :- use_module(library(clpfd),except([ins/2,sum/3,op(_,_,_)])).		% Make predicates defined
%:- absolute_file_name(library('clp/clpr.pl'),File),writeln(File).
%:- use_module(user:library(clpr)).		% Make predicates defined
% @TODO DMILES RE-ADD :- use_module(library(clpr),except(['{}'/1])).		% Make predicates defined
:- use_module(user:library(simplex)).		% Make predicates defined

%:- set_prolog_flag(verbose_file_search,false).

:- meta_predicate lazy_pfa(*,+,*).  % arg1 was 0
:- meta_predicate #(*).  %  was 0
'#'(G):- map_lits(lazy,G).

my_when(If,Goal):- when(If,Goal).

%% lazy( :GoalG) is semidet.
%
% Lazy.
%
lazy(G):- var(G),!,freeze(G,lazy(G)).
lazy(G):- ground(G),!,call(G).
lazy((G1,G2)):- !, lazy(G1),lazy(G2).
lazy(is(X,G)):- !,clpr:{X =:= G}.
lazy(G):- functor(G,F,2),clp_r_arithmetic(F),!,clpr:{G}.
lazy(G):- term_variables(G,Vs),maplist(freeze_rev(lazy_1(G)),Vs).


lazy_1(G):- var(G),!,freeze(G,lazy_1(G)).
lazy_1(G):- ground(G),!,call(G).
lazy_1((G1,G2)):- !, lazy_1(G1),lazy_1(G2).
lazy_1(is(X,G)):- !,clpr:{X =:= G}.
lazy_1(G):- functor(G,F,2),clp_r_arithmetic(F),!,clpr:{G}.
lazy_1(G):- term_variables(G,[_]),!,call(G).
lazy_1(G):- term_variables(G,Vs),maplist(freeze_rev(lazy_1(G)),Vs).

freeze_rev(G,V):- freeze(V,G).
% lazy(is(X,G)):-!,term_variables(G,Vs),lazy(Vs,is(X,G)).

clp_r_arithmetic(=<).
clp_r_arithmetic(=:=).
clp_r_arithmetic( := ).
clp_r_arithmetic(<).
clp_r_arithmetic(>=).
clp_r_arithmetic(>).

lazy_pfa(G,F,2):- clp_r_arithmetic(F),!,clpr:{G}.
/*
lazy_pfa(G,_,1):- term_variables(G,[V1|Vs1]),!,
      (Vs1 = [V2|Vs0] -> lazy([V1,V2|Vs0],G)
                      ; freeze(V1,G)).

lazy_pfa(G,_,_):- term_variables(G,[V1|Vs1]),
      (Vs1 = [V2|Vs0] -> lazy([V1,V2|Vs0],G)
                      ; freeze(V1,G)).
*/


%% lazy( ?V, :GoalG) is semidet.
%
% Lazy.
%
lazy([V],G):- !, freeze(V,G).
%lazy([V|Vs],G):- or_any_var([V|Vs],C)->when(C,lazy(G)).
lazy([V|Vs],G):- !, lazy(Vs,freeze(V,G)).
lazy(_,G):- call(G).


or_any_var([V],nonvar(V)).
or_any_var([V|Vs],(nonvar(V);C)):-or_any_var(Vs,C).

% test  lazy(isa(X,Y)),!,X=tCol,melt(Y).

%% thaw( ?Var) is semidet.
%
% Thaw.
%
thaw(Var):- var(Var),!,thaw_var(Var).
thaw(G):- term_attvars(G,Vs),maplist(thaw,Vs).

thaw_var(Var):- term_attvars_deep(Var,Vs),Vs\==[Var],!,maplist(melt,Vs).
thaw_var(Var):- frozen(Var,G),call(G).

term_attvars_deep(Term,VsO):- term_attvars_deep([],Term,VsO).

term_attvars_deep(Sofar,Term,Vs):- notrace(ground(Term)),!,Vs=Sofar.
term_attvars_deep(Sofar,Term,Vs):- \+ var(Term),!, term_attvars(Term,AVs), 
  maplist(term_attvars_deep(Sofar),AVs,VVs),ord_union([Sofar|VVs],Vs),!.
term_attvars_deep(Sofar,Var,VsO):- ord_memberchk(Var,Sofar),!,VsO=Sofar.
term_attvars_deep(Sofar,Var,VsO):- get_attrs(Var,Term),term_attvars(Term,AVs),
 ord_del_element(AVs,Var,Rest),ord_subtract(Rest,Sofar,NewVars),ord_add_element(Sofar,Var,WithNewVar),!,
 (NewVars==[] -> VsO=WithNewVar;  maplist(term_attvars_deep(WithNewVar),NewVars,VVs),ord_union([WithNewVar|VVs],VsO)),!.
term_attvars_deep(Sofar,_,Sofar).



%% melt( ?G) is semidet.
%
% melt.
%
melt(V):-frozen(V,G),call(G).

/*
  call_grounded_constraints,disable_callable_constraints,call_universals,call_each_with_ignore,
  
  call newly grounded_constraints  
  
  enable_callable_constraints
  call_unground_constraints

*/

nonground(G):- \+ ground(G).
enable_reactions(V):- put_attr(V,enable_reactions,true).
disable_reactions(V):- put_attr(V,enable_reactions,false).

:- meta_predicate(mpred_label(:)).
:- module_transparent(mpred_label/1).
:- meta_predicate(mpred_label(+,:)).
:- module_transparent(mpred_label/2).
mpred_label(M:G):- term_attvars(G,Vars),maplist(mpred_label_var(M,pre),Vars),maplist(mpred_label_var(M,post),Vars).
mpred_label(How,M:G):- term_attvars(G,Vars),maplist(mpred_label_var(M,How),Vars).

:- module_transparent(mpred_label_var/3).
mpred_label_var(M,pre,V):-
   obtain_conds(V,List),!,
   put_attr(V,iza,[]),
   maplist(call_when_and_save(M,V,ground),List,MidList),
   maplist(call_when_and_save(M,V,nonground),MidList,NewMidList),
   maplist(call_when_and_save(M,V,nonground),NewMidList,NewList),
   put_attr(V,iza,NewList).

mpred_label_var(M,while,V):-   
   obtain_conds(V,List),!,
   maplist(call_when_and_save(M,V,ground),List,MidList),
   maplist(call_when_and_save(M,V,nonground),MidList,NewMidList),
   maplist(call_when_and_save(M,V,nonground),NewMidList,NewList),
   put_attr(V,iza,NewList).

mpred_label_var(M,post,V):-
   obtain_conds(V,List),
   put_attr(V,iza,[]),!,
   maplist(call_when_and_save(M,V,ground),List,MidList),
   maplist(call_when_and_save(M,V,nonground),MidList,NewMidList),
   maplist(call_when_and_save(M,V,nonground),NewMidList,NewList),
   put_attr(V,iza,NewList).

mpred_label_var(M,Stage,V):- 
   obtain_conds(V,List),
   maplist(call_when_and_save(M,V,Stage),List,NewList),
   put_attr(V,iza,NewList).


call_when_and_save(M,V,When,Cond,Cond):- M:call(When,Cond)-> call_and_save_as_proof(M,V,Cond,Cond) ; true.

call_and_save_as_proof(_,_,call(proved,_),_CCond):- !.
call_and_save_as_proof(M,_,call(call,_),CCond):- !, M:call(CCond),setarg(1,CCond,proved).
call_and_save_as_proof(M,_V,call(ignore,_),CCond):-  (M:call(CCond)->setarg(1,CCond,proved);true).
call_and_save_as_proof(_,_V,aoc(_SK,_What),_CCond):-!.
call_and_save_as_proof(M,_V,dif_objs(X,Y),_CCond):- !, M:dif_objs(X,Y).
call_and_save_as_proof(M,_,CCond,CCond):- M:call(CCond),!.




%% inst_cond( ?X, ?List) is semidet.
%
% An attributed variable with attribute value DVar has been
%
% assigned the value Y
%
% Inst Isac.
%
inst_cond(X, List):- predsort(comp_type,List,SList),call_cond(X,SList).


iza_id(_).

:- module_transparent unify_attr_iza/2.
:- module_transparent unify_attr_iza/3.
:- module_transparent unify_attr_iza_1/3.
:- module_transparent iza:attr_unify_hook/2.

iza:attr_unify_hook(DVar, Y):- unify_attr_iza(DVar, Y).
unify_attr_iza(Dom1, Y):- show_failure(mtc_get_attvar(Dom1,Self)),!,unify_attr_iza_self(Self,Dom1, Y).
unify_attr_iza(Dom1, Y):-
  dumpST,
  dmsg(lhs=(Dom1)),
  dmsg(rhs=(Y)),
  must(show_failure(attvar(Y))),!,
  mtc_put_attr(Y, iza, Dom1 ).

unify_attr_iza_self(Self,Dom1, Y):- atom(Y),as_existential(Y,YY),% isNamed(YY,What),!,
   mtc_get_attr(YY, iza, Dom2),!,
   unify_conds(Dom1,Dom2,Result1),!,
   unify_conds(Dom2,Dom1,Result2),!,
   mtc_put_attr(YY, iza, Result2),
   mtc_put_attr(Self, iza, Result1).
   

unify_attr_iza_self(Self,Dom1, Y):- is_existential(Y),=(Y,YY),!,
   mtc_get_attr(YY, iza, Dom2),!,
   unify_conds(Dom1,Dom2,Result1),!,
   unify_conds(Dom2,Dom1,Result2),!,
   mtc_put_attr(YY, iza, Result2),
   mtc_put_attr(Self, iza, Result1).


unify_attr_iza_self(Self,Dom1, Y):- nonvar(Y),isNamed(Y,What),!,
  (attvar(Self)-> \+ \+ (((attv_bind(Self,Y),chk_cond(Y,Dom1)))) ; chk_cond(Y,Dom1)),!,
  add_cond(Self,aoc(isName,What)).

unify_attr_iza_self(Self,Dom1, Y):- 
  must(show_failure(var(Self))),
  (show_failure(attvar(Y))),!,
  mtc_put_attr(Y, iza, Dom1 ).
unify_attr_iza_self(_Self,Dom1, Y):- chk_cond(Y,Dom1).



local_memberchk_variant(H,Dom1):- memberchk_variant(H,Dom1).

:- module_transparent unify_conds/3.
unify_conds(Dom1,Dom2,Dom1):- Dom1=@=Dom2,!.
unify_conds(Dom1,[],Dom1):-!.
unify_conds(Dom1,[H|Dom2],NewDomain):- local_memberchk_variant(H,Dom1),!,unify_conds(Dom1,Dom2,NewDomain).
unify_conds(Dom1,[H|Dom2],NewDomain):- \+ rejects_cond(H,Dom1),!,
   unify_conds(Dom1,Dom2,NewDomain1),
   (private_cond(H) -> NewDomain1=NewDomain ;
   \+ local_cond(H) -> ord_union(NewDomain1,[H],NewDomain) ;
   \+ memberchk_variant(H,Dom1) -> ord_union(NewDomain1,[H],NewDomain) ;
   NewDomain1=NewDomain).
   

hide_unify_conds(Dom1,Dom2,NewDomain):- show_failure(( \+ disjoint_conds(Dom1,Dom2))),
   % sanity(must(\+ disjoint_conds(Dom2,Dom1))), % ensure the checks got both ways
   ord_union(Dom1, Dom2, NewDomain).


get_typeinfos(Var,List):- obtain_conds(Var,Pre),include(is_typeinfo,Pre,List).
get_post_labeling(Var,List):- obtain_conds(Var,Pre),exclude(is_typeinfo,Pre,List).


is_typeinfo(Pre):- compound(Pre),!,functor(Pre,_,1).
is_typeinfo(Pre):- atom(Pre),!.

% add_all_differnt(QuantsList):-  bagof(differentFromAll(I,O),QuantsList,O),L),maplist(call,L).
add_all_differnt(QuantsList):- 
   maplist(add_all_differnt2(QuantsList),QuantsList),!.

add_all_differnt2(QuantsList,Ex):-
    delete_eq(QuantsList,Ex,DisjExs),
    differentFromAll(Ex,DisjExs).


add_cond_differentFromAll(Ex,DisjExs):- add_cond(Ex,differentFromAll(Ex,DisjExs)).

differentFromAll(One,List):- maplist(dif_objs(One),List).



%% dif_objs( ?A, ?B) is semidet.
%
% Mdif.
%
% dif_objs(A,B):- tlbugger:attributedVars,!,dif(A,B).
dif_objs(A,B):- A==B,!,fail.
dif_objs(A,B):- obtain_object_conds(A,B,Dom1,Dom2),!, 
 dif_objs_doms(Dom1,Dom2).
dif_objs(A,B):- dif(A,B),add_cond(A,dif_objs(A,B)),add_cond(B,dif_objs(B,A)).

dif_objs_doms(Dom1,Dom2):- ((member(aoc(SK,N1),Dom1),memberchk(aoc(SK,N2),Dom2),N1=@=N2)),!,fail.
dif_objs_doms(Dom1,Dom2):-
  \+ non_disjoint_conds(Dom1,Dom2),
   disjoint_conds(Dom1,Dom2).

disjoint_object_conds(Var1,Var2):- 
  obtain_object_conds(Var1,Var2,Dom1,Dom2),
  disjoint_conds(Dom1,Dom2).

obtain_object_conds(Var1,Var2,Dom1,Dom2):- 
  obtain_conds(Var1,Dom1),obtain_conds(Var2,Dom2).

obtain_conds(Var,Doms):- mtc_get_attr(Var,iza,Doms),!.
obtain_conds(Var,DomsO):- compound(Var),\+ is_fort(Var),functor(Var,_,A),arg(A,Var,Doms),
  (is_list(Doms)->DomsO=Doms; obtain_conds(Doms,DomsO)).
obtain_conds(Var,DomsO):- as_existential(Var,X),obtain_conds(X,DomsO).
% obtain_conds(_,[]).

% conds may not be merged
disjoint_conds(Dom1,Dom2):- 
  member(Prop,Dom1), 
  rejects_cond(Prop,Dom2).

% disjoint skolems
rejects_cond(aoc(SK,W1),Dom2):- !, memberchk(aoc(SK,W2),Dom2),'#\\='(W1,W2),!.
rejects_cond(male,Dom2):- !, memberchk(female,Dom2).
rejects_cond(_,_):- fail.

% conds may not be merged
non_disjoint_conds(Dom1,Dom2):- 
  member(Prop,Dom1), 
  not_rejected_cond(Prop,Dom2).


aoc(_,_).

% already same skolems
not_rejected_cond(aoc(SK,W1),Dom2):- !, memberchk(aoc(SK,W2),Dom2),'#='(W1 , W2),!.
not_rejected_cond(male,Dom2):- memberchk(female,Dom2).

as_existential(In,Out):- is_existential(In),!,must(In=Out).
as_existential(In,Out):- var(In),!,decl_existential(In),must(In=Out).
% as_existential(In,Out):- strip_module(In,M,X), oo_deref(M,X,Out)->(X\==Out,is_existential(Out)),!.
as_existential(In,Out):- \+ is_fort(In),!,trace_or_throw(as_existential(In,Out)).
as_existential(In,Out):- nb_current_value(?('$fort2exist$'),In,Out),!.
as_existential(In,Out):- decl_existential(Out0),!,add_cond(Out0,aoc(isNamed,In)),!,
   must(nb_set_value(?('$fort2exist$'),In,Out0)),!,
   must(nb_current_value(?('$fort2exist$'),In,Out)),
   must(add_var_to_env(In,Out)).

% :- ensure_loaded(library(multivar)).
l_xvarx(Var):- xvarx(Var).

decl_existential(Var):- is_existential(Var),!.
decl_existential(Var):- var(Var),!,l_xvarx(Var),put_attr(Var,x,Var),mtc_put_iza(Var,[iza_id(Var)]).
decl_existential(Atomic):- trace_or_throw(\+ decl_existential(Atomic)).

is_existential(Var):- var(Var),!,get_attr(Var,x,V),var(V).
is_existential(the(_)):-!.

:- if(\+ current_predicate(attv_bind/2)).
attv_bind(Var,Value):- Var=Value -> true; put_value(Var,Value).
:- endif.

x:attr_unify_hook(_Was,_Becoming):-!.
x:attr_unify_hook(Was,Becoming):- (attvar(Was),attvar(Becoming)) ->  attv_bind(Was,Becoming) ; true.
x:attribute_goals(Var) --> 
  ({is_existential(Var)} -> [decl_existential(Var)] ; []).
x:attr_portray_hook(Attr,Var):- one_portray_hook(Var,x(Var,Attr)).

one_portray_hook(Var,Attr):-
  locally(set_prolog_flag(write_attributes,ignore),
  ((setup_call_cleanup(set_prolog_flag(write_attributes,ignore),
  ((subst(Attr,Var,SName,Disp),!,
  get_var_name(Var,Name),
   (atomic(Name)->SName=Name;SName=self),
   format('~p',[Disp]))),
   set_prolog_flag(write_attributes,portray))))).

:- module_transparent(user:portray_var_hook/1).
:- multifile(user:portray_var_hook/1).
:- dynamic(user:portray_var_hook/1).

user:portray_var_hook(Var) :- 
 current_prolog_flag(write_attributes,portray),
 attvar(Var),
 get_attr(Var,x,Val),
  current_prolog_flag(write_attributes,Was),
  setup_call_cleanup(set_prolog_flag(write_attributes,ignore),
    writeq({exists(Var,Val)}),
    set_prolog_flag(write_attributes,Was)),!.


show_frame_and_goal(Prefix,Frame):- 
    prolog_frame_attribute(Frame,has_alternatives,Alt),
    prolog_frame_attribute(Frame,goal,Goal),
    prolog_frame_attribute(Frame,parent,Parent),
    prolog_frame_attribute(Parent,goal,PGoal),
    dmsg(frame(Prefix,Frame,Alt,Goal,PGoal)),!.

clause_or_top(clause).
clause_or_top(top).

% non-repeating var
xnr_var(Var):- 
  nonvar(Var) ->true; (get_attr(Var,xnr,_)->true;
   ((gensym(xnr_,Id),
   ((prolog_current_choice(clause_or_top,CP),prolog_choice_attribute(CP,frame,Frame))->true;prolog_current_frame(Frame)),
   % show_frame_and_goal(xnr_var,Frame),
   put_attr(Var,xnr,old_vals(Var,xnr_dif,Id,[],Frame,State)),
   l_xvarx(Var),
   nop(setup_call_cleanup(true,(true;(State=state(redoing))),setarg(1,State,exited)))))).

xnr_var(Cmp,Var):- nonvar(Var) ->true; (get_attr(Var,xnr,_)->true;(gensym(xnr_,Id),put_attr(Var,xnr,old_vals(Var,Cmp,Id,[])))).
xnr:attr_unify_hook(AttValue,VarValue):-
  ((prolog_current_choice(clause_or_top,CP),prolog_choice_attribute(CP,frame,Frame))->true;prolog_current_frame(Frame)),
  AttValue=old_vals(Var,_Cmp,_Id,WazU,OldFrame,State),  
  nb_setarg(4,AttValue,[VarValue|WazU]), 
  once(has_redos(Frame,OldFrame,N)->true;N=0),
  (var(State)->(nb_setarg(6,AttValue,N));true),
  ((N==0) -> 
  ((arg(4,AttValue,List),show_frame_and_goal(has_redos(N),Frame),merge_compatibles(List,Set),!,
  (member(X,Set),attv_bind(Var,X))));(show_frame_and_goal(has_redos(N),Frame),fail)).


% :- ain(((((deduce_neg(P):- _), \+ (deduce_tru(P):-_))) ==> ((deduce_tru(P):- on_bind(P, \+ deduce_neg(P)))))).

xnr(Goal):-term_variables(Goal,Vars),xnr(Vars,Goal).

xnr([A],Goal):- xnr_var(A),!,Goal.
xnr([A|AA],Goal):- xnr_var(xnr_dif_l,[A|AA]),!,Goal.
xnr(_,Goal):-Goal,!.

has_redos(CPFrame,OldCPFrame,0):- OldCPFrame==CPFrame,!.
  
has_redos(CPFrame,OldCPFrame,N):- 
  (prolog_frame_attribute(CPFrame,parent,Parent),has_redos(Parent,OldCPFrame,Nm1)),
  (prolog_frame_attribute(CPFrame,has_alternatives,true)-> ( N is Nm1 + 1) ; N is Nm1).


prolog_current_choice(Type,CPO):-prolog_current_choice(CP),prolog_current_choice(Type,CP,CPO).
prolog_current_choice(Type,CP,CPO):-prolog_choice_attribute(CP,type,WasType),(call(Type,WasType) -> CP=CPO ;
   (prolog_choice_attribute(CP,parent,CPP)->prolog_current_choice(Type,CPP,CPO);CPO=null)).

   
/*
xnr:attr_unify_hook(AttValue,VarValue):- 
  AttValue=old_vals(_Var,_Cmp,_Id,WazU,_Frame,_CP),
  (WazU = [Old|Waz] -> 
   xnr_attr_unify_hook(AttValue,Old,Waz,VarValue) 
   ; nb_setarg(4,AttValue,[VarValue])).
*/

xnr_attr_unify_hook(_,Old,Waz,VarValue):- member_eqz(VarValue,[Old|Waz]),!,fail.
xnr_attr_unify_hook(AttValue,Old,Waz,VarValue):- (is_existential(Old);is_existential(VarValue)),xnr_attr_unify_hook_ex(AttValue,Old,Waz,VarValue). 
xnr_attr_unify_hook(AttValue,Old,Waz,VarValue):- (var(Old);var(VarValue)),!,nb_setarg(4,AttValue,[VarValue,Old|Waz]).
xnr_attr_unify_hook(AttValue,Old,Waz,VarValue):- Old\=@=VarValue,!,nb_setarg(4,AttValue,[VarValue,Old|Waz]).

xnr_attr_unify_hook_ex(AttValue,Old,Waz,VarValue):- ( \+ \+ (Old=VarValue) ),!,
   nb_setarg(4,AttValue,[VarValue,Old|Waz]),member(VarValue,[Old|Waz]).
   
xnr_attr_unify_hook_ex(AttValue,Old,Waz,VarValue):- nb_setarg(4,AttValue,[VarValue,Old|Waz]).


xnr:attribute_goals(_Var) --> !.
xnr:attribute_goals(Var) --> {fail},
  ({is_existential(Var)} -> [] ; [xnr_var(Var)]).

xnr_dif(Old,VarValue):- Old\==VarValue,!,fail.
xnr_dif(Old,VarValue):- (is_existential(Old);is_existential(VarValue)),!,=(Old,VarValue),!,get_attrs(Old,Attrs),nb_put_attrs(Old,Attrs),!,fail.
xnr_dif(Old,VarValue):- (is_fort(Old);is_fort(VarValue)),!,\=(Old,VarValue).
xnr_dif(Old,VarValue):- (var(Old);var(VarValue)),!.
xnr_dif(Old,VarValue):- is_list(Old),!,xnr_dif_l(Old,VarValue).
xnr_dif(Old,VarValue):- nonvar(VarValue),Old\=@=VarValue.

xnr_dif_l([A|Old],[B|VarValue]):- !,(xnr_dif(A,B);xnr_dif_l(Old,VarValue)).
xnr_dif_l(_,_).

merge_compatibles([],[]):-!.
merge_compatibles([N],[N]):-!.
merge_compatibles([N|List],ListOut):-
   member(N,List) *-> merge_compatibles(List,ListOut);
      (merge_compatibles(List,ListMid),ListOut=[N|ListMid]).
  


existential_var(Var,_):- nonvar(Var),!.
existential_var(Var,_):- attvar(Var),!.
existential_var(Var,P):- put_attr(Var,x,P),!.


:- meta_predicate add_constraint_ex(*,*,*).
 % add_constraint_ex(_Call,_P,_V):-!,fail.
add_constraint_ex(_,P,V):- \+ contains_var(V,P),!.
add_constraint_ex(_,P,V):- add_cond(V,P),!.
add_constraint_ex(Call,P,V):-freeze(V,call(Call,V,P)).


unify_two(AN,AttrX,V):- nonvar(V),!, (V='$VAR'(_)->true;throw(unify_two(AN,AttrX,V))).
unify_two(AN,AttrX,V):- get_attr(V,AN,OAttr),!,OAttr=@=AttrX,!. % ,show_call(OAttr=@=AttrX).
unify_two(AN,AttrX,V):- put_attr(V,AN,AttrX).



add_cond_list_val(_,_,_,[]):- !.
add_cond_list_val(Pred1,_,X,[Y]):- atom(Pred1), X==Y -> true;P=..[Pred1,X,Y],add_cond(X,P). 
add_cond_list_val(Pred1,Pred,X,FreeVars):- list_to_set(FreeVars,FreeVarSet),FreeVars\==FreeVarSet,!,
  add_cond_list_val(Pred1,Pred,X,FreeVarSet).
add_cond_list_val(_Pred,Pred,X,FreeVars):- P=..[Pred,X,FreeVars],add_cond(X,P).


:- meta_predicate never_cond(?,*).
never_cond(Var,nesc(b_d(_,nesc,poss), ~ P )):- !, ensure_cond(Var,poss(P)).
never_cond(Var,nesc(~ P )):- !, ensure_cond(Var,poss(P)).
never_cond(Var,(~ P )):- !, ensure_cond(Var,poss(P)).
never_cond(NonVar,Closure):- nonvar(NonVar),!, \+ call_e_tru(NonVar,Closure).
never_cond(_Var,Closure):- ground(Closure),!, call_u(~Closure).
never_cond(Var,Closure):- attvar(Var),!,add_cond(Var,~Closure).
%never_cond(Var,Closure):- add_cond(Var,Closure).


private_cond(iza_id(_)).
local_cond(iza_id(_)).

not_nameOf(Ex,V):- \+ nesc(isNamed(Ex,V)).

var_plain(Var):-var(Var), \+ attvar(Var).

:- module_transparent(isNamed_impl/2).
:- module_transparent(isNamed_const_var/2).
:- module_transparent(isNamed_var/2).

isNamed_impl(Var,Str):- Var=@=Str,!.
isNamed_impl(Var,Str):- atom(Str),!,as_existential(Str,SVar),!,SVar=Var.
isNamed_impl(Var,Str):- var(Var),!,isNamed_var(Var,Str).
isNamed_impl(Var,Str):- atom(Var),!,as_existential(Var,X),!,isNamed_var(X,Str).
isNamed_impl(Var,Str):- !, Var=Str.
isNamed_impl(Var,Str):- isNamed_const_var(Var,Str).


isNamed_const_var(Var,Str):- compound(Str),!,proven_tru(isNamed(Var,Str)).
isNamed_const_var(Var,Str):- number(Var),!,number_string(Var,Str).
isNamed_const_var(Var,Str):- atomic(Var),!,text_to_string(Var,Str).
isNamed_const_var(Var,Str):- term_string(Var,Str).


  

isNamed_var(Var,Str):- var_plain(Var),var_plain(Str),!,strip_module(_,M,_),
 my_when((nonvar(Str);nonvar(Var);?=(Var,Str)),M:isNamed(Var,Str)).
isNamed_var(Var,Str):- nonvar(Str),(has_cond(Var,isNamed(Var,V0));has_cond(Var,aoc(isNamed,V0))),!,text_to_string(V0,Str).
isNamed_var(Var,Str):- nrlc(proven_tru(isNamed(Var,Str))).
isNamed_var(Var,Str):- nonvar(Str),!,add_cond(Var,isNamed(Var,Str)),add_cond(Var,aoc(isNamed,Str)),!,add_var_to_env(Str,Var).
isNamed_var(Var,Str):- var(Str),(has_cond(Var,isNamed(Var,Str));has_cond(Var,aoc(isNamed,Str))),!,
   (nonvar(Str)->add_var_to_env(Str,Var);true).

% isNamed_impl(Var,Str):- proven_tru(isNamed(Var,Str)).
% isNamed_impl(Var,Str):- var(Str),!,add_cond(Var,isNamed(Var,Str)),!.

:- export(isNamed_impl/2).
:- baseKB:import(isNamed_impl/2).
:- module_transparent(baseKB:isNamed/2).
baseKB:isNamed(X,Y):- strip_module(_,M,_),M:isNamed_impl(X,Y).

%:- ain((mtHybrid(Mt)==> {kb_local(Mt:isNamed/2)})).

nrlc(G):- no_repeats(loop_check(G,(((dmsg(warn(looped(G)))),fail)))).


% Translate attributes from this module to residual goals
iza:attribute_goals(X) -->
      { mtc_get_attr(X, iza, List) },!,
      [add_cond(X, List)].

%% add_cond( ?Var, ?HintE) is semidet.
%
% Add Iza.
%
as_constraint_for(Arg,isa(AArg,FA),FA):- \+ kif_option_value(iza_atoms,false), atom(FA),AArg==Arg,!.
as_constraint_for(Arg,ISA,FA):- \+ kif_option_value(iza_atoms,false), compound(ISA), ISA=..[FA,AArg],AArg==Arg,!.
as_constraint_for(Arg,props(AArg,FA),props(FA)):- \+ kif_option_value(iza_atoms,false), atom(FA),AArg==Arg,!.
as_constraint_for(Arg,PROP,props(ASPROP)):- \+ kif_option_value(iza_atoms,false), compound(PROP), PROP=..[FA,AArg|Rest],AArg==Arg,ASPROP=..[FA|Rest].
as_constraint_for(_,FA,FA).


add_cond_rev(Prop,Var):- add_cond(Var,Prop).

:- meta_predicate ensure_cond(?,*).
:- module_transparent(ensure_cond/1).
ensure_cond(Var,Closure):-!, add_cond(Var,Closure).
ensure_cond(NonVar,Closure):- nonvar(NonVar),!,call_e_tru(NonVar,Closure).
ensure_cond(Var,Closure):- is_existential(Var),!,show_failure(add_cond(Var,Closure)).
ensure_cond(Var,Closure):- attvar(Var),!,show_failure(add_cond(Var,Closure)).
ensure_cond(Var,Closure):- as_existential(Var,VarX),must(add_cond(VarX,Closure)),!.

add_cond(Var,Prop):- is_list(Prop),!,as_existential(Var,VarX),obtain_conds(VarX,Dom1),!,maplist(add_cond3(VarX,Dom1),Prop).
add_cond(Var,Prop):- as_existential(Var,VarX),obtain_conds(VarX,Dom1),add_cond3(VarX,Dom1,Prop).

add_cond1(Var,Prop):- obtain_conds(Var,Dom1),add_cond3(Var,Dom1,Prop).

add_cond3(Var,Dom1,Prop):- as_constraint_for(Var,Prop,Constraint),
   show_failure(( \+ rejects_cond(Constraint,Dom1))),
   ord_union(Dom1, [Constraint], NewDomain),
   mtc_put_attr(Var,iza,NewDomain).


:- meta_predicate map_one_or_list(1,?).


map_one_or_list(Call2,ArgOrL):- is_list(ArgOrL)->maplist(Call2,ArgOrL);call(Call2,ArgOrL).

has_cond(Var,Prop):- obtain_conds(Var,Doms),map_one_or_list(has_cond(Doms,Var),Prop).
has_cond(Doms,Var,Prop):- as_constraint_for(Var,Prop,C),member(C,Doms).

rem_cond(Var,Prop):- obtain_conds(Var,Doms),map_one_or_list(rem_cond(Doms,Var),Prop).
rem_cond(Doms,Var,Prop):- as_constraint_for(Var,Prop,C),select(C,Doms,NewDoms),mtc_put_attr(Var,iza,NewDoms).

not_has_cond(Var,Prop):- obtain_conds(Var,Doms),map_one_or_list(not_has_cond(Doms,Var),Prop).
not_has_cond(Doms,Var,Prop):- \+ has_cond(Doms,Var,Prop).




%% chk_cond( ?E, ?Cs) is semidet.
%
% Isac Checking.
%
:- module_transparent(chk_cond/2).
chk_cond(_,_):- local_override(no_kif_var_coroutines,G),!,call(G).
chk_cond(E,Cs):- once(call_cond(E,Cs)).


:- module_transparent(call_cond/2).
:- module_transparent(call_cond_x/2).
%% call_cond( ?VALUE1, :TermARG2) is semidet.
%
% Isac Gen.
%
call_cond(Var):- as_existential(Var,X),obtain_conds(X,Conds),call_cond_x(X,Conds).
call_cond(Var,Conds):- is_fort(Var),!,as_existential(Var,X),call_cond_x(X,Conds).
call_cond(Var,Conds):- call_cond_x(Var,Conds).

call_cond_x(Y, [H|List]):- ground(Y),!,cond_call0(Y,H),!,cond_call00(Y, List).
call_cond_x(Y, [H|List]):- !,maplist(cond_call0(Y),[H|List]).
call_cond_x(_, _).

cond_call00(Y, [H|List]):-!,cond_call0(Y,H),!,cond_call00(Y, List).
cond_call00(_, _).

cond_call0(Y,H):- atom(H),!,nesc(isa(Y,H)).
cond_call0(_,dif_objs(X,Y)):-!,X\==Y.
cond_call0(Y,props(H)):- ereq(props(Y,H)).
cond_call0(Y,H):- arg(_,H,E),Y==E,!,call_u(H).
cond_call0(_,H):- call_u(H).

                     

/*
enforce_fa_unify_hook([Goal|ArgIsas],Value):- !,
  enforce_fa_call(Goal,Value),
  enforce_fa_unify_hook(ArgIsas,Value).
enforce_fa_unify_hook(_,_).

enforce_fa_call(Goal,Value):- atom(Goal),!,call(Goal,Value).
enforce_fa_call(Goal,Value):- arg(_,Goal,Var),Var==Value,!,call(Goal).
enforce_fa_call(Goal,Value):- prepend_arg(Goal,Value,GVoal),!,call(GVoal).

prepend_arg(M:Goal,Value,M:GVoal):- !, prepend_arg(Goal,Value,GVoal).
prepend_arg(Goal,Value,GVoal):- Goal=..[F|ARGS],GVoal=..[F,Value|ARGS].
*/

/*

 G=(loves(X,Y),~knows(Y,tHuman(X))),args_enforce(G,Out),maplist(call,Out).

*/


%% attribs_to_atoms( ?ListA, ?List) is semidet.
%
% Attribs Converted To Atoms.
%
attribs_to_atoms(ListA,List):-map_subterms(attribs_to_atoms0,ListA,List).




%% map_subterms( :PRED2Pred, ?I, ?O) is semidet.
%
% Map Subterms.
%
map_subterms(Pred,I,O):-is_list(I),!,maplist(map_subterms(Pred),I,O).
map_subterms(Pred,I,O):-call(Pred,I,O),!.
map_subterms(Pred,I,O):-compound(I),!,I=..IL,maplist(map_subterms(Pred),IL,OL),O=..OL.
map_subterms(_Pred,IO,IO).




%% condz_to_isa( :TermAA, :TermAB) is semidet.
%
% iza Converted To  (iprops/2).
%
condz_to_isa(Iza,ftTerm):-var(Iza),!.
condz_to_isa((A,B),isAnd(ListO)):-!,conjuncts_to_list((A,B),List),list_to_set(List,Set),min_cond_l(Set,ListO).
condz_to_isa((A;B),isOr(Set)):-!,conjuncts_to_list((A,B),List),list_to_set(List,Set).
condz_to_isa(AA,AB):-must(AA=AB).




%% attribs_to_atoms0( ?Var, ?Isa) is semidet.
%
% Attribs Converted To Atoms Primary Helper.
%
attribs_to_atoms0(Var,Isa):-mtc_get_attr(Var,iza,Iza),!,must(condz_to_isa(Iza,Isa)).
attribs_to_atoms0(O,O):- \+ (compound(O)).


%% min_cond_l( ?List, ?ListO) is semidet.
%
% min  (sub_super/2) (List version).
%
min_cond_l(List,ListO):-isa_pred_l(lambda(Y,X,sub_super(X,Y)),List,ListO).



%% max_cond_l( ?List, ?ListO) is semidet.
%
% max  (sub_super/2) (List version).
%
max_cond_l(List,ListO):-isa_pred_l(sub_super,List,ListO).



%% isa_pred_l( :PRED2Pred, ?List, ?ListO) is semidet.
%
%  (iprops/2) Predicate (List version).
%
isa_pred_l(Pred,List,ListO):-isa_pred_l(Pred,List,List,ListO).




%% isa_pred_l( :PRED2Pred, ?UPARAM2, ?List, ?UPARAM4) is semidet.
%
%  (iprops/2) Predicate (List version).
%
isa_pred_l(_Pred,[],_List,[]).
isa_pred_l(Pred,[X|L],List,O):-member(Y,List),X\=Y,call_u(call(Pred,X,Y)),!,isa_pred_l(Pred,L,List,O).
isa_pred_l(Pred,[X|L],List,[X|O]):-isa_pred_l(Pred,L,List,O).




%% min_cond( :TermHintA, ?HintE, ?HintE) is semidet.
%
% min  (sub_super/2).
%
min_cond([H],In,Out):- !, min_cond0(H,In,Out).
min_cond([H|T],In,Out):- !, min_cond0(H,In,Mid),min_cond(T,Mid,Out).
min_cond(E,In,Out):- min_cond0(E,In,Out).

min_cond0(HintA,[],[HintA]).
min_cond0(HintA,[HintB|HintL],[HintB|HintL]):- HintA==HintB,!.
min_cond0(HintA,[HintB|HintL],[HintA,HintB|HintL]):- functor(HintA,_,A),functor(HintB,_,B),B>A,!.
min_cond0(HintA,[HintB|HintL],[HintA|HintL]):- sub_super(HintA,HintB),!.
min_cond0(HintA,[HintB|HintL],[HintB|HintL]):- sub_super(HintB,HintA),!.
min_cond0(HintA,[HintB|HintL],[HintB|HintS]):- !,min_cond0(HintA,HintL,HintS).



sub_super(Col1,Col2):- tCol(Col1),!,genls(Col1,Col2).

%% max_cond( :TermHintA, ?HintE, ?HintE) is semidet.
%
% max  (sub_super/2).
%
max_cond([H],In,Out):- !, max_cond0(H,In,Out).
max_cond([H|T],In,Out):- !, max_cond0(H,In,Mid),max_cond(T,Mid,Out).
max_cond(E,In,Out):- max_cond0(E,In,Out).

max_cond0(HintA,[],[HintA]).
max_cond0(HintA,[HintB|HintL],[HintB|HintL]):- HintA==HintB,!.
max_cond0(HintA,[HintB|HintL],[HintA,HintB|HintL]):- functor(HintA,_,A),functor(HintB,_,B),B>A,!.
max_cond0(HintA,[HintB|HintL],[HintA|HintL]):- sub_super(HintB,HintA),!.
max_cond0(HintA,[HintB|HintL],[HintB|HintL]):- sub_super(HintA,HintB),!.
max_cond0(HintA,[HintB|HintL],[HintB|HintS]):- !,max_cond0(HintA,HintL,HintS).





:- style_check(-singleton).




%% unrelax( ?X) is semidet.
%
% Domain Labeling (residuals).
%
unrelax(X):-copy_term(X,X,Gs),maplist(iz_member,Gs).




%% iz_member( :GoalG) is semidet.
%
% Domain Member.
%
iz_member(iz(X,List)):-!,member(X,List).
iz_member(G):-G.


:- style_check(-singleton).

%% attempt_attribute_args( ?AndOr, ?Hint, :TermVar) is semidet.
%
% Attempt Attribute Arguments.
%

attempt_attribute_args(_AndOr,Hint,Var):- var(Var),add_cond(Var,Hint),!.
attempt_attribute_args(_AndOr,_Hint,Grnd):-ground(Grnd),!.
attempt_attribute_args(_AndOr,_Hint,Term):- \+ (compound(Term)),!.
attempt_attribute_args(AndOr,Hint,+(A)):-!,attempt_attribute_args(AndOr,Hint,A).
attempt_attribute_args(AndOr,Hint,-(A)):-!,attempt_attribute_args(AndOr,Hint,A).
attempt_attribute_args(AndOr,Hint,?(A)):-!,attempt_attribute_args(AndOr,Hint,A).
attempt_attribute_args(AndOr,Hint,(A,B)):-!,attempt_attribute_args(AndOr,Hint,A),attempt_attribute_args(AndOr,Hint,B).
attempt_attribute_args(AndOr,Hint,[A|B]):-!,attempt_attribute_args(AndOr,Hint,A),attempt_attribute_args(AndOr,Hint,B).
attempt_attribute_args(AndOr,Hint,(A;B)):-!,attempt_attribute_args(';'(AndOr),Hint,A),attempt_attribute_args(';'(AndOr),Hint,B).
attempt_attribute_args(_AndOr,_Hint,Term):- use_was_isa(Term,I,C), add_cond(I,C).
attempt_attribute_args(AndOr,_Hint,Term):- Term=..[F,A],tCol(F),!,attempt_attribute_args(AndOr,F,A).
attempt_attribute_args(AndOr,Hint,Term):- Term=..[F|ARGS],!,attempt_attribute_args(AndOr,Hint,F,1,ARGS).




%% attempt_attribute_args( ?AndOr, ?Hint, ?F, ?N, :TermARG5) is semidet.
%
% Attempt Attribute Arguments.
%
attempt_attribute_args(_AndOr,_Hint,_F,_N,[]):-!.
attempt_attribute_args(AndOr,_Hint,t,1,[A]):-attempt_attribute_args(AndOr,callable,A).
attempt_attribute_args(AndOr,Hint,t,N,[A|ARGS]):-atom(A),!,attempt_attribute_args(AndOr,Hint,A,N,ARGS).
attempt_attribute_args(_AndOr,_Hint,t,_N,[A|_ARGS]):- \+ (atom(A)),!.
attempt_attribute_args(AndOr,Hint,F,N,[A|ARGS]):-attempt_attribute_one_arg(Hint,F,N,A),N2 is N+1,attempt_attribute_args(AndOr,Hint,F,N2,ARGS).




%% attempt_attribute_one_arg( ?Hint, ?F, ?N, ?A) is semidet.
%
% Attempt Attribute One Argument.
%
attempt_attribute_one_arg(_Hint,F,N,A):-call_u(argIsa(F,N,Type)),Type\=ftTerm, \+ (compound(Type)),!,attempt_attribute_args(and,Type,A).
attempt_attribute_one_arg(_Hint,F,N,A):-call_u(argQuotedIsa(F,N,Type)),Type\=ftTerm, \+ (compound(Type)),!,attempt_attribute_args(and,Type,A).
attempt_attribute_one_arg(_Hint,F,N,A):-call_u(argIsa(F,N,Type)),Type\=ftTerm,!,attempt_attribute_args(and,Type,A).
attempt_attribute_one_arg(_Hint,F,N,A):-attempt_attribute_args(and,argi(F,N),A).

                                     

:- was_export((samef/2,same/2)).



%% same( ?X, ?Y) is semidet.
%
% Same.
%
same(X,Y):- samef(X,Y),!.
same(X,Y):- compound(X),arg(1,X,XX)->same(XX,Y),!.
same(Y,X):- compound(X),arg(1,X,XX),!,same(XX,Y).




%% samef( ?X, ?Y) is semidet.
%
% Samef.
%
samef(X,Y):- quietly(((to_functor(X,XF),to_functor(Y,YF),(XF=YF->true;string_equal_ci(XF,YF))))).




%% to_functor( ?A, ?O) is semidet.
%
% Converted To Functor.
%
to_functor(A,O):-is_ftVar(A),!,A=O.
to_functor(A,O):-compound(A),get_functor(A,O),!. % ,to_functor(F,O).
to_functor(A,A).

:- was_export(arg_to_var/3).



%% arg_to_var( ?Type, ?String, ?Var) is semidet.
%
% Argument Converted To Variable.
%
arg_to_var(_Type,_String,_Var).

:- was_export(same_arg/3).




%% same_arg( ?How, ?X, ?Y) is semidet.
%
% Same Argument.
%
same_arg(_How,X,Y):-var(X),var(Y),!,X=Y.
same_arg(equals,X,Y):-!,equals_call(X,Y).
same_arg(tCol(_Type),X,Y):-!, unify_with_occurs_check(X,Y).

same_arg(ftText,X,Y):-(var(X);var(Y)),!,X=Y.
same_arg(ftText,X,Y):-!, string_equal_ci(X,Y).

same_arg(same_or(equals),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(sub_super),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(sub_super),Sub,Sup):- holds_t(sub_super,Sub,Sup),!.
same_arg(same_or(isa),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(isa),I,Sup):- !, holds_t(Sup,I),!.

same_arg(same_or(_Pred),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(Pred),I,Sup):- holds_t(Pred,I,Sup),!.

% same_arg(I,X):- promp_yn('~nSame Objects: ~q== ~q ?',[I,X]).



%% promp_yn( ?Fmt, ?A) is semidet.
%
% Promp Yn.
%
promp_yn(Fmt,A):- format(Fmt,A),get_single_char(C),C=121.



% :-swi_module(iz, [ iz/2  ]). % Var, ?Domain
:- use_module(library(ordsets)).

%% iz( ?X, ?Dom) is semidet.
%
% Domain.
%
:- was_export(iz/2).

iz(X, Dom) :- var(Dom), !, mtc_get_attr(X, iz, Dom).
% iz(X, Dom) :- var(Dom), !, (mtc_get_attr(X, iz, Dom)->true;mtc_put_attr(X, iz, [iziz(Dom)])).
iz(X, List) :- 
      listify(List,List0),
      list_to_ord_set(List0, Domain),
      mtc_put_attr(Y, iz, Domain),
      X = Y.

:- was_export(extend_iz_member/2).



%% extend_iz_member( ?X, ?DomL) is semidet.
%
% Extend Domain.
%
extend_iz_member(X, DomL):- init_iz(X, Dom2), ord_union(Dom2, DomL, NewDomain),mtc_put_attr( X, iz, NewDomain ).

:- was_export(extend_iz/2).



%% extend_iz( ?X, ?DomE) is semidet.
%
% Extend Domain.
%
extend_iz(X, DomE):-  init_iz(X, Dom2),ord_add_element(Dom2, DomE, NewDomain),mtc_put_attr( X, iz, NewDomain ).

:- was_export(init_iz/2).



%% init_iz( ?X, ?Dom) is semidet.
%
% Init Domain.
%
init_iz(X,Dom):-mtc_get_attr(X, iz, Dom),!.
init_iz(X,Dom):-Dom =[_], mtc_put_attr(X, iz, Dom),!.

% An attributed variable with attribute value Domain has been
% assigned the value Y

iz:attr_unify_hook([Y], Value) :-  same(Y , Value),!.
iz:attr_unify_hook(Domain, Y) :-
   ( mtc_get_attr(Y, iz, Dom2)
   -> ord_intersection(Domain, Dom2, NewDomain),
         ( NewDomain == []
         -> fail
         ; NewDomain = [Value]
          -> same(Y , Value)
             ; mtc_put_attr(Y, iz, NewDomain)
           )
   ; var(Y)
   -> mtc_put_attr( Y, iz, Domain )
   ; (\+ \+ (cmp_memberchk_0(Y, Domain)))
).



% Translate attributes from this module to residual goals
iz:attribute_goals(X) --> { mtc_get_attr(X, iz, List) },!,[iz(X, List)].



%iz:attr_portray_hook(Val, _) :- write('iz:'), write(Val),!.

%iza:attr_portray_hook(Val, _) :- write('iza:'), write(Val),!.


%% cmp_memberchk_0( ?X, ?Y) is semidet.
%
% Cmp Memberchk.
%
cmp_memberchk_0(X,Y):-numbervars(X,0,_,[attvars(skip)]),member(X,Y),!.



%% cmp_memberchk_00( ?Item, :TermX1) is semidet.
%
% Cmp Memberchk Primary Helper.
%
cmp_memberchk_00(Item, [X1,X2,X3,X4|Xs]) :- !,
	compare(R4, Item, X4),
	(   R4 = (>) -> cmp_memberchk_00(Item, Xs)
	;   R4 = (<) ->
	    compare(R2, Item, X2),
	    (   R2 = (>) -> Item = X3
	    ;   R2 = (<) -> Item = X1
	    ;/* R2 = (=),   Item = X2 */ true
	    )
	;/* R4 = (=) */ true
	).
cmp_memberchk_00(Item, [X1,X2|Xs]) :- !,
	compare(R2, Item, X2),
	(   R2 = (>) -> cmp_memberchk_00(Item, Xs)
	;   R2 = (<) -> Item = X1
	;/* R2 = (=) */ true
	).
cmp_memberchk_00(Item, [X1]) :-
	Item = X1.


:- meta_predicate(call_engine_m(?,0,-,-)).
call_engine_m(Templ,Goal,Engine,Det):-
  call_engine_start_m(Templ,Goal,Engine),
  call_engine_next_m(Engine,Templ,Det).

:- meta_predicate(call_engine_start_m(?,0,-)).
call_engine_start_m(Templ,Goal,Engine):-
   engine_create(Templ-TF0,(Goal,deterministic(TF0)),Engine).

call_engine_next_m(Engine,Templ,Det):-
   repeat,
    engine_next(Engine,Templ-Det),
     (Det==true->!;true).

metapred_plus(_,_):-!.
metapred_plus(Cmp,Plus):-
  (\+ compound(Cmp) -> S=0 ; compound_name_arity(Cmp,F,S)),
  A is S + Plus,
  current_predicate(F/A),!.
metapred_plus(_,_).

not_dif_objs(A,B):- \+ dif_objs(A,B).

:- meta_predicate(pred1_to_unique_pairs(1,-,-)).
pred1_to_unique_pairs(Pred1,Obj1,Obj2):-
  sanity(assertion(metapred_plus(Pred1,1))),
  lazy_findall(Elem,call(Pred1,Elem),List),
  list_to_unique_pairs(List,Obj1,Obj2).

:- meta_predicate(pred1_to_unique_pairs_confirmed(1,-,-)).
pred1_to_unique_pairs_confirmed(Pred1,Obj1,Obj2):-
   Tracker = '$t'([]),
   Same2 = not_dif_objs,
   pred1_to_unique_pairs(Pred1,ObjA,ObjB),
   different_pairs(Same2,Tracker,ObjA,ObjB,Obj1,Obj2).

list_to_unique_pairs(List,Obj1,Obj2):-
  append(_Left,[Obj1|Rest],List),member(Obj2,Rest).

:- meta_predicate different_pairs(2,+,?,?,?,?).
different_pairs(Same2,Tracker,ObjA,ObjB,Obj1,Obj2):- 
 Test = p(TObj1,TObj2),
 zotrace(sanity((must_be(compound,Tracker),
    assertion(metapred_plus(Pred2InstsDiff,2))))),
 zotrace((\+ call(Same2, ObjA, ObjA))),
 zotrace((( ObjA @> ObjB -> (ObjA = Obj1, ObjB = Obj2) ; (ObjA = Obj2, ObjB = Obj1)))),
 must(arg(1,Tracker,PrevPairs)),
 (((member(Test,PrevPairs),call(Same2,Obj1,TObj1),call(Same2,Obj2,TObj2)))-> fail ; true),
 must(nb_setarg(1,Tracker,[p(Obj1,Obj2)|PrevPairs])).



/** <module> The difv/2 constraint
*/

%!  difv(+Term1, +Term2) is semidet.
%
%   Constraint that expresses that  Term1   and  Term2  never become
%   variant (=@@=/2). Fails if `Term1 =@@=  Term2`. Succeeds if Term1
%   can  never  become  identical  to  Term2.  In  other  cases  the
%   predicate succeeds after attaching constraints   to the relevant
%   parts of Term1 and Term2 that prevent   the  two terms to become
%   identical.

=@@=(X,Y):-!, ==(X,Y).
% =@@=(X,Y):- (attvar(X);attvar(Y))-> X==Y ;((var(X);var(Y))-> X==Y ; X=@=Y).

:- op(700,xfx,user:('=@@=')).

% difv(_X,_Y):-!.
difv(X,Y) :-
    \+ (X =@@= Y),
    difv_c_c(X,Y,_).

difv_unifiable(X, Y, Us) :-
    (    current_prolog_flag(occurs_check, error) ->
         catch(unifiable(X,Y,Us), error(occurs_check(_,_),_), false)
    ;    unifiable(X, Y, Us)
    ).

difv_c_c(X,Y,OrNode) :-
    (       difv_unifiable(X, Y, Unifier) ->
            ( Unifier == [] ->
                    or_one_failv(OrNode)
            ;
                    difv_c_c_l(Unifier,OrNode)
            )
    ;
            or_succeedv(OrNode)
    ).


difv_c_c_l(Unifier,OrNode) :-
    length(Unifier,N),
    extend_ornodevv(OrNode,N,List,Tail),
    difv_c_c_l_aux(Unifier,OrNode,List,Tail).

extend_ornodevv(OrNode,N,List,Vars) :-
    ( get_attr(OrNode,difv,Attr) ->
            Attr = nodev(M,Vars),
            O is N + M - 1
    ;
            O = N,
            Vars = []
    ),
    put_attr(OrNode,difv,nodev(O,List)).

difv_c_c_l_aux([],_,List,List).
difv_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
    List = [X=Y|Rest],
    add_ornodevv(X,Y,OrNode),
    difv_c_c_l_aux(Unifier,OrNode,Rest,Tail).

add_ornodevv(X,Y,OrNode) :-
    add_ornodev_var1(X,Y,OrNode),
    ( var(Y) ->
            add_ornodev_var2(X,Y,OrNode)
    ;
            true
    ).

add_ornodev_var1(X,Y,OrNode) :-
    ( get_attr(X,difv,Attr) ->
            Attr = vardifv(V1,V2),
            put_attr(X,difv,vardifv([OrNode-Y|V1],V2))
    ;
            put_attr(X,difv,vardifv([OrNode-Y],[]))
    ).

add_ornodev_var2(X,Y,OrNode) :-
    ( get_attr(Y,difv,Attr) ->
            Attr = vardifv(V1,V2),
            put_attr(Y,difv,vardifv(V1,[OrNode-X|V2]))
    ;
            put_attr(Y,difv,vardifv([],[OrNode-X]))
    ).

difv:attr_unify_hook(vardifv(V1,V2),Other) :-
    ( var(Other) ->
            reverse_lookupsv(V1,Other,OrNodes1,NV1),
            or_one_failvsv(OrNodes1),
            get_attr(Other,difv,OAttr),
            OAttr = vardifv(OV1,OV2),
            reverse_lookupsv(OV1,Other,OrNodes2,NOV1),
            or_one_failvsv(OrNodes2),
            remove_obsoletev(V2,Other,NV2),
            remove_obsoletev(OV2,Other,NOV2),
            append(NV1,NOV1,CV1),
            append(NV2,NOV2,CV2),
            ( CV1 == [], CV2 == [] ->
                    del_attr(Other,difv)
            ;
                    put_attr(Other,difv,vardifv(CV1,CV2))
            )
    ;
            verify_compoundsv(V1,Other),
            verify_compoundsv(V2,Other)
    ).

remove_obsoletev([], _, []).
remove_obsoletev([N-Y|T], X, L) :-
    (   Y=@@=X ->
        remove_obsoletev(T, X, L)
    ;   L=[N-Y|RT],
        remove_obsoletev(T, X, RT)
    ).

reverse_lookupsv([],_,[],[]).
reverse_lookupsv([N-X|NXs],Value,Nodes,Rest) :-
    ( X =@@= Value ->
            Nodes = [N|RNodes],
            Rest = RRest
    ;
            Nodes = RNodes,
            Rest = [N-X|RRest]
    ),
    reverse_lookupsv(NXs,Value,RNodes,RRest).

verify_compoundsv([],_).
verify_compoundsv([OrNode-Y|Rest],X) :-
    ( var(Y) ->
            true
    ; OrNode == (-) ->
            true
    ;
            difv_c_c(X,Y,OrNode)
    ),
    verify_compoundsv(Rest,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
or_succeedv(OrNode) :-
    ( attvar(OrNode) ->
            get_attr(OrNode,difv,Attr),
            Attr = nodev(_Counter,Pairs),
            del_attr(OrNode,difv),
            OrNode = (-),
            del_or_difv(Pairs)
    ;
            true
    ).

or_one_failvsv([]).
or_one_failvsv([N|Ns]) :-
    or_one_failv(N),
    or_one_failvsv(Ns).

or_one_failv(OrNode) :-
    ( attvar(OrNode) ->
            get_attr(OrNode,difv,Attr),
            Attr = nodev(Counter,Pairs),
            NCounter is Counter - 1,
            ( NCounter == 0 ->
                    fail
            ;
                    put_attr(OrNode,difv,nodev(NCounter,Pairs))
            )
    ;
            fail
    ).

del_or_difv([]).
del_or_difv([X=Y|Xs]) :-
    cleanup_dead_nodesv(X),
    cleanup_dead_nodesv(Y),
    del_or_difv(Xs).

cleanup_dead_nodesv(X) :-
    ( attvar(X) ->
            get_attr(X,difv,Attr),
            Attr = vardifv(V1,V2),
            filter_dead_orsv(V1,NV1),
            filter_dead_orsv(V2,NV2),
            ( NV1 == [], NV2 == [] ->
                    del_attr(X,difv)
            ;
                    put_attr(X,difv,vardifv(NV1,NV2))
            )
    ;
            true
    ).

filter_dead_orsv([],[]).
filter_dead_orsv([Or-Y|Rest],List) :-
    ( var(Or) ->
            List = [Or-Y|NRest]
    ;
            List = NRest
    ),
    filter_dead_orsv(Rest,NRest).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The attribute of a variable X is vardifv/2. The first argument is a
   list of pairs. The first component of each pair is an OrNode. The
   attribute of each OrNode is node/2. The second argument of node/2
   is a list of equations A = B. If the LHS of the first equation is
   X, then return a goal, otherwise don''t because someone else will.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

difv:attribute_goals(Var) --> !.
difv:attribute_goals(Var) -->
    (   { get_attr(Var, difv, vardifv(Ors,_)) } ->
        or_nodesv(Ors, Var)
    ;   or_nodev(Var)
    ).

or_nodev(O) -->
    (   { get_attr(O, difv, nodev(_, Pairs)) } ->
        { eqs_lefts_rightsv(Pairs, As, Bs) },
        mydifv(As, Bs),
        { del_attr(O, difv) }
    ;   []
    ).

or_nodesv([], _)       --> [].
or_nodesv([O-_|Os], X) -->
    (   { get_attr(O, difv, nodev(_, Eqs)) } ->
        (   { Eqs = [LHS=_|_], LHS =@@= X } ->
            { eqs_lefts_rightsv(Eqs, As, Bs) },
            mydifv(As, Bs),
            { del_attr(O, difv) }
        ;   []
        )
    ;   [] % or-node already removed
    ),
    or_nodesv(Os, X).

mydifv([X], [Y]) --> !, difv_if_necessary(X, Y).
mydifv(Xs0, Ys0) -->
    { reverse(Xs0, Xs), reverse(Ys0, Ys), % follow original order
      X =.. [f|Xs], Y =.. [f|Ys] },
    difv_if_necessary(X, Y).

difv_if_necessary(X, Y) -->
    (   { difv_unifiable(X, Y, _) } ->
        [difv(X,Y)]
    ;   []
    ).

eqs_lefts_rightsv([], [], []).
eqs_lefts_rightsv([A=B|ABs], [A|As], [B|Bs]) :-
    eqs_lefts_rightsv(ABs, As, Bs).

%% type_size( ?VALUE1, :PRED1000VALUE2) is semidet.
%
% Type Size.
%
type_size(C,S):-a(completeExtentEnumerable,C),!,setof(E,call_u(t(C,E)),L),length(L,S).
type_size(C,1000000):-a(ttExpressionType,C),!.
type_size(_,1000).

/*

?-  Z #=:= 2 + X, Z #< 2 .

succ(succ(0)).

S2I
I2E

2
2
2
E2S

S = succ/1.
I = integer
E = 2

a:p(1).

a:p(X):-b:p(X).
b:p(X):-c:p(X).

b:p(2).

*/ 


%% comp_type( ?Comp, ?Col1, ?Col2) is semidet.
%
% Comp Type.
%
comp_type(Comp,Col1,Col2):-type_size(Col1,S1),type_size(Col2,S2),compare(Comp,S1,S2).


:- fixup_exports.

mpred_type_constraints_file.


%% goal_expansion( ?LC, ?LCOO) is semidet.
%
% Hook To [system:goal_expansion/2] For Module Mpred_type_constraints.
% Goal Expansion.
%
% system:goal_expansion(G,O):- \+ current_prolog_flag(xref,true),\+ pldoc_loading, nonvar(G),boxlog_goal_expansion(G,O).


