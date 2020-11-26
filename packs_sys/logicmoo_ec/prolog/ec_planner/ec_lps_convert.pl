% =========================================
% Goal/Plan translating
% =========================================
:- module(ec_lps_convert,[%load_e/1, needs_proccess/3,process_ec/2,fix_time_args/3,fix_goal/3, 
  %brk_on_bind/1,assert_axiom_2/2,
   assert_lps/1,
   test_logicmoo_ec_lps_reader/0,
   test_lps_ereader/0,
   test_logicmoo_ec_sanity/0,
   test_logicmoo_ec_reader_2/0,
   test_logicmoo_ec_lps_reader/2,test_logicmoo_ec_lps_reader/1]).

skip_tests. 

test_logicmoo_ec_sanity:- skip_tests,!. 
test_logicmoo_ec_sanity:- test_lps_ereader. 

:- use_module(library(logicmoo_common)).

:- use_module(library(ec_planner/ec_loader)).

/*export_transparent(P):-
  export(P),
  module_transparent(P).
*/
:- use_module(library(logicmoo_lps)).
% system:ec_current_domain(X):- wdmsg(ec_current_domain(X)),fail.
:- user:use_module(library('ec_planner/ec_planner_dmiles')).
:- use_module(library(ec_planner/ec_reader)).

:- use_module(library(lps_corner)).

:- set_prolog_flag(lps_translation_only_HIDE,false).
:- set_prolog_flag(lps_translation_only,false).


assert_lps(user:ec_current_domain_db(Stuff,_)):-!,  assert_lps(Stuff).
assert_lps(axiom(Stuff,Nil)):- Nil==[],!,assert_lps(Stuff).
assert_lps(axiom(Stuff,List)):- !,must_or_rtrace(list_to_conjuncts(List,Body)),!,assert_lps(->(Body,Stuff)).
assert_lps(Stuff):- assert_ep(lps_test_mod,Stuff).

print_tree_cmt(C,P):-
 mention_o_s_l,
 notrace((echo_format('~N'),  
  with_output_to(string(S), in_cmt((
    format('~NFrom E: \n\n',[]),
    print_tree(P)))),
  to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]))).

already_lps(Form):- var(Form),!,throw(var_already_lps(Form)).
already_lps(:- _):-!.
already_lps(option(_,_)):-!.
already_lps(false(_)):-!.
already_lps(mpred_prop(_,_)):-!.
already_lps(sort(_)):-!.
already_lps(subsort(_,_)):-!.


assert_ep( _M,Form):- print_tree_cmt(white,Form),fail.
assert_ep(Mod,Form):- already_lps(Form),assert_post(Mod,Form,Form).
assert_ep(Mod,Form):-
  format('~N',[]),
  must_or_rtrace(ep_to_lps(Form,Lps)),
  major_debug(ignore((Form\==Lps->pprint_ecp_cmt(hfg(green),Form)))),
  must_or_rtrace(assert_post(Mod,Form,Lps)),!.


%assert_post(Mod,_,include(F)):- include_e_lps_file_now(Type,Mod:F).
%assert_post(Mod,_,load(F)):- include_e_lps_file_now(Type,Mod:F). 
%assert_post(Mod,_,include(F)):- !, with_e_file(assert_ep(Mod),current_output, [ec(F)]). 
%assert_post(Mod,_,load(X)):- nop(assert_ep(Mod,include(X))),!.
assert_post(Mod,Form,Lps):- nortrace, is_list(Lps),!, maplist(assert_post(Mod,Form),Lps).
assert_post(Mod,t(Type,Inst),_):- atom(Type), M=..[Type,Inst],!,assert_post(Mod,M,M),!.
assert_post(Mod,_Form,Lps):- 
  lps_xform(Mod,Lps,Prolog),!,
  must_or_rtrace((Lps\==Prolog->(ignore(( /*(Form\==Prolog,Lps==Prolog)-> */
    print_lps_syntax(yellow,Lps),
     nop(pprint_ecp(yellow,Lps)))),
    pprint_ecp_cmt(cyan,Prolog),pprint_ecp_cmt(white,"% ================================="))
   ;assert_lps_try_harder(Prolog))),
  must_or_rtrace(assert_pl(Mod,Prolog)),!.

print_lps_syntax(Color,Lps):- 
 with_lps_operators2(user,
  ec_lps_convert:with_lps_operators2(pretty_clauses,pretty_clauses:clause_to_string(Lps,S))),
    real_ansi_format(hfg(Color), '~N~s.~N', [S]),!.

lps_xform(Mod,Lps,Prolog):- 
 locally(current_prolog_flag(lps_translation_only_HIDE,true),
   locally(t_l:lps_program_module(Mod),
    must_or_rtrace(lps_f_term_expansion_now(Mod,Lps,Prolog)))),!.

:- export_transparent(with_lps_operators2/2).
with_lps_operators2(M,Goal):- 
   setup_call_cleanup(push_operators(M:[op(900, fy, M:not),  op(1200, xfx, M:then), op(1185, fx, M:if), op(1190, xfx, M:if), op(1100, xfy, M:else), op(1050, xfx, M:terminates), op(1050, xfx, M:initiates), op(1050, xfx, M:updates), op(1050, fx, M:observe), op(1050, fx, M:false), op(1050, fx, M:initially), op(1050, fx, M:fluents), op(1050, fx, M:events), op(1050, fx, M:prolog_events), op(1050, fx, M:actions), op(1050, fx, M:unserializable), op(999, fx, M:update), op(999, fx, M:initiate), op(999, fx, M:terminate), op(997, xfx, M:in), op(995, xfx, M:at), op(995, xfx, M:during), op(995, xfx, M:from), op(994, xfx, M:to), op(1050, xfy, M: ::), op(1200, xfx, M:(<-)), op(1050, fx, M:(<-)), op(700, xfx, M: <=)],Undo),
     M:call(Goal),pop_operators(Undo)).

:- export_transparent(with_lps_operators/1).
with_lps_operators(MGoal):- 
  strip_module(MGoal,M,Goal),
  with_lps_operators2(user,ec_lps_convert:with_lps_operators2(M,M:Goal)).


assert_pl(Mod,Prolog):- is_list(Prolog),!, maplist(assert_pl(Mod),Prolog).
assert_pl(Mod,(P1,P2)):- !,assert_pl(Mod,P1),assert_pl(Mod,P2).
assert_pl(_Mod,Prolog):- % get_source_location(File,Line),
   major_debug(pprint_ecp_pl(yellow,Prolog)).

include_e_lps_file_now(Type,MFile):- strip_module(MFile,M,File), include_e_lps_file_now(Type,M,File).
include_e_lps_file_now(Type,M,File):- absolute_file_name(File,AbsFile),File\==AbsFile,exists_file(AbsFile), !,include_e_lps_file_now(Type,M,AbsFile).

include_e_lps_file_now(Type,Mod,File):- 
   translate_e_to_filetype(Mod:Type,File).

load_e_lps_file(Type,File):- retractall(etmp:ec_option(load(_), _)), include_e_lps_file(Type,File).
load_e_lps_file(Type,File):- retractall(etmp:ec_option(load(_), _)), include_e_lps_file(Type,File).
  
include_e_lps_file(Type,File):- is_list(File), !, maplist(include_e_lps_file(Type),File).
include_e_lps_file(Type,File):- wdmsg(include_e_lps_file(Type,File)),fail.
include_e_lps_file(Type,File):- needs_resolve_local_files(File,Resolved),!,include_e_lps_file(Type,Resolved).
include_e_lps_file(Type,File):- absolute_file_name(File,DB), exists_file(DB),!, 
  update_changed_files,   
  strip_module(_,M,_), prolog_statistics:time(M:include_e_lps_file_now(Type,File)),!.
include_e_lps_file(Type,File):- throw(with_abs_paths(include_e_lps_file(Type),File)).


test_logicmoo_ec_lps_reader(File):- test_logicmoo_ec_lps_reader(lps, File).
test_logicmoo_ec_lps_reader(Proc1,File):- load_e_lps_file(Proc1,File).

test_logicmoo_ec_lps_reader:- skip_tests, !.
test_logicmoo_ec_lps_reader:- 
 test_logicmoo_ec_lps_reader([ec('ecnet/Diving.e'), ec('foundations/*.e'), ec('ecnet/*.e')]).

test_logicmoo_ec_reader_2:- 
 test_logicmoo_ec_lps_reader(library('../test/ec_planner/*/*/*/*.e')).

test_lps_ereader:- skip_tests, !.
test_lps_ereader:- 
 convert_e(assert_ep(test_lps_mod),user_error,
 [ec('ecnet/Diving.e'), ec('foundations/*.e'), ec('ecnet/*.e'),
   library('../test/ec_planner/*/*/*/*.e')]).

get_time_arg(Holds1,T1):- compound_gt(Holds1,1),
   functor(Holds1,F,_),
   time_arg(F,N),
   arg(N,Holds1,T1),can_be_time_arg(T1),( var(T1); compound(T1)),!.

ep_to_lps_remove_time(FormI,Form):- bagof(Time,Sub^(sub_term(Sub,FormI),get_time_arg(Sub,Time)),TArgs),
  sort(TArgs,STArgs),  
  pprint_ecp_cmt(green,STArgs),
  ((STArgs=[Time],var(Time)) -> (remove_time_arg(Time,FormI,Form), \+ sub_var(Time,Form)) ; FormI=Form),!.
ep_to_lps_remove_time(FormI,FormI).

ep_to_lps(Form,Lps):- \+ compound(Form),!,Lps=Form.
ep_to_lps(Form,Lps):- already_lps(Form),!,Lps=Form.
ep_to_lps(t(Type,Inst),Lps):- atom(Type), M=..[Type,Inst],!,ep_to_lps(M,Lps).
ep_to_lps(FormI,LpsO):-
  ep_to_lps_remove_time(FormI,Form),
  ep_to_lps_arg(1,[],Form,Lps), ep_to_lps_arg(2,[],Lps,LpsO),!.

%ep_to_lps_pass2(Lps,LpsO):- subst(Lps,holds_at,holds,LpsO).
ep_to_lps_pass2(Lps,LpsO):- subst(Lps,holds_at,at,LpsO).
ep_to_lps_pass2(Lps,LpsO):- must_or_rtrace((fix_axiom_head(_Global,Lps,LpsM),over_pass(2,[],LpsM,LpsO))),!.

ep_to_lps_arg(_Pass,_Top,Form,Lps):- \+ compound(Form),!,Lps=Form.
ep_to_lps_arg(_Pass,_Top,Form,Lps):- already_lps(Form),!,Lps=Form.
%ep_to_lps_arg(Pass, Top,Form,Lps):- (over_pass(Pass,Top,Form,LpsM) -> Form\=@=LpsM),!,ep_to_lps_arg(Pass, Top,LpsM,Lps).
ep_to_lps_arg(Pass, Top,Form,Lps):- 
   compound_name_arguments(Form,F,Args),
   maplist(ep_to_lps_arg(Pass,[F|Top]),Args,ArgsO),
   compound_name_arguments_maybe_zero(LpsM,F,ArgsO),
   over_pass(Pass,Top,LpsM,Lps),!.

compound_name_arguments_maybe_zero(F,F,[]):- !.
compound_name_arguments_maybe_zero(LpsM,F,ArgsO):- compound_name_arguments(LpsM,F,ArgsO).

:- use_module(library(lps_syntax)).

over_pass_ex(_Top, X, _):- \+ compound(X),!,fail.
over_pass_ex(_Top,'->'(at(F1,T1),initiates(E,F2,T2)),Becomes):- T1==T2,  
   Becomes = (F1->initiates(E,F2)).
over_pass_ex(_Top,'->'(at(F1,T1),terminates(E,F2,T2)),Becomes):- T1==T2,  
  Becomes = (F1->terminates(E,F2)).
over_pass_ex(_Top,'->'(holds_at(F1,T1),initiates(E,F2,T2)),Becomes):- T1==T2,  
   Becomes = (F1->initiates(E,F2)).
over_pass_ex(_Top,'->'(holds_at(F1,T1),terminates(E,F2,T2)),Becomes):- T1==T2,  
  Becomes = (F1->terminates(E,F2)).

over_pass(_Pass,_Top, X, X):- \+ compound(X),!.
over_pass(_Pass,_Top, X, X):- functor(X,_,1), arg(1,X,Var), is_ftVar(Var),!.
over_pass(_Pass,_Top,at(X,Y),loc_at(X,Y)).
over_pass(_Pass,_Top,metreqs(X),X).

over_pass(2,Top, X, Y):- over_pass_ex(Top, X, Y),!.

% [waiter,agent,food,time]
% HoldsAt(BeWaiter1(waiter),time) ->
% Initiates(Order(agent,waiter,food),
%           BeWaiter2(waiter),
%           time).


over_pass(Pass,Top,neg(X),Rest):- ep_to_lps_arg(Pass,Top,not(X),Rest).
over_pass(2,_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==start, !.
over_pass(2,_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==0, !.
%over_pass(_Pass,_Top,holds_at(Fluent, Time),at(Fluent, Time)):- !.
over_pass(2,[],happens_at(Event,Time),(observe Event at Time)):- !.
over_pass(2,_Top,happens(Event,Time),(Event at Time)):- !.
% observe(from(muestra_del_general('+86-555000001'),to(2,3)))
over_pass(2,  [],initiates_at(Event,Fluent,Time),initiates(Event,Fluent)):- is_ftVar(Time), !.
over_pass(2,  [],terminates_at(Event,Fluent,Time),terminates(Event,Fluent)):- is_ftVar(Time), !.

over_pass(2,_Top,initiates_at(Event,Fluent,Time),(Event initiates Fluent at Time)):- !.
over_pass(2,_Top,terminates_at(Event,Fluent,Time),(Event terminates Fluent at Time)):- !.

over_pass(1,_Top, not(exists(_,X)), not(X)):-!.
%over_pass(_Pass,_Top, not(initially(X)),(initially not X)):-!.
%over_pass(_Pass,_Top, not(holds_at(X,T)),holds_at(not(X),T)).

over_pass(2,_Top, holds_at(Fluent, From, To),holds(Fluent, From, To)):- !.



%over_pass(_Pass,_Top,happensAt(Event,Time),at(observe(Event),Time)):- !.
over_pass(_Pass,_Top,Form,LpsO):- Form=..[EFP,X], argtype_pred(EFP,_), protify(EFP,X,Lps),!,flatten([Lps],LpsO).
over_pass(_Pass,_Top,X=Y,Lps):- callable(X),append_term(X,Y,Lps).
over_pass(Pass,Top,','(X1,X2),(Lps1,Lps2)):- over_pass(Pass,Top,X1,Lps1),over_pass(Pass,Top,X2,Lps2).
over_pass(Pass,Top,'<->'(X1,X2),[Lps1,Lps2]):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2), over_pass(Pass,Top,'->'(X1,X2),Lps1),over_pass(Pass,Top,'->'(X2,X1),Lps2).
over_pass(_Pass,_Top,'->'(X1,X2),(X2 if X1)):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2),!.
over_pass(_Pass,_Top,X1,X1):- simply_atomic(X1),!.
over_pass(_Pass,_Top,X1,false(Lps)):- \+ (X1 = false(_)), into_false_conj(X1,Lps),Lps\=not(_),!.
over_pass(_Pass,_Top,X,X):-!.

into_false_conj(X1,Lps):- \+ (X1 = false(_)), into_pnf_conj(X1,Lps) -> Lps\=not(_),simply_atomic_or_conj(Lps).
into_pnf_conj(X1,Lps):- pnf(X1,X2),nnf(X2,X3),conjuncts_to_list(X3,X3L),list_to_conjuncts(X3L,X4), Lps = X4.

removes_at(F,_):- sent_op_f(F),!,fail.
removes_at(F,F1):- atom_concat(F1,'_at',F),!.
%removes_at(F,F1):- F=F1.
remove_time_arg(_Time,Holds,Holds):- \+ compound_gt(Holds,0),!.
remove_time_arg(Time,Holds,HoldsMT):- \+ sub_var(Time,Holds),!,Holds=HoldsMT.
remove_time_arg(Time,not(Holds),not(HoldsMT)):-!, remove_time_arg(Time,Holds,HoldsMT).
remove_time_arg(Time,happens_at(Holds,T1),Holds):- T1==Time.
remove_time_arg(Time,holds_at(Holds,T1),Holds):- T1==Time.
remove_time_arg(Time,at(Holds,T1),Holds):- T1==Time.
remove_time_arg(_Time,Holds,Holds):- \+ compound_gt(Holds,1),!.
remove_time_arg(Time,Holds,HoldsMT):- Holds=..[F|Args],append(Left,[T1],Args),T1==Time,removes_at(F,F1),HoldsMT=..[F1|Left],!.
remove_time_arg(Time,Holds,HoldsMT):- Holds=..[F|Args],maplist(remove_time_arg(Time),Args,Left),HoldsMT=..[F|Left],!.

simply_atomic_or_conj(X1):- var(X1),!,fail.
simply_atomic_or_conj((X1,X2)):- !, simply_atomic_or_conj(X1),simply_atomic_or_conj(X2).
simply_atomic_or_conj(X1):- simply_atomic(X1).

simply_atomic(X1):- var(X1),!,fail.
simply_atomic(X1):- \+ compound_gt(X1,0),!.
simply_atomic(not(X1)):-!, simply_atomic(X1).
simply_atomic(at(X1,_)):-!, simply_atomic(X1).
simply_atomic((_;_)):- !, fail.
simply_atomic(X1):- compound_name_arguments(X1,F,Args), simply_atomic_f(F), maplist(simply_atomic_arg,Args).
simply_atomic_f(F):- \+ sent_op_f(F).

sent_op_f(F):- upcase_atom(F,FU),FU=F.

simply_atomic_arg(A):- var(A);simply_atomic(A). 

assert_lps_try_harder_now((X2 if X1),(if X1 then X2)):- simply_atomic_or_conj(X1), simply_atomic_or_conj(X2).
assert_lps_try_harder(Prolog):- assert_lps_try_harder_now(Prolog,Again),
  lps_xform(lps_test_mod,Again,PrologAgain),Again\==PrologAgain,!, 
   print_lps_syntax(yellow,Again),
   pprint_ecp_cmt(cyan,PrologAgain),
   pprint_ecp_cmt(white,"% ================================="),
   !.
assert_lps_try_harder(Prolog):- pprint_ecp(red,Prolog).

argtype_pred(event,events).
argtype_pred(fluent,fluents).
argtype_pred(action,actions).
argtype_pred(predicate,predicates).
argtype_pred(Action,Actions):- arg_info(domain,Action,arginfo),atom_concat(Action,"s",Actions).

protify(both,Form,[Lps1,Lps2]):- protify(events,Form,Lps1),protify(action,Form,Lps2).
protify(Type,Form,Lps):- is_list(Form),!,maplist(protify(Type),Form,Lps).
protify(Type,Form,Lps):- argtype_pred(Type,LPSType), \+ callable(Form),!,Lps=..[LPSType,[Form]].
protify(Type,Form,Lps):- argtype_pred(Type,LPSType), \+ compound(Form),!,Lps=..[LPSType,[Form/0]].
protify(Type,F/A, Lps):- argtype_pred(Type,LPSType), integer(A),!,Lps=..[LPSType,[F/A]].
protify(Type,(X1,X2),[Lps1,Lps2]):- !, protify(Type,X1,Lps1),protify(Type,X2,Lps2).
%protify(Type,X,Lps):- cfunctor(X,F,A),Lps=(F/A).
protify(Event,X,LPS):- ((event) == Event), compound(X), arg(1,X,Agent),
  is_agent(Agent),
  !,protify(both,X,LPS).
protify(Type,X,[mpred_prop(X,Type),LPS]):- argtype_pred(Type,LPSType),protify(LPSType,X,LPS).
protify(LPSType,X,LPS):- cfunctor(X,F,A),cfunctor(_Lps,F,A),!,Pred=..[LPSType,[F/A]],LPS=[Pred].

is_agent(Agent):- \+ atom(Agent),!,fail.
is_agent(diver).
is_agent(agent).
is_agent(Agent):- call_u(subsort(Agent,agent)),!.

