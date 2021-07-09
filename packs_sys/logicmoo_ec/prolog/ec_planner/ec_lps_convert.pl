%:- prolog_load_context(file, File), throw(wrong_file(File)).
% =========================================
% Goal/Plan translating
% =========================================
:- module(ec_lps_convert,[%load_e/1, needs_proccess/3,process_ec/2,fix_time_args/3,fix_goal/3, 
  %brk_on_bind/1,assert_axiom_2/2,
   assert_lps/1,
   test_logicmoo_ec_lps_reader/0,
   test_lps_ereader/0,
   assert_post/3,
   print_lps_syntax/2,
   test_logicmoo_ec_sanity/0,
   test_logicmoo_ec_reader_2/0,
   test_logicmoo_ec_lps_reader/2,
   test_logicmoo_ec_lps_reader/1]).

skip_tests:- fail. 

test_logicmoo_ec_sanity:- skip_tests,!. 
test_logicmoo_ec_sanity:- test_lps_ereader. 

:- use_module(library(logicmoo_common)).

:- use_module(library(ec_planner/ec_loader)).

/*export_transparent(P):-
  export(P),
  module_transparent(P).
*/
%:- use_module(library(logicmoo_lps)).
% system:local_database(X):- wdmsg(local_database(X)),fail.
%:- user:use_module(library('ec_planner/ec_planner_dmiles')).
:- use_module(library(ec_planner/ec_reader)).

:- use_module(library(lps_corner)).

:- set_prolog_flag(lps_translation_only_HIDE,false).
:- set_prolog_flag(lps_translation_only,false).


assert_lps(user:ec_current_domain_db2(Stuff,_)):-!,  assert_lps(Stuff).
assert_lps(axiom(Stuff,Nil)):- Nil==[],!,assert_lps(Stuff).
assert_lps(axiom(Stuff,List)):- !,must_or_rtrace(list_to_conjuncts(List,Body)),!,assert_lps(->(Body,Stuff)).
assert_lps(Stuff):- assert_ep4lps(db,Stuff).

print_tree_cmt(C,P):-
 echo_format('~N'),
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
already_lps(isa(_,_)):-!. 
already_lps(sort(_)):-!. 
already_lps(subsort(_,_)):-!.

atomic_or_var(Form):- ( \+ compound_gt(Form,0) ; Form='$VAR'(_); Form='$STRING'(_)),!.

get_1_vname(CVar,'$VAR'(Name)):- 
 source_variables_l(NamedVars),
 ignore((member(Name=Var,NamedVars), Var == CVar)),!.

vars_to_dvars(Term,CTerm):- 
  must_det_l((   
  term_variables(Term,Vars),must_maplist(get_1_vname,Vars,Names),
  copy_term(Term:Vars,CTerm:CVars),
  must_maplist(=,Names,CVars),
  nop(writeq(Term:Vars-->CTerm:CVars)))),!.


assert_ep4lps(Mod,Form):-
  vars_to_dvars(Form,FormO),!,
  %kif_unnumbervars(Form,FormO),
  %display(FormO),
  assert_ep(Mod,FormO),!.

assert_ep(Mod,Form):- is_list(Form), !, must_maplist(assert_ep(Mod),Form).
assert_ep( _M,Form):- print_tree_cmt(white,Form),fail.
assert_ep(Mod,Form):- already_lps(Form), !, assert_post(Mod,Form,Form),!.
assert_ep(Mod,Form):-
  format('~N',[]),
  must_or_rtrace(ep_to_lps(Form,Lps)),
  major_debug(ignore((Form\==Lps->pprint_ecp_cmt(hfg(green),Form)))),
  must_or_rtrace(assert_post(Mod,Form,Lps)),!.


must_or_rtrace_l((A,B)):- !, must_or_rtrace_l(A),!, must_or_rtrace_l(B).
must_or_rtrace_l((C->A;B)):- !, (C-> must_or_rtrace_l(A);must_or_rtrace_l(B)).
must_or_rtrace_l(A):- must_or_rtrace(A),!.

%assert_post(Mod,_,include(F)):- include_e_lps_file_now(Type,Mod:F).
%assert_post(Mod,_,load(F)):- include_e_lps_file_now(Type,Mod:F). 
%assert_post(Mod,_,include(F)):- !, with_e_file(assert_ep4lps(Mod),current_output, [ec(F)]). 
%assert_post(Mod,_,load(X)):- nop(assert_ep(Mod,include(X))),!.
assert_post(Mod,Form,Lps):- is_list(Lps),!, maplist(assert_post(Mod,Form),Lps).
%assert_post(Mod,t(Type,Inst),_):- atom(Type), M=..[Type,Inst],!,assert_post(Mod,M,M),!.
assert_post(Mod,_Form,Lps):- already_lps(Lps),!,print_lps_syntax(yellow,Lps),must_or_rtrace(assert_pl(Mod,Lps)).
assert_post(Mod,_Form,Lps0):- 
 must_or_rtrace_l((
  last_lps_pass(Lps0,Lps),
  lps_xform_ec(Mod,Lps,Prolog),
  must_or_rtrace_l((Lps\==Prolog->
   ((ignore(( /*(Form\==Prolog,Lps==Prolog)-> */
    print_lps_syntax(yellow,Lps),
     nop(pprint_ecp(yellow,Lps)))),
    pprint_ecp_cmt(cyan,Prolog),pprint_ecp_cmt(white,"% =================================")))
   ;assert_lps_try_harder(Prolog))),
  must_or_rtrace(assert_pl(Mod,Prolog)))),!.

assert_post(Mod,_Form,Lps):- dumpST, must_or_rtrace(assert_pl(Mod,Lps)).


print_lps_syntax(Color,Lps):- 
 \+ \+ notrace((with_lps_operators2(user,
  ec_lps_convert:with_lps_operators2(pretty_clauses,pretty_clauses:clause_to_string(Lps,S))),
    real_ansi_format(hfg(Color), '~N~s.~N', [S]))),!.

unarity_zero(I,O):- map_nonvars(p,unarity_zero_c,I,O).
unarity_zero_c(C,CC):- compound(C),compound_name_arity(C,CC,0).


lps_xform_ec(Mod,Lps0,Prolog):-
 must_or_rtrace_l((
 unarity_zero(Lps0,Lps1),
 unnumbervars(Lps1,Lps),
 % =(Lps0,Lps),
 with_vars_relocked((
 with_vars_locked((
 locally(current_prolog_flag(lps_translation_only_HIDE,true),
   locally(t_l:is_lps_program_module(Mod),
    (unlock_vars(Prolog),
    % on_x_fail
    (lps_term_expander:lps_f_term_expansion_now(Mod,Lps,Prolog))))))))))),!.
%lps_xform_ec(_Mod,Prolog,Prolog):-!.

:- export_transparent(with_lps_operators2/2).
with_lps_operators2(M,Goal):- 
   setup_call_cleanup(push_operators(M:[op(900, fy, M:not),  op(1200, xfx, M:then), op(1185, fx, M:if), op(1190, xfx, M:if), op(1100, xfy, M:else), op(1050, xfx, M:terminates), op(1050, xfx, M:initiates), op(1050, xfx, M:updates), op(1050, fx, M:observe), op(1050, fx, M:false), op(1050, fx, M:initially), op(1050, fx, M:fluents), op(1050, fx, M:events), op(1050, fx, M:prolog_events), op(1050, fx, M:actions), op(1050, fx, M:unserializable), op(999, fx, M:update), op(999, fx, M:initiate), op(999, fx, M:terminate), op(997, xfx, M:in), op(995, xfx, M:at), op(995, xfx, M:during), op(995, xfx, M:from), op(994, xfx, M:to), op(1050, xfy, M: ::), op(1200, xfx, M:(<-)), op(1050, fx, M:(<-)), op(700, xfx, M: <=)],Undo),
     M:call(Goal),pop_operators(Undo)).

:- export_transparent(with_lps_operators/1).
with_lps_operators(MGoal):- 
  strip_module(MGoal,M,Goal),
  with_lps_operators2(user,ec_lps_convert:with_lps_operators2(M,M:Goal)).


assert_pl(Mod,Prolog):- is_list(Prolog),!, must_maplist(assert_pl(Mod),Prolog).
assert_pl(Mod,(P1,P2)):- !,assert_pl(Mod,P1),assert_pl(Mod,P2).
assert_pl(_Mod,Prolog):- % get_source_location(File,Line),
   major_debug(pprint_ecp_pl(yellow,Prolog)).

include_e_lps_file_now(Type,MFile):- strip_module(MFile,M,File), include_e_lps_file_now(Type,M,File).
include_e_lps_file_now(Type,M,File):- absolute_file_name(File,AbsFile),File\==AbsFile,exists_file(AbsFile), !,include_e_lps_file_now(Type,M,AbsFile).

include_e_lps_file_now(Type,Mod,File):- 
   translate_e_to_filetype(Mod:Type,File).

load_e_lps_file(Type,File):- retractall(etmp:ec_option(load(_), _)), include_e_lps_file(Type,File).
  
include_e_lps_file(Type,File):- is_list(File), !, must_maplist(include_e_lps_file(Type),File).
include_e_lps_file(Type,File):- wdmsg(include_e_lps_file(Type,File)),fail.
include_e_lps_file(Type,File):- quietly_needs_resolve_local_files(File,Resolved),!,include_e_lps_file(Type,Resolved).
include_e_lps_file(Type,File):- absolute_file_name(File,DB), exists_file(DB),!, 
  update_changed_files,   
  strip_module(_,M,_), prolog_statistics:time(M:include_e_lps_file_now(Type,File)),!.
include_e_lps_file(Type,File):- throw(with_abs_paths(include_e_lps_file(Type),File)).


test_logicmoo_ec_lps_reader(File):- test_logicmoo_ec_lps_reader(lps, File), !.
test_logicmoo_ec_lps_reader(Proc1,File):- load_e_lps_file(Proc1,File).

test_logicmoo_ec_lps_reader:- skip_tests, !.
test_logicmoo_ec_lps_reader:- 
 test_logicmoo_ec_lps_reader([ec('ecnet/Diving.e'), ec('foundations/*.e'), ec('ecnet/*.e')]).

test_logicmoo_ec_reader_1:- 
 test_logicmoo_ec_lps_reader([ec('ecnet/Diving.e')]).

test_logicmoo_ec_reader_2:- 
 test_logicmoo_ec_lps_reader(library('../test/ec_planner/*/*/*/*.e')).

test_lps_ereader:- skip_tests, !.
test_lps_ereader:- 
 convert_e(assert_ep4lps(db),user_error,
 [ec('ecnet/Diving.e'), ec('foundations/*.e'), ec('ecnet/*.e'),
   library('../test/ec_planner/*/*/*/*.e')]).

get_time_arg(Holds1,T1):- compound_gt(Holds1,1),
   functor(Holds1,F,_),
   time_arg(F,N),
   arg(N,Holds1,T1),can_be_time_arg(T1),( var(T1); compound(T1)),!.

ep_to_lps_remove_time(FormI,Form):- fail,
  bagof(Time,Sub^(sub_term(Sub,FormI),get_time_arg(Sub,Time)),TArgs),
  sort(TArgs,STArgs),  
  pprint_ecp_cmt(green,STArgs),
  ((STArgs=[Time],is_ftVar(Time)) -> (remove_time_arg(Time,FormI,Form), \+ sub_var(Time,Form)) ; FormI=Form),!.
ep_to_lps_remove_time(FormI,FormI).


correct_builtin_at_names(F1,F2):- map_nonvars(p,rename_at_bis,F1,F2).
rename_at_bis(neg(F), not(F1)):- compound(F),!,correct_builtin_at_names(F,F1).
rename_at_bis(at(X,Y), at_place(X,Y)):-!.
rename_at_bis(false(F), not(F1)):- compound(F),!,correct_builtin_at_names(F,F1).
rename_at_bis(F, F1):- atom(F), atom_concat(F1,'_at',F), builtin_pred(F1).

ep_to_lps(Form,Lps):- atomic_or_var(Form),!,Lps=Form.
ep_to_lps(t(Type,Inst),Lps):- !, ep_to_lps(isa(Inst,Type),Lps).
ep_to_lps(Form,Lps):- already_lps(Form),!,Lps=Form.
%ep_to_lps(t(Type,Inst),Lps):- atom(Type), M=..[Type,Inst],!,ep_to_lps(M,Lps).
ep_to_lps(FormI,LpsO):-
  correct_builtin_at_names(FormI,FormII),
  ep_to_lps_remove_time(FormII,Form),
  ep_to_lps_arg(1,[],Form,Lps), 
  ep_to_lps_arg(2,[],Lps,Lps2),
  ep_to_lps_arg(3,[],Lps2,LpsO),!.


last_lps_pass(Form,Lps):- 
  ep_to_lps_arg(3,[],Form,Lps), !.


ep_to_lps_arg(_Pass,_Top,Form,Lps):- atomic_or_var(Form),!,Lps=Form.
ep_to_lps_arg(_Pass,_Top,Form,Lps):- already_lps(Form),!,Lps=Form.
ep_to_lps_arg(Pass, Top,Form,Lps):- (over_pass(Pass,Top,Form,LpsM) -> Form\=@=LpsM),!,ep_to_lps_arg(Pass, Top,LpsM,Lps).
ep_to_lps_arg(Pass, Top, Form,Lps):- 
   compound_name_arguments(Form,F,Args),
   must_maplist(ep_to_lps_arg(Pass,[F|Top]),Args,ArgsO),
   compound_name_arguments_maybe_zero(LpsM,F,ArgsO),
   over_pass(Pass,Top,LpsM,Lps),!.

compound_name_arguments_maybe_zero(F,F,[]):- \+ compound(F), !.
compound_name_arguments_maybe_zero(LpsM,F,ArgsO):- (compound(LpsM);atom(F)),!,compound_name_arguments(LpsM,F,ArgsO),!.
%compound_name_arguments_maybe_zero(LpsM,F,ArgsO):- dumpST,break.

map_nonvars(_,_Pred2,Data,DData):- is_ftVar(Data), !, DData = Data.
map_nonvars(O, Pred2,Data,DData):- once(call(Pred2,Data,MData)), Data\=@=MData,!, map_nonvars(O, Pred2,MData,DData).
map_nonvars(_,_Pred2,Data,DData):- atomic_or_var(Data), !, DData=Data.
map_nonvars(p, Pred2,Data,DData):- 
  compound_name_arguments(Data,F,ARGS), !, 
  maplist(map_nonvars(p,Pred2),[F|ARGS],[DF|DARGS]),!,
  compound_name_arguments(DData,DF,DARGS).
map_nonvars(O, Pred2,Data,DData):- 
  compound_name_arguments(Data,F,ARGS), !, 
  maplist(map_nonvars(O,Pred2),ARGS,DARGS),!,
  compound_name_arguments(DData,F,DARGS).
  

:- use_module(library(lps_syntax)).

final_top_pass( X, _):- atomic_or_var(X),!,fail.

final_top_pass('->'(at(F1,T1),initiates(E,F2,T2)),Becomes):- T1==T2,  
   Becomes = (F1->initiates(E,F2)).
final_top_pass('->'(at(F1,T1),terminates(E,F2,T2)),Becomes):- T1==T2,  
  Becomes = (F1->terminates(E,F2)).
final_top_pass('->'(holds(F1,T1),initiates(E,F2,T2)),Becomes):- T1==T2,  
   Becomes = (F1->initiates(E,F2)).
final_top_pass('->'(holds(F1,T1),terminates(E,F2,T2)),Becomes):- T1==T2,  
  Becomes = (F1->terminates(E,F2)).

% [waiter,agent,food,time]
% HoldsAt(BeWaiter1(waiter),time) ->
% Initiates(Order(agent,waiter,food),
%           BeWaiter2(waiter),
%           time).

join_conj(Lps1,Lps2,Out):- ands_to_list(Lps1,List1),ands_to_list(Lps2,List2),append(List1,List2,List), list_to_conjuncts(List,Out).
%over_pass(3,_,holds(X,T),at(X,T)).


over_pass(Pass,Top,neg(X),Rest):- over_pass(Pass,Top,not(X),Rest).
over_pass(_Pass,_Top, X, X):- atomic_or_var(X),!.
over_pass(_Pass,_Top, X, X):- functor(X,_,1), arg(1,X,Var), is_ftVar(Var),!.
over_pass(_Pass,_Top,metreqs(X),X).
% over_pass(3,[], X, Y):- final_top_pass(X, Y),!.

over_pass(2,[],not(initially(P)),initially(not(P))):- !.
over_pass(2,[],holds(Fluent, Time),initially(Fluent)):- nonvar(Time), get_zero(Time), !.
over_pass(2,[],not(holds(Fluent, Time)),initially(not(Fluent))):- nonvar(Time), get_zero(Time), !.
over_pass(2,[],at(Fluent, Time),initially(Fluent)):- nonvar(Time), get_zero(Time), !.
%over_pass(_Pass,_Top,holds(Fluent, Time),at(Fluent, Time)):- !.
over_pass(2,[],  happens(Event,Time),(observe Event at Time)):- !.
over_pass(2,[],  happens(Event,Time1,Time2),(observe Event from Time1 to Time2)):- !.
% over_pass(2,_Top,happens(Event,Time),(Event at Time)):- !.
% observe(from(muestra_del_general('+86-555000001'),to(2,3)))

over_pass(2,  [],initiates(Event,Fluent,Time), initiates(Event,Fluent)):- is_ftVar(Time), !.
over_pass(2,  [],terminates(Event,Fluent,Time),terminates(Event,Fluent)):- is_ftVar(Time), !.
over_pass(2,  [],releases(Event,Fluent,Time),  releases(Event,Fluent)):- is_ftVar(Time), !.
%over_pass(2,_Top,initiates(Event,Fluent,Time), (Event initiates Fluent at Time)):- !.
%over_pass(2,_Top,terminates(Event,Fluent,Time),(Event terminates Fluent at Time)):- !.
%over_pass(2,_Top,releases(Event,Fluent,Time),(Event terminates Fluent at Time)):- !.

over_pass(Pass,Top,Holds->initiates(Event,Fluent,Time), (Event initiates Fluent at Time if HoldsA)):-  over_pass(Pass,Top,Holds,HoldsA), !.
over_pass(Pass,Top,Holds->terminates(Event,Fluent,Time),(Event terminates Fluent at Time if HoldsA)):-  over_pass(Pass,Top,Holds,HoldsA), !.
over_pass(Pass,Top,Holds->releases(Event,Fluent,Time),(Event terminates Fluent at Time if HoldsA)):-  over_pass(Pass,Top,Holds,HoldsA), !.

over_pass(_,_Top, not(exists(_,X)), not(X)):-!.
over_pass(_Pass,_Top, not(initially(X)),(initially not X)):-!.
over_pass(_Pass,_Top, at(not(X),T),holds(not(X),T)).
over_pass(_Pass,_Top, not(at(X,T)),holds(not(X),T)).
over_pass(_Pass,_Top, not(holds(X,T)),holds(not(X),T)).
over_pass(_Pass,_Top, happens(not(X),T),not(happens(X,T))).
over_pass(_Pass,_Top, happens(not(X),T1,T2),not(happens(X,T1,T2))).
over_pass(_,_Top,':-'(X1,X2),(X2 -> X1)):-!.

%over_pass(2,_Top, holds(Fluent, From, To),holds(Fluent, From, To)):- !.
%over_pass(_Pass,_Top,happensAt(Event,Time),at(observe(Event),Time)):- !.

over_pass(_Pass,_Top,Form,OO):- Form=..[EFP,X], needs_protify(EFP,_), protify(EFP,X,Lps),!,flatten([Lps],LpsO),unlistify(LpsO,OO).
over_pass(_Pass,_Top,X=Y,Lps):- \+ atomic_or_var(X), append_term(X,Y,Lps).

over_pass(Pass,Top,(X1,X2), Out):- over_pass(Pass,Top,X1,Lps1),over_pass(Pass,Top,X2,Lps2),
  join_conj(Lps1,Lps2,Out).

over_pass(Pass,Top,[X1|X2], Out):- over_pass(Pass,Top,X1,Lps1),over_pass(Pass,Top,X2,Lps2),
  join_conj(Lps1,Lps2,Out).


%over_pass(Pass,_Top,';'(not(X2);X1),if(X2,X1)):- !.
over_pass(_Pass,[],(A;B),if(AA, BB)):- clausify_pnf_conv(B,BB),clausify_pnf_conv(not(A),AA),simply_atomic(BB).


over_pass(Pass,Top,exists(Stuff,'<->'(X1,X2)),[Lps1,Lps2]):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2), over_pass(Pass,Top,'->'(X1,exists(Stuff,X2)),Lps1),over_pass(Pass,Top,'->'(X2,exists(Stuff,X1)),Lps2).
over_pass(Pass,Top,'<->'(X1,X2),[Lps1,Lps2]):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2), over_pass(Pass,Top,'->'(X1,X2),Lps1),over_pass(Pass,Top,'->'(X2,X1),Lps2).

over_pass(2,_Top,'->'(X1,X2),(X2 if X1)):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2),!.
over_pass(_Pass,_Top,'->'(X1,X2),(X2 if X1)):- simply_atomic(X1),simply_atomic_or_conj(X2),!.
over_pass(3,_Top,'->'(X1,X2),(X2 if X1)).

over_pass(Pass,Top, In, Out) :- Top==[], clausify_pnf_conv(In,Mid)-> \+ sub_var(Mid,In), !, over_pass(Pass,Top, Mid, Out).
over_pass(_Pass,_Top,X1,X1):- simply_atomic(X1),!.
over_pass(_Pass,[],X1,Lps):- \+ (X1 = false(_)), into_false_conj(X1,Lps),Lps\=not(_),!.
over_pass(_Pass,_Top,X,X):-!.

clausify_pnf_conv(In,OO):- is_list(In),!,maplist(clausify_pnf_conv,In,Mid),flatten([Mid],MidM),list_to_set(MidM,MidO),unlistify(MidO,OO).
clausify_pnf_conv(cl(A,B),cl(A,B)):- !.
clausify_pnf_conv(In,MidO):-   
  expandQuants(KB,In,In2),
  un_quant3(KB,In2,In3),
  thereExists_to_exists(In3,In4),!,
  clausify_pnf_v1(In4,MidM),!,
  list_to_set(MidM,MidO),!.

thereExists_to_exists(In3,In4):- map_nonvars(p,thereExists1_to_exists,In3,In4).
thereExists1_to_exists(thereExists,exists).
thereExists1_to_exists(forAll,all).
thereExists1_to_exists(forall,all).

negate_to_false(Lps,Out):- Lps=not(Neg), Out=Neg, !.
negate_to_false(Lps,Out):- simply_atomic_or_conj(Lps), !, Out=false(Lps),!.
% negate_to_false(A,A):-!.

into_false_conj(X1,Out):- \+ (X1 = false(_)), into_pnf_conj(X1,Lps) -> negate_to_false(Lps,Out). %,simply_atomic_or_conj(Lps).
%into_false_conj((not(A);B),if(A, B)):- nonvar(A). % clausify_pnf_conv(B,BB),clausify_pnf_conv(not(A),AA).
%into_false_conj((B;not(A)),if(A, B)):- nonvar(A). % clausify_pnf_conv(B,BB),clausify_pnf_conv(not(A),AA).
into_pnf_conj(X0,Lps):- clausify_pnf_conv(X0,X1),pnf(X1,X2),nnf(X2,X3),conjuncts_to_list(X3,X3L),list_to_conjuncts(X3L,X4), Lps = X4.





%removes_at(F,F1):- F=F1.
remove_time_arg(_Time,Holds,Holds):- atomic_or_var(Holds), !.
remove_time_arg(Time,Holds,HoldsMT):- \+ sub_var(Time,Holds),!,Holds=HoldsMT.
remove_time_arg(Time,not(Holds),not(HoldsMT)):-!, remove_time_arg(Time,Holds,HoldsMT).
remove_time_arg(_Time,Holds,Holds):- \+ compound_gt(Holds,1),!.
remove_time_arg(Time,happens(Holds,T1),Holds):- T1==Time.
remove_time_arg(Time,happens(Holds,T1,T2),Holds):- T1==Time, T2==T1.
remove_time_arg(Time,holds(Holds,T1),Holds):- T1==Time.
remove_time_arg(Time,at(Holds,T1),Holds):- T1==Time.
%remove_time_arg(Time,Holds,HoldsMT):- Holds=..[F|Args],append(Left,[T1],Args),T1==Time,removes_at(F,F1),HoldsMT=..[F1|Left],!.
remove_time_arg(Time,Holds,HoldsMT):- Holds=..[F|Args],must_maplist(remove_time_arg(Time),Args,Left),HoldsMT=..[F|Left],!.
%removes_at(F,_):- sent_op_f(F),!,fail.

simply_atomic_or_conj(X1):- var(X1),!,fail.
simply_atomic_or_conj((X1,X2)):- !, simply_atomic_or_conj(X1),simply_atomic_or_conj(X2).
simply_atomic_or_conj(X1):- simply_atomic(X1).

simply_atomic(X1):- is_ftVar(X1),!,fail.
simply_atomic(X1):- atomic_or_var(X1),!.
simply_atomic(not(X1)):-!, simply_atomic(X1).
simply_atomic(at(X1,_)):-!, simply_atomic(X1).
simply_atomic((_;_)):- !, fail.
simply_atomic(X1):- compound_name_arguments(X1,F,Args), simply_atomic_f(F), maplist(simply_atomic_arg,Args).
simply_atomic_f(F):- \+ sent_op_f(F).

sent_op_f(F):- upcase_atom(F,FU),FU=F.

simply_atomic_arg(A):- once(var(A);simply_atomic(A)). 

assert_lps_try_harder_now(X1,X1):- !. %@todo unworkarround
assert_lps_try_harder_now((X2 if X1),(if X1 then X2)):- simply_atomic_or_conj(X1), simply_atomic(X2).
assert_lps_try_harder_now((X2 if X1),(if X1 then X2)):- simply_atomic_or_conj(X1), simply_atomic_or_conj(X2).
assert_lps_try_harder(Prolog):- assert_lps_try_harder_now(Prolog,Again),
  lps_xform_ec(db,Again,PrologAgain),Again\==PrologAgain,!, 
   print_lps_syntax(yellow,Again),
   pprint_ecp_cmt(cyan,PrologAgain),
   pprint_ecp_cmt(white,"% ================================="),
   !.
assert_lps_try_harder(Prolog):- pprint_ecp(red,Prolog),!.

needs_protify(event,events).
needs_protify(fluent,fluents).
needs_protify(action,actions).
needs_protify(predicate,predicates).
needs_protify(Action,Actions):- arg_info(domain,Action,arginfo),atom_concat(Action,"s",Actions).

protify(both,Form,[Lps1,Lps2]):- protify(events,Form,Lps1),protify(action,Form,Lps2).
protify(Type,Form,Lps):- is_list(Form),!,must_maplist(protify(Type),Form,Lps).
protify(Type,Form,Lps):- needs_protify(Type,LPSType), \+ callable(Form),!,Lps=..[LPSType,[Form]].
protify(Type,Form,Lps):- needs_protify(Type,LPSType), \+ compound(Form),!,Lps=..[LPSType,[Form/0]].
protify(Type,F/A, Lps):- needs_protify(Type,LPSType), integer(A),!,Lps=..[LPSType,[F/A]].
protify(Type,(X1,X2),[Lps1,Lps2]):- !, protify(Type,X1,Lps1),protify(Type,X2,Lps2).
%protify(Type,X,Lps):- cfunctor(X,F,A),Lps=(F/A).
protify(Event,X,LPS):- ((event) == Event), compound(X), arg(1,X,Agent),
  is_agent_ec(Agent),
  !,protify(both,X,LPS).
protify(Type,X,[mpred_prop(X,Type),LPS]):- needs_protify(Type,LPSType),protify(LPSType,X,LPS).
protify(LPSType,X,LPS):- cfunctor(X,F,A),cfunctor(_Lps,F,A),!,Pred=..[LPSType,[F/A]],LPS=[Pred].

is_agent_ec(Agent):- \+ atom(Agent),!,fail.
is_agent_ec(diver).
is_agent_ec(agent).
is_agent_ec(object):- !, fail.
is_agent_ec(Agent):- notrace(catch(call_u(subsort(Agent,agent)),_,fail)),!.

:- fixup_exports.


