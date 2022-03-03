% :- module(unity_prolog,[]).
:- dynamic(tmpu:(is_unity_file/1)).
:- dynamic(unity_module_name/1).

:- set_prolog_flag(occurs_check,error).

make_pred_narity_call_list(P,A):- 
  length(L,A),
  Head=..[P|L],Body=..[P,L],
  assert((Head:-Body)).

begin(G):- is_list(G),!,maplist(begin,G).
begin(G):- expand_ugoal(G,GG),call(GG).

:- forall(between(2,11,A), make_pred_narity_call_list(begin,A)).
displayln(X):- is_list(X),!,maplist(write,X),nl.
displayln(X):- write(X),nl.
:- forall(between(2,7,A), make_pred_narity_call_list(displayln,A)).

:- arithmetic_function('$'/1).
:- arithmetic_function('now'/0).

:- op(1100,fx,@),
   op(1100,xfy,:=),
   op(1100,xfy,:^).

:- multifile(menu_action/2).

:- op(800,fx,'/').
:- op(900,xfx,'::').
:- op(399,xfy,':').

string_representation(Term,String):- term_to_string(Term,String).

:- op(399,fx,'~').

%:- op(1000,fx,('$')).
%:- multifile (/)/1.
:- multifile  '~' / 1.
%:- multifile  :: / 2.

:- multifile slash_db/2.
:- dynamic slash_db/2.

:- discontiguous (':=')/2.
:- discontiguous  '~' / 1.

swi_or_unity(G,_U):- call(G).

%:- discontiguous(ugoal_expansion/2).


db_pred(assert).
db_pred(retract).
db_pred(retractall).
db_pred(asserta).
db_pred(assertz).
db_pred(clause).
db_pred(abolish).
db_pred(assert_if_new).
db_pred(asserta_if_new).
db_pred(assertz_if_new).
db_pred(retract_all).


% ugoal_expansion(X=Y,unity_call(X=Y)).
:- meta_predicate(system:unity_call(:)).
system:unity_call(M:G):- G=..[F|Args],unity_apply(M:F,Args).

:- meta_predicate(system:unity_apply(:,+)).
system:unity_apply(F,[Arg]):- once(convert_assert(Arg,Slash)),Arg\==Slash,!, unity_apply(F,[Slash]).
system:unity_apply(M:(public),[Arg]):- !, system:unity_apply(M:(external),[Arg]).
system:unity_apply(F,Args):- \+ ground(Args), log(unity_apply(F,Args)), fail.
system:unity_apply(F,Args):- apply(F,Args).


convert_assert(G,G):- var(G),!.
convert_assert(::(G,Arg),Slash):- G == $global,!, convert_assert(Arg,Slash).
convert_assert(X,X):- \+ \+ X = slash_db(_,_),!.
convert_assert(Arg,Slash):-  maybe_into_slash_db(Arg,Slash),!.
convert_assert(X,Y):- compound(X),
  compound_name_arguments(X,F,AX),
  maplist(convert_assert,AX,AY),
  compound_name_arguments(Y,F,AY).
convert_assert(X,X).

:- system:import(convert_assert/2).

maybe_into_slash_db(C,_):- \+ compound(C),!,fail.
maybe_into_slash_db(slash_db(_,_),_):- !,fail.
%maybe_into_slash_db(F/A,_):- atom(F),integer(A), !,fail.
maybe_into_slash_db(F/_,_):- \+ slash_arg(F),!,fail.
maybe_into_slash_db(_/A,_):- \+ slash_arg(A),!,fail.
maybe_into_slash_db(C,slash_db(top,C)):- \+ \+ (numbervars(C,0,_,[attvar(bind)]),is_slashed_g(C)).
maybe_into_slash_db(C,slash_db(rel,C)):- \+ \+ (numbervars(C,0,_,[attvar(bind)]),is_slashed(C)).

slash_arg(E):- var(E),!.
slash_arg(E):- atom(E),!.
slash_arg(E):- compound(E),functor(E,F,A),slash_arg_f(F,A).
slash_arg_f($,1).
slash_arg_f(/,2).
slash_arg_f(/,1).
slash_arg_f(:,2).

is_slashed(_/_).
is_slashed(_/_:_).
is_slashed_g(/_/_).
is_slashed_g(/_:_).
is_slashed_g((/_)/_).
is_slashed_g((/_):_).

%slash_db(X,Y):- log(slash_db(X,Y)),fail.
%slash_db(top,_X):- fail.
slash_db(rel,X):- slash_db(top,/X).
slash_db(rel,X):- slash_db(top,/_/X).
slash_db(rel,X):- slash_db(top,/_/_/X).
 
expand_uarg(X,Y):- uarg_exp(X,Y),!.
expand_uarg(X,X).

uarg_exp(C,O):- no_more_expansion(C,O),!.
uarg_exp(is(X,Y),is(X,Y)):-!.
uarg_exp(X,Y):- maybe_into_slash_db(X,Y),!.
%uarg_exp(X:-B,Y:-B):- maybe_into_slash_db(X,Y),!.
%uarg_exp(X,Y):- compound(X),once(ugoal_expansion(X,Y)),X\==Y.

%uarg_exp(G::X,G::Y):- !,expand_uarg(X,Y).
uarg_exp(G::X,Y):- G == $global,!,expand_uarg(X,Y).
uarg_exp(G::X,M:Y):- var(G),G=M,!,expand_uarg(X,Y).

uarg_exp(G:X,M:Y):- G=M,!,expand_uarg(X,Y).
%uarg_exp(::(G,X),M:Y):- nonvar(G),G='$'(M),uarg_exp(X,Y).
%uarg_exp(G::X,G{}.X):- !.
%uarg_exp($Kavi,K{k:K,name:Kavi}).
uarg_exp(X,Y):- compound(X),!,
  compound_name_arguments(X,F,AX),
  maplist(expand_uarg,AX,AY),
  functor_expansion(F,FF),
  compound_name_arguments(Y,FF,AY).

% functor_expansion('::',':').
functor_expansion(F,F).

dv(E,V):-compound(E), E='$'(V), V \== global.
%uclause_expansion_inner(hb,H,B,HHBB):- sub_term(E,H),dv(E,V),subst(H,E,Var,HH),!,
%  conjoin(must_getvar(V,Var),B,BB),uclause_expansion_inner(hb,HH,BB,HHBB).
%uclause_expansion_inner(hb,H,B,HHBB):- sub_term(E,B),dv(E,V),subst(B,E,Var,BB),!,
%  conjoin(must_getvar(V,Var),BB,BBB),uclause_expansion_inner(hb,H,BBB,HHBB).
uclause_expansion_inner(hb,H,B,HB):-  as_clause_hb(H,B,HB).

uclause_expansion_inner(dcg,H,B,HHBB):- sub_term(E,H),dv(E,V),subst(H,E,Var,HH),!,
  conjoin({must_getvar(V,Var)},B,BB),uclause_expansion_inner(dcg,HH,BB,HHBB).
uclause_expansion_inner(dcg,H,B,HHBB):- sub_term(E,B),dv(E,V),subst(B,E,Var,BB),!,
  conjoin({must_getvar(V,Var)},BB,BBB),uclause_expansion_inner(dcg,H,BBB,HHBB).
uclause_expansion_inner(dcg,H,B,H-->B).

uclause_expansion(T,H,B,HHBB):- uclause_expansion_inner(T,H,B,HB),expand_uarg(HB,HHBB).

typed_uclause_expansion(T,H,B,HHBBO):-
  expand_ugoal(B,BB1),expand_ugoal(BB1,BB),
  uclause_expansion_inner(T,H,BB,HB),
  expand_uarg(HB,HHBB),
  uctx_corrections(HHBB,HHBBO).

uctx_corrections(H,H):- \+ compound(H), !.
uctx_corrections(H:-B,HB):- !, as_clause_hb(H,B,HB).
uctx_corrections(H,CH):- as_clause_head(H,CH).

as_clause_hb($Ctx::H,B,((H:- ( must_getvar(Ctx,V), if_uctx_equals(V), in_uctx(V,BB))))):- nonvar(Ctx),!, expand_ugoal(B,BB).
as_clause_hb(Ctx::H,B,((H:- ( if_uctx_equals(Ctx), in_uctx(Ctx,BB))))):-  expand_ugoal(B,BB).
as_clause_hb(H,B,Out):- B == true,!,as_clause_head(H,Out).
%as_clause_hb(H,B,(:-BB)):- H==goal,!, expand_ugoal(B,BB).
as_clause_hb(H,B,(HH:-BBB)):- get_var_expansions(H,HH,G),expand_ugoal(B,BB),conjoin(G,BB,BBB).


as_clause_head(H,Out):-get_var_expansions(H,HH,G),(G==true -> Out = H ; Out = (HH:- G)).

:- thread_local(t_l:(unity_ctx/1)).
in_uctx(M,G):- locally(t_l:unity_ctx(M), M:call(G)).
if_uctx_equals(T):-  t_l:unity_ctx(M),!,T==M.

uterm_expansion(X,_):- prolog_load_context(term,T),T\==X,!,fail.
uterm_expansion(X,Y):- expand_uterm(X,Y).

expand_uterm(C,O):- no_more_expansion(C,O),!.
expand_uterm(:-B,:-BB):- !, expand_ugoal(B,BB).
expand_uterm(H:-B,HB):- !, typed_uclause_expansion(hb,H,B,HB).
expand_uterm(H-->B,HB):- !, typed_uclause_expansion(dcg,H,B,HB).
expand_uterm(H,HB):- typed_uclause_expansion(hb,H,true,HB),!.
%expand_uterm(H,HB):- expand_uarg(H,HB).

meta_pred_style(Cmp,Meta):- compound(Cmp),compound_name_arity(Cmp,F,A), \+ never_meta_fa(F,A),
  predicate_property(Cmp,meta_predicate(Meta)),!.

never_meta_fa('/',_).

%meta_pred_style(Cmp,Meta):- predicate_property(Cmp,transparent),!,functor(Cmp,F,A),functor(Meta,F,A).

/*
ugoal_expansion_3(V,A,A):- var(V),!,shouldnt_need_expansion(A).
ugoal_expansion_3(+,A,A):- shouldnt_need_expansion(A).
ugoal_expansion_3(-,A,A):- shouldnt_need_expansion(A).
ugoal_expansion_3(?,A,A):- shouldnt_need_expansion(A).
*/
ugoal_expansion_3(_,X,Y):- expand_ugoal(X,Y).

expand_ugoal(C,O):- no_more_expansion(C,O),!.
expand_ugoal(X,Y):- ugoal_expansion(X,XY),!,expand_uarg(XY,Y).
expand_ugoal(X,Y):- expand_uarg(X,Y).

shouldnt_need_expansion(_):- !.

ugoal_expansion(C,O):- no_more_expansion(C,O),!.
ugoal_expansion(G,BArgs):- compound_name_arguments(G,B,Args),expand_f_args_to_list(B),Args\=[_],!,BArgs=..[B|Args].
ugoal_expansion(G::X,in_uctx(X,Y)):- G == $global,!,ugoal_expansion(X,Y).

ugoal_expansion(unity_db_call(DB,Args),BBB):- get_var_expansions(Args,ArgsO,VarsOut),!,
  conjoin(VarsOut,unity_db_call(DB,ArgsO),BBB).

ugoal_expansion(B,BB):- sub_term(E,B),compound(E),compound_name_arguments(E,DB,Args),db_pred(DB),
   notrace(subst(B,E,Var,BBM)),
   Var = unity_db_call(DB,Args),
   expand_ugoal(BBM,BB).

ugoal_expansion(begin(List),begin(List)):- var(List),!.
ugoal_expansion(begin([H|B]),begin(Program)):- list_to_conjuncts([H|B],Conj),
  expand_ugoal(Conj,Program),!.
ugoal_expansion(begin(H),begin(HH)):- !, expand_ugoal(H,HH).

% xfy_mathese_arity(XFY, !.
ugoal_expansion(G::X,in_uctx(G,Y)):- !, expand_ugoal(X,Y).
ugoal_expansion(M:Cmp,M:CmpO):- atom(M),meta_pred_style(M:Cmp,Meta),
  compound_name_arguments(Cmp,F,Args),compound_name_arguments(Meta,_,MArgs),
  maplist(ugoal_expansion_3,MArgs,Args,ArgsO),!,compound_name_arguments(CmpO,F,ArgsO).
ugoal_expansion(CmpI,CmpO):- strip_module(CmpI,_,Cmp),CmpI==Cmp,meta_pred_style(Cmp,Meta),
  compound_name_arguments(Cmp,F,Args),compound_name_arguments(Meta,_,MArgs),
  maplist(ugoal_expansion_3,MArgs,Args,ArgsO),!,compound_name_arguments(CmpO,F,ArgsO).
  
ugoal_expansion(B,BBBB):- sub_term(E,B),dv(E,V),subst(B,E,Var,BB),!,
  conjoin(must_getvar(V,Var),BB,BBB),expand_goal(BBB,BBBB).
  
%ugoal_expansion(B,BBB):- once(uclause_expansion_inner(hb,goal,B,goal:-BB)),B\==BB,!,expand_goal(BB,BBB).
unity_db_call(F,Args):- maplist(expand_uterm,Args,EArgs),!,apply(F,EArgs).

get_var_expansions(B,O,VarsOut):- compound(B), sub_term(E,B),dv(E,V),subst(B,E,Var,BB),!,
  get_var_expansions(BB,O,SVars),conjoin(must_getvar(V,Var),SVars,VarsOut).
get_var_expansions(O,O,true).

expand_ugoal_vars(B,O):- get_var_expansions(B,O,G),call(G).

no_more_expansion(C,C):- \+ compound(C),!.
no_more_expansion(unity_call(C),Out):- get_var_expansions(C,O,Vars),conjoin(Vars,unity_call(O),Out).
no_more_expansion(slash_db(S,C),Out):- get_var_expansions(C,O,Vars),conjoin(Vars,slash_db(S,O),Out).
no_more_expansion(Dyn,Dyn):-  compound_name_arity(Dyn,F,1), current_op(X,Y,dynamic),\+ upcase_atom(F,F), current_op(X,Y,F),current_predicate(F/1),!.
%ugoal_expansion(X,_):- \+ compound(X),!,fail.
%ugoal_expansion(Var,unity_call(Var)):- var(Var),!.
ugoal_expansion(public(X),unity_call(public(X))).
ugoal_expansion(X,Y):- compound(X),maybe_into_slash_db(X,Y).
%ugoal_expansion(X,Y):- prolog_load_context(term,:-T),T==X,uarg_exp(X,Y).
%ugoal_expansion(X,Y):- prolog_load_context(term,_:-T),T==X,uarg_exp(X,Y).
ugoal_expansion(G,unity_call(G)):-  compound_name_arguments(G,DBPred,_),db_pred(DBPred).
%ugoal_expansion(X,Y):- expand_uarg(X,Y).

expand_f_args_to_list(begin).
expand_f_args_to_list(displayln).

%:- use_module(library(pfc_lib)).

:- set_prolog_flag_until_eof(allow_variable_name_as_functor,true).
:- style_check(- singleton).

:- current_op(X,Y,meta_predicate),op(X,Y,higher_order).
higher_order(X):-
  ho2mp(X,Y),
  meta_predicate(Y).

ho2mp(X,Y):- compound(X),!,
  compound_name_arguments(X,F,AX),
  maplist(ho2mp,AX,AY),
  compound_name_arguments(Y,F,AY).
ho2mp(0,'?').
ho2mp(1,0).
ho2mp(X,X).


%% thaw(?X)
%  If X is an unbound variable with a frozen_u goal, wakes the goal.
frozen_u(X,G) :-
   frozen(X, T),
   unwrap_popsickle(T,TT),G=TT.

unwrap_popsickle(O,O):- \+ compound(O),!.
unwrap_popsickle(freeze(_,G),O):- !, unwrap_popsickle(G,O).
unwrap_popsickle(M:G,O):- nonvar(M),!, unwrap_popsickle(G,O).
unwrap_popsickle(Comp,CmpO):- predicate_property(Comp,meta_predicate(Meta)),
  strip_module(Comp,_,Cmp),
  compound_name_arguments(Cmp,F,Args),compound_name_arguments(Meta,_,MArgs),
  maplist(unwrap_popsickle_3,MArgs,Args,ArgsO),!,compound_name_arguments(CmpO,F,ArgsO).
unwrap_popsickle(O,O).

unwrap_popsickle_3(?,A,A):-!.
unwrap_popsickle_3(+,A,A):-!.
unwrap_popsickle_3(-,A,A):-!.
unwrap_popsickle_3(_,A,O):- unwrap_popsickle(A,O).


all(X,Y,Z):- findall(X,Y,L), list_to_set(L,Z).

:- current_op(X,Y,dynamic),op(X,Y,indexical).
indexical(X=Y):- !, bind(X,Y),log(indexical(X=Y)).
indexical(X):- atom(X),!,unknownvar_value(X,V),bind(X,V).
indexical(X):- %compound(X),!, 
  compound_name_arguments(X,_,AX), maplist(indexical,AX).

indexical_named(X,Y):- must_getvar(X,Y).

/*
            DeclareIndexical("this",
                context =>
                {
                    if (context.This == null)
                        throw new Exception("Indexical $this has not been given a value");
                    return context.This;
                });
            DeclareIndexical("me",
                context =>
                {
                    if (context.KnowledgeBase.GameObject == null)
                        throw new Exception("Current KnowledgeBase has no associated game object");
                    return context.GameObject;
                });
            DeclareIndexical("parent",
                context =>
                {
                    if (context.KnowledgeBase.Parent == null)
                        throw new Exception("Current KnowledgeBase has no parent.");
                    return context.KnowledgeBase.Parent;
                });
            DeclareIndexical("global", context => KnowledgeBase.Global);
            DeclareIndexical("root", context => context.KnowledgeBase.ELRoot);
            DeclareIndexical("global_root", context => KnowledgeBase.Global.ELRoot);
            DeclareIndexical("now", context => Time.time);
*/

'$'(Var,Value):- must_getvar(Var,Value).

must_getvar(N,V):- nonvar(V),!,must_getvar(N,Y),!,V=Y.
must_getvar(now,Value):- !, get_time(Value).
must_getvar(X,Y):- nb_current(X,Y),!.
%must_getvar(me,Y):- !, Y = me.
%must_getvar(X,Y):- log(warn(must_getvar(X,Y))),fail.
must_getvar(X,Y):- number(X),!,Y=X.
must_getvar(X,Y):- unknownvar_value(X,Y),!.
% for DCGs
must_getvar(X,Y,A,A):- must_getvar(X,Y).
% unknownvar_value(X,V):- atom(X),!,atom_concat('',X,V).
unknownvar_value(X,V):- atom(X),!,atom_concat('unknown_',X,V).
unknownvar_value(X,'#'(X)).

bind(X,Y):- b_setval(X,Y).

log(warn(X)):- !, dmsg(warn(X)).
log(X):- dmsg(X).

starts_with_one_of(String,Word):- sub_string(Word,0,1,_,L),sub_string(String,_,_,_,L).

for_all_unique(T,Gen):- for_all_unique(T,Gen,true).
for_all_unique(T,Gen,Goal):-
  all(T,Gen,Set),member(T,Set),call(Goal).

:- current_op(X,Y,dynamic),op(X,Y,register_lexical_items).
:- dynamic(is_lexical_item/1).
register_lexical_item(X):- is_lexical_item(X),!.
register_lexical_item(X):- log(register_lexical_item(X)), assert_if_new(is_lexical_item(X)).


:- current_op(X,Y,dynamic),op(X,Y,external).
:- meta_predicate(system:external(:)).
system:external(X):- nop(log(external(X))),discontiguous(X),distributed_pred(X).

:- current_op(X,Y,dynamic),op(X,Y,distributed_pred).
:- meta_predicate(system:distributed_pred(:)).
system:distributed_pred(X):- nop(log(distributed_pred(X))),
  dynamic(X),multifile(X),public(X),discontiguous(X).

:- distributed_pred(fkey_command/2).

:- current_op(X,Y,dynamic),op(X,Y,randomizable).
randomizable(X):- dynamic(X),multifile(X),discontiguous(X),nop(log(randomizable(X))).

load_unity_prolog_file(F):- 
  log(load_unity_prolog_file(F)),
  unity_prolog_filename(F,Filename),
  asserta_if_new(tmpu:is_unity_file(Filename)),
  unity_module_name(Unity),
  load_files(Filename,[module(Unity),must_be_module(false),redefine_module(false),scope_settings(false)]).

load_unity_csv_file(F):- 
  log(load_unity_csv_file(F)),
  unity_prolog_filename(F,Prolog),
  load_unity_csv_file_data(Prolog).

unity_prolog_filename(F,Filename):- exists_file(F),!,Filename=F.
unity_prolog_filename(F,Filename):- lmchat_dir(D),atomic_list_concat([D,'/',F],Filename),exists_file(Filename),!.
unity_prolog_filename(N,Filename):- name(F,N), lmchat_dir(D),absolute_file_name(F,Filename,[relative_to(D),extensions(['prolog','pl','P','']),access(read)]),!.
unity_prolog_filename(F,F).

now(Now):-  get_time(Now).


call_with_step_limit(Limit,Goal):- ALimit is Limit ^ 3, call_with_inference_limit(Goal, ALimit, Result), 
    ignore((inference_limit_exceeded == Result, throw(Result))),
    (((Result == (!))-> ! ; Result)).

:- assume_done(step_limit/1).

% :- assume_todo(component_of_gameobject_with_type/3).

component_of_gameobject_with_type(X,X,_).

:- assume_todo(parent_of_gameobject/2).

:- assume_todo(sumall/3).
:- assume_todo(generate_unique/2).
:- assume_todo(call_method/3).
:- assume_todo(arg_min/3).
:- assume_todo(arg_max/3).

:- assume_dyn_fail(type/2).
:- dynamic(relation_type/3).
:- assume_dyn_fail(property_type/3).
:- assume_dyn_fail(property_name/3).
:- assume_dyn_fail(property/3).
:- assume_dyn_fail(predicate_type/2).
:- assume_todo(is_class/2).

:- assume_done(pause_game/0).
:- assume_done(unpause_game/0).
:- assume_done(randomize/1).


:- assume_done(set_property/3).

%:- assume_todo(prop/1).
%:- assume_todo(now/1).
%:- assume_todo(consult/2).

%:- assume_dyn_fail(word_list/2).
word_list(X,Y):- nonvar(X),tokenize_atom(X,Y),!.
word_list(X,Y):- nonvar(Y),atomics_to_string(Y,' ',X).

consult(File, M):- M:consult(File).

module_uctx(X):- '$current_typein_module'(X),!.
module_uctx(X):- prolog_load_context(module,X),!.

unity_uctx:- prolog_load_context(file,File),!,tmpu:is_unity_file(File).
unity_uctx:- unity_module_name(M),module_uctx(T),!,M==T.

print_clexp(X,Y):- X=@=Y,!.
%print_clexp(X,Y):- in_cmt(print_tree(X)),print_tree(Y).
print_clexp(X,Y):- dmsg(X),writeln('%~'),dmsg(Y).




csv_predicate(CSV_file,Pred):-
    file_base_name(CSV_file,Name),file_name_extension(Pred,_,Name).

load_unity_csv_file_data(CSV_file):-
    csv_predicate(CSV_file,Pred),
    csv_read_file(CSV_file, CSV, [table(Pred)]),
    CSV = [Props|Rows],
    Props=..[_|Types],
    must_maplist(into_row_types,Types,RTypes),
    adbg(rowTypes(Types) --> RTypes),
    ignore(begin_csv_loading(Pred)),
    set_flag('$csv_row',1),!,
    maplist(load_csv_row_data(RTypes,Pred), Rows),
    ignore(end_csv_loading(Pred)).

load_csv_row_data(Types,Pred, RowTerm):- (load_csv_row_data_now(Types,Pred, RowTerm)),!.
load_csv_row_data_now(Types,Pred, RowTerm) :-     
    RowTerm=..[_|Row],
    Row = [F|_],
    % allow comments
    (atom_concat('%',_,F) -> ! ;
     (must_maplist(correct_row,Types,Row,CRow),
      RowCall=..[Pred|CRow],
      flag('$csv_row',RowNumber,RowNumber+1),!,
      adbg(load_csv_row(RowNumber,RowCall)),
      once(load_csv_row(RowNumber,RowCall)))),
    !.

vs_name(N='$VAR'(N)).
into_row_types('Description',string).
into_row_types('Role',[prefix,'']).
into_row_types(Type,[prefix,RType]):- atom(Type),atomic_list_concat([L,R],'prefix:',Type),atomic_list_concat([RType,_],')',R).
into_row_types(Type,RType):- atom(Type),tokenize_atom(Type,M),M\==[Type],!,into_row_types(M,RType).
into_row_types([A],A).
into_row_types(List,RTypeO):- is_list(List),append(_,['('|Rest],List),append(RType,[')'],Rest),!,into_row_types(RType,RTypeO).
into_row_types(Type,Type).

correct_row([prefix,Prefix],A,V):- atom(A),atom_concat(Prefix,A,R),
 read_term_from_atom(R,T,[variable_names(Vs)]),maplist(vs_name,Vs),
 correct_row_type(Type,T,V),
 notrace(ignore((V\==[],A\==V,fail,adbg(Prefix+A --> V)))),!.
correct_row(Type,A,V):- atom(A),
 once(atom_concat(_,')',A);atom_contains(A,',');atom_contains(A,':');atom_contains(A,'$');atom_contains(A,'=');atom_contains(A,')')),
 read_term_from_atom(A,T,[variable_names(Vs)]),maplist(vs_name,Vs),
 correct_row_type(Type,T,V),
 notrace(ignore((V\==[],A\==V,fail,adbg(Type+A --> V)))),!.
correct_row(Type,A,V):-
  correct_row_type(Type,A,V),
  notrace(ignore((V\==[],Type\==list,Type\==string,Type\=[_,list],A\==V,adbg(Type+A --> V)))),!.

correct_row_type(T,A,A):- var(T),!.
correct_row_type([_Word,list],A,V):- !,correct_row_type('list',A,V).
correct_row_type([T],A,V):- correct_row_type(T,A,V),!.
correct_row_type(list,A,V):- listify_row(A,V),!.
correct_row_type(string,A,V):- cell_to_string(A,V),!.
correct_row_type(_,V,V).

cell_to_string(A,V):- atom(A),atom_concat('"',R,A),string_concat(V,'"',R),!.
cell_to_string(A,V):- notrace(any_to_string(A,V)),!.

listify_row(A,V):- is_list(A),!,A=V.
listify_row((A,B),V):- !, conjuncts_to_list((A,B),V).
listify_row('',[]):-!.
listify_row('-',[['-']]):-!.
listify_row(A,V):- atom(A),!,tokenize_atom(A,M),join_underscores(M,V).
listify_row(A,[A]).

join_underscores(A,V):- append(Left,[L,'_',R|More],A),atomic_list_concat([L,'_',R],LR),append(Left,[LR|More],M),!,join_underscores(M,V).
join_underscores(V,V).

:- fixup_exports.

:- multifile(term_expansion/2).
:- dynamic(term_expansion/2).
term_expansion(X,Y):- compound(X), unity_uctx, uterm_expansion(X,Y),X\==Y,ansicall(yellow,print_clexp(X,Y)).
system:goal_expansion(X,Y):- compound(X), unity_uctx, expand_ugoal(X,Y),X\==Y,ansicall(gold,print_clexp(X,Y)).
:- load_unity_prolog_file('Utilities/startup.prolog').


end_of_file.

%:- dynamic($/2).

