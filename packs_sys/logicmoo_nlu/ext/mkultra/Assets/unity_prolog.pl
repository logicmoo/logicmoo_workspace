% :- module(unity_prolog,[]).


make_pred_narity_call_list(P,A):- 
  length(L,A),
  Head=..[P|L],Body=..[P,L],
  assert((Head:-Body)).

begin(G):- is_list(G),!,maplist(begin,G).
begin(G):- must(G).

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
convert_assert(X,X):- X = slash_db(_,_),!.
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

slash_db(X,Y):- log(slash_db(X,Y)),fail.
%slash_db(top,_X):- fail.
slash_db(rel,X):- slash_db(top,/X).
slash_db(rel,X):- slash_db(top,/_/X).
slash_db(rel,X):- slash_db(top,/_/_/X).
 
uarg_expansion(X,Y):- var(X),!,X=Y.
uarg_expansion(X,Y):- \+ compound(X),!,X=Y.
uarg_expansion(is(X,Y),is(X,Y)):-!.
uarg_expansion(slash_db(X,Y),slash_db(X,Y)):-!.
uarg_expansion(X,Y):- maybe_into_slash_db(X,Y),!.
%uarg_expansion(X:-B,Y:-B):- maybe_into_slash_db(X,Y),!.
%uarg_expansion(X,Y):- compound(X),once(ugoal_expansion(X,Y)),X\==Y.

uarg_expansion(G::X,M:Y):- var(G),G=M,!,uarg_expansion(X,Y).
uarg_expansion(G::X,Y):- G == $global,!,uarg_expansion(X,Y).
uarg_expansion(G:X,M:Y):- G=M,!,uarg_expansion(X,Y).
%uarg_expansion(::(G,X),M:Y):- nonvar(G),G='$'(M),uarg_expansion(X,Y).
%uarg_expansion(G::X,G{}.X):- !.
%uarg_expansion($Kavi,K{k:K,name:Kavi}).
uarg_expansion(X,Y):- compound(X),!,
  compound_name_arguments(X,F,AX),
  maplist(uarg_expansion,AX,AY),
  functor_expansion(F,FF),
  compound_name_arguments(Y,FF,AY).

% functor_expansion('::',':').
functor_expansion(F,F).

dv(E,V):-compound(E), E='$'(V), V \== global.
uclause_expansion_inner(hb,H,B,HHBB):- sub_term(E,H),dv(E,V),subst(H,E,Var,HH),!,
  conjoin(must_getvar(V,Var),B,BB),uclause_expansion_inner(hb,HH,BB,HHBB).
uclause_expansion_inner(hb,H,B,HHBB):- sub_term(E,B),dv(E,V),subst(B,E,Var,BB),!,
  conjoin(must_getvar(V,Var),BB,BBB),uclause_expansion_inner(hb,H,BBB,HHBB).
uclause_expansion_inner(hb,H,B,H:-B).

uclause_expansion_inner(dcg,H,B,HHBB):- sub_term(E,H),dv(E,V),subst(H,E,Var,HH),!,
  conjoin({must_getvar(V,Var)},B,BB),uclause_expansion_inner(dcg,HH,BB,HHBB).
uclause_expansion_inner(dcg,H,B,HHBB):- sub_term(E,B),dv(E,V),subst(B,E,Var,BB),!,
  conjoin({must_getvar(V,Var)},BB,BBB),uclause_expansion_inner(dcg,H,BBB,HHBB).
uclause_expansion_inner(dcg,H,B,H-->B).

uclause_expansion(T,H,B,HHBB):- uclause_expansion_inner(T,H,B,HB),uarg_expansion(HB,HHBB).

uterm_expansion(X,_):- prolog_load_context(term,T),T\==X,!,fail.
uterm_expansion(:-B,:-BB):- !, ugoal_expansion(B,BB).
uterm_expansion(H:-B,HB):- !, uclause_expansion(hb,H,B,HB).
uterm_expansion(H-->B,HB):- !, uclause_expansion(dcg,H,B,HB).
uterm_expansion(H,HB):- uclause_expansion(hb,H,true,HB).

ugoal_expansion(Var,Var):- var(Var),!.
ugoal_expansion(Var,Var):- \+ compound(Var),!.
ugoal_expansion(unity_call(Var),unity_call(Var)):- !.
ugoal_expansion(G::X,Y):- G == $global,!,ugoal_expansion(X,Y).
ugoal_expansion(B,BBB):- once(uclause_expansion_inner(hb,goal,B,goal:-BB)),B\==BB,!,expand_goal(BB,BBB).
%ugoal_expansion(X,_):- \+ compound(X),!,fail.
%ugoal_expansion(Var,unity_call(Var)):- var(Var),!.
ugoal_expansion(G,BArgs):- compound_name_arguments(G,B,Args),args_to_list(B),Args\=[_],!,BArgs=..[B|Args].
ugoal_expansion(public(X),unity_call(public(X))).
ugoal_expansion(X,Y):- compound(X),maybe_into_slash_db(X,Y).
%ugoal_expansion(X,Y):- prolog_load_context(term,:-T),T==X,uarg_expansion(X,Y).
%ugoal_expansion(X,Y):- prolog_load_context(term,_:-T),T==X,uarg_expansion(X,Y).
ugoal_expansion(G,unity_call(G)):-  compound_name_arguments(G,DBPred,_),db_pred(DBPred).
ugoal_expansion(X,Y):- uarg_expansion(X,Y).

args_to_list(begin).
args_to_list(displayln).

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


all(X,Y,Z):- findall(X,Y,L), list_to_set(L,Z).

:- current_op(X,Y,dynamic),op(X,Y,indexical).
indexical(X=Y):- !, nb_setval(X,Y),log(indexical(X=Y)).
indexical(X):- %compound(X),!, 
  compound_name_arguments(X,_,AX), maplist(indexical,AX).

indexical_named(X,Y):- must_getvar(X,Y).

must_getvar(X,Y):- nb_current(X,Y),!.
%must_getvar(me,Y):- !, Y = me.
must_getvar(X,Y):- log(warn(must_getvar(X,Y))),fail.
must_getvar(X,Y):- atom(X),!,atom_concat('unknown__',X,Y).
must_getvar(X,'#'(X)).
'$'(Number,Value):- number(Number),!,Number=Value.
'$'(now,Value):- !, get_time(Value).
'$'(Var,Value):- must_getvar(Var,Value).

bind(X,Y):- b_setval(X,Y).

log(warn(X)):- !, dmsg(warn(X)).
log(X):-nop(dmsg(X)).

starts_with_one_of(String,Word):- sub_string(Word,0,1,_,L),sub_string(String,_,_,_,L).

for_all_unique(T,Gen):- for_all_unique(T,Gen,true).
for_all_unique(T,Gen,Goal):-
  all(T,Gen,Set),member(T,Set),call(Goal).

:- current_op(X,Y,dynamic),op(X,Y,register_lexical_items).
register_lexical_item(X):- nop(log(register_lexical_item(X))), assert(is_lexical_item(X)).


:- current_op(X,Y,dynamic),op(X,Y,external).
:- meta_predicate(system:external(:)).
system:external(X):- nop(log(external(X))),discontiguous(X),distributed_pred(X).

:- current_op(X,Y,dynamic),op(X,Y,distributed_pred).
:- meta_predicate(system:distributed_pred(:)).
system:distributed_pred(X):- nop(log(distributed_pred(X))),dynamic(X),multifile(X),discontiguous(X).

:- distributed_pred(fkey_command/2).

:- current_op(X,Y,dynamic),op(X,Y,randomizable).
randomizable(X):- dynamic(X),multifile(X),discontiguous(X),nop(log(randomizable(X))).

load_unity_prolog_file(F):- 
  log(load_unity_prolog_file(F)),
  unity_prolog_filename(F,FN),
  load_files(FN,[module(mkultra),must_be_module(false),redefine_module(false),scope_settings(false)]).

unity_prolog_filename(F,FN):- exists_file(F),!,FN=F.
unity_prolog_filename(F,FN):- mkultra_dir(D),atomic_list_concat([D,'/',F],FN),exists_file(FN),!.
unity_prolog_filename(F,F).

now(Now):-  get_time(Now).


call_with_step_limit(Limit,Goal):- call_with_inference_limit(Goal, Limit, Result), 
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
:- assume_dyn_fail(relation_type/3).
:- assume_dyn_fail(property_type/3).
:- assume_dyn_fail(property_name/3).
:- assume_dyn_fail(property/3).
:- assume_dyn_fail(predicate_type/2).
:- assume_dyn_fail(is_class/2).

:- assume_done(pause_game/0).
:- assume_done(unpause_game/0).
:- assume_done(randomize/1).


:- assume_done(set_property/3).

%:- assume_todo(prop/1).
%:- assume_todo(now/1).
%:- assume_todo(consult/2).

%:- assume_dyn_fail(word_list/2).
word_list(X,Y):- atomic_list_concat(Y,X).

consult(File, M):- M:consult(File).

module_ctx(X):- '$current_typein_module'(X),!.
module_ctx(X):- prolog_load_context(module,X),!.

unity_ctx:- module_ctx(mkultra).

print_clexp(Y):- \+ \+ (numbervars(Y,9,_), print_tree(Y)).

:- fixup_exports.

:- multifile(term_expansion/2).
:- dynamic(term_expansion/2).
term_expansion(X,Y):- compound(X), unity_ctx, uterm_expansion(X,Y),!. %,print_clexp(Y),nl.
system:goal_expansion(X,Y):- compound(X), unity_ctx, ugoal_expansion(X,Y),!.
:- load_unity_prolog_file('Utilities/startup.prolog').


end_of_file.

%:- dynamic($/2).

