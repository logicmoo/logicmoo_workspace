
begin(G):- is_list(G),!,maplist(begin,G).
begin(G):- must_det_l(G).

:- arithmetic_function('$'/1).
:- arithmetic_function('now'/0).

:- op(1100,fx,@),
   op(1100,xfy,:=),
   op(1100,xfy,:^).


:- op(800,fx,'/').
:- op(900,xfx,'::').
:- op(399,xfy,':').

:- op(399,fx,'~').

%:- op(1000,fx,('$')).
:- multifile (/)/1.
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


system:unity_apply(F,Args):- !, wdmsg(unity_apply(F,Args)), apply(F,Args).

convert_assert(G,G):- var(G),!.
convert_assert(::(G,Arg),Slash):- G == $global,!, convert_assert(Arg,Slash).
convert_assert(X,X):- X = slash_db(_,_),!.
convert_assert(Arg,Slash):-  maybe_into_slash_db(Arg,Slash),!.
convert_assert(X,Y):- compound(X),
  compound_name_arguments(X,F,AX),
  maplist(convert_assert,AX,AY),
  compound_name_arguments(Y,F,AY).
convert_assert(X,X).

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
 
uarg_expansion(X,Y):- var(X),!,X=Y.
uarg_expansion(X,Y):- \+ compound(X),!,X=Y.
uarg_expansion(is(_,_),_):-!,fail.
uarg_expansion(X,Y):- maybe_into_slash_db(X,Y),!.
%uarg_expansion(X:-B,Y:-B):- maybe_into_slash_db(X,Y),!.
%uarg_expansion(X,Y):- compound(X),once(ugoal_expansion(X,Y)),X\==Y.
uarg_expansion(X,Y):- compound(X),!,
  compound_name_arguments(X,F,AX),
  maplist(uarg_expansion,AX,AY),
  compound_name_arguments(Y,F,AY).

uterm_expansion(X,Y):- prolog_load_context(term,T),compound(T),T==X,uarg_expansion(X,Y).

ugoal_expansion(Var,Var):- var(Var),!.
ugoal_expansion(Var,Var):- \+ compound(Var),!.
ugoal_expansion(unity_call(Var),unity_call(Var)):- !.
%ugoal_expansion(X,_):- \+ compound(X),!,fail.
%ugoal_expansion(Var,unity_call(Var)):- var(Var),!.
ugoal_expansion(G,begin(Args)):- compound_name_arguments(G,begin,Args),Args\=[_],!.
ugoal_expansion(public(X),unity_call(public(X))).
ugoal_expansion(X,Y):- prolog_load_context(term,:-T),T==X,uarg_expansion(X,Y).
ugoal_expansion(X,Y):- prolog_load_context(term,_:-T),T==X,uarg_expansion(X,Y).
ugoal_expansion(G,unity_call(G)):-  compound_name_arguments(G,DBPred,_),db_pred(DBPred).


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
indexical(X=Y):- !, nb_setval(X,Y),wdmsg(indexical(X=Y)).
indexical(X):- compound(X),!, compound_name_arguments(X,_,AX), maplist(indexical,AX).

bind(X,Y):- b_setval(X,Y).

log(X):-dmsg(X).

starts_with_one_of(String,Word):- sub_string(Word,0,1,_,L),sub_string(String,_,_,_,L).

for_all_unique(T,Gen):- for_all_unique(T,Gen,true).
for_all_unique(T,Gen,Goal):-
  all(T,Gen,Set),member(T,Set),call(Goal).

:- current_op(X,Y,dynamic),op(X,Y,register_lexical_items).
register_lexical_item(X):- wdmsg(register_lexical_item(X)), assert(is_lexical_item(X)).


indexical_named(X,Y):- wdmsg(too_early(indexical_named(X,Y))).


:- current_op(X,Y,dynamic),op(X,Y,external).
:- meta_predicate(system:external(:)).
system:external(X):- wdmsg(external(X)),discontiguous(X),distributed_pred(X).

:- current_op(X,Y,dynamic),op(X,Y,distributed_pred).
:- meta_predicate(system:distributed_pred(:)).
system:distributed_pred(X):- wdmsg(distributed_pred(X)),dynamic(X),multifile(X),discontiguous(X).

:- distributed_pred(fkey_command/2).

:- current_op(X,Y,dynamic),op(X,Y,randomizable).
randomizable(X):- dynamic(X),multifile(X),discontiguous(X),wdmsg(randomizable(X)).

load_unity_prolog_file(F):- 
  load_files(F,[module(unity_prolog),dialect(pfc),must_be_module(false),redefine_module(false),scope_settings(false)]).

assume_todo(F/A):- functor(P,F,A), assert(P:- wdmsg(warn(todo(P)))).

now(Now):-  get_time(Now).

'$'(Number,Value):- number(Number),!,Number=Value.
'$'(now,Value):- !, get_time(Value).
'$'(Var,Value):- nb_current(Var,Value).

:- assume_todo(unpause_game/0).
:- assume_todo(type/2).
:- assume_todo(sumall/3).
:- assume_todo(step_limit/1).
:- assume_todo(set_property/3).
:- assume_todo(relation_type/3).
:- assume_todo(randomize/1).
:- assume_todo(property_type/3).
:- assume_todo(property_name/3).
:- assume_todo(property/3).
%:- assume_todo(prop/1).
:- assume_todo(predicate_type/2).
:- assume_todo(pause_game/0).
:- assume_todo(parent_of_gameobject/2).
%:- assume_todo(now/1).
:- assume_todo(is_class/2).
:- assume_todo(generate_unique/2).
:- assume_todo(displayln/7).
:- assume_todo(displayln/5).
:- assume_todo(displayln/4).
:- assume_todo(displayln/3).
:- assume_todo(displayln/1).
%:- assume_todo(consult/2).
:- assume_todo(component_of_gameobject_with_type/3).
:- assume_todo(call_with_step_limit/2).
:- assume_todo(call_method/3).
:- assume_todo(arg_min/3).
:- assume_todo(arg_max/3).
%:- assume_dyn(word_list/2).
word_list(X,Y):- atomic_list_concat(Y,X).

consult(File, M):- M:consult(File).

term_expansion(X,Y):- prolog_load_context(module,mkultra),uterm_expansion(X,Y).
system:goal_expansion(X,Y):- prolog_load_context(module,mkultra), ugoal_expansion(X,Y).
:- load_unity_prolog_file('Utilities/startup.prolog').



end_of_file.

:- dynamic($/2).

