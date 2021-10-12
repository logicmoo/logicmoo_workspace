

begin(G):- must_det_l(G).

:- arithmetic_function('$'/1).
:- arithmetic_function('now'/0).
:- op(800,fx,'/').
:- op(900,xfx,'::').
:- op(399,xfy,':').
:- op(1200,xfy,':=').
:- op(700,xfx,':^').
:- op(1000,fx,('@')).
%:- op(1000,fx,('$')).
:- multifile (/)/1.
:- multifile  '~' / 1.
%:- multifile  :: / 2.

:- multifile slash_db/2.
:- dynamic slash_db/2.

:- discontiguous (':=')/2.
:- discontiguous  '~' / 1.



:- discontiguous(ugoal_expansion/2).

ugoal_expansion(Var,Var):- var(Var),!.
%ugoal_expansion(Var,unity_call(Var)):- var(Var),!.
ugoal_expansion(Var,Var):- \+ compound(Var),!.
ugoal_expansion(unity_call(Var),unity_call(Var)):- !.
ugoal_expansion(G,begin(Args)):- compound_name_arguments(G,begin,Args),Args\=[_],!.

db_pred(assert).
db_pred(retract).
db_pred(retractall).
db_pred(asserta).
db_pred(assertz).
db_pred(clause).
db_pred(abolish).


% ugoal_expansion(X=Y,unity_call(X=Y)).
/*
unity_apply(F,Args):- unity_apply(a,F,Args).
unity_call(G):- unity_call(c,G).

unity_apply(a, Assert,[Arg]):- convert_assert(Arg,Slash),!, unity_apply(a, Assert,[Slash]).
unity_apply(c, F,Args):- !, apply(F,Args).
unity_apply(a, F,Args):- !, G=..[F|Args],unity_call(a,G).
*/
unity_call(G):- G=..[F|Args],unity_apply(F,Args).

unity_apply(Assert,[Arg]):- once(convert_assert(Arg,Slash)),Arg\==Slash, unity_apply(Assert,[Slash]).
unity_apply(F,Args):- !, wdmsg(unity_apply(F,Args)), apply(F,Args).

unity_assert(G):- wdmsg(unity_assert(G)),fail.
unity_assert(G):- in_slash_db(G,GG),!,assert(GG). 
unity_assert(G):- assert(G).

convert_assert(G,_):- var(G),!,fail.
convert_assert(::(G,Arg),Slash):- G == $global, convert_assert(Arg,Slash).
convert_assert(Arg,Slash):- in_slash_db(Arg,Slash).
convert_assert(X,X).

in_slash_db(C,_):- \+ compound(C),!,fail.
in_slash_db(slash_db(_,_),_):- !,fail.
%in_slash_db(F/A,_):- atom(F),integer(A), !,fail.
in_slash_db(F/_,_):- \+ slash_arg(F),!,fail.
in_slash_db(_/A,_):- \+ slash_arg(A),!,fail.
in_slash_db(C,slash_db(top,C)):- \+ \+ (numbervars(C,0,_,[attvar(bind)]),is_slashed_g(C)).
in_slash_db(C,slash_db(rel,C)):- \+ \+ (numbervars(C,0,_,[attvar(bind)]),is_slashed(C)).

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
uarg_expansion(X,Y):- in_slash_db(X,Y),!.
%uarg_expansion(X:-B,Y:-B):- in_slash_db(X,Y),!.
%uarg_expansion(X,Y):- compound(X),once(ugoal_expansion(X,Y)),X\==Y.
uarg_expansion(X,Y):- compound(X),!,
  compound_name_arguments(X,F,AX),
  maplist(uarg_expansion,AX,AY),
  compound_name_arguments(Y,F,AY).

uterm_expansion(X,Y):- prolog_load_context(term,T),compound(T),T==X,uarg_expansion(X,Y).
ugoal_expansion(X,_):- \+ compound(X),!,fail.
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

for_all_unique(List,Gen):- is_list(List),!, subst(Gen,List,Var,NewGen),
  forall(member(Var,List),NewGen).
for_all_unique(One,Gen):- subst(Gen,One,Var,NewGen),
  forall(Var=One,NewGen).
for_all_unique(T,Gen,Goal):-
  all(T,Gen,Set),forall(member(T,Set),Goal).

:- current_op(X,Y,dynamic),op(X,Y,register_lexical_items).
register_lexical_item(X):- wdmsg(register_lexical_item(X)), assert(is_lexical_item(X)).


indexical_named(X,Y):- wdmsg(too_early(indexical_named(X,Y))).

:- current_op(X,Y,dynamic),op(X,Y,external).
external(X):- wdmsg(external(X)),dynamic(X),multifile(X),discontiguous(X).

:- current_op(X,Y,dynamic),op(X,Y,randomizable).
randomizable(X):- dynamic(X),multifile(X),discontiguous(X),wdmsg(randomizable(X)).

load_unity_prolog_file(F):- 
  load_files(F,[module(unity_prolog),dialect(pfc),must_be_module(false),redefine_module(false),scope_settings(false)]).


term_expansion(X,Y):- uterm_expansion(X,Y).
goal_expansion(X,Y):- ugoal_expansion(X,Y).
:- load_unity_prolog_file('Utilities/startup.prolog').

