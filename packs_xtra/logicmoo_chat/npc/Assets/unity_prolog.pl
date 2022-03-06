% :- module(unity_prolog,[]).
:- dynamic(tmpu:(is_unity_file/1)).
:- dynamic(unity_module_name/1).

%:- set_prolog_flag(occurs_check,error).

make_pred_narity_call_list(P,A):- 
  length(L,A),
  Head=..[P|L],Body=..[P,L],
  assert_if_new((Head:-Body)).

begin(G):- locally(t_l:pretend_expansion(call),unity_begin(G)).

unity_begin(G):- is_list(G),!,maplist(unity_begin,G).
unity_begin(G):- unity_call(G).


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
:- op(900,xfy,'::').
:- op(399,xfy,':').

string_representation(Term,String):- term_to_string(Term,String).

:- op(399,fx,'~').

%:- op(1000,fx,(getvar)).
%:- multifile (/)/1.
:- multifile  '~' / 1.
%:- multifile  '::' / 2.

:- multifile uslash/2.
:- dynamic uslash/2.

:- discontiguous (':=')/2.
:- discontiguous  '~' / 1.

swi_or_unity(G,_U):- call(G).

:- discontiguous(ugoal_expansion/2).


db_pred(assert).
db_pred(retract).
db_pred(retractall).
db_pred(asserta).
db_pred(assertz).
db_pred(clause).
db_pred(abolish).
db_pred(assert_if_unew).
db_pred(asserta_if_new).
db_pred(assertz_if_new).
db_pred(retract_all).


:- meta_predicate(system:unity_call(:)).

unity_call_p(O,P):- property_value(O,P,V),V==true.

system:unity_call(M:G):- 
  expand_ugoal(G,GG), GG\==G,!, nop(ansicall(cyan, (print_clexp(call,G,GG)))),!, M:call(GG).
system:unity_call(M:G):- !, call(M:G).
%system:unity_call(M:G):- G=..[F|Args],unity_apply(M:F,Args).
%ugoal_expansion(B,BBB):- once(uclause_expansion_inner(hb,goal,B,goal:-BB)),B\==BB,!,expand_goal(BB,BBB).
unity_db_call(F,Args):- maplist(expand_assert,[F],Args,EArgs),!,unity_apply(F,EArgs).


:- meta_predicate(system:unity_apply(:,+)).
system:unity_apply(F,[Arg]):- once(expand_assert(unity_apply,Arg,Slash)),Arg\==Slash,!, unity_apply(F,[Slash]).
system:unity_apply(M:(public),[Arg]):- !, system:unity_apply(M:(external),[Arg]).
%system:unity_apply(F,Args):- \+ ground(Args), log(unity_apply(F,Args)), fail.
system:unity_apply(F,Args):- apply(F,Args).

expand_assert(Why,I,O):-
  expand_slash(I,II), convert_assert(Why,II,O),II\==O,!,ansicall(fushia, print_clexp(Why,I,O)),!.
expand_assert(_Why,O,O).

convert_assert(_Why,G,G):- \+ compound(G),!.
convert_assert(_Why,X,X):- \+ \+ X = uslash(_,_),!.
convert_assert(Why,'::'(G,Arg),Slash):- G == $global,!, unity_module_name(M), expand_assert(Why,M:Arg,Slash).
convert_assert(Why,M:Arg,M:Slash):- !,expand_assert(Why,Arg,Slash).
convert_assert(_Why,C,O):- no_more_expansion(a,C,O),!.
convert_assert(Why,B,(O)):- get_var_expansions(a,B,M,G),G\==true,immediate_expansion,!,call(G),!,expand_assert(Why,M,O).
convert_assert(Why,B,(O:-G)):- get_var_expansions(h,B,M,G),G\==true,!,expand_assert(Why,M,O).
convert_assert(_Why,Arg,Slash):-  maybe_into_slash_db(Arg,Slash),!.
convert_assert(_Why,Arg,Slash):- expand_uterm(Arg,Slash),!.
convert_assert(Why,X,Y):- 
  compound_name_arguments(X,F,AX),
  maplist(expand_assert([F|Why]),AX,AY),
  compound_name_arguments(Y,F,AY),!.

:- system:import(convert_assert/3).

maybe_into_slash_db(C,_):- \+ compound(C),!,fail.
maybe_into_slash_db(uslash(X,Y),uslash(X,Y)):- !.
%maybe_into_slash_db(F/A,_):- atom(F),integer(A), !,fail.
maybe_into_slash_db(F/_,_):- \+ slash_arg(F),!,fail.
maybe_into_slash_db(_/A,_):- \+ slash_arg(A),!,fail.
%maybe_into_slash_db(M'::' /(A),uslash(M,/(A))).
%maybe_into_slash_db(M'::' (A),uslash(M,(A))).
maybe_into_slash_db(C,TO):- \+ \+ (copy_term_nat(C,Nat),(numbervars(Nat,0,_,[attvar(skip)]),is_slashed_g(Nat))),to_slash_db(C,top,TO).
maybe_into_slash_db(C,TO):- \+ \+ (copy_term_nat(C,Nat),(numbervars(Nat,0,_,[attvar(skip)]),is_slashed(Nat))),to_slash_db(C,rel,TO).

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

%uslash(X,Y):- log(uslash(X,Y)),fail.
%uslash(me,_X):- fail.
'/'(X):- uslash(me,/X).

to_slash_db(C,Top,uslash(Top,C)) :- !.
%to_slash_db(C,Top,uslash(Top,O)) :- subst(C,'/','[|]',O).

uslash(rel,X):- !, uslash(this,X).
uslash(top,X):- !, uslash(me,X).

uslash(me,/(X)):- me_path(Root),uslash(Root,X).
uslash(this,X):- this_path(Root),uslash(Root,X).
%uslash(rel,X):- uslash(($this)/X).
%uslash(me,X):- getvar(me,Me),extend_path(Me,X,Full),uslash(full,Full).

me_path(($global) / ($me) ).
this_path( ($global)/($me)/this ).

:- thread_local(t_l:(unity_this_path/1)).
'::'(C,G):- dv(C,N),getvar(N,V),!,'::'(V,G).
'::'(V,G):- var(V),get_uctx_equals(V),!,'::'(V,G).
'::'(Me=Foo,G):- !, locally(b_setval(Me,Foo),'::'($me,G)).
'::'(parent,G):- get_uctx_equals(P), trans_p_to_v(P,up,V),!,
  locally(t_l:unity_this_path(V), locally(nb_setval(this,V), begin(G))).
'::'(C,G):- get_uctx_equals(P), trans_p_to_v(P,C,V),!,
  locally(t_l:unity_this_path(V), locally(nb_setval(this,V), begin(G))).

%'::'(C,G):- t_l:unity_this_path(C),  locally(t_l:unity_this_path(C,P), begin(G)).

:- dynamic(tmp_u:(cached_path/3)).
trans_p_to_v(P,C,[C,P]):- \+ is_list(P),!.
trans_p_to_v([_|List],up,List):-!.
trans_p_to_v(List,C,[C|Rest]):- append(_,[C|Rest],List),!.
trans_p_to_v(P,C,[C|P]).


ephemerally(A):- write(ephemerally(A)),asserta(A,C),undo(erase(C)).

if_uctx_equals(X):-get_uctx_equals(M),(M=X;(is_list(M),member(X,M))),!.
get_uctx_equals(X):- t_l:unity_this_path(X),!.
get_uctx_equals([X]):- getvar(root,X).


calc_to_hashmap(I):- calc_to_hashmap(I,O),print_tree(hasmap_was(I,O)).
calc_to_hashmap(/Nextuid,O):- !, me_path(Me),extend_path(Me,Nextuid,O).
calc_to_hashmap(Nextuid,O):- !, if_uctx_equals(This),extend_path(This,Nextuid,O).
/*
calc_to_hashmap(Nextuid:Var,O,Var):- !, getvar(this,Y),extend_path(Y,Nextuid,O).
calc_to_hashmap(/Nextuid,O,t):- !, getvar(me,Y),extend_path(Y,Nextuid,O).
calc_to_hashmap(Nextuid,O,r):- !, getvar(this,Y),extend_path(Y,Nextuid,O).
*/
extend_path(X,Y,'/'(X,Y)).

/*
calc_full_slash_db(/X,X,Full):- !, getvar(me,Y),extend_path(Y,X,Full).
calc_full_slash_db(M'::'From,X,Full):- '::'(M,calc_full_slash_db(From,X,Full)).
calc_full_slash_db(From,X,Full):- getvar(this,Y),extend_path(From,X,Full).
calc_full_slash_db(me,X,Full):- getvar(me,Y),extend_path(Y,X,Full).
calc_full_slash_db(rel,X,Full):- getvar(this,Y),extend_path(Y,X,Full).
*/
expand_slash(X,Y):- slash_exp(X,Y),!.
expand_slash(X,X).

slash_exp(C,O):- \+ compound(C),!,C=O.
slash_exp(G,G):- compound_name_arity(G,B,_),atom_contains(B,'reduc'),!.
slash_exp(G,O):- compound_name_arguments(G,B,Args),expand_f_args_to_list(B),Args\=[_],!,O=..[B,Args].
slash_exp(C,O):- no_more_expansion(e,C,O),!.
slash_exp(library(X),library(X)):-!.
slash_exp(is(X,Y),is(X,Y)):-!.
slash_exp(X,Y):- maybe_into_slash_db(X,Y),!.
%slash_exp(X:-B,Y:-B):- maybe_into_slash_db(X,Y),!.
%slash_exp(X,Y):- compound(X),once(ugoal_expansion(X,Y)),X\==Y.

%slash_exp(G'::'X,G'::'Y):- !,expand_slash(X,Y).
%slash_exp(G'::'X,Y):- G == $global,!,expand_slash(X,Y).
%slash_exp(G'::'X,M:Y):- var(G),G=M,!,expand_slash(X,Y).

%slash_exp(G:X,M:Y):- G=M,!,expand_slash(X,Y).
%slash_exp('::'(G,X),M:Y):- nonvar(G),G='$'(M),slash_exp(X,Y).
%slash_exp(G'::'X,G{}.X):- !.
%slash_exp($Kavi,K{k:K,name:Kavi}).
slash_exp(X,Y):- compound(X),!,
  compound_name_arguments(X,F,AX),
  maplist(expand_slash,AX,AY),
  functor_expansion(F,FF),
  compound_name_arguments(Y,FF,AY).

% functor_expansion('::',':').
functor_expansion(F,F).


dv(E,V):-compound(E), (E= ('$'(V))), nonvar(V), V \== global. %, V \== this, V \== me.
%uclause_expansion_inner(hb,H,B,HHBB):- sub_uterm(E,H),dv(E,V),usubst(H,E,Var,HH),!,
%  conjoin(getvar(V,Var),B,BB),uclause_expansion_inner(hb,HH,BB,HHBB).
%uclause_expansion_inner(hb,H,B,HHBB):- sub_uterm(E,B),dv(E,V),usubst(B,E,Var,BB),!,
%  conjoin(getvar(V,Var),BB,BBB),uclause_expansion_inner(hb,H,BBB,HHBB).
uclause_expansion_inner(hb,H,B,HB):-  as_clause_hb(H,B,HB).

uclause_expansion_inner(dcg,H,B,HHBB):- sub_term(E,H),dv(E,V),subst(H,E,Var,HH),!,
  conjoin({getvar(V,Var)},B,BB),uclause_expansion_inner(dcg,HH,BB,HHBB).
uclause_expansion_inner(dcg,H,B,HHBB):- sub_uterm(E,B),dv(E,V),subst(B,E,Var,BB),!,
  conjoin({getvar(V,Var)},BB,BBB),uclause_expansion_inner(dcg,H,BBB,HHBB).
uclause_expansion_inner(dcg,H,B,H-->B).

uclause_expansion(T,H,B,HHBB):- uclause_expansion_inner(T,H,B,HB),expand_slash(HB,HHBB).

typed_uclause_expansion(T,H,B,O):-
  expand_ugoal(B,BB1),expand_ugoal(BB1,BB),
  uclause_expansion(T,H,BB,HB),
  finish_te(HB,O).

finish_te(HB,O):-
  expand_slash(HB,HHBB),
  uctx_corrections(HHBB,M),
  cl_hb_to_cl(M,O).


cl_hb_to_cl(HB,HB):- \+ compound(HB),!.
cl_hb_to_cl(H:-True,H):- True==true,!.
cl_hb_to_cl(HB,HB).


uctx_corrections(H,H):- \+ compound(H), !.
uctx_corrections(H:-B,HB):- !, as_clause_hb(H,B,HB).
uctx_corrections(H,CH):- as_clause_head(H,CH).

%lock_var(_).
lock_var(V):- freeze(V, (ignore((nonvar(V),break)))).
as_clause_hb(H,B,HBO):- term_variables(H+B,Vars),nop(maplist(lock_var,Vars)),!,
  as_clause_hb0(H,B,HBO).
as_clause_hb0(H,B,H):- var(H), B ==true,!.
as_clause_hb0(H,B,(H:-BB)):- var(H),!, expand_ugoal(B,BB).
as_clause_hb0(M:H,B,M:(HB)):-!,as_clause_hb0(H,B,HB).

as_clause_hb0('::'( Ctx,H),B,((H:- Ctx::BB))):- var(Ctx),!, expand_ugoal(B,BB).
as_clause_hb0('::'($Ctx,H),B,((H:- BB))):- Ctx==global,!, expand_ugoal(B,BB).
as_clause_hb0('::'($Ctx,H),B,((H:- ( getvar(Ctx,V), if_uctx_equals(V), BB)))):- nonvar(Ctx),!, expand_ugoal(B,BB).
as_clause_hb0('::'( Ctx,H),B,((H:- ( if_uctx_equals(Ctx), BB)))):-  expand_ugoal(B,BB).
%as_clause_hb(H,B,Out):- B == true,!,as_clause_head(H,Out).
%as_clause_hb(H,B,(:-BB)):- H==goal,!, expand_ugoal(B,BB).
as_clause_hb0(H,B,(HH:-BBB)):- get_var_expansions(h,H,HH,G),expand_ugoal(B,BB),conjoin(G,BB,BBB).

as_clause_head(H:-B,GOut):-get_var_expansions(h,H,HH,G),(G==true -> Out = H ; Out = (HH:- G)),conjoin(Out,B,GOut).
as_clause_head(H,Out):-get_var_expansions(h,H,HH,G),(G==true -> Out = H ; Out = (HH:- G)).

uterm_expansion(X,_):- prolog_load_context(term,T),T\==X,!,fail.
uterm_expansion(X,Y):- expand_uterm(X,Y),!.

expand_uterm(C,O):- no_more_expansion(h,C,O),!.
expand_uterm(:-B,:-BB):- !, expand_ugoal(B,BB),!.
expand_uterm(H:-B,HB):- !, typed_uclause_expansion(hb,H,B,HB),!.
expand_uterm(H-->B,HBO):- !, dcg_translate_rule((H-->B),HB),!,expand_uterm(HB,HBO).
expand_uterm(H,HB):- typed_uclause_expansion(hb,H,true,HB),!.
expand_uterm(H,H).
%expand_uterm(H,HB):- expand_slash(H,HB).

:- dynamic(is_meta_predicate/1).

meta_pred_style(Cmp,Meta):- compound(Cmp),compound_name_arity(Cmp,F,A), \+ never_meta_fa(F,A),
  predicate_property(Cmp,meta_predicate(Meta)),!.
meta_pred_style(Cmp,Meta):- compound(Cmp),compound_name_arity(Cmp,F,A), \+ never_meta_fa(F,A),
  compound_name_arity(Meta,F,A),is_meta_predicate(Meta),!.

never_meta_fa('/',_).

%meta_pred_style(Cmp,Meta):- predicate_property(Cmp,transparent),!,functor(Cmp,F,A),functor(Meta,F,A).

/*
ugoal_expansion_3(V,A,A):- var(V),!,shouldnt_need_expansion(A).
ugoal_expansion_3(+,A,A):- shouldnt_need_expansion(A).
ugoal_expansion_3(-,A,A):- shouldnt_need_expansion(A).
ugoal_expansion_3(?,A,A):- shouldnt_need_expansion(A).
*/
ugoal_expansion_3(_,X,Y):- expand_ugoal(X,Y).

expand_ugoal(C,O):- no_more_expansion(h,C,O),!.
expand_ugoal($X,(getvar(X,G),call(G))):-atomic(X),debug_var(X,G).
expand_ugoal('::'(X,G),'::'(X,GG)):-var(X),!,expand_ugoal(G,GG).
expand_ugoal('::'($X,G),(getvar(X,V),'::'(V,G))):-atomic(X),debug_var(X,V).
expand_ugoal(X,Y):- expand_slash(X,XX), ugoal_expansion(XX,XY),!,expand_slash(XY,Y).
expand_ugoal(X,Y):- expand_slash(X,Y).

shouldnt_need_expansion(_):- !.


ugoal_expansion(C,C):- \+ compound(C),!.
ugoal_expansion(\+ C , \+ O):- !, expand_ugoal(C,O).
ugoal_expansion(C,O):- no_more_expansion(b,C,O),!.
ugoal_expansion((A;B),(AA;BB)):- !, expand_ugoal(A,AA),expand_ugoal(B,BB).
ugoal_expansion((A->B),(AA->BB)):- !, expand_ugoal(A,AA),expand_ugoal(B,BB).
ugoal_expansion((A*->B),(AA*->BB)):- !, expand_ugoal(A,AA),expand_ugoal(B,BB).
ugoal_expansion('::'(G,X),'::'(G,Y)):- !, expand_ugoal(X,Y).
ugoal_expansion(':'(G,X),':'(G,Y)):- !, expand_ugoal(X,Y).

/*
subst_vars(G,B,BM):- \+ compound(G),!,B=BM.
subst_vars(_,B,BM):- B=BM,!.
subst_vars([H|T],B,BO):- !,
   subst_vars(H,B,BM),
   subst_vars(T,BM,BO).
subst_vars(G,B,BM):- compound_name_arguments(G,_,[N,V]),atom(N),var(V),subst(B,$N,V,BM).
subst_vars(G,B,BM):- compound_name_arguments(G,_,Args),subst_vars(Args,B,BM).
*/

%ugoal_expansion((A,B),(AA,BB)):- get_var_expansions(h,A,AH,G), subst_vars(G,B,BM), G\== true, !, expand_ugoal(AH,AA),expand_ugoal(BM,BB).

%ugoal_expansion(unity_db_call(DB,Args),BBB):- get_var_expansions(h,Args,ArgsO,VarsOut), \+ source_file_expansion, !,
%  conjoin(VarsOut,unity_db_call(DB,ArgsO),BBB).

ugoal_expansion(E,unity_db_call(DB, Args)):- unity_uctx,compound_name_arguments(E,DB,Args),db_pred(DB),!.
ugoal_expansion(unity_db_call(DB, Args),unity_db_call(DB, Args)):- !.

ugoal_expansion(begin(List),begin(List)):- var(List),!.
ugoal_expansion(begin(List),begin(List)):- is_list(List),!.
%ugoal_expansion(begin([H|B]),begin(Program)):- !, list_to_conjuncts([H|B],Conj),
%  expand_ugoal(Conj,Program),!.
ugoal_expansion(begin(H),begin(HH)):- \+ source_file_expansion, !, expand_ugoal(H,HH).
ugoal_expansion(unity_call(G),unity_call(G)):- !.

%ugoal_expansion(X,Y):- maybe_into_slash_db(X,YY),X\==YY,expand_ugoal(YY,Y).

ugoal_expansion(B,BBBB):- B=(F,_), sub_uterm(E,F),dv(E,V),subst(B,E,Var,BB),!,
  conjoin(getvar(V,Var),BB,BBB),expand_ugoal(BBB,BBBB).
ugoal_expansion((A,B),(AA,BB)):- !, expand_ugoal(A,AA),expand_ugoal(B,BB).
ugoal_expansion(B,BBBB):- sub_uterm(E,B),dv(E,V),subst(B,E,Var,BB),!,
  conjoin(getvar(V,Var),BB,BBB),expand_ugoal(BBB,BBBB).

%ugoal_expansion(X,_):- \+ compound(X),!,fail.
%ugoal_expansion(Var,unity_call(Var)):- var(Var),!.
% xfy_mathese_arity(XFY, !.
ugoal_expansion(M:Cmp,M:CmpO):- atom(M),meta_pred_style(M:Cmp,Meta),
  compound_name_arguments(Cmp,F,Args),compound_name_arguments(Meta,_,MArgs),
  maplist(ugoal_expansion_3,MArgs,Args,ArgsO),!,compound_name_arguments(CmpO,F,ArgsO).
ugoal_expansion(CmpI,CmpO):- strip_module(CmpI,_,Cmp),CmpI==Cmp,meta_pred_style(Cmp,Meta),
  compound_name_arguments(Cmp,F,Args),compound_name_arguments(Meta,_,MArgs),
  maplist(ugoal_expansion_3,MArgs,Args,ArgsO),!,compound_name_arguments(CmpO,F,ArgsO).
ugoal_expansion(public(X),unity_call(public(X))).
%ugoal_expansion(X,Y):- prolog_load_context(term,:-T),T==X,slash_exp(X,Y).
%ugoal_expansion(X,Y):- prolog_load_context(term,_:-T),T==X,slash_exp(X,Y).
ugoal_expansion(X=Y,unity_call(X=Y)):- contains_uvar(X=Y),!.
ugoal_expansion(G,unity_call(G)):-  source_file_expansion, is_unity(G).
ugoal_expansion(G,unity_call(G)):-  \+ source_file_expansion, is_unity(G).
ugoal_expansion(X,Y):- expand_slash(X,Y).

is_unity(X):- contains_uvar(X).
is_unity(X):- sub_uterm(E,X),compound(E),E='::'(_,_).
contains_uvar(X):- sub_uterm(E,X),compound(E),E='$'(_).

get_var_expansions(_,O,O,true):- \+ compound(O),!.
%get_var_expansions(h,'::'(C,T),'::'(CO,T),VarsOut):- get_var_expansions(h,C,CO,VarsOut).
get_var_expansions(b,B,O,VarsOut):- compound(B), sub_uterm(E,B),dv(E,V),subst(B,E,Var,BB),!,get_var_expansions(b,BB,O,SVars),debug_var(V,Var),conjoin(getvar(V,Var),SVars,VarsOut).
get_var_expansions(HA,H,O,VarsOut):- HA\==b,compound(H), sub_term(E,H),dv(E,V),subst(H,E,Var,HH),!,get_var_expansions(HA,HH,O,SVars),debug_var(V,Var),conjoin(getvar(V,Var),SVars,VarsOut).
get_var_expansions(_,O,O,true).

no_more_expansion(_,C,C):- \+ compound(C),!.
no_more_expansion(_,Dyn,Dyn):-  compound_name_arity(Dyn,F,1), current_op(X,Y,dynamic), \+ upcase_atom(F,F), current_op(X,Y,F),current_predicate(F/1),!.
no_more_expansion(b,'::'(_,G),true):- G==true,!.
no_more_expansion(_,(A,G),A):- G==true,!.
no_more_expansion(b,In,true):- In =@= (getvar(global, A),if_uctx_equals(A)).
no_more_expansion(b,In,!):- In =@= (getvar(global, A),if_uctx_equals(A),!).
%no_more_expansion(h,'::'(C,G),'::'(C,G)):-!.
no_more_expansion(b,'::'(C,G),OUT):- get_var_expansions(b,C,CO,Vars),!,expand_ugoal(G,GG), conjoin(Vars,'::'(CO,GG),OUT).
no_more_expansion(b,unity_call(C),Out):- get_var_expansions(h,C,O,Vars),conjoin(Vars,unity_call(O),Out).
no_more_expansion(b,unity_db_call(C,Args),Out):- fail, get_var_expansions(h,Args,VArgs,Vars),Vars\==true,conjoin(Vars,unity_call(C,VArgs),Out).
%no_more_expansion(h,uslash(S,C),Out):- get_var_expansions(h,uslash(S,C),O,Vars),conjoin(Vars,O,Out).
no_more_expansion(b,uslash(S,C),Out):- get_var_expansions(h,C,O,Vars),conjoin(Vars,uslash(S,O),Out).

expand_f_args_to_list(begin).
expand_f_args_to_list(displayln).

%:- use_module(library(pfc_lib)).

:- set_prolog_flag_until_eof(allow_variable_name_as_functor,true).
:- style_check(- singleton).

:- current_op(X,Y,meta_predicate),op(X,Y,higher_order).
higher_order(X):-
  ho2mp(X,Y),
  assert_if_new(
    is_meta_predicate(Y)),
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

indexical_named(X,Y):- getvar(X,Y).

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

'must_getvar'(Var,Value):- getvar(Var,V),!,V=Value.
'$'(Var,Value):- number(Value),!,Var= Value.
'$'(Value,Var):- number(Value),!,Var= Value.
'$'(Var,Value):- getvar(Var,Value).
%'must_getvar'(Var,Value):- getvar(Var,Value).

only_getvar(N,V):- var(N),!,throw(var_only_getvar(N,V)).
only_getvar(N,V):- nonvar(V),!,getvar(N,Y),!,V=Y.
only_getvar(now,Value):- !, now(Value).
only_getvar(X,Y):- nb_current(X,Y),!.
only_getvar(parent,Parent):- getvar(this,Value),Value=[_|Parent].
only_getvar(this,Value):- t_l:unity_this_path(Value),!.
only_getvar(this,Value):- !, getvar(me,Value),listify(me,Value).
only_getvar(me,Value):- !, getvar(root,Value).
only_getvar(root,Value):- !, getvar(global,Value).
%getvar(me,Y):- !, Y = me.
%getvar(X,Y):- log(warn(getvar(X,Y))),fail.
%only_getvar(X,Y):- number(X),!,Y=X.
%only_getvar(X,Y):- unknownvar_value(X,Y),!.

getvar(X,Y):- only_getvar(X,Y),!.
getvar(X,Y):- unknownvar_value(X,Y),!.

dont_udecend_into('::'(_,_)).
dont_udecend_into(Begin):- compound_name_arity(Begin,begin,_),source_file_expansion,!.
dont_udecend_into(Begin):- compound_name_arity(Begin,uslash,_),source_file_expansion,!.
dont_udecend_into(uslash(_,_)).

sub_uterm(X, X):- \+compound(X),!.
sub_uterm(X, '::'(C,_)):- source_file_expansion,!, sub_uterm(X, C).
sub_uterm(_, X):- source_file_expansion,dont_udecend_into(X),!,fail.
sub_uterm(X, X).
sub_uterm(X, Term) :-
    arg(_, Term, Arg),
    sub_term(X, Arg).


usubst(W,X,Y,Z):- !, notrace(subst(W,X,Y,Z)),!.
usubst(Var, VarS, SUB, SUB) :- Var==VarS, !.
usubst(Var, _, _, Var) :- \+ compound(Var), !.
usubst(P, _, _, P) :- \+ \+ dont_udecend_into(P).
usubst(P, X, Sk, P1) :- compound_name_arity(P, _, N), usubst1st(X, Sk, P, N, P1), !.
usubst1st(_, _, P, 0, P).
usubst1st(X, Sk, P, N, P1) :- N>0, 
    univ_term(P, [F|Args]), usubst2st(X, Sk, Args, ArgS), 
    usubst2st(X, Sk, [F], [FS]), univ_term(P1, [FS|ArgS]).
usubst2st(_, _, [], []).
usubst2st(X,Sk,[A|As],[Sk|AS]) :- X==A, !, usubst2st(X, Sk, As, AS).
usubst2st(X, Sk,[A|As],[A|AS]) :- var(A), !, usubst2st(X, Sk, As, AS).
usubst2st(X,Sk,[A|As],[Ap|AS]) :- usubst(A, X, Sk, Ap), !, usubst2st(X, Sk, As, AS).
usubst2st(_X, _Sk, L, L).


atom_or_var(AV):- atom(AV); var(AV).
% for DCGs
getvar(X,Y,A,A):- getvar(X,Y).
% unknownvar_value(X,V):- atom(X),!,atom_concat('',X,V).
unknownvar_value(X,V):- atom(X),atom_or_var(Y),atom_concat('unknown_',X,V),!.
unknownvar_value(X,V):- nonvar(V),!,X=V,log(assume_bind(X,V)),bind(X,V).
unknownvar_value(X,'#'(X)).

%'#'(_).

bind(X,Y):- nb_current(X,O),O\==[],!,b_setval(X,Y).
bind(X,Y):- nb_setval(X,Y).

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
  locally(t_l:pretend_expansion(file),
   (unity_prolog_filename(F,Prolog),
    load_unity_csv_file_data(Prolog))).

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

:- thread_local(t_l:(pretend_expansion/1)).

immediate_expansion:- t_l:pretend_expansion(YN),!,YN==call.
source_file_expansion:- t_l:pretend_expansion(YN),!,YN==file.
source_file_expansion:- prolog_load_context(file,File),prolog_load_context(source,File),!,tmpu:is_unity_file(File).
unity_uctx:- source_file_expansion,!.
unity_uctx:- prolog_load_context(file,File),prolog_load_context(source,File),!,tmpu:is_unity_file(File).
unity_uctx:- unity_module_name(M),module_uctx(T),!,M==T.

%print_clexp(_,_,_):-!.
print_clexp(_,X,Y):- X=@=Y,!.
print_clexp(_,X,Y):- (($global)::X)=@=Y,!.
%print_clexp(X,Y):- in_cmt(print_tree(X)),print_tree(Y).
print_clexp(W,X,Y):- wdmsg(X),write('%~  '),writeln(W),wdmsg(Y).




csv_predicate(CSV_file,Pred):-
    file_base_name(CSV_file,Name),file_name_extension(Pred,_,Name).

load_unity_csv_file_data(CSV_file):-
    csv_predicate(CSV_file,Pred),
    csv_read_file(CSV_file, CSV, [table(Pred)]),
    CSV = [Props|Rows],
    Props=..[_|Types],
    must_maplist(into_row_types,Types,RTypes),
    dmsg(rowTypes(Types) --> RTypes),
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
      dmsg(load_csv_row(RowNumber,RowCall)),
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
 notrace(ignore((V\==[],A\==V,fail,dmsg(Prefix+A --> V)))),!.
correct_row(Type,A,V):- atom(A),
 once(atom_concat(_,')',A);atom_contains(A,',');atom_contains(A,':');atom_contains(A,'$');atom_contains(A,'=');atom_contains(A,')')),
 read_term_from_atom(A,T,[variable_names(Vs)]),maplist(vs_name,Vs),
 correct_row_type(Type,T,V),
 notrace(ignore((V\==[],A\==V,fail,dmsg(Type+A --> V)))),!.
correct_row(Type,A,V):-
  correct_row_type(Type,A,V),
  notrace(ignore((V\==[],Type\==list,Type\==string,Type\=[_,list],A\==V,dmsg(Type+A --> V)))),!.

correct_row_type(T,A,A):- var(T),!.
correct_row_type([_Word,list],A,V):- !,correct_row_type('list',A,V).
correct_row_type([T],A,V):- correct_row_type(T,A,V),!.
correct_row_type(list,A,V):- listify_row(A,V),!.
correct_row_type(string,'',null):-!.
correct_row_type(string,A,V):- cell_to_string(A,V),!.
correct_row_type(_,'',null):-!.
correct_row_type(_,V,V).

cell_to_string(A,V):- atom(A),atom_concat('"',R,A),string_concat(V,'"',R),!.
cell_to_string(A,V):- atom(A),atom_concat('''',R,A),string_concat(V,'''',R),!.
cell_to_string(A,V):- notrace(any_to_string(A,V)),!.

listify_row(A,V):- is_list(A),!,A=V.
listify_row(Var,[Var]):- var(Var),throw(var_listify_row(Var,[Var])).
listify_row((A,B),V):- !, conjuncts_to_list((A,B),V).
listify_row('',[]):-!.
listify_row('-',[['-']]):-!.
listify_row(A,V):- atom(A),!,cell_to_string(A,S),tokenize_atom(S,M),join_underscores(M,V).
listify_row(A,[A]).

join_underscores(A,V):- append(Left,[L,'_',R|More],A),atomic_list_concat([L,'_',R],LR),append(Left,[LR|More],M),!,join_underscores(M,V).
join_underscores(V,V).

:- fixup_exports.

:- multifile(term_expansion/2).
:- dynamic(term_expansion/2).
term_expansion(X,Y):- notrace((compound(X), unity_uctx, uterm_expansion(X,Y),X\==Y,ansicall(yellow,print_clexp(ce,X,Y)))).
system:goal_expansion(X,Y):- notrace((compound(X), unity_uctx, \+ source_file_expansion, expand_ugoal(X,Y),X\==Y,ansicall(blue,print_clexp(ge,X,Y)))).
:- load_unity_prolog_file('Utilities/startup.prolog').

ltest:- 
  nb_setval(c,writeln(hi_c)),
  P =  (b:-if_uctx_equals(a), '::'(a, $c)),
  assert_if_new(P),
  '::'(a,b).


end_of_file.

%:- dynamic($/2).
Warning: baseKB:calc_to_hashmap/3, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_xtra/logicmoo_chat/npc/Assets/unity_prolog.pl:141:21: 1-st clause of baseKB:calc_to_hashmap/1
Warning: baseKB:would/1 is declared as discontiguous, but has no clauses
Warning: baseKB:veto_strategy/1 is declared as discontiguous, but has no clauses
Warning: baseKB:transcript/1 is declared as discontiguous, but has no clauses
Warning: baseKB:trace_task/2 is declared as discontiguous, but has no clauses
Warning: baseKB:trace_reduction/1 is declared as discontiguous, but has no clauses
Warning: baseKB:threatening_to_stop/2 is declared as discontiguous, but has no clauses
Warning: baseKB:sup/2 is declared as discontiguous, but has no clauses
Warning: baseKB:special_qud/2 is declared as discontiguous, but has no clauses
Warning: baseKB:should/1 is declared as discontiguous, but has no clauses
Warning: baseKB:shall/1 is declared as discontiguous, but has no clauses
Warning: baseKB:self_defense/2 is declared as discontiguous, but has no clauses
Warning: baseKB:revealed/1 is declared as discontiguous, but has no clauses
Warning: baseKB:recent_dialog/1 is declared as discontiguous, but has no clauses
Warning: baseKB:quip/3 is declared as discontiguous, but has no clauses
Warning: baseKB:quip/2 is declared as discontiguous, but has no clauses
Warning: baseKB:prefer_strategy/3 is declared as discontiguous, but has no clauses
Warning: baseKB:preemption/0 is declared as discontiguous, but has no clauses
Warning: baseKB:possession/2 is declared as discontiguous, but has no clauses
Warning: baseKB:plot_relevant_assertion/4 is declared as discontiguous, but has no clauses
Warning: baseKB:plot_question_introduced/1 is declared as discontiguous, but has no clauses
Warning: baseKB:plot_question_flavor_text/2 is declared as discontiguous, but has no clauses
Warning: baseKB:plot_question_answered/1 is declared as discontiguous, but has no clauses
Warning: baseKB:plot_event/1 is declared as discontiguous, but has no clauses
Warning: baseKB:player_character/0 is declared as discontiguous, but has no clauses
Warning: baseKB:player_achieves_task_during_beat/2 is declared as discontiguous, but has no clauses
Warning: baseKB:past/1 is declared as discontiguous, but has no clauses
Warning: baseKB:parser_opt_pp/5 is declared as discontiguous, but has no clauses
Warning: baseKB:on_exit_state/3 is declared as discontiguous, but has no clauses
Warning: baseKB:noun/3 is declared as discontiguous, but has no clauses
Warning: baseKB:necessary/2 is declared as discontiguous, but has no clauses
Warning: baseKB:may/1 is declared as discontiguous, but has no clauses
Warning: baseKB:log_when_added_action/2 is declared as discontiguous, but has no clauses
Warning: baseKB:log_events/1 is declared as discontiguous, but has no clauses
Warning: baseKB:knows_value/2 is declared as discontiguous, but has no clauses
Warning: baseKB:know/1 is declared as discontiguous, but has no clauses
Warning: baseKB:intend/2 is declared as discontiguous, but has no clauses
Warning: baseKB:good_ending/1 is declared as discontiguous, but has no clauses
Warning: baseKB:future/1 is declared as discontiguous, but has no clauses
Warning: baseKB:failed_task/2 is declared as discontiguous, but has no clauses
Warning: baseKB:explanation/2 is declared as discontiguous, but has no clauses
Warning: baseKB:examined/1 is declared as discontiguous, but has no clauses
Warning: baseKB:everyday_life_task/1 is declared as discontiguous, but has no clauses
Warning: baseKB:consent/2 is declared as discontiguous, but has no clauses
Warning: baseKB:combinatoric/1 is declared as discontiguous, but has no clauses
Warning: baseKB:clue_flavor_text/2 is declared as discontiguous, but has no clauses
Warning: baseKB:clue/1 is declared as discontiguous, but has no clauses
Warning: baseKB:can/1 is declared as discontiguous, but has no clauses
Warning: baseKB:begin_csv_loading/1 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_start_task/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_sequel/2 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_priority/2 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_precondition/2 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_monolog/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_menu_question/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_menu_command/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_menu_automa_command/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_menu_assertion/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_menu_action/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_leads_to_event/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_is_character_reaction/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_idle_task/3 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_follows/2 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_expected_during/2 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_excursion/2 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_dialog/4 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_delay/2 is declared as discontiguous, but has no clauses
Warning: baseKB:beat_completion_condition/2 is declared as discontiguous, but has no clauses
Warning: baseKB:beat/1 is declared as discontiguous, but has no clauses
Warning: baseKB:bad_ending/1 is declared as discontiguous, but has no clauses
Warning: baseKB:aux_aspect/7 is declared as discontiguous, but has no clauses
Warning: baseKB:==>>/2 is declared as discontiguous, but has no clauses

