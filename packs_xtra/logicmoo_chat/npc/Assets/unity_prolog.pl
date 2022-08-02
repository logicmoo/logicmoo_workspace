% :- module(unity_prolog,[]).

:- dynamic(tmpu:(is_unity_file/1)).
:- dynamic(lmconf:unity_module_name/1).

%:- set_prolog_flag(occurs_check,error).

make_pred_narity_call_list(P,A):- 
  length(L,A),
  Head=..[P|L],Body=..[P,L],
  assert_if_new((Head:-Body)).

begin(G):- locally(t_l:pretend_expansion(call),unity_begin(G)).

unity_begin(G):- is_list(G),!,maplist(unity_begin,G).
unity_begin(G):- unity_call(G),!.


:- forall(between(2,11,A), make_pred_narity_call_list(begin,A)).
displayln(X):- is_list(X),!,maplist(displayln0,X),nl.
displayln(X):- displayln0(X), nl.
displayln0(C):- atomic(C),!,write(C).
displayln0(C):- var(C),!, writeq(C).
%displayln0(_):- !.
%displayln0(print(C)):- !, write(' '),print_tree(C),write(' ').
displayln0(C):- write_term(C,[attributes(dots),quoted(true)]).

:- forall(between(2,7,A), make_pred_narity_call_list(displayln,A)).

:- arithmetic_function('$'/1).
:- arithmetic_function('now'/0).

:- op(1100,fx,@),
   op(1100,xfy,:=),
   op(1100,xfy,:^).

:- multifile(menu_action/2).

:- npc_chat:op(800,fx,'/').
:- op(900,xfy,'::').
:- npc_chat:op(399,xfy,':').

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

dumpST_throw(G):- dumpST,throw(G).

:- meta_predicate(system:unity_call(:)).

unity_call_p(O,P):- t(P,O,V),V==true.

system:unity_call(MG):- strip_module(MG,M,G), unity_call(M,G).

system:unity_call(M,G):- \+ callable(G),!,dumpST_throw(uncallable_unity_call(M:G)).
system:unity_call(M,public(Arg)):- !, apply(M:(external),[Arg]).
%system:unity_call(M,assert(Arg)):- !,system:unity_call(M,assert(Arg)).
system:unity_call(M,G):-
  expand_goal(G,GG), GG\==G, unity_call(G)\==GG, !, ansicall(cyan, (print_clexp(call,G,GG))),!,  M:call(GG).
system:unity_call(M,G):- call(M:G).
%system:unity_call(M:G):- G=..[F|Args],unity_apply(M:F,Args).
%ugoal_expansion(B,BBB):- once(uclause_expansion_inner(hb,goal,B,goal:-BB)),B\==BB,!,expand_goal(BB,BBB).
:- meta_predicate(unity_db_call(:,+)).
%unity_db_call(F,Args):- maplist(expand_assert,[F],Args,EArgs),!,unity_apply(F,EArgs).
unity_db_call(F,Args):- catch(unity_apply(F,Args),E,(dumpST,dmsg(E=unity_apply(F,Args)),rtrace(unity_apply(F,Args)))).


:- meta_predicate(system:unity_apply(:,+)).
system:unity_apply(M:F,Args):- system:unity_apply(M,F,Args).
:- meta_predicate(system:unity_apply(+,+,+)).
system:unity_apply(M,(public),[Arg]):- !, apply(M:(external),[Arg]).
%system:unity_apply(M,F,Args):- \+ ground(Args), Call=..[F|Args],log(unity_apply_call(M:Call)), fail.
system:unity_apply(M,F,[Arg|Rest]):- db_pred(F),expand_assert([F,module(M),db_pred],Arg,Slash),Arg\==Slash,!, system:unity_apply(M:F,[Slash|Rest]).
system:unity_apply(M,F,Args):- M:apply(F,Args).

expand_assert(Why,I,O):-
  expand_slash(I,II), convert_assert(Why,II,O),nop(ansicall(fushia, print_clexp(Why,I,O))),!.
expand_assert(Why,O,O):- dumpST_throw(cant_expand_assert(Why,O)).

convert_assert(_Why,Arg,Slash):- expand_uterm(Arg,Slash),!.
/*
convert_assert(_Why,G,G):- \+ compound(G),!.
convert_assert(_Why,X,X):- \+ \+ X = uslash(_,_),!.
convert_assert(Why,'::'(G,Arg),Slash):- G == $global,!, lmconf:unity_module_name(M), expand_assert(Why,M:Arg,Slash).
convert_assert(Why,M:Arg,M:Slash):- !,expand_assert(Why,Arg,Slash).
convert_assert(_Why,C,O):- no_more_expansion(a,C,O),!.
convert_assert(Why,B,(O)):- get_var_expansions(a,B,M,G),G\==true,immediate_expansion,!,call(G),!,expand_assert(Why,M,O).
convert_assert(Why,B,(O:-G)):- get_var_expansions(h,B,M,G),G\==true,!,expand_assert(Why,M,O).
convert_assert(_Why,Arg,Slash):-  maybe_into_slash_db(Arg,Slash),!.
convert_assert(Why,X,Y):- 
  compound_name_arguments(X,F,AX),
  maplist(expand_assert([F|Why]),AX,AY),
  compound_name_arguments(Y,F,AY),!.

:- system:import(convert_assert/3).
*/

maybe_into_slash_db(C,_):- \+ compound(C),!,fail.
maybe_into_slash_db(uslash(X,Y),uslash(X,Y)):- !.
%maybe_into_slash_db(F/A,_):- atom(F),integer(A), !,fail.
maybe_into_slash_db(F/_,_):- \+ slash_arg(F),!,fail.
maybe_into_slash_db(_/A,_):- \+ slash_arg(A),!,fail.
%maybe_into_slash_db(M'::' /(A),uslash(M,/(A))).
%maybe_into_slash_db(M::A,uslash(M,AA)):- maybe_into_slash_db(A,AA).
maybe_into_slash_db(C,TO):- \+ \+ (copy_term_nat(C,Nat),(numbervars(Nat,0,_,[attvar(skip)]),is_slashed_g(Nat))),to_slash_db(C,top,TO).
maybe_into_slash_db(C,TO):- \+ \+ (copy_term_nat(C,Nat),(numbervars(Nat,0,_,[attvar(skip)]),is_slashed(Nat))),to_slash_db(C,rel,TO).

slash_arg(E):- var(E),!.
slash_arg(E):- atom(E),!.
slash_arg(E):- compound(E),functor(E,F,A),slash_arg_f(F,A).
slash_arg_f($,1).
slash_arg_f(/,2).
slash_arg_f(/,1).
slash_arg_f('>>',2).

slash_arg_f(:,2).

is_slashed(S>>_):- is_slashed(S).
is_slashed(_/_).
is_slashed(_/_:_).
is_slashed_g(S>>_):- is_slashed_g(S).
is_slashed_g(/_/_).
is_slashed_g(/_:_).
is_slashed_g((/_)/_).
is_slashed_g((/_):_).

%uslash(X,Y):- log(uslash(X,Y)),fail.
%uslash(me,_X):- fail.

to_slash_db(C,Top,uslash(Top,C)) :- !.
%to_slash_db(C,Top,uslash(Top,O)) :- subst(C,'/','[|]',O).

slash_top(Rel):- arg(_,e(rel,top,this,me,global),Rel).

uslash_call(X,Y):- uslash(X,Y).
uslash_call_top(Y):- compound(Y), (Y = (L>>R)),!,uslash_completion('/'(L),R).
uslash_call_top(Y):- uslash(top,Y).
uslash_completion(X,Y):- dmsg(uslash_completion(X,Y)),fail.
uslash_completion(/(X),Y):- uslash(top,X/Y).
uslash_completion((X),Y):- uslash(rel,X/Y).

uslash(Rel,X):- var(Rel),!,slash_top(Rel),uslash(Rel,X).
uslash(#(Rel),X):- !, uslash(Rel,X).
uslash($(Rel),X):- getvar(Rel,From), !, uslash(From,X).
%uslash(top,/(X)):- uslash(me,X).
%uslash(rel,X):- uslash(this,X).
uslash(me,/(X)):- me_path(Root),!,extend_path(Root,X,Full),uslash(full,Full).
uslash(this,X):- this_path(Root),!,extend_path(Root,X,Full),uslash(full,Full).
%uslash(top,X):- !, uslash(me,X).
uslash(Rel,Path/Name:Value):- gen_pnv(Path,Name,Value,PathVarValue), Path/Name:Value\==PathVarValue,uslash(Rel,PathVarValue).


gen_pnv(Path,Name,Value,Path/Name:Value).
gen_pnv(Path,Name,Value,Path/VarValue):- VarValue\=(_:_),VarValue=Name,Value=true.

:- dynamic(ufslash/1).

extend_path(_,SFirst,SFirst):- compound(SFirst), SFirst = '/'(_),!.
extend_path(X,Y,XY):- get_slash(Y,YY),Y\==YY,!,extend_path(X,YY,XY).
extend_path(X,Y,XY):- get_slash(X,XX),X\==XX,!,extend_path(XX,Y,XY).
extend_path(Y,This,Y):- This==this, !.
%extend_path(X,'/'(Y),'/'(XX,YY)):- get_slash(X,XX),get_slash(Y,YY).
extend_path(X,Y,X/Y).
slash_props(Path,Name,Value):- gen_pnv(Path,Name,Value,PathVarValue),uslash(rel,PathVarValue).

get_slash(X,X):- \+ compound(X),!.
get_slash(uslash(rel,X),XX):- get_slash(X,XX).
get_slash(uslash(this,X),X):-!.
get_slash(uslash(top,X),X):-!.
get_slash(uslash(Var,X),Y):- atom(Var), getvar(Var,Val),!,get_slash(uslash(Val,X),Y).
get_slash(uslash(Var,X),Var/X):- !. 
get_slash(X,X).

uslashv(ME,X,V):- getvar(me,ME),uslash(_,Y),slash_to_list(Y,Z), (append([ME|X],[val(V)],Z)-> true ; (append([ME|X],[],Z),V=true)).

slash_to_list(X,Y):- slash_to_list0(X,Y),!.
%slash_to_list(X,Y:true):-slash_to_list0(X,Y).
slash_to_list0(XY,[XY]):- \+ compound(XY),XY\==[],!.
slash_to_list0([],[]):-!.
slash_to_list0(X:V,VXX):-!,slash_to_list0(X,XX),append(XX,[val(V)],VXX). 
slash_to_list0(X/Y,XXYY):- !, slash_to_list0(X,XX),slash_to_list0(Y,YY),append(XX,YY,XXYY). 
%slash_to_list0(X/Y,[X|List]):- \+ compound(X),!,slash_to_list0(Y,List).
%slash_to_list0(X/Y,XXYY):- !, slash_to_list0(X,XX),slash_to_list0(Y,YY),append(XX,YY,XXYY). 
%slash_to_list0(X/YV,List=V):- nonvar(YV),!,(YV = Y:V), slash_to_list0(X/Y,List).
%slash_to_list0(X/Y,ListY:true):- \+ compound(Y),!,slash_to_list0(X,List),append(List,[Y],ListY).
%slash_to_list0(XY,[XY]):- \+ compound(XY),!.
slash_to_list0(XY,[XY]).

%uslash(rel,X):- uslash(($this)/X).
%uslash(me,X):- getvar(me,Me),extend_path(Me,X,Full),uslash(full,Full).

me_path( R ):- getvar(global,G), getvar(me,Me), extend_path(G, Me, R).
this_path( R ):- me_path( W ), extend_path(W, this, R).

:- thread_local(t_l:(unity_this_path/1)).
'::'(V,G):- var(V),!,get_uctx_equals(V),!,'::'(V,G).
'::'(C,G):- dv(C,N),getvar(N,V),!,'::'(V,G).
'::'(parent,G):- !, '::'(up,G).
'::'(Me=Foo,G):- !, locally(b_setval(Me,Foo),'::'($me,G)).
%'::'(parent,G):- get_uctx_equals(P), trans_p_to_v(P,up,V),!, locally(t_l:unity_this_path(V), locally(nb_setval(this,V), begin(G))).
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
calc_to_hashmap(/Nextuid,O,iz):- !, getvar(me,Y),extend_path(Y,Nextuid,O).
calc_to_hashmap(Nextuid,O,r):- !, getvar(this,Y),extend_path(Y,Nextuid,O).
*/

/*
calc_full_slash_db(/X,X,Full):- !, getvar(me,Y),extend_path(Y,X,Full).
calc_full_slash_db(M'::'From,X,Full):- '::'(M,calc_full_slash_db(From,X,Full)).
calc_full_slash_db(From,X,Full):- getvar(this,Y),extend_path(From,X,Full).
calc_full_slash_db(me,X,Full):- getvar(me,Y),extend_path(Y,X,Full).
calc_full_slash_db(rel,X,Full):- getvar(this,Y),extend_path(Y,X,Full).
*/
expand_slash(X,Y):- notrace(slash_exp(X,Y)),!.
expand_slash(X,X):- dumpST_throw(failed_expand_slash(X)).

slash_exp(C,O):- \+ compound(C),!,C=O.
slash_exp(library(X),library(X)):-!.
slash_exp(is(X,Y),is(X,Y)):-!.
slash_exp(C,O):- no_more_expansion(g,C,O),!.
slash_exp(G,G):- compound_name_arity(G,B,_),atom_contains(B,'reduc'),!.
slash_exp(X,Y):- maybe_into_slash_db(X,Y),!.
%slash_exp(X:-B,Y:-B):- maybe_into_slash_db(X,Y),!.
%slash_exp(X,Y):- compound(X),once(ugoal_expansion(X,Y)),X\==Y.

%slash_exp(G'::'X,G'::'Y):- !,expand_slash(X,Y).
%slash_exp(G'::'X,Y):- G == $global,!,expand_slash(X,Y).
%slash_exp(G'::'X,M:Y):- var(G),G=M,!,expand_slash(X,Y).

%slash_exp(G:X,M:Y):- G=M,!,expand_slash(X,Y).
%slash_exp('::'(G,X),M:Y):- nonvar(G),G='$'(M),slash_exp(X,Y).
%slash_exp(G'::'X,G{}.X):- !.
%slash_exp($Sophia,K{k:K,name:Sophia}).
slash_exp(X,Y):- compound(X),!,
  compound_name_arguments(X,F,AX),
  maplist(expand_slash,AX,AY),
  compound_name_arguments(Y,F,AY).

% functor_expansion('::',':').
functor_expansion(F,F).


dv(E,V):-compound(E), (E= ('$'(V))), nonvar(V), V \== global. % , V \== this, V \== me.
%uclause_expansion_inner(hb,H,B,HHBB):- sub_uterm(b,E,H),dv(E,V),usubst(H,E,Var,HH),!,
%  conjoin(getvar(V,Var),B,BB),uclause_expansion_inner(hb,HH,BB,HHBB).
%uclause_expansion_inner(hb,H,B,HHBB):- sub_uterm(b,E,B),dv(E,V),usubst(B,E,Var,BB),!,
%  conjoin(getvar(V,Var),BB,BBB),uclause_expansion_inner(hb,H,BBB,HHBB).
uclause_expansion_inner(hb,H,B,HB):-  as_clause_hb(H,B,HB).

uclause_expansion_inner(dcg,H,B,HHBB):- sub_term(E,H),dv(E,V),subst(H,E,Var,HH),!,
  conjoin({getvar(V,Var)},B,BB),uclause_expansion_inner(dcg,HH,BB,HHBB).
uclause_expansion_inner(dcg,H,B,HHBB):- sub_uterm(h,E,B),dv(E,V),subst(B,E,Var,BB),!,
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

as_clause_hb0('::'( Ctx,H),B,((H:- nonvar(Ctx), Ctx::BB))):- var(Ctx),!, expand_ugoal(B,BB).
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

expand_uterm(C,C):- \+ compound(C),!.
expand_uterm(C,O):- no_more_expansion(h,C,O),!.
expand_uterm(M:I,M:O):- !, expand_uterm(I,O),!.
expand_uterm($global::I,O):- !, expand_uterm(I,O),!.
expand_uterm(I,O):- copy_term(I,II),expand_uterm0(I,O),!,notrace(ignore(study_body(II,_))).
expand_uterm(I,O):- throw(failed_expand_uterm(I,O)).

expand_uterm0(C,C):- \+ compound(C),!.
expand_uterm0(:-B,:-BB):- !, expand_ugoal(B,BB),!.
expand_uterm0(H-->T,HBO):- expand_dcg_body(T,B), !, dcg_translate_rule((H-->B),HB),!,expand_uterm(HB,HBO).
expand_uterm0(C,O):- no_more_expansion(h,C,O),!.
expand_uterm0(H:-B,HB):- !, typed_uclause_expansion(hb,H,B,HB),!.
expand_uterm0(H,HB):- typed_uclause_expansion(hb,H,true,HB),!.
expand_uterm0(H,H).
%expand_uterm(H,HB):- expand_slash(H,HB).

study_body(T,T):- var(T),!.
study_body(C, O):- dont_reduce(C),!,O=C.
study_body(P,iz(uslash)):- maybe_into_slash_db(P,_),!.
study_body(S,iz(string)):-string(S),!.
study_body(S,iz(number)):-number(S),!.
study_body(T,_):- \+ compound(T),!.
study_body(Dyn,Dyn):-  compound_name_arity(Dyn,F,1), current_op(X,Y,dynamic), \+ upcase_atom(F,F), current_op(X,Y,F),current_predicate(F/1),!.
%study_body((_/_),iz(uslash)):-!.
%study_body(/(_),iz(uslash)):-!.
study_body(log(Comp),log(Comp)):- !.
study_body(nop(_),_):-!.
study_body(throw(_),_):-!.
study_body(getvar(_,_),_):-!.
study_body(DCGText,iz(list)):- DCGText=@=[_|_],!.
study_body(DCGText,O):- DCGText=[_|_], member_no_open(E,DCGText),\+is_list(E),compound(E),O=[E],!.
study_body(mexp(Comp),CmpO):- !, study_body(Comp,CmpO).
study_body(_:Comp,CmpO):- !, study_body(Comp,CmpO).
study_body((B,A),AA):- !, study_body(A,AA), study_body(B,_),!.
study_body((A;B),AA):- !, study_body((A,B),AA),!.
study_body((A:-B),AA):- !, study_body((A,B),AA),!.
study_body((A->B),AA):-  !, study_body((A,B),AA),!.
study_body((A*->B),AA):-  !, study_body((A,B),AA),!.
study_body(\+ (B),O):- !, study_body(B,O).
study_body('$VAR'(V),'$VAR'(V)):- !.
study_body(_=Comp,CmpO):- !, study_body(Comp,CmpO).
study_body(writeln(_),iz(call)):- !.
study_body(call(Comp),CmpO):- !, study_body(Comp,CmpO).
study_body(_::Comp,CmpO):- !, study_body(Comp,CmpO).
study_body(_^Comp,CmpO):- !, study_body(Comp,CmpO).
study_body(findall(_,Comp,_),CmpO):- !, study_body(Comp,CmpO).
study_body(Cmp,CmpO):- compound_name_arguments(Cmp,F,[Comp|_]),db_pred(F),!,study_body(Comp,CmpO).
study_body(all(_,Comp,_),CmpO):- !, study_body(Comp,CmpO).
study_body(Comp,CmpO):-
  strip_module(Comp,_,Cmp),
  compound_name_arguments(Cmp,F,Args),
  upcase_atom(F,F), % is meta?
  maplist(study_body,Args,ArgsO),
  compound_name_arguments(CmpO,F,ArgsO),!.
study_body(Cmp,CmpO):- 
  compound_name_arguments(Cmp,F,Args),
  maplist(study_body,Args,ArgsO),
  compound_name_arguments(CmpO,F,ArgsO),
  ignore((\+ \+ grok_cmp(CmpO))),!.
study_body(Comp,CmpO):- dumpST_throw(cant_study_body(Comp,CmpO)).

share_functor(C,O):- share_functor(C,O,_F).
share_functor(C,O,F):- compound(C), compound_name_arity(C,F,A),compound_name_arity(O,F,A),!.
share_functor(C,O,F):- compound(O), compound_name_arity(O,F,A),compound_name_arity(C,F,A),!.
share_functor(C,O,_):- throw(failed_share_functor(C,O)).

share_arg(Args,ArgsT):- atom(Args),var(ArgsT),!,nop(share_arg(ArgsT,Args)). 
share_arg(Args,ArgsT):- atom(ArgsT),!,assert_arg_type(Args,ArgsT).
share_arg(_Args,_ArgsT).

assert_arg_type(Args,ArgsT):- var(Args),!, freeze(Args,share_arg(Args,ArgsT)).
assert_arg_type(iz(Args),ArgsT):- assert_subtype( Args,ArgsT).
assert_arg_type(Args,ArgsT):- compound(Args), assert_result_isa(Args,ArgsT).
assert_arg_type(Args,ArgsT):- assert_arg_isa(Args,ArgsT).

assert_result_isa(Args,Args):- !. 
assert_result_isa(Args,ArgsT):- log_assert(decl_asap(result_isa(Args,ArgsT))).

log_assert(G):- log(ap(G)),assert_if_new(G).
assert_arg_isa(Args,ArgsT):- log_assert(decl_asap(arg_isa(Args,ArgsT))).

assert_subtype( Args,Args):- !. 
assert_subtype( Args,ArgsT):-  log_assert(decl_asap(subtype( Args,ArgsT))).


share_args(C,O):- share_functor(C,O),compound_name_arguments(C,F,Args),compound_name_arguments(O,F,ArgsT),maplist(share_arg,Args,ArgsT).
share_args(C,O):- dumpST_throw(failed_share_args(C,O)).

:- dynamic(mexp/1).
grok_cmp(Cmp):- 
  shallow(Cmp,CmpO),
  ignore((share_functor(CmpO,CmpS),predicate_type(_,CmpS),share_args(CmpO,CmpS))),
  ignore((share_functor(CmpO,CmpX),mexp(CmpX),share_args(CmpO,CmpX))),
  ignore((contains_compound(CmpO),call(assert_if_new,mexp(CmpO)),log(mexp(CmpX-->CmpO)))).

contains_compound(CmpS):- compound(CmpS),arg(_,CmpS,E), compound(E),\+ is_list(E),!.

dont_reduce(Var):- var(Var),!.
dont_reduce(Var):- \+ compound(Var),!,fail.
dont_reduce(iz(T)):- !, nonvar(T).
dont_reduce(G):- compound_name_arity(G,F,A),dont_reduce_f_a(F,A),!.
dont_reduce((A,B)):-!,dont_reduce(A),dont_reduce(B).
dont_reduce((A:B)):-!,dont_reduce(A),dont_reduce(B).
%dont_reduce(Cmp):- compound_name_arguments(Cmp,_,[Atom,Compd]),atom(Atom),compound(Compd),\+ is_list(Compd).

dont_reduce_f_a(expand_goal,_).
dont_reduce_f_a(expand_term,_).
dont_reduce_f_a(expand_ugoal,_).
dont_reduce_f_a(expand_uterm,_).
dont_reduce_f_a(goal_expansion,_).
dont_reduce_f_a(term_expansion,_).
dont_reduce_f_a(ugoal_expansion,_).
dont_reduce_f_a(uterm_expansion,_).
dont_reduce_f_a(unity_call,_).
dont_reduce_f_a(unity_call_db,_).
dont_reduce_f_a(unity_apply,_).
dont_reduce_f_a(predicate_type,_).
dont_reduce_f_a(iz,_).
dont_reduce_f_a(log,_).

shallow(C, O):- dont_reduce(C),!,O=C.
shallow(Cmp,CmpO):- 
  compound_name_arguments(Cmp,F,Args),
  maplist(no_subcmps,Args,ArgsO),
  compound_name_arguments(CmpO,F,ArgsO).

no_subcmps(C,C):- dont_reduce(C),!.
no_subcmps(C,_):- \+ compound(C),!.
no_subcmps(P,iz(uslash)):- maybe_into_slash_db(P,_),!.
no_subcmps(iz(_),_):- !.
no_subcmps($(_),_):- !.
no_subcmps(#(_),_):- !.
no_subcmps(/(_),_):- !.
no_subcmps(/(_,_),_):- !.
no_subcmps(C,T):- compound_name_arity(C,F,A),compound_name_arity(O,F,A),maybe_pred_type(O,T).

maybe_pred_type(C,C):- dont_reduce(C),!.
maybe_pred_type(O,T):- predicate_type(T,O),!.
maybe_pred_type(O,O).

mexp:-
   listing(mexp/1),
   forall(retract(mexp(P)), study_body(P,_)),
   setup_call_cleanup(open(mpred,write,S),with_output_to(S,listing(mexp/1)),close(S)).

%expand_dcg_body(T,T):- !.
expand_dcg_body(T,T):- \+ compound(T),!.
expand_dcg_body(T,T):- compound_name_arity(T,F,_),atom_concat('the',_,F),!.
expand_dcg_body({T},{T}).
expand_dcg_body(DCGText,TheText):- DCGText=[_|_], wrap_words(DCGText,TheText),!,ignore(( \+ ground(DCGText), 
  source_location(S,F), wdmsg(source_location(S,F)), log(wrap_words(DCGText,TheText)))),!.
expand_dcg_body(Comp,CmpO):-
  meta_pred_style(Comp,Meta),
  strip_module(Comp,_,Cmp),
  compound_name_arguments(Cmp,F,Args),
  compound_name_arguments(Meta,_,MArgs),
  maplist(meta_expansion(expand_dcg_body),MArgs,Args,ArgsO),!,
  compound_name_arguments(CmpO,F,ArgsO).
expand_dcg_body(Cmp,CmpO):-
  compound_name_arguments(Cmp,F,Args),
  maplist(expand_only_meta(expand_dcg_body),Args,ArgsO),!,
  compound_name_arguments(CmpO,F,ArgsO).
expand_dcg_body(Comp,CmpO):- throw(cant_expand_dcg_body(Comp,CmpO)).

expand_only_meta(_,A,A):- \+ compound(A),!.
expand_only_meta(_,{T},{T}).
expand_only_meta(Exp,Comp,CmpO):-
  meta_pred_style(Comp,Meta),
  strip_module(Comp,_,Cmp),
  compound_name_arguments(Cmp,F,Args),
  compound_name_arguments(Meta,_,MArgs),
  maplist(meta_expansion(Exp),MArgs,Args,ArgsO),!,
  compound_name_arguments(CmpO,F,ArgsO).
expand_only_meta(Exp,Cmp,CmpO):-
  compound_name_arguments(Cmp,F,Args),
  maplist(expand_only_meta(Exp),Args,ArgsO),!,
  compound_name_arguments(CmpO,F,ArgsO).
expand_only_meta(Exp,Comp,CmpO):- throw(cant_expand_only_meta(Exp,Comp,CmpO)).

meta_expansion(_Exp,V,A,A):- var(V),!,shouldnt_need_expansion(A).
meta_expansion(_Exp,+,A,A):- shouldnt_need_expansion(A).
meta_expansion(_Exp,-,A,A):- shouldnt_need_expansion(A).
meta_expansion(_Exp,?,A,A):- shouldnt_need_expansion(A).
meta_expansion(Exp,_,X,Y):- call(Exp,X,Y),!.
meta_expansion(_,_,_,_).


wrap_words([H|T],theTextM1(H)):- T == [],!.
wrap_words([H|T],[H|T]):- member_no_open(E,[H|T]),compound(E),!,dmsg(no_wrap_words([H|T])). % other kind of DCG
wrap_words(L,theTextM(L)).


same_s(MX,X):- atom(MX),atom(X),!, upcase_atom(MX,MXU),upcase_atom(X,XU),!,XU==MXU.
same_s(X,X).

theTextM1(X) --> [M],{(compound(M)->arg(1,M,MX);M=MX),same_s(MX,X)}.
theTextM([X|L]) --> theTextM1(X),!,theTextML(L).
theTextML([]) --> [].
theTextML([X|L]) --> theTextM1(X),theTextM(L).

enforce_set(Set):- term_variables(Set,Vars),maplist(freeze_as_set(Set),Vars).
freeze_as_set(Set,Var):- 
  A = npc_chat, %freeze_as_set(Set,Var):- freeze(Var,check_set(Set)).
  (get_attr(Var,A,ListOfSets) 
   -> ((member(E,ListOfSets),E==Set) -> true ; put_attr(Var,A,[Set|ListOfSets]))
   ; put_attr(Var,A,[Set])).
attr_unify_hook(ListOfSets,_):- maplist(check_set,ListOfSets).
check_set(Set):- check_set2(Set,Set).
check_set2(Set,Var):- var(Var),!,freeze_as_set(Set,Var).
check_set2(Set,[]):- !,enforce_set(Set).
check_set2(Set,[Var|Rest]):- var(Var),freeze_as_set(Set,Var),!,check_set2(Set,Rest).
check_set2(Set,[Var|Rest]):- \+ (member_no_open(E,Rest), E==Var),!,check_set2(Set,Rest).
member_no_open(_,Rest):- var(Rest),!,fail.
member_no_open(E,[V|Rest]):- (V=E ; member_no_open(E,Rest)),!.


:- dynamic(is_meta_predicate/1).

:- meta_predicate(meta_pred_style(+,-)).
meta_pred_style(Cmp,Meta):- 
  strip_module(Cmp,M,Pred),
  compound(Pred),compound_name_arity(Pred,F,A), 
  compound_name_arity(Meta,F,A),
  meta_pred_style(M,Pred,Meta),  
  \+ never_meta_fa(F,A).

:- meta_predicate(meta_pred_style(+,?,?)).
meta_pred_style(_,Cmp,_):- var(Cmp),!,fail.
meta_pred_style(_,Cmp,Meta):- compound_name_arity(Cmp,F,A), compound_name_arity(Meta,F,A),meta_pred_style(Meta).
meta_pred_style(M,Cmp,Meta):- predicate_property(M:Cmp,meta_predicate(Meta)).

meta_pred_style((+ :: 0)).
meta_pred_style((+ : 0)).
meta_pred_style('$'(?)).
meta_pred_style({0}).
meta_pred_style(X):- is_meta_predicate(X).
meta_pred_style('//' => '//').
meta_pred_style('//' --> '//').

never_meta_fa('/',_).
% never_meta_fa(':',_).
never_meta_fa('$',_).
never_meta_fa('=>',_).
never_meta_fa('#',_).
never_meta_fa(F,A):- functor([list],F,A).

%meta_pred_style(Cmp,Meta):- predicate_property(Cmp,transparent),!,functor(Cmp,F,A),functor(Meta,F,A).

maybe_readd_module(CmpI,CmpM,M:CmpM):- strip_module(CmpI,M,Cmp),CmpI\==Cmp,!.
maybe_readd_module(_,CmpM,CmpM).

ugoal_expansion_3(V,A,A):- var(V),!,shouldnt_need_expansion(A).
ugoal_expansion_3('+',A,A):- shouldnt_need_expansion(A).
ugoal_expansion_3('-',A,A):- shouldnt_need_expansion(A).
ugoal_expansion_3('?',A,A):- shouldnt_need_expansion(A).
ugoal_expansion_3(':',A,A):- !.
ugoal_expansion_3(N,In,Out):- number(N),expand_ugoal(In,Out).
ugoal_expansion_3(_,In,Out):- expand_ugoal(In,Out).

expand_ugoal(In,Out):- copy_term(In,II),expand_ugoal0(In,Mid),expand_slash(Mid,Out),!,notrace(ignore(study_body(II,_))).
expand_ugoal(In,Out):- dumpST_throw(failed_expand_ugoal(In,Out)).

expand_ugoal0(In,Out):- no_more_expansion(g,In,Out),!.
%expand_ugoal0('::'(V,G),'::'(V,GG)):-var(V),!,expand_ugoal(G,GG).
%expand_ugoal0('::'($N,G),(getvar(N,V),'::'(V,G))):-atomic(N),debug_var(N,V).
expand_ugoal0(In,Out):- ugoal_expansion(In,Mid),!,(Mid==In->Out=In;(expand_goal(Mid,OutM),ugoal_expansion(OutM,Out))).
expand_ugoal0(In,Out):- dumpST_throw(failed_expand_ugoal0(In,Out)).

shouldnt_need_expansion(_):- !.
shouldnt_need_expansion(In):- expand_term(In,Out),assertion(Out==In,shouldnt_need_expansion(In-->Out)).


ugoal_expansion(C,C):- \+ compound(C),!.
ugoal_expansion(public(X),unity_call(public(X))).
ugoal_expansion(\+ C , \+ O):- !, ugoal_expansion(C,O).
ugoal_expansion($V,(getvar(V,G),call(G))):-atomic(V),debug_var(V,G).
ugoal_expansion((A;B),(AA;BB)):- !, ugoal_expansion(A,AA),ugoal_expansion(B,BB).
ugoal_expansion((A->B),(AA->BB)):- !, ugoal_expansion(A,AA),ugoal_expansion(B,BB).
ugoal_expansion((A*->B),(AA*->BB)):- !, ugoal_expansion(A,AA),ugoal_expansion(B,BB).
ugoal_expansion( ','(Ctx,Body),CtxCall):-  get_var_expansions(g,'::'(Ctx,Body),'::'(CtxO,BodyO),G), G\==true,!,conjoin(G,','(CtxO,BodyO),CtxCall).
ugoal_expansion((A,B),(AA,BB)):- !, ugoal_expansion(A,AA),ugoal_expansion(B,BB).
ugoal_expansion('::'(Ctx,Body),CtxCall):-  get_var_expansions(g,'::'(Ctx,Body),'::'(CtxO,BodyO),G), G\==true,!,conjoin(G,'::'(CtxO,BodyO),CtxCall).
ugoal_expansion('::'(Ctx,Body),'::'(Ctx,BodyO)):- !, ugoal_expansion(Body,BodyO).
ugoal_expansion(':'(Ctx,Body),CtxCall):-  get_var_expansions(g,'::'(Ctx,Body),'::'(CtxO,BodyO),G), G\==true,!,conjoin(G,':'(CtxO,BodyO),CtxCall).
ugoal_expansion(':'(Ctx,Body),':'(Ctx,BodyO)):- !, ugoal_expansion(Body,BodyO).




%ugoal_expansion((A,B),(AA,BB)):- get_var_expansions(h,A,AH,G), subst_vars(G,B,BM), G\== true, !, ugoal_expansion(AH,AA),ugoal_expansion(BM,BB).
%ugoal_expansion(unity_db_call(DB,Args),BBB):- get_var_expansions(g,Args,ArgsO,VarsOut), !,
%  conjoin(VarsOut,unity_db_call(DB,ArgsO),BBB).

ugoal_expansion(unity_db_call(DB, Args),unity_db_call(DB, Args)):- !.
ugoal_expansion(unity_call(G),unity_call(G)):- !.


ugoal_expansion(begin(List),begin(List)):- var(List),!.
ugoal_expansion(begin(List),begin(List)):- is_list(List),!.
ugoal_expansion(begin(H),begin(HH)):- \+ source_file_expansion, !, ugoal_expansion(H,HH).
ugoal_expansion(G,O):- compound_name_arguments(G,B,Args),expand_f_args_to_list(B),Args\=[_],!,O=..[B,Args].
%ugoal_expansion(begin([H|B]),begin(Program)):- !, list_to_conjuncts([H|B],Conj),
%  ugoal_expansion(Conj,Program),!.
%ugoal_expansion(X,Y):- maybe_into_slash_db(X,YY),X\==YY,ugoal_expansion(YY,Y).

ugoal_expansion(C,O):- no_more_expansion(g,C,O),!.

ugoal_expansion(E,DBCALLO):- compound_name_arguments(E,DB,Args),db_pred(DB),!,
  get_var_expansions(h,Args,EArgs,G),
  conjoin(G,unity_db_call(DB,EArgs),DBCALL),
  ugoal_expansion(DBCALL,DBCALLO).

ugoal_expansion(CtxBody,CtxCall):-  get_var_expansions(b,CtxBody,CtxBodyO,G), G\==true,!,conjoin(G,CtxBodyO,CtxCallM),
  ugoal_expansion(CtxCallM,CtxCall).

ugoal_expansion(CmpI,CmpO):- 
  once((meta_pred_style(CmpI,Meta),
  strip_module(CmpI,_,Cmp),compound_name_arguments(Cmp,F,Args),
  compound_name_arguments(Meta,_,MArgs),
  maplist(ugoal_expansion_3,MArgs,Args,ArgsO),compound_name_arguments(CmpM,F,ArgsO))),
  maybe_readd_module(CmpI,CmpM,CmpMO),
  CmpMO\==CmpMO,!,ugoal_expansion(CmpMO,CmpO).

ugoal_expansion(G,unity_call(G)):-  source_file_expansion, is_unity(G).
ugoal_expansion(G,unity_call(G)):-  \+ source_file_expansion, is_unity(G).
ugoal_expansion(G,G).

is_unity(X):- contains_uvar(X),!.
%is_unity(X):- sub_term(E,X),compound(E),E='::'(_,_).
contains_uvar(X):- sub_uterm(b,E,X),compound(E),E='$'(N),atom(N),N\==global.

get_var_expansions(_,O,O,true):- \+ compound(O),!.
%get_var_expansions(h,'::'(C,T),'::'(CO,T),VarsOut):- get_var_expansions(h,C,CO,VarsOut).
get_var_expansions(Type,B::C,O,VarsOut):- compound(B), sub_uterm(Type,E,B),dv(E,V),subst(B,E,Var,BB),subst(C,E,Var,CC),!,get_var_expansions(Type,BB::CC,O,SVars),debug_var(V,Var),conjoin(getvar(V,Var),SVars,VarsOut).
get_var_expansions(Type,B,O,VarsOut):- compound(B), sub_uterm(Type,E,B),dv(E,V),subst(B,E,Var,BB),!,get_var_expansions(Type,BB,O,SVars),debug_var(V,Var),conjoin(getvar(V,Var),SVars,VarsOut).
get_var_expansions(_,O,O,true).

get_var_expansions(Type,B,O,VarsOut):- compound(B), sub_uterm(Type,E,B),dv(E,V),subst(B,E,Var,BB),!,get_var_expansions(Type,BB,O,SVars),debug_var(V,Var),conjoin(getvar(V,Var),SVars,VarsOut).

no_more_expansion_f(module).
no_more_expansion_f(wdmsg).
no_more_expansion_f(dmsg).
no_more_expansion_f(library).
no_more_expansion_f(nop).
no_more_expansion_fa(b,F,1):- current_op(X,Y,dynamic), \+ upcase_atom(F,F), current_op(X,Y,F). % ,current_predicate(F/1),!.
no_more_expansion_fa(b,Dmsg,A):-no_more_expansion_f(Dmsg), A < 3.

no_more_expansion(_,C,C):- \+ compound(C),!.
no_more_expansion(_,C,C):- dont_reduce(C),!.
no_more_expansion(g,C,O):- no_more_expansion(b,C,O),!.
no_more_expansion(g,C,C):- C=dmsg(_),!.
no_more_expansion(G,C,C):- compound_name_arity(C,F,A), no_more_expansion_fa(G,F,A).
%no_more_expansion(g,(A,B),(A,B)):- dont_reduce(A),!.
no_more_expansion(b,'::'(_,G),true):- G==true, !.
%no_more_expansion(_,(A,G),A):- G==true,!.
no_more_expansion(b,In,true):- In =@= (getvar(global, A),if_uctx_equals(A)).
no_more_expansion(b,In,!):- In =@= (getvar(global, A),if_uctx_equals(A),!).
%no_more_expansion(h,'::'(C,G),'::'(C,G)):-!.
no_more_expansion(b,'::'(C,G),OUT):- get_var_expansions(b,C,CO,Vars),!,expand_ugoal(G,GG), conjoin(Vars,'::'(CO,GG),OUT).
%no_more_expansion(b,unity_call(C),Out):- get_var_expansions(h,C,O,Vars),conjoin(Vars,unity_call(O),Out).
no_more_expansion(b,unity_db_call(C,Args),Out):- fail, get_var_expansions(h,Args,VArgs,Vars),Vars\==true,conjoin(Vars,unity_call(C,VArgs),Out).
%no_more_expansion(h,uslash(S,C),Out):- get_var_expansions(h,uslash(S,C),O,Vars),conjoin(Vars,O,Out).
no_more_expansion(b,uslash(S,C),Out):- get_var_expansions(h,C,O,Vars),conjoin(Vars,uslash(S,O),Out).
no_more_expansion(h,C,C):- C= uslash(_,_), !.
no_more_expansion(h,C,C):- C= unity_db_call(_,_), !.
no_more_expansion(h,C,C):- C= unity_call(_,_), !.

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
ho2mp(1,'*').
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
indexical(X=Y):- !, bind(X,Y),nb_setval(X,Y),log(indexical(X=Y)).
indexical(X):- atom(X),!,unknownvar_value(X,V), bind(X,V).
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
                    if (context.KnowledgeBase.MetaverseObject == null)
                        throw new Exception("Current KnowledgeBase has no associated metaverse object");
                    return context.MetaverseObject;
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

only_getvar(N,V):- var(N),!,dumpST_throw(var_only_getvar(N,V)).
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
dont_udecend_into(Begin):- dont_reduce(Begin).

sub_uterm(Type, Y, X):- \+compound(X),!,X=Y.
%sub_uterm(Type, X, '::'(C,_)):- source_file_expansion,!, sub_uterm(Type, X, C).
sub_uterm(Type, X, Arg):- is_db_call(Arg),!, sub_term(X, Arg).
%sub_uterm(Type, _, X):- source_file_expansion,dont_udecend_into(X),!,fail.
sub_uterm(Type, X, X).
sub_uterm(Type, X, Term) :-
    arg(_, Term, Arg),
    sub_uterm(Type, X, Arg).

is_db_call(Arg):- compound(Arg), compound_name_arity(Arg,DB,_),db_pred(DB).

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
getvar(N,V,A,A):- getvar(N,V).
%unknownvar_value(N,V):- atom(N),atom_or_var(V),atom_concat('',N,V),!,log(warn(assume_bind(N,V))),bind(N,V).
%unknownvar_value(N,V):- \+ \+ iz_a(_,N),!, freeze(V,iz_a(V,N)).
%unknownvar_value(N,V):- atom(N),atom_or_var(V),!,atom_concat('unknown_',N,V),!.
%unknownvar_value(N,V):- nonvar(V),!,N=V,log(warn(assume_bind(N,V))),bind(N,V).
%unknownvar_value(N,'$'(N)).
unknownvar_value(N,'#'(N)).

%'#'(IsDef):- current_predicate(IsDef/0),!,log(call(IsDef)),fail,unity_call(IsDef).
'#'(Undef):- log(('#'(Undef))),break,fail.

bind(X,Y):- must_be(atom,X),notrace(var(Y)->dumpST;true),duplicate_term(Y,YY), nb_setval(X,YY).

unbind(X):- is_list(X),!,maplist(unbind,X).
unbind(X):- nb_delete(X).

with_bind(X,Y,G):- nb_current(X,Was) 
  -> setup_call_cleanup(nb_setval(X,Y),locally(b_setval(X,Y),G),nb_setval(X,Was))
  ;  setup_call_cleanup(nb_setval(X,Y),locally(b_setval(X,Y),G),nb_delete(X)).

with_bind(X=Y,G):- !, with_bind(X,Y,G).
with_bind([H|T],G):- with_bind(H,with_bind(T,G)).

log(error(X)):- !, ansicall(bg(red),dmsg(error(X))).
log(warn(X)):- !, ansicall(red,dmsg(warn(X))).
log(mexp(X)):- !, log(v(x=X)),!. % overly verbose
log(ap(_)):- !. % overly verbose
log(apr(_)):- !. % overly verbose
log(v(_)):- !. % overly verbose
log(X):- with_output_to(string(Str),print_tree(X)), dmsg(Str).

starts_with_one_of(String,Word):- sub_string(Word,0,1,_,L),sub_string(String,_,_,_,L).

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
  lmconf:unity_module_name(Unity),
  load_files(Filename,[module(Unity),must_be_module(false),redefine_module(false),scope_settings(false)]).

load_unity_csv_file(F):- 
  log(load_unity_csv_file(F)),
  locally(t_l:pretend_expansion(file),
   (unity_prolog_filename(F,Prolog),
    load_unity_csv_file_data(Prolog))).

unity_prolog_filename(F,Filename):- exists_file(F),!,Filename=F.
unity_prolog_filename(F,Filename):- lmconf:lmchat_dir(D),atomic_list_concat([D,'/',F],Filename),exists_file(Filename),!.
unity_prolog_filename(N,Filename):- name(F,N), lmconf:lmchat_dir(D),absolute_file_name(F,Filename,[relative_to(D),extensions(['prolog','pl','P','']),access(read)]),!.
unity_prolog_filename(F,F).

now(Now):-  get_time(Now).


call_with_step_limit(Limit,Goal):- ALimit is Limit ^ 3, call_with_inference_limit(Goal, ALimit, Result), 
    ignore((inference_limit_exceeded == Result, throw(Result))),
    (((Result == (!))-> ! ; Result)).

:- assume_done(step_limit/1).

% :- assume_todo(component_of_metaverse_object_with_type/3).

% "True if component is a component of metaverseobject with type class."
% "?component", "?metaverseobject", "+class"
%:- assume_done(is_class/2). 
is_class(Obj, #('MetaverseObject')):-
  iz_a(Obj,metaverse_object).
is_class(Obj, #('MetaverseObject')):-
  iz_a(Obj,entity).

component_of_metaverse_object_with_type(X,X,Class):- Class== #('MetaverseObject'),!.
component_of_metaverse_object_with_type(X,X,Class):- Class== #('PropInfo'),!.
component_of_metaverse_object_with_type(X,X,Class):- Class== #('MetaverseObject'),!.
component_of_metaverse_object_with_type(X,X,Class):- is_class(X,Class),!.
component_of_metaverse_object_with_type(instanceOfClassFn(X,Class),X,Class).

%:- assume_todo(parent_of_metaverse_object/2).
%                "True if CHILD is a child of PARENT in the metaverse's rendering hierarchy.",
%                "?child", "?parent".
parent_of_metaverse_object(Child,Parent):- location(Child,Parent)*->Child=Parent.
  

% True if GOAL is true given variable bindings of each unique value of TEMPLATE produced by GENERATOR.
for_all_unique(T,Gen):- for_all_unique(T,Gen,true). 
%for_all_unique(T,Gen,Goal):- all(T,Gen,Set),member(T,Set),call(Goal).
for_all_unique(T,G,Goal):- forall(generate_unique(T,G),Goal).
%:- assume_todo(generate_unique/2).
% Succeeds once for each unique value of TEMPLATE produced by GENERATOR.
generate_unique(T,G):- no_repeats_u(V,G).
%:- assume_todo(sumall/3).
sumall(D,G,S):- aggregate_all(sum(D),G,S).
:- assume_todo(call_method/3).
%:- assume_todo(arg_min/3).
%arg_min(T,S,G):- findall(S-T,G,L),keysort(L,[_-T|_]).
arg_min(T,S,G):- aggregate(min(S,T),G,min(S,T)).
%:- assume_todo(arg_max/3).
%arg_max(T,S,G):- findall(S-T,G,L),keysort(L,KS),last(KS,_-T).
arg_max(T,S,G):- aggregate(max(S,T),G,max(S,T)).


:- assume_dyn_fail(type/2).
:- dynamic(relation_type/3).
:- assume_dyn_fail(property_type/3).
%:- assume_dyn_fail(property_name/3).
:- assume_dyn_fail(predicate_type/2).

%:- assume_dyn_fail(property/3).
%:- assume_done(is_class/2).

:- assume_done(pause_metaverse/0).
:- assume_done(unpause_metaverse/0).
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

unity_uctx:- prolog_load_context(file,File),prolog_load_context(source,File),!,tmpu:is_unity_file(File).
unity_uctx:- lmconf:unity_module_name(M),module_uctx(T),!,M==T.

print_clexp(_,X,Y):- X=@=Y,!.
print_clexp(_,X,Y):- ((#(global))::X)=@=Y,!.
print_clexp(ce,_,_):- source_file_expansion,!.
%print_clexp(asserta,_,_):-!.
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
    warn_but_keep_going(begin_csv_loading(Pred)),
    set_flag('$csv_row',1),!,
    maplist(load_csv_row_data(RTypes,Pred), Rows),
    listing(Pred),
    warn_but_keep_going(end_csv_loading(Pred)).

load_csv_row_data(Types,Pred, RowTerm):- warn_but_keep_going(load_csv_row_data_now(Types,Pred, RowTerm)),!.
load_csv_row_data_now(Types,Pred, RowTerm) :-     
    RowTerm=..[_|Row],
    Row = [F|_],
    flag('$csv_row',RowNumber,RowNumber+1),!,
    % allow comments
    (atom_concat('%',_,F) -> in_cmt(log(ap(load_csv_row(RowNumber,RowCall)))) ;
     (must_maplist(correct_row,Types,Row,CRow),
      RowCall=..[Pred|CRow],
      log(ap(load_csv_row(RowNumber,RowCall))),
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
 once(atom_concat(_,')',A);atom_contains(A,':');atom_contains(A,'$');atom_contains(A,'=');atom_contains(A,')')),
 read_term_from_atom(A,T,[variable_names(Vs)]),maplist(vs_name,Vs),
 correct_row_type(Type,T,V),
 notrace(ignore((V\==[],A\==V,fail,dmsg(Type+A --> V)))),!.

correct_row(Type,A,V):- atom(A), atom_contains(A,','), 
 read_term_from_atom(A,TT,[variable_names(Vs)]),maplist(vs_name,Vs),
 conjuncts_to_list(TT,TL),
 ((Type\==[phrase,list])->T=TL;maplist(listify,TL,T)),
 correct_row_type(Type,T,V),
 notrace(ignore((V\==[],A\==V,dmsg(Type+A --> V)))),!.


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


ltest:- 
  nb_setval(c,writeln(hi_c)),
  P =  (b:-if_uctx_equals(a), '::'(a, $c)),
  assert_if_new(P),
  '::'(a,b).


:- arithmetic_function('<'/2).
'<'(X,Y,Z):- 
  (X < Y) -> Z = 1 ; Z = 0.
:- arithmetic_function('>'/2).
'>'(X,Y,Z):- 
 (X > Y) -> Z = 1 ; Z = 0.

:- arithmetic_function('if'/3).
if([Cond],True,False,RetVal):- (Cond -> RetVal is True ; RetVal is False),!.
if(Cond,True,False,RetVal):- number(Cond),!,( Cond>0 -> RetVal is True ; RetVal is False),!.
:- arithmetic_function('vector'/3).
vector(X,Y,Z,RetVal):-
 RetVal is 
      (if(X < 0.0, 1, 0) * 0x40000000 + (integer(abs(X) /\ 0xFF) << 22)) \/
      (if(Y < 0.0, 1, 0) * 0x200000 + (integer(abs(Y) /\ 0xFF) << 13)) \/
      (if(Z < 0.0, 1, 0) * 0x1000 + (integer(abs(Z)) /\ 0xFFF)), !.

:- arithmetic_function('v_x'/1).
v_x(V3,X):- X is if(V3 /\ 0x40000000, -1, 1) * ((V3 >> 22) /\ 0xFF).
:- arithmetic_function('v_y'/1).
v_y(V3,X):- X is if(((V3 /\ 0x200000) > 0), -1, 1) * ((V3 >> 13) /\ 0xFF).
:- arithmetic_function('v_z'/1).
v_z(V3,X):- X is if(((V3 /\ 0x1000) > 0), -1, 1) * (V3 /\ 0xFFF).

:- arithmetic_function('position'/1).

position(V3,X):- number(V3) -> X = V3 ; (v3_position(V3,X1,Y1,Z1), X is vector(X1,Y1,Z1)).

:- arithmetic_function('tlen'/1).
tlen([V3],X):-!, tlen0(V3,X).
tlen(V3,X):-!, tlen0(V3,X).
tlen0(V3,X):- number(V3),!,X is V3.
tlen0(V3,X):- atomic(V3),!,atom_length(V3,X).
tlen0(V3,X):- is_list(V3),!,length(V3,X).
tlen0(V3,X):- term_hash(V3,H), X is H rem 100.

v3_position(V3,X,Y,Z):- number(V3),!, X is v_x(V3), Y is v_y(V3), Z is v_z(V3).
v3_position([V3],X,Y,Z):- nonvar(V3),!, v3_position(V3,X,Y,Z).
v3_position(V3,X,Y,Z):- t(x,V3,X),t(y,V3,Y),t(z,V3,Z),!.
v3_position(V3,X,Y,Z):- T is tlen(V3), X is T,Y is T*100, Z is T*10000.

:- arithmetic_function('magnitude_squared_v3'/3).
magnitude_squared_v3(X1,Y1,Z1,RetVal):-
  RetVal is (X1)^2 +(Y1)^2 +(Z1)^2. 

:- arithmetic_function('magnitude_squared'/1).
magnitude_squared(V3,RetVal):-
  v3_position(V3,X1,Y1,Z1),
  RetVal is magnitude_squared_v3(X1,Y1,Z1). 
:- arithmetic_function('magnitude'/1).
magnitude(V3,RetVal):-
  RetVal is sqrt(magnitude_squared(V3)).

:- arithmetic_function('distance_squared'/2).
distance_squared(V3,P2,RetVal):- 
  v3_position(V3,X1,Y1,Z1),
  v3_position(P2,X2,Y2,Z2),
  RetVal is magnitude_squared_v3(X1-X2,Y1-Y2,Z1-Z2).

:- arithmetic_function('distance'/2).
distance(V3,P2,RetVal):-
  RetVal is sqrt(distance_squared(V3,P2)).

:- arithmetic_function('property'/2).
property(Obj,Prop,Num):-
  t(Prop,Obj,Value),
  Num is Value.

/*
               case "magnitude":
                {
                    if (iz.Arguments.Length != 1)
                        throw new ArgumentCountException("magnitude", iz.Arguments, "Vector3");
                    object v = Eval(iz.Argument(0), context);
                    if (!(v is Vector3))
                        throw new ArgumentTypeException("magnitude", "vector", v, typeof (Vector3));
                    return ((Vector3) v).magnitude;
                }

                case "magnitude_squared":
                {
                    if (iz.Arguments.Length != 1)
                        throw new ArgumentCountException("magnitude_squared", iz.Arguments, "Vector3");
                    object v = Eval(iz.Argument(0), context);
                    if (!(v is Vector3))
                        throw new ArgumentTypeException("magnitude_squared", "vector", v, typeof (Vector3));
                    return ((Vector3) v).sqrMagnitude;
                }

                case "distance":
                {
                    if (iz.Arguments.Length != 2)
                        throw new ArgumentCountException("distance", iz.Arguments, "v1", "v2");
                    object v1 = Eval(iz.Argument(0), context);
                    if (v1 is MetaverseObject)
                        v1 = ((MetaverseObject)v1).transform.position;
                    object v2 = Eval(iz.Argument(1), context);
                    if (v2 is MetaverseObject)
                        v2 = ((MetaverseObject)v2).transform.position;
                    if (!(v1 is Vector3))
                        throw new ArgumentTypeException("distance", "v1", v1, typeof (Vector3));
                    if (!(v2 is Vector3))
                        throw new ArgumentTypeException("distance", "v2", v2, typeof (Vector3));
                    return Vector3.Distance((Vector3) v1, (Vector3) v2);
                }

                case "distance_squared":
                {
                    if (iz.Arguments.Length != 2)
                        throw new ArgumentCountException("distance_squared", iz.Arguments, "v1", "v2");
                    object v1 = Eval(iz.Argument(0), context);
                    if (v1 is MetaverseObject)
                        v1 = ((MetaverseObject)v1).transform.position;
                    object v2 = Eval(iz.Argument(1), context);
                    if (v2 is MetaverseObject)
                        v2 = ((MetaverseObject)v2).transform.position;
                    if (!(v1 is Vector3))
                        throw new ArgumentTypeException("distance_squared", "v1", v1, typeof (Vector3));
                    if (!(v2 is Vector3))
                        throw new ArgumentTypeException("distance_squared", "v2", v2, typeof (Vector3));
                    return Vector3.SqrMagnitude((Vector3) v1 - (Vector3) v2);
                }

                case "position":
                {
                    if (iz.Arguments.Length != 1)
                        throw new ArgumentCountException("position", iz.Arguments, "metaverseObject");
                    var metaverseObject = Eval(iz.Argument(0), context);
                    var go = metaverseObject as MetaverseObject;
                    if (go==null)
                        throw new ArgumentTypeException("position", "metaverseObject", metaverseObject, typeof(MetaverseObject));
                    return go.transform.position;
                }

                case ".":
                    if (iz.Arguments.Length != 2)
                    {
                        throw new ArgumentCountException(".", iz.Arguments, "object");
                    }
                    return EvalMemberExpression(iz.Arguments[0], iz.Arguments[1], context);

                case "property":
                {
                    if (iz.Arguments.Length != 2)
                        throw new ArgumentCountException("property", iz.Arguments, "object", "property_name");
                    object o = iz.Argument(0);
                    if (o is Structure)
                        o = Eval(o, context);
                    var name = iz.Argument(1) as Symbol;
                    if (name == null)
                        throw new ArgumentTypeException("property", "property_name", iz.Argument(1), typeof(Symbol));
                    return o.GetPropertyOrField(name.Name);
                }

                case "vector":
                {
                    if (iz.Arguments.Length != 3)
                        throw new ArgumentCountException("vector", iz.Arguments, "x", "y", "z");
                    return new Vector3(Convert.ToSingle(Eval(iz.Argument(0), context)),
                        Convert.ToSingle(Eval(iz.Argument(1), context)),
                        Convert.ToSingle(Eval(iz.Argument(2), context)));
                }

                case "instance_id":
                {
                    if (iz.Arguments.Length != 1)
                        throw new ArgumentCountException("instance_id", iz.Arguments, "metaverse_object");
                    var arg = iz.Argument(0) as UnityEngine.Object;
                    if (arg == null)
                        throw new ArgumentTypeException("instance_id", "object", iz.Argument(0), typeof(UnityEngine.Object));
                    return arg.GetInstanceID();
                }

                default:
                    throw new BadProcedureException(iz.Functor, iz.Arguments.Length);
            }
        }
*/


:- fixup_exports.
:- use_module(library(yall)).
:- unload_file(library(yall)).
'>>'(X,Y):- uslash_completion(X,Y).
(X/Y) :- uslash_call(X,Y).
'/'(X):- uslash_call_top(X).

:- multifile(term_expansion/4).
:- dynamic(term_expansion/4).
system:term_expansion(X,O,Y,O):- notrace(compound(X)), unity_uctx, uterm_expansion(X,Y),ansicall(yellow,print_clexp(ce,X,Y)).
:- multifile(system:(goal_expansion/2)).
:- dynamic(system:(goal_expansion/2)).
system:goal_expansion(X,Y):- notrace((compound(X), unity_uctx, \+ source_file_expansion, expand_ugoal(X,Y),X\==Y,ansicall(blue,print_clexp(ge,X,Y)))).
%:- load_unity_prolog_file('Utilities/startup.prolog').



end_of_file.

%:- dynamic($/2).
Warning: baseKB:calc_to_hashmap/3, which is referenced by
Warning:        /opt/logicmoo_workspace/packs_xtra/logicmoo_chat/npc_chat/Assets/unity_prolog.pl:141:21: 1-st clause of baseKB:calc_to_hashmap/1
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

