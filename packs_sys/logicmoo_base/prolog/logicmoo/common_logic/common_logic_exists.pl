:- module(kbe,[ % existentialize/2,
         % kbi_define/1
]).
/** <module> common_logic_kbi
% Provides a prolog database replacement that uses an interpretation of KIF
%
%  t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% NEW
:- include(library('logicmoo/common_logic/common_header.pi')).
:- '$set_source_module'(baseKB).

%:- endif.
%:- use_module(library(dictoo)).

% :- '$set_source_module'(baseKB).
:- meta_predicate skolem_test(0).
:- meta_predicate skolem_unify(*,0).


:- module_transparent with_no_kif_var_coroutines/1.

with_no_kif_var_coroutines(Goal):- locally_each(local_override(no_kif_var_coroutines,true),Goal).

% :- use_module(library(tabling)).

%:- use_module(library(logicmoo_common)).
%:- use_module(library(loop_check)).
%:- use_module(library(logicmoo_typesystem)).


 :- meta_predicate query_ex(?).
 :- meta_predicate body_call(?).
 :- meta_predicate bless_ex(*,*).
 :- meta_predicate reify(?).
 :- meta_predicate test_count(0,*).
% :- meta_predicate undo(0).

:-  system:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'))).


:- set_how_virtualize_file(false).

/*

  existentialize( +FmlIn, -FmlOut, [options...]).


      converts terms like...

         loves(joe,mary)

      Into...

         exists([R,X,Y,T], 
            ((subRelation(R,loves), is_a(T,time), is_a(T,context),
              exists_durring(X,T),exists_durring(Y,T),
              ~different(joe,X),~different(mary,Y)) 
                  => trueIn(T,holds(R,X,Y)))).


      options are...


*/

existentialize(P,NewP):- 
   kif_optionally_e(true,existentialize_objs,P,MidP),
   kif_optionally_e(false,existentialize_rels,MidP,NewP),!.

existentialize_objs(P,NewP):- 
 get_named_objs(P,Names),
 quantify_names(name_to_string,isNamed,Names,P,NewP),!.
  
existentialize_rels(P,NewP):-
 current_outer_modal_t(HOLDS_T),
 to_tlog(HOLDS_T,_KB,P,TLog),
 get_named_rels(TLog,Names),
 quantify_names(=,subrelation,Names,TLog,NewP),!.
  


already_existentialized(Term,Str):-  sub_term(NameOf,Term), compound(NameOf), NameOf=isNamed(_,StrW), name(Str,StrW).
already_existentialized(Term,Str):-  sub_term(NameOf,Term), compound(NameOf), NameOf=subrelation(_,Str).
already_existentialized(Term,Str):-  sub_term(NameOf,Term), compound(NameOf), NameOf=exists(X,_),Str==X.
already_existentialized(Term,Str):-  sub_term(NameOf,Term), compound(NameOf), NameOf=quant(_,X,_),Str==X.
already_existentialized(Term,Str):-  sub_term(NameOf,Term), compound(NameOf), NameOf=all(X,_),Str==X.

should_existentialize(Term,~ Sub,Str):-!,compound(Sub), should_existentialize(Term,Sub,Str).
should_existentialize(Term,Sub,Text) :- compound(Sub),functor(Sub,F,A),
  \+ dont_existentialize_args(Term,F,A),
  arg(N,Sub,Text),
  (string(Text);(existentialize_args(Sub,Term,F,A,N),atom(Text))).

string_better(Str,StrO):- string_lower(Str,StrL),StrL\==Str,!, StrO=Str.
string_better(Str,StrO):- toCamelcase(Str,StrL),text_to_string(StrL,StrO),!.

existentialize_args(_Sub,_Term,_F,1,1).
existentialize_args(Sub,Term,F,A,N):- 
  \+ dont_existentialize_args(Term,F,A),
  arg(_,Sub,Text),
  \+ string(Text),
  \+ compound(Text),
  (is_2nd_order_holds(F) -> N \==1 ; true),
  do_existentialize_f(F).

do_existentialize_f(loves).
do_existentialize_f(man).
do_existentialize_f(_).

dont_existentialize_args(_Term,F,_A):- dont_existentialize_f(F).

dont_existentialize_f(skolem).
dont_existentialize_f(skF).
dont_existentialize_f(call).
dont_existentialize_f(isNamed).
dont_existentialize_f(equals).
dont_existentialize_f(different).
dont_existentialize_f(subrelation). 

get_named_objs(Term,Names):- 
   findall(Str,
    (sub_term(Sub,Term),
      should_existentialize(Term,Sub,Str),
      \+ already_existentialized(Term,Str)),
     NamesL),
   list_to_set(NamesL,Names).

should_existentialize_rel(_Term,Sub,Str):-compound(Sub),Sub=..[F,Str|_],is_2nd_order_holds(F).

get_named_rels(Term,Names):- 
   findall(Str,
    (sub_term(Sub,Term),
      should_existentialize_rel(_Term,Sub,Str),
      \+ dont_existentialize_f(Str),
      \+ already_existentialized(Term,Str)),
     NamesL),
   list_to_set(NamesL,Names).

:- meta_predicate quantify_names(2,*,*,*,*).
quantify_names(_Conv,_Oper,[],P,P).
quantify_names(Conv,Oper,[Name|TODO],P,
 exists(X,(OperC & NewP))):- 
   OperC =.. [Oper,X,Str],
   call(Conv,Name,Str)->
   subst(P,Name,X,NextPM),
   add_var_to_env(Str,X),
   quantify_names(Conv,Oper,TODO,NextPM,NewP).

name_to_string(Name,Name):-!.
name_to_string(Name,Str):- text_to_string(Name,Text),string_better(Text,Str).


memberchk_eq(X, [Y|Ys]) :- (   X == Y ->  ! ;   memberchk_eq(X, Ys)).
subtract_eq([],_,[],[]).
subtract_eq([X|Xs],Ys,L,[X|Intersect]) :-   memberchk_eq(X,Ys),subtract_eq(Xs,Ys,L,Intersect).
subtract_eq([X|Xs],Ys,[X|T],Intersect) :-   subtract_eq(Xs,Ys,T,Intersect).

% :- abolish(user:portray/1).
:- dynamic(user:portray/1).
:- multifile(user:portray/1).





:- kb_shared(baseKB:proven_tru/1).
:- kb_shared(baseKB:proven_helper/1).
% :- kb_shared(baseKB:proven_tru/1).

modal_functor((poss)).
modal_functor((nesc)).
modal_functor((falsify)).
modal_functor((~)).


non_modal_positive(P):- atomic(P),!.
non_modal_positive(P):- compound(P),functor(P,F,_), \+ modal_functor(F).
:- export(non_modal_positive/1).
:- system:import(non_modal_positive/1).
:- system:export(non_modal_positive/1).

groundoid(P):- term_variables(P,AV), maplist(is_existential,AV).
% groundoid(P):- term_attvars(P,AV), maplist(is_existential,AV).
% groundoid(P):- term_variables(P,AV), (AV==[] -> true; (term_attvars(P,AV),maplist(is_existential,AV))).

:- module_transparent(skolem_neg/1).
skolem_neg(P):- bagof(B,clause(deduce_neg(P),B),List),List\==[],must(combined_skolem(P,List,Call)),Call.

:- module_transparent(falsify/1).
falsify(MP):- strip_module(MP,M,P),(falsify(M,P)).
:- module_transparent(falsify/2).
falsify(M,P):-  
  loop_check(M:proven_neg(P)),
  nop(nrlc0(\+ M:proven_tru(P))).
%falsify(M,P):- nrlc0(M:skolem_neg(P)).

falsify(_,P):- var(P),!,fail.
falsify(M,poss( P)):- !,nonvar(P),falsify_poss(M,P),groundoid(P).
falsify(M,different(Puppy1, Puppy2)):- M:clause(proven_neg(different(Puppy1, Puppy2)),Body)*-> M:once(Body);(!,fail).
%falsify(M,~P):- !, non_modal_positive(P),(nesc_lc(M,poss(P));nesc_lc(M,P)).
%falsify(M, P):- non_modal_positive(P),!,falsify_lc(M,poss(P)).

falsify_poss(M,(~P)):- !, non_modal_positive(P),nesc(M,P).
falsify_poss(M,( P)):- non_modal_positive(P),!,falsify_lc(M,P).

falsify_lc(M,P):-loop_check(falsify(M, P)),groundoid(P).

:- export(falsify/1).
:- public(falsify/1).
:- system:import(falsify/1).
:- system:export(falsify/1).

%:- baseKB:ain((proven_tru(P):- (proven_tru_0(P),groundoid(P)))).
:- baseKB:ain(((poss(P)) :- (non_modal_positive(P),(falsify(poss(~P));nesc(P)),groundoid(P)))).
:- baseKB:ain(((poss(P)) :- (nesc(P)))).
:- baseKB:ain(((poss(~P)) :- (non_modal_positive(P),(falsify(poss(P));falsify(P)),groundoid(P)))).

functor_skell(P0,P0):- var(P0),!.
functor_skell(P0,_):- \+ compound(P0),!.
functor_skell(P0,P0F):- compound_name_arguments(P0,F,ARGS),maplist(functor_skell,ARGS,ARGSO),!,compound_name_arguments(P0F,F,ARGSO).


functor_skell(P0,P0F):- functor(P0,F,A),functor(P0F,F,A).

:- module_transparent(skolem_tru/1).

skolem_tru(MP):- 
   strip_module(MP,M,P), 
   term_variables(P,Vs),!,
   copy_term_nat(P:Vs,P0:VCs),
   functor_skell(P0,P0F),
   setof(apb(P0F,B),system:clause(M:deduce_tru(P0F),B),List),
   List\==[],
   must(combined_skolem(P0,List,Call)),!,
   dmsg(sk_set_maker(M,P0,Call)),
   M:Call,
   P0=P0F,
   Vs=VCs,
   term_variables(P,_RVs),

   nop(ignore(show_failure(\+ (deduce_neg(P))))).
/*
skolem_tru(MP):- 
   strip_module(MP,M,P), 
   term_variables(P,Vs),
   copy_term_nat(P:Vs,P0:VCs),
   sk_set_maker(M,P0),
   Vs=VCs,
   term_variables(P,+RVs),

   nop(ignore(show_failure(\+ (deduce_neg(P))))).

sk_set_maker(M,P0):-
   functor_skell(P0,P0F),
   setof(apb(P0F,B),system:clause(M:deduce_tru(P0F),B),List),
   List\==[],
   must(combined_skolem(P0,List,Call)),!,
   dmsg(sk_set_maker(M,P0,Call)),
   M:Call,
   P0=P0F.
*/

% skolem_tru(P):- strip_module(P,M,P0),M:deduce_tru(P0).

:- dynamic(baseKB:(nesc)/1).
:- export(baseKB:(nesc)/1).
:- public(baseKB:(nesc)/1).
:- system:import(baseKB:(nesc)/1).                                                                                
:- system:export(baseKB:(nesc)/1).
%:- module_transparent(system:(nesc)/1).
%:- module_transparent(system:(nesc)/1).
%baseKB:nesc(MP):- strip_module(MP,M,P),no_repeats((nesc_lc(M,P))).
:- module_transparent(nesc_lc/2).

% :- table(nesc_lc/2).
nesc_lc(M,P):- same_compound(P,proven_tru(PP)),!,nesc(M,PP),show_failure(groundoid(PP)).
nesc_lc(M,P):- !, nesc(M,P),ignore(show_failure(groundoid(P))).

first_of([X|Rest]):- call(X) ; first_of(Rest).

:- module_transparent((nesc)/2).
% :- dra_table((nesc)/2).
nesc(_M,isNamed(X,Y)):-!,isNamed_impl(X,Y),!.
nesc(M,P):- swc, 
   
   first_of(
     [(loop_check(M:proven_tru(P)),nop(nrlc0(call(call, \+ M:proven_neg(P))))),
      (M:proven_helper(P), \+ M:proven_helper(~P)),
      
      M:skolem_tru(P)]).
%   \+  (loop_check((system:clause(M:nesc(P),B),((B\=nesc_lc(M, _),(M:B)))))).

   % (loop_check(M:isNamed(X,Y)))]).


%nesc(M,P):- swc,!,nonvar(P), M:clause((P),B), B \= (_,_), M:B.
%nesc(_,P):- swc, \+ (var(P);non_modal_positive(P);P=poss(_)),!,fail.
%nesc(M,P):- swc, nonvar(P),M:clause(P,B), B \= call_tru(_,_), M:B.
:- export((nesc)/2).
:- public((nesc)/2).
:- system:import((nesc)/2).                                                                                
:- system:export((nesc)/2).


:-multifile(baseKB:proven_helper/1).
%:- rtrace.
baseKB:proven_helper(~equals(X,Y)):- dif_objs(X,Y),!.
%:- (nortrace,break).
%:- xlisting(dif_objs/2).

%:- break.

make_identity(I):- make_wrap(identityPred,I,2).


make_type(P):-make_wrap(call_tru,P,1).

make_wrap(T,MFA,D):- 
   get_mfa(MFA,M,F,AN),
   (AN==0->A=D;A=AN),
   functor(P,F,A), 
   asserta_if_new( ((P):- call(T,P))),
   asserta_if_new(baseKB:safe_wrap(M,F,A,T)),
   M:export(F/A),
   import(F/A).



assign_ex(Ex,V):- isNamed(Ex,V).

reify((P,Q)):-!,reify(P),reify(Q).
reify(P):- query_ex(P).

% :- assert_if_new((genlPreds(X,Y):- fail,trace_or_throw(genlPreds(X,Y)))).


% ex(P):- compound(P),P=..[_,I], (var(I)-> freeze(I,from_ex(P)) ; fail).



solve_ex(Var,_Vs,_Head,P,BodyList):- 
  existential_var(Var,P), 
  maplist(bless_with,BodyList), maplist(body_call,BodyList).

bless_with(P):- ground(P),!.
bless_with(P):- bless(P).

% body_call(P):- recorded(kbi,P).
body_call(P):- ground(P),!,loop_check(P).
body_call(P):- bless(P).


is_recorded(A):- context_module(M), recorded(M,A)*->nop(sanity(\+cyclic_term(A)));body_call(A).

% WORDED call_tru(P):- (clause(can_bless(P),Body)*->Body; ((fail,bless(P)))),is_recorded(P).


bless(P):-ground(P),!.
bless(P):- 
 (get_ev(P,Attvars,Univ)),
   (Univ == [] -> true ;
       maplist(add_constraint_ex(bless_ex,P),Univ)),
   (Attvars == [] -> true ;
        maplist(add_constraint_ex(bless_ex,P),Attvars)),
 nop(Attvars == [] -> true ;
      maplist(add_constraint_ex(bless_ex2,P),Attvars)).

get_ev(P,Annotated,Plain):- 
    term_variables(P,Vars),
    term_attvars(P,Annotated),
    subtract_eq(Vars,Annotated,Plain).

labling_ex(P):- copy_term(P,PP,Residuals),maplist(call,Residuals),P=PP.


bless_ex2(_X,P):- \+ ground(P).
bless_ex(X, P):- nonvar(X)->call(P); true.


:- meta_predicate query_tru(?).
query_tru(Qry) :- nrlc0((nesc(Qry))).

query_ex(PQ):-   update_changed_files1,
  existentialize(PQ,P),
   dmsg(query_ex(P)),
   sanity(((existentialize(P,PEx), ignore((PEx\==P,dmsg(query_ex(PEx))))))),
   gensym(skTrue,QryF),
   gensym(skFalse,NotQryF),
   term_variables(P,Vars),
   Qry=..[QryF|Vars],
   NotQry=..[NotQryF|Vars],
   assert_kif((P => Qry)),
   subst(P,exists,all,PA),
   assert_kif(((~(PA)) => NotQry)),!,
   (query_tru(Qry) *-> dmsg(Qry); 
     (query_tru(NotQry)*-> dmsg(NotQry); dmsg(unknown(Qry)))).
        

% query_ex(P):-  ignore(show_failure(P)).
% query_ex(P):- is_recorded(P),recorded(complete,P).

minus_vars(Head,Minus,Dif):-
   term_variables(Head,HeadVars),
   term_variables(Minus,BodyVars),
   subtract_eq(HeadVars,BodyVars,Dif).



%create_ex(X,Lit1,BodyLits,Prop,DisjExs):- \+ contains_var(X,Lit1),assert_if_new((gen_out(Lit1):- ensure_sneezed(X,Lit1,BodyLits,Prop,[]))),fail.
create_ex(Lit1,Prop,UniqeHead,Intersect,UniqeBody,BodyLits):-
   recorda_if_new(Lit1,head_body(Lit1,BodyLits,UniqeHead,Intersect,UniqeBody,Prop)).


recorda_if_new(K,Lit1):- functor(Lit1,F,A),functor(Lit0,F,A),recorded(K,Lit0),Lit0=@=Lit1,!,dmsg(skip_recorda(Lit0=@=Lit1)).
recorda_if_new(K,Lit1):- show_call(recorda(K,Lit1)).

recorda_if_new(Lit1):- context_module(M), recorda_if_new(M,Lit1). 

assert_ex2(P):- test_boxlog(P),!.

assert_ex2(P):- 
  kif_to_boxlog(P,BLU),
  sort(BLU,BL),
  must_maplist_det(assert_ex3,BL).

assert_ex3('$unused'(_):-_).

assert_ex3((H:-B)):- sort_body_better(H,B,BO),assert_ex4((H:-BO)).
assert_ex3(P):- assert_ex4(P).

assert_ex4(P):- assert_ex5(P).

assert_ex5(P):- assert_ex9(P).

assert_ex8(P):- nop(ain(P)),assert_ex9(P).
assert_ex9(P):- guess_varnames(P),portray_clause_w_vars(P).





at_least_one_of(X,List):- Possibles=[_|_],at_least_one_of1(List,Possibles),add_cond(X,Possibles).
at_least_one_of1([P],Possibles):-!,must_be(nonvar,P), (Possibles=[P] ; Possibles=[]).
at_least_one_of1([P|List],Possibles):-!, ((Possibles=[P|More] ; Possibles=More),at_least_one_of1(List,More)).



sort_body_list(List,SList):-predsort(cmp_cardinality,List,SList).

cmp_cardinality(Cmp,Body1,Body2):- body_cardinality(Body1,Score1)->body_cardinality(Body2,Score2)->compare(Cmp0,Score1,Score2)->Cmp0 \== (=),!,(Cmp0== (>) -> Cmp= (<) ; Cmp= (>)).
cmp_cardinality(Cmp,Body1,Body2):- compare(Cmp,Body1,Body2).

body_cardinality(apb(_,Body),Size):- sub_term(SKF,Body),same_compound(SKF,skolem(_,S)),sub_term(Size,S),integer(Size),!.
body_cardinality(Body,Size):- sub_term(SKF,Body),same_compound(SKF,skolem(_,S)),sub_term(Size,S),integer(Size),!.
body_cardinality(_,0).


new_sk_dict( _:{vs:_,sks:_,more:_}).
get_sk_props(X,Dict):- attvar(X),get_attr(X,skp,Dict).
ensure_sk_props(X,Dict):- sanity(var(X)),(get_sk_props(X,Dict)->true;((new_sk_dict(Dict),put_attr(X,skp,Dict)))).

skolem_var_body(B,V):- sub_term(S,B),same_compound(S,skolem(V,_)).

combined_skolem(P,[List],apply_cond(P, List)):-!.
combined_skolem(P,List,sk_some(P,Cardin,[B|SList])):- sort_body_list(List,[B|SList]),
   body_cardinality(B,Cardin),Cardin>0,!.
combined_skolem(_P,List,B*->freeze(V,maplist(call,SList))):- sort_body_list(List,[B|SList]),skolem_var_body([B|SList],V).
combined_skolem(_P,List,maplist(call,SList)):- sort_body_list(List,SList).



sk_some(_).



%:- set_prolog_flag(gvar_skolems,true).
:- set_prolog_flag(gvar_skolems,true).
:- set_prolog_flag(gvar_skolem_combine,true).

/*

true
false
dont_care
true_or_false

DATA= [sk(3,foo=true),sk(3,bar=true),sk(3,bar=false),sk(5,zee=true),
  sk(1,name=a),sk(1,name=b),sk(1,name=c)]
3*foo=1
3*bar=2
5*zee=1
1*name=3


DATA= [sk(3,foo=true&bar=true),sk(3,bar=true),sk(3,bar=false),sk(5,zee=true),
  sk(1,name=a),sk(1,name=b),sk(1,name=c)]
3*foo=1 w/ bar=1
3*bar=2
5*zee=1
1*name=3


get_min_keys()
get_min_count(DATA,6).
get_min_perms(DATA,...).

*/
set_exists_enum(SkV,Value):- quietly(nb_set_value(?('$exist$'),SkV,Value)).
get_exists_enum(SkV,Value):- quietly(nb_current_value(?('$exist$'),SkV,Value)).
set_some_enum(SkV,Value):- quietly(nb_set_value(?('$some$'),SkV,Value)).
get_some_enum(SkV,Value):- quietly(nb_current_value(?('$some$'),SkV,Value)).


sk_some_from_current(Set,SkV,Conds):-
  length(Set,Cardin),
  which_skv(SkV,Cardin,Which),
  nth1(Which,Set,Conds).

sk_some(P,Cardin,Conds):- nop((Count = count(Cardin))),!,
   sk_some1(P,Conds),
   nop((arg(1,Count,N),N1 is N-1,N>1,nb_setarg(1,Count,N1))).

sk_some1(P,Conds):-
   (get_some_enum(P,Set)*-> true ;
    ((Possibles = [_|_], 
      bagof(Possibles,at_least_one_of1(Conds,Possibles),Combos),set_some_enum(P,Combos),get_some_enum(P,Set)))),
   sk_some_from_current(Set,P,NewConds),
   maplist(apply_cond(P),NewConds).

% apply_cond(P,Y):-dmsg(apply_cond(P,Y)),fail.
apply_cond(P,Y):- var(Y),throw(apply_cond(P,Y)).
apply_cond(P,(G1,G2)):- !, apply_cond(P,G1),apply_cond(P,G2).
apply_cond(P,(G1;G2)):- !, apply_cond(P,G1);apply_cond(P,G2).
apply_cond(P,apb(P,Goal)):-!,apply_cond(P,Goal).
%TODO check to esures something is shared? 
apply_cond(_P,Goal):- call(Goal).

apb(X,Y):-dmsg( apb(X,Y)),!,fail.

specialize_1(X):- 
  setof(at_least_one_of(SOME,Conds),(has_cond(X,at_least_one_of(SOME,Conds))),CondSets),
  maplist(specialize_2(X),CondSets).

specialize_2(X,at_least_one_of(_SOME,Conds)):-
  ((at_least_one_of1(Conds,Possibles),add_cond(X,Possibles))).


use_skf(SKF,Cardin,SkV,DFml):- var(SKF),!,strip_module(SkV,M,_),call(call,clause(M:make_existential(X,SKF),_)),SKF=skF(Cardin,SkV,X,DFml).
use_skf(SKF,Cardin,SkV,DFml):- SKF=skF(Cardin,SkV,_X,DFml).

not_in(X,B):- current_skolems(B,Set),must(Set\=[]),\+ (member(Z,Set),Z==X).

%current_skolems(In,Set):- current_skolems(In,_SKF,Set)*->true;current_skolems(_SkV,In,Set).
current_skolems(In,Set):- current_skolems(_,In,Set)*->true;current_skolems(In,_,Set).
current_skolems(SkV,SKF,Set):- 
   use_skf(SKF,Cardin,SkV,DFml),
   (get_exists_enum(SkV,Set)
          *->true;
          (make_skolem_list(SkV,SKF,Cardin,SkV,DFml),
           get_exists_enum(SkV,Set))).


:- kb_shared(baseKB:make_existential/2).
skolem(X, UNKKEY):- var(UNKKEY),!,strip_module(X,M,_),
  call(call,clause(M:make_existential(_,UNKKEY),_)),nonvar(UNKKEY),
  M:skolem(X, UNKKEY).
skolem(E,SK):- nonvar(E),!,nop(dmsg(warn(nonvar_skolem(E,SK)))),  
  isNamed(E,Named),!,
  (skolem(X,SK),isNamed(X,Named)),
  dmsg(E=X).

skolem(X, SKF):- sanity(var(X)),
  use_skf(SKF,_Cardin,SkV,_DFml),
  ((has_cond(X,aoc(SkV,N)),nonvar(N)) -> ! % already has this constraint 
  ; ((current_skolems(SKF,Set),
   must(skolem_from_set(Set,Y,SKF)),
    attempt_checkout(Y,SkV),
     X=Y))).

attempt_checkout(Y,SkV):- \+ has_cond(Y,co(SkV)),add_cond(Y,co(SkV)).

make_skolem_list(SkV,SKF,Cardin,SkV,DFml):-
  must((use_skf(SKF, Cardin,SkV,DFml),
  must((findall(X,((between(1,Cardin,Which),
  must((once((make_existential(X,SKF))))),
     must((once((add_cond(Y,aoc(SkV,Which)))))),
     must(X=Y)
     
     )),
   List),List\==[])),
  % all_different_existentials(List),
  must(show_call(set_exists_enum(SkV,List))))).

all_different_existentials([_]):-!.
all_different_existentials([In|ListIn]):-
  maplist(difv(In),ListIn),!,
  all_different_existentials(ListIn).



:- meta_predicate(no_repeats_ex(0)).
:- meta_predicate(no_repeats_ex(+,0)).
no_repeats_ex(G):- term_variables(G,VsIn),no_repeats_ex(VsIn,G).
no_repeats_ex([],G):-!, once(G).
no_repeats_ex([X],G):- assigned_label(X),!,copy_term(G:X,G2:X2,_Goals2),call(G2),X=X2.
no_repeats_ex([X],G):- is_many_values(X),!,call(G).
no_repeats_ex([X],G):- no_repeats(G),(is_many_values(G)->(true;begin_assign(X));true).

assigned_label(V):-nonvar(V),!,term_variables(V,Vars),maplist(assigned_label,Vars).
assigned_label(V):-attvar(V),has_cond(V,aoc(_,Num)),groundoid(Num),!.

begin_assign(V):-nonvar(V),!,term_variables(V,Vars),member(VV,Vars),begin_assign(VV).
begin_assign(V):- \+ attvar(V),!.
begin_assign(V):- has_cond(V,aoc(SkV,Num)),var(Num),!,which_skv(SkV,_Cardin,Num),add_cond(V,aoc(SkV,Num)).
% begin_assign(V):- has_cond(V,aoc(_SkV,Num)),atomic(Num),!.
begin_assign(_).


is_many_values(V):-nonvar(V),!,term_variables(V,Vars),member(E,Vars),is_many_values(E).
is_many_values(V):-attvar(V),has_cond(V,aoc(_,Num)), \+ groundoid(Num),!.


skolem_from_set(Set,X,SKF):-
  use_skf(SKF,Cardin,SkV,_DFml),  
  which_skv(SkV,Cardin,Which),
  nth1(Which,Set,X),
 %TODO attempt_checkout(X,SkV),
  nop(sanity(has_cond(X,aoc(SkV ,Which)))).

range_int(Which,N,Cardin):- '#>='(Which,N), '#=<'(Which, Cardin).

which_skv_soft(_SkV,Cardin,Which):- kif_option_value(gvar_skolem_combine,true),!, range_int(Which,1,Cardin),!.
which_skv(SkV,Cardin,Which):- var(SkV),!, between(1,Cardin,Which).
which_skv(SkV,Cardin,Which):-
  nb_get_next(SkV,Cardin,Offset),between(1,Cardin,For),
  once((Which1 is For+Offset,
  max_out_at(Which1,Cardin,Which))).

nb_get_next(SkV, Max, Which):-
  functor(SkV,SK,_),
  flag(SK,X,X+1), 
  (X == 0 -> Which=Max ; 
  ((X >= Max -> (Which=Max,flag(SK,_,1)) ; Which = X))).

max_out_at(In,Max,Out):- In =< Max -> In = Out 
  ; (Mid is In - Max, max_out_at(Mid,Max,Out)).
  
disp_ex(X):-fmt9(X).

lr:- quietly((listing(producing/1),listing(proven_tru/1),listing(make_existential/2),
  doall((current_key(K),recorded(K,P),
    locally(set_prolog_flag(write_attributes,portray),dmsg(P)))))).

clr:-
  doall((current_key(K),recorded(K,_,Ref),erase(Ref))).



test_count(Goal,N):- 
   findall(Goal,(Goal,format('~N~p~n',[Goal])),List),
   length(List,LL),
   LL==N.


%undo(Goal):- Redo = call(Goal), super_call_cleanup(true, (true; (Redo,setarg(1,Redo,true))), Redo).
%undo(Goal):- true; (Goal,fail).

/*
% one list note on PNF  the Way i convert loves(joe,mary) to PNF...

loves(joe,mary)  <=> 
exists([R,X,Y,T], ((subRelation(R,loves), is_a(T,time), is_a(T,context),exists_durring(X,T),exists_durring(Y,T),
 ~different(joe,X),~different(mary,Y)) -> trueIn(T,holds(R,X,Y)))).

*/

attvar_or_const(C):- attvar(C); (nonvar(C),nop((C==1->break,true))).

/*
:- kbe:import(baseKB:never_assert_u/1).
:- kbe:import(baseKB:never_assert_u/2).
:- system:import(baseKB:que/2).
*/
:- baseKB:ain((mtHybrid(Mt)==> {kb_shared(Mt:(nesc)/1)})).
:- baseKB:ain((mtHybrid(Mt)==> {kb_shared(Mt:proven_helper/1)})).
:- baseKB:ain((mtHybrid(Mt)==> {assert_if_new((Mt:nesc(P):- (zwc, nesc_lc(Mt, P))))})).
:- baseKB:ain(((mtHybrid(Mt)/(Mt\==baseKB)),mpred_prop(_,F,A,kbi_define))==> {kb_shared(Mt:F/A)}).

%:- kb_shared(call_tru/2).
% :- meta_predicate call_tru(?).
% :- ain((mtHybrid(Mt)==> {kb_shared(Mt:call_tru/2)})).

system:call_tru(M,X):- call(M:call,nesc(X)).
%system:call_tru(M,X):- findall(X,M:nesc(X),L),merge_compatibles(L,LO),!,member(X,LO).
% call_tru(X):- arg(1,X,E),call_e_tru(E,X).

:- module_transparent(call_e_tru/2).
% :- meta_predicate call_e_tru(*,*).
call_e_tru(_E,X):- proven_tru(X), \+ proven_neg((X)).

call_e_tru(_,X):- context_module(M), inherit_above(M, (X)).


% call_tru(P):- is_recorded(P).
% call_tru(P):- bless(P),(clause(existing(P),Body)*->Body; true),ignore(is_recorded(P)).

:- kb_shared(baseKB:proven_neg/1).

boxlog_to_prolog(IO,IO).


:- meta_predicate nrlc0(+).
:- module_transparent nrlc0/1.
nrlc0(G):- no_repeats(loop_check(G,(((dmsg(warn(looped(G)))),fail)))).


man(X):- \+ ground(X),
    (has_cond(X,man(X))->rem_cond(X,man(X)); true),
   nrlc0((proven_tru(man(X)))),has_cond(X,man(X)).
man(X):- (nonvar(X);not_has_cond(X,man(X)),!, nrlc0((nesc(man(X)))), \+ proven_neg(man(X))).
man(X):- context_module(M), inherit_above(M, man(X)).



/*

  rejiggle_quants( +FmlIn, -FmlOut, [options...]).


      converts terms like...

         forall(P,exists(M, person(P) => mother(P,M))).

      Into...

         forall(P, person(P) => exists(M, mother(P,M))).


      options are...


*/
% =================================
% Quanitifier Expansions
% =================================

rejiggle_quants(KB,In,Out2):-
  expandQuants(KB,In,Mid1),
  kif_optionally_e(false,moveInwardQuants([],elim(all),KB),Mid1,Mid2),
  un_quant3(KB,Mid2,Out),
  Out2 = Out.

expandQuants(_,Fml,Fml):- is_leave_alone(Fml),!.
expandQuants(_,[],[]):- !.
expandQuants(KB,[A|B],[AO|BO]):- expandQuants(KB,A,AO),expandQuants(KB,B,BO),!.

expandQuants(KB,all(XL,NNF),FmlO):- is_list(XL),
    (get_quantifier_isa(XL,X,Col) -> 
      expandQuants(KB,all(X,isa(X,Col) => NNF),FmlO);
      (XL=[X|MORE],!,
      (MORE==[] -> 
            expandQuants(KB,all(X,NNF),FmlO);
            expandQuants(KB,all(X,all(MORE,NNF)),FmlO)))).

expandQuants(KB,exactly(N,X,NNF),FmlO):- expandQuants(KB,quant(exactly(N),X,NNF),FmlO).
expandQuants(KB,atleast(N,X,NNF),FmlO):- expandQuants(KB,quant(atleast(N),X,NNF),FmlO).
expandQuants(KB,atmost(N,X,NNF),FmlO):- expandQuants(KB,quant(atmost(N),X,NNF),FmlO).
expandQuants(KB,exists(X,NNF),FmlO):- expandQuants(KB,quant(atleast(1),X,NNF),FmlO).
expandQuants(KB,some(X,NNF),FmlO):- expandQuants(KB,quant(exactly(1),X,NNF),FmlO).
expandQuants(KB,quant(exactly(0),X,NNF),FmlO):- expandQuants(KB,~exists(X,NNF),FmlO).
expandQuants(KB,quant(atmost(0),X,NNF),FmlO):- expandQuants(KB,quant(exactly(0),X,NNF),FmlO).


expandQuants(KB,quant(Quant,XL,NNF),FmlO):- is_list(XL),
    (get_quantifier_isa(XL,X,Col) -> 
      expandQuants(KB,quant(Quant,X,quant(isa(Col),X, NNF)),FmlO);
      (XL=[X|MORE],!,
      (MORE==[] -> 
            expandQuants(KB,quant(Quant,X,NNF),FmlO);
            expandQuants(KB,quant(Quant,X,quant(Quant,MORE,NNF)),FmlO)))).

expandQuants(KB,PAB,FmlO):- PAB=..[F|AB], must_maplist_det(expandQuants(KB),AB,ABO), FmlO=..[F|ABO].

un_quant3(_,Fml,Fml):- is_leave_alone(Fml),!.
un_quant3(KB,all(X,NNF),all(X,FmlO)):- !,un_quant3(KB,NNF,FmlO).
un_quant3(KB,exists(X,NNF),exists(X,FmlO)):- !,un_quant3(KB,NNF,FmlO).
un_quant3(KB,quant(atleast(1),X,NNF),exists(X,FmlO)):- !,un_quant3(KB,NNF,FmlO).
un_quant3(KB,quant(isa(K),X,NNF),FmlO):- un_quant3(KB,NNF,NNFO),un_quant3(KB,isa(X,K) & NNFO,FmlO).
un_quant3(KB,quant(Quant,X,NNF),quant(Quant,X,FmlO)):- un_quant3(KB,NNF,FmlO).
% un_quant3(KB,quant(Quant,X,NNF),FmlO):- un_quant3(KB,NNF,NNFO),Quant=..[Q|AUNT],append([Q|AUNT],[X,NNFO],STERM),FmlO=..STERM.
un_quant3(KB,PAB,FmlO):- PAB=..[F|AB], must_maplist_det(un_quant3(KB),AB,ABO), FmlO=..[F|ABO].

contains_compound(IN,CMP):-
 \+ ((sub_term(SUB,IN),compound(SUB),SUB=CMP)).

term_each_slot(IN,SUB):- sub_term(SUB,IN),is_ftVar(SUB).


%% moveInwardQuants(VarsQ,MayElimAll, ?KB, ?Kif, ?FmlO) is det.
%
% Move Existential Quantifiers Inward
%
moveInwardQuants(_,_,_,Fml,Fml):- is_leave_alone(Fml),!.
moveInwardQuants(_,_,_,[],[]):- !.
moveInwardQuants(_,_,_,~(NNF),~(FmlO)):-!,NNF=FmlO.
moveInwardQuants(VarsQ,MayElimAll,KB,[A|B],[AO|BO]):- moveInwardQuants(VarsQ,MayElimAll,KB,A,AO),moveInwardQuants(VarsQ,MayElimAll,KB,B,BO),!.
moveInwardQuants(VarsQ,cant(May),KB,all(X,NNF),all(X,FmlO)):-!,moveInwardQuants([X|VarsQ],cant(May),KB,NNF,FmlO).
moveInwardQuants(VarsQ,MayElimAll,KB,all(X,NNF),all(X,FmlO)):-!,moveInwardQuants([X|VarsQ],MayElimAll,KB,NNF,FmlO).


moveInwardQuants(VarsQ,MayElimAll,KB,quant(Quant,X,all(Y,FmlAB)),quant(Quant,X,FmlABM)):- !,
  moveInwardQuants([X|VarsQ],cant(MayElimAll),KB,all(Y,FmlAB),FmlABM).

moveInwardQuants(VarsQ,MayElimAll,KB,quant(Quant,X,quant(Q2,Y,FmlAB)),quant(Quant,X,FmlABM)):- !,
  moveInwardQuants([X|VarsQ],cant(MayElimAll),KB,quant(Q2,Y,FmlAB),FmlABM).

/*
moveInwardQuants(VarsQ,MayElimAll,KB,quant(Quant,X,FmlAB),quant(Quant,X,FmlABM)):- 
  (term_each_slot(FmlAB,Var),Var\==X,\+ member_eq(Var,VarsQ)),!,
  moveInwardQuants([Var,X|VarsQ],cant(MayElimAll),KB,FmlAB,FmlABM).
*/

moveInwardQuants(VarsQ,MayElimAll,KB,quant(Quant,X,FmlAB),FmlABO):- split_dlog_formula(FmlAB,OP,FmlA,FmlB),
   (((not_contains_var(X,FmlB)-> (moveInwardQuants(VarsQ,cant(MayElimAll),KB,quant(Quant,X,FmlA),FmlAO),unsplit_dlog_formula(OP,FmlAO,FmlB,FmlABO)));
   ((not_contains_var(X,FmlA)-> (moveInwardQuants([X|VarsQ],cant(MayElimAll),KB,quant(Quant,X,FmlB),FmlBO),unsplit_dlog_formula(OP,FmlA,FmlBO,FmlABO)));
    fail))),!.

moveInwardQuants(VarsQ,MayElimAll,KB,quant(Quant,X,FmlAB),quant(Quant,X,FmlABM)):- 
   moveInwardQuants([X|VarsQ],cant(MayElimAll),KB,FmlAB,FmlABM).


moveInwardQuants(VarsQ,MayElimAll,KB,PAB,FmlO):- PAB=..[F|AB], must_maplist_det(moveInwardQuants(VarsQ,MayElimAll,KB),AB,ABO), FmlO=..[F|ABO].

                                         

demodal_formua(KB,Fml,DLOG):- demodal_sents(KB,Fml,DFml),as_dlog(DFml,DLOG0),nnf(KB,DLOG0,DLOG).

fairness(KB,X,SkV,Slots,Fml,SFml):-   fail,
  minus_vars(Slots+Fml+SkV+X,KB,DFml),
  b_get_value('$nnf_outer',FullQuant),
  member(Form,FullQuant),
  minus_vars(DFml,Form,LeftOver),LeftOver==[],!,  
  demodal_formua(KB,Form,DFml),sformat(SFml,'~@',portray_clause_w_vars(DFml,[fullstop(false)])).
fairness(KB,_X,_SkV,_Slots,_Fml,DFml):-  fail,
  b_get_value('$nnf_outermost',FullQuant),  
  demodal_formua(KB,FullQuant,DFml).
fairness(KB,X,SkV,_Slots,Fml,Set):- 
  demodal_formua(KB,Fml,DFml),
  as_prolog_hook(DFml,PFml),
  conjuncts_to_list(PFml,List),
  fair_equality(SkV,X,List,List2),  
  predsort(fair_sort,List2,Set),!.
fairness(KB,_X,_SkV,_Slots,Fml,DFml):- 
  demodal_formua(KB,Fml,DFml),!.

fair_equality(SkV,X,[A],B):-fair_equality(SkV,X,A,AA),flatten([AA],B).
fair_equality(SkV,X,[A|B],AB):-fair_equality(SkV,X,A,AA),fair_equality(SkV,X,B,BB),append(AA,BB,AB).
fair_equality(SkV,X,(~B;A),AB):-fair_equality(SkV,X,A,AA),fair_equality(SkV,X,B,BB),append(AA,BB,AB).
fair_equality(SkV,X,(A,B),AB):-fair_equality(SkV,X,A,AA),fair_equality(SkV,X,B,BB),append(AA,BB,AB).
fair_equality(SkV,X,A,B):-fair_equality_3rd(SkV,X,A,AA),flatten([AA],B).

fair_equality_3rd(_SkV,_X,Var,z(Var)):-is_ftVar(Var).
fair_equality_3rd(SkV,X,(~A;B),BB):- \+ contains_var(X,A),!,fair_equality_3rd(SkV,X,B,BB).
fair_equality_3rd(SkV,X,(A;B),or(AA,BB)):-!,fair_equality_3rd(SkV,X,A,AA),fair_equality_3rd(SkV,X,B,BB).
fair_equality_3rd(SkV,X,(A,B),[AA,BB]):-!,fair_equality_3rd(SkV,X,A,AA),fair_equality_3rd(SkV,X,B,BB).
fair_equality_3rd(SkV,X,~(A),AA=false):- !,fair_equality_lit(SkV,X,A,AA).
fair_equality_3rd(SkV,X,poss(A),AA=poss):- !,fair_equality_lit(SkV,X,A,AA).
fair_equality_3rd(SkV,X,nesc(A),AA=true):- !,fair_equality_lit(SkV,X,A,AA).
fair_equality_3rd(SkV,X,poss(_,A),AA=poss):- !,fair_equality_lit(SkV,X,A,AA).
fair_equality_3rd(SkV,X,nesc(_,A),AA=true):- !,fair_equality_lit(SkV,X,A,AA).
fair_equality_3rd(SkV,X,(A),AA=true):- !,fair_equality_lit(SkV,X,A,AA).
fair_equality_3rd(_SkV,_X,A,A):-!.
fair_equality_3rd(SkV,X,(A),AA):-fair_equality_3rd(SkV,X,nesc(A),AA).


fair_equality_lit(_SkV,X,(A),a(A)):- A=..[_,XX],XX==X,!.
fair_equality_lit(SkV,X,(A;B),oR(AA,BB)):-!,fair_equality_3rd(SkV,X,A,AA),fair_equality_3rd(SkV,X,B,BB).
fair_equality_lit(SkV,_X,Var,m(Var)):- minus_vars(Var,SkV,Rest),Rest==[],!.
fair_equality_lit(SkV,X,Var,o(Var)):- minus_vars(Var,SkV+X,Rest),Rest==[],!.
fair_equality_lit(_SkV,X,Var,q(Var)):- \+ contains_var(X,Var),!.
fair_equality_lit(_SkV,_,A,p(A)):-!.

fair_sort(C,_=true,_=poss):- C = (<) .
fair_sort(C,X,Y):-compare(C,X,Y).


subst_except_copy(Fml,X,Y,FmlY):- subst(Fml,X,Y,FmlY).
% subst_except_copy(Fml,X,Y,FmlY):- subst(Fml,X,Z,FmlZ),copy_term(Z=FmlZ,Y=FmlY).

% =================================
% Typed (Exactly/AtMost/AtLeast 2 ((?x Man)(?y Woman)(?z Child)) ...                     )
% =================================

nnf_ex_nnf(KB,Fml,FreeV,NNF,Paths):- nnf_ex(KB,Fml,FreeV,NNF,Paths);nnf(KB,Fml,FreeV,NNF,Paths).


:- discontiguous(nnf_ex/5).

nnf_ex(KB,quant(exactly(N),XL,NNF),FreeV,FmlO,Paths):- is_list(XL),
    (get_quantifier_isa(XL,X,Col) -> 
      nnf(KB,quant(exactly(N),X,isa(X,Col) & NNF),FreeV,FmlO,Paths);
      (XL=[X|MORE],!,
      (MORE==[] -> 
            nnf(KB,quant(exactly(N),X,NNF),FreeV,FmlO,Paths);
            nnf(KB,quant(exactly(N),X,quant(exactly(N),MORE,NNF)),FreeV,FmlO,Paths)))).

nnf_ex(KB,quant(atleast(N),XL,NNF),FreeV,FmlO,Paths):- is_list(XL),
    (get_quantifier_isa(XL,X,Col) -> 
      nnf(KB,quant(atleast(N),X,isa(X,Col) & NNF),FreeV,FmlO,Paths);
      (XL=[X|MORE],!,
      (MORE==[] -> 
            nnf(KB,quant(atleast(N),X,NNF),FreeV,FmlO,Paths);
            nnf(KB,quant(atleast(N),X,quant(atleast(N),MORE,NNF)),FreeV,FmlO,Paths)))).

nnf_ex(KB,quant(atmost(N),XL,NNF),FreeV,FmlO,Paths):- is_list(XL),
    (get_quantifier_isa(XL,X,Col) -> 
      nnf(KB,quant(atmost(N),X,isa(X,Col) & NNF),FreeV,FmlO,Paths);
      (XL=[X|MORE],!,
      (MORE==[] -> 
            nnf(KB,quant(atmost(N),X,NNF),FreeV,FmlO,Paths);
            nnf(KB,quant(atmost(N),X,quant(atmost(N),MORE,NNF)),FreeV,FmlO,Paths)))).

nnf_ex(KB,exists(XL,NNF),FreeV,FmlO,Paths):- is_list(XL),
    (get_quantifier_isa(XL,X,Col) -> 
      nnf(KB,exists(X,isa(X,Col) & NNF),FreeV,FmlO,Paths);
      (XL=[X|MORE],!,
      (MORE==[] -> 
            nnf(KB,exists(X,NNF),FreeV,FmlO,Paths);
            nnf(KB,exists(X,exists(MORE,NNF)),FreeV,FmlO,Paths)))).


% =================================
% Typed (Exists ((?x Man)(?y Woman)) ... )
% =================================

nnf_ex(KB,exists(TypedX,NNF),FreeV,FmlO,Paths):- get_quantifier_isa(TypedX,X,Col),!,
    nnf(KB,exists(X, NNF & isa(X,Col)),FreeV,FmlO,Paths).
nnf_ex(KB,exists(XL,NNF),FreeV,FmlO,Paths):- is_list(XL),XL=[X],!,
    nnf(KB,exists(X,NNF),FreeV,FmlO,Paths).
nnf_ex(KB,exists(XL,NNF),FreeV,FmlO,Paths):- is_list(XL),XL=[X|MORE],!,
    nnf(KB,exists(X,exists(MORE,NNF)),FreeV,FmlO,Paths).

% =================================
% Untyped (Exists (?x)  Fml)
% =================================


% nnf_ex(KB,exists(X,Fml),FreeV,NNF,Paths):-  \+ contains_var(X,Fml),!, trace_or_throw(bad_nnf(KB,exists(X,Fml),FreeV,NNF,Paths)).
% maybe this instead ? 
nnf_ex(KB,exists(X,Fml),FreeV,NNF,Paths):-  \+ contains_var(X,Fml),dmsg(( \+ contains_var(X,Fml))),!,nnf(KB,Fml,FreeV,NNF,Paths).


% =================================
% Untyped (Exists (?x)  Fml)
% =================================

nnf_ex(KB,exists(X,Fml),FreeV,NNF,Paths):- !,
   nnf_ex_nnf(KB,quant(atleast(1),X,Fml),FreeV,NNF,Paths).

% ATTVAR WAY
nnf_ex(KB,exists(X,Fml),FreeV,NNF1,Paths):- fail, !,
 must_det_l((
    % add_cond(X,extensional(X)),
    term_slots(KB+FreeV+Fml,SSlots),list_to_set(SSlots,Slots),
    fairness(KB,X,SkV,Slots,Fml,DFml),
    skolem_f(1,KB, DFml, X, Slots, _Fun,SkV),
    put_attr(X,skv,SkV),    
    nnf(KB, Fml <=> skolem(X,skF(1,SkV,X,DFml)),FreeV,NNF1,Paths)
   )),!.

skv:attr_unify_hook(A,V):- dmsg(skv:attr_unify_hook(A,V)).


% =================================
% ==== AtLeast N ========
% ==== Cardinality (quantifier macros) ========
% =================================
% AtLeast 1:  We simply create the existence of 1
nnf_ex(KB,quant(atleast(N),X,Fml),FreeV,NNF,Paths):- fail, N==1, !,
   nnf_ex_nnf(KB,exists(X,Fml),FreeV,NNF,Paths).


nnf_ex(KB, ~ quant(atleast(N),X,Fml), FreeV,NNF,Paths):- NN is N - 1,
   nnf_ex_nnf(KB,quant(atmost(NN),X,Fml),FreeV,NNF,Paths).

nnf_ex(KB,quant(atleast(N),X,Fml),FreeV,NNF1,Paths):-  kif_option(true,skolem(nnf)), !,
 must_det_l((
    % add_cond(X,extensional(X)),
    term_slots(KB+FreeV+Fml,SSlots),list_to_set(SSlots,Slots),
    fairness(KB,X,SkV,Slots,Fml,DFml),
    skolem_f(N,KB, DFml, X, Slots,_Fun,SkV),
    put_attr(X,skv,SkV),
    nnf(KB, (Fml <=> skolem(X, skF(N,SkV,X,DFml))),FreeV,NNF1,Paths)
   )),!.

/*
nnf_ex(KB,quant(atleast(N),X,Fml),FreeV,NNF,Paths):- N == 2, !, 
   NEWFORM = ((skolem(X,skFn_only(Id,Set),KB) & idOf(X,Id,Set) & isSet(Set)) <=> Fml),
   add_var_to_env("Id",Id),add_var_to_env("Set",Set),
   nnf(KB,NEWFORM,FreeV,NNF,Paths),!.

% AtLeast 2: (This is just to confirm code .. thus, will comment out to use "AtLeast X:" rule)
nnf_ex(KB,quant(atleast(N),X,Fml),FreeV,NNF,Paths):-  N==2, !,          
  subst_except(Fml,X,Y,FmlY),
  %  Would this work?             
  % NEWFORM = ((exists(X,Fml) & exists(Y, FmlY & different(X,Y)))),
  %  or does it reify to be implication?
  NEWFORM =  ~  ( ~ different(X,Y) v exists(X,Fml)) v exists(Y,FmlY),
  %  exists 2 differnt?
  % NEWFORM =  (different(X,Y) <=> (exists(X,Fml) & exists(Y,FmlY))),
  nnf(KB,NEWFORM,FreeV,NNF,Paths).

nnf_ex(KB,quant(atleast(N),X,Fml),FreeV,NNF,Paths):- N > 1, kif_option(false,skolem(setOf)),!,  
   NEWFORM =  (all(X, exists(Set, (sizeOfLeast(Set,N) & elem(X,Set)))) <=> Fml), add_var_to_env("Set",Set),
   nnf(KB,NEWFORM,FreeV,NNF,Paths).
*/

% =================================
% ==== AtMost N ========
% ==== Cardinality (quantifier macros) ========
% =================================
nnf_ex(KB,~quant(atmost(0),X,Fml),FreeV,NNF,Paths):-  !,
  nnf_ex_nnf(KB, exists(X,Fml),FreeV,NNF,Paths).
                                               
nnf_ex(KB,quant(atmost(0),X,Fml),FreeV,NNF,Paths):-  !,
  nnf_ex_nnf(KB,all(X,~(Fml)),FreeV,NNF,Paths).

nnf_ex(KB,quant(atmost(N),X,Fml),FreeV,NNF1,Paths):-  kif_option(true,skolem(nnf)), !,
 must_det_l((
    % add_cond(X,extensional(X)),
    term_slots(KB+FreeV+Fml,SSlots),list_to_set(SSlots,Slots),
    fairness(KB,X,SkV,Slots,Fml,DFml),
    skolem_f(N,KB, DFml, X, Slots,_Fun,SkV),
    put_attr(X,skv,SkV),
    nnf(KB, ( skolem(X, if_all_different(N,SkV,DFml)) => ~Fml),FreeV,NNF1,Paths)
   )),!.


% AtMost 1: "If there exists 1 there does not exist 1 other"
nnf_ex(KB,quant(atmost(N),X,Fml),FreeV,NNF,Paths):- N == 1, !, trace,
   subst_except_copy(Fml,X,Y,FmlY),
   NEWFORM =  ~( exists(X,Fml) & exists(Y,FmlY) & different(X,Y)),
   nnf(KB,NEWFORM,FreeV,NNF,Paths).

% AtMost N: "If there exists at least N there does not exist 1 other"
nnf_ex(KB,quant(atmost(N),X,Fml),FreeV,NNF,Paths):-   !,  trace,
   subst_except_copy(Fml,X,Y,FmlY),
   NEWFORM =  (quant(atleast(N),X,Fml) => ~(exists(Y, FmlY & different(X,Y)))),
   nnf(KB,NEWFORM,FreeV,NNF,Paths).

/*
% AtMost N: "If there exists 1 then there exists at most N-1"
nnf_ex(KB,quant(atmost(N),X,Fml),FreeV,NNF,Paths):- NewN is N - 1, !,
   subst_except(Fml,X,Y,FmlY),
   NEWFORM = (exists(Y, FmlY) => quant(atmost(NewN),X,Fml)),
  nnf(KB,~different(X,Y) v NEWFORM,FreeV,NNF,Paths).
*/

% nnf_ex(KB,t(isNamed,X,Name),FreeV,NNF,Paths):- !,nnf_ex(KB,exactly(1,X,isNamed(X,Name)),FreeV,NNF,Paths).

% =================================
% ==== Exactly N ========
% ==== Cardinality (quantifier macros) ========
% =================================
nnf_ex(KB,quant(exactly(0),X,Fml),FreeV,NNF,Paths):- !,
  nnf(KB,all(X,~Fml),FreeV,NNF,Paths).

nnf_ex(KB,~(quant(exactly(0),X,Fml)),FreeV,NNF,Paths):- !,
  nnf(KB,exists(X,Fml),FreeV,NNF,Paths).

% Exactly 1: "If there exists 1 there does not exist 1 other"
nnf_ex(KB,quant(exactly(N),X,Fml),FreeV,NNF,Paths):- fail, N == 1, !,
   subst_except_copy(Fml,X,Y,FmlY),
   NEWFORM =  (exists(Y,FmlY) & (exists(Y,FmlY) & different(X,Y) => ~( exists(X,Fml)))),
   nnf(KB,NEWFORM,FreeV,NNF,Paths).

% Exactly N: states "There is AtMost N /\ AtLeast N"
nnf_ex(KB,quant(exactly(N),X,Fml),FreeV,NNF,Paths):- !,
   % subst_except_copy(Fml,X,Y,FmlY),
   X=Y,FmlY=Fml,
   NEWFORM = (quant(atleast(N),X,Fml) & quant(atmost(N),Y,FmlY)),
   nnf(KB,NEWFORM,FreeV,NNF,Paths).

/*
nnf_ex(KB,~ quant(exactly(N),X,Fml),FreeV,NNF,Paths):- N > 0,!,
   Minus1 is N-1,Plus1 is N+1,
   NEWFORM =  (quant(atleast(Plus1),X,Fml) <=> ~quant(atmost(Minus1),X,Fml)),
   nnf(KB,NEWFORM,FreeV,NNF,Paths).


nnf_ex(KB,quant(exactly(N),X,Fml),FreeV,NNF,Paths):- !,
   subst_except_copy(Fml,X,Y,FmlY),
   NEWFORM = all(Y , ((quant(atleast(N),X,Fml) & different(X,Y))) => ~FmlY),
   nnf(KB,NEWFORM,FreeV,NNF,Paths).


nnf_ex(KB,quant(exactly(N),X,Fml),FreeV,NNF1,Paths):- fail,
 kif_option(true,skolem(nnf)), !,
 must_det_l((
    % add_cond(X,extensional(X)),
    term_slots(KB+FreeV+Fml,SSlots),list_to_set(SSlots,Slots),
    skolem_f(N,KB, Fml, X, [KB|Slots], SkF),    
    nnf(KB, Fml <=> skolem(X, co unt(N,N,SkF)),FreeV,NNF1,Paths)
   )),!.
*/

% =================================
% ==== basic macros ========
% ==== Cardinality (quantifier macros) ========
% =================================

% AllDifferent 4: "All 4 members are different"
nnf_ex(KB,allDifferent([A,B,C,D]),FreeV,NNF,Paths):- 
  NEWFORM =  (different(A,B) & different(A,C) & different(A,D) & different(B,C) & different(B,D) & different(C,D)),
  nnf(KB,NEWFORM,FreeV,NNF,Paths).

% AllDifferent SET: "All members are different"
nnf_ex(KB,allDifferent(SET),FreeV,NNF,Paths):- is_using_feature(list_macros),is_using_feature(inline_prolog),!,
  NEWFORM =  (
    {member(X,SET),member(Y,SET),X\==Y} 
       =>different(X,Y) ),
   nnf(KB,NEWFORM,FreeV,NNF,Paths).

  	 

%% mk_skolem( ?KB, ?F, ?X, ?FreeV, ?Out) is det.
%
% Make Skolem.
%

mk_skolem(KB, Fml, X, FreeV, FmlOut):-  
   must(skolem_f(1,KB, Fml, X, FreeV,_Fn, Sk)),   
   must(FmlOut= Fml),
   !,show_call(why, asserta((constraintRules(X,Sk,Fml)))),
   form_sk(X,Sk).

mk_skolem(KB, F, X, FreeV, FmlSk):- 
    must(skolem_f(1,KB, F, X, FreeV,_Fn, Sk)), 
    must(subst_except(F, X, Sk, FmlSk)),!.

%% skolem_f(N, ?KB, ?F, ?X, ?FreeVIn, ?Sk) is det.
%
% Skolem Function.
%

skolem_f(_N,_KB, _F, X, _FreeVIn,Fun,SkV):- get_attr(X,skv,SkV),!,functor(SkV,Fun,_),!.
skolem_f(N,KB, F, X, FreeVIn,Fun,SkV):- 
       must_det_l((
        delete_eq(FreeVIn,KB,FreeV0),
        delete_eq(FreeV0,X,FreeV),
        list_to_set(FreeV,FreeVSet),
	% contains_var_lits(F,X,_LitsList),
        (get_var_name(X,VN)->true;VN='Exists'),
        mk_skolem_name(KB,X,F,VN,SK),
        flag(skolem_count,SKN,SKN+1),
        concat_atom(['sk',SK,'_',N,'_',SKN,'FnSk'],Fun),
        SkV =..[Fun|FreeVSet],
        put_attr(X,skv,SkV))),!.
   
    
:- if(app_argv('--www') ; app_argv('--plweb'); app_argv('--irc')).
%:- if(exists_source(pack(logicmoo_base/t/examples/fol/attvar_existentials))).
%:- user:ensure_loaded((pack(logicmoo_base/t/examples/fol/attvar_existentials))).
%:- endif.
:- endif.        




is_fort(Term):- var(Term)->is_existential(Term);atom(Term).

is_value(Term):-atomic(Term).

% ?- A = a(1) mpred_constrain_w_proxy(A).
mpred_constrain_w_proxy(Goal):- \+ compound(Goal),!.
mpred_constrain_w_proxy(Goal):- functor(Goal,F,_),  mpred_constrain_w_proxy_enter(1,F,Goal),
  term_attvars(Goal,Vars),
  maplist(show_attrs,Vars),
  dmsg(mpred_constrain_w_proxy(Goal)).

show_attrs(Var):- oo_get_attrs(Var,Atts),dmsg(Var=Atts).

% todo use: push_cond(X,Dom)
mpred_set_arg_isa(Pred,N,Term,_Outer):- holds_attrs(Term),push_cond(Term,argIsaFn(Pred,N)),!.
mpred_set_arg_isa(Pred,N,Term,Outer):- 
  (is_value(Term);is_function_expr('=>',Term)),!,
  must(( setarg(N,Outer,NewVar),
   % destructive_replace(Outer,Term,NewVar),   
   push_condx(NewVar,mudEquals(NewVar,Term)))),
   push_cond(NewVar,argIsaFn(Pred,N)),!.
mpred_set_arg_isa(_Pred,_N,Term,_Outer):- compound(Term),!,must(mpred_constrain_w_proxy(Term)),!.
mpred_set_arg_isa(_Pred,_N,_Term,_Outer).

mpred_constrain_w_proxy_enter(N,Pred,Outer):- arg(N,Outer,Term),!,
 must(mpred_set_arg_isa(Pred,N,Term,Outer)),
 N2 is N+1,
 mpred_constrain_w_proxy_enter(N2,Pred,Outer).
mpred_constrain_w_proxy_enter(_,_,_).

% ?- A=a(1,1),destructive_replace(A,1,one),A=a(one,one).
destructive_replace(Outer,Term,NewVar):-arg(N,Outer,Value) ,Value==Term,setarg(N,Outer,NewVar),destructive_replace(Outer,Term,NewVar),!.
destructive_replace(Outer,Term,NewVar):- Outer=..[_|ARGS],maplist(destructive_replace,ARGS,Term,NewVar),!.   
destructive_replace(_Outer,_Term,_NewVar).


form_sk(OtherValue, Skolem):- sk:attr_unify_hook(Skolem, OtherValue),!.
% form_sk(OtherValue, Skolem):- nonvar(OtherValue).


% push_cond(_,_):- \+ is_skolem_setting(push_skolem),!.
% push_cond(X,Form2):-annote(cond, X,Form2,_Merged).
push_cond(X,Dom):- push_condx(X,isaDom(X,Dom)).

isaDom(X,[Y|Z]):- !,maplist(isaDom(X),[Y|Z]).
isaDom(X,Y):- ((call_u(isa(X,Y)) *-> true; true)).

annote(Dom,X,Form2):- must(annote(Dom,X,Form2,_)).

% annote(_,_,IO,IO):- \+ is_skolem_setting(push_skolem),!.
% annote(_,X,Form2,Form2):- var(X), freeze(X,(ground(X)->show_call(for(X),lazy(call_u(Form2)));true)).
annote(Dom,X,Form2,SK_FINAL):- oo_get_attr(X,Dom,Form1),merge_forms(Form1,Form2,SK_FINAL),oo_put_attr(X,Dom,SK_FINAL),!.
annote(_,X,IO,IO):- is_ftNonvar(X),!.
annote(Dom,X,Form2,Form2):- oo_put_attr(X,Dom,Form2).

is_skolem(Sk):-get_skolem(Sk,_Form).
get_skolem(Sk,Form):-oo_get_attr(Sk, sk, Form),!.

x_skolem(Sk,skolem(Sk,Form)):-get_skolem(Sk,Form),del_attr(Sk,sk).

transform_skolem_forms(Sk,Form):- x_skolem(Sk,Form),!.
transform_skolem_forms(Var,true):- var(Var),!.
transform_skolem_forms(Head,HeadExtra):- term_attvars(Head,AttVars),include(AttVars,is_skolem,HeadAttVars),
  transform_skolem_forms_l(HeadAttVars,HeadExtra).

transform_skolem_forms_l([Sk],Form):- x_skolem(Sk,Form),!.
transform_skolem_forms_l([Sk|Head],(Form,HeadExtra)):-  x_skolem(Sk,Form), transform_skolem_forms_l(Head,HeadExtra).


%%	sk_form(+Sk, -Form) is semidet.
%
%	True if Sk has been assigned Form or is a Free variable.

sk_form(Sk, Form) :- oo_get_attr(Sk, sk, Form),!.
sk_form(Var,Form):- var(Var),!,gensym(sk_other_,Form), dtrace, oo_put_attr(Var, sk, Form).
sk_form(sk(Value),Value):-!.

push_condx(X,Form):- annote(condx,X,Form,_Merged).

condx:attr_unify_hook(Cond,Value):- var(Value),!,push_condx(Value,Cond),!. 
% ?- A=a(1),mpred_constrain_w_proxy(A),trace,A=a(Z),Z=1.0.
condx:attr_unify_hook([X|Cond],_Value):- !, maplist(call_u,[X|Cond]).
condx:attr_unify_hook(Cond,_Value):- call_u(Cond).

%push_skolem(Onto,SK_ADD):- var(Onto), \+ attvar(Onto), nop(dmsg(warn(var_not_push_skolem(Onto,SK_ADD)))),!.
push_skolem(Onto,SK_ADD):-push_skolem(Onto,SK_ADD,_).

push_skolem(Onto,SK_ADD,SK_FINAL):- oo_get_attr(Onto,sk,SLPREV),!,merge_forms(SLPREV,SK_ADD,SK_FINAL),sk_replace(Onto,SK_FINAL),!.

push_skolem(Onto,SK_ADD,SK_FINAL):- var(Onto),!,SK_FINAL=SK_ADD,sk_replace(Onto,SK_FINAL),!.
push_skolem(Onto,SK_ADD,SK_FINAL):- sk_form(Onto,SLPREV),!,merge_forms(SLPREV,SK_ADD,SK_FINAL),sk_replace(Onto,SK_FINAL),!.
push_skolem(Onto,SK_ADD,SK_ADD):- sk_replace(Onto,SK_ADD).

sk_replace(Onto,SK_FINAL):-var(Onto),!,annote(sk,Onto,SK_FINAL).
sk_replace(_Into,_SKFINAL):-!,fail.


sk:attr_unify_hook(Form, OtherValue):-OtherValue==Form,!.
sk:attr_unify_hook(_Form, _OtherValue):- local_override(no_kif_var_coroutines,G),!,call(G).
sk:attr_unify_hook(Form, OtherValue):- var(OtherValue),!,push_skolem(OtherValue,Form),!.
%sk:attr_unify_hook(Form, OtherValue):- contains_var(OtherValue,Form),!.
%sk:attr_unify_hook(Form, OtherValue):- contains_var(Form,OtherValue),!.
% sk:attr_unify_hook(Form, OtherValue):- skolem_unify(OtherValue,Form).

sk:attr_portray_hook(Form, SkVar) :- writeq(sk(SkVar,Form)).

%sk:project_attributes(QueryVars, ResidualVars):- fail,nop(dmsg(sk:proj_attrs(skolem,QueryVars, ResidualVars))).

:- module_transparent(portray_sk/1).
portray_sk(Sk) :- not_debugging, dictoo:oo_get_attr(Sk, sk, Form),!, printable_variable_name(Sk,Name), format('sk_avar(~w,~p)',[Name,Form]).

:- system:import(portray_sk/1).



%=

%% attr_portray_hook( ?Value, ?Var) is semidet.
%
% Attr Portray Hook.
%
 % vn:attr_portray_hook(Name, _) :- write('???'), write(Name),!.
%vn:attr_portray_hook(_, _) :- !.
%sk:attr_portray_hook(_, _) :- !.

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
% user:portray(Sk):- get_attr(Sk, vn, Name), get_attrs(Sk,att(vn,Name,[])),write(Name),!,write('{}').

/*

%% portray_attvar( ?Var) is semidet.
%
% Hook To [portray_attvar/1] For Module Logicmoo_varnames.
% Portray Attribute Variable.
%
:- if(false).

:- abolish('$attvar':portray_attvar/1). 
:- public('$attvar':portray_attvar/1).

'$attvar':portray_attvar(Sk) :- get_attr(Sk, vn, Name),atomic(Name), write('_'),write(Name),get_attrs(Sk,att(vn,Name,[])),!.

'$attvar':portray_attvar(Sk) :-
   get_attr(Sk, sk, _), write('_sk'),!. % get_attrs(Sk,att(vn,Name,att(sk,Name,[]))),!.

'$attvar':portray_attvar(Var) :-
   write('{'),
   get_attrs(Var, Attr),
   '$attvar':portray_attrs(Attr, Var),
   write('}'),!.

'$attvar':portray_attvar(Var) :-
	write('{<'),
        ((get_attr(Var,vn, VarName))->true;sformat(VarName,'~w',[Var])),
	get_attrs(Var, Attr),
	catch(writeq('??'(VarName,Attr)),_,'$attvar':portray_attrs(Attr, Var)),
	write('>}').

:- endif.
*/

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
% user:portray(Sk):- get_attr(Sk, sk, _Form) , loop_check(common_logic_skolem:portray_sk(Sk)),!.

%% sk_form:attribute_goals(@V)// is det.
%	copy_term/3, which also determines  the   toplevel  printing  of
%	residual constraints.

% % % sk:attribute_goals(Sk) --> {sk_form(Sk, Form)},!,[form_sk(Sk,Form)].

skolem_test(_):- !.
skolem_test(Form):- show_call(call_u(Form)).

skolem_unify(_Var,Form):- skolem_test(Form).


member_eqz(E, [H|T]) :-
    (   E == H
    ->  true
    ;   member_eqz(E, T)
    ).

merge_forms(A,B,A):- A==B,!.
merge_forms(A,B,B):- member_eqz(A,B),!.
merge_forms(A,B,A):- member_eqz(B,A),!.
merge_forms(A,B,A):- A=B,!,dmsg(seeeeeeeeeeeee_merge_forms(A,B)),!.
merge_forms(A,B,C):- flatten([A,B],AB),must(list_to_set(AB,C)),!.



atom_concat_if_new_info(SIn,CU,SIn):- atom_length(CU,L),L>1,atom_contains(SIn,CU),!.
atom_concat_if_new_info(CU,SIn,SIn):- atom_length(CU,L),L>1,atom_contains(SIn,CU),!.
atom_concat_if_new_info(SIn,CU,SOut):- atom_concat(SIn,CU,SOut).


mk_skolem_name(X,Fml,Name):-mk_skolem_name(_KB,X,Fml,'',Name).

skip_log_op(true).
skip_log_op(X):- is_log_op(X).
%% mk_skolem_name(KB, +Var, +TermFml, +SuggestionIn, -NameSuggestion) is det.
%
%  generate a skolem name..
%
mk_skolem_name(_KB,Var,Fml,SIn,SOut):- is_ftVar(Fml),same_var_sk(Var,Fml),!,atom_concat_if_new_info('VFml',SIn,SOut).
mk_skolem_name(_KB,_V, Fml,SIn,SOut):- is_ftVar(Fml),!,atom_concat_if_new_info('VaR',SIn,SOut).

mk_skolem_name(_KB,_V,[],SIn,SIn):- !.
mk_skolem_name(_KB,_V, OP,SIn,SIn):- atom(OP),OP\==poss,OP\==(&),skip_log_op(OP),!.
mk_skolem_name(KB,Var,H=Y,SIn,SOut):- atom(Y),mk_skolem_name(KB,Var,[Y,H],SIn,SOut),!.
mk_skolem_name(KB,Var,z(H),SIn,SOut):- mk_skolem_name(KB,Var,['Of',H],SIn,SOut),!.
mk_skolem_name(KB,Var,a(H),SIn,SOut):- mk_skolem_name(KB,Var,H,SIn,SOut),!.
mk_skolem_name(KB,Var,FH,SIn,SOut):- FH=..[Y,H],atom_length(Y,1),!,mk_skolem_name(KB,Var,[H],SIn,SOut).
mk_skolem_name(KB,Var,(H,T),SIn,SOut):- !,mk_skolem_name(KB,Var,H,SIn,M),mk_skolem_name(KB,Var,T,M,SOut).
mk_skolem_name(KB,Var,(H&T),SIn,SOut):- !,mk_skolem_name(KB,Var,H,SIn,M),mk_skolem_name(KB,Var,T,M,SOut).
mk_skolem_name(KB,Var,(H;T),SIn,SOut):- !,mk_skolem_name(KB,Var,H,SIn,M),!,mk_skolem_name(KB,Var,['Or'|T],M,SOut).
mk_skolem_name(_KB,_V, OP,SIn,SIn):- atom(OP),atom_concat('sk',_,OP),!.
mk_skolem_name(_KB,_V,Fml,SIn,SOut):- atomic(Fml),!,must((i_name(Fml,N),toPropercase(N,CU))),!,atom_concat_if_new_info(SIn,CU,SOut).
mk_skolem_name(KB,Var,[H|T],SIn,SOut):- T==[], !,mk_skolem_name(KB,Var,H,SIn,SOut),!.
mk_skolem_name(KB,Var,[H|T],SIn,SOut):- !,mk_skolem_name(KB,Var,H,SIn,M),!,mk_skolem_name(KB,Var,T,M,SOut).
mk_skolem_name(KB,Var,isa(VX,Lit),SIn,SOut):- same_var_sk(Var,VX),is_ftNonvar(Lit),!,mk_skolem_name(KB,Var,['Isa',Lit],'',Mid),atom_concat_if_new_info(Mid,SIn,SOut).
mk_skolem_name(KB,Var,inst(VX,Lit),SIn,SOut):- same_var_sk(Var,VX),is_ftNonvar(Lit),!,mk_skolem_name(KB,Var,['Inst',Lit],'',Mid),atom_concat_if_new_info(Mid,SIn,SOut).

% mk_skolem_name(KB,Var,FmlO,SIn,SOut):-FmlO=..[FO|ARGS],member(Fml,ARGS),compound(Fml),Fml=..[F,VX],same_var_sk(Var,VX),!,mk_skolem_name(KB,Var,[FO,F],SIn,Mid),atom_concat_if_new_info(Mid,SIn,SOut).
mk_skolem_name(KB,Var,FmlO,SIn,SOut):-FmlO=..[FO|ARGS],member(Fml,ARGS),compound(Fml),contains_var(Var,Fml),!,mk_skolem_name(KB,Var,[FO,Fml],SIn,SOut).

mk_skolem_name(KB,Var,Fml,SIn,SOut):- fail, Fml=..[F,Other,VX|_],same_var_sk(Var,VX),!,(type_of_var(KB,Other,OtherType0),
   (OtherType0=='Unk'->OtherType='';OtherType=OtherType0)),
   mk_skolem_name(KB,Var,[OtherType,'Arg2Of',F],SIn,SOut).

mk_skolem_name(_KB,Var,Fml,SIn,SOut):- arg(1,Fml,VX),functor(Fml,F,_),same_var_sk(Var,VX),!,
  i_name(F,Lit), atomic_list_concat([Lit],Added),atom_concat_if_new_info(SIn,Added,SOut).

mk_skolem_name(_KB,Var,Fml,SIn,SOut):- arg(N,Fml,VX),functor(Fml,F,_),number_string(N,NStr),same_var_sk(Var,VX),!,
  i_name(F,Lit), atomic_list_concat(['Arg',NStr,Lit],Added),atom_concat_if_new_info(SIn,Added,SOut).

  

mk_skolem_name(_KB,Var,Fml,SIn,SOut):- Fml=..[F,VX|_],same_var_sk(Var,VX),!,i_name(F,Lit), atomic_list_concat([Lit],Added),atom_concat_if_new_info(SIn,Added,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F,VX],same_var_sk(Var,VX),!,mk_skolem_name(KB,Var,['Is',F],'',Mid),atom_concat_if_new_info(Mid,SIn,SOut).

mk_skolem_name(_KB,_Var,_Fml,SIn,SIn).

same_var_sk(X,Y):-X==Y.

% same_var_sk(Var,Fml):-  ~(  ~( Var=Fml)),!.

:- module_transparent(kbi_define/3).
kbi_define(M,F,A):- M:clause_b(mpred_prop(M,F,A,kbi_define)),!.
kbi_define(M,F,A):- M:ain(mpred_prop(M,F,A,kbi_define)),
 functor(P,F,A),(predicate_property(M:P,static)->true;kbi_define_now(M,F,A,P)).
                           
:- module_transparent(kbi_define_now/3).
kbi_define_now(M,F,A,P):-
  M:kb_shared(M:F/A),
  dmsg(kbi_define(M:F/A)),
  % ((M:ain(P:- (findall(P,call_tru(M, P),L),merge_compatibles(L,LO),!,member(P,LO))))),
  ((M:ain(P:- call_tru(M, P)))),
  kb_shared(M:F/A).


:- meta_predicate(kbi:kbi_define(+)).
:- module_transparent(kbi:kbi_define/1).
:- export(kbi:kbi_define/1).
kbi:kbi_define(MFA):- 
  get_mfa(MFA,M,F,A),
  M:kbi_define(M,F,A).


:- fixup_exports.



end_of_file.

:- kbi:kbi_define(baseKB:isNamed/2).
:- kbi:kbi_define(baseKB:proven_tru/1).


notes about transfation:



loves(X,Y):-  (nonvar(X);nonvar(Y)),
              (has_cond(X,(loves(X,Y)))->rem_cond(X,(loves(X,Y))); true),
              (has_cond(Y,(loves(X,Y)))->rem_cond(Y,(loves(X,Y))); true),
              nrlc0(proven_tru(loves(X,Y))),
              (has_cond(X,(loves(X,Y)));has_cond(Y,(loves(X,Y)))),
              (attvar_or_const(X),attvar_or_const(Y)).
loves(X,Y):- (nonvar(X);not_has_cond(X,(loves(X,Y))),!, nrlc0((nesc((loves(X,Y)))))),
             (nonvar(Y);not_has_cond(Y,(loves(X,Y))),!, nrlc0((nesc((loves(X,Y)))))), 
             \+ proven_neg(loves(X,Y)),
             attvar_or_const(X),attvar_or_const(Y).
loves(X,Y):- context_module(M), inherit_above(M, (loves(X,Y))).










