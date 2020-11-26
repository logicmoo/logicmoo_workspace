/* <module> mpred_pttp
% Provides a prolog database replacent that uses PTTP
%
%  wid/3
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_pttp,[]).
%:- endif.

:- '$set_source_module'(baseKB).

:- thread_local(t_l:disable_px/0).

%:- ensure_loaded(library(pfc)).
:- include(logicmoo('pfc2.0'/'mpred_header.pi')).
% :- ensure_loaded(library(logicmoo_utils)).

ainz_pttp(A):-if_defined(mpred_ainz(A),assertz_new(A)).
%:- was_export(internal_functor/1).
%:- was_export(was_pttp_functor/1).
%:- was_dynamic(was_pttp_functor/1).
:- kb_shared(baseKB:wid/3).
:- was_export(int_query/7).
:- was_dynamic(int_query/7).
:- was_export(int_not_query/7).
:- was_dynamic(int_not_query/7).

% -- CODEBLOCK
:- was_export(pttp_ask/1).
pttp_ask(CALL):-nonegate(_KB,CALL,NNCALL),correct_pttp(NNCALL,REALCALL),apply(REALCALL,[ [], [], 100, _OneHundred, _Proof, [_In|[]]]).

%:- was_dynamic(was_pttp_functor/2).

/*
% -- CODEBLOCK
%=% Substitution
:- was_export(subst_eq/4).
% Usage: subst_eq(+Fml,+X,+Sk,?FmlSk)
subst_eq(Fml,X,Sk,FmlSkO):- pred_subst(==,Fml,X,Sk,FmlSk),!,must(FmlSkO=FmlSk),!.


% -- CODEBLOCK
% Usage: pred_subst(+Pred,+Fml,+X,+Sk,?FmlSk)
:- was_export(pred_subst/5).

pred_subst( Pred, P,       X,Sk,       P1    ) :- call(Pred,P,X),!,must( Sk=P1),!.
pred_subst(_Pred, P,       _,_ ,       P1    ) :- is_ftVar(P),!, must(P1=P),!.
pred_subst( Pred,[P|Args], X,Sk,    [P1|ArgS]) :- !, pred_subst(Pred,P,X,Sk,P1),!, must(pred_subst( Pred, Args,X, Sk, ArgS )),!.
pred_subst( Pred, P,       X,Sk,       P1    ) :- compound(P),!, P =..Args, pred_subst( Pred, Args,X, Sk, ArgS ),!, must(P1 =..ArgS),!.
pred_subst(_Pred ,P,       _, _,       P     ).

% -- CODEBLOCK
:- was_export(must/1).
:- meta_predicate(must(0)).
must(Call):-(repeat, (catch(Call,E,(dmsg(E:Call),set_prolog_flag(runtime_debug,true),fail)) *-> true ; (ignore(ftrace(Call)),leash(+all),repeat,wdmsg(failed(Call)),dtrace,Call)),!).

*/

% --
:- was_export(pttp_call/1).
pttp_call(Goal) :- !,pttp_call(Goal,70,0,3,[],_,no).
pttp_call(Goal,Max,Min,Inc,ProofIn,ProofOut,ShowProof):-
  pttp_prove(Goal,Max,Min,Inc,ProofIn,ProofOut,ShowProof).


% -- CODEBLOCK
:- was_export(pttp_load_wid/1).
pttp_load_wid(Name):-must(pttp_logic(Name,Data)),!,must(pttp_load_wid(Name,Data)).
:- was_export(pttp_load_wid/2).
pttp_load_wid(Name,Data):- must(retractall_wid(Name)),dmsg(pttp_load_wid(Name)),must(pttp_tell_wid(Name:0,Data)),!.
uses_logic(Name):-pttp_logic(Name,Data),pttp_load_wid(Name,Data).


% -- CODEBLOCK
:- was_export(pttp_assert/1).
pttp_assert(X) :- must_pttp_id(ID),pttp_tell_wid(ID,X).

% -- CODEBLOCK
:- was_export(pttp_tell_wid/2).
pttp_tell_wid(ID,XY):- 
    with_no_mpred_expansions(
       locally_tl(disable_px,
          locally_tl(infSkipFullExpand,
            must(pttp_assert_wid(ID,pttp,XY))))),!.

:- was_export(pttp_assert_wid/3).
pttp_assert_wid(ID,Mode,(X,Y)):- !, pttp_assert_wid(ID,Mode,X),kb_incr(ID,ID2), pttp_assert_wid(ID2,Mode,Y).
pttp_assert_wid(ID,Mode,[X|Y]):- !, pttp_assert_wid(ID,Mode,X),kb_incr(ID,ID2), pttp_assert_wid(ID2,Mode,Y).
pttp_assert_wid(ID,Mode,(call:-CALL)):-!,pttp_assert_wid(ID,Mode,(call(CALL))).
pttp_assert_wid(_, _Mode,uses_logic(Name)):-!,must(pttp_logic(Name,Data)),!,must(pttp_load_wid(Name,Data)).
pttp_assert_wid(ID,_Mode,kif(YY)):-!, must((numbervars(YY,'$VAR',7567,_),must(pttp_assert_wid(ID,kif,YY)))).
pttp_assert_wid(ID,_Mode,call(CALL)):-!, must((save_wid(ID,call,call(CALL)),unnumbervars(CALL,RCALL),show_failure(why,must(RCALL)))).
%pttp_assert_wid(ID,Mode,(query:-B)):- must(assertz_unumbered((query:-B))),PNF =(query:-B), must( pttp_nnf(PNF,X)),!,must(must(pttp_assert_real_wid(ID,X))).
pttp_assert_wid(ID,pttp,X):- must(( \+ \+ (b_setval('$current_why',wp(ID,X),) must(( pttp1_wid(ID,X,Y), pttp2_wid(ID,Y)))))).
% pttp_assert_wid(ID,Mode,KIF):- must(kif_add(ID,KIF)),!.
pttp_assert_wid(ID,kif,X):- show_failure(why,must(kif_add_boxes1(ID,X))).

pttp_assert_wid(ID,pttp_in,HB):- !,must((save_wid(ID,pttp_in,HB), pttp_assert_real_wid(ID,HB))).
pttp_assert_wid(ID,Mode,(X:-Y)):- !,must((save_wid(ID,Mode,(X:-Y)), pttp_assert_real_wid(ID,(X:-Y)))).
pttp_assert_wid(ID,_Mode,X):-  show_failure(why,must(pttp_assert_real_wid(ID,X))),!.
pttp_assert_wid(ID,_Mode,PNF):-  must( pttp_nnf(PNF,X)),!,must(must(pttp_assert_real_wid(ID,X))).

infer_by(_).

% -- CODEBLOCK
:- was_export(pttp_assert_real_wid/2).
pttp_assert_real_wid(ID,X):-
  must( pttp1_wid(ID,X,Y)),!, must(pttp_assert_int_wid(ID,Y)),!.


% -- CODEBLOCK
:- was_export(is_static_predicate_pttp/2).
:- meta_predicate(is_static_predicate_pttp(0,?)).
is_static_predicate_pttp(M:(Y:-_),Why):-!,is_static_predicate_pttp(M:Y,Why).
is_static_predicate_pttp((Y:-_),Why):-!,is_static_predicate_pttp(Y,Why).
is_static_predicate_pttp(_:Y,file(F)):-!,predicate_property(_:Y,file(F)),not(predicate_property(_:Y,dynamic)).
is_static_predicate_pttp(Y,file(F)):-predicate_property(_:Y,file(F)),not(predicate_property(_:Y,dynamic)).


% -- CODEBLOCK
:- was_export(pttp_assert_int_wid_for_conjuncts/3).
:- meta_predicate(pttp_assert_int_wid_for_conjuncts(+,0,+)).
pttp_assert_int_wid_for_conjuncts(ID,Y,_):- must(pttp_assert_int_wid(ID,Y)).


% -- CODEBLOCK
:- was_export(save_wid/3).
save_wid(IDWhy,Atom,Wff):-must(Atom\=','),to_numbered_ground(wid(IDWhy,Atom,Wff),Assert),show_failure(why,ainz_pttp(Assert)).

to_numbered_ground(I,O):-ground(I)->I=O;(copy_term(I,M),numbervars(M,766,_,[functor_name('$VAR')]),O=M->true;trace_or_throw(to_numbered_ground(I,O))).

% -- CODEBLOCK
clauses_wid(ID,ID:R,F,Y,Ref):-atomic(ID),!,nonvar(ID),clause_asserted(wid(ID:R,F,Y),true,Ref).
clauses_wid(ID,ID,F,Y,Ref):- clause_asserted(wid(ID,F,Y),true,Ref).


% -- CODEBLOCK
:- was_export(retract_if_no_wids/1).
retract_if_no_wids(Y):- \+ wid(_,_,Y) -> retractall_matches(Y) ; true.

% -- CODEBLOCK
:- was_export(is_wid_key/2).
is_wid_key(Other,_):-compound(Other),not(not(is_wid_key2(Other))).
   is_wid_key2(C:N):-number(N),!,(compound(C);atom(C)),!.
   is_wid_key2(+N):-!,nonvar(N),!,is_wid_key2(N).
   is_wid_key2(-N):-!,nonvar(N),!,is_wid_key2(N).
   is_wid_key2(_:N):-!,nonvar(N),!,is_wid_key2(N).
   is_wid_key2(N):-number(N),!.
   is_wid_key2(N):-compound(N),!,N=..[_,A],!,number(A).


% -- CODEBLOCK
erase_safe_pttp(_,Ref):-erase(Ref).

% -- CODEBLOCK
retractall_matches(Y):-unnumbervars(Y,YY),retractall_matches_0(YY).
retractall_matches_0((Y:-B)):-!,pred_subst(is_wid_key,B,_,_,BB),forall(clause(Y,BB,Ref),erase_safe_pttp(clause(Y,BB,Ref),Ref)).
retractall_matches_0(Y):-forall(clause(Y,true,Ref),erase_safe_pttp(clause(Y,true,Ref),Ref)).

% -- CODEBLOCK
:- was_export(retractall_wid/1).
retractall_wid(ID):- 
 forall(clauses_wid(ID,A,B/C,Y,Ref),must(show_failure(why,(erase_safe_pttp(clauses_wid(ID,A,B/C,Y,Ref),Ref),retract_if_no_wids(Y))))),
 forall(clauses_wid(ID,A,B,Y,Ref),must(erase_safe_pttp(clauses_wid(ID,A,B,Y,Ref),Ref))).

% -- CODEBLOCK
:- was_export(listing_wid/1).
:- was_export(listing_wid/0).
listing_wid:- listing_wid(_).
listing_wid(ID):- forall(((no_repeats(RID,clauses_wid(ID,RID,_,_,_)))),write_rid(RID)).

% -- CODEBLOCK
:- was_export(write_rid/1).
write_rid(RID):- 
((nl,write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% '),nl,write('%   '),
   write(RID),nl,
   write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n '),nl)),!,
  forall(((clauses_wid(RID,RID,Atomic,Wff,_),
          Atomic\=pttp_nnf,atomic(Atomic))),
   (write('% '),write(Atomic), write(': '), ansicall(green,portray_clause_0( Wff )),nl)),
  forall(no_repeats(FY,(clauses_wid(RID,RID,NA,FY,_),compound(NA))),
    show_wid_int(FY)).

:- style_check(+singleton).

show_wid_int(FY):-once((reassemble_intified(FY,FYI),renumbervars_a(FYI,FY2),unnumbervars(FY2,FY3), portray_clause_0( FY3 ),nl)).

reassemble_intified(H :- B, OUT ):- 
   H=..HEADL,append(HARGS,[_G, _E, _A, _M, _F, _O, _D],HEADL),
   grab_body(B,BOD), FHARGS=..HARGS,
    OUT = (FHARGS :- BOD),!.
reassemble_intified(H, FHARGS ):-compound(H),H=..HEADL,append(HARGS,[_G, _E, _A, _M, _F, _O, _D],HEADL), FHARGS=..HARGS,!.
reassemble_intified(OUT,OUT).

grab_body(call_proof(_,A),A):-!.
grab_body((A,B),AB):-grab_body(A,AA),grab_body(B,BB),conjoin_pttp(AA,BB,AB).
grab_body(_,true).

:- was_export(portray_clause_0/1).
portray_clause_0(  Cmp ):- compound(Cmp),call(=,Cmp,(AA:-BB)),!, renumbervars_prev((AA:-BB) ,(A:-B) ),call(=,NV,(A:-B)),
  current_output(Out),portray_clause(Out,NV,[numbervars(true)]).
portray_clause_0( (A;B)):- writeq((A;B)),nl,!.
portray_clause_0(  AB  ):- current_output(Out),portray_clause(Out,(AB),[numbervars(true)]).



% -- CODEBLOCK
:- was_export(clear_pttp/0).
clear_pttp:- 
  eraseall(int_query,_),eraseall(int_not_query,_),
  forall(wid(ID,_/_,_),retractall(wid(ID,_,_))),
  forall(was_pttp_functor(internal,F,A),(abolish(F,A),dynamic(F/A))).

%eraseall(M:F,A):-!,functor(C,F,A),forall(clause(M:C,_,X),erase(X)).
%eraseall(F,A):-current_predicate(F/A),functor(C,F,A),forall(clause(C,_,X),erase(X)).                                                            

:- kb_shared(pttp_test/2).
:- kb_shared(pttp_logic/2).


% -- CODEBLOCK
:- dynamic(pttp_test_took/3).

:- export(do_pttp_test_maybe/1).
:- kb_shared(pttp_test_fails_is_ok/1).
:- export(do_pttp_test_maybe/2).

do_pttp_test_maybe(TestName):- forall(pttp_test(TestName,Data),do_pttp_test_maybe(TestName,Data)),listing(pttp_test_took).
do_pttp_test_maybe(TestName,_) :- pttp_test_fails_is_ok(TestName),!.
do_pttp_test_maybe(TestName,Data) :- do_pttp_test(TestName,Data).

:- was_export(do_pttp_test/1).
do_pttp_test(TestName):- forall(pttp_test(TestName,Data),do_pttp_test(TestName,Data)),listing(pttp_test_took).
:- was_export(do_pttp_test/2).
do_pttp_test(TestName,Data) :-   
           call_cleanup((     
             catch((      
               clear_pttp,
                must_det_l((
                          dmsg(do_pttp_test(TestName)),
                          retractall_wid(TestName),
                           eraseall(int_query,_),eraseall(int_not_firstOrder,_),eraseall(int_firstOrder,_),                                                           
                               pttp_tell_wid(TestName:0,Data), 
                               once((ignore(call_print_tf(pttp_test_prove(TestName,query))))),
                               sleep(1)))),E,dmsg(error(TestName:E)))),retractall_wid(TestName)).
                              

% -- CODEBLOCK
:- was_export(pttp_test_prove/2).
pttp_test_prove(TestName,_):- pttp_test_query(TestName,Other),!,call30timed(TestName,Other).
pttp_test_prove(TestName,A):- call30timed(TestName,pttp_prove(A)).

:- meta_predicate call30timed(*,0).
call30timed(TestName,CALL):-  
   statistics(cputime, D),
        (   CALL  % catch(call_with_time_limit(CALL,30),time_limit_exceeded,(wdmsg(error_time_limit_exceeded(CALL)),fail))
        ->  B=success
        ;   B=failure
        ),
        statistics(cputime, C),
        F is C-D,
        ainz_pttp(pttp_test_took(TestName,B,F)),!,
        B=success.


:- was_export(call_print_tf/1).
:- meta_predicate(call_print_tf(0)).
call_print_tf(G):-(G *-> dmsg(succceeded(G)) ; (dmsg(warning(error(failed_finally(G)))),sleep(5))).

:- was_export(do_pttp_tests/0).
do_pttp_tests :- do_pttp_test_maybe(_), 
   forall(pttp_test_took(Test, failure, _Time),gripe_pttp_failure(Test)).

gripe_pttp_failure(Test):- pttp_test_fails_is_ok(Test),!.
gripe_pttp_failure(Test):- dmsg(gripe_pttp_failure(Test)),!.
gripe_pttp_failure(Test):- ignore(pttp_test_took(Test, failure, Time)),trace_or_throw(pttp_test_took(Test, failure, Time)).

:- kb_shared(baseKB:sanity_test/0).
% baseKB:sanity_test :- do_pttp_tests.


:- was_export(isNegOf/2).
isNegOf(N1,N):-number(N),!,N1 is -N.
isNegOf(N,-N):-is_ftNonvar(N),!.
isNegOf(-N,N):-is_ftNonvar(N),!.
isNegOf(N1,N):-dtrace(not(isNegOf(N1,N))),isNegOf(N,N1).



:- kb_shared(was_pttp_functor/3).



% -- CODEBLOCK


was_pttp_functor(internal, query,7).


% -- CODEBLOCK
int_listing_wid0:-
  forall(was_pttp_functor(external,F,A),catch(prolog_list:listing(F/A),_,fail)),
  forall(was_pttp_functor(internal,F,A),catch(prolog_list:listing(F/A),_,fail)),!.

int_listing_wid:-
  forall(was_pttp_functor(external,F,A),(functor(P,F,A),forall(clause(P,B),portray_clause_0((P:-B))))),
  forall(was_pttp_functor(internal,F,A),(functor(P,F,A),forall(clause(P,B),portray_clause_0((P:-B))))).

:- thread_local(is_query_functor/1).
must_pttp_id(ID):- must(nb_current('$current_why',wp(ID,_))).
is_query_lit(Q):- functor(Q,F,_),atom_concat('quer',_,F).

get_int_query(Int_query):- is_query_functor(X),!, atom_concat('int_',X,Int_query).
get_int_query(int_query).

:- was_export(pttp_query/1).
pttp_query(X) :- must_pttp_id(ID),pttp_query_wid(ID,X).
:- was_export(pttp_query_wid/2).
pttp_query_wid(ID, Y):- dtrace,pttp_tell_wid(ID,(query:-Y)), pttp_test_prove(ID,query),!.

/*
 A thread local safe way to do it
 pttp_query_wid(ID, Y):- term_variables(Y,Vars),gensym(query_pttp,Q),Z=..[Q|Vars],
    atom_concat('int_',Q,Int_query),
    locally(is_query_functor(Q), 
           (pttp_assert_int_wid(ID,((Z:-Y))), pttp_test_prove(ID,Int_query))).
*/

% ===============================================


renumbervars_a(In,Out):-renumbervars_prev(In,Out),!.

:- was_export(assertz_unumbered/1).

assertz_unumbered(B,_):-assertz_unumbered(B).

assertz_unumbered(B):-is_ftVar(B),trace_or_throw(var_assertz_unumbered(B)).
assertz_unumbered(true):-!.
assertz_unumbered(_:true):-!.
%assertz_unumbered((H:- (infer_by(_),B))):- !,assertz_unumbered((H:-B)).
%assertz_unumbered((H:- (infer_by(_)))):- !,assertz_unumbered((H)).
assertz_unumbered(:-(B)):- !, show_call(why,must(B)),!.
assertz_unumbered(B):- t_l:current_pttp_db_oper(OP),!, must((unnumbervars(B,BB),show_failure(why,call(OP,BB)))).
assertz_unumbered(B):-must((unnumbervars(B,BB),show_failure(why,ainz_pttp(BB)))).

:- was_export(add_functor/2).
add_functor(Ext,F/A):- must(( export(F/A),ainz_pttp(was_pttp_functor(Ext,F,A)))).


pttp_tell(Wff):- why_to_id(pttp_tell,Wff,Why),pttp_assert_int_wid(Why,Wff).


% ===============================================================================
% pttp_assert_int
% ===============================================================================
:- was_export(pttp_assert_int/1).
pttp_assert_int(Y):- must_pttp_id(ID),pttp_assert_int_wid(ID,Y).
:- was_export(pttp_assert_int_wid/2).
:- meta_predicate(pttp_assert_int_wid(+,+)).


pttp_assert_int_wid(ID,Var):-is_ftVar(Var),trace_or_throw(var_pttp_assert_int_wid(ID,Var)).
pttp_assert_int_wid(_ID,true):-!.
pttp_assert_int_wid(ID,[H|B]):-!,pttp_assert_int_wid(ID,H),!,pttp_assert_int_wid(ID,B),!.
pttp_assert_int_wid(ID,(H,B)):-!,pttp_assert_int_wid(ID,H),!,pttp_assert_int_wid(ID,B),!.
pttp_assert_int_wid(ID,_:L):-!, pttp_assert_int_wid(ID,L).
pttp_assert_int_wid(ID,YB):- must((get_functor(YB,F,A),renumbervars_a(YB,Y),pttp_assert_int_wid04(ID,Y,F,A),assertz_unumbered(Y))).


:- meta_predicate pttp_assert_int_wid04(*,0,*,*).

pttp_assert_int_wid04(_,Y,_,_):- is_static_predicate_pttp(Y,Why),must( dmsg(error(warn(is_static_predicate_pttp(Y,Why))))),!.
pttp_assert_int_wid04(_,_,F,A):- was_pttp_functor(external,F,A),!.
pttp_assert_int_wid04(_,Y,F,A):- not(internal_functor(F)),add_functor(external,F/A),assertz_unumbered(Y),!.
pttp_assert_int_wid04(ID,Y,F,A):- show_success(why,wid(ID,F/A,Y)),!.
%pttp_assert_int_wid04(ID,Y,F,A):- fail, once((must((must((renumbervars_a(Y,BB),nonvar(BB))),pred_subst(is_wid_key,BB,_,_,YCheck),nonvar(YCheck),BB \=@= YCheck)))),wid(_,F/A,YCheck),!,ainz_pttp(wid(ID,F/A,Y)),!.
pttp_assert_int_wid04(ID,Y,F,A):- wid(_,_,Y),!,ainz_pttp(wid(ID,F/A,Y)),!.
pttp_assert_int_wid04(ID,Y,F,A):- ainz_pttp(wid(ID,F/A,Y)),add_functor(internal,F/A),!,assertz_unumbered(Y),!.
/*
pttp_assert_int_wid04(ID,Y,F,A):- 
   is_static_predicate_pttp(Y,Why)-> (dtrace,wdmsg(warn(error(is_static_predicate_pttp(Y,Why))))); 
    must(show_failure(why,assertz_unumbered(Y)),
    must((not(internal_functor(F))-> add_functor(external,F/A); (ainz_pttp(wid(ID,F/A,Y)),add_functor(internal,F/A)))))
*/
                                     
:- ensure_loaded(dbase_i_mpred_pttp_statics).
:- ensure_loaded(dbase_i_mpred_pttp_precompiled).
:- ensure_loaded(dbase_i_mpred_pttp_testing).

:- ensure_loaded(dbase_i_mpred_pttp_compile_stickel_orig).

:- fixup_exports.

:- if_startup_script(do_pttp_tests).



