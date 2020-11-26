% MODULE rul EXPORTS
:- module( rul,
           [ learn_rul/0
           ]).


% IMPORTS
:- use_module(home(kb),
                   [ get_example/3,get_clause/5, get_evaluation/2,store_ex/3,
                     delete_clause/1,store_clauses/2,store_clauses/3,store_clause/4,
                     delete_all/1]).
:- use_module(home(argument_types),
              [type_sub/2,type_equal/4,replace_t/4]). 
:- use_module(home(gencon),
              [gilppi/12]).                    
:- use_module(home(show_utils),
              [show_kb/0]).                     
:- use_module(home(evaluation),
              [eval_examples/0,covered_pos_examples/1]).
:- use_module(home(lgg),
                   [set_lgg/2]).
:- use_module(home(div_utils),
                   [different_predicates/2,
                    remove_v/3, 
                    mysetof/3]).
:- use_module(home(var_utils),
                   [only_vars/2]).
:- use_module(home(td_basic),
                   [append_body/3]).
:- use_module(home(newpred),
                  [is_newpred/1]).
:- use_module_if_exists(library(basics),
                      [member/2]).
:- use_module_if_exists(library(strings),
                      [gensym/2]).

% METAPREDICATES
% none


%***********************************************************************
%*	
%* module: rul.pl
%*									
%* author: I.Stahl              date:7/93	
%*									
%* changed:								
%*									
%*									
%* description: instantiation of gilppi for RUL-programs
%*		
%* 
%* see also:								
%*									
%***********************************************************************

learn_rul:-
   gilppi(initialize,stop_c, quality_c, update, select, add, filter,
          one_of, spec, gen, l_newp,output).




initialize([HL:active]):-
   mysetof(E,I^get_example(I,E,'+'),Elist),
   different_predicates(Elist,Elist1),
   initialize1(Elist1,HL).

initialize1([],[]).
initialize1([[E|ER]|R],HL):-
   initialize1(R,HL0),
   functor(E,T,_),
   mysetof(A,M^(member(M,[E|ER]),arg(1,M,A)),Alist),
   different_predicates(Alist,Alist1),
   initialize2(Alist1,T,HL1),
   append(HL1,HL0,HL).

initialize2([],_,[]).
initialize2([A|R],T,[(H:-true)|R1]):-
   set_lgg(A,A1),
   H =.. [T,A1],
   initialize2(R,T,R1).


stop_c([_]).

quality_c([]).
quality_c([(H:-B)|R]):-
   only_vars(H,HV),
   only_vars(B,BV),
   remove_v(BV,HV,[]),
   quality_c(R).

update(L,L).

select(Partial_Sols,PS,active,Partial_Sols1):-
   select_active(Partial_Sols,PS,Partial_Sols1).
select(Partial_Sols,PS,passive,Partial_Sols):-
   select_passive(Partial_Sols,PS).

select_active([PS:active|R],PS,[PS:passive|R]).
select_active([P|R],PS,[P|R1]):-
   select_active(R,PS,R1).

select_passive([PS:_],PS):- !.
select_passive([PS:_|R],PS2):-
   select_passive(R,PS1),
   most_specific(PS,PS1,PS2).

most_specific(PS,PS1,PS2):-
   (   more_specific(PS,PS1) ->
       PS2 = PS
   ;   PS2 = PS1
   ).

more_specific(Spec,Gen):-
   copy_term((Spec,Gen),(Spec0,Gen0)),
   normalize(Spec0,Spec1),
   normalize(Gen0,Gen1),
   rename_types(Gen1,Spec1,Spec2,Tlist),
   store_clauses(Spec2,type,IDS),
   store_clauses(Gen1,type,IDG),
   append(IDS,IDG,IDA),
   (   more_spec(Tlist) ->
       delete_all(IDA)
   ;   delete_all(IDA),!,
       fail
   ).


more_spec([]).
more_spec([Spec:Gen|R]):-
   type_sub(Gen,Spec),
   more_spec(R).

normalize([],[]).
normalize([(H:-B)|R],[(H:-B1)|R1]):-
   normalize(R,R1),
   only_vars(H,HV),
   only_vars(B,BV),
   remove_v(BV,HV,RV),
   normalize(RV,B,B1).

normalize([V],true,all(V)):- !.
normalize([],B,B).
normalize([V|R],B,(all(V),B1)):-
   normalize(R,B,B1).

rename_types(Gen,Spec,Spec1,Tlist):-
   mysetof(Pred,H^B^(member((H:-B),Gen),functor(H,Pred,1)),Plist),
   rename_t(Plist,Tlist),
   transform_t(Spec,Spec1,Tlist).

rename_t([],[]).
rename_t([P|R],[P1:P|R1]):-
   rename_t(R,R1),
   gensym(P,P1).

transform_t([],[],_).
transform_t([(H:-B)|R],[(H1:-B1)|R1],Tlist):-
   transform_t(R,R1,Tlist),
   transform_t1((H,B),Tlist,(H1,B1)).

transform_t1((A,B),Tlist,(A1,B1)):-
   !,transform_t1(A,Tlist,A1),
   transform_t1(B,Tlist,B1).
transform_t1(true,_,true):- !.
transform_t1(A,Tlist,A1):-
   A =.. [Pred,Arg],
   (   member(Pred1:Pred,Tlist) ->
       A1 =.. [Pred1,Arg]
   ;   A1 = A
   ).




add(Partial_Sols,PSL,Partial_Sols1):-
   append(PSL,Partial_Sols,Partial_Sols1).

filter([],[]).
filter([CL|R],[CL1|R2]):-
   filter(R,CL,R1,CL1),
   filter(R1,R2).
filter([],CL,[],CL).
filter([CL1:A|R],CL:B,R1,CL2):-
   (   more_specific(CL,CL1) ->
       (   (B == active ; A == passive) ->
           filter(R,CL:B,R1,CL2)
       ;   filter(R,CL1:A,R1,CL2)
       )
   ;   (   more_specific(CL1,CL) ->
           (   (A == active; B == passive) ->
               filter(R,CL1:A,R1,CL2)
           ;   filter(R,CL:B,R1,CL2)
           )
       ;   R1 = [CL1:A|R0],
           filter(R,CL:B,R0,CL2)
       )
    ).



one_of(PS,M):-
   store_clauses(PS,hypo,IDL),
   eval_examples,
   mysetof(ID:P,get_example(ID,P,+),Pos),
   rem_other_covered(IDL,Pos,Pos1),
   delete_all(IDL),
   (   Pos1 = [] ->
       M = spec
   ;   M = gen
   ).


spec(PS,PSL):-
   store_clauses(PS,hypo,IDL),
   eval_examples,
   mysetof(P,ID0^H0^B0^CL0^L0^(get_clause(ID0,H0,B0,CL0,L0),functor(H0,P,1)),
           Predlist),
   spec(IDL,Predlist,PSL).

spec([],_,[]):- 
   mysetof((H1:-B1),ID1^CL^(get_clause(ID1,H1,B1,CL,hypo),
                            delete_clause(ID1)),_).
spec([ID|R],Preds,PSL):-
   get_clause(ID,H,B,_,_),
   (   specable(H,B,RV) ->
       get_evaluation(ID,evaluation(_,_,Pos,_,_,_,_,_,_)),
       remove_other_covered(H,ID,Pos,Pos1),
       delete_clause(ID),
       spec_c(RV,H,B,Preds,ID,Pos1,PSL0),
       (   PSL0 \== [] ->
           PSL0 = PSL,
           mysetof((H1:-B1),ID1^CL^(get_clause(ID1,H1,B1,CL,hypo),
                                    delete_clause(ID1)),_)
       ;   store_clause((H:-B),_,hypo,ID),
           spec(R,Preds,PSL)
       )
   ;   spec(R,Preds,PSL)
   ).


specable(H,B,RV):-
   only_vars(H,HV),
   only_vars(B,BV),
   remove_v(BV,HV,RV),
   RV \== [],!.

remove_other_covered(H,ID,Pos,Pos1):-
   functor(H,F,N),functor(H1,F,N),
   mysetof(ID0,H1^B1^CL1^get_clause(ID0,H1,B1,CL1,hypo),IDL0),
   remove_v([ID],IDL0,IDL),
   rem_other_covered(IDL,Pos,Pos1),!.

rem_other_covered([],Pos,Pos).
rem_other_covered([ID|R],Pos,Pos2):-
   rem_other_covered(R,Pos,Pos1),
   get_evaluation(ID,evaluation(_,_,P,_,_,_,_,_,_)),
   remove_v(P,Pos1,Pos2).


spec_c([],_,_,_,_,_,[]).
spec_c([V|R],H,B,Predlist,ID,Pos,PSL):-
   spec_c(R,H,B,Predlist,ID,Pos,PSL0),
   spec_c1(Predlist,V,H,B,ID,Pos,PSL1),
   append(PSL0,PSL1,PSL).

spec_c1([],_,_,_,_,_,[]).
spec_c1([Pred|R],V,H,B,ID,Pos,PSL):-
   copy_term((V,H,B),(V1,H1,B1)),
   Lit =.. [Pred,V1],
   append_body((H1:-B1),Lit,C),
   store_clause(C,_,hypo,ID),
   eval_examples,
   get_evaluation(ID,evaluation(_,_,Pos1,_,_,_,_,_,_)),
   delete_clause(ID),
   (   remove_v(Pos1,Pos,[]) ->
       mysetof((H2:-B2),ID2^CL^get_clause(ID2,H2,B2,CL,hypo),RestPS),
       PSL = [[C|RestPS]:active|PSL0]
   ;   PSL = PSL0
   ),
   spec_c1(R,V,H,B,ID,Pos,PSL0).



gen(PS,[[(H:-true)|RestPS]:active]):-
   store_clauses(PS,hypo),
   eval_examples,
   covered_pos_examples(Cov),
   get_clause(ID,H,_,_,hypo),
   mysetof(IDE,H^(get_example(IDE,H,+)),PH),
   remove_v(Cov,PH,P1),
   P1 \== [],
   delete_clause(ID),
   mysetof((H2:-B2),ID2^CL2^(get_clause(ID2,H2,B2,CL2,hypo),
                             delete_clause(ID2)),RestPS).
   





l_newp(PS,[Clist1:active],_,_,_,_,_,_,_,_,_,_,_,_):-
   store_clauses(PS,hypo),
   eval_examples,
   get_all_clauses(Clist),
   correct_with_newp(Clist,Clist1).

get_all_clauses([(H:-B):Pos:RV|R]):-
   get_clause(ID,H,B,_,hypo),
   get_evaluation(ID,evaluation(_,_,Pos,_,_,_,_,_,_)),
   only_vars(H,HV), only_vars(B,BV),
   remove_v(BV,HV,RV),
   delete_clause(ID),
   get_all_clauses(R).
get_all_clauses([]).

correct_with_newp([],[]).
correct_with_newp([(H:-B):_:[]|R],[(H:-B)|R1]):-
   !,correct_with_newp(R,R1).
correct_with_newp([(H:-B):Pos:RV|R],R2):-
   correct_with_newp(R,R1),
   c_with_newp(RV,B,B1,Newps),
   instances(Newps,Pos,H,Elist),
   initialize1(Elist,HL),
   append([(H:-B1)|R1],HL,R2).

c_with_newp([],B,B,[]).
c_with_newp([V],true,New,[New]):-
   !,gensym(newp,Newp),
   New =.. [Newp,V].
c_with_newp([V|R],B,(New,B1),[New|R1]):-
   c_with_newp(R,B,B1,R1),
   gensym(newp,Newp),
   New =.. [Newp,V].

instances([],_,_,[]).
instances([New|R],Pos,H,[NewE|R1]):-
   instances(R,Pos,H,R1),
   mysetof(New,I^J^H^(member(I:H,Pos),store_ex(New,+,J)),NewE).

output([CL]):- 
   mysetof(PN,H^B^R^(member((H:-B),CL),H =.. [PN|R], is_newpred(PN)),Newpredlist),
   minimize_output(Newpredlist,CL,CL1),
   store_clauses(CL1,rul), show_kb.

minimize_output([],CL,CL).
minimize_output([P|R],CL,CL2):-
   findall(P1,(member(P1,R), type_equal(P,P1,[P:P1],CL)),P1L),
   replace_t(CL,P1L,P,CL1),
   findall(I,(member(NP,P1L),get_example(I,NPP,_),NPP =.. [NP|_]),IDL),
   delete_all(IDL),
   remove_v(P1L,R,R1),
   minimize_output(R1,CL1,CL2).
