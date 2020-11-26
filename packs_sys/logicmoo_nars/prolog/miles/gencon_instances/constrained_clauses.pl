% MODULE constrained_clauses EXPORTS
:- module( constrained_clauses,
           [ learn_constrained/0
           ]).

% METAPREDICATES
:- meta_predicate ccl_newp(+,+,:,:,:,:,:,:,:,:,:,:,:,:).


% IMPORTS
:- use_module(home(kb),
                   [get_example/3,
                    get_clause/5, 
                    get_evaluation/2,     
                    delete_clause/1,
                    delete_example/1,
                    store_clauses/2,
                    store_clause/4,
                    store_ex/3,
                    delete_all/1,
                    known/6]).
:- use_module(home(gencon),
              [gilppi/12,gilppi/14]).
:- use_module(home(argument_types),
              [types_of/3,type_restriction/2]). 
:- use_module(home(clause_heads),
              [heads/1]).                    
:- use_module(home(show_utils),
              [show_kb/0,write_list/1]).                     
:- use_module(home(evaluation),
              [eval_examples/0,complete_chk/0,correct_chk/0,
               covered_pos_examples/1,clear_evaluation/0,
               covered_neg_examples/1,fp_hyp/1,change_evaluated/1]).
:- use_module(home(lgg),
                   [set_lgg/2]).
:- use_module(home(div_utils),
                   [best/2,       
                    remove_v/3, 
                    mysetof/3,
                    make_unique/2,
                    body2list/2]).
:- use_module(home(var_utils),
                   [only_vars/2,clause_terms/2]).
:- use_module(home(tdref_it),
                   [refinement_add_body_literal/3]).
:- use_module(home(newpred),
                  [specialize_with_newpred/7]).
:- use_module(home(interpreter),
                  [prooftrees/3]).
:- use_module_if_exists(library(basics),
                      [member/2]).
:- use_module_if_exists(library(strings),
                      [gensym/2]).
:- use_module_if_exists(library(sets),
                     [subset/2]).
:- use_module_if_exists(library(subsumes),
                     [variant/2]).
% METAPREDICATES
% none


%***********************************************************************
%*	
%* module: constrained_clauses.pl
%*									
%* author: I.Stahl              date:8/93	
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

learn_constrained:-
   gilppi(ccinitialize,ccstop_c,ccquality_c,ccupdate,ccselect,
          ccadd,ccfilter,ccone_of,ccspec,ccgen,ccl_newp,ccoutput).


ccinitialize([(HL:[S1,S2]:HL,active)]):-
   heads(HL),
   store_clauses(HL,hypo),
   eval_examples,
   findall((ID,H,B,CL,E),(get_clause(ID,H,B,CL,hypo),
                          get_evaluation(ID,E), delete_clause(ID)),S1),
   findall((ID1,M1,T1),(retract(prooftrees(ID1,M1,T1))),S2).

ccinitialize_newp([(PS:[S1,S2]:PS,active)]):-
   heads(HL),
   store_clauses(HL,hypo),
   eval_examples,
   findall((H0:-B0),get_clause(_,H0,B0,_,hypo),PS),
   findall((ID,H,B,CL,E),(get_clause(ID,H,B,CL,hypo),
                          get_evaluation(ID,E), delete_clause(ID)),S1),
   findall((ID1,M1,T1),(retract(prooftrees(ID1,M1,T1))),S2).

ccstop_c([_]).


ccquality_c(_:[SC,SP]:_):-
   sclauses(SC),
   sprooftrees(SP),
   (  (complete_chk,correct_chk) ->
      true
   ;  fail
   ).

ccupdate(L,L).

sclauses([]).
sclauses([(ID,H,B,CL,E)|R]):-
   sclauses(R),
   assertz(kb:known(ID,H,B,CL,hypo,E)).

sprooftrees([]):- change_evaluated(yes).
sprooftrees([(ID,M,T)|R]):-
   sprooftrees(R),
   assertz(interpreter:prooftrees(ID,M,T)).   


ccselect(Partial_Sols,PS,active,Partial_Sols1):-
   select_active(Partial_Sols,PS,Partial_Sols1).
ccselect(Partial_Sols,PS,passive,Partial_Sols):-
   select_passive(Partial_Sols,PS).

select_active([(PS,active)|R],PS,[(PS,passive)|R]).
select_active([P|R],PS,[P|R1]):-
   select_active(R,PS,R1).

select_passive(Partial_Sols,PS):-
   candidates(Partial_Sols,[],Partial_Sols1),
   best(Partial_Sols1,_-PS).

candidates([],PSS,PSS).
candidates([(PS:[SC,SP]:Hist,_)|R],PSS,PSS2):-
   candidates(R,PSS,PSS1),
   sclauses(SC), sprooftrees(SP),
   (   complete_chk ->
       covered_neg_examples(N), length(N,NN),
       ccins(NN-(PS:[SC,SP]:Hist), PSS1,PSS2)
   ;   PSS1 = PSS2
   ),
   clear_evaluation,
   findall(ID,(get_clause(ID,_,_,_,hypo),delete_clause(ID)),_).

ccins(N-PS,[N1-PS1|R],[N1-PS1|R1]):-
   N > N1,!,
   ccins(N-PS,R,R1).
ccins(X,L,[X|L]).

ccadd(Partial_Sols,PSL,Partial_Sols1):-
   append(Partial_Sols,PSL,Partial_Sols1).

ccfilter(Partial_Sols,Partial_Sols1):-
   ccfilter(Partial_Sols,[],Partial_Sols1).

ccfilter([],Partial_Sols,Partial_Sols).
ccfilter([X|R],Partial_Sols,Partial_Sols2):-
   ccfilter1(X,Partial_Sols,Partial_Sols1),
   ccfilter(R,Partial_Sols1,Partial_Sols2).

ccfilter1(X,[],[X]).
ccfilter1((PS:DB:Hist,M),[(PS1:DB1:Hist1,M1)|R],[(PS1:DB1:Hist1,M1)|R1]):-
   (   (M = M1,clause_variants(PS,PS1)) ->
       R1 = R
   ;   ccfilter1((PS:DB:Hist,M),R,R1)
   ).

ccone_of(_,M):-
   (   complete_chk ->
       M = spec
   ;   M = gen
   ).

ccspec(_:_:Hist,PSL):-
   ccspec1(Hist,PSL),
   write_l(PSL),
   findall(ID,get_clause(ID,_,_,_,hypo),IDL),
   delete_all(IDL).

ccspec1(Hist,PSL):-
   covered_neg_examples(NIDs),
   fp_hyp(OR),
   best(OR,I:_),
   get_clause(I,H,B,_,hypo),
   get_evaluation(I,evaluation(_,_,Pos,_,_,_,_,_,_)),
   Pos \== [],
   clause_terms((H:-B),Terms),
   types_of(Terms,(H:-B),TTerms),
   refinement_add_body_literal((H:-B),TTerms,CL),
   length(CL,CLl),nl,write('no refs: '),write(CLl),nl,nl,
   check_refinements(CL,NIDs,I,Hist,(H:-B),PSL).

check_refinements(CL,NIDs,I,Hist,C,PSL):-
   delete_clause(I),
   clear_evaluation,
   check_refinements(CL,NIDs,I,Hist,PSL),
   store_clause(C,_,hypo,I),!.

check_refinements([],_,_,_,[]).
check_refinements([C|R],NID,I,Hist,PSL2):-
   (   (constrained(C), \+(clause_in(C,Hist,_))) ->
       store_clause(C,_,hypo,I),
       eval_examples,
       get_evaluation(I,evaluation(_,_,Pos,_,_,_,_,_,_)),
       covered_neg_examples(NID1),
       (   (genuine_subset(NID1,NID), Pos \== []) ->
           findall((H:-B),get_clause(ID,H,B,_,hypo),PS),
           findall((ID,H,B,CL,E),(get_clause(ID,H,B,CL,hypo),
                                  get_evaluation(ID,E)),S1),
           findall((ID1,M1,T1),retract(prooftrees(ID1,M1,T1)),S2),
           PSL2 = [(PS:[S1,S2]:[C|Hist],active)|PSL1]
       ;   PSL2 = PSL1,clear_evaluation
       ),
       delete_clause(I)
    ;  PSL2 = PSL1
    ),
   check_refinements(R,NID,I,Hist,PSL1).


constrained((H:-B)):-
   only_vars(H,HV),
   only_vars(B,BV),
   remove_v(HV,BV,[]).

clause_in((H:-true),[(H1:-B1)|R],R2):-
   !, 
   (   (variant(H,H1), B1 == true) ->
       R2 = R
   ;   clause_in((H:-true),R,R1),
       R2 = [(H1:-B1)|R1]
   ).
clause_in((H:-B),[(H1:-B1)|R],R2):-
   body2list(B,BL),
   body2list(B1,B1L),!,
   (   (variant(H,H1),length(BL,N),length(B1L,N)) ->
       H = H1,
       (   c_in(BL,B1L) ->
           R2 = R
       ;   clause_in((H:-B),R,R1), R2 = [(H1:-B1)|R1]
       )
   ;   clause_in((H:-B),R,R1), R2 = [(H1:-B1)|R1]
   ).

c_in([],[]).
c_in([L|R],B):-
   remove_v([L],B,B1),!,
   c_in(R,B1).

clause_variants([],[]).
clause_variants([C|R],CL):-
   clause_in(C,CL,CL1),
   clause_variants(R,CL1).

genuine_subset(L1,L2):-
   length(L1,L1n),
   length(L2,L2n),
   L1n < L2n,
   subset(L1,L2).

ccgen(_:_:Hist,[(PS:[S1,S2]:Hist,active)]):-
   covered_pos_examples(Cov),
   findall(ID1:E1,(member(ID1,Cov),get_example(ID1,E1,_)),Cov1),
   get_clause(ID,H,B,_,hypo),
   mysetof(IDE:H,get_example(IDE,H,+),PH),
   remove_v(Cov1,PH,P1),
   P1 \== [],
   ccgen2(ID,H,B,PH,B1),
   store_clause((H:-B1),_,hypo,_),
   eval_examples,
   findall((ID2,H2,B2,CL2,E2),(get_clause(ID2,H2,B2,CL2,hypo),
                          get_evaluation(ID2,E2)),S1),
   findall((ID3,M3,T3),(retract(prooftrees(ID3,M3,T3))),S2),
   findall((H4:-B4),(get_clause(ID4,H4,B4,_,hypo),
                     delete_clause(ID4)),PS),
   write_l([(PS:[S1,S2]:Hist,active)]).

ccgen2(ID,H,B,PH,B1):-
   delete_clause(ID),
   body2list(B,BL),
   ccgen3(BL,ID,H,PH,BL1),
   (   BL1 == [] ->
       B1 = true
   ;   body2list(B1,BL1)
   ),
   store_clause((H:-B),_,hypo,ID).

ccgen3([],_,_,_,[]).
ccgen3([L:M|R],ID,H,PH,B):-
   ccgen3(R,ID,H,PH,B1),
   store_clause((H:-L),_,hypo,ID),
   eval_examples,
   get_evaluation(ID,evaluation(_,_,Pos,_,_,_,_,_,_)),
   delete_clause(ID),
   (   remove_v(Pos,PH,[])->
       B = [L:M|B1]
   ;   B = B1
   ).
   

ccl_newp(_:_:Hist,[(PS:[S1,S2]:Hist,active)], Initialize, Stop_C, Quality_C, Update,
          Select, Add, Filter, One_of, Spec, Gen, L_newp,Output):-
   fp_hyp(OR),
   best(OR,I:_),
   get_clause(I,H,B,_,hypo),
   get_evaluation(I,evaluation(_,_,Pos,_,Neg,_,_,_,_)),
   specialize_with_newpred((H:-B),Pos,Neg,NC,NPos,NNeg,NType),
   delete_clause(I),
   delete_old_ex(Elist),
   store_newp_ex(NPos,NNeg,IDL0),
   make_unique(IDL0,IDL),
   assertz(NType),
   store_clause(NC,_,hypo,I),
   ccinitialize_newp(PSL),
   gilppi(PSL, [], Initialize, Stop_C, Quality_C, Update, Select, 
          Add, Filter, One_of, Spec, Gen, L_newp,Output),
   delete_all(IDL),
   store_old_ex(Elist),
   findall(ID0,(get_clause(ID0,H0,B0,_,constrained_clause),
               delete_clause(ID0),
               store_clause((H0:-B0),_,hypo,ID0)),_),
   eval_examples,  
   findall((ID3,H3,B3,CL3,E3),(get_clause(ID3,H3,B3,CL3,hypo),
                          get_evaluation(ID3,E3)),S1),
   findall((ID1,M1,T1),(retract(prooftrees(ID1,M1,T1))),S2),
   findall((H2:-B2),(get_clause(ID2,H2,B2,_,hypo),
                     delete_clause(ID2)),PS).

delete_old_ex([ex(ID,F,M)|R]):-
   get_example(ID,F,M),
   delete_example(ID),
   delete_old_ex(R).
delete_old_ex([]).

store_old_ex([]).
store_old_ex([ex(ID,F,M)|R]):-
   store_ex(F,M,ID),
   store_old_ex(R).

store_newp_ex([],[],[]).
store_newp_ex([],[F|R],[ID|R1]):-
   store_ex(F,'-',ID),
   store_newp_ex([],R,R1).
store_newp_ex([F|R],L,[ID|R1]):-
   store_ex(F,'+',ID),
   store_newp_ex(R,L,R1).

ccoutput([CL:_:_]):- 
   findall(ID,get_clause(ID,_,_,_,hypo),IDL),
   delete_all(IDL),  
   store_clauses(CL,constrained_clause), 
   nl,nl,write('gilppi completed..........'),nl,nl,
   show_kb.


write_l([(CL:_:_,_)|R]):- write_list(CL),nl,write_l(R).
write_l([]).