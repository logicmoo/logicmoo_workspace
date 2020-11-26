% MODULE evaluation EXPORTS

:- module( evaluation,
	[ covered_pos_examples/1,
          covered_neg_examples/1,
          all_covered_examples/1,
          
	  complexity/2,
          clear_evaluation/0,
          evaluated/1,
          change_evaluated/1,

          eval_examples/0,             % Compute complete evaluation for all examples
                                       % AND clauses in kb
          eval_pos_examples/1,         % Compute evaluation for all pos examples in kb
	  complete_chk/0,              % check completeness, all pos examples covered?
	  correct_chk/0,               % check correctness, no neg examples covered?
 	  fp/1,
          fpo/1,
          fp_hyp/1, 
          ip/1,
          herbrand_base_ff/1,
          ivonTunterE/1,ivonBundE/1, 
          code_length/2,
          encoding_length_examples/1,
          encoding_length_clause/2]).

%IMPORTS
:- use_module(home(div_utils),
              [make_unique/2,insert_unique/4,sort_by_length/3,mysetof/3,
               remove/3,append_all/2,sum/2,identical_make_unique/2,best/2,
               remove_variant/3,make_unique/2,
               fak/2,fak1/3,nueberk/3,log2/2,log2nueberk/3,sum_of_logs/3]).
:- use_module(home(environment),
              [ask_for/1]).
:- use_module(home(var_utils),
              [term_size/2,vars/2,skolemize/3]).
:- use_module(home(kb),
              [get_example/3,ex/3,known/6,assertallz/1,get_predlist/1,
               get_evaluation/2,delete_example/1,delete_clause/1,get_clause/5]).
:- use_module(home(interpreter),
              [prooftrees/3,solve_once/3,proof_close/2,
               solve/3,ip_part1/2,ip_part2/3]).
:- use_module_if_exists(library(basics),
              [member/2]).
:- use_module_if_exists(library(subsumes),
              [subsumes_chk/2,variant/2]).
:- use_module_if_exists(library(occurs),
              [sub_term/2]).
:- use_module_if_exists(library(math),
              [pow/3]).


% METAPREDICATES
% none



:- dynamic evaluated/1.


%***********************************************************************
%*	
%* module: evaluation.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92
%*									
%* changed:								
%*									
%* description:	evaluation of (parts of) the knowledge base
%*                                                                      
%* see also:	
%*									
%***********************************************************************



%***********************************************************************
%*									
%* predicate:	ip/1							
%*									
%* syntax:	ip(-UA_List)							
%*									
%* args: -UA_List ... list of ground atoms
%*									
%* description:	Shapiro's algorithm for diagnosing finite failure       
%*              ip in our framework. Returns a set of ground atoms that        
%*              has to be covered to make all uncovered positive        
%*              examples succeed.
%*  	        Allows backtracking on alternative sets of ground atoms
%*              that make all examples succeed. 
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

ip(UA_List):-
   (   evaluated(no) ->
       eval_examples
   ;   true
   ),
   mysetof(E,I^Trees^(get_example(I,E,'+'),prooftrees(I,fail,Trees)),Elist),!,
   ip_list(Elist,[],UA_List1),
   make_unique(UA_List1,UA_List).

ip_list([],L,L).
ip_list([E|R],L,L2):-
   ip0(E,UAs),
   append(L,UAs,L1),
   ip_list(R,L1,L2).



%***********************************************************************
%*									
%* predicate:								
%*									
%* syntax:	ip(+UA,-UAs)							
%*									
%* args:								
%*									
%* description: UA is an uncovered atom, i.e. both prooftrees(I,fail,Trees) 
%*   	and ex(I,UA,+) are in the knowledge base.
%*   	UAs is a list [A1,...,An] such that a proof of UA would 
%*   	succeed if A1 through An were covered by the knowledge base.
%*   	Cave!: Extensive oracle interaction
%*
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

ip0(Goal,UAs):-
   setof(Proof,Goal^ip_part1(Goal,Proof),Proofs0),
   append_all(Proofs0,Proofs1),
   proof_close(Proofs1,Proofs),!,
   ip_part2(Proofs,Goal,UAs0),
   make_unique(UAs0,UAs).


%***********************************************************************
%*									
%* predicate:	fp/1							
%*									
%* syntax:	fp(-OR)						
%*									
%* args:	OR:							
%*									
%* description: a kind of shapiro's contradiction backtracing that      
%*              aims to detect possibly overgeneral clauses.
%*              As it does not use an oracle, all possibly overgeneral
%*              clauses are considered and a minimal combination 
%*              such that all negative examples become uncovered is
%*              returned.
%*              Allows backtracking to an alternative set of possibly
%*              overgeneral clauses
%* 	        OR is a list [I:E,...], where I is the index of a possibly
%*   	        overgeneral clause and E is the set of wrong (head-)instantiations of
%*   	        clause I that should be excluded by specializing I.
%*   	        OR is a minimal selection of possibly overgeneral clauses such
%*   	        that by specialising them all negative examples become uncovered.
%*   	        On backtracking, the second selection is returned, and so on.
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

fp(OR):-
   (   evaluated(no) ->
       eval_examples
   ;   true
   ),
   bagof(TL,I^E^Trees^P^(get_example(I,E,'-'),prooftrees(I,success,Trees),
                         member(P,Trees),fp(P,[],TL)), TList),
   collect_indices(TList,[],Indices),
   or_subsets(Indices,TList,OR_List),!,
   best(OR_List,OR).
fp([]).


%***********************************************************************
%*									
%* predicate:	fp/3							
%*									
%* syntax: fp(+Prooftree,+L,-L)								
%*									
%* args: Prooftree is a prooftree for a succeeding negative example
%*       L = [...,ID:[G1,..,Gn],...] where ID is a clause index and
%*       G1,..,Gn are the head instantiations  the clause has been applied with
%*       during the proof Prooftree.
%*									
%* description:	collects clauses and goals that have been used during 
%*              a successfull proof of a negative example
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

fp([sys,_,_],L,L):- !.
fp([I,A,SG],L,L2):-
   fp_list(SG,L,L1),
   insert_unique(I,A,L1,L2).

fp_list([],L,L).
fp_list([G|R],L,L2):-
   fp_list(R,L,L1),
   fp(G,L1,L2).



%***********************************************************************
%*									
%* predicate:	fp_hyp/1							
%*									
%* syntax:	fp_hyp(-OR)						
%*									
%* args:	OR:							
%*									
%* description: as fp/1, but considers only clauses with label 'hypo' as 
%*              possibly overgeneral
%*              Allows backtracking to an alternative set of possibly
%*              overgeneral clauses
%* 	        OR is a list [I:E,...], where I is the index of a possibly
%*   	        overgeneral clause and E is the set of wrong (head-)instantiations of
%*   	        clause I that should be excluded by specializing I.
%*   	        OR is a minimal selection of possibly overgeneral clauses such
%*   	        that by specialising them all negative examples become uncovered.
%*   	        On backtracking, the second selection is returned, and so on.
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

fp_hyp(OR):-
   (   evaluated(no) ->
       eval_examples
   ;   true
   ),
   bagof(TL,I^E^Trees^P^(get_example(I,E,'-'),prooftrees(I,success,Trees),
                         member(P,Trees),fp_hyp(P,[],TL)), TList),
   collect_indices(TList,[],Indices),
   or_subsets(Indices,TList,OR_List),!,
   best(OR_List,OR).
fp_hyp([]).


%***********************************************************************
%*									
%* predicate:	fp_hyp/3							
%*									
%* syntax: fp_hyp(+Prooftree,+L,-L)	
%*									
%* args: Prooftree is a prooftree for a succeeding negative example
%*       L = [...,ID:[G1,..,Gn],...] where ID is a clause index of a clause with
%*       label 'hypo', and
%*       G1,..,Gn are the head instantiations  the clause has been applied with
%*       during the proof Prooftree.
%*									
%* description:	collects clauses and goals that have been used during 
%*              a successfull proof of a negative example
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

fp_hyp([sys,_,_],L,L):- !.
fp_hyp([I,A,SG],L,L2):-
   fp_hyp_list(SG,L,L1),
   (   get_clause(I,_,_,_,hypo) ->
       insert_unique(I,A,L1,L2)
   ;   L2 = L1
   ).

fp_hyp_list([],L,L).
fp_hyp_list([G|R],L,L2):-
   fp_hyp_list(R,L,L1),
   fp_hyp(G,L1,L2).



%***********************************************************************
%*									
%* predicate:	fpo/1							
%*									
%* syntax:	fpo(-OR)						
%*									
%* args:	OR:							
%*									
%* description: as fp/1, but uses oracle
%*              Allows backtracking to an alternative set of possibly
%*              overgeneral clauses
%* 	        OR is a list [I:E,...], where I is the index of a possibly
%*   	        overgeneral clause and E is the set of wrong (head-)instantiations of
%*   	        clause I that should be excluded by specializing I.
%*   	        OR is a minimal selection of possibly overgeneral clauses such
%*   	        that by specialising them all negative examples become uncovered.
%*   	        On backtracking, the second selection is returned, and so on.
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

fpo(OR):-
   (   evaluated(no) ->
       eval_examples
   ;   true
   ),
   bagof(TL,I^E^Trees^P^(get_example(I,E,'-'),prooftrees(I,success,Trees),
                         member(P,Trees),fpo(P,[],TL,_)), TList),
   collect_indices(TList,[],Indices),
   or_subsets(Indices,TList,OR_List),!,
   best(OR_List,OR).
fpo([]).


%***********************************************************************
%*									
%* predicate:	fpo/4							
%*									
%* syntax: fpo(+Prooftree,+L,-L,-M)
%*									
%* args: Prooftree is a prooftree for a succeeding negative example
%*       L = [...,ID:[G1,..,Gn],...] where ID is a clause index and
%*       G1,..,Gn are the head instantiations  the clause has been applied with
%*       during the proof Prooftree.
%*       M indicates whether Prooftree is successful in the oracle-simulation (ok)
%*       or not (not_ok)
%*									
%* description:	collects wrong clauses and goals that have been used during 
%*              a successfull proof of a negative example (uses oracle)
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

fpo([sys,_,_],L,L,ok):- !.
fpo([I,A,SG],L,L1,X):-
   (   ask_for(A) ->
       fpo_list(SG,L,L1,X)
   ;   insert_unique(I,A,L,L1),
       X = not_ok
   ).

fpo_list([],L,L,ok).
fpo_list([G|R],L,L2,X):-
   fpo(G,L,L1,Xg),
   (   Xg = ok ->
       fpo_list(R,L1,L2,X)
   ;   X = Xg
   ).


%***********************************************************************
%*									
%* predicate:	collect_indices/3							
%*									
%* syntax: collect_indices(+L,+Accu,-Accu)
%*									
%* args: L = [[I1:[G11,..,G1n],...,Im:[Gm1,...,Gmn]],...]
%*	 Accu = [I1,...,Ik]
%*								
%* description:	given the list of lists produced by fp/3, all indices of
%*     clauses that participated in successful proofs of negative examples
%*     are collected									
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

collect_indices([],L,L).
collect_indices([X|R],L,L2):-
   c_indices(X,L,L1),
   collect_indices(R,L1,L2).

c_indices([],L,L).
c_indices([I:_|R],L,L2):-
   c_indices(R,L,L1),
   (   member(I,L1) ->
       L2 = L1
   ;   L2 = [I|L1]
   ).



%***********************************************************************
%*									
%* predicate:	or_subsets/3							
%*									
%* syntax: or_subsets(+Indices,+Tlist,-OR_List)
%*									
%* args: Indices ... list of indices of clauses that participated in successful
%*                   proofs of negative examples
%*       Tlist = [[I:[G1,..,Gn],..],..] list of lists produced by fp/3
%*       OR_List = list of lists [I:E,..] where I is the index of a possibly
%*       overgeneral clause and E is the set of wrong (head-)instantiations of
%*       clause I that should be excluded by specializing I. OR_List is sorted 
%*       ascendingly according to the length of the sublists
%*									
%* description: selects all possible combinations of possibly overgeneral clauses
%*     	such that by specialising them all negative examples become uncovered.
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

or_subsets(IX,TL,ORL):-
   initialize_or_subsets(IX,IX,TL,TL1),
   or_all_subsets(TL1,[],TL2),
   sort_by_length(TL2,[],ORL).


%***********************************************************************
%*									
%* predicate:	initialize_or_subsets/4
%*									
%* syntax: initialize_or_subsets(+IX,+IX,+TL,-TL1)
%*									
%* args: IX list of clauseIDs 
%*       TL = [[I:CoveredI,J:CoveredJ,...],...] resulting from fp/3.
%*       each sublist in TL corresponds to a successful proof of a negative
%*       example
%* 									
%* description:	TL1 contains for each I in IX and entry [I:A]:IX1:TLI,
%*      where IX1 = IX - {I} and TLI results from TL by deleting every
%*      sublist [J:CJ,..,I:CI,...] that contains I:CI, and accumulating
%*      the head instances of I in A. 
%*      The set TLI contains all proofs of negative examples that are 
%*      still possible if clause I is excluded (e.g. specialised).
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

initialize_or_subsets([],_,_,[]).
initialize_or_subsets([I|R],IX,TL,[[I:A]:IX1:TL1|R1]):-
   initialize_or_subsets(R,IX,TL,R1),
   remove(I,IX,IX1),
   remove_conjuncts(I,TL,TL1,[],A).


%***********************************************************************
%*									
%* predicate:	remove_conjuncts/5							
%*									
%* syntax: remove_conjuncts(+I,+TL,-TLI,+A,-A)
%*									
%* args: I .. clause Index, TL = [[I:CI,J:CJ,...],...],
%*       A = [G1,..,Gn] head instances of I
%*									
%* description:	removes from TL every sublist containing I:CI, and accumulates
%*		CI in A. Each sublist in TL corresponds to a successful proof of 
%*              a negative example. If clause I is assumed to be overgeneral and 
%*              therefore excluded, the proof fails and the remaining clauses that
%*              have been used need not be specialised. Therefore, the sublist is
%*              removed from TL.
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

remove_conjuncts(_,[],[],A,A).
remove_conjuncts(I,[X|R],R1,A,A2):-
   member(I:E,X),!,
   append(E,A,A0),
   identical_make_unique(A0,A1),
   remove_conjuncts(I,R,R1,A1,A2).
remove_conjuncts(I,[X|R],[X|R1],A,A1):-
   remove_conjuncts(I,R,R1,A,A1).


%***********************************************************************
%*									
%* predicate:	or_all_subsets/3							
%*									
%* syntax: or_all_subsets(+TL1,+Accu,-Accu)
%*									
%* args: TL1 = [IXS:IXR:TLI,...] where IXS = [I:CI,...], IXR the indices not
%*       occurring in IXS, and TLI the remaining proofs of negative examples
%*       Accu = [IXS,...]
%*									
%* description:	tests every combination of clause indices whether all proofs
%*    of negative examples are excluded when the clauses are assumed to be 
%*    overgeneral. A combination IXS is successful, if all proofs are excluded,
%*    i.e. TLI = [].							
%*
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

or_all_subsets([IXS:_:[]|R],L,[IXS|L1]):-
   or_all_subsets(R,L,L1).
or_all_subsets([IXS:RI:TL|R],L,L1):-
   or_asubsets(RI,RI,IXS,TL,R1),
   append(R,R1,R2),
   or_all_subsets(R2,L,L1).
or_all_subsets([],L,L).

or_asubsets([],_,_,_,[]).
or_asubsets([I|R],RI,IXS,TL,[[I:A|IXS]:RI1:TL1|R1]):-
   remove(I,RI,RI1),
   remove_conjuncts(I,TL,TL1,[],A),
   or_asubsets(R,RI,IXS,TL,R1).


%***********************************************************************
%*									
%* predicate:	eval_pos_examples/1							
%*									
%* syntax:	 eval_pos_examples ( - List_of_Exs )	
%*									
%* args:								
%*									
%* description:	Evaluate (= try to prove) all positive examples, return a list of the
%*	ones which *cannot* be proved (empty list if successful).	
%*									
%* example:								
%*									
%* peculiarities: Output-argument looks like [exID1:Fact1, exID2:Fact2, ...].		
%*	!!! Procedure does not compute evaluation for clauses!!
%*									
%* see also:								
%*									
%***********************************************************************

eval_pos_examples(Exlist) :-
	retractall(prooftrees(_,_,_)),
        findall(I:Fact, ( ex(I,Fact,'+'), solve_once(Fact,fail,_) ), Exlist).        



%******************************************************************************
%* 
%* predicate: eval_examples/0 
%*
%* syntax:
%*
%* args:
%*
%* description: One should use this to compute the evaluation for kb clauses!
%*              - asserts for each example ID prooftrees(ID,Mark,Proofs), where
%*                Mark in {success,fail} and Proofs are the successful/failing
%*                proofs accordingly
%*              - determines the evaluation of each rule in the kb according to
%*                the current examples
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%******************************************************************************

eval_examples:- evaluated(yes),!.

eval_examples :-
	retractall(prooftrees(_,_,_)),
	eval_examples1,!,
        change_evaluated(yes).
eval_examples1 :-
        ex(I,Fact,_),
        solve(Fact,M,Proofs),
        assert(prooftrees(I,M,Proofs)),
        fail.
eval_examples1 :-
        bagof(I:Proofs,prooftrees(I,success,Proofs),Plist),
        findall(known(J,H,B,Clist,L,_),(get_clause(J,H,B,Clist,L),delete_clause(J)),Klist),
        % don't use bagof here!
        compute_evaluation(Klist,Plist,Klist1),
        assertallz(Klist1),!.
eval_examples1.               % in case there are no examples

clear_evaluation:- 
        retractall(prooftrees(_,_,_)),
        change_evaluated(no).

change_evaluated(X):-
    retractall(evaluated(_)),
    assert(evaluated(X)). 


%***********************************************************************
%*									
%* predicate:correct_chk/0 								
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description: fails when first *negative* example covered	
%*									
%* example:								
%*									
%* peculiarities: Does not compute evaluation for clauses!!
%*									
%* see also:								
%*									
%***********************************************************************

correct_chk :-
   (   evaluated(no) ->
       (   (ex(_ID,Fact,'-'),solve_once(Fact,success,_)) ->
           fail
       ;   true
       )
   ;   (   (ex(ID,_Fact,'-'),prooftrees(ID,success,_)) ->
	   fail
       ;   true
       )
    ).

%***********************************************************************
%*									
%* predicate: complete_chk/0 								
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:  fails if not all *positive* examples covered
%*									
%* example:								
%*									
%* peculiarities: Does not compute evaluation for clauses!!
%*									
%* see also:								
%*									
%* origin:  kb.pl (Irene/Markus)
%*									
%***********************************************************************

complete_chk :-
   (   evaluated(no) ->
       (   (ex(_ID,Fact,'+'), solve_once(Fact,fail,_)) ->
           fail
       ;   true
       )
   ;   (   (ex(ID,_Fact,'+'),prooftrees(ID,fail,_)) ->
	   fail
       ;   true
       )
   ).



%***********************************************************************
%*									
%* predicate:compute_evaluation/3
%*									
%* syntax: compute_evaluation(+Klist,+Plist,-Klist)
%*									
%* args: Klist ... list of kb-entries [known(I,H,B,Clist,Label,E),...]
%*		   where E is the evaluation of clause I
%*       Plist ... list of all successfule Proofs using Klist
%*                 = [I:Proofs,...] where prooftrees(I,success,Proofs) in kb
%*
%* description:	computes for each kb-entry in Klist the evaluation 
%*   E = evaluation(RA,NPos,Pos,NNeg,Neg,UNPos,UPos,UNNeg,UNeg), where
%*       RA ... #applications of the clause
%*       NPos ... #definitively positive examples covered by the clause
%*       Pos ... list of definitively positive examples covered by the clause
%*       NNeg ... #definitively negative  examples covered by the clause
%*       Neg ... list of definitively negative  examples covered by the clause
%*       UNPos ... #probably positive examples covered by the clause
%*       i.e. instantiations of the clause used in successful proofs of positive 
%*       examples
%*       UPos ... list of probably positive examples covered by the clause
%*       UNNeg ... #probably negative  examples covered by the clause
%*       i.e. instantiations of the clause used in successful proofs of negative 
%*       examples
%*       UNeg ... list of probably negative examples covered by the clause
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

compute_evaluation([],_,[]).
compute_evaluation(
   [known(I,H,B,Clist,O,_)|R],Plist,
   [known(I,H,B,Clist,O,evaluation(RA,NPos,Pos,NNeg,Neg,UNPos,UPos,UNNeg,UNeg))|R1]
   ):-
   compute_evaluation(R,Plist,R1),
   compute_eval(Plist,I,RA,NPos,Pos,NNeg,Neg,UNPos,UPos,UNNeg,UNeg).


compute_eval([],_,0,0,[],0,[],0,[],0,[]).
compute_eval([I:Proofs|R],J,RA,NPos,Pos,NNeg,Neg,UNPos,UPos,UNNeg,UNeg):-
   compute_eval(R,J,RA0,NPos0,Pos0,NNeg0,Neg0,UNPos0,UPos0,UNNeg0,UNeg0),
   (   ex(I,_,'-') ->
       compute_eval(Proofs,t,I,J,RA0,RA,NNeg0,NNeg,Neg0,Neg,UNNeg0,UNNeg,UNeg0,UNeg),
       NPos = NPos0, Pos = Pos0, UNPos = UNPos0, UPos = UPos0
   ;   compute_eval(Proofs,t,I,J,RA0,RA,NPos0,NPos,Pos0,Pos,UNPos0,UNPos,UPos0,UPos),
       NNeg = NNeg0, Neg = Neg0, UNNeg = UNNeg0, UNeg = UNeg0
   ).

compute_eval([],_,_,_,RA,RA,N,N,L,L,UN,UN,UL,UL).
compute_eval([[I,H,B]|R],T,K,J,RA,RA3,N,N3,L,L3,UN,UN3,UL,UL3):-
   compute_eval(R,T,K,J,RA,RA1,N,N1,L,L1,UN,UN1,UL,UL1),
   compute_eval(B,b,K,J,RA1,RA2,N1,N2,L1,L2,UN1,UN2,UL1,UL2),
   (   I == J ->
       RA3 is RA2 + 1,
       (   T == t ->
           (   member(_:H,L2) ->
               L3 = L2, N3 = N2, UN3 = UN2, UL3 = UL2
           ;   N3 is N2 + 1,
               L3 = [K:H|L2], 
               UN3 = UN2, UL3 = UL2
           )
       ;   (   member(_:H,UL2) ->
               L3 = L2, N3 = N2, UN3 = UN2, UL3 = UL2
           ;   UN3 is UN2 + 1,
               UL3 = [K:H|UL2],
               N3 = N2, L3 = L2
           )
       )
   ;   RA3 = RA2, L3 = L2, N3 = N2, UN3 = UN2, UL3 = UL2
   ).


%***********************************************************************
%*									
%* predicate:	covered_pos_examples/1
%*									
%* syntax: covered_examples(-CE)
%*									
%* args: CE ... list of IDs of covered positive examples
%*									
%* description:	returns IDs of all covered positive examples
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

covered_pos_examples(Bag):- 
        (   evaluated(no) ->
            eval_examples
        ;   true
        ),
        findall( ID, (  get_example(ID,_,'+'), prooftrees(ID,success,_) ),
                 Bag),!.


%***********************************************************************
%*									
%* predicate:	covered_neg_examples/1
%*									
%* syntax: covered_neg_examples(-CE)
%*									
%* args: CE ... list of IDs of covered negative examples
%*									
%* description:	returns IDs of all covered negative examples
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

covered_neg_examples(Bag):- 
        (   evaluated(no) ->
            eval_examples
        ;   true
        ),
        findall( ID, (  get_example(ID,_,'-'), prooftrees(ID,success,_) ),
                 Bag),!.



%***********************************************************************
%*									
%* predicate:	all_covered_examples/1
%*									
%* syntax: all_covered_examples(-CE)
%*									
%* args: CE ... list of IDs of covered negative examples
%*									
%* description:	returns IDs of all covered  examples
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

all_covered_examples(Bag):- 
        (   evaluated(no) ->
            eval_examples
        ;   true
        ),
        findall( ID, (  get_example(ID,_,_), prooftrees(ID,success,_) ),
                 Bag),!.



%***********************************************************************
%*									
%* predicate:	complexity/2								
%*									
%* syntax: complexity(+ClauseID,-Size)
%*									
%* args: 								
%*									
%* description:	for kb references							
%*		complexity/2 calculates the size of a clause,		
%*		defined to be the number of constant and function       
%*		symbol occurences in the literals of the clause.	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

complexity( I, C):-
        integer(I),
        get_clause(I,_,_,Clause,_),
        compute_complexity(Clause,C),!.



%***********************************************************************
%*									
%* predicate:	complexity/2								
%*									
%* syntax: complexity(+CL,-Size)
%*									
%* args:								
%*									
%* description: for clauses in list representation
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*									
%***********************************************************************

complexity( Clause, C):-
        Clause = [ _H:p | _ ],
        compute_complexity(Clause,C),!.


%***********************************************************************
%*									
%* predicate: complexity/2
%*									
%* syntax: complexity(List_of_ClauseIDs,-Size)
%*									
%* args:								
%*									
%* description:	for a list of kb references
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*									
%***********************************************************************
 
complexity( [ID|List], C) :-
	integer(ID),
        findall(Com, ( member(I, [ID|List]),
	               get_clause(I,_,_,Clause,_),
		       compute_complexity(Clause,Com) ), Bag),
        sum( Bag,C),!.
	

%***********************************************************************
%*									
%* predicate:	complexity/2								
%*									
%* syntax: complexity(+Term,-Size)
%*									
%* args:								
%*									
%* description:	for arbitrary prolog terms ( but not integers)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

complexity(Term,Complexity):-
        term_size(Term,Complexity),!.



%***********************************************************************
%*									
%* predicate:	complexity/2							
%*									
%* syntax: complexity(+usr,-Size)
%*									
%* args:								
%*									
%* description:	for all clauses with label usr
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

complexity( usr, C):- 
        findall( I, ( get_clause(_,_,_,Clause,usr), compute_complexity(Clause,I) ), Bag),
        sum( Bag,C),!.


%***********************************************************************
%*									
%* predicate: complexity/2								
%*									
%* syntax: complexity(+examples,-Size)
%*									
%* args:								
%*									
%* description: for all examples
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

complexity( examples, C):- 
        findall( I, ( get_example(_,Clause,_), compute_complexity(Clause,I) ), Bag),
        sum( Bag,C),!.



%***********************************************************************
%*									
%* predicate:	compute_complexity/2							
%*									
%* syntax: compute_complexity(+CL,-Size)
%*									
%* args: CL ... clause in list represenation
%*									
%* description:	complexity for a clause in list representation
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

compute_complexity( [], 0).
compute_complexity( [L:_|More],C):-
        term_size( L, C1),
        compute_complexity(More,C2),
        C is C1 + C2.


%***********************************************************************
%*									
%* predicate:	ivonTunterE/1						
%*									
%* syntax: ivonTunterE(-ITE)						
%*									
%* args: ITE... information content of T, given E
%*       (only for funtion-free T and E!!)
%*									
%* description:	Given evidence E for T. Then if T|=E, then
%*     I(T|E) = I(T) + I(E|T). If T = B & H, then T compresses
%*     the examples E if I(T|E) =< I(B & E)
%*     Precondition: B, T, E function-free!
%*     
%*     How to compute I(T) and I(E|T) (for function-free T,E):
%*     -  I(E|T) = log2( (|M+(T)|  |E+|) ) + log2( (|M-(T)|  |E-(T)|) )
%*     -  P(T) .. #Pred. Symbols in T
%*        C(T) .. #Constants in T
%*        V(T) .. max number of vars of any clause in T
%*        a .. max arity of any pred. symbol in T
%*        l .. max cardinality of the body of any clause in T
%*        |T| .. #clauses in T
%*        
%*        |A(T)| =< P(T)*(C(T) + V(T))^a
%*        |CL(T)| =< |A(T)| * (|A(T)|  l)
%*        I(T) = log2( (|CL(T)|  |T|) )
%*        where (a  b) == (a) = n!/(k!*(n-k)!)
%*                        (b)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also: L. DeRaedth, S. Muggleton: ILP: Theory and Methods
%*	     submitted to Journal of LP
%*
%***********************************************************************

ivonTunterE(ITE):-
   findall(P/A, (get_clause(_,_,_,CL,_),
                 member(F:_,CL),functor(F,P,A)),Predlist0),
   make_unique(Predlist0,Predlist),
   max_arity(Predlist,A),
   length(Predlist,PT),
   findall(C,(get_clause(_,H,B,_,_),
              sub_term(C,(H,B)),
              atomic(C),C \== true),Clist0),
   make_unique(Clist0,Clist),
   length(Clist,CT),
   findall(V/L2,(get_clause(_,_,_,CL,_),
                 length(CL,L1), L2 is L1 - 1,
                 vars(CL,VL),length(VL,V)),LList),
   maxvars(LList,VT),
   max_arity(LList,L),
   findall(ID,get_clause(ID,_,_,_,_),IDL),
   length(IDL,BT),
   ivonT(PT,CT,VT,A,L,BT,IT),
   ivonEunterT(Predlist,CT,IET),
   ITE is IT + IET.



ivonT(PT0,CT0,VT0,A0,L0,BT0,IT):-
   PT is float(PT0), 
   CT is float(CT0), 
   VT is float(VT0), 
   A is float(A0),
   L is float(L0),
   BT is float(BT0),
   X1 is CT + VT,
   pow(X1,A,X2),
   AT is PT * X2,
   nueberk(AT,L,X3),
   CLT is AT * X3,
   log2nueberk(CLT,BT,IT).

ivonEunterT(Predlist,CT,IET):-
   all_atoms(Predlist,CT,HT0),
   mTplus(CT,MTP0),
   HT is float(HT0),
   MTP is float(MTP0),
   MTM is HT - MTP,
   findall(P,get_example(_,P,+),PL),
   length(PL,PLN0),
   findall(N,get_example(_,N,-),NL),
   length(NL,NLN0),
   PLN is float(PLN0),
   NLN is float(NLN0),
   log2nueberk(MTP,PLN,LX),
   log2nueberk(MTM,NLN,LY),
   IET is LX + LY.

%***********************************************************************
%*									
%* predicate:	ivonBundE/1						
%*									
%* syntax: ivonBundE(-IBE)						
%*									
%* args: ITE... information content of B & E
%*       (only for funtion-free B and E!!)
%*									
%* description:	computes information content of B & E.
%*     If T = B & H, then T compresses
%*     the examples E if I(T|E) =< I(B & E)
%*     Precondition: B, T, E function-free!
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also: L. DeRaedth, S. Muggleton: ILP: Theory and Methods
%*	     submitted to Journal of LP
%*
%***********************************************************************

ivonBundE(IBE):-
   findall(P/A, (get_clause(_,_,_,CL,_),
                 member(F:_,CL),functor(F,P,A)),Predlist00),
   findall(P1/A1, (get_example(_,F,_),
                 functor(F,P1,A1)),Predlist01),
   append(Predlist00,Predlist01,Predlist0),
   make_unique(Predlist0,Predlist),
   max_arity(Predlist,A),
   length(Predlist,PT),
   findall(C,(get_clause(_,H,B,_,_),
              sub_term(C,(H,B)),
              atomic(C),C \== true),Clist00),
   findall(C1,(get_example(_,H1,_),
               sub_term(C1,H1),
               atomic(C1),C1 \== true),Clist01),
   append(Clist00,Clist01,Clist0),
   make_unique(Clist0,Clist),
   length(Clist,CT),
   findall(V/L2,(get_clause(_,_,_,CL,_),
                 length(CL,L1), L2 is L1 - 1,
                 vars(CL,VL),length(VL,V)),LList),
   maxvars(LList,VT),
   max_arity(LList,L),
   findall(ID,get_clause(ID,_,_,_,_),IDL00),
   findall(ID1,get_example(ID1,_,_),IDL01),
   append(IDL00,IDL01,IDL),
   length(IDL,BT),
   ivonT(PT,CT,VT,A,L,BT,IBE).


%***********************************************************************
%*									
%* predicate:	all_atoms/3							
%*									
%* syntax: all_atoms(+Predlist,+No_constants,-No_atoms)
%*									
%* args: Predlist = [p1/arity1,...,pn/arityn] list of pred. symbols and 
%*                   their arities
%*       No_constants.... number c of constants in the current theory
%*       No_atoms = number of atoms that can be built from the preds
%*                  in predlist and the c constants
%*                = c^arity1 + .... + c^arityn
%*									
%* description:	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

all_atoms([],_,0).
all_atoms([_/A0|R],CT0,HT):-
   all_atoms(R,CT0,HT0),
   CT is float(CT0),
   A is float(A0),
   pow(CT,A,X),
   HT is HT0 + X.


%***********************************************************************
%*									
%* predicate:	max_arity/2, maxvars/2, maxi/3
%*									
%* syntax: max_arity(+Plist,-A), maxvars(+Vlist,-V), maxi(+X,+Y,-Z)
%*									
%* args: Plist = [_/n1,...,_/nn] for numbers ni, A is the max of the ni
%*	 Vlist = [_/n1,...,_/nn] for numbers ni, V is the max of the ni
%*       Z is the max of X and Y
%*							
%* description:	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

max_arity([_/A],A):- !.   
max_arity([_/A|R],C):-
   max_arity(R,B),
   maxi(A,B,C).

maxvars([A/_],A):- !.
maxvars([A/_|R],C):-
   maxvars(R,B),
   maxi(A,B,C).

maxi(A,B,C):-
   (   A >= B ->
       C = A
   ;   C = B
   ).

%***********************************************************************
%*									
%* predicate:	herbrand_base_ff/1							
%*									
%* syntax: herbrand_base_ff(-M)
%*									
%* args: M .. reduced list of atoms entailed by the current 
%*       function-free theory
%*									
%* description:	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

herbrand_base_ff(M):-
   findall(H, get_clause(_,H,true,_,_),M00),
   reduce_hb(M00,M0),
   findall(ID,(get_clause(ID,_,B,_,_),B \== true),IDlist),
   herbrand_base_ff(IDlist,M0,M).

herbrand_base_ff(IDlist,M,M2):-
   herbrand_base_ff(IDlist,M,M,M1,Mark),
   (   Mark == changed ->
       herbrand_base_ff(IDlist,M1,M2)
   ;   M2 = M1
   ).

herbrand_base_ff([],_,M,M,not_changed).
herbrand_base_ff([ID|R],M,M1,M4,Mark):-
   herbrand_base_ff(R,M,M1,M2,Mark0),
   get_clause(ID,H,B,_,_),
   findall(H1,match_body(H,B,M,H1),HL),
   append(HL,M2,M3),
   make_unique(M3,M31),
   reduce_hb(M31,M4),
   (   remove_variant(M2,M4,[]) ->
       Mark = Mark0
   ;   Mark = changed
   ).

match_body(H,B,M,H1):-
   copy_term((H,B),(H1,B1)),
   copy_term(M,M1),
   match_body(B1,M1).

match_body((A,B),M):- !,
   member(A,M),
   match_body(B,M).
match_body(A,M):-
   member(A,M).

reduce_hb(L,L1):-
   reduce_hb(L,L,L1).
reduce_hb([],_,[]).
reduce_hb([H|R],L,R2):-
   reduce_hb(R,L,R1),
   (   \+(sub_contained_in(H,L)) ->
       R2 = [H|R1]
   ;   R2 = R1
   ).

sub_contained_in(H,[H1|R]):-
   (   (H1 \== H, subsumes_chk(H1,H)) ->
       true
   ;   sub_contained_in(H,R)
   ).


%***********************************************************************
%*									
%* predicate:	mTplus/2							
%*									
%* syntax: mTplus(+No_constants,-MT)
%*									
%* args: No_constants... number c of constants in T
%*       MT ... size of M+(T) for theory T
%*									
%* description:	determines first the reduced Herbrand base of T, i.e.
%*       a list [A1,...,An] where Ai are atoms that might contain variables.
%*       The size of M+(T) is then
%*        |vars(A1)|^c + .... + |vars(An)|^c
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

mTplus(CT,MT):-
   herbrand_base_ff(M),
   hb_plus(M,CT,MT).

hb_plus([],_,0).
hb_plus([T|R],CT0,MT):-
   hb_plus(R,CT0,MT1),
   vars(T,V),
   length(V,VN0),
   CT is float(CT0),
   VN is float(VN0),
   pow(CT,VN,X),
   MT is MT1 + X.


%***********************************************************************
%*									
%* predicate:	code_length/2							
%*									
%* syntax: code_length(+Term,-CL)
%*									
%* args: CL .. code length of Term
%*									
%* description:	code length of a term a la R. Wirth/S. Muggleton: 
%*              let sym(Term) be all symbols in Term, and N the number of
%*              all symbol occurrences in Term. Let ps be the relative 
%*              frequency of symbol s in Term. Then
%*              code_length(Term)= N * sum_{s in sym(Term)} -ps log2ps
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

code_length(S,L):-
   skolemize(S,_,S0),
   symbol_frequencies(S0,[],SymS),
   relative_frequencies(SymS,0,N,SymS1),
   code_length1(SymS1,L0),
   L is N * L0.

code_length1([],0).
code_length1([F|R],L):-
   code_length1(R,L0),
   log2(F,LF),
   L1 is F * (-LF),
   L is L0 + L1.

relative_frequencies([],N,N,[]).
relative_frequencies([_/_/M|R],N0,N,[RM|R1]):-
   N1 is N0 + M,
   relative_frequencies(R,N1,N,R1),
   RM is M/N.

symbol_frequencies(X,L,L1):-
   atomic(X),!,
   update_frequency_list(L,X,0,L1).
symbol_frequencies(X,L,L1):-
   functor(X,F,N),
   update_frequency_list(L,F,N,L0),
   symbol_frequencies(N,X,L0,L1).

symbol_frequencies(0,_,L,L):- !.
symbol_frequencies(N,X,L,L2):-
   N1 is N - 1,
   symbol_frequencies(N1,X,L,L1),
   arg(N,X,Xn),
   symbol_frequencies(Xn,L1,L2).

update_frequency_list([],F,N,[F/N/1]).
update_frequency_list([F/N/M|R],F,N,[F/N/M1|R]):-
   !, M1 is M + 1.
update_frequency_list([X|R],F,N,[X|R1]):-
   update_frequency_list(R,F,N,R1).



%***********************************************************************
%*									
%* predicate:	encoding_length_examples/1, encoding_length_clause/2
%*									
%* syntax: encoding_length_examples(-EE)
%*         encoding_length_clause(+CL,-EC)
%*									
%* args: EE, EC.. floats
%*       CL... clause in list representation
%*									
%* description:	encoding length a la Quinlan:
%*    for examples: PN.. no of pos ex., NN.. no. of neg ex, U = PN + NN
%*             EE = log2(U) + log2((U  PN))
%*    for clauses: N.. length of Clause, Preds.. no of preds,
%*                 A .. no of poss. args
%*             EC = (sum_{i=1}^{N} bits for literal i)/log2(N!)
%*             bits for literali = 1 + log2(Preds) + log2(A)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

encoding_length_examples(X):-
   mysetof(ID,F^(get_example(ID,F,+)),PL),
   length(PL,PN),
   mysetof(ID1,F1^(get_example(ID1,F1,-)),NL),
   length(NL,NN),
   U is PN + NN,
   log2(U,LU),
   U1 is float(U),
   PN1 is float(PN),
   log2nueberk(U1,PN1,Y),
   X is LU + Y.

encoding_length_clause(CL,EL):-
   length(CL,N),
   N1 is float(N),
   sum_of_logs(1.0,N1,LNF),
   encoding_length_lits(CL,Lits0),
   get_predlist(PList),
   length(PList,Preds),
   log2(Preds,LPreds),
   Lits is Lits0 + N + (N * LPreds),
   EL is Lits/LNF.

encoding_length_lits([H:p|R],M):-
   functor(H,_,N),
   H =.. [_|Args],
   log2(N,LN),
   encoding_length_lits(R,Args,M1),
   M is M1 + LN.

encoding_length_lits([L:_|R],Args,M):-
   length(Args,LA),
   log2(LA,M0),
   L =.. [_|Args1],
   append(Args1,Args,Args2),
   identical_make_unique(Args2,Args3),
   encoding_length_lits(R,Args3,M1),
   M is M0 + M1.
encoding_length_lits([],_,0). 
   

