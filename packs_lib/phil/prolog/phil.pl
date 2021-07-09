/** <module> hplp

This module provides algorithms for learning the structure and the parameters of Hierachical Probabilic Logic Programs (HPLP) from data. Structure learning is done by predicates invention and parameter learning by gradient descent (Backpropagation) or Expectation maximization.

@author Arnaud Nguembang Fadja and Fabrizio Riguzzi
@copyright Arnaud Nguembang Fadja and Fabrizio Riguzzi
*/

/*
 HPLP: Hierarchical Probabilistic Logic Programs
 Copyright (c) 2020, Arnaud Nguembang Fadja and Fabrizio Riguzzi

*/

:- module(phil,[induce_hplp_par/2,induce_hplp/2,sample_hplp/4,inference_hplp/3,inference_hplp/4,
  test_hplp/7,op(500,fx,#),op(500,fx,'-#'),set_hplp/2]).


:-use_module(library(auc)).
:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).
:-use_module(library(terms)).
:-use_module(library(rbtrees)).
:-use_module(library(apply)).
:- use_module(library(pio)).
:- use_module(library(dialect)).
:-set_prolog_flag(unknown,warning).

:-load_foreign_library(foreign(phil)).
%:-load_foreign_library(phil).


:- dynamic getIndex/1.
:- dynamic getRate/1.
:- dynamic db/1.

:- dynamic input_mod_hplp/1.

:- thread_local  input_mod_hplp/1.

/*
Warning: 14:43:55.911 phil:induce_hplp_par_func/9 is declared as meta_predicate induce_hplp_par_func(:,-,-,-,-,-,-,-,-), but has no clauses
Warning: 14:43:55.911 phil:setting_hplp/2 is declared as meta_predicate setting_hplp(:,-), but has no clauses
Warning: 14:43:55.912 phil:objective_hplp_func/8 is declared as meta_predicate objective_hplp_func(:,-,-,-,-,-,-,-), but has no clauses
Warning: 14:43:55.912 phil:induce_hplp_parameters/4 is declared as meta_predicate induce_hplp_parameters(:,-,-,-), but has no clauses
Warning: 14:43:55.912 phil:induce_hplp_par_func/5 is declared as meta_predicate induce_hplp_par_func(:,-,-,-,-), but has no clauses

*/
%:- meta_predicate induce_hplp(:,-).
%:- meta_predicate objective_hplp_func(:,-,-,-,-,-,-,-).

:- meta_predicate induce_hplp_rules(:,-).
:- meta_predicate induce_hplp_par(:,-).
%:- meta_predicate induce_hplp_par_func(:,-,-,-,-,-,-,-,-).
%:- meta_predicate induce_hplp_par_func(:,-,-,-,-).
:- meta_predicate induce_hplp_parameters(:,-,-).
:- meta_predicate test_hplp(:,+,-,-,-,-,-).

:- meta_predicate inference_hplp(:,+,-).
:- meta_predicate inference_hplp(:,+,-,-).

:- meta_predicate test_hplp_prob(:,+,-,-,-,-).
:- meta_predicate set_hplp(:,+).
%:- meta_predicate setting_hplp(:,-).



%      SLEAHP default settings
default_setting_hplp(group,1). % use in the predicate derive_circuit_groupatoms (..)
default_setting_hplp(megaex_bottom,1).  % Necessary for the predicate test_hplp(..)
default_setting_hplp(initial_clauses_per_megaex,1).
default_setting_hplp(neg_literals,false).
default_setting_hplp(neg_ex,cw).
default_setting_hplp(epsilon_parsing, 1e-5).
default_setting_hplp(depth_bound,false).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_hplp(depth,2).
default_setting_hplp(single_var,false). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.
default_setting_hplp(verbosity,1).
default_setting_hplp(compiling,off).
default_setting_hplp(d,1).

default_setting_hplp(probability,1.0). % Initial value which indicates the probability to go to the next layer during HPLP generation
default_setting_hplp(rate,0.95). % Rate to multiply to the current probability at each layer
default_setting_hplp(min_probability,1e-5).  % Threshold of the probability under which the clause is dropped
default_setting_hplp(unifyRoot,yes).
default_setting_hplp(use_all_mega_examples,yes).
default_setting_hplp(saveHPLP,no). 
default_setting_hplp(saveFile,"hplp"). % File where to save the initial large HPLP generated
default_setting_hplp(max_layer,-1). % Indicates the Max number of clause layer. -1 indicates the highest layer possible


%     PHIL  default settings
default_setting_hplp(maxIter_phil,1000). % Max iteration
default_setting_hplp(epsilon_deep,0.0001). % epsilon 
default_setting_hplp(epsilon_deep_fraction,0.00001). % delta
default_setting_hplp(max_initial_weight,0.5). % initial weights of dphil in [-0.5 0.5]
default_setting_hplp(adam_params,[0.001,0.9,0.999,1e-8]). % default Adam hyper-pameters
default_setting_hplp(batch_strategy,minibatch(100)). % allowed values: batch, minibatch(size), stoch_minibatch(size)
default_setting_hplp(algorithmType,dphil).% allowed values: dphil, emphil
default_setting_hplp(saveStatistics,no). % indicates wheter  to safe the statistics or no
default_setting_hplp(statistics_folder,statistics). %if saveStatistics is different to "no", it indicates the folder where to safe the statistics during paramenter learning.
default_setting_hplp(zero,0.000001).  % Approximate value of 0 
default_setting_hplp(useInitParams,no). % if yes the initial parameters during the learning are the ones indicated in the progam. Otherwise, the parameters are initialy generated randomly
default_setting_hplp(setSeed,no). 
default_setting_hplp(c_seed,c_seed).
default_setting_hplp(logzero,log(0.000001)).
default_setting_hplp(seed,seed(3032)).
% regularization parameters
default_setting_hplp(regularized,no). % yes for activating the regularization and no otherwise
default_setting_hplp(regularizationType,1). % set the regulariztion type. 1,2 if algorithmType=dphil. Otherwise 1,2,3 
default_setting_hplp(gamma,10). % set the value of gamma according to the type (gamma=a if type=3)
default_setting_hplp(gammaCount,0). % set the value of gammaCount according to the type (gammaCount=b if type=3)



/*
  dphil/8, emphil/6, forward/4
*/

/**
 * dphil(++ListOfACs:list,++Params1:list,++StopCondition:list,++Folder:list,++AdamParams:list,++Params2:list,-CLL:float,-Probabilities:list) is det
 *
 * Performs gradient descent learning on HPLP.
 * Takes as input  a list of ACs each representing one example,
 * Params is a list whose elements are [NR,ZERO,Seeded,Seed] where NR is the number of rule, ZERO is the value to use if the 
 * denominator of a particular fraction is zero, Seeded (yes to set a Seed or no otherwise).
 * StopCond is a list whose elements are[MaxIter,EA,ER] where MaxIter is the maximum number of iterations
 * EA and ER are the minimum absolute difference  and relative between the
 * log likelihood of examples in two different iterations 
 * Folder is a list whose elements are [Saved,StatisticsFolder] where if Saved is yes, the statistics are
 * saved in StatisticsFolder and no otherwise
 * AdamParams is a list whose elements are [Eta,Beta1,Beta2,Epsilon_adam_hat] containing the Adam parameters
 * Params2 is a list whose elements are [Strategy,BatchSize,MAX_W] where Strategy is the  strategy used in
 * gradient descent (batch, minibatch or stochastic minibatch) and BatchSize the corresponding batch.
 * weights are initialized in the range [-MAX_W,+MAX_W]
 *
 * It returns the final log likelihood of examples CLL and the list of learned Parameters probabilities
 */


/**
 * emphil(++ListOfACs:list,++Params1:list,++StopCondition:list,++Folder:list,-CLL:float,-Probabilities:list) is det
 *
 * Performs EM learning on HPLP.
 * Takes as input  a list of ACs each representing one example,
 * Params is a list whose elements are [NR,ZERO,Seeded,Seed] where NR is the number of rule, ZERO is the value to use if the 
 * denominator of a particular fraction is zero, Seeded (yes to set a Seed or no otherwise).
 * StopCond is a list whose elements are[MaxIter,EA,ER] where MaxIter is the maximum number of iterations
 * EA and ER are the minimum absolute difference  and relative between the
 * log likelihood of examples in two different iterations 
 * Folder is a list whose elements are [Saved,StatisticsFolder] where if Saved is yes, the statistics are
 * saved in StatisticsFolder and no otherwise
 *
 * It returns the final log likelihood of examples CLL and the list of learned Parameters probabilities
 */

%!
 % forward(AC:+term,++Pobabilities:list,+:integer-Probability:+double) is det
 %
 % Evaluate the arithmetic circuit AC.
 % Takes as input the list of learned probabilities and the number of rule NR,
 %
 % Returns in Probability the evaluated output of the AC


orc(or(A),or(B),or(C)):-
  (B=[and([zero])]->
    C=A
  ;
    (A=[and([zero])]->
      C=B
    ;
      ((A=[and([one])];B=[and([one])])->
        C=[one]
      ;
        append(A,B,C)
      )
    )
  ).

combine_or(A,B,or(C1)):-
  argument_or(A,A1),
  argument_or(B,B1),
  append(A1,B1,C1).

argument_or(or(A),A):-!.

argument_or(A,[A]).

onec(and([one])).

zeroc(and([zero])).

andc(and(A),and(B),and(C)):-!,
  ((A=[zero];B=[zero])->
    %C=and(A,B)
    fail
  ;
    (A=[one]->
      C=B
    ;
      (B=[one]->
        C=A
      ;
        append(A,B,C)
      )
    )
  ).

andc(and(A),or(B),and(C)):-
  ((A=[zero];B=[and([zero])])->
    %C=and(A,B)
    fail
  ;
    (A=[one]->
      C=or(B)
    ;
      (B=[one]->
        C=A
      ;
        append(A,[or(B)],C)
      )
    )
  ).

combine_and(A,B,and(C1)):-
  argument_and(A,A1),
  argument_and(B,B1),
  append(A1,B1,C1).

argument_and(and(A),A):-!.

argument_and(A,[A]).


ac_notc(A,B):-
  (A=or([and([zero])])->
    B=one
  ;
    (A=or([one])->
      B=zero
    ;
      B=not(A)
    )
  ).

equalityc(V,N,V=N).


/**
 * or_list(++ListOfACs:list,++Environment,--AC:int) is det
 *
 * Returns in AC a pointer to a AC belonging to environment Environment
 * representing the disjunction of all the ACs in ListOfACs
 */
or_list([H],H):-!.

or_list([H|T],B):-
  or_list1(T,H,B).


or_list1([],B,B).

or_list1([H|T],B0,B1):-
  orc(B0,H,B2),
  or_list1(T,B2,B1).


/**
 * test_hplp(:P:probabilistic_program,+TestFolds:list_of_atoms,-LL:float,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns the
 * log likelihood of the test examples in LL, the area under the Receiver
 * Operating Characteristic curve in AUCROC, a dict containing the points
 * of the ROC curve in ROC, the area under the Precision Recall curve in AUCPR
 * and a dict containing the points of the PR curve in PR
 */
test_hplp(M:P,TestFolds,LL,AUCROC,ROC,AUCPR,PR):-
  test_hplp_prob(M:P,TestFolds,_NPos,_NNeg,LL,LG),
  compute_areas_diagrams(LG,AUCROC,ROC,AUCPR,PR).



/**
 * test_hplp_prob(:P:probabilistic_program,+TestFolds:list_of_atoms,-NPos:int,-NNeg:int,-LL:float,-Results:list) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns
 * the number of positive examples in NPos, the number of negative examples
 * in NNeg, the log likelihood in LL
 * and in Results a list containing the probabilistic result for each query contained in TestFolds.
 */

test_hplp_prob(M:P,TestFolds,NPos,NNeg,CLL,Results) :-
  write2(M,'Testing\n'),
  findall(Exs,(member(F,TestFolds),M:fold(F,Exs)),L),
  append(L,TE),
  process_clauses(P,M,[],_,[],PRules),
  generate_clauses(PRules,M,RuleFacts,0,[],Th),
  assert_all(Th,M,ThRef),
  assert_all(RuleFacts,M,RFRef),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,[],_,[],RBG),
    generate_clauses(RBG,M,_RBGRF,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef),
    assert_all(ThBG,ThBGRef)
  ;
    true
  ), 
  test_no_area([TE],M,NPos,NNeg,CLL,Results),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ),
  retract_all(ThRef),
  retract_all(RFRef).

/**
 * induce_hplp_par(:TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate learns the parameters of the program stored in the in/1 fact
 * of the input file using the folds indicated in TrainFolds for training.
 * It returns in P the input program with the updated parameters.
 */
induce_hplp_par(M:Folds,P):-
  induce_hplp_parameters(M:Folds,_DB,R),
  rules2termHPLPs(R,P),
  saveHPLP_learned(M,P).



/**
 *  inference_hplp(:Query:atom, +Model:constant -Prob:number)
 * 
 *  Performs inference and returns in Prob  the probability of atom.
 */
inference_hplp(M:Query, Model, Prob):-
  inference_hplp(M:Query, Model,Prob, _Circuit).




/**
 *  inference_hplp(:Query:atom, +Model:constant, -Prob:probabity, -Circuit: Term)
 * 
 *   Performs inference and returns both the probability and the arithmetic circuit of atom in Prob and Circuit respectively. 
 */
inference_hplp(M:Query,Model, Prob,Circuit):-
  Fold=[Model],
  findall(Exs,(member(F,Fold),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,[],_,[],RBG),
    generate_clauses(RBG,M,_RBG1,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef),
    assert_all(ThBG,ThBGRef)
  ;
    true
  ),
  M:in(R00),
  process_clauses(R00,M,[],_,[],R0),
  statistics(walltime,[_,_]),
  generate_clauses(R0,M,R1,0,[],Th0),
  assert_all(Th0,M,Th0Ref),
  assert_all(R1,M,R1Ref),!,
  retractall(M:v(_,_,_)),
  Query=..[Func|Args],
  append(Fold,Args,ArgsNew),
  QueryNew=..[Func|ArgsNew],
  findall(P,M:rule(_R,[_:P|_],_BL,_Lit),LR),
  abolish_all_tables,
  get_node(QueryNew,M,Circuit),!,
  length(LR,NUMBER),
  forward(Circuit,LR,NUMBER,Prob),
  retract_all(Th0Ref),
  retract_all(R1Ref),
  retract_all(ClBGRef),
  retract_all(ThBGRef).




saveHPLP_learned(M,P):- 
  M:local_setting(saveHPLP,Save),
  (Save=yes ->
      M:local_setting(saveFile,SaveFile),
      string_concat(SaveFile,"_Learned",Learned),
      writeClause(P,Learned)
    ;
    true
  ).

rules2termHPLPs(R,T):-
  maplist(rules2termHPLP,R,T).

rules2termHPLP(rule(_N,[H:Par|_],BL,_Lit),(H:Par:-B)):-
list2and(BL,B).

induce_hplp_parameters(M:Folds,DB,R):-
  M:local_setting(seed,Seed),
  set_random(Seed),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),
  statistics(walltime,[_,_]),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,[],_,[],RBG),
    generate_clauses(RBG,M,_RBG1,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef),
    assert_all(ThBG,ThBGRef)
  ;
    true
  ),
  M:in(R00),
  process_clauses(R00,M,[],_,[],R0),
  statistics(walltime,[_,_]),
  learn_params_hplp(DB,M,R0,R,Score2),
  statistics(walltime,[_,CT]),
  CTS is CT/1000,
  format2(M,'Wall time=~f  CLL=~f ~n',[CTS,Score2]),
  write_rules2(M,R,user_output),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ).

 

% Other useful predicates
remove_p([N,_],N).



%!	resetProb
%
%
resetProb(_,[],[]).
resetProb(Min_prob,[Prob|Rest],ProbFinal):-
  (Prob<Min_prob ->
    ProbNew is 0.0
    ;
    ProbNew is Prob
    ),
    ProbFinal=[ProbNew|ProbFinalRest],
    resetProb(Min_prob, Rest, ProbFinalRest).

  
  % Read probabilistic program from file
  read_Program(Filename,Type,P):-
    open(Filename, read, Stream),
    read_file(Stream,[_|P1]),
    removeArgument(P1,Type,P),
    close(Stream).
  
  read_file(Stream,[]) :-
    at_end_of_stream(Stream).
  
  read_file(Stream,[(Y)|L]) :-
    \+ at_end_of_stream(Stream),
    readWord(Stream,X),
    %read(Stream,X),
    %read_clause(Stream,X,[]),
    atom_codes(X,X1),
    atom_string(Sub,"::"),
    atom_codes(Sub,CodSub),
    substring(CodSub,X1),
    %write_to_codes(X2,X1),
    %term_to_atom(X2,X),
    Y=X,
    read_file(Stream,L).
  
  read_file(_Stream,[]) :-
    true.
  
  removeArgument([],_,[]).
  removeArgument([Clause|Rest],Type,[Pout|RestOut]):-
    atomic_list_concat(Subatoms, '::', Clause),
    Subatoms=[Prob1,Clause1],
    term_string(Prob,Prob1),
    term_string(Clause2,Clause1),
    Clause2 =.. List,
    List=[Val,Head,Body],
    removeArg(Head,Type,Head1),
    addProb(Head1,Prob,Head2),
    removeBody(Body,Type,Body1),
    List1=[Val,Head2,Body1],
    Pout=..List1,
    removeArgument(Rest,Type,RestOut).
    %
    %Pout=Head:-Body.
  addProb(Head1,Prob,Head1:Prob).
  removeArg(Pred,Type,PredNew):-
    Pred =.. [Functor|Arg],
    (Type=:=0 ->
      Arg=[_|ArgNew]
      ;
      Arg=[_,_|ArgNew]
    ),
    PredNew =..[Functor|ArgNew].
  
  removeBody(Body,Type,(PredNew,RestR)):-
    Body=(B1,Rest),
    removeArg(B1,Type,PredNew),
    removeBody(Rest,Type,RestR).
  removeBody(Body,Type,PredNew):-
    removeArg(Body,Type,PredNew),!.
  
  readWord(InStream,W):- 
    get_code(InStream,Char), 
    checkCharAndReadRest(Char,Chars,InStream),
    dropLast(Chars,Chars1),
    atom_codes(W,Chars1).
   
  dropLast(List,Result):-
    reverse(List,[_|List1]),
    reverse(List1,Result).
  
  substring(X,S) :-
    append(_,T,S) ,
    append(X,_,T) ,
    X \= [].
  
  
  checkCharAndReadRest(10,[],_):-  !. 
  
  %checkCharAndReadRest(32,[],_):-  !. 
  
  checkCharAndReadRest(-1,[],_):-  !. 
  
  checkCharAndReadRest(end_of_file,[],_):-  !. 
  
  checkCharAndReadRest(Char,[Char|Chars],InStream):- 
    get_code(InStream,NextChar), 
    checkCharAndReadRest(NextChar,Chars,InStream).



take(0, _, []) :- !.
take(0, _, []) :- !.
  take(N, [H|TA], [H|TB]) :-
  N > 0,
  N2 is N - 1,
  take(N2, TA, TB).


writefile(List,FileName):-
 open(FileName,write, Stream),
 writefile1(Stream,List),
 close(Stream).

writefile1(_Stream,[]):-!.
writefile1(Stream,[Head|Tail]):-
   writeln(Stream,Head),
   writefile1(Stream,Tail).
  
delete_AC(_, [], []):-!.
delete_AC(ACs, [Term|Tail],Result):-
    member(Term,ACs),!,
    writeln(Term),
    delete_AC(ACs, Tail, Result).

delete_AC(ACs, [Head|Tail], [Head|Result]):-
    delete_AC(ACs, Tail, Result).

getInitialParameters([],[]):-!.
getInitialParameters([rule(_Number,[_Head:Param,'':_],_Body,_)|Rest],[Param|RestParams]):-
   getInitialParameters(Rest,RestParams).

  

/**
 * learn_params_hplp(+DB:list_of_atoms,+M:atom,+R0:probabilistic_program,-R:probabilistic_program,-Score:float) is det
 *
 * The predicate learns the parameters of the program R0 and returns
 * the updated program in R and the score in Score.
 * DB contains the list of interpretations ids and M the module where
 * the data is stored.
 */
learn_params_hplp(DB,M,R0,R,CLL):-
  generate_clauses(R0,M,R1,0,[],Th0),
  assert_all(Th0,M,Th0Ref),
  assert_all(R1,M,R1Ref),!,
  length(R0,NR),
  retractall(M:v(_,_,_)),
  length(DB,NEx),
  abolish_all_tables,
  M:local_setting(group,G),
  %trace,
  derive_circuit_groupatoms(DB,M,NEx,G,[],Nodes0,0,CLL0,_LE,[]),!,
  maplist(remove_p,Nodes0,Nodes),
  M:local_setting(algorithmType,Algorithm),
  M:local_setting(maxIter_phil,MaxIter),
  M:local_setting(epsilon_deep,EA),
  M:local_setting(epsilon_deep_fraction,ER),
  StopCond=[MaxIter,EA,ER],
  M:local_setting(statistics_folder,Statistics_folder),
  M:local_setting(saveStatistics,Save),
  Folder=[Save,Statistics_folder],
  M:local_setting(zero,ZERO),
  M:local_setting(setSeed,Seeded),
  (Seeded=yes ->
    M:local_setting(c_seed,Seed)
  ;
   Seed is 0
  ),
  M:local_setting(useInitParams,Init),
  (Init=yes ->
    getInitialParameters(R0,InitParameters),
    Params=[NR,ZERO,Seeded,Seed,Init,InitParameters]
    ;
    Params=[NR,ZERO,Seeded,Seed,Init]
  ),
  format3(M,'Initial CLL on PHIL ~f */~n',[CLL0]),
  retract_all(Th0Ref),
  retract_all(R1Ref),
  % Regularized parameters
  M:local_setting(regularized,Regularized),
  
  (Regularized=yes ->
    M:local_setting(regularizationType,TypeReg),
    (TypeReg=:=0 ->
      RegParams=[no]
      ;
      M:local_setting(gamma,Gamma),
      M:local_setting(gammaCount,GammaCount),
      RegParams=[Regularized,TypeReg,Gamma,GammaCount]
    )
  ;
    RegParams=[Regularized]
  ),
  (Algorithm = emphil ->
     emphil(Nodes,Params,RegParams,StopCond,Folder,CLL,ProbFinal)
    ;
     (Algorithm = dphil ->
        M:local_setting(adam_params,Adam),
        M:local_setting(max_initial_weight,MAX_W),
        AdamReg=[Adam,RegParams],
        dphil_C(M,Nodes,Params,StopCond,Folder,AdamReg,MAX_W,CLL,ProbFinal)
        ;
        format("The algorithm ~w does not exist. Do you mean dphil or emphil? ~n",[Algorithm]),
        halt
     )
  ),
  format3(M,'~nFinal CLL ~f ~n',[CLL]),
  (Regularized=yes ->
    update_theory_par(R1,ProbFinal,R2),
    M:local_setting(min_probability,Min_prob),
    remove_clauses(R2,Min_prob,R21,Num),
    %format("~d clauses removed",[Num]),
    (Num=:=0 ->    % if no rules was removed
      R=R21
      ;
      R1=[Rule|_], % I have removed some rules so have to insure the program remains hierarchical
      Rule1=(Rule,_),
      getHead(Rule1,HeadFunctor),
      removeHidden(R21,R,[HeadFunctor],_)
    )
  ;
   update_theory_par(R1,ProbFinal,R)
  ).

dphil_C(M,NodesNew,Params,StopCond,Folder,AdamReg,MAX_W,CLL,ProbFinal):-
   M:local_setting(batch_strategy,minibatch(BatchSize)),!,
   Params2=[minibatch,BatchSize,MAX_W],
   dphil(NodesNew,Params,StopCond,Folder,AdamReg,Params2,CLL,ProbFinal).

dphil_C(M,NodesNew,Params,StopCond,Folder,AdamReg,MAX_W,CLL,ProbFinal):-
   M:local_setting(batch_strategy,stoch_minibatch(BatchSize)),!,
   Params2=[stochastic,BatchSize,MAX_W],
   dphil(NodesNew,Params,StopCond,Folder,AdamReg,Params2,CLL,ProbFinal).

dphil_C(M,NodesNew,Params,StopCond,Folder,AdamReg,MAX_W,CLL,ProbFinal):-
   M:local_setting(batch_strategy,batch),!,
   BatchSize is 0,
   Params2=[batch,BatchSize,MAX_W],
   dphil(NodesNew,Params,StopCond,Folder,AdamReg,Params2,CLL,ProbFinal).

%-----------------------------End parameter learning PHIL --------------------------------


  
 %----------------------Start HPLPs structure------------------


%!	induce
%
%
induce_hplp(M:TrainFolds,P):-
  induce_hplp_rules(M:TrainFolds,P0),
  rules2termHPLPs(P0,P),
  saveHPLP_learned(M,P).

induce_hplp_rules(M:Folds,R):-
  set_hplp(M:compiling,on),
  M:local_setting(seed,Seed),
  set_random(Seed),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,[],_,[],RBG),
    generate_clauses(RBG,M,_RBG1,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ThBG,M,ThBGRef),
    assert_all(ClBG,M,ClBGRef)
  ;
    true
  ),
  length(DB,NMegaEx),
  M:local_setting(megaex_bottom, NumMB),
  %trace,
  (NMegaEx >= NumMB ->
      true
    ;
      format2(M,"~nWARN: Number of required bottom clauses is greater than the number of training examples!~n. The number of required bottom clauses will be equal to the number of training examples", []),
      set_hplp(M:megaex_bottom, NMegaEx)
  ),
  statistics(walltime,[_,_]),
  M:local_setting(megaex_bottom,MB),
  deduct(MB,M,DB,[],InitialTheory),
  remove_duplicates(InitialTheory,R1),
  genHPLP(M,R1,HPLP),
  learn_params_hplp(DB,M,HPLP,R,CLL),
  statistics(walltime,[_,WT]),
  WTS is WT/1000,
  %write2(M,'\n\n'),
  format2(M,' HPLP Final score ~f~n',[CLL]),
  format2(M,'Time=~f ~n',[WTS]),
  set_hplp(M:compiling,off),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ).

writeParams(M,InitProb,Rate, MaxLayer,GenerateBottom):-
  format2(M, "Hyper-parameters for generating the hierachical PLP \n \n", []),
  (GenerateBottom=yes ->
    format2(M, "Number of bottom clauses used: all \n", [])
    ;
  format2(M, "Number of bottom clauses used: one \n", [])

  ),
  format2(M, 'Initial Probability=~f ~n',[InitProb]),
  format2(M, 'Rate=~f ~n',[Rate]),
  Temp is -1,
  (MaxLayer=:=Temp ->
    format2(M, 'MaxLayer= no limit ~n',[])
    ;
    format2(M, 'MaxLayer=~d ~n',[MaxLayer])
  ).

genHPLP(M,Bottoms,HPLP):-
  (Bottoms=[] ->
    format("No Bottom has been generated~n",[]),
    halt
    ;
    Bottoms=[Rule1|_Rest]
  ),
  M:local_setting(probability,InitProb),
  M:local_setting(rate,Rate),
  M:local_setting(max_layer,Max1),
  M:local_setting(use_all_mega_examples,GenerateBottom),
  M:local_setting(unifyRoot,UnifyRoot),
  Temp is -1,
  (Max1=:=Temp ->
     MaxLayer is 2147000000,
     writeParams(M,InitProb,Rate, Temp,GenerateBottom)
    ;
     MaxLayer is Max1,
     writeParams(M,InitProb,Rate, MaxLayer,GenerateBottom)
  ),
  getHead(Rule1,HeadFunctor),
  Prob is InitProb,
  assert(getRate(Rate)),
  (GenerateBottom=yes ->
    BottomsNew=Bottoms
    ;
    Bottoms=[Head|_],
    BottomsNew=[Head]
  ),
  (UnifyRoot=yes ->
      maplist(unification(Rule1),BottomsNew)
    ;
    true
  ),
  getTrees(BottomsNew,Trees),
  Trees1=[t(head,Trees)],
  generateHPLPs(-1,HeadFunctor,MaxLayer,Prob,Trees1,HPLPs,Levels),
  M:local_setting(saveHPLP,Save),
  (Save=yes ->
      M:local_setting(saveFile,SaveFile),
      saveHPLPs(HPLPs,SaveFile,Levels)
    ;
    true
  ),
  HPLPs=[HPLP|_],
  retractall(getRate(_Rate)).



getHead((rule(_,[Head:_,_],_,_),_),HeadName):-
    functor(Head,HeadName,_).

selectHead(HeadName,[Rule|Rest],HeadRulesCur,RestCur,RulesHead):-
  Rule=rule(_N,[Head:_|_],_,_),
  functor(Head,HeadName,_),
  append(HeadRulesCur,[Rule],HeadRulesCurNew),
  selectHead(HeadName, Rest, HeadRulesCurNew, RestCur, RulesHead).

selectHead(_,[Rule|Rest],HeadRulesCur,[Rule|Rest],HeadRulesCur):-!.


remove_clauses(Rules,Prob,RulesOut,Num):-
  remove_clauses_loop(Rules,Prob,0,Num,[],RulesOut).

remove_clauses_loop([],_,Num,Num,Rules,Rules):-!.
remove_clauses_loop([Rule|Rest],Prob,NumCur,Num,RulesCur,RulesOut):-
  Rule=rule(_N,[_Head:Par|_],_,_),
  (Par < Prob ->
    NumCur1 is NumCur+1,
    remove_clauses_loop(Rest, Prob, NumCur1,Num,RulesCur, RulesOut)
    ;
    append(RulesCur,[Rule],RulesCurNew),
    remove_clauses_loop(Rest, Prob, NumCur,Num,RulesCurNew, RulesOut)
  ).


unification(Rule1,Rule2):-
   Rule1=(rule(_,[Head1:_,_],_,_),_),
   Rule2=(rule(_,[Head2:_,_],_,_),_),
   Head1=Head2.
 % unify_with_occurs_check(Head1,Head2).

%------------------Create a tree from Bottom clauses------------------------
getTrees([],[]):-!.
getTrees([(rule(_,[Head:_,_],_,Body),_)|RestBottom],[Tree|RestTrees]):-
  functor(Head,_N, A),
  (A=:=0 ->
      getTree(Body,[],BodyTrees),
      Tree=t(Head,BodyTrees)
    ;
    %trace,
    Bod=[Head|Body],
    getTree(Bod,[],BodyTrees),
    length(BodyTrees, LTree),
    (LTree=:=0 ->
      Tree=[]
      ;
      (LTree=:=1 ->
        BodyTrees=[Tree]
        ;
        BodyTrees=[H1|B1],
        Tree=t(H1,B1)
      )
    )
  ),
%getTree(Body,[],BodyTrees),
getTrees(RestBottom, RestTrees).


%!	getTree
%
%
getTree([],Trees,Trees):-!.
getTree([Pred|RestPred],TreesCurr,Treesresult):-
insert_tree(Pred,[],TreesCurr,TreesCurr1),
getTree(RestPred,TreesCurr1,Treesresult).


%!	insert_tree
%
%
insert_tree(Pred,[],[],[t(Pred,[])]):-!.
insert_tree(Pred,PreviousTrees,[],TreesResult):-!,
append_No_empty(PreviousTrees,[t(Pred,[])],TreesResult).

insert_tree(Pred1,Previoustrees,[Tree|RestTreesCurr],TreesResult):-
insert_tree_loop(Pred1,Tree,TreeResultLoop),!,  % attento a questa cut
append_No_empty(Previoustrees,[TreeResultLoop],TreesResult1),
append_No_empty(TreesResult1,RestTreesCurr,TreesResult).

insert_tree(Pred1,Previoustrees,[Tree|RestTreesCurr],TreesResult):-
append_No_empty(Previoustrees,[Tree],PreviousTreesCurr),
insert_tree(Pred1,PreviousTreesCurr,RestTreesCurr,TreesResult).


%!	insert_tree_loop
%
%
insert_tree_loop(Pred1,t(Pred2,Forest),TreeResult):-
  ischild(Pred1,Pred2),!,
  insert_Forest(Pred1,t(Pred2,Forest),TreeResult).
  
  insert_tree_loop(Pred1,t(Pred2,Forest),TreeResult):-
  insert_tree_loop_Forest(Pred1,[],Forest,NewForest),
  TreeResult=t(Pred2,NewForest).


%!	insert_Forest
%
%
insert_Forest(Pred1,t(Pred2,Forest),TreeResult):-
  append_No_empty(Forest,[t(Pred1,[])],Forest1),
  TreeResult=t(Pred2,Forest1).


%!	insert_tree_loop_Forest
%
%
insert_tree_loop_Forest(_Pred,_AccForest,[],_ForestResult):-fail.
insert_tree_loop_Forest(Pred,PreviousForest,[ForestCurr|RestForest],ForestResult):-
  append_No_empty(PreviousForest,[ForestCurr],PreviousForestCurr),
  insert_tree_loop_Forest(Pred,PreviousForestCurr, RestForest, ForestResult).
  

insert_tree_loop_Forest(Pred,PreviousForest,[ForestCurr|RestForest],ForestResult):-
  insert_tree_loop(Pred,ForestCurr,NewForest),!,
  append_No_empty(PreviousForest,[NewForest],ForestResult1),
  append_No_empty(ForestResult1,RestForest,ForestResult).



% Append a list Whitout considering the empty lits 
append_No_empty([],[],[]).
append_No_empty(List,[],List):-!.
append_No_empty([],List,List):-!.
append_No_empty(List1,List2,List):-!,
append(List1,List2,List).


ischild(Pred1,Pred2):-
  getVariableArgument(Pred1,VarPr1),
  getVariableArgument(Pred2,VarPr2),
  inter(VarPr1,VarPr2,Inter),
  length(Inter,L),
  L >0.


%! getVariableArgument(+Predicate:Term,-VarArguments:list)
%Given a predicate Term, return in VarArguments the list of variable argument %of Term.

getVariableArgument(Predicate,VarArguments):-
  Predicate=..List,
  List=[_Functor|Arguments],
  remove_constants(Arguments,VarArguments).
  

/**
 * remove_constantss(+List1:list,-List2:list) is det
 *
 * Removes constants form list List1. List2 returns only variables
 */
remove_constants(L0, L) :-
  remove_constants(L0, [], L1),
  reverse(L1, L).

remove_constants([], L, L).

remove_constants([H|T], L0, L) :-
  nonvar(H), !,
  remove_constants(T, L0, L).

remove_constants([H|T], L0, L) :-
  remove_constants(T, [H|L0], L).


inter([], _, []).
inter([H1|T1], L2, [H1|Res]) :-
  memberVar(H1, L2),
  inter(T1, L2, Res).
inter([_|T1], L2, Res) :-
  inter(T1, L2, Res).


memberVar(X, [Y|T]) :-
  (   X==Y
  ;   memberVar(X, T)
  ).

%------------------End tree creation------------------------


%---------------Generate HPLPs from trees-----------

generateHPLPs(_,_,_,_,[],[],[]):-!.
generateHPLPs(InitLevel,HeadFunctor,MaxLayer,Prob,[Tree|RestTrees],[HPLP|RestHPLs],[Level|RestLevel]):-
  Tree=t(Head,Forest),
  assert(getIndex(1)),
  generateHPLPloop(MaxLayer,1,InitLevel,[h([Head,'',Prob],Forest)],[],HPLP1,0,Level),
  retractall(getIndex(_Index1)),
  %trace,
  removeHidden(HPLP1,HPLP11,[HeadFunctor],_),
  reduce([],HPLP11,0,[],HPLP),
  %HPLP=HPLP11,
  %reduceprogram(Head,HPLP1,HPLP),
  generateHPLPs(InitLevel,HeadFunctor,MaxLayer,Prob,RestTrees,RestHPLs,RestLevel).


/*
generateHPLPs(MaxLayer,Prob,Tree,HPLP,Level):-
Tree=t(Head,Forest),
generateHPLPloop(MaxLayer,1,0,[h([Head,'',Prob],Forest)],[],HPLP1,0,Level),
removeHidden(HPLP1,HPLP,[Head],_).
*/


generateHPLPloop(MaxLayer,_,MaxLayer,_,HPLPs,HPLPs,_,MaxLayer):-!.
generateHPLPloop(_,_,Level,[],HPLPs,HPLPs,_,Level):-!.
generateHPLPloop(MaxLayer,Count,Level,[Head|Rest],HPLPs,HPLPsR,NumRule,LevelOut):-
Head=h([Hidden,PathIndex,Prob],Forest),
generateHPLPloopCurr(Hidden,PathIndex,Prob,1,Forest,Rest,RestNew,HPLPs,HPLPsNew,NumRule,NumRuleNew),
Count1 is Count-1,
(Count1=:=0 ->
    length(RestNew,CountNew),
    LevelNew is Level +1
  ;
  LevelNew is Level,
  CountNew is Count1
  ),
generateHPLPloop(MaxLayer,CountNew,LevelNew,RestNew,HPLPsNew,HPLPsR,NumRuleNew,LevelOut).


generateHPLPloopCurr(_,_,_,_,[],Forest,Forest,HPLPs,HPLPs,NumR,NumR):-!.
generateHPLPloopCurr(Hidden,PathIndex,Prob,Index,[t(Pred2,ForestChild)|RestForest],AccForest,ForestResult,HPLPs,HPLPsR,NumR,NumResult):-
  %write(Stream,Hidden),
  %write(Stream,":0.5:-"),
  %write(Stream,Pred2),
  (maybe(Prob) ->
      Head=[Hidden:0.5,'':0.5],
      (ForestChild=[] ->
            Temp=[],
            Body=[Pred2],
            IndexNew is Index
        ;
        (Hidden=head ->
            %IndexNew is Index,
            Temp=[h([Pred2,'',Prob],ForestChild)]
          ;
            %write(Stream,","),
            atom_concat(PathIndex,'_',PathIndex1),
            (( functor(Hidden, Name, _),atom_concat(hidden,_,Name)) ->
               atom_concat(PathIndex1,Index,PathIndexNew)
            ;
                getIndex(Index1),
                atom_concat(PathIndex1,Index1,PathIndexNew),
                retractall(getIndex(_Index1)),
                Index1New is Index1 +1,
                assert(getIndex(Index1New))
                
            ),
            %atom_concat(PathIndex1,Index,PathIndexNew),
            getHidden(Hidden,Pred2,PathIndexNew,HiddenNew),
            %trace,
            getRate(Rate),
            ProbNew is Prob*Rate,
            Temp=[h([HiddenNew,PathIndexNew,ProbNew],ForestChild)],
            Body=[Pred2,HiddenNew]
            %write(Stream,HiddenNew),
            %IndexNew is Index+1
         ),
         IndexNew is Index+1   
        ),
       append(AccForest,Temp,AccForestNew),
      (Hidden=head ->
          R=[],
          NumRNew is NumR
        ;
        R=[rule(NumR,Head,Body,true)],
        NumRNew is NumR+1
      ),
      append(HPLPs,R,HPLPsNew),   
      generateHPLPloopCurr(Hidden,PathIndex,Prob,IndexNew,RestForest,AccForestNew,ForestResult,HPLPsNew,HPLPsR,NumRNew,NumResult)
    ;
      generateHPLPloopCurr(Hidden,PathIndex,Prob,Index,RestForest,AccForest,ForestResult,HPLPs,HPLPsR,NumR,NumResult)
    ).
  
getHidden(Hidden,Pred2,PathIndex,HiddenNew):-
getVariableArgument(Hidden,Var1),
getVariableArgument(Pred2,Var2),
unionVar(Var1,Var2,UnionVar),
atom_concat(hidden,PathIndex,HiddenNew1),
HiddenNew=..[HiddenNew1|UnionVar].

unionVar([],ListVar,ListVar):-!.
unionVar(ListVar,[],ListVar):-!.

unionVar([Var1|R1],List2,Rest):-
  memberVar(Var1,List2),!,
  unionVar(R1, List2, Rest).

unionVar([Var1|R1],List2,[Var1|Rest]):-
  unionVar(R1, List2, Rest).

%--------------End HPLPS generation--------------------------


% --------Reduce the initial HPLP---------- 
reduce(_ACC,[],_Count,Rule,Rule):-!.
reduce(Acc,[Rule2|RestRule],Count,RuleCur,Rule):-
  (Rule2=rule(_Num,_Head,[_Body1,_Body2],_Val) ->
     %R=rule(Count,Head,[Body1,Body2],Val),
     copy_term(Rule2, Rule2_copy),
     append(RuleCur,[Rule2_copy],RuleCur1),
     %append(RuleCur,[R],RuleCur1),
     Count1 is Count+1,
     reduce(Acc,RestRule,Count1,RuleCur1,Rule)
   ;
    Rule2=rule(_,[Head1,_],[Pred2],_Val),
    Head1=Head:_Prob,
    functor(Pred2,Name,Arity),
    New=(Head,Name,Arity),
    (member(New,Acc)->
       reduce(Acc,RestRule,Count,RuleCur,Rule)
    ;
       append(Acc,[New],AccNew),
       %R=rule(Count,[Head1,Head2],[Pred2],Val),
       copy_term(Rule2, Rule2_copy),
       append(RuleCur,[Rule2_copy],RuleCur1),
       Count1 is Count+1,
       reduce(AccNew,RestRule,Count1,RuleCur1,Rule)
    )
  ).
  

match(rule(_,Head,[Pred1],_),rule(_,Head,[Pred2],_)):-
  functor(Pred1,Name,Arity),
  functor(Pred2,Name,Arity).
  

removeHidden([],[],Hidden,Hidden):-!.
removeHidden([Rule|Rest],HPLPOut,HiddenCur,HiddenR):-
 Rule=rule(N,Head,[Pred,Hidden],True),
 (exist1(Hidden,Rest) ->
  functor(Hidden,Name,_),
  append(HiddenCur,[Name],HiddenCurNew),
  removeHidden(Rest,HPLPOutLoop,HiddenCurNew,HiddenR),!,
  HPLPOut=[Rule|HPLPOutLoop]
  ;
   removeHidden(Rest,HPLPOutLoop,HiddenCur,HiddenR),!,
   HPLPOut=[rule(N,Head,[Pred],True)|HPLPOutLoop]
).

removeHidden([Rule|Rest],HPLPOut,HiddenCur,HiddenR):-
Rule=rule(_,Head,[Pred],_),
Head=[Hidden:_,_],
functor(Hidden,Name,_),
(memberVar(Name,HiddenCur) ->
    HPLPOut=[Rule|HPLPOutLoop]
  ;
    (isHiddenPredicate(Pred) ->
        HPLPOut=[Rule|HPLPOutLoop]
      ;
        HPLPOut=HPLPOutLoop
    )
  ),
removeHidden(Rest,HPLPOutLoop,HiddenCur,HiddenR).

exist1(_,[]):-fail,!.
exist1(Hidden,[rule(_,Head,_,_)|_Rest]):-
Head=[Hidden:_,_],!.

exist1(Hidden,[_|Rest]):-
exist1(Hidden,Rest).

isHiddenPredicate(Hidden):-
  functor(Hidden,Name,_),
  atom_concat(hidden,_,Name).
% -------End Reduction the initial HPLP----------

%----------------------------Save HPLPs in a file------------------------------------

saveHPLPs(HPLPs,FileName,Levels):-
  open(FileName,write, Stream),
  %saveHPLPsLoop(Stream,1,HPLPs,Nodes_Heigths),
  length(Levels,NumHPLPs),
  numlist(1,NumHPLPs, Numbers),
  maplist(saveHPLP(Stream), Numbers,HPLPs,Levels),
  close(Stream).
  
  saveHPLPsStream(HPLPs,Stream,Levels):-
    %saveHPLPsLoop(Stream,1,HPLPs,Nodes_Heigths),
    length(Levels,NumHPLPs),
    numlist(1,NumHPLPs, Numbers),
    maplist(saveHPLP(Stream), Numbers,HPLPs,Levels).
  
  /*
  saveHPLPsLoop(_,_,[],[]).
  saveHPLPsLoop(Stream,Iter,[HPLP|RestHPLPs],[(Nodes,Height)|Rest]):-
  format(Stream,"HPLP Number ~d: Number of nodes=~d  Height=~d ~n",[Iter,Nodes,Height]),
  copy_term(HPLP,HPLPCopy),
  numbervars((HPLPCopy,_HPLPVar),0,_V),
  saveHPLP(Stream,HPLPCopy),
  (RestHPLPs=[]->
    true
    ;
    nl(Stream),
    nl(Stream)
    ),
  Iter1 is Iter+1,
  saveHPLPsLoop(Stream, Iter1, RestHPLPs,Rest).
  */
  
  saveHPLP(Stream,Number,HPLP,Level):-
  %Params=(Nodes,Height),
  format(Stream,"HPLP ~d: Height=~d ~n",[Number,Level]),
  copy_term(HPLP,HPLPCopy),
  numbervars((HPLPCopy,_HPLPVar),0,_V),
  %saveHPLPLoop(Stream,HPLPCopy),
  maplist(saveHPLPLoop(Stream),HPLPCopy),
  nl(Stream),
  nl(Stream). 
  
  saveHPLPLoop(Stream,rule(_,[Head,_],Body,_)):-
  write(Stream,Head),
  write(Stream,":-"),
  %flush_output(Stream),
  writeBody(Stream,Body).
  
  /*
  saveHPLPLoop(_,[]).
  saveHPLPLoop(Stream,[rule(_,[Head,_],Body,true)|RestRule]):-
  write(Stream,Head),
  write(Stream,":-"),
  writeBody(Stream,Body),
  saveHPLPLoop(Stream,RestRule).
  */
  
  writeBody(_,[]):-!.
  writeBody(Stream,[Body1|RestBody]):-
  write(Stream,Body1),
  (RestBody=[] ->
      writeln(Stream,".")
    ;
      write(Stream,",")
    ),
  writeBody(Stream, RestBody).
  
  %--------------End save HPLPs------------------------------------


%-------------------Other useful predicates--------------------------

saveTrees(Bottoms,FileName):-
  open(FileName,write, Stream),
  saveTrees1(Stream,1,Bottoms),
  close(Stream).
  
  saveTrees1(_,_,[]).
  saveTrees1(Stream,Iter,[Hplp|Rest]):-
  nnodes(Hplp,Number),
  height(Hplp,Height),
  format(Stream,"HPLP N°~d: Number of nodes=~d; Height=~d ~n",[Iter,Number,Height]),
  copy_term(Hplp,HplpCopy),
  numbervars((HplpCopy,_HplpVar),0,_V),
  writeTreeloop(Stream,0,HplpCopy),
  length(Rest,LR),
  (LR=:=0->
    true
    ;
    nl(Stream),
    nl(Stream)
    ),
  Iter1 is Iter+1,
  saveTrees1(Stream,Iter1,Rest).
  
  
  writeTrees(Trees,FileName):-
  open(FileName,write, Stream),
  writeTrees1(Stream,Trees),
  close(Stream).
  
  writeTrees1(_Stream, []).
  writeTrees1(Stream,[HeadTree|RestTree]):-
  writeTreeloop(Stream,0,HeadTree),
  nl(Stream),
  nl(Stream),
  writeTrees1(Stream, RestTree).
  
  
  writeTreeloop(Stream,Iter,t(Pred,Forest)):-
  tab(Stream,Iter),
  %write(Stream,"|"),
  writeln(Stream,Pred),
  IterCurr is 2+Iter,
  writeForest(Stream,IterCurr,Forest).
  
  writeForest(_Stream,_Iter,[]).
  writeForest(Stream,Iter,[HeadForest|RestForest]):-
  writeTreeloop(Stream,Iter,HeadForest),
  writeForest(Stream,Iter,RestForest).
  
  
  
%    Some useful predicate on Trees
  

%    Count the Number of Node in a tree
  
  nnodes(t(_,F),N) :- nnodes(F,NF), N is NF+1.
  nnodes([],0).
  nnodes([T|Ts],N) :- nnodes(T,NT), nnodes(Ts,NTs), N is NT+NTs.
  
  
%    True if its arguments is a tree
  
  istree(t(_,F)):-isforest(F).
  isforest([]).
  isforest([T|Ts]) :- istree(T), isforest(Ts).
  
  
%    add a child to and existing tree
  
  
  addChild(t(Pred,Forest),NewChild,t(Pred,NewForest)):-
  append_No_empty(Forest,NewChild,NewForest).
  
%    Return the height of a tree
  
  height(Tree,H):-
  tree_depth(Tree,Max),
  H is Max-1.
  tree_depth(nil,0) .   % the depth of an empty tree is 0.
  tree_depth(t(_,Forest),D) :-  % the depth of a non-empty tree is computed thus:
  depth_max(Forest,0,Max),
  D is 1 + Max.             % - the overall depth is 1 more than the maximum depth in the forest.
  
  depth_max([],Max,Max).
  depth_max([Child|RestForest],MaxCurr,Max):-
  tree_depth(Child,MaxChild),
  (MaxChild > MaxCurr ->
    depth_max(RestForest,MaxChild,Max)
    ;
  depth_max(RestForest,MaxCurr,Max)
    ).
  
  
  %!	writeList
  % Write the list List (of terms) in the file FileName
  %
  
  writeList(List,FileName):-
  open(FileName,write, Stream),
  writeList1(Stream,List),
  close(Stream).
  writeList1(_,[]).
  writeList1(Stream,[HeadList|RestList]):-
  writeln(Stream,HeadList),
  writeList1(Stream,RestList).
  
  
  %!	writeClause
  % Write the list List (of terms) in the file FileName
  %
  
  writeClause(List,FileName):-
    open(FileName,write, Stream),
    copy_term(List,ListCopy),
    numbervars((ListCopy,_ListVar),0,_V),
    writeln(Stream,"Learned Program"),
    writeClause1(Stream,ListCopy),
    close(Stream).
  
  
  writeClauseOutput(List):-
    copy_term(List,ListCopy),
    numbervars((ListCopy,_ListVar),0,_V),
    writeln(user_output,"Learned Program"),
    writeClause1(user_output,ListCopy).
  
  writeClause1(_,[]).
  writeClause1(Stream,[HeadList|RestList]):-
    write(Stream,HeadList),
    writeln(Stream,"."),
    writeClause1(Stream,RestList).


  /*update_theory_par([],[],[]).

update_theory_par([(Head:_:-Body)|T0],[Par|ParT],[(Head:Par:-Body)|T]):-
  update_theory_par(T0,ParT,T).
*/

update_theory_par([],[],[]).

update_theory_par([rule(N,[H:_,'':_],B,L)|T0],[Par|ParT],
  [rule(N,[H:Par,'':P0],B,L)|T]):-
  P0 is 1-Par,
  update_theory_par(T0,ParT,T).


update_theory(R,initial,R):-!.

update_theory([],_Par,[]).

update_theory([def_rule(H,B,L)|T0],Par,[def_rule(H,B,L)|T]):-!,
  update_theory(T0,Par,T).

update_theory([(H:-B)|T0],Par,[(H:-B)|T]):-!,
  update_theory(T0,Par,T).

update_theory([rule(N,H,B,L)|T0],Par,[rule(N,H1,B,L)|T]):-
  member([N,P],Par),!,
  reverse(P,P1),
  update_head_par(H,P1,H1),
  update_theory(T0,Par,T).

update_head_par([],[],[]).

update_head_par([H:_P|T0],[HP|TP],[H:HP|T]):-
  update_head_par(T0,TP,T).

%----------------------End HPLPs structure------------------





%---- derive the aritmetic circuits from the examples and the program--------
derive_circuit_groupatoms_output_atoms([],_M,_O,_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_circuit_groupatoms_output_atoms([H|T],M,O,E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-
  generate_goal(O,M,H,[],GL),
  

    CardEx is 1.0
  ,
  get_node_list_groupatoms(GL,M,ACs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,ACs,Nodes1),
  derive_circuit_groupatoms_output_atoms(T,M,O,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).


derive_circuit_groupatoms([],_M,_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_circuit_groupatoms([H|T],M,E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-
  get_output_atoms(O,M),
  generate_goal(O,M,H,[],GL),
  CardEx is 1.0,
  %sample_hplp(1000,GL,GLTemp,_),
  get_node_list_groupatoms(GL,M,ACs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,ACs,Nodes1),
  derive_circuit_groupatoms(T,M,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).

get_node_list_groupatoms([],_M,[],_CE,_Gmax,CLL,CLL,LE,LE).

get_node_list_groupatoms([H|T],M,[[AC1,CE]|ACT],CE,Gmax,CLL0,CLL,[H|LE0],LE):-
  get_node(H,M,AC1), 		%creates the AC for atom ,
  CLL2 is CLL0,
  get_node_list_groupatoms(T,M,ACT,CE,Gmax,CLL2,CLL,LE0,LE).

%---- End aritmetic circuits derivation--------
 
 
 % -- Compute the areas under the ROC and PR curve
compute_prob([],[],[],Pos,Pos,Neg,Neg).

compute_prob([\+ HE|TE],[HP|TP],[P- (\+ HE)|T],Pos0,Pos,Neg0,Neg):-!,
  P is 1-HP,
  Neg1 is Neg0+1,
  compute_prob(TE,TP,T,Pos0,Pos,Neg1,Neg).

compute_prob([ HE|TE],[HP|TP],[HP-  HE|T],Pos0,Pos,Neg0,Neg):-
  Pos1 is Pos0+1,
  compute_prob(TE,TP,T,Pos1,Pos,Neg0,Neg).


compute_aucpr(L,Pos,Neg,A):-
  L=[P_0-E|TL],
  (E= (\+ _ )->
    FP=1,
    TP=0,
    FN=Pos,
    TN is Neg -1
  ;
    FP=0,
    TP=1,
    FN is Pos -1,
    TN=Neg
  ),
  compute_curve_points(TL,P_0,TP,FP,FN,TN,Points),
  Points=[R0-P0|_TPoints],
  (R0=:=0,P0=:=0->
    Flag=true
  ;
    Flag=false
  ),
  area(Points,Flag,Pos,0,0,0,A).

compute_curve_points([],_P0,TP,FP,_FN,_TN,[1.0-Prec]):-!,
  Prec is TP/(TP+FP).

compute_curve_points([P- (\+ _)|T],P0,TP,FP,FN,TN,Pr):-!,
  (P<P0->
    Prec is TP/(TP+FP),
    Rec is TP/(TP+FN),
    Pr=[Rec-Prec|Pr1],
    P1=P
  ;
    Pr=Pr1,
    P1=P0
  ),
  FP1 is FP+1,
  TN1 is TN-1,
  compute_curve_points(T,P1,TP,FP1,FN,TN1,Pr1).

compute_curve_points([P- _|T],P0,TP,FP,FN,TN,Pr):-!,
  (P<P0->
    Prec is TP/(TP+FP),
    Rec is TP/(TP+FN),
    Pr=[Rec-Prec|Pr1],
    P1=P
  ;
    Pr=Pr1,
    P1=P0
  ),
  TP1 is TP+1,
  FN1 is FN-1,
  compute_curve_points(T,P1,TP1,FP,FN1,TN,Pr1).

area([],_Flag,_Pos,_TPA,_FPA,A,A).

area([R0-P0|T],Flag,Pos,TPA,FPA,A0,A):-
 TPB is R0*Pos,
  (TPB=:=0->
    A1=A0,
    FPB=0
  ;
    R_1 is TPA/Pos,
    (TPA=:=0->
      (Flag=false->
        P_1=P0
      ;
        P_1=0.0
      )
    ;
      P_1 is TPA/(TPA+FPA)
    ),
    FPB is TPB*(1-P0)/P0,
    N is TPB-TPA+0.5,
    interpolate(1,N,Pos,R_1,P_1,TPA,FPA,TPB,FPB,A0,A1)
  ),
  area(T,Flag,Pos,TPB,FPB,A1,A).

interpolate(I,N,_Pos,_R0,_P0,_TPA,_FPA,_TPB,_FPB,A,A):-I>N,!.

interpolate(I,N,Pos,R0,P0,TPA,FPA,TPB,FPB,A0,A):-
  R is (TPA+I)/Pos,
  P is (TPA+I)/(TPA+I+FPA+(FPB-FPA)/(TPB-TPA)*I),
  A1 is A0+(R-R0)*(P+P0)/2,
  I1 is I+1,
  interpolate(I1,N,Pos,R,P,TPA,FPA,TPB,FPB,A1,A).
% End Areas computation


randomize([],[]):-!.

randomize([rule(N,V,NH,HL,BL,LogF)|T],[rule(N,V,NH,HL1,BL,LogF)|T1]):-
  length(HL,L),
  Int is 1.0/L,
  randomize_head(Int,HL,0,HL1),
  randomize(T,T1).

randomize_head(_Int,['':_],P,['':PNull1]):-!,
  PNull is 1.0-P,
  (PNull>=0.0->
    PNull1 =PNull
  ;
    PNull1=0.0
  ).

randomize_head(Int,[H:_|T],P,[H:PH1|NT]):-
  PMax is 1.0-P,
  random(0,PMax,PH1),
  P1 is P+PH1,
  randomize_head(Int,T,P1,NT).



update_head([],[],_N,[]):-!.

update_head([H:_P|T],[PU|TP],N,[H:P|T1]):-
  P is PU/N,
  update_head(T,TP,N,T1).

/* utilities */
/**
 * rules2terms(:R:list_of_rules,-T:tern) is det
 *
 * The predicate translates a list of rules from the internal
 * representation format (rule/4 and def_rule/3) to the
 * LPAD syntax.
 */
/*
rules2terms(R,T):-
  maplist(rule2term,R,T).

rule2term(rule(_N,HL,BL,_Lit),(H:-B)):-
  list2or(HL,H),
  list2and(BL,B).

rule2term(def_rule(H,BL,_Lit),((H:1.0):-B)):-
  list2and(BL,B).
*/

write_rules([],_S).

write_rules([rule(_N,HL,BL,Lit)|T],S):-!,
  copy_term((HL,BL,Lit),(HL1,BL1,Lit1)),
  numbervars((HL1,BL1,Lit1),0,_M),
  write_disj_clause(S,(HL1:-BL1)),
  write_rules(T,S).

write_rules([def_rule(H,BL,Lit)|T],S):-
  copy_term((H,BL,Lit),(H1,BL1,Lit1)),
  numbervars((H1,BL1,Lit1),0,_M),
  write_disj_clause(S,([H1:1.0]:-BL1)),
  write_rules(T,S).


new_par([],[],[]).

new_par([HP|TP],[Head:_|TO],[Head:HP|TN]):-
  new_par(TP,TO,TN).



write_disj_clause(S,(H:-[])):-!,
  write_head(S,H),
  format(S,".~n~n",[]).

write_disj_clause(S,(H:-B)):-
  write_head(S,H),
  format(S,' :-',[]),
  nl(S),
  write_body(S,B).


write_head(S,[A:1.0|_Rest]):-!,
  format(S,"~q:1.0",[A]).

write_head(S,[A:P,'':_P]):-!,
  format(S,"~q:~g",[A,P]).

write_head(S,[A:P]):-!,
  format(S,"~q:~g",[A,P]).

write_head(S,[A:P|Rest]):-
  format(S,"~q:~g ; ",[A,P]),
  write_head(S,Rest).

write_body(S,[]):-!,
  format(S,"  true.~n~n",[]).

write_body(S,[A]):-!,
  format(S,"  ~q.~n~n",[A]).

write_body(S,[A|T]):-
  format(S,"  ~q,~n",[A]),
  write_body(S,T).

/**
 * list2or(+List:list,-Or:term) is det.
 * list2or(-List:list,+Or:term) is det.
 *
 * The predicate succeeds when Or is a disjunction (using the ; operator)
 * of the terms in List
 */
list2or([],true):-!.

list2or([X],X):-
    X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
    list2or(T,Ta).


/**
 * list2and(+List:list,-And:term) is det.
 * list2and(-List:list,+And:term) is det.
 *
 * The predicate succeeds when And is a conjunction (using the , operator)
 * of the terms in List
 */
list2and([],true):-!.

list2and([X],X):-
    X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
    list2and(T,Ta).


deduct(0,_Mod,_DB,Th,Th):-!.

deduct(NM,Mod,DB,InTheory0,InTheory):-
  get_head_atoms(O,Mod),
  sample_hplp(1,DB,Sampled,DB1),
  (Sampled=[M]->
    generate_head(O,M,Mod,[],HL),
    NM1 is NM-1,
    ( HL \== [] ->
       (generate_body(HL,Mod,InTheory1),
    	append(InTheory0,InTheory1,InTheory2),
    	deduct(NM1,Mod,DB1,InTheory2,InTheory)
       )
      ;
       deduct(NM1,Mod,DB1,InTheory0,InTheory)
    )
  ;
    InTheory=InTheory0
  ).


get_head_atoms(O,M):-
  findall(A,M:modeh(_,A),O0),
  findall((A,B,D),M:modeh(_,A,B,D),O1),
  append(O0,O1,O).

generate_top_cl([],_M,[]):-!.

generate_top_cl([A|T],M,[(rule(R,[A1:0.5,'':0.5],[],true),-1e20)|TR]):-
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  A1=..[F|Arg],
  get_next_rule_number(M,R),
  generate_top_cl(T,M,TR).


generate_head([],_M,_Mod,HL,HL):-!.

generate_head([(A,G,D)|T],M,Mod,H0,H1):-!,
  generate_head_goal(G,M,Goals),
  findall((A,Goals,D),(member(Goal,Goals),call(Mod:Goal),ground(Goals)),L),
  Mod:local_setting(initial_clauses_per_megaex,IC),   %IC: represents how many samples are extracted from the list L of example
  sample(IC,L,L1),
  append(H0,L1,H2),
  generate_head(T,M,Mod,H2,H1).

generate_head([A|T],M,Mod,H0,H1):-
  functor(A,F,N),
  functor(F1,F,N),
  F1=..[F|Arg],
  Pred1=..[F,M|Arg],
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  findall((A,Pred1),call(Mod:Pred1),L),
  Mod:local_setting(initial_clauses_per_megaex,IC),
  sample(IC,L,L1),
  append(H0,L1,H2),
  generate_head(T,M,Mod,H2,H1).

generate_head_goal([],_M,[]).

generate_head_goal([H|T],M,[H1|T1]):-
  H=..[F|Arg],
  H1=..[F,M|Arg],
  generate_head_goal(T,M,T1).

keep_const([],[]).

keep_const([- _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([+ _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([-# _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([H|T],[H1|T1]):-
  H=..[F|Args],
  keep_const(Args,Args1),
  H1=..[F|Args1],
  keep_const(T,T1).


/**
 * sample_hplp(+N,List:list,-Sampled:list,-Rest:list) is det
 *
 * Samples N elements from List and returns them in Sampled.
 * The rest of List is returned in Rest
 * If List contains less than N elements, Sampled is List and Rest
 * is [].
*/
sample_hplp(0,List,[],List):-!.

sample_hplp(N,List,List,[]):-
  length(List,L),
  L=<N,!.

sample_hplp(N,List,[El|List1],Li):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El,Rest),
  N1 is N-1,
  sample_hplp(N1,Rest,List1,Li).

sample(0,_List,[]):-!.

sample(N,List,List):-
  length(List,L),
  L=<N,!.

sample(N,List,[El|List1]):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El,Rest),
  N1 is N-1,
  sample(N1,Rest,List1).

get_args([],[],[],A,A,AT,AT,_).

get_args([HM|TM],[H|TH],[(H,HM)|TP],A0,A,AT0,AT,M):-
  HM=..[F|ArgsTypes],
  H=..[F,M|Args],
  append(A0,Args,A1),
  append(AT0,ArgsTypes,AT1),
  get_args(TM,TH,TP,A1,A,AT1,AT,M).

/* Generation of the bottom clauses */

gen_head([],P,['':P]).

gen_head([H|T],P,[H:P|TH]):-
  gen_head(T,P,TH).

get_modeb([],_Mod,B,B).

get_modeb([F/AA|T],Mod,B0,B):-
  findall((R,B),(Mod:modeb(R,B),functor(B,F,AA)),BL),
  (setting_sc(neg_literals,true)->
    findall((R,(\+ B)),(Mod:modeb(R,B),functor(B,F,AA),all_plus(B)),BNL)
  ;
    BNL=[]
  ),
  append([B0,BL,BNL],B1),
  get_modeb(T,Mod,B1,B).

all_plus(B):-
  B=..[_|Args],
  all_plus_args(Args).

all_plus_args([]).

all_plus_args([+ _ |T]):-!,
  all_plus_args(T).

all_plus_args([H|T]):-
  H \= - _,
  H \= # _,
  H \= -# _,
  H=..[_|Args],
  all_plus_args(Args),
  all_plus_args(T).

generate_body([],_Mod,[]):-!.

generate_body([(A,H,Det)|T],Mod,[(rule(R,HP,[],BodyList),-1e20)|CL0]):-!,
  get_modeb(Det,Mod,[],BL),
  get_args(A,H,Pairs,[],Args,[],ArgsTypes,M),
  Mod:local_setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],Mod,BL,a,[],BLout0,D,M),
  variabilize((Pairs:-BLout0),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=(Head1:-BodyList1),
  remove_int_atom_list(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList2),
  remove_duplicates(BodyList2,BodyList),
  get_next_rule_number(Mod,R),
  length(Head,LH),
  Prob is 1/(LH+1),
  gen_head(Head,Prob,HP),
  copy_term((HP,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format2(Mod,"Bottom clause: example ~q~nClause~n",[H]),
  write_disj_clause2(Mod,user_output,(HeadV:-BodyListV)),
  generate_body(T,Mod,CL0).

generate_body([(A,H)|T],Mod,[(rule(R,[Head:0.5,'':0.5],[],BodyList),-1e20)|CL0]):-
  functor(A,F,AA),
  findall(FB/AB,Mod:determination(F/AA,FB/AB),Det),
  get_modeb(Det,Mod,[],BL),
  A=..[F|ArgsTypes],
  H=..[F,M|Args],
  Mod:local_setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],Mod,BL,a,[],BLout0,D,M),
  variabilize(([(H,A)]:-BLout0),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=([Head1]:-BodyList1),
  remove_int_atom(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList2),
  remove_duplicates(BodyList2,BodyList),
  get_next_rule_number(Mod,R),
  copy_term((Head,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format2(Mod,"Bottom clause: example ~q~nClause~n~q:0.5 :-~n",[H,HeadV]),
  write_body2(Mod,user_output,BodyListV),
  generate_body(T,Mod,CL0).





remove_int_atom_list([],[]).

remove_int_atom_list([\+ A|T],[\+ A1|T1]):-!,
  A=..[F,_|Arg],
  A1=..[F|Arg],
  remove_int_atom_list(T,T1).

remove_int_atom_list([A|T],[A1|T1]):-
  A=..[F,_|Arg],
  A1=..[F|Arg],
  remove_int_atom_list(T,T1).



remove_int_atom(\+ A,\+ A1):-!,
  A=..[F,_|T],
  A1=..[F|T].

remove_int_atom(A,A1):-
  A=..[F,_|T],
  A1=..[F|T].



variabilize((H:-B),(H1:-B1)):-
  variabilize_list(H,H1,[],AS,M),
  variabilize_list(B,B1,AS,_AS,M).


variabilize_list([],[],A,A,_M).

variabilize_list([(\+ H,Mode)|T],[\+ H1|T1],A0,A,M):-
  builtin(H),!,
  H=..[F|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(\+ H,Mode)|T],[\+ H1|T1],A0,A,M):-!,
  H=..[F,_M|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(H,Mode)|T],[H1|T1],A0,A,M):-
  builtin(H),!,
  H=..[F|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(H,Mode)|T],[H1|T1],A0,A,M):-
  H=..[F,_M|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).


variabilize_args([],[],[],A,A).

variabilize_args([C|T],[C|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[# _Ty|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[-# _Ty|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  (Ty = +Ty1;Ty = -Ty1),
  member(C/Ty1/V,A0),!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  (Ty = +Ty1;Ty = -Ty1),!,
  variabilize_args(T,TT,TV,[C/Ty1/V|A0],A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  compound(C),
  C=..[F|Args],
  Ty=..[F|ArgsT],
  variabilize_args(Args,ArgsT,ArgsV,A0,A1),
  V=..[F|ArgsV],
  variabilize_args(T,TT,TV,A1,A).


cycle_modeb(ArgsTypes,Args,ArgsTypes,Args,_Mod,_BL,L,L,L,_,_M):-!.

cycle_modeb(_ArgsTypes,_Args,_ArgsTypes1,_Args1,_Mod,_BL,_L,L,L,0,_M):-!.

cycle_modeb(ArgsTypes,Args,_ArgsTypes0,_Args0,Mod,BL,_L0,L1,L,D,M):-
  find_atoms(BL,Mod,ArgsTypes,Args,ArgsTypes1,Args1,L1,L2,M),
  D1 is D-1,
  cycle_modeb(ArgsTypes1,Args1,ArgsTypes,Args,Mod,BL,L1,L2,L,D1,M).


find_atoms([],_Mod,ArgsTypes,Args,ArgsTypes,Args,L,L,_M).

find_atoms([(R,\+ H)|T],Mod,ArgsTypes0,Args0,ArgsTypes,Args,L0,L1,M):-!,
  H=..[F|ArgsT],
  findall((A,H),instantiate_query_neg(ArgsT,ArgsTypes0,Args0,F,M,A),L),
  call_atoms(L,Mod,[],At),
  remove_duplicates(At,At1),
  ((R = '*' ) ->
    R1= +1e20
  ;
    R1=R
  ),
  sample(R1,At1,At2),
  append(L0,At2,L2),
  find_atoms(T,Mod,ArgsTypes0,Args0,ArgsTypes,Args,L2,L1,M).

find_atoms([(R,H)|T],Mod,ArgsTypes0,Args0,ArgsTypes,Args,L0,L1,M):-
  H=..[F|ArgsT],
  findall((A,H),instantiate_query(ArgsT,ArgsTypes0,Args0,F,M,A),L),
  call_atoms(L,Mod,[],At),
  remove_duplicates(At,At1),
  ((R = '*' ) ->
    R1= +1e20
  ;
    R1=R
  ),
  sample(R1,At1,At2),
  extract_output_args(At2,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  append(L0,At2,L2),
  find_atoms(T,Mod,ArgsTypes1,Args1,ArgsTypes,Args,L2,L1,M).


call_atoms([],_Mod,A,A).

call_atoms([(H,M)|T],Mod,A0,A):-
  findall((H,M),Mod:H,L),
  append(A0,L,A1),
  call_atoms(T,Mod,A1,A).


extract_output_args([],_ArgsT,ArgsTypes,Args,ArgsTypes,Args).

extract_output_args([(H,_At)|T],ArgsT,ArgsTypes0,Args0,ArgsTypes,Args):-
  builtin(H),!,
  H=..[_F|ArgsH],
  add_const(ArgsH,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  extract_output_args(T,ArgsT,ArgsTypes1,Args1,ArgsTypes,Args).

extract_output_args([(H,_At)|T],ArgsT,ArgsTypes0,Args0,ArgsTypes,Args):-
  H=..[_F,_M|ArgsH],
  add_const(ArgsH,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  extract_output_args(T,ArgsT,ArgsTypes1,Args1,ArgsTypes,Args).


add_const([],[],ArgsTypes,Args,ArgsTypes,Args).

add_const([_A|T],[+_T|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[-Type|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  (already_present(ArgsTypes0,Args0,A,Type)->
    ArgsTypes1=ArgsTypes0,
    Args1=Args0
  ;
    ArgsTypes1=[+Type|ArgsTypes0],
    Args1=[A|Args0]
  ),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).

add_const([A|T],[-# Type|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  (already_present(ArgsTypes0,Args0,A,Type)->
    ArgsTypes1=ArgsTypes0,
    Args1=Args0
  ;
    ArgsTypes1=[+Type|ArgsTypes0],
    Args1=[A|Args0]
  ),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).

add_const([_A|T],[# _|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[A|TT],ArgsTypes0,Args0,ArgsTypes,Args):-
  atomic(A),!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[AT|TT],ArgsTypes0,Args0,ArgsTypes,Args):-
  A=..[F|Ar],
  AT=..[F|ArT],
  add_const(Ar,ArT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).


already_present([+T|_TT],[C|_TC],C,T):-!.

already_present([_|TT],[_|TC],C,T):-
  already_present(TT,TC,C,T).


instantiate_query_neg(ArgsT,ArgsTypes,Args,F,M,A):-
  instantiate_input(ArgsT,ArgsTypes,Args,ArgsB),
  A1=..[F|ArgsB],
  (builtin(A1)->
    A= (\+ A1)
  ;
    A0=..[F,M|ArgsB],
    A = (\+ A0)
  ).

instantiate_query(ArgsT,ArgsTypes,Args,F,M,A):-
  instantiate_input(ArgsT,ArgsTypes,Args,ArgsB),
  A1=..[F|ArgsB],
  (builtin(A1)->
    A=A1
  ;
    A=..[F,M|ArgsB]
  ).


instantiate_input([],_AT,_A,[]).

instantiate_input([-_Type|T],AT,A,[_V|TA]):-!,
  instantiate_input(T,AT,A,TA).

instantiate_input([+Type|T],AT,A,[H|TA]):-!,
  find_val(AT,A,+Type,H),
  instantiate_input(T,AT,A,TA).

instantiate_input([# Type|T],AT,A,[H|TA]):-!,
  find_val(AT,A,+Type,H),
  instantiate_input(T,AT,A,TA).

instantiate_input([-# _Type|T],AT,A,[_V|TA]):-!,
  instantiate_input(T,AT,A,TA).

instantiate_input([C|T],AT,A,[C1|TA]):-
  C=..[F|Args],
  instantiate_input(Args,AT,A,Args1),
  C1=..[F|Args1],
  instantiate_input(T,AT,A,TA).


find_val([T|_TT],[A|_TA],T,A).

find_val([HT|_TT],[HA|_TA],T,A):-
  nonvar(HA),
  HT=..[F|ArgsT],
  HA=..[F|Args],
  find_val(ArgsT,Args,T,A).

find_val([_T|TT],[_A|TA],T,A):-
  find_val(TT,TA,T,A).


get_output_atoms(O,M):-
  findall((A/Ar),M:output((A/Ar)),O).


generate_goal([],_M,_H,G,G):-!.

generate_goal([P/A|T],M,H,G0,G1):-
  functor(Pred,P,A),
  Pred=..[P|Rest],
  Pred1=..[P,H|Rest],
  findall(Pred1,call(M:Pred1),L),
  findall(\+ Pred1,call(M:neg(Pred1)),LN),
  append(G0,L,G2),
  append(G2,LN,G3),
  generate_goal(T,M,H,G3,G1).

remove_duplicates(L0,L):-
  remove_duplicates(L0,[],L1),
  reverse(L1,L).

remove_duplicates([],L,L).

remove_duplicates([H|T],L0,L):-
  member_eq(H,L0),!,
  remove_duplicates(T,L0,L).

remove_duplicates([H|T],L0,L):-
  remove_duplicates(T,[H|L0],L).





banned_clause(M,H,B):-
  numbervars((H,B),0,_N),
  M:banned(H2,B2),
  mysublist(H2,H),
  mysublist(B2,B).


mysublist([],_).

mysublist([H|T],L):-
  member(H,L),
  mysublist(T,L).




remove_prob(['':_P],[]):-!.

remove_prob([X:_|R],[X|R1]):-
  remove_prob(R,R1).


convert_to_input_vars([],[]):-!.

convert_to_input_vars([+T|RT],[+T|RT1]):-
  !,
  convert_to_input_vars(RT,RT1).

convert_to_input_vars([-T|RT],[+T|RT1]):-
  convert_to_input_vars(RT,RT1).



remove_eq(X,[Y|R],R):-
  X == Y,
  !.

remove_eq(X,[_|R],R1):-
  remove_eq(X,R,R1).




input_variables(\+ LitM,M,InputVars):-
  !,
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  M:modeb(_,Lit1),
  Lit1 =.. [P|Args1],
  convert_to_input_vars(Args1,Args2),
  Lit2 =.. [P|Args2],
  input_vars(LitM,Lit2,InputVars).

input_variables(LitM,M,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  M:modeb(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_variables(LitM,M,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  M:modeh(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_vars(Lit,Lit1,InputVars):-
  Lit =.. [_|Vars],
  Lit1 =.. [_|Types],
  input_vars1(Vars,Types,InputVars).


input_vars1([],_,[]).

input_vars1([V|RV],[+_T|RT],[V|RV1]):-
  !,
  input_vars1(RV,RT,RV1).

input_vars1([_V|RV],[_|RT],RV1):-
  input_vars1(RV,RT,RV1).


exctract_type_vars([],_M,[]).

exctract_type_vars([Lit|RestLit],M,TypeVars):-
  Lit =.. [Pred|Args],
  length(Args,L),
  length(Args1,L),
  Lit1 =.. [Pred|Args1],
  take_mode(M,Lit1),
  type_vars(Args,Args1,Types),
  exctract_type_vars(RestLit,M,TypeVars0),
  !,
  append(Types,TypeVars0,TypeVars).


take_mode(M,Lit):-
  M:modeh(_,Lit),!.

take_mode(M,Lit):-
  M:modeb(_,Lit),!.

take_mode(M,Lit):-
  M:mode(_,Lit),!.


type_vars([],[],[]).

type_vars([V|RV],[+T|RT],[V=T|RTV]):-
  !,
  type_vars(RV,RT,RTV).

type_vars([V|RV],[-T|RT],[V=T|RTV]):-atom(T),!,
  type_vars(RV,RT,RTV).

type_vars([_V|RV],[_T|RT],RTV):-
  type_vars(RV,RT,RTV).


take_var_args([],_,[]).

take_var_args([+T|RT],TypeVars,[V|RV]):-
  !,
  member(V=T,TypeVars),
  take_var_args(RT,TypeVars,RV).

take_var_args([-T|RT],TypeVars,[_V|RV]):-
  atom(T),
  take_var_args(RT,TypeVars,RV).

take_var_args([-T|RT],TypeVars,[V|RV]):-
  member(V=T,TypeVars),
  take_var_args(RT,TypeVars,RV).

take_var_args([T|RT],TypeVars,[T|RV]):-
  T\= + _,(T\= - _; T= - A,number(A)),
  take_var_args(RT,TypeVars,RV).



add_probs([],['':P],P):-!.

add_probs([H|T],[H:P|T1],P):-
  add_probs(T,T1,P).


extract_fancy_vars(List,Vars):-
  term_variables(List,Vars0),
  fancy_vars(Vars0,1,Vars).


fancy_vars([],_,[]).

fancy_vars([X|R],N,[NN2=X|R1]):-
  name(N,NN),
  append([86],NN,NN1),
  name(NN2,NN1),
  N1 is N + 1,
  fancy_vars(R,N1,R1).


delete_one([X|R],R,X).

delete_one([X|R],[X|R1],D):-
  delete_one(R,R1,D).



make_dynamic(M):-
  M:(dynamic int/1),
  findall(O,M:output(O),LO),
  findall(I,M:input(I),LI),
  %findall(I,M:input_cw(I),LIC),
  findall(D,M:determination(D,_DD),LDH),
  findall(DD,M:determination(_D,DD),LDD),
  findall(DH,(M:modeh(_,_,_,LD),member(DH,LD)),LDDH),
  %append([LO,LI,LIC,LDH,LDD,LDDH],L0),
  append([LO,LI,LDH,LDD,LDDH],L0),
  remove_duplicates(L0,L),
  maplist(to_dyn(M),L).

to_dyn(M,P/A):-
  %atomic_concat(P, ' tabled',PT),
  A1 is A+1,
  M:(dynamic P/A1),
  A2 is A1+1,
  M:(dynamic P/A2),
  %M:(dynamic PT/A2),
  A3 is A2+1,
  M:(dynamic P/A3).
 % M:(dynamic PT/A3).


%Computation of the depth of the variables in the clause head/body
dv(H,B,M,DV1):-			%DV1: returns a list of couples (Variable, Max depth)
	term_variables(H,V),
	head_depth(V,DV0),
	findall((MD-DV),var_depth(B,M,DV0,DV,0,MD),LDs),
        get_max(LDs,-1,-,DV1).


input_variables_b(\+ LitM,M,InputVars):-!,
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  M:modeb(_,Lit1),
	  all_plus(Lit1),
	  input_vars(LitM,Lit1,InputVars).

input_variables_b(LitM,M,InputVars):-
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  M:modeb(_,Lit1),
	  input_vars(LitM,Lit1,InputVars).



%associates depth 0 to each variable in the clause head
head_depth([],[]).
head_depth([V|R],[[V,0]|R1]):-
  head_depth(R,R1).

%associates a depth to each variable in the clause body
var_depth([],_M,PrevDs1,PrevDs1,MD,MD):-!.

var_depth([L|R],M,PrevDs,PrevDs1,_MD,MD):-    		%L = a body literal, MD = maximum depth set by the user
  input_variables_b(L,M,InputVars),
  term_variables(L, BodyAtomVars),
  output_vars(BodyAtomVars,InputVars,OutputVars),
  depth_InputVars(InputVars,PrevDs,0,MaxD),   		%MaxD: maximum depth of the input variables in the body literal
  D is MaxD+1,
  compute_depth(OutputVars,D,PrevDs,PrevDs0), 		%Computes the depth for the output variables in the body literal
  var_depth(R,M,PrevDs0,PrevDs1,D,MD).

get_max([],_,Ds,Ds).

get_max([(MD-DsH)|T],MD0,_Ds0,Ds):-
  MD>MD0,!,
  get_max(T,MD,DsH,Ds).

get_max([_H|T],MD,Ds0,Ds):-
	get_max(T,MD,Ds0,Ds).

delete_eq([],_E,[]).

delete_eq([H|T],E,T1):-
  H==E,!,
  delete_eq(T,E,T1).

delete_eq([H|T],E,[H|T1]):-
  delete_eq(T,E,T1).

output_vars(OutVars,[],OutVars):-!.
output_vars(BodyAtomVars,[I|InputVars],OutVars):-
  delete_eq(BodyAtomVars, I, Residue),
  output_vars(Residue,InputVars, OutVars).

% returns D as the maximum depth of the variables in the list (first argument)
depth_InputVars([],_,D,D).
depth_InputVars([I|Input],PrevDs,D0,D):-
	 member_l(PrevDs,I,MD),
	 (MD>D0->
		D1=MD
	;
		D1=D0
         ),
	 depth_InputVars(Input,PrevDs,D1,D).

member_l([[L,D]|_P],I,D):-
     I==L,!.
member_l([_|P],I,D):-
     member_l(P,I,D).

compute_depth([],_,PD,PD):-!.
compute_depth([O|Output],D,PD,RestO):-
	member_l(PD,O,_),!,
	compute_depth(Output,D,PD,RestO).

compute_depth([O|Output],D,PD,[[O,D]|RestO]):-
	compute_depth(Output,D,PD,RestO).



assert_all([],_M,[]).

assert_all([H|T],M,[HRef|TRef]):-
  assertz(M:H,HRef),
  assert_all(T,M,TRef).

assert_all([],[]).

assert_all([H|T],[HRef|TRef]):-
  assertz(phil:H,HRef),
  assert_all(T,TRef).

/*
retract_all([],_):-!.

retract_all([H|T],M):-
  erase(M,H),
  retract_all(T,M). */

retract_all([]):-!.

retract_all([H|T]):-
  erase(H),
  retract_all(T).


read_clauses_dir(S,[Cl|Out]):-
  read_term(S,Cl,[]),
  (Cl=end_of_file->
    Out=[]
  ;
    read_clauses_dir(S,Out)
  ).

process_clauses([],_M,C,C,R,R):-!.

process_clauses([end_of_file],_M,C,C,R,R):-!.

process_clauses([H|T],M,C0,C1,R0,R1):-
  (term_expansion_int(H,M,H1)->
    true
  ;
    H1=(H,[])
  ),
  (H1=([_|_],R)->
    H1=(List,R),
    append(C0,List,C2),
    append(R0,R,R2)
  ;
    (H1=([],_R)->
      C2=C0,
      R2=R0
    ;
      H1=(H2,R),
      append(C0,[H2],C2),
      append(R0,R,R2)
    )
  ),
  process_clauses(T,M,C2,C1,R2,R1).


get_next_rule_number(M,R):-
  retract(M:rule_sc_n(R)),
  R1 is R+1,
  assert(M:rule_sc_n(R1)).
/*
get_node(\+ Goal,M,AC):-
  M:local_setting(depth_bound,true),!,
  M:local_setting(depth,DB),
  retractall(M:v(_,_,_)),
  add_ac_arg_db(Goal,B,DB,Goal1),
  (M:Goal1->
    ac_notc(B,(_,AC))
  ;
    zeroc((_,AC))
  ).

get_node(\+ Goal,M,AC):-!,
  retractall(M:v(_,_,_)),
  add_ac_arg(Goal,B,Goal1),
  (M:Goal1->
    ac_notc(B,(_,AC))
  ;
    zeroc((_,AC))
  ).

get_node(Goal,M,AC):-
  M:local_setting(depth_bound,true),!,
  M:local_setting(depth,DB),
  retractall(M:v(_,_,_)),
  add_ac_arg_db(Goal,B,DB,Goal1),%DB=depth bound
  (M:Goal1->
    (_,AC)=B
  ;
    zeroc((_,AC))
  ).

get_node(Goal,M,AC):- %with DB=false
  retractall(M:v(_,_,_)),
  add_ac_arg(Goal,B,Goal1),
  (M:Goal1->
    (_,AC)=B
  ;
    zeroc((_,AC))
  ).
*/

get_node(\+ Goal,M,AC):-
  M:local_setting(depth_bound,true),!,
  M:local_setting(depth,DB),
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_ac_arg_db(Goal,C,DB,Goal1),
  (M:Goal1->
    B=C
  ;
    zeroc(B)
  ),
  ac_notc(B,AC).

get_node(\+ Goal,M,AC):-!,
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_ac_arg(Goal,C,Goal1),
  (M:Goal1->
    B=C
  ;
    zeroc(B)
  ),
  ac_notc(B,AC).

get_node(Goal,M,B):-
  M:local_setting(depth_bound,true),!,
  M:local_setting(depth,DB),
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_ac_arg_db(Goal,AC,DB,Goal1),%DB=depth bound
  (M:Goal1->
    B=AC
  ;
    zeroc(B)
  ).

get_node(Goal,M,B):- %with DB=false
  retractall(M:v(_,_,_)),
  add_ac_arg(Goal,AC,Goal1),
  abolish_all_tables,
  (M:Goal1->
    B=AC
  ;
    zeroc(B)
  ).


add_ac_arg(A,AC,A1):-
  A=..[P|Args],
  append(Args,[AC],Args1),
  A1=..[P|Args1].


add_ac_arg_db(A,AC,DB,A1):-
  A=..[P|Args],
  append(Args,[DB,AC],Args1),
  A1=..[P|Args1].


add_ac_arg(A,AC,Module,A1):-
  A=..[P|Args],
  append(Args,[AC],Args1),
  A1=..[P,Module|Args1].


add_ac_arg_db(A,AC,DB,Module,A1):-
  A=..[P|Args],
  append(Args,[DB,AC],Args1),
  A1=..[P,Module|Args1].

add_mod_arg(A,Module,A1):-
  A=..[P|Args],
  A1=..[P,Module|Args].


generate_rules_fact([],_VC,_R,_Probs,_N,[],_Module,_M).

generate_rules_fact([Head:_P1,'':_P2],VC,R,Probs,N,[Clause],Module,M):-!,
  add_ac_arg(Head,AC,Module,Head1),
  Clause=(Head1:-(phil:get_var_n(M,R,VC,Probs,V),phil:equalityc(V,N,AC))).

generate_rules_fact([Head:_P|T],VC,R,Probs,N,[Clause|Clauses],Module,M):-
  add_ac_arg(Head,AC,Module,Head1),
  Clause=(Head1:-(phil:get_var_n(M,R,VC,Probs,V),phil:equalityc(V,N,AC))),
  N1 is N+1,
  generate_rules_fact(T,VC,R,Probs,N1,Clauses,Module,M).


generate_rules_fact_db([],_VC,_R,_Probs,_N,[],_Module,_M).

generate_rules_fact_db([Head:_P1,'':_P2],VC,R,Probs,N,[Clause],Module,M):-!,
  add_ac_arg_db(Head,AC,_DB,Module,Head1),
  Clause=(Head1:-(phil:get_var_n(M,R,VC,Probs,V),phil:equalityc(V,N,AC))).

generate_rules_fact_db([Head:_P|T],VC,R,Probs,N,[Clause|Clauses],Module,M):-
  add_ac_arg_db(Head,AC,_DB,Module,Head1),
  Clause=(Head1:-(phil:get_var_n(M,R,VC,Probs,V),phil:equalityc(V,N,AC))),
  N1 is N+1,
  generate_rules_fact_db(T,VC,R,Probs,N1,Clauses,Module,M).


generate_clause(Head,Body,_VC,_R,_Probs,ACAnd,_N,Clause,Module,_M):-
  add_ac_arg(Head,or([ACAnd]),Module,Head1),
  Clause=(Head1:-Body).


generate_clause_db(Head,Body,_VC,_R,_Probs,DB,ACAnd,_N,Clause,Module,_M):-
  add_ac_arg_db(Head,or([ACAnd]),DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body)).


generate_rules([],_Body,_VC,_R,_Probs,_ACAnd,_N,[],_Module,_M).

generate_rules([Head:_P1,'':_P2],Body,VC,R,Probs,ACAnd,N,[Clause],Module,M):-!,
  generate_clause(Head,Body,VC,R,Probs,ACAnd,N,Clause,Module,M).

generate_rules([Head:_P|T],Body,VC,R,Probs,ACAnd,N,[Clause|Clauses],Module,M):-
  generate_clause(Head,Body,VC,R,Probs,ACAnd,N,Clause,Module,M),
  N1 is N+1,
  generate_rules(T,Body,VC,R,Probs,ACAnd,N1,Clauses,Module,M).


generate_rules_db([],_Body,_VC,_R,_Probs,_DB,_ACAnd,_N,[],_Module,_M):-!.

generate_rules_db([Head:_P1,'':_P2],Body,VC,R,Probs,DB,ACAnd,N,[Clause],Module,M):-!,
  generate_clause_db(Head,Body,VC,R,Probs,DB,ACAnd,N,Clause,Module,M).

generate_rules_db([Head:_P|T],Body,VC,R,Probs,DB,ACAnd,N,[Clause|Clauses],Module,M):-
  generate_clause_db(Head,Body,VC,R,Probs,DB,ACAnd,N,Clause,Module,M),!,%agg.cut
  N1 is N+1,
  generate_rules_db(T,Body,VC,R,Probs,DB,ACAnd,N1,Clauses,Module,M).

process_body_bg([],[],_Module).

process_body_bg([\+ H|T],[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_bg(T,Rest,Module).

process_body_bg([\+ H|T],[\+ H1|Rest],Module):-!,
  add_mod_arg(H,Module,H1),
  process_body_bg(T,Rest,Module).

process_body_bg([H|T],[H|Rest],Module):-
  builtin(H),!,
  process_body_bg(T,Rest,Module).

process_body_bg([H|T],[H1|Rest],Module):-!,
  add_mod_arg(H,Module,H1),
  process_body_bg(T,Rest,Module).




process_body([],AC,AC,Vars,Vars,[],_Module,_M):-!.

process_body([\+ H|T],AC,AC1,Vars,Vars1,[\+ H|Rest],Module,M):-
  builtin(H),!,
  process_body(T,AC,AC1,Vars,Vars1,Rest,Module,M).

process_body([\+ H|T],AC,AC1,Vars,Vars1,[
  neg(H1)|Rest],Module,M):-
  given_cw(M,H),!,
  add_mod_arg(H,Module,H1),
  process_body(T,AC,AC1,Vars,Vars1,Rest,Module,M).

process_body([\+ H|T],AC,AC1,Vars,[ACH,ACN,AC2|Vars1],
[H1,phil:ac_notc(ACH,ACN),
  phil:andc(AC,ACN,AC2)|Rest],Module,M):-!,
  add_ac_arg(H,ACH,Module,H1),
  process_body(T,AC2,AC1,Vars,Vars1,Rest,Module,M).

process_body([H|T],AC,AC1,Vars,Vars1,[H|Rest],Module,M):-
  builtin(H),!,
  process_body(T,AC,AC1,Vars,Vars1,Rest,Module,M).

process_body([H|T],AC,AC1,Vars,Vars1,
[H1|Rest],Module,M):-
  given_cw(M,H),!,
  add_mod_arg(H,Module,H1),
  process_body(T,AC,AC1,Vars,Vars1,Rest,Module,M).

process_body([H|T],AC,AC1,Vars,[ACH,AC2|Vars1],
[H1,phil:andc(AC,ACH,AC2)|Rest],Module,M):-!, %agg. cut
  add_ac_arg(H,ACH,Module,H1),
  process_body(T,AC2,AC1,Vars,Vars1,Rest,Module,M).

process_body_db([],AC,AC,_DB,Vars,Vars,[],_Module,_M):-!.

process_body_db([\+ H|T],AC,AC1,DB,Vars,Vars1,[\+ H|Rest],Module,M):-
  builtin(H),!,
  process_body_db(T,AC,AC1,DB,Vars,Vars1,Rest,Module,M).

process_body_db([\+ H|T],AC,AC1,DB,Vars,Vars1,[
  neg(H1)|Rest],Module,M):-
  given_cw(M,H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,AC,AC1,DB,Vars,Vars1,Rest,Module,M).

process_body_db([\+ H|T],AC,AC1,DB,Vars,[ACH,ACN,AC2|Vars1],
[H1,phil:ac_notc(ACH,ACN),
  phil:andc(AC,ACN,AC2)|Rest],Module,M):-!,
  add_ac_arg_db(H,ACH,DB,Module,H1),
  process_body_db(T,AC2,AC1,DB,Vars,Vars1,Rest,Module,M).

process_body_db([H|T],AC,AC1,DB,Vars,Vars1,[H|Rest],Module,M):-
  builtin(H),!,
  process_body_db(T,AC,AC1,DB,Vars,Vars1,Rest,Module,M).

process_body_db([H|T],AC,AC1,DB,Vars,Vars1,
[H1|Rest],Module,M):-
  given_cw(M,H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,AC,AC1,DB,Vars,Vars1,Rest,Module,M).

process_body_db([H|T],AC,AC1,DB,Vars,[ACH,AC2|Vars1],
[H1,phil:andc(AC,ACH,AC2)|Rest],Module,M):-!, %agg. cut
  add_ac_arg_db(H,ACH,DB,Module,H1),
  process_body_db(T,AC2,AC1,DB,Vars,Vars1,Rest,Module,M).



process_body_cw([],AC,AC,Vars,Vars,[],_Module).

process_body_cw([\+ H|T],AC,AC1,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_cw(T,AC,AC1,Vars,Vars1,Rest,Module).

process_body_cw([\+ H|T],AC,AC1,Vars,Vars1,[\+ H|Rest],Module):-
  db(H),!,
  process_body_cw(T,AC,AC1,Vars,Vars1,Rest,Module).

process_body_cw([\+ H|T],AC,AC1,Vars,Vars1,[
  \+(H1)|Rest],Module):-
  add_mod_arg(H,Module,H1),
  process_body_cw(T,AC,AC1,Vars,Vars1,Rest,Module).

process_body_cw([H|T],AC,AC1,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body_cw(T,AC,AC1,Vars,Vars1,Rest,Module).

process_body_cw([H|T],AC,AC1,Vars,Vars1,[H|Rest],Module):-
  db(H),!,
  process_body_cw(T,AC,AC1,Vars,Vars1,Rest,Module).

process_body_cw([H|T],AC,AC1,Vars,Vars1,
[H1|Rest],Module):-
  add_mod_arg(H,Module,H1),
  process_body_cw(T,AC,AC1,Vars,Vars1,Rest,Module).


given(M,H):-
  functor(H,P,Ar),
  (M:input(P/Ar)).


given_cw(M,H):-
  functor(H,P,Ar),
  (M:input(P/Ar)).
  %(M:input_cw(P/Ar)).

/*
and_list([],B,B).

and_list([H|T],B0,B1):-
  and(B0,H,B2),
  and_list(T,B2,B1).
*/

/**
 * set_hplp(:Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_hplp(M:Parameter,Value):-
  retract(M:local_setting(Parameter,_)),
  assert(M:local_setting(Parameter,Value)).

/**
 * setting_sc(:Parameter:atom,-Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_sc(M:P,V):-
  M:local_setting(P,V).

extract_vars_list(L,[],V):-
  rb_new(T),
  extract_vars_tree(L,T,T1),
  rb_keys(T1,V).

extract_vars_term(Variable, Var0, Var1) :-
  var(Variable), !,
  (rb_lookup(Variable, Var0,_) ->
    Var1 = Var0
  ;
    rb_insert(Var0,Variable,1,Var1)
  ).

extract_vars_term(Term, Var0, Var1) :-
  Term=..[_F|Args],
  extract_vars_tree(Args, Var0, Var1).



extract_vars_tree([], Var, Var).

extract_vars_tree([Term|Tail], Var0, Var1) :-
  extract_vars_term(Term, Var0, Var),
  extract_vars_tree(Tail, Var, Var1).


difference([],_,[]).

difference([H|T],L2,L3):-
  member_eq(H,L2),!,
  difference(T,L2,L3).

difference([H|T],L2,[H|L3]):-
  difference(T,L2,L3).


member_eq(E,[H|_T]):-
  E==H,!.

member_eq(E,[_H|T]):-
  member_eq(E,T).




process_head(HeadList,M, GroundHeadList) :-
  ground_prob(HeadList), !,
  process_head_ground(HeadList,M, 0, GroundHeadList).

process_head(HeadList,_M, HeadList).



/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([Head:ProbHead],M, Prob, [Head:ProbHead1|Null]) :-!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  M:local_setting(epsilon_parsing, Eps),
  EpsNeg is - Eps,
  ProbLast > EpsNeg,
  (ProbLast > Eps ->
    Null = ['':ProbLast]
  ;
    Null = []
  ).

process_head_ground([Head:ProbHead|Tail], M, Prob, [Head:ProbHead1|Next]) :-
  ProbHead1 is ProbHead,
  ProbNext is Prob + ProbHead1,
  process_head_ground(Tail, M, ProbNext, Next).


ground_prob([]).

ground_prob([_Head:ProbHead|Tail]) :-
  ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
  ground_prob(Tail).


get_probs([], []).

get_probs([_H:P|T], [P1|T1]) :-
  P1 is P,
  get_probs(T, T1).


generate_clauses_cw([],_M,[],_N,C,C):-!.

generate_clauses_cw([H|T],M,[H1|T1],N,C0,C):-
  gen_clause_cw(H,M,N,N1,H1,CL),!,  %agg.cut
  append(C0,CL,C1),
  generate_clauses_cw(T,M,T1,N1,C1,C).

to_tabled(M,H0,H):-
  (M:tabled(H0)->
    H0=..[P|Args],
    %atomic_concat(P, ' tabled',PT),
    %H=..[PT|Args]
    H=..[P|Args]
  ;
    H=H0
  ).

to_tabled_head_list(M,A0:P,A:P):-
  to_tabled(M,A0,A).

gen_clause_cw((H :- Body),_M,N,N,(H :- Body),[(H1 :- Body)]):-
  !,
  H1=H.
 % phil:to_tabled(H,H1).

gen_clause_cw(rule(_R,HeadList,BodyList,Lit),M,N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-!,
% disjunctive clause with more than one head atom senza depth_bound
  process_body_cw(BodyList,AC,ACAnd,[],_Vars,BodyList1,Module),
  append([phil:onec(AC)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  maplist(to_tabled_head_list,M,HeadList,HeadList1),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList1,Body1,[],N,Probs,ACAnd,0,Clauses,Module,M)
  ;
    generate_rules(HeadList1,Body1,VC,N,Probs,ACAnd,0,Clauses,Module,M)
  ),
  N1 is N+1.

gen_clause_cw(def_rule(H,BodyList,Lit),_M,N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body_cw(BodyList,AC,ACAnd,[],_Vars,BodyList2,Module),
  append([phil:onec(AC)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_ac_arg(H,ACAnd,Module,Head1),
  %phil:to_tabled(Head1,Head2),
  Head2=Head1,
  Clauses=[(Head2 :- Body1)].


generate_clauses([],_M,[],_N,C,C):-!.

generate_clauses([H|T],M,[H1|T1],N,C0,C):-
  gen_clause(H,M,N,N1,H1,CL),!,  %agg.cut
  append(C0,CL,C1),
  generate_clauses(T,M,T1,N1,C1,C).


gen_clause((H :- Body),_M,N,N,(H :- Body),[(H1 :- Body)]):-
  !,
  H1=H.
 % phil:to_tabled(H,H1).


gen_clause(rule(_R,HeadList,BodyList,Lit),M,N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-
  M:local_setting(depth_bound,true),!,
% disjunctive clause with more than one head atom e depth_bound
  process_body_db(BodyList,and([N]),ACAnd, DB,[],_Vars,BodyList1,Module,M),
  list2and(BodyList1,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  maplist(to_tabled_head_list(M),HeadList,HeadList1),
  (M:local_setting(single_var,true)->
    generate_rules_db(HeadList1,Body1,[],N,Probs,DB,ACAnd,0,Clauses,Module,M)
  ;
    generate_rules_db(HeadList1,Body1,VC,N,Probs,DB,ACAnd,0,Clauses,Module,M)
   ),
  N1 is N+1.

gen_clause(rule(_R,HeadList,BodyList,Lit),M,N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-!,
% disjunctive clause with more than one head atom senza depth_bound
  process_body(BodyList,and([N]),ACAnd,[],_Vars,BodyList1,Module,M),
  list2and(BodyList1,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  maplist(to_tabled_head_list(M),HeadList,HeadList1),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList1,Body1,[],N,Probs,ACAnd,0,Clauses,Module,M)
  ;
    generate_rules(HeadList1,Body1,VC,N,Probs,ACAnd,0,Clauses,Module,M)
  ),
  N1 is N+1.

gen_clause(def_rule(H,BodyList,Lit),M,N,N,def_rule(H,BodyList,Lit),Clauses) :-
% disjunctive clause with a single head atom e depth_bound
  M:local_setting(depth_bound,true),!,
  process_body_db(BodyList,AC,ACAnd,DB,[],_Vars,BodyList2,Module,M),
  append([phil:onec(AC)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_ac_arg_db(H,ACAnd,DBH,Module,Head1),
  to_tabled(M,Head1,Head2),
  Clauses=[(Head2 :- (DBH>=1,DB is DBH-1,Body1))].

gen_clause(def_rule(H,BodyList,Lit),M,N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body(BodyList,AC,ACAnd,[],_Vars,BodyList2,Module,M),
  append([phil:onec(AC)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_ac_arg(H,ACAnd,Module,Head1),
  %trace,
  to_tabled(M,Head1,Head2),
  Clauses=[(Head2 :- Body1)].


generate_clauses_bg([],[]):-!.

generate_clauses_bg([H|T],[CL|T1]):-
  gen_clause_bg(H,CL),  %agg.cut
  generate_clauses_bg(T,T1).

gen_clause_bg(def_rule(H,BodyList,_Lit),Clauses) :-
% disjunctive clause with a single head atom e depth_bound
  process_body_bg(BodyList,BodyList2,Module),
  list2and(BodyList2,Body1),
  add_mod_arg(H,Module,Head1),
  Clauses=(Head1 :- Body1).


/**
 * builtin(+Goal:atom) is det
 *
 * Succeeds if Goal is an atom whose predicate is defined in Prolog
 * (either builtin or defined in a standard library).
 */
builtin(G):-
  builtin_int(G),!.

builtin_int(average(_L,_Av)).
builtin_int(G):-
  predicate_property(G,built_in).
builtin_int(G):-
  predicate_property(G,imported_from(lists)).
builtin_int(G):-
  predicate_property(G,imported_from(apply)).
builtin_int(G):-
  predicate_property(G,imported_from(nf_r)).
builtin_int(G):-
  predicate_property(G,imported_from(matrix)).
builtin_int(G):-
  predicate_property(G,imported_from(clpfd)).

average(L,Av):-
        sum_list(L,Sum),
        length(L,N),
        Av is Sum/N.


term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true)])) :-
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  M:local_setting(depth_bound,true),
  ((Head:-Body) \= ((system:term_expansion(_,_) ):- _ )),
  Head = (H:_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,AC,ACAnd,DB,[],_Vars,BodyList2,Module,M),
  append([phil:onec(AC)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_var
  (M:local_setting(single_var,true)->
    generate_clause_db(H,Body2,[],R,Probs,DB,ACAnd,0,Clauses,Module,M)
  ;
    generate_clause_db(H,Body2,VC,R,Probs,DB,ACAnd,0,Clauses,Module,M)
  ).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true)])) :-
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  M:local_setting(depth_bound,false),
  ((Head:-Body) \= ((system:term_expansion(_,_) ):- _ )),
  Head = (H:_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,AC,ACAnd,[],_Vars,BodyList2,Module,M),
  append([phil:onec(AC)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_var
  (M:local_setting(single_var,true)->
    generate_clause(H,Body2,[],R,Probs,ACAnd,0,Clauses,Module,M)
  ;
    generate_clause(H,Body2,VC,R,Probs,ACAnd,0,Clauses,Module,M)
  ).


  

term_expansion_int((Head :- Body),M,(Clauses,[def_rule(Head,BodyList,true)])) :-
  % definite clause senza DB
    %M:local_setting(compiling,on),
    %trace,
    ((Head:-Body) \= ((system:term_expansion(_,_)) :- _ )),!,
    list2and(BodyList, Body),
    process_body(BodyList,AC,AC1,[],_Vars,BodyList2,Module,M),
    append([phil:onec(AC)],BodyList2,BodyList3),
    list2and(BodyList3,Body2),
    add_bdd_arg(Head,AC1,Module,Head1),
    Clauses=(Head1 :- Body2).





  add_bdd_arg(A,BDD,A1):-
    A=..[P|Args],
    append(Args,[BDD],Args1),
    A1=..[P|Args1].
  
  
  add_bdd_arg_db(A,BDD,DB,A1):-
    A=..[P|Args],
    append(Args,[DB,BDD],Args1),
    A1=..[P|Args1].
  
  
  add_bdd_arg(A,BDD,Module,A1):-
    A=..[P|Args],
    append(Args,[BDD],Args1),
    A1=..[P,Module|Args1].
    

/*-----------*/



:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(phil:induce_hplp_par(_,_) ,[]).
sandbox:safe_meta(phil:induce_hplp(_,_), []).
sandbox:safe_meta(phil:get_node(_,_), []).
sandbox:safe_meta(phil:test_prob_hplp(_,_,_,_,_,_), []).
sandbox:safe_meta(phil:test_hplp(_,_,_,_,_,_,_), []).
sandbox:safe_meta(phil:inference_hplp(_,_,_,_), []).
sandbox:safe_meta(phil:inference_hplp(_,_,_), []).
sandbox:safe_meta(phil:set_hplp(_,_), []).
sandbox:safe_meta(phil:setting_hplp(_,_), []).



test_no_area(TestSet,M,NPos,NNeg,CLL,Results):-
  test_folds(TestSet,M,[],Results,0,NPos,0,NNeg,0,CLL).


test_folds([],_M,LG,LG,Pos,Pos,Neg,Neg,CLL,CLL).

test_folds([HT|TT],M,LG0,LG,Pos0,Pos,Neg0,Neg,CLL0,CLL):-
  test_1fold(HT,M,LG1,Pos1,Neg1,CLL1),
  append(LG0,LG1,LG2),
  Pos2 is Pos0+Pos1,
  Neg2 is Neg0+Neg1,
  CLL2 is CLL0+CLL1,
  test_folds(TT,M,LG2,LG,Pos2,Pos,Neg2,Neg,CLL2,CLL).

test_1fold(F,M,LGOrd,Pos,Neg,CLL1):-
  find_ex(F,M,LG,Pos,Neg),
  compute_CLL_atoms(LG,M,0,0,CLL1,LG1),
  keysort(LG1,LGOrd).


find_ex(DB,M,LG,Pos,Neg):-
  findall(P/A,M:output(P/A),LP),
  M:local_setting(neg_ex,given),!,
  find_ex_pred(LP,M,DB,[],LG,0,Pos,0,Neg).

find_ex(DB,M,LG,Pos,Neg):-
  findall(P/A,M:output(P/A),LP),
  M:local_setting(neg_ex,cw),
  find_ex_pred_cw(LP,M,DB,[],LG,0,Pos,0,Neg).


find_ex_pred([],_M,_DB,LG,LG,Pos,Pos,Neg,Neg).

find_ex_pred([P/A|T],M,DB,LG0,LG,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  find_ex_db(DB,M,At,LG0,LG1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred(T,M,DB,LG1,LG,Pos1,Pos,Neg1,Neg).

find_ex_db([],_M,_At,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db([H|T],M,At,LG0,LG,Pos0,Pos,Neg0,Neg):-
  At=..[P|L],
  At1=..[P,H|L],
  findall(At1,M:At1,LP),
  findall(\+ At1,M:neg(At1),LN),
  length(LP,NP),
  length(LN,NN),
  append([LG0,LP,LN],LG1),
  Pos1 is Pos0+NP,
  Neg1 is Neg0+NN,
  find_ex_db(T,M,At,LG1,LG,Pos1,Pos,Neg1,Neg).


find_ex_pred_cw([],_M,_DB,LG,LG,Pos,Pos,Neg,Neg).

find_ex_pred_cw([P/A|T],M,DB,LG0,LG,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  findall(Types,get_types(At,M,Types),LT),
  append(LT,LLT),
  remove_duplicates(LLT,Types1),
  find_ex_db_cw(DB,M,At,Types1,LG0,LG1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred_cw(T,M,DB,LG1,LG,Pos1,Pos,Neg1,Neg).

get_types(At,_M,[]):-
  At=..[_],!.

get_types(At,M,Types):-
  M:modeh(_,At),
  At=..[_|Args],
  get_args(Args,Types).

get_types(At,M,Types):-
  M:modeh(_,HT,_,_),
  member(At,HT),
  At=..[_|Args],
  get_args(Args,Types).


get_args([],[]).

get_args([+H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([-H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([#H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([-#H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([H|T],[H|T1]):-
  get_args(T,T1).




get_constants([],_M,_Mod,[]).

get_constants([Type|T],M,Mod,[(Type,Co)|C]):-
  find_pred_using_type(Type,Mod,LP),
  find_constants(LP,M,Mod,[],Co),
  get_constants(T,M,Mod,C).

find_pred_using_type(T,M,L):-
  (setof((P,Ar,A),pred_type(T,M,P,Ar,A),L)->
    true
  ;
    L=[]
  ).

pred_type(T,M,P,Ar,A):-
  M:modeh(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

pred_type(T,M,P,Ar,A):-
  M:modeb(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

scan_args([+T|_],T,A,A):-!.

scan_args([-T|_],T,A,A):-!.

scan_args([#T|_],T,A,A):-!.

scan_args([-#T|_],T,A,A):-!.

scan_args([_|Tail],T,A0,A):-
  A1 is A0+1,
  scan_args(Tail,T,A1,A).

find_constants([],_M,_Mod,C,C).

find_constants([(P,Ar,A)|T],M,Mod,C0,C):-
  gen_goal(1,Ar,A,Args,ArgsNoV,V),
  G=..[P,M|Args],
  (setof(V,ArgsNoV^call_goal(Mod,G),LC)->
    true
  ;
    LC=[]
  ),
  append(C0,LC,C1),
  remove_duplicates(C1,C2),
  find_constants(T,M,Mod,C2,C).

call_goal(M,G):-
  M:G.

gen_goal(Arg,Ar,_A,[],[],_):-
  Arg =:= Ar+1,!.

gen_goal(A,Ar,A,[V|Args],ArgsNoV,V):-!,
  Arg1 is A+1,
  gen_goal(Arg1,Ar,A,Args,ArgsNoV,V).

gen_goal(Arg,Ar,A,[ArgV|Args],[ArgV|ArgsNoV],V):-
  Arg1 is Arg+1,
  gen_goal(Arg1,Ar,A,Args,ArgsNoV,V).



find_ex_db_cw([],_M,_At,_Ty,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db_cw([H|T],M,At,Types,LG0,LG,Pos0,Pos,Neg0,Neg):-
  get_constants(Types,H,M,C),
  At=..[P|L],
  get_types(At,M,TypesA),!,
  length(L,N),
  length(LN,N),
  At1=..[P,H|LN],
  findall(At1,M:At1,LP),
  (setof(\+ At1,neg_ex(LN,M,TypesA,At1,C),LNeg)->true;LNeg=[]),
  length(LP,NP),
  length(LNeg,NN),
  append([LG0,LP,LNeg],LG1),
  Pos1 is Pos0+NP,
  Neg1 is Neg0+NN,
  find_ex_db_cw(T,M,At,Types,LG1,LG,Pos1,Pos,Neg1,Neg).

neg_ex([],M,[],At1,_C):-
  \+ M:At1.

neg_ex([H|T],M,[HT|TT],At1,C):-
  member((HT,Co),C),
  member(H,Co),
  neg_ex(T,M,TT,At1,C).

compute_CLL_atoms([],_M,_N,CLL,CLL,[]):-!.

compute_CLL_atoms([],_M,_N,CLL,CLL,[]):-!.

compute_CLL_atoms([\+ H|T],M,N,CLL0,CLL1,[PG- (\+ H)|T1]):-!,
  findall(P,M:rule(_R,[_:P|_],_BL,_Lit),LR),
  abolish_all_tables,
  get_node(H,M,Circuit),!,
  %writeln(Circuit),
  length(LR,NR),
  forward(Circuit,LR,NR,PG),
  PG1 is 1-PG,
  (PG1=:=0.0->
    setting_sc(logzero,LZ),
    CLL2 is CLL0+LZ
  ;
    CLL2 is CLL0+ log(PG1)
  ),
  N1 is N+1,
  compute_CLL_atoms(T,M,N1,CLL2,CLL1,T1).

compute_CLL_atoms([H|T],M,N,CLL0,CLL1,[PG-H|T1]):-
  findall(P,M:rule(_R,[_:P|_],_BL,_Lit),LR),
  abolish_all_tables,
  get_node(H,M,Circuit),!,
  length(LR,NR),
  forward(Circuit,LR,NR,PG),
  %writeln(Circuit),
  
  (PG=:=0.0->
    M:local_setting(logzero,LZ),
    CLL2 is CLL0+LZ
  ;
    CLL2 is CLL0+ log(PG)
  ),
  N1 is N+1,
  compute_CLL_atoms(T,M,N1,CLL2,CLL1,T1).



write2(M,A):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write(A)
  ;
    true
  ).

write3(M,A):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write(A)
  ;
    true
  ).

nl2(M):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    nl
  ;
    true
  ).

nl3(M):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    nl
  ;
    true
  ).

format2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    format(A,B)
  ;
    true
  ).

format3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    format(A,B)
  ;
    true
  ).

write_rules2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_rules(A,B)
  ;
    true
  ).

write_rules3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_rules(A,B)
  ;
    true
  ).


write_disj_clause2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_disj_clause(A,B)
  ;
    true
  ).

write_disj_clause3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_disj_clause(A,B)
  ;
    true
  ).

write_body2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_body(A,B)
  ;
    true
  ).

write_body3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_body(A,B)
  ;
    true
  ).


tab(M,A/B,P):-
  length(Args0,B),
  (M:local_setting(depth_bound,true)->
    append(Args0,[_,_,lattice(phil:orc/3)],Args)
  ;
    append(Args0,[_,lattice(phil:orc/3)],Args)
  ),
  P=..[A|Args],
  PT=..[A|Args0],
  assert(M:tabled(PT)).

/**
 * zero_clause(+Module:atom,+PredSpec:pred_spec,-ZeroClause:term) is det
 *
 * Generates the zero clause for predicate PredSpec. 
 * Module is the module of the input file.
 */
zero_clause(M,A/B,(H:-maplist(nonvar,Args0),phil:zeroc(AC))):-
  B1 is B+1,
  length(Args0,B1),
  (M:local_setting(depth_bound,true)->
    ExtraArgs=[_,or([AC])]
  ;
    ExtraArgs=[or([AC])]
  ),
  append(Args0,ExtraArgs,Args),
  H=..[A|Args].



system:term_expansion((:- phil), []) :-!,
  prolog_load_context(module, M),
  retractall(M:local_setting(_,_)),
  findall(local_setting(P,V),default_setting_hplp(P,V),L),
  assert_all(L,M,_),
  assert(input_mod_hplp(M)),
  retractall(M:rule_sc_n(_)),
  assert(M:rule_sc_n(0)),
  M:dynamic((modeh/2,modeh/4,fixed_rule/3,banned/2,lookahead/2,
    lookahead_cons/2,lookahead_cons_var/2,'$prob'/2,output/1,input/1,input_cw/1,
    ref_clause/1,ref/1,model/1,neg/1,rule/5,determination/2,
    bg_on/0,bg/1,bgc/1,in_on/0,in/1,inc/1,int/1,v/3,
    query_rule/4,database/1,
    zero_clauses/1,tabled/1)),
  retractall(M:tabled(_)),
  style_check(-discontiguous).

system:term_expansion(end_of_file, C) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  make_dynamic(M),
  findall(LZ,M:zero_clauses(LZ),L0),
  append(L0,L),
  retractall(M:zero_clauses(_)),
%  retractall(M:tabled(_)),
  %retractall(input_mod_hplp(M)),
  append(L,[(:- style_check(+discontiguous)),end_of_file],C).

system:term_expansion((:- begin_bg), []) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  assert(M:bg_on).

system:term_expansion(C, M:bgc(C)) :-
  prolog_load_context(module, M),
  C\= (:- end_bg),
  input_mod_hplp(M),
  M:bg_on,!.

system:term_expansion((:- end_bg), []) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  retractall(M:bg_on),
  findall(C,M:bgc(C),L),
  retractall(M:bgc(_)),
  (M:bg(BG0)->
    retract(M:bg(BG0)),
    append(BG0,L,BG),
    assert(M:bg(BG))
  ;
    assert(M:bg(L))
  ).

system:term_expansion((:- begin_in), []) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  assert(M:in_on).

system:term_expansion(C, M:inc(C)) :-
  prolog_load_context(module, M),
  C\= (:- end_in),
  input_mod_hplp(M),
  M:in_on,!.

system:term_expansion((:- end_in), []) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  retractall(M:in_on),
  findall(C,M:inc(C),L),
  retractall(M:inc(_)),
  (M:in(IN0)->
    retract(M:in(IN0)),
    append(IN0,L,IN),
    assert(M:in(IN))
  ;
    assert(M:in(L))
  ).

system:term_expansion(output(P/A), [output(P/A)|TabDir]) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  tab(M,P/A,P1),
  zero_clause(M,P/A,Z),
  system:term_expansion((:- table P1),TabDir),
  assert(M:zero_clauses([Z])).

system:term_expansion(input(P/A), [input(P/A)|TabDir]):-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  tab(M,P/A,P1),
  zero_clause(M,P/A,Z),
  system:term_expansion((:- table P1),TabDir),
  assert(M:zero_clauses([Z])).

system:term_expansion(begin(model(I)), []) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  retractall(M:model(_)),
  assert(M:model(I)),
  assert(M:int(I)).

system:term_expansion(end(model(_I)), []) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),!,
  retractall(M:model(_)).

system:term_expansion(At, A) :-
  prolog_load_context(module, M),
  input_mod_hplp(M),
  M:model(Name),
  At \= (_ :- _),
  At \= end_of_file,
  (At=neg(Atom)->
    Atom=..[Pred|Args],
    Atom1=..[Pred,Name|Args],
    A=neg(Atom1)
  ;
    (At=prob(Pr)->
      A=prob(Name,Pr)
    ;
      At=..[Pred|Args],
      Atom1=..[Pred,Name|Args],
      A=Atom1
    )
  ).
