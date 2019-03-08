:- module(bddem,[
  init_em/1,init_ex/2,init/1,end_em/1,end_ex/1,end/1,
  one/2,zero/2,and/4,or/4,bdd_not/3,
  ret_prob/3,equality/4,add_var/4,
  add_abd_var/4,ret_abd_prob/4,
  add_query_var/4,ret_map_prob/4,ret_vit_prob/4,
  onec/2,zeroc/2,andc/4,andcnf/4,bdd_notc/3,
  orc/3,
  ret_probc/3,equalityc/4,
  or_list/3,
  or_listc/3,
  make_query_var/3,create_dot/3,create_dot_string/3,
  em/9,rand_seed/1,
  gamma_sample/3,
  gauss_sample/3,
  uniform_sample/1,
  dirichlet_sample/2,
  symmetric_dirichlet_sample/3,
  discrete_sample/2,
  initial_values/2,
  add_decision_var/3,
  probability_dd/3,
  add_prod/4,
  add_sum/4,
  ret_strategy/4 
    ]).
/** <module> bddem


Module for manipulating Binary Decision Diagrams.

It contains programs for building BDDs, for computing probabilities and for performing Expectation Maximization.

@author Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Fabrizio Riguzzi
*/
:-use_foreign_library(foreign(bddem),install).

/**
 * init_em(--Context:int) is det
 *
 * Initializes a data structure for performing parameter learning.
 * It returns an integer in Context that is a pointer to a
 * context data structure for performing the EM algorithm.
 */

/**
 * end_em(++Context:int) is det
 *
 * Terminates the context data structure for performing parameter learning.
 * Context is a pointer to a context data structure for performing
 * the EM algorithm.
 * Context must have been returned by a call to init_em/1.
 * It frees the memory occupied by Context.
 */

/**
 * init_ex(++Context:int,--Environment:int) is det
 *
 * Initializes an enviroment data structure for storing a BDD.
 * Context is an integer that is a pointer to a context data structure
 * created using init_em/1.
 * Returns an integer Environment that is a pointer to a data structure for
 * storing a single BDD to be used for the EM algorithm.
 */

/**
 * end_ex(++Environment:int) is det
 *
 * Terminates the evnironment data structure for storing a BDD.
 * Environment is a pointer to a data structure returned by init_ex/2.
 * It frees the memory occupied by the BDD.
 */


/**
 * init(--Environment:int) is det
 *
 * Initializes a data structure for storing a single BDD.
 * Returns an integer Environment that is a pointer to a data structure for
 * storing a single BDD to be used for inference only (no learning).
 */

/**
 * end(++Environment:int) is det
 *
 * Terminates the environment data structure for storing a single BDD.
 * Environment is a pointer to a data structure returned by a call
 * to init/1.
 */

/**
 * one(++Environment:int,--One:int) is det
 *
 * Returns in One a pointer to a BDD belonging to environment Environment
 * representing the one Boolean function.
 */

/**
 * zero(++Environment:int,--Zero:int) is det
 *
 * Returns in Zero a pointer to a BDD belonging to environment Environment
 * representing the zero Boolean function.
 */

/**
 * and(++Environment:int,++A:int,++B:int,--AandB:int) is det
 *
 * Returns in AandB a pointer to a BDD belonging to environment Environment
 * representing the conjunction of BDDs A and B.
 */

/**
 * or(++Environment:int,++A:int,++B:int,--AorB:int) is det
 *
 * Returns in AorB a pointer to a BDD belonging to environment Environment
 * representing the disjunction of BDDs A and B.
 */

/**
 * ret_prob(++Environment:int,++BDD:int,-Probability:float) is det
 *
 * Returns the Probability of BDD belonging to environment Environment.
 */

/**
 * ret_map_prob(++Environment:int,++BDD:int,-Probability:float,-MAPState:list) is det
 *
 * Returns the MAP state MPAState of BDD and its Probability.
 * BDD belongs to environment Environment.
 */

/**
 * ret_abd_prob(++Environment:int,++BDD:int,-Probability:float,-Explanation:list) is det
 *
 * Returns the abductive Explanation of BDD and its Probability.
 * BDD belongs to environment Environment.
 */

/**
 * ret_vit_prob(++Environment:int,++BDD:int,-Probability:float,-MPEState:list) is det
 *
 * Returns the MPE (Viterbi) state MPEState of BDD and its Probability.
 * BDD belongs to environment Environment.
 */

/**
 * bdd_not(++Environment:int,++A:int,--NotA:int) is det
 *
 * Returns in NotA a pointer to a BDD belonging to environment Environment
 * representing the negation of BDD A.
 */

/**
 * equality(++Environment:int,++Variable:int,++Value:int,--BDD:int) is det
 *
 * Returns in BDD the BDD belonging to environment Environment
 * that represents the equation Variable=Value.
 */

/**
 * em(++Context:int,++RuleInfo:list,++ListOfBDDs:list,++EA:float,++ER:float,++Iterations:int,-LL:float,-Parameters:list,-ExampleProbabilities:list) is det
 *
 * NumberOfHeads is a list of terms, one for each rule. Each term is either
 * an integer, indicating the number
 * of head atoms in the rule, or a list [N] where N
 * is the number of head atoms. In the first case, the parameters of the rule are tunable,
 * in the latter they are fixed.

 * Performs EM learning.
 * Takes as input the Context, information on the rules,
 * a list of BDDs each representing one example,
 * the minimum absolute difference EA and relative difference ER between the
 * log likelihood of examples in two different iterations and the maximum number of iterations
 * Iterations.
 * RuleInfo is a list of elements, one for each rule, with are either
 *
 * * an integer, indicating the number of heads, in which case the parameters of the
 *   corresponding rule should be randomized,
 * * a list of floats, in which case the parameters should be set to those indicated
 *   in the list and not changed during learning (fixed parameters)
 * * [a list of floats], in which case the initial values of the parameters should
 *   be set to those indicated
 *   in the list and changed during learning (initial values of the parameters)
 * Returns the final log likelihood of examples LL, the list of new Parameters
 * and a list with the final probabilities of each example.
 * Parameters is a list whose elements are of the form [N,P] where N is the rule
 * number and P is a list of probabilities, one for each head atom of rule N,
 * in reverse order.
 */


/**
 * add_var(++Environment:int,++ProbabilityDistribution:list, ++Rule:int,-Variable:int) is det.
 *
 * Returns in Variable the index of a new random variable in Environment with
 * NumberOHeads values and probability distribution ProbabilityDistribution.
 */

/**
 * add_abd_var(++Environment:int,++ProbabilityDistribution:list, ++Rule:int,-Variable:int) is det.
 *
 * Returns in Variable the index of a new abducible random variable in Environment with
 * NumberOHeads values and probability distribution ProbabilityDistribution.
 */

/**
 * add_query_var(++Environment:int,++ProbabilityDistribution:list, ++Rule:int,-Variable:int) is det.
 *
 * Returns in Variable the index of a new random variable to be queried in MAP inference with
 * NumberOHeads values and probability distribution ProbabilityDistribution.
 * The variable belongs to Environment. 
 */

/**
 * make_query_var(++Environment:int,+Variable:int,--BDD:int) is det.
 *
 * Makes Variable belonging to Environment a query random variable for MAP inference.
 * Returns in BDD the diagram of the formula encoding the required constraints among the
 * Boolean random variable that represent Variable. 
 */

/**
 * create_dot_string(++Env:int,++BDD:int,-Dot:string) is det
 *
 * The predicate returns the BDD in dot format.
 */

/**
 * create_dot(++Env:int,++BDD:int,++File:string) is det
 *
 * The predicate writes the BDD in dot format to
 * to file FileName.
 */

/**
 * rand_seed(+Seed:int) is det
 *
 * The pseudo-random number generator is initialized using the argument passed as Seed.
 * It calls the C function srand.
 */

/**
 * orc(++A:couple,++B:couple,--AorB:couple) is det
 *
 * A and B are couples (Environment, BDDA) and  (Environment, BDDB) respectively
 * Returns in AorB a couple (Environment, BDDAorB) where BDDAorB is pointer to a BDD belonging to environment Environment
 * representing the disjunction of BDDs BDDA and BDDB.
 */
orc((Env,A),(_,B),(Env,C)):-
  or(Env,A,B,C).

/**
 * onec(++Environment:int,--One:couple) is det
 *
 * Returns in One a couple (Environment,BDD) where BDD is pointer to a BDD belonging to environment Environment
 * representing the one Boolean function
 */
onec(Env,(Env,One)):-
  one(Env,One).

/**
 * zeroc(++Environment:int,--Zero:couple) is det
 *
 * Returns in Zero a couple (Environment,BDD) where BDD is pointer to a BDD belonging to environment Environment
 * representing the zero Boolean function
 */
zeroc(Env,(Env,Zero)):-
  zero(Env,Zero).

/**
 * andc(++Environment:int,++A:couple,++B:couple,--AandB:couple) is semidet
 * 
 * A and B are couples (Environment, BDDA) and  (Environment, BDDB) respectively
 * Returns in AandB a couple (Environment, BDDAandB) where BDDAandB is pointer to a BDD belonging to environment Environment
 * representing the conjunction of BDDs BDDA and BDDB.
 * fails if BDDB represents the zero function
 */
andc(Env,(_,A),(_,B),(Env,C)):-
  (zero(Env,B)->
    fail
  ;
    and(Env,A,B,C)
  ).

andcnf(Env,(_,A),(_,B),(Env,C)):-
  and(Env,A,B,C).
/**
 * bdd_notc(++Environment:int,++EBDD:couple,--NotEBDD:couple) is det
 *
 * EBDD is a couple (Environment,A)
 * Returns in NotEBDD a couple  (Environment,NotA) where NotA is
 * pointer to a BDD belonging to environment Environment
 * representing the negation of BDD A
 */
bdd_notc(Env,(_,A),(Env,NA)):-
  bdd_not(Env,A,NA).

/**
 * equalityc(++Environment:int,++Variable:int,++Value:int,--EBDD:couple) is det
 *
 * Returns in EBDD a couple (Environment,BDD) where BDD belongs to environment Environment
 * and represents the equation Variable=Value.
 */
equalityc(Env,V,N,(Env,B)):-
  equality(Env,V,N,B).

/**
 * ret_probc(++Environment:int,++EBDD:couple,-Probability:float) is det
 * 
 * EBDD is a couple (Environment,BDD)
 * Returns the Probability of BDD belonging to environment Environment
 * Uses 
 */
ret_probc(Env,(_,BDD),P):-
  ret_prob(Env,BDD,P).

/**
 * initial_values(++Environment:int,++Alpha:float) is det
 *
 * Sets the type of parameter initialization for EM on Environment:
 * if Alpha is 0.0, it uses a truncated Dirichlet process
 * if Alpha is a float > 0.0, it uses a symmetric Dirichlet distribution
 * with that value as parameter
 */

/**
 * or_list(++ListOfBDDs:list,++Environment,--BDD:int) is det
 *
 * Returns in BDD a pointer to a BDD belonging to environment Environment
 * representing the disjunction of all the BDDs in ListOfBDDs
 */
or_list([H],_Env,H):-!.

or_list([H|T],Env,B):-
  or_list1(T,Env,H,B).


or_list1([],_Env,B,B).

or_list1([H|T],Env,B0,B1):-
  or(Env,B0,H,B2),
  or_list1(T,Env,B2,B1).

/**
 * or_listc(++ListOfBDDs:list,++Environment,--BDD:int) is det
 *
 * Returns in BDD a couple (Env,B) with B a pointer to a
 * BDD belonging to environment Environment
 * representing the disjunction of all the BDDs in
 * ListOfBDDs (a list of couples (Env,BDD))
 */
or_listc([H],_Env,H):-!.

or_listc([H|T],Env,B):-
  or_listc1(T,Env,H,B).


or_listc1([],_Env,B,B).

or_listc1([H|T],Env,B0,B1):-
  orc(B0,H,B2),
  or_listc1(T,Env,B2,B1).

/**
 * gamma_sample(++Shape:float,++Scale:float,--Value:float) is det
 *
 * Returns a Value sampled from a gamma distribution with parameters Shape and Scale
 */

/**
 * gauss_sample(++Mean:float,++Variance:float,--Value:float) is det
 *
 * Returns a Value sampled from a Gaussian distribution with parameters Mean and Variance
 */

/**
 * uniform_sample(--Value:float) is det
 *
 * Returns a Value sampled from a uniform distribution in [0,1]
 */

/**
 * dirichlet_sample(++Alpha:list,--Value:list) is det
 *
 * Returns a Value sampled from a Dirichlet distribution with parameters Alpha.
 * Alpha and Value are lists of floating point numbers of the same length.
 */

/**
 * symmetric_dirichlet_sample(++Alpha:float,++K:int,--Value:list) is det
 *
 * Returns a Value sampled from a symmetric Dirichlet distribution with parameter Alpha.
 * K is the number of dimensions of the result.
 */
/**
 * discrete_sample(++Theta:list,--Value:int) is det
 *
 * Returns a Value sampled from a discrete distribution with parameters Theta.
 * Theta is a list of floating point numbers in [0,1] that sum to 1.
 * Value is in 0..(length(Theta)-1)
 */

/*
 * probability_dd(++Environment:int,++BDD:int,--ADD:int) is det
 * 
 * Converts the BDD passed as argument into an ADD.
*/

/*
  * add_prod(++Environment:int,++ADDIn:int,++Utility:int,--ADDOut:int) is det
  *
  * Multiply the ADD passed as argument with the Utility.
*/

/* 
 * add_sum(++Environment:int,++ADD1:int,++ADD2:int,--ADDOut:int) is det
 * 
 * Computes the sum ADD1+ADD2. Stores the result in ADDOut.
*/

/*
 * ret_strategy(++Environment:int,++ADD:int,--Decision:list,--Cost:int) is det
 *
 * Computes the optimal strategy.
*/

/**
 * add_decision_var(++Environment:int,-Variable:int) is det.
 *
 * Returns in Variable the index of a new decision variable in Environment
 */