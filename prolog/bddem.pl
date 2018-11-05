:- module(bddem,[
  init_em/1,init_ex/2,init/1,end_em/1,end_ex/1,end/1,
  one/2,zero/2,and/4,or/4,bdd_not/3,
  ret_prob/3,equality/4,add_var/4,
  add_abd_var/4,ret_abd_prob/4,
  add_query_var/4,ret_map_prob/4,ret_vit_prob/4,
  make_query_var/3,create_dot_string/3,
  em/9,rand_seed/1
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
 * the EM algorithm
 * Context must have been returned by a call to init_em/1.
 * It frees the memory occupied by Context.
 */

/**
 * init_ex(++Context:int,--Environment:int) is det
 *
 * Initializes an enviroment data structure for storing a BDD.
 * Context is an integer that is a pointer to a context data structure
 * created using init/3.
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
 * representing the one Boolean function
 */

/**
 * zero(++Environment:int,--Zero:int) is det
 *
 * Returns in Zero a pointer to a BDD belonging to environment Environment
 * representing the zero Boolean function
 */

/**
 * and(++Environment:int,++A:int,++B:int,--AandB:int) is det
 *
 * Returns in AandB a pointer to a BDD belonging to environment Environment
 * representing the conjunction of BDDs A and B
 */

/**
 * or(++Environment:int,++A:int,++B:int,--AorB:int) is det
 *
 * Returns in AorB a pointer to a BDD belonging to environment Environment
 * representing the disjunction of BDDs A and B
 */

/**
 * ret_prob(++Environment:int,++BDD:int,-Probability:float) is det
 *
 * Returns the Probability of BDD belonging to environment Environment
 */

/**
 * bdd_not(++Environment:int,++A:int,--NotA:int) is det
 *
 * Returns in NotA a pointer to a BDD belonging to environment Environment
 * representing the negation of BDD A
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
 * NumberOHeads values and probability distribution ProbabilityDistribution
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
 * to file FileName
 */

/**
 * rand_seed(+Seed:int) is det
 *
 * The pseudo-random number generator is initialized using the argument passed as Seed.
 * It calls the C function srand
 */
