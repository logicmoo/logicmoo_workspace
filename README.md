trill
=====

trill is a tableau reasoner able to compute probability of queries from probabilistic knowledge bases.
 
You can try it online at http://trill.lamping.unife.it/

Installation
-------------
In case of problems when compiling cudd, please check the configure and Makefile in cudd-3.0.0/ directory and set the correct flags for gcc. 
Then run the goal
pack_rebuild(trill).

Example of use
---------------

    $ cd <pack>/trill/prolog/examples
    $ swipl
    ?- [peoplePets].
    ?- prob_instanceOf('natureLover','Kevin',Prob).
