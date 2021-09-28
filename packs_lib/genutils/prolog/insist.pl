:- module(insist, [insist/1,insist/2, insist/3]).
/** <module> Tools for enforcing conditions
   
   This module provides what are usually called assertions in 
   other languages, but of course the word 'assert' is already taken
   in Prolog. insist/1 and insist/2 can be used to wrap any goal 
   that is not allowed to fail.
 */

:- meta_predicate insist(0,:), insist(0), insist(+,0,:).

insist(G) :- insist(G,failed(G)).
insist(G,Ex) :- call(G) -> true; throw(Ex).
insist(det,G,Ex) :- insist(G,Ex).
insist(multi,G,Ex) :- call(G) *-> true; throw(Ex).

user:goal_expansion(insist(G),X) :- user:goal_expansion(insist(G,failed(G)),X).
user:goal_expansion(insist(G,Ex),(G->true;throw(Ex))).
user:goal_expansion(insist(det,G,Ex),X) :- user:goal_expansion(insist(G,Ex),X).
user:goal_expansion(insist(multi,G,Ex),(G*->true;throw(Ex))).
