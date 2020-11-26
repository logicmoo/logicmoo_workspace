:- include(test_header).




end_of_file.





% Tests that you can express regular semweb cardinatility though in FOL

/*



#import: written-on(word,paper).


my-minus-one(N,M) <->  (N > 1)  &  (M = N-1)


legal-pad(paper) <-> min-elements(1, paper)

exactly-n-elements(1,paper) <->
(exists paper word.  
    (   written-on(word,paper) 
      & ~(exists other-word.
           (  written-on(other-word,paper) 
             & ~same(other-word,word)))))

exactly-n-elements(2,paper) <->
(exists paper word1 word2. 
  (  written-on(word1,paper) 
   & written-on(word2,paper) 
   & ~same(word1,word2)   
   & ~ (exists other-word. 
         (   written-on(other-word,paper) 
          & ~same(other-word,word1) 
          & ~same(other-word,word2)))))

exactly-n-elements(N,paper) <-> 
   range-elements(N,N,paper)  

range-elements(N,M,paper) <-> 
 (exists paper.
  ( min-elements(N,paper) 
    & max-elements(M,paper)))

min-elements(1,paper) <-> 
 (exists paper word1.
     (written-on(word1,paper)))

min-elements(2,paper) <->
 (exists paper word1 word2. 
   (  written-on(word1,paper) 
    & written-on(word2,paper)
    & ~same(word1,word2)))

max-elements(2,paper) <->  
(exists paper word1 word2. 
   (  written-on(word1,paper) 
    & written-on(word2,paper) 
    &  ~(exists other-word.
          (  written-on(other-word,paper) 
            & ~same(other-word,word1)
            & ~same(other-word,word2)))))

% exists a paper with no elements
exactly-n-elements(0,paper) <->
  (exists paper
     ~(exists word1. written-on(word1,paper)))

max-elements(1,paper) <->
 exactly-n-elements(0,paper) v exactly-n-elements(1,paper)

containsAtLeastOneUnique(paper1,paper2) <->
  (exists word. (written-on(word,paper1) -> ~written-on(word,paper2)))

disjoint(paper1,paper2) <->
  ~(exists word. (written-on(word,paper1) & written-on(word,paper2)))

subset(paper1,paper2) <-> 
    (forall word.
      written-on(word,paper1) -> written-on(word,paper2) )

union(paper1,paper2,paper) <->
 (forall word.
   (written-on(word,paper) <-> 
       (written-on(word,paper1) v written-on(word,paper2))))

union(paper1,paper2,paper) <->
  (exists scratchpad. 
      union(paper1,paper2,scratchpad)
     & ~containsAtLeastOneUnique(paper,scratchpad))


union-disjoint(paper1,paper2,paper) <-> 
   (   union(paper1,paper2,paper) 
     & disjoint(paper1,paper2))


min-elements(4,paper) <-> 
  (exists paper1 paper2.
     min-elements(2,paper1) 
   & min-elements(2,paper2)
   & union-disjoint(paper1,paper2))

min-elements(N,paper) <-> 
  (exists paper1 paper2.
     min-elements(1,paper1) 
   & min-elements(M,paper2)
   & legal-pad(paper2)
   & union-disjoint(paper1,paper2)
   & my-minus-one(N,M))


max-elements(N,paper) <-> 
  (exists paper1 paper2.
     max-elements(1,paper1) 
   & max-elements(M,paper2)
   & legal-pad(paper2)
   & union-disjoint(paper1,paper2)
   & my-minus-one(N,M))


equal_papers_v2(paper1,paper2) <->
   ( ~containsAtLeastOneUnique(paper1,paper2)
     & ~containsAtLeastOneUnique(paper2,paper1)
     & legal-pad(paper1)
     & legal-pad(paper2))

equal_papers_v1(paper1,paper2) <->
 ( legal-pad(paper1)
   & (forall word. 
      (written-on(word,paper1) <-> written-on(word,paper2))))



*/


:- debug_logicmoo(_).
:- nodebug_logicmoo(http(_)).
:- begin_pfc.

house(red).
house(blue).
% house(green).

:- must((existing_count(X,house(X),EC),EC==2)).

singleValuedInArg(existing_count,1).

exists_count(3,X,house(X)).

exists_count(N,X,G),{(need_plugs(X,G,EP); EP=0),existing_count(X,G,EC),Need is N-(EC-EP), copy_term(G,GG)}
  ==>
    (need_plugs(Need,X,G),
    (\+ GG ==> exists_count(N,X,G)),
    (   GG ==> exists_count(N,X,G))).

need_plugs(EP,X,G) ==> {between(1,EP,Plug),copy_term(G,GG,_),X=skFn(Plug,GG)},G.

:- listing(exists_count).
:- listing(need_plugs).
:- listing(house).

/*

exists_count(3, A, house(A)).


need_plugs(1, A, house(A)).

house(red).
house(blue).
house(skFn(1, house(_))).

*/



:- must((existing_count(X,house(X),EC),EC==3)).
:- break.

