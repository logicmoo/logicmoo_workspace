:- include(test_header).




my-minus-one(N,M) <->  (N > 1)  &  (M = N-1)

equiv(kb,logsent1,logsent2) <->
  true-in((logsent1 <-> logsent2),kb)

% Lits in KBs are considered true
consistent(kb) <-> min-lits(1, kb)

exactly-N-lits(1,kb) <->
(exists kb logsent.  
    (   true-in(logsent,kb) 
      & ~(exists other-logsent.
           (  true-in(other-logsent,kb) 
             & ~equiv(kb,other-logsent,logsent)))))

exactly-N-lits(2,kb) <->
(exists kb logsent1 logsent2. 
  (  true-in(logsent1,kb) 
   & true-in(logsent2,kb) 
   & ~equiv(kb,logsent1,logsent2)   
   & ~ (exists other-logsent. 
         (  true-in(other-logsent,kb) 
          & ~equiv(kb,other-logsent,logsent1) 
          & ~equiv(kb,other-logsent,logsent2)))))

exactly-N-lits(N,kb) <-> 
   range-lits(N,N,kb)  


range-lits(N,M,kb) <-> 
 (exists kb.
  ( min-lits(N,kb) 
    & max-lits(M,kb)))

min-lits(1,kb) <-> 
 (exists kb logsent1.
     (true-in(logsent1,kb)))

min-lits(2,kb) <->
 (exists kb logsent1 logsent2. 
   (  true-in(logsent1,kb) 
    & true-in(logsent2,kb)
    & ~equiv(kb,logsent1,logsent2)))

max-lits(2,kb) <->  
(exists kb logsent1 logsent2. 
   (  true-in(logsent1,kb) 
    & true-in(logsent2,kb) 
    &  ~(exists other-logsent.
          (  true-in(other-logsent,kb) 
            & ~equiv(kb,other-logsent,logsent1)
            & ~equiv(kb,other-logsent,logsent2)))))

% exists a kb with no lits?
exactly-N-lits(0,kb) <->
  (exists kb
     ~(exists logsent1. true-in(logsent1,kb)))

max-lits(1,kb) <->
 exactly-N-lits(0,kb) v exactly-N-lits(1,kb)

difference_at_least_1_truths(kb1,kb2) <->
  (exists logsent. (true-in(logsent,kb1) -> ~true-in(logsent,kb2)))

disjoint_truths(kb1,kb2) <->
  ~(exists logsent. (true-in(logsent,kb1) & true-in(logsent,kb2)))

subset_truths(kb1,kb2) <-> 
    (forall logsent.
      true-in(logsent,kb1) -> true-in(logsent,kb2) )

union_s_v2(kb1,kb2,kb) <->
 (forall logsent.
   (true-in(logsent,kb) <-> 
       (true-in(logsent,kb1) v true-in(logsent,kb2))))

union_truths(kb1,kb2,kb) <->
  (exists scratchpad. 
      union_truths(kb1,kb2,scratchpad)
     & ~difference_at_least_1_truths(kb,scratchpad))


union-disjoint_truths(kb1,kb2,kb) <-> 
   (   union_truths(kb1,kb2,kb) 
     & disjoint_truths(kb1,kb2))


min-lits(4,kb) <-> 
  (exists kb1 kb2.
     min-lits(2,kb1) 
   & min-lits(2,kb2)
   & union-disjoint_truths(kb1,kb2))

min-lits(N,kb) <-> 
  (exists kb1 kb2.
     min-lits(1,kb1) 
   & min-lits(M,kb2)
   & consistent(kb2)
   & union-disjoint_truths(kb1,kb2)
   & my-minus-one(N,M))


max-lits(N,kb) <-> 
  (exists kb1 kb2.
     max-lits(1,kb1) 
   & max-lits(M,kb2)
   & consistent(kb2)
   & union-disjoint_truths(kb1,kb2)
   & my-minus-one(N,M))


equal_v1_truths(kb1,kb2) <->
   ( ~difference_at_least_1_truths(kb1,kb2)
     & ~difference_at_least_1_truths(kb2,kb1)
     & consistent(kb1)
     & consistent(kb2))

equal_v2_truths(kb1,kb2) <->
 ( consistent(kb1)
   & (forall logsent. 
      (true-in(logsent,kb1) <-> true-in(logsent,kb2))))





