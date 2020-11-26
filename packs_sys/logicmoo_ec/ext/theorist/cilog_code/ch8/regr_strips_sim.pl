% A SIMPLE REGRESSION PLANNER FOR ACTIONS IN STRIPS NOTATION

% solve(G,NS,P) is true if P is a plan to solve goal G that uses 
% less than NS steps. G is a list of atomic subgoals.

solve(G,_,init) <-
   solved(G).

solve(G,NAs,do(A,Pl)) <-
   NAs > 0 &
   useful(G,A) &
   wp(G,A,G1) &
   NA1 is NAs-1 &
   solve(G1,NA1,Pl).

% solved(G) is true if goal list G is true initially
solved([]).
solved([G|R]) <-
   holds(G,init) &
   solved(R).

% useful(G,A) is true if action A is useful to solve a goal in goal list G
%useful([S|R],A) <-
%   holds(S,init) &
%   useful(R,A).
useful([S|R],A) <-
%   ~ holds(S,init) &
   useful(R,A).
useful([S|_],A) <-
   achieves(A,S).

% wp(G,A,G0) is true if G0 is the weakest precondition that needs to hold
% immediately before action A to ensure that G is true immediately after A
wp([],A,G1) <-
   preconditions(A,G) &
   filter_derived(G,[],G1).
wp([S|R],A,G1) <-
   wp(R,A,G0) &
   regress(S,A,G0,G1).

% regress(Cond,Act,SG0,SG1) is true if regressing Cond through Act
% starting with subgoals SG0 produces subgoals SG1
regress(S,A,G,G) <-
   achieves(A,S).
regress(S,A,G,G1) <-
   primitive(S) &
   ~ achieves(A,S) &
   ~ deletes(A,S) &
   insert(S,G,G1).

filter_derived([],L,L).
filter_derived([G|R],L,[G|L1]) <-
   primitive(G) &
   filter_derived(R,L,L1).
filter_derived([A \= B | R],L,L1) <-
   A \= B &
   filter_derived(R,L,L1).
filter_derived([G|R],L0,L2) <-
   clause(G, B) &
   filter_derived(R,L0,L1) &
   filter_derived(B,L1,L2).

regress_all([],_,G,G).
regress_all([S|R],A,G0,G2) <-
   regress(S,A,G0,G1) &
   regress_all(R,A,G1,G2).

% =============================================================================

% member(X,L) is true if X is a member of list L
member(X,[X|_]).
member(X,[_|L]) <-
   member(X,L).

% subset(L1,L2) is true if L1 is a subset of list L2
subset([],_).
subset([A|B],L) <-
   member(A,L) &
   subset(B,L).

% insert(E,L0,L1) inserts E into list L0 producing list L1.
% If E is already a member it is not added.
insert(A,[],[A]).
insert(A,[A|L],[A|L]).
insert(A,[B|L],[B|R]) <-
   A \= B &
   insert(A,L,R).

% =============================================================================

% TRY THE FOLLOWING QUERIES with delrob_strips.pl:
% ask solve([carrying(rob,k1)],5,P).
% ask solve([sitting_at(k1,lab2)],8,P).
% ask solve([carrying(rob,parcel),sitting_at(rob,lab2)],10,P).
% ask solve([sitting_at(rob,lab2),carrying(rob,parcel)],10,P).
