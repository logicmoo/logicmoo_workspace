% A REGRESSION PLANNER FOR ACTIONS IN STRIPS NOTATION
% WITH LOOP DETECTION + HEURISTIC INFORMATION ON UNSATISFIABLE GOALS

% solve(G,AS,NS,P) is true if P is a plan to solve goal G that uses 
% less than NS steps.
% G is a list of atomic subgoals. AS is the list of ancestor goal lists.

solve(G,N,P) <-
   solve(G,[G],N,P).

solve(G,_,_,init) <-
   solved(G).

solve(G,AS,NAs,do(A,Pl)) <-
   NAs > 0 &
   satisfiable(G) &
   useful(G,A) &
   wp(G,A,G1) &
   ~ subgoal_loop(G1,AS) &
   NA1 is NAs-1 &
   solve(G1,[G1|AS],NA1,Pl).

% subgoal_loop(G,AS) is true if we are in a loop of subgoals to solve.
% This occurs if G is a more difficult to solve goal than one of its ancestors
subgoal_loop(G1,AS) <- 
   grnd(G1)& member(An,AS)&  subset(An,G1).

% solved(G) is true if goal list G is true initially
solved([]).
solved([G|R]) <-
   holds(G,init) &
   solved(R).

% satisfiable(G) is true if (based on a priori information) it is possible for
%  goal list G to be true all at once.
satisfiable(G) <-
   ~ unsatisfiable(G).

% useful(G,A) is true if action A is useful to solve a goal in goal list G
% we try first those subgoals that do not hold initially
useful([S|R],A) <-
   holds(S,init) &
   useful(R,A).
useful([S|_],A) <-
   achieves(A,S).
useful([S|R],A) <-
   ~ holds(S,init) &
   useful(R,A).

% domain specific rule about what may be useful to solve even if it was true
%  initially. 
useful(G,A) <-
   member(S,G) &
   member(S,[handempty])& % handempty is the only such goal in this domain
   holds(S,init) &
   achieves(S,A).

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
   dif(A,B) &
   filter_derived(R,L,L1).
filter_derived([G|R],L0,L2) <-
   (G <- B) &
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

notin(_,[]).
notin(A,[B|C]) <-
   dif(A,B) &
   notin(A,C).

% subset(L1,L2) is true if L1 is a subset of list L2
subset([],_).
subset([A|B],L) <-
   member(A,L) &
   subset(B,L).

% insert(E,L0,L1) inserts E into list L0 producing list L1.
% If E is already a member it is not added.
insert(A,[],[A]).
insert(A,[B|L],[A|L]) <- A==B.
insert(A,[B|L],[B|R]) <-
   ~ A == B &
   insert(A,L,R).
grnd(G) <-
   numbervars(G,0,_).

% =============================================================================
% DOMAIN SPECIFIC KNOWLEDGE
unsatisfiable(L) <-
   member(sitting_at(X1,Y1),L) &
   member(sitting_at(X2,Y2),L) &
   X1 == X2 &
   ~ (Y1=Y2).
unsatisfiable(L) <-
   member(sitting_at(X1,_),L) &
   member(carrying(_,Y2),L) &
   X1 == Y2.
unsatisfiable(L) <-
   member(carrying(X1,Y1),L) &
   member(carrying(X2,Y2),L) &
   Y1 == Y2 &
   (X1\=X2).

% TRY THE FOLLOWING QUERIES with delrob_strips.pl:
% solve([carrying(rob,k1)],5,P).
% solve([sitting_at(k1,lab2)],8,P).
% solve([carrying(rob,parcel),sitting_at(rob,lab2)],10,P).
% solve([sitting_at(rob,lab2),carrying(rob,parcel)],10,P).
