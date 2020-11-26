% A SIMPLE STRIPS PLANNER USING STRIPS NOTATION

%achieve_all(Gs,S0,S1,U0,U1) is true if every element of list Gs can be 
%   achieved  going from state S0 to state S1.
% U0 is the bound on the number of actions going into achieve_all 
%   and U1 is the limit coming out

achieve_all([],World,World,U,U).
achieve_all(Gs,W0,W2,U0,U2) <-
   remove(G1,Gs,Gr) &
   achieve(G1,W0,W1,U0,U1) &
   achieve_all(Gr,W1,W2,U1,U2).

% achieve(G,S0,S1) is true if goal G can be achieved going from S0 to S1

achieve(G,W,W,U,U) <-              % goals already true
   true_in(G,W).
achieve(G,W0,W1,U0,U1) <-          % derived relations
   clause(G, Body) &
   achieve_all(Body,W0,W1,U0,U1).
achieve(A \= B,W,W,U,U) <-         % inequality constraints
   A \= B.
achieve(G,W0,do(Act,W1),U0,U2) <-  % primitive relations
   U0>0 &
   U1 is U0-1 &
   achieves(Act,G) &
   preconditions(Act,PreAct) &
   achieve_all(PreAct,W0,W1,U1,U2).

% remove(Elt,List,RemainingElts).
remove(X,[X|Y],Y).

% true_in(Goal,State) is true if Goal is true in State.
true_in(G,init) <-
   holds(G,init).
true_in(G,do(A,_)) <-
   achieves(A,G).
true_in(G,do(A,S)) <-
   true_in(G,S) &
   ~ deletes(A,G).

% TRY THE FOLLOWING QUERIES with delrob_strips.pl:
% ask achieve(carrying(rob,k1),init,S,10,_).
% ask achieve(at(k1,lab2),init,S,6,N).
% ask achieve_all([carrying(rob,parcel),sitting_at(rob,lab2)],init,S,10,N).
% ask achieve_all([sitting_at(rob,lab2),carrying(rob,parcel)],init,S,10,N).
%    is the plan returned correct?
