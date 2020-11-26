% ***************************************************************
% HEURISTIC SEARCH ENGINE in CILog, from
% Computational Intelligence: A Logical Approach
% Copyright, Poole, Mackworth, Goebel, and Oxford University Press, 1997. 

% hsearch is an instance of the genertic search algorithm.

% Elements of the frontier are of the form: 
% node(Node,Path,Pathcost,Nodecost)
%   where Node is the current node, Path is the path found to Node,
%   Pathcost is the cost of the path and Nodecost is the `value' of the node,
%     for which ever search strategy we are using.

% hsearch(M,F,S) if method M from frontier F results in path S to goal.
%   This works for methods in {breadth,depth,astar,best,hdepth,shortest}.
%   Note that S is the list of nodes in the reverse order.

hsearch(M,F,[N|P]) <-
   choose(M,node(N,P,_,_),F,_) &
   is_goal(N).
hsearch(M,F,S) <-
   choose(M,node(N,P,PC,_),F,F1) &
   neighbours(N,NN) &
   add_paths2(M,N,NN,[N|P],PC,NN2) &
   hadd_to_frontier(M,NN2,F1,F2) &
   hsearch(M,F2,S).

% add_paths2(Method,Node,Neighs,Path,PathCost,NewFrontierElts) is true if
%   Method is a search method
%   Node is a node in the graph
%   Neighs is a list of neighbors of Node
%   Path is a path from the start to node N
%   PathCost is the cost of this path
%   NewFrontierElts is the list of elements that needs to be added to
%       the frontier for neighbor in Neighs
add_paths2(_,_,[],_,_,[]).
add_paths2(M,N,[NN|R],P,PC,[node(NN,P,NPC,NNC)|PR]) <-
   cost(N,NN,AC) &
   NPC is PC + AC &
   value(M,NPC,NN,NNC) &
   add_paths2(M,N,R,P,PC,PR).

% value(Method,NPC,NN,NNC) is true if NNC is the value of the node NN
% given that the search strategy is Method, and NPS is the path cost to NN
value(astar,NPC,NN,NNC) <-
   h(NN,HNN) &
   NNC is NPC+HNN.
value(best,_,NN,HNN) <-
   h(NN,HNN).
value(hdepth,_,NN,HNN) <-
   h(NN,HNN).
value(shortest,NPC,_,NPC).
value(breadth,_,_,0).
value(depth,_,_,0).


% choose(M,E,F,NF) is true if E is an element of frontier F and NF is
%   the remaining frontier after E is removed. M is the search method used.
% In each of these the frontier is the list of elements in order they
%   are to be chosen.

choose(_,N,[N|F],F).

% hadd_to_frontier(M,Ns,F1,F2) is true if when using search method M, when
%   nodes Ns are added to frontier F1, the resulting frontier is list F2.

hadd_to_frontier(depth,Ns,F1,F2) <- 
   append(N,F1,F2).

hadd_to_frontier(breadth,N,F1,F2) <- 
   append(F1,N,F2).

hadd_to_frontier(hdepth,N,F1,F2) <- 
   mergeinto(N,[],NF) &
   append(NF,F1,F2).

hadd_to_frontier(astar,N,F1,F2) <-
   mergeinto(N,F1,F2).
hadd_to_frontier(best,N,F1,F2) <-
   mergeinto(N,F1,F2).
hadd_to_frontier(shortest,N,F1,F2) <-
   mergeinto(N,F1,F2).

% mergeinto(NNs,Fr0,Fr1) is true if adding frontier elements NNs to
% frontier Fr0 results in frontier Fr1. The frontier is sorted by the
% fourth argument to the node function sysmbol.
mergeinto([],L,L).
mergeinto([H|T],L1,L3) <-
   insertinto(H,L1,L2) &
   mergeinto(T,L2,L3).

% insertinto(NN,Fr0,Fr1) is true if adding frontier element NN to
% frontier Fr0 results in frontier Fr1. The frontier is sorted by the
% fourth argument to the node function sysmbol.

insertinto(E,[],[E]).
insertinto(node(N,P,PC,NC),[node(N1,P1,PC1,NC1)|R],
              [node(N,P,PC,NC),node(N1,P1,PC1,NC1)|R]) <-
   NC =< NC1.
insertinto(node(N,P,PC,NC),[node(N1,P1,PC1,NC1)|R],
              [node(N1,P1,PC1,NC1)|R1]) <-
   NC > NC1 &
   insertinto(node(N,P,PC,NC),R,R1).

% **************************************************
% Auxiliary definitions

% append(A,B,R) is true if R is the list containing the elements of A
% followed by the elements of B 
append([],R,R).
append([H|T],L,[H|R]) <-
   append(T,L,R).

