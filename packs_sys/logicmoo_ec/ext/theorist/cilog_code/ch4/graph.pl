% ***************************************************************
% The following defines the graph with cycles of Figure 4.2 in
% Computational Intelligence: A Logical Approach
% Copyright, Poole, Mackworth, Goebel, and Oxford University Press, 1997. 

% A search is carried out by using something like:
% ask search(depth,[o103],P).
% ask psearch(depth,[[o103]],P).
% ask hsearch(astar,[node(o103,[],0,0)],P).

% neighbours(N,NN) is true if NN is the list of neighbours of node N
neighbours(o103,[ts,l2d3,o109]).
neighbours(ts,[mail]).
neighbours(mail,[]).
neighbours(o109,[o111,o119]).
neighbours(o111,[]).
neighbours(o119,[storage,o123]).
neighbours(storage,[]).
neighbours(o123,[r123,o125]).
neighbours(o125,[]).
neighbours(l2d1,[l3d2,l2d2]).
neighbours(l2d2,[l2d4]).
neighbours(l2d3,[l2d1,l2d4]).
neighbours(l2d4,[o109]).
neighbours(l3d2,[l3d3,l3d1]).
neighbours(l3d1,[l3d3]).
neighbours(l3d3,[]).
neighbours(r123,[]).


% is_goal(N) is true if N is a goal node.
is_goal(r123).

% cost(N,M,C) is true if C is the arc cost for the arc from node N to node M
cost(N,M,C) <-
   neighbours(N,NN) &
   member(M,NN) &
   position(N,NX,NY) &
   position(M,MX,MY) &
   C is abs(NX-MX)+abs(NY-MY).                  % `Manhattan distance'
%   C is sqrt((NX-MX)*(NX-MX)+(NY-MY)*(NY-MY)). % Euclidean Distance

% N.B. the cost database in the book is obtained by the instances of the query
% ? cost(A,B,C).

% h(N,C) is true if C is the heuristic cost of node N
%  This assumes that there is only one goal node.
h(N,C) <-
   position(N,NX,NY) &
   is_goal(G) &
   position(G,GX,GY) &
   C is abs(NX-GX)+abs(NY-GY).

% position(N,X,Y) is true if node X is at position (X,Y)

position(mail,17,43).
position(ts,23,43).
position(o103,31,43).
position(o109,43,43).
position(o111,47,43).
position(o119,42,58).
position(o123,33,58).
position(o125,29,58).
position(r123,33,62).
position(l2d1,33,49).
position(l2d2,39,49).
position(l2d3,32,46).
position(l2d4,39,46).
position(l3d1,34,55).
position(l3d2,33,52).
position(l3d3,39,52).
position(storage,45,62).

member(A,[A|_]).
member(A,[_|T]) <-
   member(A,T).
