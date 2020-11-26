% ITERATIVE-DEEPENING A* SEARCHER using CILOG
% Copyright David Poole, 1996

% idsearch(N,P) is true if path P is a path found from node N
% using iterative deepening A* search 
% Example query: ask idsearch(o103,P).
idsearch(N,P) <-
   h(N,HN) &
   dbsearch([node(N,[],0)],HN,[node(N,[],0)],natural,P).

% dbsearch(F,DB,Q,How1,P) is true if a depth bound search from frontier F
% can find a path P of length >= DB.
% where Q is the initial frontier to (re)start from,
% How specifies whether the previous bound failed naturally or gives
% the minimum f-value for which the search failed.

% The frontier is a list of  node(Node,Path,PathLength)
%   where Node is a node, Path is the path found to Node,
%   PathLength is the length of the path.

dbsearch([node(N,P,DB)|_],DB,_,_,[N|P]) <-
   is_goal(N).
dbsearch([node(N,P,PL)|F1],DB,Q,H,S) <-
   h(N,HN) &
   HN+PL =< DB &
   neighbours(N,NNs) &
   add_paths_db(NNs,N,[N|P],PL,F1,F2) &
   dbsearch(F2,DB,Q,H,S).
dbsearch([node(N,_,PL)|F1],DB,Q,H,S) <-
   h(N,HN) &
   HN+PL > DB &
   min1(HN+PL,H,LUB) &
   dbsearch(F1,DB,Q,LUB,S).
dbsearch([],_,Q,NDB,S) <-
   NDB \= natural &
   dbsearch(Q,NDB,Q,natural,S).

%   add_paths(NNs,N,Path,PL,F0,F1)
add_paths_db([],_,_,_,F,F).
add_paths_db([NN|R],N,Path,PL,F0,[node(NN,Path,PL1)|F1]) <-
   cost(N,NN,AC) &
   PL1 is PL+AC &
   add_paths_db(R,N,Path,PL,F0,F1).

min1(E,natural,V) <- V is E.
min1(E,V,V) <- V\= natural & V =< E.
min1(E,V,V1) <-V\= natural & V > E & V1 is E.

