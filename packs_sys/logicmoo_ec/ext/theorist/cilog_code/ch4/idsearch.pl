% ITERATIVE-DEEPENING SEARCHER
% Copyright David Poole, 1996

% idsearch(Ns,P) is true if path P is a path found from starting nodes Ns
% using iterative deepening search 
% Example query: idsearch([o103],P).
idsearch(Ns,P) <-
   add_paths_db(Ns,[],0,[],Fr) &
   dbsearch(Fr,0,Fr,natural,P).

% dbsearch(F,DB,Q,How1,P) is true if a depth bound search from frontier F
% can find a path P of length >= DB.
% where Q is the initial frontier to (re)start from,
% How specifies whether the previous bound failed naturally or unnaturally

% The frontier is a list of  node(Node,Path,PathLength)
%   where Node is a node, Path is the path found to Node,
%   PathLength is the number of arcs in the path.

dbsearch([node(N,P,DB)|_],DB,_,_,[N|P]) <-
   is_goal(N).
dbsearch([node(N,P,PL)|F1],DB,Q,H,S) <-
   PL<DB &
   neighbours(N,NNs) &
   PL1 is PL+1 &
   add_paths_db(NNs,[N|P],PL1,F1,F2) &
   dbsearch(F2,DB,Q,H,S).
dbsearch([node(_,_,PL)|F1],DB,Q,_,S) <-
   PL >= DB &
   dbsearch(F1,DB,Q,unnatural,S).
dbsearch([],DB,Q,unnatural,S) <-
   DB1 is DB+1 &
   dbsearch(Q,DB1,Q,natural,S).

% add\_paths\_db(NNs,Path,PL,F_0,F_1) adds the
% neighbours NNs to the front of frontier F_0
% forming frontier F_1. The neighbors need to be
% converted to the form of elements of the frontier.
% Path is the path found to the neighbor, and PL
% is the path's length.
add_paths_db([],_,_,F,F).
add_paths_db([NN|R],Path,PL,F0,[node(NN,Path,PL)|F1]) <-
   add_paths_db(R,Path,PL,F0,F1).


