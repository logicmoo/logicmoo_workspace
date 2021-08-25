%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE    : lib/alpha_star.pl
%
%    Abstract A* Algorithm Variation
%
%    DESCRIPTION:    An A* path finding implementation for use with
%                    indigolog programs.
%    ORIGINAL:	     http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/5_1.html
%    LAST REVISED:	 Stavros Vassos (March 8st, 2005)
%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%    MAIN PREDICATE: pathfind(+Start, +End, -Solution)
%    NEEDS:          pathfind_move(+Start, -End, -Action) 
%                       pathfind_move/3 specifies what moves can be achieved 
%                       and where they leed to
%                    pathfind_heuristic(+State,+Goal,-H)
%                       pathfind_heuristic/3 specifies a heuristic metric needed
%                       for prioritizing the possible actions
%    
%    NOTES:          
%                    - All predicates in this file have the prefix "pathfind" so
%                    that there is no conflict with any other library.
%    
%    NOTES/DESCRIPTION FROM ORIGINAL:
%     Nodes have form    S#D#F#A
%            where S describes the state or configuration
%                  D is the depth of the node
%                  F is the evaluation function value
%                  A is the ancestor list for the node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile pathfind_f_function/6.
	
:- op(400,yfx,'#').    /* Node builder notation */
%:- dynamic(pathf/1).


%path finding that takes considers exactly one heuristic function along with the depth of
%the search so far. pathfind_f_function is the actual function used to order the nodes,
%pathfind_heuristic is the one given by the user
pathfind(State,Goal,Soln) :- 
		pathfind_f_function(State,Goal,0,F),
		%retractall(pathf(_)),
		pathfind_search([State#0#F#[]],Goal,S), reverse(S,Soln).

pathfind_f_function(State,Goal,D,F) :- pathfind_heuristic(State,Goal,H), F is D + H.

pathfind_search([State#_#_#Soln|_], State, Soln).
pathfind_search([B|R],Goal,S) :- 
		%B = State#_#_#_,
		%assert(pathf(State)),
		pathfind_expand(B,Goal,Children),
		pathfind_insert_all(Children,R,Open),
		pathfind_search(Open,Goal,S).

pathfind_insert_all([F|R],Open1,Open3) :- 
		pathfind_insert(F,Open1,Open2),
		pathfind_insert_all(R,Open2,Open3).
pathfind_insert_all([],Open,Open).

pathfind_insert(B,Open,Open) :- pathfind_repeat_node(B,Open), ! .
pathfind_insert(B,[C|R],[B,C|R]) :- pathfind_cheaper(B,C), ! .
pathfind_insert(B,[B1|R],[B1|S]) :- pathfind_insert(B,R,S), !.
pathfind_insert(B,[],[B]).

pathfind_repeat_node(P#_#_#_, [P#_#_#_|_]).

pathfind_cheaper( _#_#F1#_ , _#_#F2#_ ) :- F1 < F2.

pathfind_expand(State#D#_#S,Goal,All_My_Children) :-
     bagof(Child#D1#F#[Move|S],
			(D1 is D+1,
			pathfind_move(State,Child,Move),
			%\+ pathf(Child),
			pathfind_f_function(Child,Goal,D1,F)),
           	All_My_Children).


%path finding that takes as an argument which determins which pair of heuristic-moves to use.
%the user provides functions by giving definitions for pathfind_f_function/5 and pathfind_move/4
%which are conditioned on a type

%     Nodes have form    S#D#F#P#A#T
%            where S describes the state or configuration
%                  D is the cost computed so far to go from the starting node to this one
%                  F is the evaluation of the additional cost needed to get to the destination
%                  P is the list of actions performed so far
%                  A is the list of ancestor nodes visited so far
%                  T termination condition

pathfind(State,Goal,Type,Lim,Soln,Stats) :- 
		pathfind_f_function(State,Goal,Type,0,_,0,_,F),
		pathfind_search_adv([State#0#F#[]#[State]#0],Goal,Type,Lim,S,Stats), reverse(S,Soln).

pathfind_search_adv([State#D#_#Soln#_#T|L], State, _, _, Soln,
			[[totalcost,D],[stacksize,T],[termvalue,N]]):- length(L,N).
%pathfind_search_adv([State#D#_#Soln#_#T|L], State, _, _, Soln,[D,T,N]):- length(L,N).
pathfind_search_adv([B|R],Goal,Type,Lim,S,Stats) :-
		pathfind_expand_adv(B,Goal,Type,Lim,Children),
		pathfind_insert_all_adv(Children,R,Open),
		pathfind_search_adv(Open,Goal,Type,Lim,S,Stats).

pathfind_insert_all_adv([F|R],Open1,Open3) :- 
		pathfind_insert_adv(F,Open1,Open2),
		pathfind_insert_all_adv(R,Open2,Open3).
pathfind_insert_all_adv([],Open,Open).

pathfind_insert_adv(B,Open,Open) :- pathfind_repeat_node_adv(B,Open), ! .
pathfind_insert_adv(B,[C|R],[B,C|R]) :- pathfind_cheaper_adv(B,C), ! .
pathfind_insert_adv(B,[B1|R],[B1|S]) :- pathfind_insert_adv(B,R,S), !.
pathfind_insert_adv(B,[],[B]).

pathfind_repeat_node_adv(P#_#_#_#_, [P#_#_#_#_|_]).

pathfind_cheaper_adv( _#D1#F1#_#_#_ , _#D2#F2#_#_#_ ) :- V1 is F1+D1,V2 is F2+D2, V1 < V2.

pathfind_expand_adv(State#D#_#S#Path#T,Goal,Type,Lim,All_My_Children) :-
     bagof(Child#D1#F1#[Move|S]#[Child|Path]#T1,
			(pathfind_move(State,Child,Type,Move),
			\+ member(Child,Path),
			pathfind_f_function(Child,Goal,Type,D,D1,T,T1,F1),
			(Lim=inf -> true; T1<Lim)), 
           	All_My_Children).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: lib/alpha_star.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
