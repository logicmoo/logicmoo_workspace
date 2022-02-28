% This version uses breadth first search to find a plan of a given quality.
% Used for comparison with PPLAN.


%Main Predicate: pplan
%pplan(InitState,Goal,Pref,LBound)
%InitState and Goal are lists, Pref is a preference expression, Bound is a bound on the
%length of the plan. 
%Assume that domain theory uses the following predicates:
% action(A): A is an action    
% fluent(F): F is a fluent
% indPred(I): I is a non-fluent domain predicate
% poss(A,S): A is executable in state S
% deleteList(A,L): L is the delete list of action A
% addList(A,L): L is the add list of action A
  
pplan(InitState,Goal,Pref,LBound,QBound):-
      computeWeights(Pref,W,[[],InitState],ProgPref),
      pplan2([W-[[],InitState,ProgPref]],Goal,LBound, 0,0, QBound).

%CASE 1: The front node meets the goal, so its a plan, also it has the an
%acceptable weight, so we are done.
pplan2([Val-[Plan,State,_]|_],Goal,_, NE,NC, QBound):-
	holdsL(Goal,State), \+(Val > QBound), !, 
	 write('Plan: '), reverse(Plan,RPlan), writeln(RPlan), 
	 write('Value: '), writeln(Val),  
	 write('Nodes Expanded: '), writeln(NE),
	 write('Nodes Considered: '), writeln(NC).


%CASE 2: We have't found an acceptable plan that is the first element of the frontier,
%we continue our search.
pplan2([_-[Plan,State,ProgPref]|NodeList],Goal,LBound, NE,NC,QBound):-
	expand([Plan,State,ProgPref],LBound, Neighbours, Goal),
	append(NodeList,Neighbours,NewNodeList), 
	length(Neighbours,L), NC2 is NC + L, NE2 is NE + 1, 
        pplan2(NewNodeList,Goal,LBound, NE2,NC2,QBound).

%Auxiliary Predicates:

holds(F,S):- member(F,S).
holdsL([F],S):- holds(F,S).
holdsL([F|Rest],S):- holds(F,S), holdsL(Rest,S).

%Expanding a node
expand([Plan,_,_],Bound,[], _):- length(Plan,Bound), !.
expand([Plan,State,ProgPref],_, Neighbours, Goal):- getPossActions(State,Actions),
                               getNewStates(State,Actions,NewStates),       
			       getNewPlans(Plan,Actions,NewPlans),
			       getNewPStates(NewPlans,NewStates,NewPStates),
           getValsPrefs(NewPStates,ProgPref,NewValues, NewPrefs, Goal),
           getVPStates(NewValues,NewPrefs,NewPStates,Neighbours).

%Finds all actions possible in State
getPossActions(State,Actions):- findall(A,(action(A),poss(A,State)),Actions).

%For each action, finds the corresponding new state
getNewStates(_,[],[]):- !.
getNewStates(State,[Action|Rest],[NewS|RestNewS]):-
	progress(State,Action,NewS), getNewStates(State,Rest,RestNewS). 

%Takes each action and adds it to the current partial plan to get a new set
% of partial plans
getNewPlans(_,[],[]):- !.						  
getNewPlans(Plan,[Action|Rest],[[Action|Plan]|RestPlans]):- 
                                          getNewPlans(Plan,Rest,RestPlans).

%Attaches each partial plan to its corresponding state
getNewPStates([],[],[]):- !.
getNewPStates([Plan|RestP],[State|RestS],[[Plan,State]|RestPS]):- 
                                           getNewPStates(RestP,RestS,RestPS).

%For each plan-state, we compute the value of Pref and find the progressed
% preference
getValsPrefs([],_,[], [], _):- !.
getValsPrefs([PState|Rest],Pref,[Val|RestVals],[ProgPref|RestPrefs], Goal):- 
                         computeWeights(Pref,Val,PState,ProgPref), 
                         getValsPrefs(Rest,Pref,RestVals,RestPrefs, Goal).

%We combine the values, progressed preferences, and plan-states into one
% structure
getVPStates([],[],[],[]):- !.
getVPStates([Val|RestV], [Pref|RestP],[[Plan,State]|RestPS],
[Val-[Plan,State,Pref]|RestVPS]):-
	getVPStates(RestV,RestP,RestPS,RestVPS).

%Given an action and a state, we compute the resulting state
progress(State,Action,NewState):- deleteList(Action,DList), 
                                  removeAll(State,DList,RState),
                                  addList(Action,AList), 
                                  addAll(RState,AList,NewState).

%Given a list of fluents and a state, we remove all of the fluents from 
%the state
removeAll(State,[],State):- !.
removeAll(State,[F|Rest],RState):- member(F,State), !, delete(State,F,State2), 
                                   removeAll(State2,Rest,RState).
%We delete X from the list
delete([X|T],X,T).
delete([Y|T],X,[Y|NT]):- delete(T,X,NT).

%Given a list, we add each element to the state to get a new state
addAll(State,[],State):- !.
addAll(State,[A|Rest],RState):- member(A,State), !, addAll(State,Rest,RState).
addAll(State,[A|Rest],[A|RState]):- addAll(State,Rest,RState).
                                                             
