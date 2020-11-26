%?-use_module(library(lists)).
%:-prolog_flag(compiling,_,profiledcode).
:-ensure_loaded('temporalOrderCleaner.pl').


mapToTotalOrderPlan( Plan, Ordering ,NewOrdering) :-


 fetchTemporalReferences(Plan, OrderingMinusAbstractTimepoints),!,

 navigatePlanTree(Plan, Ordering, OrderingMinusAbstractTimepoints, t, NewOrdering),
 %We only every want one solution
 !.


navigatePlanTree(Plan, Ordering, OrderingMinusAbstractTimepoints, Timepoint, NewOrdering) :- 

 %Find the timepoint that is constrained to occur b Timepoint
 member( b(X, Timepoint), Ordering),
 %Check that this Timepoint occurs in the plan
 member( X,  OrderingMinusAbstractTimepoints),
 navigatePlanTree(Plan, Ordering, OrderingMinusAbstractTimepoints, X, NewOrdering).

  
%Abstract Timepoint has been identified
navigatePlanTree(Plan, Ordering, OrderingMinusAbstractTimepoints, Timepoint, NewOrdering) :- 

 %Find the timepoint AbstractTimepoint
 member( b(AbstractTimepoint, Timepoint), Ordering),
 
 \+ member( AbstractTimepoint,  OrderingMinusAbstractTimepoints),
 
 %find the left branch and the right branch connected to this abstract timepoint
 
 %currently only holds for two branches
 findall(BranchTimepoint, member( b(BranchTimepoint, AbstractTimepoint), Ordering), [Head, Tail]),
 mergeBranches(Plan, Head, Tail, Timepoint,AbstractTimepoint, Ordering, NewMergedOrdering ),
  
 navigatePlanTree(Plan, NewMergedOrdering, OrderingMinusAbstractTimepoints, Timepoint, NewOrdering ).  
 
navigatePlanTree(Plan, NewOrdering, OrderingMinusAbstractTimepoints, Timepoint, NewOrdering).
 

 
  
mergeBranches(Plan, LeftBranch, RightBranch, AbstractTimepointsChild,AbstractTimepoint, Ordering, NewOrdering):-
 findEndTimepoint(LeftBranch, Ordering, LeftEndTimepoint),
 removeAbstractTemporalOrderings(Ordering, AbstractTimepoint, OrderingWithoutTimepoint),
 append(OrderingWithoutTimepoint, [b(RightBranch, LeftEndTimepoint)], BuildingNewOrdering ),
 append(BuildingNewOrdering, [b(LeftBranch, AbstractTimepointsChild)], NewOrdering).
 

 

% Search for the last timepoint connected to Branch
findEndTimepoint(Branch,Ordering, EndTimepoint):-
 member(b(ParentNode,Branch), Ordering),
 findEndTimepoint(ParentNode, Ordering, EndTimepoint).

%Last Timepoint has been found 
findEndTimepoint(Branch, Ordering, Branch).
 
 
removeAbstractTemporalOrderings(Ordering, AbstractTimepoint, OrderingWithoutTimepoint) :-
 findall( b(X,AbstractTimepoint), member(b(X,AbstractTimepoint), Ordering), ChildrenOrderingToDelete ), 
 findall( b(AbstractTimepoint, Y), member(b(AbstractTimepoint,Y), Ordering), ParentOrderingToDelete  ), 
 deleteList(ChildrenOrderingToDelete, Ordering, OrderingMinusAbstractChildrenTimepoints ),
 deleteList(ParentOrderingToDelete,   OrderingMinusAbstractChildrenTimepoints, OrderingWithoutTimepoint ).

  
deleteList([], Result, Result). 
deleteList([Head | Tail], WorkingList, Accum):-
 delete(WorkingList, Head, ResultList),
 deleteList(Tail, ResultList, Accum).
 

 
% --------------Tests ----------------------

test1_OrderingWithoutTimepoint(OrderingWithoutTimepoint):- removeAbstractTemporalOrderings([b(t5,t6),b(t1,t2),b(t3,t2), b(t2,t5)],t2, OrderingWithoutTimepoint).

% test1_FindStartTimepoint :- findStartTimepoint([b(y,x)], Timepoint). 

test1_findEndTimepoint(Result):- findEndTimepoint(t1, [b(t2,t1), b(t3,t2),b(t4,t3) ], Result).


test1_mapToTotalOrderPlan(Result):- mapToTotalOrderPlan(
                     [happens(b,t70,t70),happens(a,t67,t67),happens(c,t66,t66),happens(d,t25,t25)],
                     [b(t25,tabstract), b(t66,tabstract), b(t66, t67), b(t70,t66), b(tabstract,t)], 
                     Result).
