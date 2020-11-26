%  converts state_inv List of lists to a flat list

get_invariants(LL, Inv_List) :-
 	setof(P,A^P^substate_classes(A,S,P),LL),
	append_llists(LL,Inv_List1),    
	append_llists(Inv_List1, Inv_List).

%gets term numbered N from list L and assigns it to T.

getnum(N,L,T):-get_term(N,L,T,1).
get_term(N,[L|_],L,N).
get_term(N,[_|Lt],X,M):-Q is M+1,get_term(N,Lt,X,Q).


%counts the elements in list L and assigns the number to X.
%count(L,X).

count([],0).
count([_|Lt],N):-count(Lt,M),N is 1+M.


%concatenates list of lists.

append_lists_together(X,Z) :-
     reverse(X,X1),append_lists_together1(X1,[],Z).
append_lists_together1([],[],[]).
append_lists_together1([X],I,O) :-
    append(X,I,O),!.
append_lists_together1([X|T],I,O) :-
    append(X,I,I1),
    append_lists_together1(T,I1,O),!.


%reverses list L1 to give list L2.

%don't know what use this is but its here anyway.

cons(A,B,C):-cons(A,B,[A,B]).


%sublist checks if X is a sublist of L.

sublist(X,L):-append(L1,L2,L),append(A,X,L1).


%subset

subset(S,[H|T]):-
	subset(R,T),
	(S = R;S = [H|R]).
subset([],[]).


%permutation

permutation([],[]).
permutation([X|L],P):-
permutation(L,L1),insert(X,L1,P).

%del

del(X,[X|Tail],Tail).
del(X,[Y|Tail],[Y|Tail1]):-
del(X,Tail,Tail1).


%insert

insert(X,List,Biggerlist):-
del(X,Biggerlist,List).


%remove duplicates, ie if element'a' appears three times get rid of two of them.

add(X,L,L):-member(X,L),!.
add(X,L,[X|L]).

inc:-tell(incons1),inconsistent_constraint(X),write('inconsistant_constraint(['),write(X),write(']).'),nl,fail.

inc:-told.

% first arg is a subset of second
% -------------------------------
is_a_subset([S|Set],Set2) :-
     member(S,Set2),
     is_a_subset(Set,Set2).
 
is_a_subset([],_):-!.

% find the ancestor list of a Sort
% -------------------------------
find_ancest(Sort,Ancestors):-
      sorts(non_primitive_sorts,NP_Sortlist),
      find_ancest1(Sort,NP_Sortlist,NP_Sortlist,[],Ancestors),!.

find_ancest1(Sort,NP_Sortlist,[NP_Sort|Tlist],List,Ancestors):-
      sorts(NP_Sort,Sortlist),
      member(Sort,Sortlist),
      append(List,[NP_Sort],List1),
      find_ancest1(NP_Sort,NP_Sortlist,NP_Sortlist,List1,Ancestors),!.
find_ancest1(Sort,NP_Sortlist,[NP_Sort|Tlist],List,Ancestors):-
      find_ancest1(Sort,NP_Sortlist,Tlist,List,Ancestors),!.
find_ancest1(_,NP_Sortlist,[],Ancestors,Ancestors):-!.

