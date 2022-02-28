 

%Evaluates the weight of a preference formula, supporting quantifiers
%and annotated APFs. This file is for use in testing with BFS.

:- nb_linkval(vmin, 0).
:- nb_linkval(vmax, 1).
  
%ComputeweightsV6BFS.pl
%For the purpose of clarity we make use of the following:
%+ArgName This argument should be instantiated to a non-variable term. 
%-ArgName This argument should not be instantiated.
%?ArgName This argument may or may not be instantiated.

%computeWeights(+Pref,?W,+PlanState,?NewPref): W is the weight achieved by the 
%preference Pref in the plan-state PlanState, and NewPref is the progression
%of Pref through PlanState.
% W is arrived from the  heuristic function that says if we have found a plan
% then we take the actual weight, otherwise we take the optimistic weight.
computeWeights(Pref,W,[Plan,State],ProgPref):- 
	prog(Pref,[Plan,State],ProgPref),
	findall(V,w(Pref,V,[Plan,State]),WList), minList(WList, W).


%WEIGHTS FOR QUANTIFIERS

w(exists(_,_, VMAX), VMAX, [_,_]) :- nb_getval(vmax, VMAX),!.

w(exists(X,C, R), VMIN, [P, S]) :- nb_getval(vmin, VMIN),findall(X, w(andB(andB(isThing(X), C), R),VMIN,[P,S]), L), \+(L == []) , !.

w(exists(_,_,_), VMAX, [_,_]) :- nb_getval(vmax, VMAX),!.

w(forall(X,C,R), VMAX, [P,S]) :- nb_getval(vmax, VMAX),nb_getval(vmin, VMIN),w(exists(X,C, notB(R)), VMIN, [P,S]), !.
w(forall(_,_,_), VMIN, [_,_]) :- nb_getval(vmin, VMIN),!.


%WEIGHTS FOR GENERAL PREFERENCES 
%We have the following types of general preferences:
% 1. andG(R), where R is a list composed of BDFs, atomic preferences and 
%             general preferences, where the user would like all of them to
%             be satisfied as much as possible.
% 2. orG(R),  where R is a list composed of BDFs, atomic Preferences and 
%             general preferences, where the user wishes one of the 
%             preferences to be satisfied as much as possible.
% 3. notG(R), where R is a list composed of BDFs, atomic preferences and 
%             general preferences, where the user wishes that R not be 
%             satisfied.
% 4. cond(C,R), where C is a BDF expressing a condition, and R is a BDF, 
%               atomic or general preference, with the intended meaning that
%               the user has the preference R when C holds.
% 5. orderS(R), where R is a list composed of BDFs, atomic and general 
%               preferences, and the user expresses a lexicographic 
%               preference over the preference formulas in R
%__________________________________________________________________________

%w(+PF, ?N, +PlanState) : N is the weight of the preference formula PF with
% respect to the current plan and state.

%WEIGHTS FOR GENERAL PREFERENCES 
w(andG(R),N,[P,S]):- listWeights(R,[P,S],L), maxList(L,N).

w(orG(R),N,[P,S]):- listWeights(R,[P,S],L), minList(L,N).

w(cond(C,_),VMIN,[P,S]):- nb_getval(vmin, VMIN),\+ w(C,VMIN,[P,S]), !.
w(cond(C,R),N,[P,S]):- nb_getval(vmin, VMIN),w(C,VMIN,[P,S]), w(R,N,[P,S]). 


%WEIGHTS FOR ATOMIC PREFERENCES
%Atomic preferences will look like:
%atomic([BDF1, V1, BDF2, V2,..., BDFn, Vn]) where each BDFi is a basic desire formula, and
%each Vi is a value between vmin and vmax. The weight of a preference in S is Vi where BDFi  
%is the first BDF such that w(BDFi,0,[P,S]).

w(atomic(R),V,[P,S]):- firstSat(R,V,[P,S]).

%WEIGHTS FOR BASIC DESIRE FORMULAS
%Basic desire formulas are properties of states or trajectories.
%A BDF can either be satisfied (w=0) or not satisfied (w=1)
%in a given plan-state [P,S].

%TEMPORAL BASIC DESIRE FORMULAS

w(always(X),W,[P,S]):- !, w(X,W,[P,S]).
w(next(_),VMIN,[_,_]):- nb_getval(vmin, VMIN),!.
w(eventually(X),W,[P,S]):- !, w(X,W,[P,S]).
w(until(_,Y),W,[P,S]):- !, w(Y,W,[P,S]).
w(final(X),W,[P,S]):- !, w(X,W,[P,S]). 

%LOGICAL CONNECTIVES

w(andB(X,Y),N,[P,S]):- !, w(X,W1,[P,S]), w(Y,W2,[P,S]), max(W1,W2,W), N is W.
w(orB(X,Y),N,[P,S]):- !, w(X,W1,[P,S]), w(Y,W2,[P,S]), min(W1,W2,W), N is W.
w(notB(X),VMIN,[P,S]):- nb_getval(vmin, VMIN),\+ (w(X,VMIN,[P,S])), !.
w(notB(X),VMAX,[P,S]):- nb_getval(vmax, VMAX), nb_getval(vmin, VMIN),!, w(X,VMIN,[P,S]).

%ELEMENTARY BASIC DESIRE FORMULAS

w([X,X],VMIN,_):-nb_getval(vmin, VMIN),!.
w([X,Y],VMAX,_):- nb_getval(vmax, VMAX), \+ (X==Y).

w(occ(_),VMAX,_):- nb_getval(vmax, VMAX),!.

w(occ2(A),VMIN,[[A2|_],_]):- nb_getval(vmin, VMIN),A=A2.
w(occ2(A),VMAX,[[A2|_],_]):- nb_getval(vmax, VMAX),\+ (A==A2) .

w(F,VMIN,[_,S]):- nb_getval(vmin, VMIN),fluent(F), member(F,S).
w(F,VMAX,[_,S]):- nb_getval(vmax, VMAX),fluent(F), \+(member(F,S)).

w(I,VMIN,_):- nb_getval(vmin, VMIN),indPred(I), I.
w(I,VMAX,_):- nb_getval(vmax, VMAX),indPred(I), \+ ( I). 
w(N,N,_):- number(N), !.

%______________________________________________________________________

%pos(+W,+V,?P) : P is the first position of W with respect to the set V
%Example: 
%pos(3/4, [0,1/4, 1/3,1/2,2/3,3/4,1], P).
%P =5.
pos(W, [W|_], 0) :- !.
pos(W, [_|Rest],P) :- pos(W, Rest, P2), P is P2 + 1.

%v(+PF, -L) : L is a set of possible weights of the preference formula PF
v(atomic(R), L) :- !, length(R, N), getList(N, L). 
v(notG(R), L) :- !, v(R, L).
v(cond(_, R), L ) :- !, v(R, L).

v(andG(R), L) :- !, listV(R, L1), sort(L1, L2), removeDuplicates(L2, L).
v(orG(R), L) :- !, listV(R, L1), sort(L1, L2), removeDuplicates(L2, L).

v(orderS(R), L) :- !, getAllC(R, C), multiply(C, M), N is M-1, getList(N, L).  

%if none of the above holds it must be a BDF formula
v(_, [0, 1]).


%sumOrder(+R, ?Sum, +Acc, +[P, S]) :
%R is a general preference formula and Sum is the numerator part of the
%the lex Order formula.
%Example :
%sumOrder([atomic([at(italianRest), at(frenchRest), at(home)]), atomic([at(italianRest), at(home), at(frenchRest), at(groceryStore)])], Sum, 0, [[eat(salad, home), drive(home, groceryStore)], [at(home)]]).
%Sum = 11
sumOrder([], Sum, Sum, _) :- ! .
sumOrder([G|Rest], Sum, Acc, [P, S]) :- w(G, W, [P, S]), v(G, V),
	pos(W, V, Pos), getAllC(Rest, C), multiply(C, M), F is Pos*M,
	Acc2 is Acc+F, sumOrder(Rest, Sum, Acc2, [P, S]). 

%getAllC(+R, -L) : L is a list that contains the cardinality of all the 
%elements in R in the same order as they appear in R.
%Example : 
%listV([atomic([at(a),at(b),at(c)]),atomic([at(a), at(b), at(c), at(d)])], L).
%L = [4,5]
getAllC([], []) :- !.
getAllC([X|Rest], [Y|Rest2]) :- v(X,X1),length(X1, Y), getAllC(Rest, Rest2).

%listV(+R, -L) : L is a list that contains a union of all possible weights in 
%the list R. This list is not sorted and it contains duplicates.
%Example : 
%listV([atomic([at(a),at(b),at(c)]),atomic([at(a), at(b), at(c), at(d)])], L).
%L = [0, 0.333333, 0.666667, 1, 0, 0.25, 0.5, 0.75, 1]
listV([],[]) :- !.
listV([G|Rest], L) :- v(G, L1), listV(Rest, Rest2), append(L1, Rest2, L).

%removeDuplicates(+R, -L) : L is the sorted list R only that duplicates are 
%removed. R must be sorted.
%Example : 
%removeDuplicates([1,2,2,2,3,4,4], L).
%L = [1,2,3,4]
removeDuplicates([], []) :- !.
removeDuplicates([X], [X]) :- !.
removeDuplicates([X, X|Rest], L) :- !,   removeDuplicates([X|Rest], L).
removeDuplicates([X, Y|Rest], [X|L]) :- removeDuplicates([Y|Rest], L).

%getList(+N, -L) : L is a set of possible weights of a Atomic formula given 
%that the they are N elements in that formula, (i.e, n= N-1).
%Example :
%getList(4, L).
%L = [0,0.25, 0.5, 0.75, 1]
getList(0, [0]) :- !.
getList(N, L) :- getL(N, L1), reverse(L1, L2), divide(N, L2, L).


%getL(+N, -L) : L is a list of elements from 0 to N
%Example : 
%getL(4, L).
%L = [4,3,2,1,0]
getL(0,[0]) :- !.
getL(N, L) :- M is N-1, getL(M, L1), append([N], L1, L).

%divide(+N, +L1,- L2) : L2 is L1 only elements of L1 are divided by N
divide(_, [], []).
divide(N, [X|Rest], [Y|Rest2]) :- Y is X/N, divide(N, Rest, Rest2). 

%firstSat(+PrefList,?N,+[P,S]): the N-th basic desire formula in PrefList is
%the first basic desire formula satisfied by the plan-state [P,S]
firstSat([],V,_):- nb_getval(vmax, V). 
firstSat([BDF, V|_],V,[P,S]):- nb_getval(vmin, VMIN), w(BDF,VMIN,[P,S]), !.
firstSat([BDF,_|Rest],VRest,[P,S]):- nb_getval(vmax, VMAX), w(BDF,VMAX,[P,S]), firstSat(Rest,VRest,[P,S]).


______________________________________prog_________________________________

%PROGRESSION OF PREFERENCES
%prog(+PF, +PlanState, -ProgPref) : ProgPref is the progression of the 
% prefernce formula PF with respect to the current plan and state.


prog(exists(_,_ , Pref), [P, S], 0) :- prog(Pref, [P, S], 0), !.
prog(exists(_,_, Pref), [P,S], 1) :- prog(Pref, [P,S], 1), !.


prog(exists(X, C, Pref), [P,S], exists(X, C, ProgPref)) :- !, prog(Pref, [P,S], ProgPref).

prog(forall(X, C, Pref), [P,S], notB(exists(X, C, notB(ProgPref)))):- !, prog(Pref, [P,S], ProgPref).

prog(andG(L),[P,S],ProgPref):- !, progList(L,[P,S],New), simplifyAndG(New,ProgPref).
prog(orG(L),[P,S],ProgPref):- !, progList(L,[P,S],New), simplifyOrG(New,ProgPref).
prog(notG(Pref),[P,S],notG(Pref2)):- !, prog(Pref,[P,S],Pref2). 
prog(cond(C,Pref),[P,S],cond(C2,Pref2)):- !, prog(C,[P,S],C2), prog(Pref,[P,S],Pref2).
prog(orderW(L),[P,S],orderW(L2)):-  !, progList(L,[P,S],L2).
prog(orderS(L),[P,S],orderS(L2)):-  !, progList(L,[P,S],L2).

prog(atomic(L),[P,S],atomic(L2)):- !, progList(L,[P,S],L2). 

prog(andB(X,Y),[P,S],ProgPref):- !, prog(X,[P,S],X2), prog(Y,[P,S],Y2),
                                    simplifyAnd(X2,Y2,ProgPref).
prog(orB(X,Y),[P,S],ProgPref):- !, prog(X,[P,S],X2), prog(Y,[P,S],Y2),
                                    simplifyOr(X2,Y2,ProgPref).
prog(notB(X),[P,S],ProgPref):- !, prog(X,[P,S],X2), simplifyNot(X2,ProgPref).

prog(next(X),_,X):- !.
prog(always(X),[P,S],ProgPref):- !, prog(X,[P,S],X2), simplifyAnd(X2,always(X),ProgPref).
prog(eventually(X),[P,S],ProgPref):- !, prog(X,[P,S],X2), simplifyOr(X2,eventually(X),ProgPref).
prog(until(X,Y),[P,S],ProgPref):- !, prog(X,[P,S],X2), prog(Y,[P,S],Y2),
                      simplifyAnd(X2,until(X,Y),Z), simplifyOr(Z,Y2,ProgPref).
prog(final(X),_,final(X)):- !.

prog([Var,Constant],_,[Var,Constant]):- !.
prog(occ(A),_,occ2(A)):- !. %we can't determine if occ(A) holds yet, so we progress it
                            %to occ2 and check occ2 holds in the next plan-state
prog(occ2(A),[[A2|_],_],0):- ground(A), A=A2, !.
prog(occ2(A),[[A2|_],_],ProgPref):- varList2(A,Vars),  
                                    findall(Vars,member(A,[A2]),Consts),
                                    Consts\=[], !, aList(Vars,Consts,ProgPref).
prog(occ2(_),_,1):- !.

prog(N,_,N):- number(N), !.
prog(F,[_,S],0):- fluent(F), ground(F), member(F,S), !.
prog(F,[_,S],ProgPref):- fluent(F), varList2(F,Vars),
                         findall(Vars,member(F,S),Consts), 
                         Consts\=[], !, aList(Vars,Consts,ProgPref).
prog(F,_,1):- fluent(F), !.
prog(I,_,0):- indPred(I), ground(I), I, !.
prog(I,_,ProgPref):- indPred(I), varList2(I,Vars),  
                     findall(Vars,I,Consts), Consts\=[], !,
                     aList(Vars,Consts,ProgPref).
prog(I,_,1):- indPred(I), !.

%progList(+PrefList,+PlanState,?ProgList): progresses each of the preferences in PrefList
progList([],_,[]):- !.
progList([Pref|Rest],[P,S],[ProgPref|ProgRest]):- prog(Pref,[P,S],ProgPref),
                                                  progList(Rest,[P,S],ProgRest).


%______________________________________________________________________________
%min(+X,+Y, ?Z) : Z is the minimum of X and Y
min(X,Y,X):- X =< Y.
min(X,Y,Y):- Y < X.

%max(+X, +Y, ?Z) : Z is the maximum of X and Y
max(X,Y,X):- X >= Y.
max(X,Y,Y):- Y > X.

%minList(+L, A) : A is the minimum element in the list L.
minList([A],A).
minList([A|Rest],N):- minList(Rest,B), min(A,B,N).

%maxList(+L, A) : A is the maximum element in the list L.
maxList([A],A).
maxList([A|Rest],N):- maxList(Rest,B), max(A,B,N).

%listWeights(+L, +PlanState, ?W) : W is a list that contains the weights of
% the elements in the list L in the same order as they appear in L.
listWeights([],_,[]).
listWeights([R|Rest],[P,S],[W1|W2]):- w(R,W1,[P,S]),listWeights(Rest,[P,S],W2).

%listMaxs(+R, ?L) : R is a list of prefernce formulas and L is a list that
%contains max weight for every element of R.
listMaxs([],[]).
listMaxs([A|Rest],[M1|M2]):- maxW(A,M1), listMaxs(Rest,M2).

%multiply(+L, ?N) : N is a result of multiplying all the elements of L.
multiply([],1).
multiply([A|Rest],N):- multiply(Rest,M), N is A*M.

%add(+N, +R, ?L) : L is the result of adding N to every element of the list R.
add(_,[],[]).
add(N,[A|Rest],[A2|Rest2]):- add(N,Rest,Rest2), A2 is A + N.

%The predicates simplifyAnd, simplifyOr, simplifyNot, simplifyAndG,
% and simplifyOrG are used to simplify progressed preferences. They are all
% in the same form as simplifyAnd(+X, +Y, ?F) where X and Y are preference 
% formula and F is the simplified version of X and Y. 

simplifyAnd(V,X,X):- nb_getval(vmin, V), !.
simplifyAnd(V,_,V):- nb_getval(vmax, V),!.
simplifyAnd(X,V,X):-nb_getval(vmin, V),  !.
simplifyAnd(_,V,V):- nb_getval(vmax, V), !.
simplifyAnd(X,Y,andB(X,Y)).

simplifyOr(V,_,V):- nb_getval(vmin, V), !.
simplifyOr(V,X,X):- nb_getval(vmax, V), !.
simplifyOr(_,V,V):-nb_getval(vmin, V),  !.
simplifyOr(X,V,X):- nb_getval(vmax, V), !.
simplifyOr(X,Y,orB(X,Y)).

simplifyNot(V,V1):- nb_getval(vmin, V), nb_getval(vmax, V1),!.
simplifyNot(V1,V):-nb_getval(vmin, V),nb_getval(vmax, V1), !.
simplifyNot(X,notB(X)).

%updated in V6
simplifyAndG(List,SList):-simplifyAndG1(List,NList),simplifyAndG2(NList,SList).

simplifyAndG1([],[V]):- nb_getval(vmin, V), !.
simplifyAndG1([New|Rest],[Max2|Rest2]):- number(New), !,
	simplifyAndG1(Rest,[Max|Rest2]),max(New,Max,Max2).

simplifyAndG1([New|Rest],[Max|[New|Rest2]]):- simplifyAndG1(Rest,[Max|Rest2]). 

simplifyAndG2([N],N):- number(N), !.
simplifyAndG2(X,andG(X)).

simplifyOrG(List,SList):- simplifyOrG1(List,NList), simplifyOrG2(NList,SList).

simplifyOrG1([A],[A]).
simplifyOrG1([Int|Rest],[Int|[First|Rest2]]):- number(Int), 
	simplifyOrG1(Rest,[First|Rest2]), \+ (number(First)) , !.
simplifyOrG1([Other|Rest],[Other|[First|Rest2]]):- \+ ( number(Other)), 
	simplifyOrG1(Rest,[First|Rest2]), \+(number(First)), !.
simplifyOrG1([Int|Rest],[Int|Rest2]):- number(Int), 
	simplifyOrG1(Rest,[Max|Rest2]), number(Max), max(Max,Int,Int), !.
simplifyOrG1([Int|Rest],[First|Rest2]):- number(Int), simplifyOrG1(Rest,[First|Rest2]), number(First).
simplifyOrG1([Other|Rest],[First|[Other|Rest2]]):- \+(number(Other)), simplifyOrG1(Rest,[First|Rest2]).

simplifyOrG2([N],N):- !.
simplifyOrG2(L,orG(L)).


%varList2(+Term,?Vars): Vars is a list containing the variables in Term, 
%without duplicates
varList2(V,[V]):- var(V), !. %variables
varList2([],[]):- !. %empty
varList2(N,[]):- number(N), !.
varList2(A,[]):- atom(A), !. %constants
varList2([H|T],L):- varList2(H,H2),varList2(T,T2),merge(H2,T2,L), !. %list
varList2(T,Vars):- argList(T,ArgList), varList2(ArgList,Vars), !.

%argList(+Functor,?ArgList): ArgList is a list of the arguments of Functor
argList(Term,ArgList):- functor(Term,_,N), argList(Term,1,N,ArgList).
argList(Term,N,N,[ArgN]):- arg(N,Term,ArgN).
argList(Term,K, N,[ArgK|RemArgs]):- arg(K,Term,ArgK), K2 is K+1, 
                                     argList(Term,K2,N,RemArgs).

%aList(+V,+C,?Pref): Pref is a basic desire formula which expresses that the
% variables in V are assigned (pointwise) to the elements of one of the 
% lists in C
aList(V,C,Pref):- combine(V,C,List), applyManyAnd(List,NList),
                  manyOr(NList,Pref).

applyManyAnd([],[]):-!.
applyManyAnd([L|Rest],[L2|Rest2]):- manyAnd(L,L2), applyManyAnd(Rest,Rest2).

manyAnd([P],P):-!.
manyAnd([P|Rest],andB(P,PRest)):- manyAnd(Rest,PRest).

manyOr([P],P):-!.
manyOr([P|Rest],orB(P,PRest)):- manyOr(Rest,PRest).

%combine(+L,+P,?R)
%Given a list L of length n and a list P of lists Qi of length n,
%returns a list containing lists of pairs of elements of L and the Qi
%Example: L=[X1,...,Xn], P=[[a1,...,an],...,[z1,...,zn]], 
%         R=[[[X1,a1],...,[Xn,an]],...,[[X1,z1],...,[Xn,zn]]]
combine(L,[L2],[R]):- combine2(L,L2,R).
combine(L,[L2|Rest],[R|CRest]):- combine2(L,L2,R), combine(L,Rest,CRest).

combine2([],[],[]):- !.
combine2([A|RestA],[B|RestB],[[A,B]|Rest]):- combine2(RestA,RestB,Rest).

%Auxiliary Predicates:

holds(F,S):- member(F,S).
holdsL([F],S):- holds(F,S).
holdsL([F|Rest],S):- holds(F,S), holdsL(Rest,S).


















