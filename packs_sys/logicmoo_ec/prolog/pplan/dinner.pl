%Dinner domain

%Type declarations

meal(spaghetti).
meal(pizza).
meal(tacos).
meal(fajitas).
meal(salad).
meal(sweetsourpork).
meal(crepes).
meal(duck).

location(home).
location(groceryStore).
location(italianRest).
location(frenchRest).
location(chineseRest).
location(pizzaPlace).


%Situation-independent domain knowledge

indPred(isThing(_)).
indPred(isMeal(_)).
indPred(isLocation(_)).
indPred(knowsHowToMake(_)).
indPref(vegetarian(_)).
indPred(italian(_)).
indPred(mexican(_)).
indPred(french(_)).
indPred(chinese(_)).
indPred(takeOutRest(_)).
indPred(dineInRest(_)).
indPred(restaurant(_)).
indPred(onMenu(_,_)).

isThing(L) :- isLocation(L).
isThing(L) :- isMeal(L).
isLocation(L) :- location(L).
isMeal(L) :- meal(L).

knowsHowToMake(spaghetti).
knowsHowToMake(tacos).
knowsHowToMake(fajitas).
knowsHowToMake(crepes).
knowsHowToMake(salad).

vegetarian(salad).

italian(spaghetti).
italian(pizza).

mexican(tacos).
mexican(fajitas).

french(crepes).
french(duck).

chinese(sweetsourpork).
chinese(vegfriedrice).

takeOutRest(chineseRest).
takeOutRest(pizzaPlace).

dineInRest(italianRest).
dineInRest(frenchRest).

restaurant(X):- takeOutRest(X).
restaurant(X):- dineInRest(X).

onMenu(spaghetti,italianRest).
onMenu(pizza,italianRest).
onMenu(crepes,frenchRest).
onMenu(duck,frenchRest).
onMenu(pizza,pizzaPlace).
onMenu(sweetsourpork,chineseRest).
%onMenu(vegfriedrice,chineseRest).

%Fluents

fluent(sated).
fluent(isSnowing).
fluent(isTired).
fluent(kitchenClean).
fluent(at(_)).
fluent(hasIngredients(_)).
fluent(readyToEat(_,_)).

%Actions

action(drive(_,_)).
action(cook(_)).
action(eat(_,_)).
action(buyIngredients(_)).
action(orderTakeout(_,_)).
action(orderRestaurant(_,_)).
action(cleanDishes).

%Preconditions

poss(drive(X,Y),S):- location(X), location(Y), X \= Y,holdsL([at(X)],S).

poss(cook(X),S):- meal(X), knowsHowToMake(X), 
	holdsL([at(home), hasIngredients(X),kitchenClean],S).

poss(eat(X,Y),S):- meal(X), holdsL([at(Y), readyToEat(X,Y)],S).

poss(buyIngredients(X),S):- meal(X), holdsL([at(groceryStore)],S),
                          \+ (holdsL([hasIngredients(X)],S)).

poss(orderTakeout(X,Y),S):- meal(X), takeOutRest(Y), onMenu(X,Y), holdsL([at(home)],S).

poss(orderRestaurant(X,Y),S):- meal(X), dineInRest(Y), onMenu(X,Y),holdsL([at(Y)],S).

poss(cleanDishes,S):- holdsL([at(home)],S).

%Effects

addList(drive(_,Y),[at(Y)]).
addList(cook(X),[readyToEat(X,home)]).
addList(eat(_,_),[sated]).
addList(buyIngredients(X),[hasIngredients(X)]).
addList(orderTakeout(X,_),[readyToEat(X,home)]).
addList(orderRestaurant(X,Y),[readyToEat(X,Y)]).
addList(cleanDishes,[kitchenClean]).

deleteList(drive(X,_),[at(X)]).
deleteList(cook(X),[hasIngredients(X),kitchenClean]).
deleteList(eat(X,Y),[readyToEat(X,Y)]).
deleteList(buyIngredients(_),[]).
deleteList(orderTakeout(_,_),[]).
deleteList(orderRestaurant(_,_),[]).
deleteList(cleanDishes,[]).


