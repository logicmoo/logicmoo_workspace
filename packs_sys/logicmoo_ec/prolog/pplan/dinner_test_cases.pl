%run command: swipl -G128M < dinner_test_cases.pl > your_file_name_here

%For normal version of PPLAN
%[dinner,pplan,computeWeights].

%To try PPLAN with hard-coded constraints
:- [dinner,pplan,computeWeights].

%variable-free BDFs
test(1) :-
	writeln('Test case 1'),
	pplan([at(home)],[at(home),sated],final(kitchenClean),7).

test(2) :-
	writeln('Test case 2'),
	pplan([at(home),hasIngredients(tacos),kitchenClean],[at(home),sated,kitchenClean],eventually(occ(cook(tacos))),7).

test(3) :-
	writeln('Test case 3'),
	pplan([at(home),hasIngredients(tacos),kitchenClean],[at(home),sated,kitchenClean],eventually(occ(cook(spaghetti))),7).

test(4) :-
	writeln('Test case 4'),
	pplan([at(home),hasIngredients(tacos),kitchenClean],[at(home),sated,kitchenClean],always(kitchenClean),7).

%BDFs with quantifiers

test(5) :-
	writeln('Test case 5'),
	pplan([at(home),kitchenClean,isSnowing,hasIngredients(crepes)],[at(home),sated,kitchenClean],always(orB(notB(isSnowing),notB(exists(Y, isLocation(Y), exists(X, isLocation(X), occ(drive(X,Y))))))),7).

test(6) :-
	writeln('Test case 6'),
	pplan([at(home),hasIngredients(tacos),kitchenClean],[at(home),sated,kitchenClean],exists(X,isLocation(X),eventually(occ(eat(spaghetti,X)))),7).

test(7) :-
	writeln('Test case 7'),
	pplan([at(home),hasIngredients(tacos),kitchenClean],[at(home),sated,kitchenClean],exists(X,isLocation(X),eventually(occ(eat(pizza,X)))),7).

test(8) :-
	writeln('Test case 8'),
	pplan([at(home),hasIngredients(tacos),kitchenClean],[at(home),sated,kitchenClean],exists(X,isLocation(X),eventually(occ(eat(sweetsourpork,X)))),7).

test(9) :-
	writeln('Test case 9'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),7).

test(10) :-
	writeln('Test case 10'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),7).

test(11) :-
	writeln('Test case 11'),
	pplan([at(home),hasIngredients(tacos), hasIngredients(spaghetti),kitchenClean],[at(home),sated,kitchenClean],exists(Y, isMeal(Y),eventually(occ(cook(Y)))),7).

test(12) :-
	writeln('Test case 12'),
	pplan([at(home),hasIngredients(tacos), hasIngredients(spaghetti),kitchenClean],[at(home),sated,kitchenClean],exists(X, mexican(X), eventually(occ(cook(X)))),7).

test(13) :-
	writeln('Test case 13'),
	pplan([at(home),hasIngredients(tacos), hasIngredients(spaghetti),kitchenClean],[at(home),sated,kitchenClean],exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),7).

test(14) :-
	writeln('Test case 14'),
	pplan([at(home),hasIngredients(tacos), kitchenClean],[at(home),sated,kitchenClean],exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),7).

test(15) :-
	writeln('Test case 15'),
	pplan([at(home),hasIngredients(crepes), hasIngredients(spaghetti),kitchenClean],[at(home),sated,kitchenClean],exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),7).

test(16) :-
	writeln('Test case 16'),
	pplan([at(home),hasIngredients(spaghetti)],[at(home),sated,kitchenClean],exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),7).

test(17) :-
	writeln('Test case 17'),
	pplan([at(home),hasIngredients(crepes), hasIngredients(spaghetti),kitchenClean],[at(home),sated,kitchenClean],exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),7).

test(18) :-
	writeln('Test case 18'),
	pplan([at(home),kitchenClean,isSnowing,hasIngredients(crepes)],[at(home),sated,kitchenClean],exists(X,isMeal(X), exists(Z, isLocation(Z), andB(eventually(occ(drive(home, Z))), eventually(occ(eat(X, Z)))))), 7).

test(19) :-
	writeln('Test case 19'),
	pplan([at(home),kitchenClean,isSnowing,hasIngredients(crepes)],[at(home),sated,kitchenClean],always(orB(notB(isSnowing),notB(exists(Y, isLocation(Y), exists(X, isLocation(X), occ(drive(X,Y))))))),7).

test(20) :-
	writeln('Test case 20'),
	pplan([at(home),kitchenClean,isSnowing,hasIngredients(crepes)],[at(home),sated,kitchenClean],exists(X,isMeal(X), exists(Z, isLocation(Z), andB(eventually(occ(drive(home, Z))), eventually(occ(eat(X, Z)))))), 7).

test(21) :-
	writeln('Test case 21'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean], exists(X, notB(italian(X)), exists(Y, 0, eventually(occ(orderRestaurant(X,Y))))),7).

test(22) :-
	writeln('Test case 22'),
	pplan([at(home)], [at(home),sated,kitchenClean], exists(X, italian(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),7).

test(23) :-
	writeln('Test case 23'),
	pplan([at(home)], [at(home),sated,kitchenClean], exists(X, orB(chinese(X), italian(X)), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),7).

test(24) :-
	writeln('Test case 24'),
	pplan([at(home),hasIngredients(spaghetti)],[at(home),sated,kitchenClean],exists(X, isMeal(X), exists(Y, notB(takeOutRest(Y)), eventually(occ(eat(X, Y))))),7).

test(25) :-
	writeln('Test case 25'),
	pplan([at(home)],[at(home),sated],exists(Y, takeOutRest(Y), exists(Z, isLocation(Z), forall(X, onMenu(X, Y), eventually(occ(eat(X, Z)))))), 7).

test(26) :-
	writeln('Test case 26'),
	pplan( [at(home),kitchenClean,isSnowing,hasIngredients(crepes)],[at(home),sated,kitchenClean],exists(X, isLocation(X), forall(Y, french(Y), orB(eventually(occ(eat(Y, X))),knowsHowToMake(Y)))), 7).

%APFs: preferences over actions (how to get food)

test(27) :-
	writeln('Test case 27'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean], atomic([exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.3,exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.7]),7).

test(28) :-
	writeln('Test case 28'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean], atomic([exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.3,exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.7]),3).

test(29) :-
	writeln('Test case 29'),
	pplan([at(home),kitchenClean,hasIngredients(spaghetti)],[at(home),sated,kitchenClean], atomic([exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.3,exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.8]),7).

test(30) :-
	writeln('Test case 30'),
	pplan([at(home),kitchenClean,hasIngredients(crepes)],[at(home),sated,kitchenClean],atomic([exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))), 0,exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.5, exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.6]),7).

test(31) :-
	writeln('Test case 31'),
	pplan([at(home),kitchenClean,hasIngredients(crepes)],[at(home),sated,kitchenClean],atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5, exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.9]),7).

test(32) :-
	writeln('Test case 32'),
	pplan([at(home),kitchenClean,hasIngredients(crepes)],[at(home),sated,kitchenClean],atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5, exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.9]),3).

				%APFs: preferences over action arguments (what kind of food to eat)

test(33) :-
	writeln('Test case 33'),
	pplan([at(home),kitchenClean,hasIngredients(spaghetti)],[at(home),sated,kitchenClean],atomic([exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.4,exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.5]),7).

test(34) :-
	writeln('Test case 34'),
	pplan([at(home),kitchenClean,hasIngredients(crepes)],[at(home),sated,kitchenClean],atomic([exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.6,exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.9]),7).

test(35) :-
	writeln('Test case 35'),
	pplan([at(home)],[at(home),sated,kitchenClean],atomic([exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.6,exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.9]),3).

test(36) :-
	writeln('Test case 36'),
	pplan([at(home),kitchenClean,hasIngredients(spaghetti)],[at(home),sated,kitchenClean],atomic([exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.5,exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.8]),7).

test(37) :-
	writeln('Test case 37'),
	pplan([at(home),kitchenClean,hasIngredients(spaghetti)],[at(home),sated,kitchenClean],atomic([exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.7,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.9]),7).

test(38) :-
	writeln('Test case 38'),
	pplan([at(home),kitchenClean,hasIngredients(crepes)],[at(home),sated,kitchenClean],atomic([exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.6,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.8]),7).

test(39) :-
	writeln('Test case 39'),
	pplan([at(home),kitchenClean,hasIngredients(spaghetti)],[at(home),sated,kitchenClean],atomic([exists(Y, isLocation(Y), eventually(occ(eat(duck,Y)))),0,exists(Y, isLocation(Y), eventually(occ(eat(crepes,Y)))),0.3,exists(Y, isLocation(Y), eventually(occ(eat(sweetsourpork,Y)))),0.4]),7).


test(40) :-
	writeln('Test case 40'),
	pplan([at(home),hasIngredients(spaghetti)],[at(home),sated,kitchenClean],atomic([exists(Y, isLocation(Y), eventually(occ(eat(duck,Y)))),0,exists(Y, isLocation(Y), eventually(occ(eat(crepes,Y)))),0.3,exists(Y, isLocation(Y), eventually(occ(eat(pizza,Y)))),0.4]),3).


test(41) :-
	writeln('Test case 41'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],atomic([exists(Y, isLocation(Y), eventually(occ(eat(pizza,Y)))),0,exists(Y, isLocation(Y), eventually(occ(eat(sweetsourpork,Y)))),0.5,exists(Y, isLocation(Y), eventually(occ(eat(salad,Y)))),0.8]),7).


				%GPFs: Conjunction 

test(42) :-
	writeln('Test case 42'),
	pplan([at(home),hasIngredients(crepes)],[at(home),sated,kitchenClean],andG([atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5, exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.9]),always(at(home))]),3).

test(43) :-
	writeln('Test case 43'),
	pplan([at(home),kitchenClean,hasIngredients(spaghetti)],[at(home),sated,kitchenClean],andG([atomic([exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.3,exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.7]),atomic([exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.6,exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.9])]) ,7).

test(44) :-
	writeln('Test case 44'),
	pplan([at(home),kitchenClean,hasIngredients(crepes)],[at(home),sated,kitchenClean],andG([atomic([exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.3,exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.7]),atomic([exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.6,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.9]),always(at(home))]) ,7).

test(45) :-
	writeln('Test case 45'),
	pplan([at(home),hasIngredients(crepes),kitchenClean],[at(home),sated,kitchenClean],andG([atomic([exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))), 0,exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.5, exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.8]),eventually(occ(buyIngredients(spaghetti)))]),7).

test(46) :-
	writeln('Test case 46'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],andG([atomic([exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))), 0,exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.5, exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.6]),atomic([exists(Y, isLocation(Y), eventually(occ(eat(pizza,Y)))),0,exists(Y, isLocation(Y), eventually(occ(eat(sweetsourpork,Y)))),0.5,exists(Y, isLocation(Y), eventually(occ(eat(salad,Y)))),0.8])]) ,7).

test(47) :-
	writeln('Test case 47'),
	pplan([at(home),kitchenClean],[at(home),sated,hasIngredients(crepes),kitchenClean],andG([atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5]),atomic([exists(Y, isLocation(Y), eventually(occ(eat(duck,Y)))),0,exists(Y, isLocation(Y), eventually(occ(eat(crepes,Y)))),0.3,exists(Y, isLocation(Y), eventually(occ(eat(pizza,Y)))),0.4])]),7).

test(48) :-
	writeln('Test case 48'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],andG([atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5]),atomic([exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.6])]) ,7).

test(49) :-
	writeln('Test case 49'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],andG([atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5, exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.9]),atomic([exists(Y, isLocation(Y), eventually(occ(eat(duck,Y)))),0,exists(Y, isLocation(Y), eventually(occ(eat(crepes,Y)))),0.3,exists(Y, isLocation(Y), eventually(occ(eat(pizza,Y)))),0.4])]) ,7).

test(50) :-
	writeln('Test case 50'),
	pplan([at(home),isSnowing],[at(home),sated,kitchenClean],andG([atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5, exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.9]),always(orB(notB(isSnowing),notB(exists(Y, isLocation(Y), exists(X, isLocation(X), occ(drive(X,Y)))))))]) ,7).

%GPFs: Disjunction 4 test cases

test(51) :-
	writeln('Test case 51'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],orG([atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5]),atomic([exists(Y, isLocation(Y), eventually(occ(eat(pizza,Y)))),0,exists(Y, isLocation(Y), eventually(occ(eat(sweetsourpork,Y)))),0.5,exists(Y, isLocation(Y), eventually(occ(eat(salad,Y)))),0.8])])  ,7).

test(52) :-
	writeln('Test case 52'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],orG([atomic([exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))), 0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.5, exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.9]),atomic([exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.6,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.8])]) ,7).

test(53) :-
	writeln('Test case 53'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],orG([atomic([exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))), 0,exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0.5, exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.6]),atomic([exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.7,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.9])]) ,7).

test(54) :-
	writeln('Test case 54'),
	pplan([at(home),kitchenClean,hasIngredients(crepes)],[at(home),sated,kitchenClean],orG([atomic([exists(Y, isMeal(Y),eventually(occ(cook(Y)))),0,exists(X, isMeal(X), exists(Y, takeOutRest(Y), eventually(occ(orderTakeout(X,Y))))),0.3,exists(X, isMeal(X), exists(Y, dineInRest(Y), eventually(occ(orderRestaurant(X,Y))))),0.7]),atomic([exists(X, italian(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0,exists(X, french(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.6,exists(X, chinese(X),exists(Y, isLocation(Y), eventually(occ(eat(X,Y))))),0.9]),always(at(home))]) ,7).

				%GPFs: Conditional

test(55) :-
	writeln('Test case 55'),
	pplan([at(home),hasIngredients(spaghetti),kitchenClean],[at(home),sated,kitchenClean],cond(hasIngredients(spaghetti),eventually(occ(cook(spaghetti)))),7).

test(56) :-
	writeln('Test case 56'),
	pplan([at(home),hasIngredients(tacos)],[at(home),sated,kitchenClean],cond(hasIngredients(spaghetti),eventually(occ(cook(spaghetti)))),7).

test(57) :-
	writeln('Test case 57'),
	pplan([at(home),hasIngredients(spaghetti),kitchenClean],[at(home),sated,kitchenClean],cond(notB(kitchenClean), notB(exists(X, isMeal(X), eventually(occ(cook(X)))))),7).

test(58) :-
	writeln('Test case 58'),
	pplan([at(home),hasIngredients(spaghetti)],[at(home),sated,kitchenClean],cond(notB(kitchenClean), notB(exists(X, isMeal(X), eventually(occ(cook(X)))))),7).

test(59) :-
	writeln('Test case 59'),
	pplan([at(home),hasIngredients(spaghetti),kitchenClean],[at(home),sated,kitchenClean],cond(exists(X, isMeal(X), andB(hasIngredients(X),knowsHowToMake(X))),exists(X, isMeal(X), eventually(occ(cook(X))))),7).

test(60) :-
	writeln('Test case 60'),
	pplan([at(home),kitchenClean],[at(home),sated,kitchenClean],cond(exists(X, isMeal(X), andB(hasIngredients(X),knowsHowToMake(X))),exists(X, isMeal(X), eventually(occ(cook(X))))),7).






