constant(X) :- member(X,[plane1,plane2,plane3,
			 person1,person2,person3,person4,person5,person6,
			 city0,city1,city2,city3,city4,
			 fl0,fl1,fl2,fl3,fl4,fl5,fl6]).

initially_true(aircraft(X)) :- member(X,[plane1,plane2,plane3]).
initially_true(person(X)) :- member(X,[person1,person2,person3,person4,person5,person6]).
initially_true(city(X)) :- member(X,[city0,city1,city2,city3,city4]).
initially_true(flevel(X)) :- member(X,[fl0,fl1,fl2,fl3,fl4,fl5,fl6]).

initially_true(at(X,Y)) :- member([X,Y],[[plane1,city0],[plane2,city3],
					 [plane3,city0],[person1,city1],
					 [person2,city0],[person3,city2],
					 [person4,city0],
					 [person5,city3],[person6,city4]]).

initially_true(fuel_level(X,Y)) :- member([X,Y],[[plane1,fl6],[plane3,fl3],[plane2,fl0]]).
initially_true(next(X,Y)) :- member([X,Y],[[fl0,fl1],[fl1,fl2],[fl2,fl3],
					   [fl3,fl4],[fl4,fl5],[fl5,fl6]]).

