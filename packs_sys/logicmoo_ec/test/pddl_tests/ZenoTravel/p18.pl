:- consult(initial1).

goal(and(all(c, or(not(city(c)), eventually(at(person1, c)))), all(c, or(not(city(c)), eventually(at(person2, c)))))).
