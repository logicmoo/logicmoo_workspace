:- consult(initial1).

goal(and(all(c, or(not(city(c)), eventually(at(person1, c)))), and(all(c, or(not(city(c)), eventually(at(person2, c)))), next(next(next(always(all(c, or(not(at(person1, c)), at(person2, c)))))))))).
