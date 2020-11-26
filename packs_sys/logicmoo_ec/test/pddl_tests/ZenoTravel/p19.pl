:- consult(initial1).

goal(and(eventually(all(p, or(not(person(p)), at(p, city3)))), all(p, all(y, or(not(person(p)), or(not(city(y)), or(not(at(p, y)), eventually(always(at(p, y)))))))))).
