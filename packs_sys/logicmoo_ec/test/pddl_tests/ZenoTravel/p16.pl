:- consult(initial1).

goal(and(eventually(and(at(person1, city1), and(at(person2, city1), next(eventually(and(at(person1, city2), at(person2, city2))))))), and(all(c, or(not(at(person1, c)), eventually(always(at(person1, c))))), all(c, or(not(at(person1, c)), eventually(always(at(person1, c)))))))).
