generate :- forall(goal(P,G), (concat_atom([P,'.pl'],Filename), tell(Filename), writef(":- consult(initial1).\n\ngoal(%w).\n",[G]), told)).
		       

goal(p1,eventually(always(at(person1,city3)))).
goal(p2,eventually(and(at(person1,city2),next(eventually(at(person1,city3)))))).
goal(p3,eventually(and(at(person1,city2),next(eventually(and(at(person1,city3),next(eventually(at(person1,city4))))))))).
goal(p4,eventually(and(at(person1,city2),next(eventually(and(at(person1,city3),next(eventually(and(at(person1,city4),next(eventually(at(person1,city0)))))))))))).
goal(p5,and(eventually(always(at(person1,city3))),eventually(always(at(person2,city3))))).
goal(p6,and(eventually(and(at(person1,city2),next(eventually(at(person1,city3))))),eventually(and(at(person2,city3),next(eventually(at(person2,city2))))))).
goal(p7,and(eventually(and(at(person1,city2),next(eventually(and(at(person1,city3),next(eventually(at(person1,city4)))))))),eventually(and(at(person2,city4),next(eventually(and(at(person2,city3),next(eventually(at(person2,city2)))))))))).
goal(p8,until(at(plane1,city0),and(at(plane1,city2),until(at(plane1,city2),and(at(plane1,city3),until(at(plane1,city3),at(plane1,city0))))))).
goal(p9,and(until(at(plane2,city3),and(at(plane2,city2),until(at(plane2,city2),at(plane2,city3)))),until(at(plane1,city0),and(at(plane2,city2),until(at(plane2,city2),at(plane2,city0)))))).
goal(p10,and(until(at(plane2,city3),and(at(plane2,city2),until(at(plane2,city2),at(plane2,city3)))),and(until(at(plane1,city0),and(at(plane1,city2),until(at(plane1,city2),at(plane1,city0)))),until(at(plane3,city0),and(at(plane3,city4),until(at(plane3,city4),at(plane3,city0))))))).
goal(p11,and(until(at(plane1,city0),and(at(plane1,city2),until(at(plane1,city2),at(plane1,city3)))),eventually(always(at(person1,city3))))).
goal(p11a,and(until(at(plane1,city0),and(at(plane1,city2),until(at(plane1,city2),at(plane1,city3)))),and(eventually(always(at(person1,city3))),eventually(always(at(person3,city3)))))).
goal(p12,and(until(at(plane2,city3),and(at(plane2,city2),until(at(plane2,city2),at(plane2,city3)))),and(until(at(plane1,city0),and(at(plane1,city2),until(at(plane1,city2),at(plane1,city0)))),eventually(always(at(person1,city3)))))).
goal(p12a,and(until(at(plane2,city3),and(at(plane2,city2),until(at(plane2,city2),at(plane2,city3)))),and(until(at(plane1,city0),and(at(plane1,city2),until(at(plane1,city2),at(plane1,city0)))),and(eventually(always(at(person1,city3))),eventually(always(at(person3,city3))))))).
goal(p13,and(until(at(plane2,city3),and(at(plane2,city2),until(at(plane2,city2),at(plane2,city3)))),and(until(at(plane1,city0),and(at(plane1,city2),until(at(plane1,city2),at(plane1,city0)))),and(until(at(plane3,city0),and(at(plane3,city4),until(at(plane3,city4),at(plane3,city0)))),eventually(always(at(person1,city3))))))).
goal(p13a,and(until(at(plane2,city3),and(at(plane2,city2),until(at(plane2,city2),at(plane2,city3)))),and(until(at(plane1,city0),and(at(plane1,city2),until(at(plane1,city2),at(plane1,city0)))),and(until(at(plane3,city0),and(at(plane3,city4),until(at(plane3,city4),at(plane3,city0)))),and(eventually(always(at(person1,city3))),eventually(always(at(person3,city3)))))))).
goal(p14,
     and(all(p,or(not(aircraft(p)),
		  eventually(at(p,city4)))),
	 all(p,all(y,or(not(aircraft(p)),
			or(not(city(y)),
			   or(not(at(p,y)),eventually(always(at(p,y)))))))))).

goal(p14a,and(all(p,or(not(aircraft(p)),eventually(at(p,city4)))),
	      and(all(p,all(y,or(not(aircraft(p)),
				 or(not(city(y)),or(not(at(p,y)),eventually(always(at(p,y)))))))),eventually(always(at(person1,city3)))))).
goal(p14b,and(all(p,or(not(aircraft(p)),eventually(at(p,city4)))),
	      and(all(p,all(y,or(not(aircraft(p)),
				 or(not(city(y)),or(not(at(p,y)),eventually(always(at(p,y)))))))),eventually(always(at(person1,city3)))))).
goal(p14c,and(all(p,or(not(aircraft(p)),eventually(at(p,city4)))),
	      and(all(p,all(y,or(not(aircraft(p)),
				 or(not(city(y)),or(not(at(p,y)),eventually(always(at(p,y)))))))),and(eventually(always(at(person1,city3))),eventually(always(at(person3,city4))))))).
goal(p15,exists(c,
		and(city(c),
		    eventually(and(and(at(person1,c),
					   at(person2,c)),
				       eventually(always(and(at(person1,city4),at(person2,city3))))))))).

goal(p15a,exists(c,
		and(city(c),
		    eventually(and(and(at(person1,c),and(at(person2,c),at(person3,c))),
				   eventually(always(and(at(person1,city4),at(person2,city3))))))))).


goal(p15b,exists(c,and(city(c),eventually(and(at(person1,c),and(at(person2,c),and(at(person3,c),eventually(always(and(at(person1,city4),and(at(person2,city3),at(person1,city1)))))))))))).



goal(p16,and(eventually(and(at(person1,city1),and(at(person2,city1),next(eventually(and(at(person1,city2),at(person2,city2))))))),and(all(c,or(not(at(person1,c)),eventually(always(at(person1,c))))),all(c,or(not(at(person1,c)),eventually(always(at(person1,c)))))))).

goal(p17,all(c,or(not(city(c)),eventually(at(person1,c))))).
goal(p18,and(all(c,or(not(city(c)),eventually(at(person1,c)))),all(c,or(not(city(c)),eventually(at(person2,c)))))).

goal(p18a,and(all(c,or(not(city(c)),eventually(at(person1,c)))),
	      and(all(c,or(not(city(c)),eventually(at(person2,c)))),
		  next(next(next(always(all(c,or(not(at(person1,c)),at(person2,c)))))))))).


goal(p19,and(eventually(all(p,or(not(person(p)),at(p,city3)))),all(p,all(y,or(not(person(p)),or(not(city(y)),or(not(at(p,y)),eventually(always(at(p,y)))))))))).








