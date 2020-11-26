/*=====================================================================
Pronoun Resolution
=====================================================================*/

%resolveDrs(Drs):- numbervars(Drs,0,_), write(d:Drs), nl, fail.

resolveDrs([merge(B1,B2)|A1]-[drs(D,C)|A3]):-
	resolveDrs([B1|A1]-A2),
	resolveDrs([B2|A2]-[drs(D1,C1),drs(D2,C2)|A3]),
	appendLists(D1,D2,D),
	appendLists(C1,C2,C).

resolveDrs([alfa(Referent,Type,Gender,B1)|A1]-A2):-
	potentialAntecedent(A1,Referent,Gender) ,
	properBinding(Type,Referent,B1),
	resolveDrs([B1|A1]-A2).

resolveDrs([drs(D1,C1)|A1]-A2):-
	resolveConds(C1,[drs(D1,[])|A1]-A2).


%resolveConds(I,O):- numbervars((I,O),0,_), write(c:(I==O)), nl, fail.

resolveConds([~B1|Conds],A1-A3):-
	resolveDrs([B1|A1]-[B2,drs(D,C)|A2]) ,
	resolveConds(Conds,[drs(D,[~B2|C])|A2]-A3).

resolveConds([B1 > B2|Conds],A1-A4):-
	resolveDrs([B1|A1]-A2),
	resolveDrs([B2|A2]-[B4,B3,drs(D,C)|A3]),
	resolveConds(Conds,[drs(D,[B3 > B4|C])|A3]-A4).

resolveConds([B1 v B2|Conds],A1-A4):-
	resolveDrs([B1|A1]-[B3|A2]),
	resolveDrs([B2|A2]-[B4,drs(D,C)|A3]) ,
	resolveConds(Conds,[drs(D,[B3 v B4|C])|A3]-A4).

resolveConds([Basic|Conds],[drs(D,C)|A1]-A2):-
	compose(Basic,_Symbol,Arguments),
	simpleTerms(Arguments),
	resolveConds(Conds, [drs(D,[Basic|C])|A1]-A2).

resolveConds([],A-A).
