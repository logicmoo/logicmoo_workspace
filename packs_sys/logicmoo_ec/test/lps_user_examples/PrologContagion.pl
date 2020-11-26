:- expects_dialect(lps).

% Wong exercise, https://twitter.com/mengwong/status/1239445489520504833
% You need to change infectionInterval (or the dataset) to see contagion
% TODO: add likelihood

% met(Day,Persons)
met(1,[alice,bob]).
met(6,[bob,charlie,delilah]).
met(12,[delilah,edgar,fiona,gertrude,iona]).
met(14,[edgar,fiona,gertrude,hannah,iona]).

% tested(Day,Person,positive/negative)
tested(15,alice,positive).

% infected(DayOfInfection,Person)
:- dynamic infected/2. 

propagate :- 
    retractall(infected(_,_)),
    tested(When,P,positive), infectionInterval(When,Begin,_), 
    assert(infected(Begin,P)), fail.
propagate :-
    met(When,Persons), select(Sick,Persons,Others), once(infected(Begin,Sick)), Begin=<When,
    member(Victim,Others), \+ infected(_,Victim), 
    assert(infected(When,Victim)), fail.
propagate :- forall(infected(When,P), writeln(When/P)).

infectionInterval(Test,Begin,End) :- Begin is Test-5, End is Test+10.