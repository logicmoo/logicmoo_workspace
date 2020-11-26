#domain time(Time), agent(Agent), phone(Phone;Phone1;Phone2).

time(0..3).
offset(1..1).

agent(agent1).
agent(agent2).
phone(phone1).
phone(phone2).


fluent(ringing(Phone,Phone1)).
fluent(dialTone(Phone)).
fluent(busySignal(Phone)).
fluent(idle(Phone)).
fluent(connected(Phone,Phone1)).
fluent(disconnected(Phone)).

event(pickUp(Agent,Phone)).
event(setDown(Agent,Phone)).
event(dial(Agent,Phone,Phone2)).



initiates(pickUp(Agent,Phone),dialTone(Phone),Time) :- holdsAt(idle(Phone),Time).

terminates(pickUp(Agent,Phone),idle(Phone),Time) :- holdsAt(idle(Phone),Time).

initiates(setDown(Agent,Phone),idle(Phone),Time) :- holdsAt(dialTone(Phone),Time).

terminates(setDown(Agent,Phone),dialTone(Phone),Time) :- holdsAt(dialTone(Phone),Time).

initiates(dial(Agent,Phone1,Phone2),ringing(Phone1,Phone2),Time) :- holdsAt(dialTone(Phone1),Time), holdsAt(idle(Phone2),Time).

terminates(dial(Agent,Phone1,Phone2),dialTone(Phone1),Time) :- holdsAt(dialTone(Phone1),Time), holdsAt(idle(Phone2),Time).

terminates(dial(Agent,Phone1,Phone2),idle(Phone2),Time) :- holdsAt(dialTone(Phone1),Time), holdsAt(idle(Phone2),Time).

initiates(dial(Agent,Phone1,Phone2),busySignal(Phone1),Time) :- holdsAt(dialTone(Phone1),Time), not holdsAt(idle(Phone2),Time).

terminates(dial(Agent,Phone1,Phone2),dialTone(Phone1),Time) :- holdsAt(dialTone(Phone1),Time), not holdsAt(idle(Phone2),Time).

initiates(setDown(Agent,Phone),idle(Phone),Time) :- holdsAt(busySignal(Phone),Time).

terminates(setDown(Agent,Phone),busySignal(Phone),Time) :- holdsAt(busySignal(Phone),Time).

initiates(setDown(Agent,Phone1),idle(Phone1),Time) :- holdsAt(ringing(Phone1,Phone2),Time).

initiates(setDown(Agent,Phone1),idle(Phone2),Time) :- holdsAt(ringing(Phone1,Phone2),Time).

terminates(setDown(Agent,Phone1),ringing(Phone1,Phone2),Time) :- holdsAt(ringing(Phone1,Phone2),Time).

initiates(pickUp(Agent,Phone2),connected(Phone1,Phone2),Time) :- holdsAt(ringing(Phone1,Phone2),Time).

terminates(pickUp(Agent,Phone2),ringing(Phone1,Phone2),Time) :- holdsAt(ringing(Phone1,Phone2),Time).

initiates(setDown(Agent,Phone1),idle(Phone1),Time) :- holdsAt(connected(Phone1,Phone2),Time).

initiates(setDown(Agent,Phone1),disconnected(Phone2),Time) :- holdsAt(connected(Phone1,Phone2),Time).

terminates(setDown(Agent,Phone1),connected(Phone1,Phone2),Time) :- holdsAt(connected(Phone1,Phone2),Time).

initiates(setDown(Agent,Phone2),idle(Phone2),Time) :- holdsAt(connected(Phone1,Phone2),Time).

initiates(setDown(Agent,Phone2),disconnected(Phone1),Time) :- holdsAt(connected(Phone1,Phone2),Time).

terminates(setDown(Agent,Phone2),connected(Phone1,Phone2),Time) :- holdsAt(connected(Phone1,Phone2),Time).

initiates(setDown(Agent,Phone),idle(Phone),Time) :- holdsAt(disconnected(Phone),Time).

terminates(setDown(Agent,Phone),disconnected(Phone),Time) :- holdsAt(disconnected(Phone),Time).

happens(pickUp(agent1,phone1),0).

happens(dial(agent1,phone1,phone2),1).

happens(pickUp(agent2,phone2),2).

holdsAt(idle(Phone),0).

:- holdsAt(dialTone(Phone),0).

:- holdsAt(busySignal(Phone),0).

:- holdsAt(ringing(Phone1,Phone2),0).

:- holdsAt(connected(Phone1,Phone2),0).

:- holdsAt(disconnected(Phone),0).

:- releasedAt(ringing(Phone1,Phone2),0).

:- releasedAt(dialTone(Phone),0).

:- releasedAt(busySignal(Phone),0).

:- releasedAt(idle(Phone),0).

:- releasedAt(connected(Phone1,Phone2),0).

:- releasedAt(disconnected(Phone),0).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
