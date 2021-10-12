propose_action(be_polite, Concern, say("Sorry!")) :-
    Concern/bumped,
    retract(Concern/bumped).

propose_action(be_polite, Concern, say("Hey")) :-
    Concern/new_arrival,
    retract(Concern/new_arrival).

score_action(be_polite, _, say("Sorry!"), 100).
score_action(be_polite, _, say("Hey"), 50).

on_event(be_polite, _, collision(X)) :-
    character(X),
    propose_once(say("Sorry!")).

on_event(be_polite, _, enter_conversational_space(X)) :-
    propose_once(say("Hey!")).