on_initiate(conversation, C) :-
    assert(C/state:initiate).

propose_action(conversation, C, ack(greeting)) :-
    C/state:initiate.

on_event(conversation, C, dialog($this, _, ack(greeting))) :-
    assert(C/state:normal).
