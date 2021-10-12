%%
%% The Patrol activity
%%

propose_action(patrol, _, goto(Prop)) :-
    not(/motor_state/walking_to),  % not already going somewhere
    prop(Prop).

score_action(patrol, Activity, goto(Prop), Score) :-
    Activity/visited/Prop:Time,
    Score is ($now-Time)-distance(Prop, $game_object).

on_event(patrol, Activity, arrived_at(Place)) :-
    Time is $now,
    assert(Activity/visited/Place:Time).

on_initiate(patrol, Activity) :-
    forall(prop(P), assert(Activity/visited/P:0)).

:- spawn_activity(patrol, _).
