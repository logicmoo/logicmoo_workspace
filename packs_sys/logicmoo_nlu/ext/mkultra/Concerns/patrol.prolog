%%%
%%% The Patrol concern
%%% Causes character to wanter around from object to object
%%%
%%% Internal state:
%%%  Concern/visited/Object:Time      When character last visited each object
%%%

% When you arrive at a new place, update its last visit time and rebid.
on_event(arrived_at(Place),
	 patrol, Concern,
	 begin(assert(Concern/visited/Place: $now),
	       rebid_patrol(Concern))).

% When you start, initialize all objects to being unvisited.
on_enter_state(start, patrol, Concern) :-
   forall(patrol_destination(P),
	  assert(Concern/visited/P:(-100))),
   rebid_patrol(Concern).

% Visit a prop if it's in a room (and not in another container).
patrol_destination(Prop) :-
   prop(Prop),
   in_room(Prop, _).

% Update location bids.
rebid_patrol(Concern) :-
   forall(Concern/visited/Prop:Time,
	  begin(Score is ($now-Time)-distance(Prop, $me),
		assert(Concern/location_bids/Prop:Score))).
