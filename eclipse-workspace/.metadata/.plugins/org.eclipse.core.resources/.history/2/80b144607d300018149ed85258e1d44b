:-dynamic in/1,			% matches the in/1 percept
	state/1,			% matches the state/1 percept
	zone/3,				% matches the zone/3 percept
	ownId/1.

% A room is a place with exactly one neighbour, i.e., there is only one way to get to and from that place.
room(PlaceID) :- zone(_,PlaceID,Neighbours), length(Neighbours,1).

% Exercise 2.2: insert a definition of the predicate "nextColorInSeq(Color)".
