%%%
%%% The Patrol qud
%%% Causes character to wanter around from object to object
%%%
%%% Internal state:
%%%  Qud/visited/Object:Time      When character last visited each object
%%%

% When you arrive at a new place, update its last visit time and rebid.
on_event( arrived_at(Place), 
  patrol, 
  Qud, 
  begin(assert(Qud/visited/Place: $now), rebid_patrol(Qud))).

% When you start, initialize all objects to being unvisited.
on_enter_state(start, patrol, Qud) :- 
  forall(patrol_destination(P), assert(Qud/visited/P: -100)), 
  rebid_patrol(Qud).

% Visit a prop if it's in a module (and not in another container).
patrol_destination(Prop) :-
   prop(Prop),
   in_module(Prop, _).

%=autodoc
%% patrol_destination( +Prop) is semidet.
%
% Patrol Destination.
%


% Update location bids.
rebid_patrol(Qud) :-  
  forall( Qud/visited/Prop:Time, 
    begin( 
       Score is $now-Time-distance(Prop, $me), 
       assert(Qud/location_bids/Prop:Score))).

%=autodoc
%% rebid_patrol( ?Qud) is semidet.
%
% Rebid Patrol.
%

