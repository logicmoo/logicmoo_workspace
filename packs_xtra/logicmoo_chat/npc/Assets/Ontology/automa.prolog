%%
%% Declarations to control how automatic beliefs get registered in the database.
%%

:- public automatically_believe/1.

%% automatically_believe(+LF)
%  Forces character to blindly believe LF, if LF is a recognized automatic command.
automatically_believe(~LF) :-
   !,
   automatically_believable(LF, Assertion),
   !,
   retract(Assertion).
automatically_believe(LF) :-
   automatically_believable(LF, Assertion),
   !,
   assert(/remote_control/remote_controled),
   assert(Assertion).

%% automatically_believable(+LF, -ELAssertion)
%  LF can be forcibly believed by asserting ELAssertion.

automatically_believable(hungry($me),
			/physiological_states/hungry).
automatically_believable(thirsty($me),
			/physiological_states/thirsty).
automatically_believable(is_a(Thing, Kind),
			/remote_control/Thing/kind/Kind).
automatically_believable(location(Thing, Place),
			/perception/location/Thing:Place:remote_controled).
automatically_believable(related(Object, Relation, Relatum),
			/remote_control/relations/Object/Relation/Relatum).
