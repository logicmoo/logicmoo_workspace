%%
%% Declarations to control how hypnotic beliefs get registered in the database.
%%

:- public hypnotically_believe/1.

%% hypnotically_believe(+LF)
%  Forces character to blindly believe LF, if LF is a recognized hypnotic command.
hypnotically_believe(~LF) :-
   !,
   hypnotically_believable(LF, Assertion),
   !,
   retract(Assertion).
hypnotically_believe(LF) :-
   hypnotically_believable(LF, Assertion),
   !,
   assert(Assertion).

%% hypnotically_believable(+LF, -ELAssertion)
%  LF can be forcibly believed by asserting ELAssertion.

hypnotically_believable(hungry($me),
			/physiological_states/hungry).
hypnotically_believable(thirsty($me),
			/physiological_states/thirsty).
hypnotically_believable(is_a(Thing, Kind),
			/brainwash/Thing/kind/Kind).
hypnotically_believable(location(Thing, Place),
			/perception/location/Thing:Place:brainwashed).
hypnotically_believable(related(Object, Relation, Relatum),
			/brainwash/relations/Object/Relation/Relatum).
