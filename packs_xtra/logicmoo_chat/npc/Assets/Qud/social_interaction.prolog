%%%
%%% Social_interaction qud
%%% Waits for someone to say hi and then spawns a conversation qud.
%%%

standard_qud(social_interaction, 1).

on_event(greet(Speaker, $me),
	 social_interaction,
	 SocialInteraction,
	 launch_conversation(SocialInteraction,
			     Speaker,
			     greet(Speaker, $me))
	) :-
   Speaker \= player,
    \+(SocialInteraction/quds/_/partner/Speaker).

on_event(greet($me, Target),
	 social_interaction,
	 SocialInteraction,
	 launch_conversation(SocialInteraction,
			     Target,
			     greet($me, Target))
	) :-
   Target \= player,
   \+(SocialInteraction/quds/_/partner/Target).


