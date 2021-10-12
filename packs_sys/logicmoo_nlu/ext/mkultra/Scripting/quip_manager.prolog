%% quip(+Task, ?QuipName, -Speech)
%  QuipName is a quip with Speech that solves Task.
%  Note: Task argument is optional and defaults to Task=QuipName.

:- external quip/2, quip/3.

strategy(Task, run_quip(QuipName)) :-
   quip(Task, QuipName, _).
strategy(Task, run_quip(Task)) :-
   quip(Task, _).

normalize_task(run_quip(String:Markup),
	       begin(monolog([String:Markup]),
		     assert(/quips/spoken/String))) :-
   string(String).

normalize_task(run_quip(String),
	       begin(monolog([String]),
		     assert(/quips/spoken/String))) :-
   string(String).

normalize_task(run_quip(Quip),
	       begin(monolog(Speech),
		     assert(/quips/spoken/Quip))) :-
   quip(_, Quip, Speech)
   ;
   quip(Quip, Speech).

normalize_task(respond_to_quip_markup([M]), respond_to_quip_markup(M)).
normalize_task(respond_to_quip_markup([M | Tail]),
	       (respond_to_quip_markup(M), respond_to_quip_markup(Tail))).

strategy(respond_to_quip_markup(surprised),
	 emote(surprise)).
strategy(respond_to_quip_markup(angry),
	 emote(anger)).
	 
strategy(respond_to_quip_markup(introduce_question(Q, FlavorText)),
	 begin(tell($global::plot_question_introduced(Q)),
	       assert($global::plot_question_flavor_text(Q, FlavorText)),
	       emote(question))).
strategy(respond_to_quip_markup(introduce_goal(G, FlavorText)),
	 begin(tell($global::plot_goal(G)),
	       assert($global::plot_goal_flavor_text(G, FlavorText)))).
strategy(respond_to_quip_markup(clue(C, FlavorText)),
	 begin(tell($global::clue(C)),
	       assert($global::clue_flavor_text(C, FlavorText)))).
strategy(respond_to_quip_markup(reveal(R)),
	 begin(tell($global::revealed(R)),
	       emote(surprise))).
strategy(respond_to_quip_markup(answered(Q)),
	 tell($global::plot_question_answered(Q))).	