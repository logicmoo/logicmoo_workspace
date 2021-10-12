strategy(Task, run_quip(Quip)) :-
   quip(Task, QuipName, _, _).

strategy(run_quip(Quip),
	 begin(say_speech(Speech),
	       assert(/quips/spoken/$addressee/QuipName),
	       Effect)) :-
   quip(_, QuipName, Speech, Effect).

quip(Task, QuipName, Speech, null) :-
   quip(Task, QuipName, Speech).