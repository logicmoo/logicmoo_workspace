
:-module(agent,[]).


toploop(UserOrSelf):- 
  re_enumerate_agent_goals(UserOrSelf,Goals),
  expand_to_other_implicit_goals(UserOrSelf,Goals,GoalsAll),
  get_previous__goals(UserOrSelf,PrevGoals),
  divide(GoalsAll,PrevGoals,NoLonger,Continued,New),
  why_goal_satisfied(UserOrSelf,NoLonger,WhyNoLonger),
  say(WhyNoLonger),
  preconds_of_goals(UserOrSelf,Continued,ContinuedPreconds),
  preconds_of_goals(UserOrSelf,New,NewPreconds),
  divide(ContinuedPreconds,NewPreconds,NoLongerPrecond,PrecondsStillNeeded,ActualNewPreconds),
  note(predconds_still_needed(UserOrSelf,PrecondsStillNeeded)),
  note(released_preconds(UserOrSelf,NoLongerPrecond)),
  note(add_req_preconds(UserOrSelf,ActualNewPreconds)),!.


satisfy(robot,wants(robot,Sit)) :- call_u(Sit),!.
satisfy(robot,wants(robot, know(user,Sit))) :-
  %Sit = prop(robot,name,_), 
  call_u(know(robot,Sit)),!,
  from_to_say(robot,user,Sit).


builtin(Mode,(A,B)):- !,play_each(Mode,A),play_each(Mode,B).
builtin(Mode,(A;B)):- !, (play_each(Mode,A);play_each(Mode,B)).
builtin(_,play_mode(Mode)):- retractall(play_mode(_)),ain(play_mode(Mode)).
builtin(_,noplay_mode):- retractall(play_mode(_)),!.
builtin(_Mode,say(X)):- say(X).
builtin(_Mode,play(Game)):- !, play(Game).
builtin(Mode,play_each(Game)):- !, play_each(Mode,Game).
builtin(_Mode,think(English)):- ain(pipeline(English)).
builtin(_Mode,expect(English)):- since_last_said(UserSaid),!,same_meanings(UserSaid,English).

play_each(A):- play_mode(Mode),!,play_each(Mode,A).
play_each(A):- play_each(ensure,A).

play_each(Mode,A):- clause(builtin(Mode,A),Body),!,call(Body).
play_each(_,A):- play_mode(Mode),!,call(Mode,A).
play_each(_,A):- call_u(A),!.
play_each(_,A):- ain(A),!.

ensure(A):- play_mode(Mode),clause(builtin(Mode,A),Body),!,call(Body).
ensure(Sit):- call_u(Sit),!.
ensure(Sit):- ain(Sit).

:- dynamic(play_mode/1).
  

play(knows_each_others_name):- 
 play_each((
  play_mode(ensure),
  prop(robot,pronoun,"I"),
  prop(user,pronoun,"you"),
  think("i want you to know my name"),
  wants(robot,know(user,prop(robot,name,_))),
  say("My name is Professor Einstein."),
  think("i want me to know your name so I am going to ask you to spell it"),
  ensure(wants(robot,know(robot,list(user,letters_of_first_name,_)))),
  say("Would you spell your first name so that I can know it?"),
 ((expect("no")->play(find_out_why(not(wants(user,know(robot,list(user,letters_of_first_name,_)))))) ;
  (expect("yes")-> ensure(wants(user,know(robot,list(user,letters_of_first_name,_))))))))).



/*
wants(robot,know(robot,prop(user,name,?))),
say("Hi, my name is Professor Einstein."),
think("i want me to know your name's length"),
wants(robot,know(robot,prop(user,name,N),prop(N,length,?))),
think("i want me to know your name's length"),
(prop(user,name,N),prop(N,length,L))=>
prop(user,name,N),prop(N,length,_))
prop(N,length,L) ? 
wants(me,know(user,prop(me,name))))
"Hi, my name is Professor Einstein."
"Would you spell your name for me?"
"D. O. U. G."
"How many letters is your name?"  
"7"
"I've only heard 4 so far.. I think you have 3 to go?"
*/
