
:-module(agent,[]).


toploop(UserOrSelf):- 
  re_enumerate_agent_goals(UserOrSelf,Goals),
  expand_to_other_implicit_goals(UserOrSelf,Goals,GoalsAll),
  get_previous__goals(UserOrSelf,PrevGoals),
  divide(GoalsAll,PrevGoals,NoLonger,Continued,New),
  why_goal_satisfied(UserOrSelf,NoLonger,WhyNoLonger),
  preconds_of_goals(UserOrSelf,Continued,ContinuedPreconds),
  preconds_of_goals(UserOrSelf,New,NewPreconds),
  divide(ContinuedPreconds,NewPreconds,NoLongerPrecond,PrecondsStillNeeded,ActualNewPreconds),
  note(predconds_still_needed(UserOrSelf,PrecondsStillNeeded)),
  note(released_preconds(UserOrSelf,NoLongerPrecond)),
  note(add_req_preconds(UserOrSelf,ActualNewPreconds)),!.


satisfy(robot,wants(robot,Sit)) :- true(Sit),!.
satisfy(robot,wants(robot, know(user,Sit))) :-
  %Sit = prop(robot,name,_), 
  true(know(robot,Sit)),!,
  from_to_say(robot,user,Sit).

ensure(Sit):- true(Sit),!.
ensure(Sit):- assert(Sit).

play(knows_each_others_name):- 
 play_each(
  self(robot),
  guest(user),
  think("i want you to know my name"),
  ensure(wants(robot,know(user,prop(robot,name,?)))),
  say("My name is Professor Einstein."),
  think("i want me to know your name so I am going to ask you to spell it"),
  ensure(wants(robot,know(robot,list(user,letters_of_first_name,_)))),
  say("Would you spell your first name so that I can know it?"),
 ((expect("no")->play(find_out_why(not(wants(user,know(robot,list(user,letters_of_first_name,_)))))) ;
  (expect("yes")-> ensure(wants(user,know(robot,list(user,letters_of_first_name,_)))))))).



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
“Hi, my name is Professor Einstein.”
“Would you spell your name for me?”
“D. O. U. G.”
“How many letters is your name?”  
“7”
“I've only heard 4 so far.. I think you have 3 to go?”
*/
