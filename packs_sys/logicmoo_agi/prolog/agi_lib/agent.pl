
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




