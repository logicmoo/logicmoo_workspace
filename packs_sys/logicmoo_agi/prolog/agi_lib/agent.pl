
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



rule_set(RuleSet,MaxRuleID):-
  MaxRuleID = 7,
  RuleSet = [
    [1,def_father_1,[-[child,Y,X],-[male,X],+[father,X,Y]]],
    [2,def_mother_1,[-[child,Y,X],-[female,X],+[mother,X,Y]]],
    [3,def_parent_1,[-[father,X,Y],+[parent,X,Y]]],
    [4,def_parent_2,[-[mother,X,Y],+[parent,X,Y]]],
    [5,fact_lucy_1,[+[female,lucy]]],
    [6,fact_alice_1,[+[child,alice,bob]]],
    [7,fact_bob_1,[+[child,bob,lucy]]]
  ].

symbol_set(SymbolSet,MaxSymbolID):-
  MaxSymbolID = 10,
  SymbolSet = [
    [1,vble],[2,father],[3,child],[4,male],[5,female],
    [6,parent],[7,lucy],[8,alice],[9,bob],[10,mother]
  ].


nnet_definition(NNet):-
  InDim = 70,
  HidDim = 32,
  OutDim = 7,
  layer_init(layer1,InDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer2,HidDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer3,HidDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer4,HidDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer5,HidDim,HidDim,relu,[-0.2,0.2]),
  layer_init(layer6,HidDim,OutDim,softmax,[-0.2,0.2]),
  NNet = [layer1,layer2,layer3,layer4,layer5,layer6].

run_main:-
  init_search_time,
  nnet_definition(NNet),
  rule_set(RS,NR),
  symbol_set(SS,NS),
  LM = method(learning(100,0.01),input(2,2),output(7)),
  RM = method(reasoning,input(2,2),output(7)),
  G1 = [-[female,lucy]],
  dnn_sl_resolution(G1,[RS,NR],[SS,NS],NNet,LM,100,_),
  G2 = [-[mother,lucy,bob]],
  dnn_sl_resolution(G2,[RS,NR],[SS,NS],NNet,LM,100,_),
  G3 = [-[mother,lucy,bob],-[female,lucy]],
  dnn_sl_resolution(G3,[RS,NR],[SS,NS],NNet,LM,100,_),
  G4 = [-[parent,lucy,bob]],
  dnn_sl_resolution(G4,[RS,NR],[SS,NS],NNet,RM,100,Path4),
  nl,print_by_line(Path4),nl.


end_of_file.


   re_enumerate_goals_as_stated(from_users,UserGoals),
   expand_to_other_implicit_goals(UserGoals,UserGoalsAll),



_by_users
re_enumerate_agent_goals
expand_to_other_implicit_goals

observe_objectively
observe_subjectively

paraphrase objectively
paraphrase subjectively

rationalize objectively
rationalize subjectively

accomidate subjectively
accomidate objectively


Self-reaction - Reactions to one’s performance can be motivating.
If progress is deemed acceptable, then one will have a feeling of self-efficacy with regards to continuing, and will be motivated towards the achievement of their goal.
A negative self evaluation may also be motivating in that one may desire to work harder providing that they consider the goal as valuable.
Self reaction also allows a person to re-evaluate their goals in conjunction with their attainments (Bandura, 1989).
If a person has achieved a goal, they are likely to re-evaluate and raise the standard (goal); whereas, if a person has not achieved the goal they are likely to re-evaluate and lower the standard (goal) to an achievable goal.
Self-efficacy - One’s belief in the likelihood of goal completion can be motivating in itself.
The idea of self-efficacy has received the most attention from scholars and researchers and thus will be the primary topic of this wiki page and will be discussed in more detail in the section.
rationalize: attempt to explain or justify (one's own or another's behavior or attitude) with logical, plausible reasons, even if these are not true or appropriate.
observe accomidate One contribution of rationalization theory is that it implies additional logical restrictions on psychological constraints.
We show that rationalization theory implies that if an alternative is psychologically feasible in a superset then it must also be feasible in any subset that contains it.
Moreover, any specification of psychological constraints that satisfies this property can be generated as the consequence off some appropriately selected set of rationales that are asymmetric and transitive.
As a result, rationalization theory allows conclusive inferences about preferences even when psychological constraints are entirely unobservable.
Basic rationalization theory allows a conclusive inference of preference follows only from a violation of WARP.
It is precisely when standard theory leads to a contradictory inference of preferences that rationalization theory leads to a conclusive inference of preference.
Further inferences are possible if preferences are assumed to be orders.
In that case it is sometimes possible to uniquely determine a preference order even in the presence of multiple violations of WARP.
In the appendix we provide a list of observed anomalies which may be consistent with rationalization theory.
Returning to the example of Synder’s discrimination study, we show that rationalization theory alone is insufficient to permit the inference of handicapped aversion.
We show that the inference of handicapped aversion requires not only the assumption that decision makers have preference orders but also some specific ad hoc assumptions about what choices Dee can rationalize.
Specifically, it requires the ad-hoc (but natural) assumption that Dee can rationalize watching movie 1 with a person in a wheelchair rather than watching movie 2 alone.
Hence, even partial knowledge of rationales can provide additional insight into preferences.
We fully characterize the inferences of preferences that must follow from rationalization theory aided by ad-hoc provisos on psychological constraints.
In a discussion section following the presentation of formal results we discuss potential methods for ascertaining information about agent’s rationales 


