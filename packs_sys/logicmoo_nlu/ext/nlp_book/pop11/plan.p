;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; plan.p [Chapter 10] Simple generation of plans involving illocutionary acts

uses bckinfer;
uses tunify;

vars operators plan plan_next;
vars try_to_achieve_can_do try_planners_own try_already_true try_to_make_true;

[
  [[request _speaker _addressee _act]
   [[can_do _addressee _act] [channel _speaker _addressee]]
   [[want _speaker [request _speaker _addressee _act]]]
   [[believe _addressee [want _speaker _act]]]]

 [ [cause_to_want _agent1 _agent2 _act]
   [[can_do _agent2 _act] [believe _agent2 [want _agent1 _act]]]
   []
   [[want _agent2 _act]]]

 [ [move _agent _source _destination]
   [[at _agent _source]]
   [[want _agent [move _agent _source _destination]]]
   [[at _agent _destination]]]

 [ [inform _speaker _addressee _proposition]
   [_proposition [channel _speaker _addressee]]
   [[want _speaker [inform _speaker _addressee _proposition]]]
   [[believe _addressee [believe _speaker _proposition]]]]

 [ [inform_ref _speaker _addressee _predicate]
   [[knows_ref _speaker _predicate] [channel _speaker _addressee]]
   [[want _speaker [inform_ref _speaker _addressee _predicate]]]
   [[knows_told_ref _addressee _speaker _predicate]]]

 [ [convince_ref _speaker _addressee _predicate]
   [[knows_told_ref _addressee _speaker _predicate]]
   []
   [[knows_ref _addressee _predicate]]]

 [ [convince _speaker _addressee _proposition]
   [[believe _addressee [believe _speaker _proposition]]]
   []
   [[believe _addressee _proposition]]]
] -> operators;

;;; predicates for which there is universal knowledge

vars universal_knowledge;
[channel at can_do knows_ref] -> universal_knowledge;

;;; The main procedure to call
;;; It assumes that the initial world model is in the variable
;;; INFRULES, as required for LIB BCKINFER.
;;; Note that facts recorded in INFRULES are interpreted
;;; outside all belief spaces, so that, for instance,
;;; beliefs of the planning agent need to be explicitly
;;; stated as such.  On the other hand, goals presented to
;;; PLAN are assumed to be interpreted in the planning
;;; agent's belief space.  Eg for the goal [at Alan inside]
;;; the planner actually tries to satisfy
;;; [believe AGENT [at Alan inside]]

define plan(agent,goals);
   vars maxdepth;
   for maxdepth from 0 to 10 do
      ;;; MAXDEPTH limits the number of actions allowed in a
      ;;; possible plan.  Increasing MAXDEPTH by 1 each time
      ;;; results in a kind of breadth-first search
      [trying depth ^maxdepth]=>
      plan_next(agent,goals,[],empty_subst,0,maxdepth)
   endfor;
   false
enddefine;

define plan_next(agent, goals, actions, currentsubst, depth, maxdepth);
   vars x y z goal;
   if goals = [] then
      apply_subst(currentsubst, actions); exitfrom(plan)
   endif;
   goals --> [?goal ??goals];
   apply_subst(currentsubst, goal) -> goal;
   if depth > maxdepth then return
   elseif goal matches [believe ?x [?y ??z]]
    and member(y,universal_knowledge) then
      plan_next(agent,[[^y ^^z] ^^goals],actions,currentsubst,depth,maxdepth)
   elseif goal matches [can_do ?x ?y] then
      try_to_achieve_can_do(x,y,agent,goals,actions,currentsubst,depth,maxdepth)
   else
        ;;; try for it being a want or belief of the planning agent
      try_planners_own(agent,goal,goals,actions,currentsubst,depth,maxdepth);
        ;;; now look for instances of the goal in the database,
        ;;; taking the point of view of the planning agent
      try_already_true(agent, goal, goals, actions, currentsubst, depth,
                       maxdepth);
        ;;; even if one instance of a goal is already true,
        ;;; there may be other instances that are achievable
      try_to_make_true(agent, goal, goals, actions, currentsubst, depth,
                       maxdepth);
   endif
enddefine;

;;; Try to enable X to do action Y. Look up the can_do preconditions
;;; for this action and introduce them as planning goals

define try_to_achieve_can_do(x,y,agent,goals,actions,currentsubst,depth,maxdepth);
   vars operator name can_do want effects subst;
   for operator in operators do
      rename(operator) --> [?name ?can_do ?want ?effects];
      termunify(y,name) -> subst;
      if subst then
         plan_next(agent,
              can_do <> goals,    ;;; since can_do is universal knowledge
              actions,
              compose_substs(subst,currentsubst),
              depth,
              maxdepth)
      endif
   endfor
enddefine;

;;; Check for the goal being a want or belief
;;; of the planning agent. Since the wanter or believer specified
;;; in the goal may not be uninstantiated, we have to consider
;;; other possible wanters/believers as well as the planner

define try_planners_own(agent,goal,goals,actions,currentsubst,depth,maxdepth);
   vars agent1 x subst;
   if goal matches [want ?agent1 =] and termunify(agent,agent1) then
      termunify(agent1,agent) -> subst;
      plan_next(agent,goals,actions,
           compose_substs(subst,currentsubst),depth,maxdepth)
   elseif goal matches [believe ?agent1 ?x] and termunify(agent,agent1) then
      termunify(agent1,agent) -> subst;
      plan_next(agent,[^x ^^goals],actions,
           compose_substs(subst,currentsubst),depth,maxdepth)
   endif
enddefine;

;;; try to match the goal against a fact already known to be true

define try_already_true(agent, goal, goals, actions, currentsubst, depth,
                           maxdepth);                
   vars x dbgoal;
   if goal matches [?x ==] and member(x, universal_knowledge) then
      goal -> dbgoal
   else
      [believe ^agent ^goal] -> dbgoal
   endif;
   for subst in back_infer(dbgoal) do
      plan_next(agent,
           goals,
           actions,
           compose_substs(subst, currentsubst),
           depth,
           maxdepth)
   endfor
enddefine;

;;; see what actions might achieve a given goal, and add their preconditions
;;; to the list of goals

define try_to_make_true(agent, goal, goals, actions, currentsubst, depth,
                           maxdepth);
   vars operator name can_do want effect effects subst;
   for operator in operators do
      rename(operator) --> [?name ?can_do ?want ?effects];
      for effect in effects do
         termunify(goal, effect) -> subst;
         if subst then
            plan_next(agent,
               ;;; don't plan for after the action
                 [^^can_do ^^want],
                 [^name ^^actions],
                 compose_substs(subst, currentsubst),
                 depth+1,
                 maxdepth)
         endif
      endfor
   endfor
enddefine;
