;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fchart.p [Chapter  7] Chart parser for PATR grammars

;;; A version that does topdown with lexical lookup bottom-up
;;;
uses pop_patr;
uses subsumes;

vars agenda existing_goals agenda_add inactive_edge_procedure;
vars active_edge_procedure category tree lexical_rules rules;

;;; EXISTING_GOALS is used to hold a DAG of the form
;;;    [[CATEGORY ?c] [START ?s] [& ..]],
;;; where c is a category and s a starting position
;;; in the chart.  The presence of one of these in EXISTING_GOALS
;;; indicates that we have already tried (topdown) looking for instances
;;; of the specified category starting at the specified position

define addedge(edge);
   vars substart subfinish subgoal subfound start finish label;
   vars found rest subst subtrees subgoal1;
   add(edge);
   if edge matches [?substart ?subfinish ?subgoal ?subfound []] then
      ;;; inactive edge
      foreach [?start ^substart ?label ?found [?subgoal1 ??rest]] do
         unify(subgoal, subgoal1) -> subst;
         if subst then
            ;;; need to rename here because the same active edge could
            ;;; contribute twice to an edge, eg. VP -> VP PP
            ;;; parsing a VP PP PP construction
            [^^found ^(tree(category(subgoal, subst), subfound))]
             -> subtrees;
            agenda_add(rename(apply_subst(subst,
                 [^start ^subfinish ^label ^subtrees ^rest])))
         endif
      endforeach;
      inactive_edge_procedure(edge)
   elseif edge matches [?start ?finish ?label ?found [?subgoal1 ??rest]] then
      ;;; active edge
      foreach [^finish ?subfinish ?subgoal ?subfound []] do
         unify(subgoal,subgoal1) -> subst;
         if subst then
            ;;; likewise should rename etc...
            [^^found ^(tree(category(subgoal,empty_subst),subfound))]
             -> subtrees;
            agenda_add(rename(apply_subst(subst,
                [^start ^subfinish ^label ^subtrees ^rest])))
         endif
      endforeach;
      active_edge_procedure(edge)
   else
      ;;; malformed edge
      mishap('adding malformed edge',[^edge])
   endif
enddefine;

;;; initialize the chart (top-down version)

define initialize_chart(goal,string);
   vars n others cat needed subst r;
   0 -> n;
   ;;; add lexical edges
   ;;; try each lexical rule in turn on each position in the chart
   ;;; this is inefficient
   until string = [] do
      for r in lexical_rules do
         rhs_match(string,r) -> subst -> others;
         if subst then
            others --> [?cat ??others];
            string --> [??needed ^^others];
            agenda_add([^n ^(n+length(needed)) ^(rename(cat)) [^^needed] []])
         endif
      endfor;
      string --> [= ??string];
      n+1 -> n
   enduntil;
   ;;; add goal edge
   agenda_add([0 0 dummy [] [^(rename(goal))]])
enddefine;

;;; MAIN CHART PARSING PROCEDURE

define chart_parse(goal,string);
   vars edge l found label;
   [] -> agenda;
   [] -> existing_goals;
   [] -> database;
   length(string) -> l;
   initialize_chart(goal,string);
   until agenda = [] do
      agenda --> [?edge ??agenda];
      addedge(edge)
   enduntil;
   [% foreach [0 ^l ?label ?found []] do
      if unify(goal,label) then
         tree(category(label,empty_subst),found);
      endif
   endforeach %]
enddefine;

;;; other top-down parsing procedures
;;; (no bottom-up operations here)

define inactive_edge_procedure(x); enddefine;

;;; The topdown rule
;;;
;;; The topdown rule is invoked when an edge requiring a next phrase of
;;; category SUBGOAL is added.  Now SUBGOAL may be a very general
;;; category and hence not subsumed by any category already recorded in
;;; EXISTING_GOALS.  When SUBGOAL is unified with the LHS of a rule,
;;; however, the result will be more specific and may be subsumed by
;;; an existing goal.  So subsumption by an existing goal is tested
;;; after unification with the LHS of a rule.  On the other hand, once
;;; all the rules have been through, all ways of finding an instance
;;; of the original SUBGOAL category category have been tried, and so
;;; it is this general category that is put into a new entry in
;;; EXISTING_GOALS

define active_edge_procedure(edge);
   vars finish subgoal rule subst LHS RHS goal1 goal2;
   edge --> [= ?finish = = [?subgoal ==]];
   for rule in rules do
      lhs_match(subgoal, rule) -> subst -> RHS;
      if subst then
         apply_subst(subst, subgoal) -> LHS;
         ;;; construct a DAG to compare with existing goals
         [[CATEGORY ^LHS] [START ^finish] [& ^(newvar())]] -> goal2;
         for goal1 in existing_goals do
            if subsumes(goal1, goal2) then
                ;;; continue looking for rules
                nextloop(2)
            endif
         endfor;
         ;;; if goal not subsumed, add the new edge,
         ;;; renaming so that it can combine with other
         ;;; edges from rules using the same variable
         ;;; symbols (e.g. edges from the same rule that just
         ;;; produced it)
         agenda_add(rename([^finish ^finish ^LHS [] ^RHS]))
      endif
   endfor;
   [[[CATEGORY ^subgoal] [START ^finish] [& ^(newvar())]]
      ^^existing_goals] -> existing_goals;
enddefine;

;;; Add an edge to the agenda.  In topdown parsing, the only way that
;;; duplicate edges can be introduced is via the topdown rule (as
;;; long as there are no duplicate edges, the fundamental rule cannot
;;; possibly create any).  So for efficiency the checking of duplications
;;; is done in ACTIVE_EDGE_PROCEDURE.

define agenda_add(edge);
      [^edge ^^agenda] -> agenda
enddefine;

vars fchart; true -> fchart;
