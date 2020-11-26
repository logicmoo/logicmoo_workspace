;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; tdparse.p [Chapter  5] Top-down parser for a CF-PSG

vars find_subtrees parse;

;;; returns a list of (parse_tree remaining_string) pairs

define find_trees(goal,string);
   vars cat rule rhs s trees remainder;
   if goal matches [?cat] then           ;;; a category
      [% for rule in rules do
            if rule matches [^goal ??rhs] then
               for s in find_subtrees(rhs,string) do
                  s --> [?trees ?remainder];
                  [[^cat ^^trees] ^remainder]
               endfor
            endif
         endfor %]
   elseif string matches [^goal ??remainder] then
      [[^goal ^remainder]]
   else
      []
   endif
enddefine;

;;;  returns list of (list_of_daughter_trees string_remainder)

define find_subtrees(goals,string);
   vars goal restgoals s1 s2 tree first_remainder remainder othertrees;
   if goals = [] then
      [[[] ^string]]
   elseif goals matches [?goal ??restgoals] then
      [% for s1 in find_trees(goal,string) do
            s1 --> [?tree ?first_remainder];
            for s2 in find_subtrees(restgoals,first_remainder) do
               s2 --> [?othertrees ?remainder];
               [[^tree ^^othertrees] ^remainder]
            endfor
         endfor %]
   else
      []
   endif
enddefine;

define parse(goal,string);
   vars p tree;
   [% for p in find_trees(goal,string) do
         if p matches [?tree []] then      ;;; if no remainder
            tree
         endif
      endfor %]
enddefine;

vars tdparse; true -> tdparse;
