;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; tdrecog.p [Chapter  5] Top-down recognition for a CF-PSG

define next(goals, string);
   vars goal restgoals rule subgoals;
   if goals = [] and string = [] then
      [yes] =>
   elseif goals matches [?goal ??restgoals] then
      if islist(goal) then
         for rule in rules do
            if rule matches [^goal ??subgoals] then
               next([^^subgoals ^^restgoals],string)
            endif
         endfor
      elseif string matches [^goal ??string] then
         next(restgoals, string)
      endif
   endif
enddefine;

vars tdrecog; true -> tdrecog;
