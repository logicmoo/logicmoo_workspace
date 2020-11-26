;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; randgen.p [Chapter  4] Random generation from a CF-PSG

vars matching_rules generate_all unify;

define generate(description) -> output;
   vars LHS RHS mrules;
   if isword(description) then
      [^description] -> output
   else
      matching_rules(description) -> mrules;
      if mrules /== [] then
         oneof(mrules) --> [?LHS ??RHS];
         generate_all(RHS) -> output
      else
         mishap('Cannot generate from description', [^description])
      endif
   endif
enddefine;

define generate_all(RHS);
   vars first rest;
   if RHS matches [?first ??rest] then
      generate(first) <> generate_all(rest)
   else
      []
   endif
enddefine;

define matching_rules(description);
   vars rule LHS RHS;
   [% for rule in rules do
          rule --> [?LHS ??RHS];
          if unify(description,LHS) then rule endif
      endfor
   %]
enddefine;

define unify(x, y);
   x = y
enddefine;

vars randgen; true -> randgen;
