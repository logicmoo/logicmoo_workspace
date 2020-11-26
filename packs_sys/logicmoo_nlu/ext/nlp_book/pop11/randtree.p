;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; randtree.p [Chapter  4] Random generation of parse trees from a CF-PSG

vars matching_rules generate_all category unify;

define generate(description) -> tree;
   vars LHS RHS rs subtrees;
   if isword(description) then
      description -> tree
   else
      matching_rules(description) -> rs;
      if rs /== [] then
         oneof(rs) --> [?LHS ??RHS];
         generate_all(RHS) -> subtrees;
         [^(category(description)) ^^subtrees] -> tree
      else
         mishap('Cannot generate from specification', [^description])
      endif
   endif
enddefine;

define generate_all(RHS);
   vars first rest;
   if RHS matches [?first ??rest] then
      generate(first) :: generate_all(rest)
   else
      []
   endif
enddefine;

define category(description) -> c;
   description --> [?c];
enddefine;

define matching_rules(description);
   vars r LHS;
   [% for r in rules do
         r --> [?LHS ??RHS];
         if unify(description, LHS) then r endif
      endfor
   %]
enddefine;

define unify(x,y);
   x = y
enddefine;

vars randtree; true -> randtree;
