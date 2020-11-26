;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; randftre.p [Chapter  7] Random generation of trees from a PATR grammar

uses randfgen;

vars category;

define generate(dag) -> tree;
   vars rule RHS rs subtrees subst;
   if isword(dag) then
      dag -> tree
   else
      matching_rules(dag) -> rs;
      if rs /= [] then
         oneof(rs) -> rule;
         lhs_match(dag,rule) -> subst -> RHS;
         compose_substs(current_substitution,subst)
           -> current_substitution;
         generate_all(RHS) -> subtrees;
         [^(category(dag,current_substitution))
            ^^subtrees] -> tree
      else
 ;;; mishap('Cannot generate from dag',[^dag])
         false; exitfrom(g)
      endif
   endif
enddefine;

define generate_all(RHS);
   vars first rest;
   if RHS matches [?first ??rest] then
      generate(apply_subst(current_substitution,first)) :: generate_all(rest)
   else
      []
   endif
enddefine;

vars randftre; true -> randftre;
