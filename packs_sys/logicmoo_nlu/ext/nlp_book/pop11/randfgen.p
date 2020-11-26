;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; randfgen.p [Chapter  7] Random generation of sentences from a PATR grammar

uses pop_patr;

vars rules lexical_rules matching_rules generate_all;
vars current_substitution;

define generate(dag) -> output;
   vars rule mrules subst RHS;
   if isword(dag) then
      [^dag] -> output
   else
      matching_rules(dag) -> mrules;
      if mrules /= [] then
         oneof(mrules) -> rule;
         lhs_match(dag, rule) -> subst -> RHS;
         compose_substs(current_substitution, subst)
           -> current_substitution;
         generate_all(RHS) -> output
      else
         ;;; mishap('Cannot generate from dag', [^dag])
         false; exitfrom(g)
      endif
   endif
enddefine;

define generate_all(RHS);
   vars first rest;
   if RHS matches [?first ??rest] then
      generate(apply_subst(current_substitution, first))
      <> generate_all(rest)
   else
      []
   endif
enddefine;

define matching_rules(dag);
   vars rule subst x;
   [% for rule in rules do
         lhs_match(dag, rule) -> subst -> x;
         if subst then rule endif
      endfor;
      for rule in lexical_rules do
         lhs_match(dag, rule) -> subst -> x;
         if subst then rule endif
      endfor
   %]
enddefine;

define g(dag);
   empty_subst -> current_substitution;
   generate(dag)
enddefine;

vars randfgen; true -> randfgen;
