;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; forinfer.p [Chapter  9] Forwards inference

uses subst;
uses tunify;

vars find find_all infrules consequences;

;;; Add ASSERTION to the database, together with all new facts that
;;; follow from it

define for_infer(assertion);
   vars toadd a new;
   if find(assertion) = [] then
      [^assertion] -> toadd;
   else
      [] -> toadd
   endif;
   until toadd = [] do
      toadd --> [?a ??toadd];
      [adding ^a] =>
      for new in consequences(a) do
         unless find(new) /= [] or member(new, toadd) or new = a then
            [^new ^^toadd] -> toadd
         endunless
      endfor;
      [[^a] ^^infrules] -> infrules;
   enduntil
enddefine;

;;; Return all immediate logical consequences of ASSERTION in a list

define consequences(assertion);
   vars rule pattern predicate len subst1 subst2 toadd RHS patterns2;
   [%
      hd(assertion) -> predicate;
      length(assertion) -> len;
      for rule in infrules do
         tl(rule) -> RHS;
         ;;; look for assertion in RHS of rule
         for pattern in RHS do
            if hd(pattern) = predicate and length(pattern) = len then
               termunify(assertion, pattern) -> subst1;
               if subst1 then
                  ;;; have found it
                  ;;; now see if the other RHS elements are true
                  apply_subst(subst1, delete(pattern, RHS)) -> patterns2;
                  for subst2 in find_all(patterns2) do
                      apply_subst(compose_substs(subst1, subst2), hd(rule))
                  endfor
                endif
             endif
         endfor
      endfor;
   %]
enddefine;

define find_all(goals1);
   vars goal goals2 subst1 subst2;
   if goals1 matches [?goal ??goals2] then
      [%
         for subst1 in find(goal) do
            for subst2 in find_all(apply_subst(subst1,goals2)) do
               compose_substs(subst1,subst2)
            endfor
         endfor
      %]
   elseif goals1 = [] then
      [^empty_subst]
   else
      mishap('Illegal format for goals',[^goals1])
   endif
enddefine;

define find(goal);
   vars rule head body subst substs predicate len;
   [] -> substs;
   hd(goal) -> predicate;
   length(goal) -> len;
   for rule in infrules do
      if hd(hd(rule)) = predicate and length(hd(rule)) = len and tl(rule) = [] then
         rename(rule) --> [?head];
         termunify(goal,head) -> subst;
         if subst then
            subst :: substs -> substs
         endif
      endif
   endfor;
   substs
enddefine;

vars forinfer; true -> forinfer;
