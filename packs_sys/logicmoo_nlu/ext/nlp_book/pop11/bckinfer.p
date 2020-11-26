;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; bckinfer.p [Chapter  9] Backwards inference

;;; NB NOT uses negation by failure

lib subst;
lib tunify;

vars infrules back_infer_all;

define back_infer(goal);
   vars rule head body1 body2 subst1 subst2 substs predicate len;
   [] -> substs;
   hd(goal) -> predicate;
   length(goal) -> len;
   ;;; special cases
   if predicate = "and" then
      back_infer_all(tl(goal))
   elseif predicate = "true" then
      [^empty_subst]
   elseif predicate = "not" and len = 1 then
      if back_infer(goal(2)) then
         []
      else
         [^empty_subst]
      endif
   else
      for rule in infrules do
          if hd(hd(rule)) = predicate and
             length(hd(rule)) = len then
             rename(rule) --> [?head ??body1];
             termunify(goal, head) -> subst1;
             if subst1 then
                apply_subst(subst1, body1) -> body2;
                for subst2 in back_infer_all(body2)
                    do compose_substs(subst1, subst2) :: substs -> substs
                endfor
             endif
          endif
      endfor;
      substs
   endif
enddefine;

define back_infer_all(goals1);
   vars goal goals2 subst1 subst2;
   if goals1 matches [?goal ??goals2] then
      [%
         for subst1 in back_infer(goal) do
             for subst2 in back_infer_all(apply_subst(subst1, goals2)) do
                 compose_substs(subst1, subst2)
             endfor
         endfor
      %]
   elseif goals1 = [] then
      [^empty_subst]
   else
      mishap('Illegal format for goals', [^goals1])
   endif
enddefine;

vars bckinfer; true -> bckinfer;
