;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; tunify.p [Chapter  7] Term unification

uses subst;
vars termunify1 var_map;

define termunify(term1,term2);
   termunify1(term1,term2,empty_subst)
enddefine;

define termunify1(term1,term2,subst);
   vars val x;
   lookup_subst(term1,subst) -> term1;
   lookup_subst(term2,subst) -> term2;
   if term1 = term2 then
      subst
   elseif isvar(term2) then
      add_subst(term2,term1,subst)
   elseif isvar(term1) then
      add_subst(term1,term2,subst)
   elseif islist(term1) and islist(term2) then
      unless length(term1) = length(term2) then return(false) endunless;
      for x from 1 to length(term1) do
         if subst then
            termunify1(term1(x),term2(x),subst) -> subst
         endif
      endfor;
      subst
   else
      false
   endif
enddefine;

vars tunify; true -> tunify;
