;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; dbq.p [Chapter  9] Database query language evaluator

;;; NB (because of negation as failure) some kinds of
;;; queries return no bindings but only success/failure

uses subst;
uses tunify;

vars retrieve_all varstoequals;

;;; return a list of substitutions

define query(formula);
   vars x xval xsubst p1 p2 subst;
   vars subst1 subst2;
   if formula matches [all ?x ?p1 ?p2] then
      for subst in query(p1) do
         lookup_subst(x, subst) -> xval;
         if xval = x then
            mishap('ALL condition does not bind variable', [^x ^p1])
         endif;
         add_subst(x, xval, empty_subst) -> xsubst;
         if query(apply_subst(xsubst,p2)) = [] then
            return([])
         endif
      endfor;
      [^empty_subst]
   elseif formula matches [exists ?x ?p1 ?p2] then
      for subst in query(p1) do
         lookup_subst(x,subst) -> xval;
         if xval = x then
            mishap('EXISTS condition does not bind variable',[^x ^p1])
         endif;
         add_subst(x,xval,empty_subst) -> xsubst;
         if query(apply_subst(xsubst,p2)) /= [] then
            return([^empty_subst])
         endif
      endfor;
      []
   elseif formula matches [and ?p1 ?p2] then
      [%
         for subst1 in query(p1) do
            for subst2 in query(apply_subst(subst1, p2))
               do compose_substs(subst1, subst2)
            endfor
         endfor
      %]
   elseif formula matches [or ?p1 ?p2] then
      query(p1) <> query(p2)
   elseif formula matches [not ?p1] then
      if query(p1) = [] then
         [^empty_subst]
      else
         []
      endif
   elseif formula matches [true] then
      [^empty_subst]
   elseif formula matches [printout ?x] then
      x =>
      [^empty_subst]
   else
      retrieve_all(formula)
   endif
enddefine;

define retrieve_all(formula);
   vars formula1 subst;
   varstoequals(formula) -> formula1;
   [% foreach formula1 do
         termunify(it,formula) -> subst;
         if subst then subst endif
      endforeach
   %]
enddefine;

define varstoequals(list);
   if isvar(list) then "="
   elseif islist(list) then
      maplist(list,varstoequals)
   else list
   endif
enddefine;

vars dbq; true -> dbq;
