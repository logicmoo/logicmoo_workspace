;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; predict.p [Chapter 10] Prediction by plausible inference rules

uses subst;
uses bckinfer;

;;; PREDICT is given a list of assertions (possibly containing
;;; variables) and attempts to predict them simultaneously

define predict(assertions);
   vars substs subst assertion;
   ;;; try to see it as already "predicted"
   back_infer_all(assertions) -> substs;
   if substs = [] and ground(assertions) then
      [assertions not predicted, but unambiguous] =>
      for assertion in assertions do
         [[^assertion] ^^infrules] -> infrules
      endfor
   elseif substs matches [?subst] then
      [assertions uniquely predicted] =>
      apply_subst(subst, assertions) ==>
   elseif substs = [] then
      [assertions ambiguous but no prediction] =>
   else
      [assertions ambiguous and multiple predictions] =>
      for subst in substs do
         apply_subst(subst,assertions) ==>
      endfor
   endif
enddefine;

;;; example inference rules

vars infrules;
[[[church [church_of _x]] [town _x]]
 [[see people _x] [famous _x] [object _x]]
 [[object _x] [spire _x]]
 [[spire [spire_of _x]] [church _x]]] -> infrules;
