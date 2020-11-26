;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; subsumes.p [Chapter  7] Subsumption for DAGs

;;; does the first subsume the second?
;;; This procedure attempts to construct a substitution which
;;; when applied to the first structure gives the second.
;;; This is put in VAR_MAP.  VAR_MAP is used to hold values for
;;; some variables in the first structure.  These are the only
;;; substitutions that can be made for these variables.

;;; The code here will not always correctly handle remainders
;;; when the first DAG contains a variable that only occurs once
;;; and the second has no entry for the relevant feature

uses dagunify;

vars var_map subsumes1 find_and_remove same_dag;

define subsumes(dag1,dag2);
   vars var_map;
   [] -> var_map;
   subsumes1(dag1,dag2)
enddefine;

define subsumes1(dag1,dag2);
   vars feature val1 val2;
   if dag1 = dag2 then
      true
   elseif isvar(dag1) then
      if var_map matches [== [^dag1 ?val1] ==] then
         same_dag(val1,dag2)
      else
         [^dag1 ^dag2]::var_map -> var_map;
         true
      endif
   elseif islist(dag1) and islist(dag2) then
      until isvar(dag1) do
         dag1 --> [[?feature ?val1] ??dag1];
         if feature = "&" then
            val1 -> dag1
         else
            find_and_remove(feature,val1,dag2) -> dag2;
            unless dag2 then return(false) endunless
         endif
      enduntil;
      subsumes1(dag1,dag2)
   else
      false
   endif
enddefine;

define same_dag(dag1,dag2);
   unify(dag1,dag2) = empty_subst
enddefine;

;;; find the value of FEATURE in DAG, checking that it is subsumed by
;;; VAL1.  If so, return the remainder of DAG

define find_and_remove(feature,val1,dag);
   vars value first rest newrest;
   if dag matches [??first [^feature ?value] ??rest] then
      if subsumes1(val1,value) then
         [^^first ^^rest]
      else
         false
      endif
   elseif dag matches [??first [& ?rest]] then
      if isvar(rest) then
         ;;; feature in the first has no analogue in the second, but
         ;;; the second is open. this will only work if the feature
         ;;; value in the first is a variable which only occurs once.
         newvar() -> value;
         if subsumes1(val1,value) then
            dag    ;;; INCORRECT
         else
            false
         endif
      else
         find_and_remove(feature,val1,rest) -> rest;
         if rest then
            [^^first [& ^rest]]
         else
            false
         endif
      endif
   else
      false
   endif
enddefine;
