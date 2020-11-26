;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; dagunify.p [Chapter  7] Unification for DAGs

uses subst;

vars put_value_in combine_values;

;;; This library file provides the following utilities for operating
;;; on DAGs:

;;;GET_VALUE(feature,dag,subst1) -> subst2 -> value;
;;;COMBINE_VALUES(value1,value2,substitution1) -> substitution2/FALSE;
;;;FIND_FEATURE_VALUE(feature,dag,substitution) -> value/ANY;
;;;UNIFY(dag1,dag2) -> substitution/FALSE;
;;;SIMPLIFY_FEATURES(subst,dag1) -> dag2;
;;;PUT_VALUE_IN(feature,value,dag,subst1) -> remainder_dag -> subst2;

;;; Unification

define unify(dag1, dag2);
   combine_values(dag1, dag2, empty_subst)
enddefine;

define combine_values(dag1, dag2, substitution);
   vars substitution feature value newvalue;
   lookup_subst(dag1, substitution) -> dag1;
   lookup_subst(dag2, substitution) -> dag2;
   if dag1 = dag2 then
      substitution
   elseif isvar(dag1) then
       add_subst(dag1, dag2, substitution)
   elseif isvar(dag2) then
       add_subst(dag2, dag1, substitution)
   elseif islist(dag1) and islist(dag2) then
      ;;; make sure everything in dag1 is in dag2
      until isvar(dag1) do
         dag1 --> [[?feature ?value] ??dag1];
         if feature = "&" then
            lookup_subst(value, substitution) -> dag1
         else
            lookup_subst(value, substitution) -> value;
            put_value_in(feature, value, dag2, substitution)
              -> dag2 -> substitution;
            unless substitution then return(false) endunless
         endif
      enduntil;
      ;;; put the rest of dag2 at the end of dag1
      add_subst(dag1, dag2, substitution);
   else
      false
   endif
enddefine;

;;; Go through a DAG and assign the value NEWVALUE to
;;; the feature FEATURE, adding to the substitution SUBSTITUTION
;;; if necessary.  This procedure returns:
;;;
;;;    a) the new value of SUBSTITUTION
;;;    b) the rest of DAG
;;;       (i.e. everything except that one feature-value pair)

define put_value_in(feature,newvalue,dag,substitution);
   vars value first rest newrest;
   lookup_subst(dag,substitution) -> dag;
   if dag matches [??first [^feature ?value] ??rest] then
      combine_values(value,newvalue,substitution), [^^first ^^rest]
   elseif dag matches [??first [& ?rest]] then
      lookup_subst(rest,substitution) -> rest;
      if isvar(rest) then
         newvar() -> newrest;
         add_subst(rest,[[^feature ^newvalue] [& ^newrest]],substitution);
         [^^first [& ^newrest]];
      else
         put_value_in(feature,newvalue,rest,substitution)
           -> newrest -> substitution;
         substitution, [^^first ^^newrest]
      endif
   elseif isvar(dag) then
      newvar() -> newrest;
      add_subst(dag,[[^feature ^newvalue] [& ^newrest]],substitution),
      newrest
   else
      mishap('Cannot find feature value in atom',[^feature ^dag])
   endif
enddefine;

;;; Get value of a feature, adding to the substitution if necessary
;;; Return:
;;;    a) the new substitution (or FALSE)
;;;    b) the value

define get_value(feature,dag,substitution);
   vars value first rest newrest newvalue;
   lookup_subst(dag,substitution) -> dag;
   if dag matches [??first [^feature ?value] ??rest] then
      lookup_subst(value,substitution), substitution
   elseif dag matches [??first [& ?rest]] then
      lookup_subst(rest,substitution) -> rest;
      if isvar(rest) then
         newvar() -> newrest;
         newvar() -> newvalue;
         newvalue,
         add_subst(rest,[[^feature ^newvalue] [& ^newrest]],substitution)
      else
         get_value(feature,rest,substitution)
      endif
   elseif isvar(dag) then
      newvar() -> newrest;
      newvar() -> newvalue;
      newvalue,
      add_subst(dag,[[^feature ^newvalue] [& ^newrest]],substitution)
   else
      dag, false
   endif
enddefine;

;;; find the value associated with a feature in a dag
;;; return ANY if there is no recorded value

define find_feature_value(feature,dag,substitution);
   vars value rest;
   lookup_subst(dag,substitution) -> dag;
   if dag matches [== [^feature ?value] ==] then
      lookup_subst(value,subst)
   elseif dag matches [== [& ?rest]] then
      lookup_subst(rest,substitution) -> rest;
      if isvar(rest) then
         "ANY"
      else
         find_feature_value(feature,rest,substitution)
      endif
   else
      "ANY"
   endif
enddefine;

;;; Version of APPLY_SUBST which produces a
;;; new version of a DAG which has all the remainders
;;; flattened out

define simplify_features(subst,dag);
   vars f v;
   lookup_subst(dag,subst) -> dag;
   if islist(dag) then
      [%
         until isvar(dag) do
            dag --> [[?f ?v] ??dag];
            if f = "&" then
               lookup_subst(v,subst) -> dag
            else
               [% f, simplify_features(subst,v) %]
            endif
         enduntil;
         [% "&", dag %]
      %]
   else
      dag
   endif
enddefine;

vars dagunify; true -> dagunify;
