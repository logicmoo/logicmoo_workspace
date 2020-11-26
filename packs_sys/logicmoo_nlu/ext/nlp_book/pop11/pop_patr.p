;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; pop_patr.p [Chapter  7] PATR rules and conditions

;;; PATR conditions are expressed in lists
;;; Individual conditions are separated by commas
;;; An individual condition can be of the form:
;;;        PATH = VALUE
;;; where PATH is a list of feature names and VALUE
;;; is either a list of feature names or a simple
;;; word or number
;;; An individual condition can also be a single
;;; word, in which case it is assumed to have an
;;; entry in the property PATR_MACRO, which provides
;;; a whole set of conditions for which the word
;;; is an abbreviation.
;;; Here is an example set of conditions (with
;;; no macros):
;;;
;;;   [ [mor form1 stem]   = [mor root],
;;;     [mor form1 suffix] = s,
;;;     [mor form2 stem]   = [mor root],
;;;     [mor form2 suffix] = NULL
;;;   ]
;;;
uses dagunify;

vars patr_macro;
newproperty([],100,false,true) -> patr_macro;

vars apply_path;

;;; apply_condition is given a set of conditions (e.g. the list
;;; shown above), a DAG and a substitution (which may provide
;;; extra information about the DAG).  It returns a new
;;; substitution (extending the initial one) which, when
;;; applied to the DAG will produce a DAG that satisfies the
;;; conditions

define apply_condition(entry, dag, subst);
   vars first rest end1 end2;
   if entry matches [??first , ??rest] then
      apply_condition(first, dag, subst) -> subst;
      if subst then
         apply_condition(rest, dag, subst)
      else
         false
      endif
   elseif entry matches [?first = ?rest] then
      apply_path(first, dag, subst) -> subst -> end1;
      if subst then
         apply_path(rest, dag, subst) -> subst -> end2;
         if subst then
            combine_values(end1, end2, subst)
         else
            false
         endif
      else
         false
      endif
   elseif entry matches [?first:isword] then
      patr_macro(first) -> entry;
      unless entry then
         mishap('missing macro definition',[^first])
      endunless;
      apply_condition(entry, dag, subst)
   else
      mishap('ill-formed lexical entry', [^entry])
   endif
enddefine;

;;; Given a sequence of feature names (a path,
;;; e.g. [verb arg0 cat]), return the value at the
;;; end of that path in a given DAG.  If the DAG
;;; does not yet have that path defined, add to
;;; the substitution SUBST so that it is.
;;; Return the substitution FALSE if the path cannot be made
;;; (i.e. if the DAG is atomic)

define apply_path(path, dag, subst);
   vars first rest val;
   if isword(path) or isnumber(path) then
      path, subst
   elseif path = [] then
      dag, subst
   elseif path matches [?first ??rest] then
      newvar() -> val;
      get_value(first, dag, subst);
      -> subst -> val;
      if subst then
         apply_path(rest, val, subst)
      else
         dag, false
      endif
   else
      mishap('ill-formed path', [^path])
   endif
enddefine;

;;; try to match a DAG with the LHS of a PATR rule.
;;; return:
;;;   a) the list of DAGs corresponding to the RHS
;;;   b) a substitution, or FALSE
;;;
;;; the substitution returned needs to be applied to
;;; the LHS and anything that might share variables with it

define lhs_match(dag,patr_rule);
   vars lhs rhs conditions name r bigdag substitution;
   if patr_rule matches [Rule ?lhs -> ??rhs | ??conditions] then
      [% [% lhs, dag %];
         for name in rhs do
            [% name, newvar() %]
         endfor;
         [% "&", newvar() %]
      %] -> bigdag;
      apply_condition(conditions,bigdag,empty_subst) -> substitution;
      if substitution then
         [% until hd(hd(bigdag)) = "&" do
               bigdag --> [[?name ?r] ??bigdag];
               unless name = lhs then
                  simplify_features(substitution,r)
               endunless
            enduntil
         %], substitution
      else
         [], false
      endif
   elseif patr_rule matches [Word ??rhs | ??conditions] then
      apply_condition(conditions,dag,empty_subst) -> substitution;
      if substitution then
         rhs, substitution
      else
         [], false
      endif
   endif
enddefine;

;;; try to match a rule, given a list of DAGs for the RHS.  Return:
;;;   a) the list of DAGs corresponding to the LHS and any unused DAGs
;;;   b) a substitution, or FALSE
;;; the substitution returned needs to be applied to
;;; the LHS and the RHS

define rhs_match(dags,patr_rule);
   vars lhs rhs conditions name bigdag n dag len substitution;
   if patr_rule matches [Rule ?lhs -> ??rhs | ??conditions] then
      length(rhs) -> len;
      ;;; make sure there are enough dags to satisfy the rule
      if length(dags) < len then
         return([], false)
      endif;
      ;;; set up dag for LHS
      newvar() -> dag;
      [% [% lhs, dag %];
         for n from 1 to len do
            [% rhs(n), dags(n) %]
         endfor;
         [% "&", newvar() %]
      %] -> bigdag;
      apply_condition(conditions,bigdag,empty_subst) -> substitution;
      if substitution then
         [% simplify_features(substitution,dag);
            for n from len+1 to length(dags) do
               dags(n)
            endfor
         %], substitution
      else
         [], false
      endif
   elseif patr_rule matches [Word ??rhs | ??conditions] then
      length(rhs) -> len;
      ;;; make sure there are enough dags to satisfy the rule
      if length(dags) < len then
         return([], false)
      endif;
      ;;; make sure the words are identical
      for n from 1 to len do
         unless dags(n) = rhs(n) then
            return([], false)
         endunless;
      endfor;
      newvar() -> dag;
      apply_condition(conditions,dag,empty_subst) -> substitution;
      unless substitution then
         mishap('inconsistent lexical entry',[^rhs])
      endunless;
      [% simplify_features(substitution,dag);
         for n from len+1 to length(dags) do
            dags(n)
         endfor
      %], substitution
   endif
enddefine;

;;; MAKE_DAG produces a minimal DAG that satisfies a set
;;; of conditions

define make_dag(conds);
   vars dag;
   newvar() -> dag;
   simplify_features(
      apply_condition(conds,dag,empty_subst),dag)
enddefine;

vars pop_patr; true -> pop_patr;
