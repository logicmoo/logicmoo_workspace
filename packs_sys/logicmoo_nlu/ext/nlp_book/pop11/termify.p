;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %   Example code from the book "Natural Language Processing in POP-11"  %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; termify.p [Chapter  7] Translation from DAGs to terms

;;; This program assumes that a set of rules for how to
;;; do the translation is specified in the global variable
;;; DAG_PATTERNS
;;; It will not deal adequately with DAGs that have
;;; shared "remainders".
;;; An example legal value for DAG_PATTERNS is the following:
;;;
;;; vars dag_patterns;
;;;
;;; [[predicate arg0 arg1]
;;;  [predicate arg0]
;;;  [predicate]
;;;  [conj prop1 prop2]
;;;  [action arg0 arg1]
;;;  [[cat VP] *meaning *presupp]
;;;  [[cat V] *subcat *preposition *object1 *object2 *meaning *presupp]
;;;  [[cat PP] *preposition *subject *object *ppresupp *objpresupp]
;;;  [[cat NP] *presupp *referent]
;;;  [[cat N] *presupp *referent]
;;;  [[cat P] *root *presupp *subject *object]
;;;  [[cat DET]]
;;;  [[cat PRO]]
;;; ] -> dag_patterns;
;;;
;;; Each rule (single line above) specifies the positions which features
;;; are to be assigned in the term representation.  For instance,
;;;    [PREDICATE ARG0 ARG1]
;;; indicates that the PREDICATE value comes first (will be the
;;; function symbol), the ARG0 value next and the ARG1 value last.
;;; No other features will be given positions if this rule is used.
;;; TERMIFY_DAG goes through the rules in sequence, until it finds
;;; one that will apply to the DAG it is given.  It then constructs
;;; a term as directed by that rule, calling TERMIFY_DAG recursively
;;; on any category-valued features.
;;;
;;; The elements in the rules are to be interpreted as follows.
;;; Each element provides a condition that the DAG may satisfy
;;; and a specification of what value is to be put in the
;;; corresponding position in the term
;;;
;;; <Simple feature name> - in order for the rule to apply, the DAG
;;;                         MUST explicitly mention this feature.
;;;                         The value used is the TERMIFY_DAG of
;;;                         the feature value.
;;; * <feature name>      - the DAG need not explicitly mention this
;;;                         feature.  The value used is the TERMIFY_DAG
;;;                         of the value in the DAG, if there is one,
;;;                         or a new variable otherwise.
;;; @ <constant>          - the constant value is to be put in this
;;;                         position in the term.
;;; [<name> <value>]      - the rule only applies if the DAG specifies
;;;                         exactly this value for this feature.  In
;;;                         this case, the value used is the value

uses dagunify;
uses subsumes;
uses tunify;
uses tsubsume;

vars dag_patterns;

define termify_dag(dag);
   vars patt res name val;
   if islist(dag) then
      for patt in dag_patterns do
         [] -> res;
         until patt = [] do
            if patt matches [@ ?val ??patt] then
               val :: res -> res
            elseif patt matches [* ?name ??patt] then
               find_feature_value(name,dag,empty_subst) -> val;
               if val = "ANY" then newvar() -> val endif;
               termify_dag(val) :: res -> res
            elseif patt matches [[?name ?val] ??patt] then
               unless find_feature_value(name,dag,empty_subst) = val then
                  nextloop(2)
               endunless;
               termify_dag(val) :: res -> res
            elseif patt matches [?name ??patt] then
               find_feature_value(name,dag,empty_subst) -> val;
               if val = "ANY" then nextloop(2) endif;
               termify_dag(val) :: res -> res
            endif
         enduntil;
         return(rev(res))
      endfor;
      mishap('no term translation for dag',[^dag])
   else
      dag
   endif
enddefine;

vars termify; true -> termify;
