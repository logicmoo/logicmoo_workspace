;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; termify.lsp [Chapter  7] translation from dags to terms

;;; This program assumes that a set of 'rules' for how to
;;; do the translation is specified in the global variable
;;; dag_patterns
;;; It will not work with dags that have shared remainders
;;; An example legal value for dag_patterns is the following:
;;;
;;; (setq dag_patterns
;;; '((predicate arg0 arg1)
;;;   (predicate arg0)
;;;   (predicate)
;;;   (conj prop1 prop2)
;;;   (action arg0 arg1)
;;;   ((cat VP) ?meaning ?presupp)
;;;   ((cat V) ?subcat ?preposition ?object1 ?object2 ?meaning ?presupp)
;;;   ((cat PP) ?preposition ?subject ?object ?ppresupp ?objpresupp)
;;;   ((cat NP) ?presupp ?referent)
;;;   ((cat N) ?presupp ?referent)
;;;   ((cat P) ?root ?presupp ?subject ?object)
;;;   ((cat D))
;;;   ((cat Pron))))                      
;;;
;;; Each rule (single line above) specifies the positions which features
;;; are to be assigned in the term representation.  For instance,
;;;    (predicate arg0 arg1)
;;; indicates that the predicate value comes first (will be the
;;; function symbol), the arg0 value next and the arg1 value last.
;;; No other features will be given positions if this rule is used.
;;; termify_dag goes through the rules in sequence, until it finds
;;; one that will apply to the dag it is given.  It then constructs
;;; a term as directed by that rule, calling termify_dag recursively
;;; on any category-valued features.
;;;
;;; The elements in the rules are to be interpreted as follows.
;;; Each element provides a condition that the dag may satisfy
;;; and a specification of what value is to be put in the
;;; corresponding position in the term
;;;
;;; <simple feature name> - in order for the rule to apply, the dag
;;;                         MUST explicitly mention this feature.
;;;                         The value used is the termify_dag of
;;;                         the feature value.
;;; ? <feature name>      - the dag need not explicitly mention this
;;;                         feature.  The value used is the termify_dag
;;;                         of the value in the dag, if there is one,
;;;                         or a new variable otherwise.
;;; * <constant>          - the constant value is to be put in this
;;;                         position in the term.
;;; (<name> <value>)      - the rule only applies if the dag specifies
;;;                         exactly this value for this feature.  In
;;;                         this case, the value used is the value

(uses 'dagunify)
(uses 'subsumes)
(uses 'tunify)
(uses 'tsubsume)

(defvar dag_patterns)

(defun termify_dag (dag)
  (if (consp dag)
    (catch 'got_it
      (dolist (patt dag_patterns)
        (do
          ((remaining patt) (result nil) (failed nil))
          ((or (null remaining) failed)
           (if failed
             nil
             (throw 'got_it (reverse result))))
          (if (equal (car remaining) '*)
            (progn
              (setq result (cons (cadr remaining) result))
              (setq remaining (cddr remaining)))
            (if (equal (car remaining) '?)
              (let
                ((val
                   (find_feature_value (cadr remaining) dag empty_subst)))
                (setq result
                  (cons
                    (if (equal val 'ANY)
                      (newvar)
                      (termify_dag val))
                    result))
                (setq remaining (cddr remaining)))
              (if (consp (car remaining))
                (let
                  ((val
                     (find_feature_value (caar remaining) dag empty_subst)))
                  (if (equal val (cadar remaining))
                    (progn
                      (setq result (cons (termify_dag val) result))
                      (setq remaining (cdr remaining)))
                    (setq failed t)))
                (let
                  ((val
                     (find_feature_value (car remaining) dag empty_subst)))
                  (if (equal val 'ANY)
                    (setq failed t)
                    (progn
                      (setq result (cons (termify_dag val) result))
                      (setq remaining (cdr remaining))))))))))
      (error "No termify rule matches dag ~S" dag))
    dag))
