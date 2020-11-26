;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; lisppatr.lsp [Chapter  7] PATR rules and conditions

;;; PATR conditions are expressed in lists
;;; Individual conditions are separated by commas
;;; An individual condition can be of the form:
;;;        path = VALUE
;;; where path is a list of feature names and VALUE
;;; is either a list of feature names or a simple
;;; symbol or number
;;; An individual condition can also be a single
;;; atom, in which case it is assumed to have an
;;; value for the property PATR_MACRO, which provides
;;; a whole set of conditions for which the atom
;;; is an abbreviation.
;;; Here is an example set of conditions (with
;;; no macros):
;;;
;;;    (((mor form1 stem)   = (mor root))
;;;     ((mor form1 suffix) = s         )
;;;     ((mor form2 stem)   = (mor root))
;;;     ((mor form2 suffix) = NULL      ))
;;;

(uses 'dagunify)

;;; apply_condition is given a set of conditions (e.g. the list
;;; shown above), a dag and a substitution (which may provide
;;; extra information about the dag).  It returns a new
;;; substitution (extending the initial one) which, when
;;; applied to the dag will produce a dag that satisfies the
;;; conditions

(defun apply_condition (entry dag subst)
  (if (null entry)
    subst
    (if (consp (car entry))
      ;; path1 = path2
      (let*
        ((end1_subst1 (apply_path (car entry) dag subst))
         (end1 (car end1_subst1))
         (subst1 (cadr end1_subst1)))
        (if subst1
          (let*
            ((end2_subst2 (apply_path (nth 2 entry) dag subst1))
             (end2 (car end2_subst2))
             (subst2 (cadr end2_subst2)))
            (if subst2
              (let ((newsubst (combine_values end1 end2 subst2)))
                (if newsubst
                  (apply_condition (cdddr entry) dag newsubst)
                  nil))
              nil))
          nil))
      (if (and (symbolp (car entry)) (get (car entry) 'patr_macro))
        ;; macro
        (let
          ((newsubst
             (apply_condition (get (car entry) 'patr_macro) dag subst)))
          (if newsubst
            (apply_condition (cdr entry) dag newsubst)
            nil))
        (error "Illegal PATR entry ~S" entry)))))

;;; Given a sequence of feature names (a path,
;;; e.g. (verb arg0 cat)), return the value at the
;;; end of that path in a given dag.  If the dag
;;; does not yet have that path defined, add to
;;; the substitution subst so that it is.
;;; This function returns a list consisting of:
;;; a) the value at the end of the path
;;; b) the modified substitution

(defun apply_path (path dag subst)
  (if (null path)
    (list dag subst)
    (if (atom path)
      (list path subst)
      (if (consp path)
        (let ((subst_val (get_value (car path) dag subst)))
          (if (car subst_val)
            (apply_path (cdr path) (car subst_val) (cadr subst_val))
            subst_val))
        (error "Ill-formed path ~S" path)))))

;;; try to match a dag with the LHS of a PATR rule.
;;; return a list consisting of
;;;   a) a substitution, or nil
;;;   b) the list of dags corresponding to the RHS
;;;
;;; the substitution returned needs to be applied to
;;; the LHS and anything that might share variables with it

(defun lhs_match (dag patr_rule)
  (if (member '-> (cadr patr_rule))
    ;; normal rule
    (let* (
       (lhs (caadr patr_rule))
       (rhs (cddadr patr_rule))
       (conditions (cddr patr_rule))
       (bigdag
         (cons
           (list lhs dag)
           (append
             ;; create a feature entry for each RHS name
             (mapcar #'new_feature_entry rhs)
             (list (list '& (newvar))))))
       (subst (apply_condition conditions bigdag empty_subst)))
      (if subst
        (list
          subst
          (extract_named_feature_values rhs subst bigdag))
        '(() ())))
    ;; lexical rule
    (let* (
       (rhs (cadr patr_rule))
       (conditions (cddr patr_rule))
       (subst (apply_condition conditions dag empty_subst)))
      (if subst
        (list subst rhs)
        '(() ())))))
                     
;;; create a new (feature value) list for a named feature

(defun new_feature_entry (name)
  (list name (newvar)))

;;; extract from a dag the values of the named list of
;;; features

(defun extract_named_feature_values (names subst dag)
  (if (null names)
    '()
    (cons
      (simplify_features subst (find_feature_value (car names) dag subst))
      (extract_named_feature_values (cdr names) subst dag))))

;;; try to match a rule, given a list of dags for the RHS.  Return a list
;;; containing:
;;;   a) a substitution, or nil
;;;   b) the list of dags corresponding to the LHS and any unused dags
;;; The substitution returned needs to be applied to
;;; the LHS and the RHS

(defun rhs_match (dags patr_rule)
  (if (member '-> (cadr patr_rule))
    ;; normal rule
    (let (
       (lhs (caadr patr_rule))
       (rhs (cddadr patr_rule))
       (conditions (cddr patr_rule))
       )
      ;; make sure there are enough non-lexical dags
      (if (< (length dags) (length rhs))
        '(() ())
        (let* (
           (dag (newvar))
           (restdags (nthcdr (length rhs) dags))
           (bigdag
             (cons
               (list lhs dag)
               (append
                 (do
                   ((needed rhs (cdr needed))
                    (available dags (cdr available))
                    (pairs nil
                      (cons (list (car needed) (car available)) pairs)))
                   ((null needed) pairs))
                 (list (list '& (newvar))))))
           (subst (apply_condition conditions bigdag empty_subst)))
          (if subst
            (list subst (cons (simplify_features subst dag) restdags))
            '(() ())))))
    ;; lexical rule
    (let (
       (rhs (cadr patr_rule))
       (conditions (cddr patr_rule)))
      (if (< (length dags) (length rhs))
        '(() ())
        (let* (
           (restdags (nthcdr (length rhs) dags))
           (useddags (ldiff dags restdags)))
          (if (equal rhs useddags)
            ;; make sure the words are right
            (let* (
               (dag (newvar))
               (subst (apply_condition conditions dag empty_subst)))
              (if subst
                (list subst (cons (simplify_features subst dag) restdags))
                '(() ())))
            '(() ())))))))

;;; make_dag produces a minimal dag that satisfies a set
;;; of conditions

(defun make_dag (conds)
  (let ((dag (newvar)))
    (simplify_features
      (apply_condition conds dag empty_subst)
      dag)))
