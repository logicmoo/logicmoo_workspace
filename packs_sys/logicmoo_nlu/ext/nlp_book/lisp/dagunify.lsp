;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; dagunify.lsp [Chapter  7] unification for dags

(uses 'subst)

;;; This library file provides the following utilities for operating
;;; on dags:

;;;(GET_VALUE feature dag subst1) returns (value subst2)
;;;(COMBINE_VALUES value1 value2 substitution1) returns substitution2/nil;
;;;(FIND_FEATURE_VALUE feature dag substitution) returns value/'ANY;
;;;(unify dag1 dag2) returns substitution/nil;
;;;(simplify_features subst dag1) returns dag2;
;;;(PUT_VALUE_IN (feature value) dag subst1) returns (subst2 remainder_dag)

;;; Unification

(defun unify (dag1 dag2)
  (combine_values dag1 dag2 empty_subst))

(defun combine_values (dag1 dag2 substitution)
  (let* (
     (realdag1 (lookup_subst dag1 substitution))
     (realdag2 (lookup_subst dag2 substitution)))
    (if (equal realdag1 realdag2)
      substitution
      (if (isvar realdag1)
        (add_subst realdag1 realdag2 substitution)
        (if (isvar realdag2)
          (add_subst realdag2 realdag1 substitution)
          (if (and (listp realdag1) (listp realdag2))
            ;; make sure that everything in dag1 is in dag2
            (do
              ((subst substitution))
              ((isvar realdag1)
               ;; finally put the rest of dag2 at the end of dag1
               ;; (as long as subst is not nil)
               (and subst (add_subst realdag1 realdag2 subst)))
              (let* (
                 (feature (caar realdag1))
                 (value (lookup_subst (cadar realdag1) subst)))
                (if (equal feature '&)
                  (setq realdag1 value)
                  (let (
                     (subst_dag2 (put_value_in (list feature value) realdag2 subst)))
                    (setq realdag2 (cadr subst_dag2))
                    (setq subst (car subst_dag2))
                    (setq realdag1 (cdr realdag1))
                    (if (null subst) (return nil))))))
            nil))))))

;;; Go through a dag and add the feature-value pair in FPAIR,
;;; adding to the substitution substitution
;;; if necessary.  This function returns in a list:
;;;
;;;    a) the new value of substitution
;;;    b) the rest of the dag
;;;       (i.e. everything except that one feature-value pair)

(defun put_value_in (fpair dag substitution)
  (let*
    ((realdag (lookup_subst dag substitution)))
    (if (consp realdag)
      (let ((value (assoc (car fpair) realdag)))
        (if value
          ;; dag already has a value for that feature
          (list
            (combine_values (cadr value) (cadr fpair) substitution)
            (delete_feature_entry  (car fpair) realdag))
          ;; try the continuation entry
          (let (
             (rest (lookup_subst (cadar (last realdag)) substitution))
             (first (butlast realdag)))
            (if (isvar rest)
              ;; continuation is empty
              (let ((newrest (newvar)))
                (list
                  (add_subst rest (list fpair (list '& newrest)) substitution)
                  (append first (list (list '& newrest))))))
            ;; continuation non-empty - recurse
            (let ((subst_rest (put_value_in fpair rest substitution)))
              (list
                (car subst_rest)
                (append first (cadr subst_rest)))))))
      (if (isvar realdag)
        ;; variable as dag - add to substitution
        (let ((newrest (newvar)))
          (list
            (add_subst realdag (list fpair (list '& newrest)) substitution)
            (list (list '& newrest))))
        (error "Cannot find feature value in atom ~S" (list (car fpair) dag))))))

;;; delete the entry for a given feature in a dag
;;; (guaranteed to come before the continuation entry)

(defun delete_feature_entry (feature dag)
  (if (equal feature (caar dag))
    (cdr dag)
    (cons (car dag) (delete_feature_entry feature (cdr dag)))))

;;; Get value of a feature, adding to the substitution if necessary
;;; return a list consisting of a value and a new substitution

(defun get_value (feature dag substitution)
  (let* (
     (realdag (apply_subst substitution dag))
     (value (and (consp realdag) (assoc feature realdag))))
    (if value
      (list (cadr value) substitution)
      (if (isvar realdag)
        (let* ((newrest (newvar)) (newvalue (newvar)))
          (list
            newvalue
            (add_subst
              realdag
              (list (list feature newvalue) (list '& newrest))
              substitution)))
        (if (consp realdag)
          (let ((rest (apply_subst substitution (cadar (last realdag)))))
            (if (isvar rest)
              (let* ((newrest (newvar)) (newvalue (newvar)))
                (list
                  newvalue
                  (add_subst
                    rest
                    (list (list feature newvalue) (list '& newrest))
                    substitution)))
              (get_value feature rest substitution)))
          '(() ()))))))

;;; find the value associated with a feature in a dag
;;; return ANY if there is no recorded value  

(defun find_feature_value (feature dag substitution)
  (let ((realdag (lookup_subst dag substitution)))
    (if (consp realdag)
      (let
        ((value (assoc feature realdag)))
        (if value
          (lookup_subst (cadr value) substitution)
          (let ((rest (lookup_subst (cadar (last realdag)) substitution)))
            (if (isvar rest)
              'ANY
              (find_feature_value feature rest substitution)))
          ))
      'ANY)))

;;; Version of apply_subst which produces a
;;; new version of a dag which has all the remainders
;;; 'flattened out'

(defun simplify_features (substitution dag)
  (let ((realdag (lookup_subst dag substitution)))
    (if (consp realdag)
      (simplify_features_list substitution realdag)
      realdag)))

(defun simplify_features_list (substitution dag)
  (if (null dag)
    '()
    (if (equal (caar dag) '&)
      (let ((remainder (lookup_subst (cadar dag) substitution)))
        (if (isvar remainder)
          (list (list '& remainder))
          (simplify_features_list substitution remainder)))
      (cons
        (list (caar dag) (simplify_features substitution (cadar dag)))
        (simplify_features_list substitution (cdr dag))))))
