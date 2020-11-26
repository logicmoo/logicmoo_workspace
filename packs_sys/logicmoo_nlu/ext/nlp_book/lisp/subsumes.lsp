;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; subsumes.lsp [Chapter  7] subsumption for dags

;;; does the first subsume the second?
;;; This function attempts to construct a substitution which
;;; when applied to the first structure gives the second.
;;; This is put in var_map.  var_map is used to hold values for
;;; some variables in the first structure.  These are the only
;;; substitutions that can be made for these variables.

;;; The code here will not always correctly handle remainders
;;; when the first dag contains a variable that only occurs once
;;; and the second has no entry for the relevant feature

(uses 'dagunify)

(defvar var_map)

(defun subsumes (dag1 dag2)
  (setq var_map nil)
  (subsumes1 dag1 dag2))

(defun subsumes1 (dag1 dag2)
  (if (equal dag1 dag2)
    t
    (if (and (isvar dag1) (assoc dag1 var_map))
      (same_dag dag2 (cadr (assoc dag1 var_map)))
      (if (isvar dag1)
        (progn
          (setq var_map (cons (list dag1 dag2) var_map))
          t)
        (if (and (consp dag1) (consp dag2))
          (do
            ((d1 dag1) (d2 dag2))
            ((or (not d2) (isvar d1)) (and d2 (subsumes1 d1 d2)))
            (let ((fpair (car d1)))
              (if (equal (car fpair) '&)
                (setq d1 (cadr fpair))
                (progn
                  (setq d1 (cdr d1))
                  (setq d2 (find_and_remove fpair d2))))))
          nil)))))

(defun same_dag (dag1 dag2)
  (equal (unify dag1 dag2) empty_subst))

;;; Given a pair (FEATURE VAL),
;;; find the value of FEATURE in dag, checking that it is subsumed by
;;; VAL.  If so, return the remainder of dag.  Otherwise return nil.

(defun find_and_remove (fpair dag)
  (if (consp dag)
    (let ((value (assoc (car fpair) dag)))
      (if value
        (and
          (subsumes1 (cadr fpair) (cadr value))
          (delete_feature_entry (car fpair) dag))
        ;; dag must have a continuation entry
        (let ((rest (cadar (last dag))) (first (butlast dag)))
          (if (isvar rest)
            ;; feature in the first has no analogue in the second, but
            ;; the second is open. this will only work if the feature
            ;; value in the first is a variable which only occurs once.
            (and
              (subsumes1 (cadr fpair) (newvar))
              dag)  ;; INCORRECT
            (let ((rest1 (find_and_remove fpair rest)))
              (and
                rest1
                (append first rest1)))))))
    nil))

;;; delete the entry for a given feature in a dag
;;; (guaranteed to come before the continuation entry)

  (defun delete_feature_entry (feature dag)
    (if (equal feature (caar dag))
      (cdr dag)
      (cons (car dag) (delete_feature_entry feature (cdr dag)))))
