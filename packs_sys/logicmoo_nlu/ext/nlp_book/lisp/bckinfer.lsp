;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; bckinfer.lsp [Chapter  9] backwards inference engine

;;; N.B. NOT uses negation by failure

(uses 'subst)
(uses 'tunify)

(defvar infrules)
(defparameter YES (list empty_subst))

(defun back_infer (goal)
  (if (equal (car goal) 'and)
    (back_infer_all (cdr goal))
    (if (equal (car goal) 'true)
      (list YES)
      (if (and (equal (car goal) 'not) (equal (length goal) 2))
        (if (back_infer (cadr goal))
          nil
          (list YES))
        (let ((substs ()))
          (dolist (rule infrules substs)
            (setq substs
              (append
                (solutions_using_rule goal rule)
                substs))))))))

(defun solutions_using_rule (goal rule)
  (let ((substs ()))
    (if (and
        (equal (caar rule) (car goal)) ; check same predicate
        (equal (length (car rule)) (length goal))) ; check same no of args
      (let*
        ((newrule (rename rule))                      
         (headsubst (termunify goal (car newrule))))
        (if headsubst ; check that conclusion of rule matches goal
          (dolist
            (bodysubst
              (back_infer_all
                (apply_subst headsubst (cdr newrule))))
            (setq substs
              (cons
                (compose_substs headsubst bodysubst)
                substs))))))
    substs))

(defun back_infer_all (goals)
  (if (null goals)
    (list empty_subst)
    (let ((substs ()))
      (dolist (subst1 (back_infer (car goals)))
        (dolist (subst2 (back_infer_all (apply_subst subst1 (cdr goals))))
          (setq substs
            (cons
              (compose_substs subst1 subst2)
              substs))))
      substs)))

