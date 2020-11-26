;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; forinfer.lsp [Chapter  9] forwards inference engine
                     
(uses 'subst)
(uses 'tunify)

(defvar infrules)

(defun for_infer (assertion)
  (let ((to_add nil))
    (if (null (fifind assertion))
      (setq to_add (list assertion))
      (setq to_add nil))
    (do
      ((assert (car to_add) (car to_add)))
      ((null to_add))
      (setq to_add (cdr to_add))
      (princ "Adding ") (princ assert) (terpri)
      (setq infrules (cons (list assert) infrules))
      (dolist (new (consequences assert))
        (if (and
            (null (fifind new))
            (not (member new to_add :test #'equal))
            (not (equal new assert)))
          (setq to_add (cons new to_add)))))))

(defun consequences (assert)
  (let ((results ()))
    (dolist (infrule infrules)
      (dolist (pattern (cdr infrule))
        (if (and
            (equal (car pattern) (car assert))
            (equal (length pattern) (length assert)))
          (let ((subst1 (termunify assert pattern)))
            (if subst1
              (dolist
                (subst2
                  (find_all (apply_subst subst1
                      (remove pattern (cdr infrule)))))
                (setq results
                  (cons
                    (apply_subst (compose_substs subst1 subst2) (car infrule))
                    results))))))))
    results))

(defun find_all (goals)
  (if (null goals)
    (list empty_subst)
    (let ((substs ()))
      (dolist (subst (fifind (car goals)))
        (dolist (newsubst (find_all (apply_subst subst (cdr goals))))
          (setq substs (cons (compose_substs subst newsubst) substs))))
      substs)))

(defun fifind (goal)
  (let ((substs ()))
    (dolist (infrule infrules)
      (if (and
          (equal (caar infrule) (car goal))
          (equal (length (car infrule)) (length goal))
          (null (cdr infrule)))
        (let ((newsubst (termunify goal (car (rename infrule)))))
          (if newsubst
            (setq substs (cons newsubst substs))))))
    substs))
