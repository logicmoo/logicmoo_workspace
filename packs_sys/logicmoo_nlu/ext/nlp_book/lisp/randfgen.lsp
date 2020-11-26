;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; randfgen.lsp [Chapter  7] random generation of sentences from a PATR grammar

(uses 'lisppatr)

(defvar current_substitution)
(defvar rules)
(defvar lexical_rules)

(defun generate (dag)
  (if (atom dag)
    (list dag)
    (let ((rs (matching_rules dag)))
      (if (null rs)
        (throw 'generate nil)
        (let ((subst_rhs (lhs_match dag (oneof rs))))
          (setq current_substitution
            (compose_substs current_substitution (car subst_rhs)))
          (generate_all (cadr subst_rhs)))))))

(defun generate_all (body)
  (if (null body)
    '()
    (append
      (generate (apply_subst current_substitution (car body)))
      (generate_all (cdr body)))))

(defun oneof (list)
  ;;  randomly returns one of the given list
  (nth (random (length list)) list))

(defun matching_rules (dag)
  (let ((results ()))
    (dolist (rule rules)
      (let ((subst_rhs (lhs_match dag rule)))
        (if (car subst_rhs)
          (setq results (cons rule results)))))
    (dolist (rule lexical_rules)
      (let ((subst_rhs (lhs_match dag rule)))
        (if (car subst_rhs)
          (setq results (cons rule results)))))
    results))

(defun g (description)
  (setq current_substitution empty_subst)
  (catch 'generate
    (generate description)))
