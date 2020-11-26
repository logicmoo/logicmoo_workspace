;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; randftre.lsp [Chapter  7] random generation of trees from a PATR grammar

(uses 'randfgen)

(defun generate (dag)
  (if (atom dag)
    dag
    (let ((rs (matching_rules dag)))
      (if (null rs)
        (throw 'generate nil)
        (let ((subst_rhs (lhs_match dag (oneof rs))))
          (setq current_substitution
            (compose_substs current_substitution (car subst_rhs)))
          (cons
            (category dag current_substitution)
            (generate_all (cadr subst_rhs))))))))

(defun generate_all (body)
  (if (null body)
    '()
    (cons
      (generate (apply_subst current_substitution (car body)))
      (generate_all (cdr body)))))
