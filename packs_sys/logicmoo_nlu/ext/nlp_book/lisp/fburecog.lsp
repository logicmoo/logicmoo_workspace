;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fburecog.lsp [Chapter  7] bottom-up recognition for PATR grammars

(uses 'lisppatr)

(defvar rules)
(defvar lexical_rules)
(defvar parses)

(defun next (string pos)
  (if (equal (length string) 1)
    (setq parses
      (cons (category (car string) empty_subst) parses))
    (do (
       (len (length string) (- len 1))
       (left nil (append left (list (car tape))))
       (newpos pos)
       (tape string (cdr tape)))
      ((null tape))
      (setq newpos (min len newpos))
      (if (atom (car tape))
        (progn
          (if (< (length (cdr tape)) newpos)
            (dolist (rule lexical_rules)
              (let ((subst_others (rhs_match tape rule)))
                (if (car subst_others)
                  (next
                    (append left (cadr subst_others))
                    (length (cadr subst_others)))))))
          (setq tape nil)) ; quit loop
        (dolist (rule rules)
          (let* (
             (subst_others (rhs_match tape rule))
             (others (cadr subst_others)))
            (if (and
                (car subst_others)
                (< (- (length others) 1) newpos))
              (next
                (apply_subst
                  (car subst_others)
                  (append left others))
                (length others)))))))))

;;; return the list of categories recognized

(defun recognize (string)
  (setq parses nil)
  (next string (length string))
  parses
  )
