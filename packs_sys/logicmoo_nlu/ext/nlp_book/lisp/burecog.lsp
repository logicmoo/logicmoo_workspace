;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; burecog.lsp [Chapter  5] bottom-up recognition for a CF-PSG

(defun next (string)
  (if (equal string '((S)))
    (print '(yes))
    (do
      (
       (left nil (append left (list (car tape))))
       (tape string (cdr tape)))
      ((null tape))      ; do until end of tape
      (dolist (rule rules)
        (let ((newstring (rewrite tape (car rule) (cdr rule))))
          (if (null newstring)
            nil    ; rewrite failed
            (next (append left newstring))))))))

(defun rewrite (string LHS RHS)
  ;; returns a new string if the rule can rewrite it - or nil
  (if (null RHS)      ; successful rewrite
    (cons LHS string)
    (if (null string) ; end of string
      nil
      (if (equal (car string) (car RHS))
        (rewrite (cdr string) LHS (cdr RHS))
        nil)))) ; rule does not match
