;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; tdrecog.lsp [Chapter  5] top-down recognition for a CF-PSG

(defun next (goals string)
  (if (and (null goals) (null string))
    (print '(yes))
    (if (listp (car goals))
      (dolist (rule rules)
        (if (equal (car goals) (car rule))
          (next (append (cdr rule) (cdr goals)) string)))
      (if (equal (car goals) (car string))
        (next (cdr goals) (cdr string))))))
