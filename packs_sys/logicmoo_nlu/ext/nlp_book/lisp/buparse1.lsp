;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; buparse1.lsp [Chapter  5] bottom-up parsing for a CF-PSG

(defvar parses)
(defvar rules)

(defun initial_segment (goals string)
  ;; returns nil or a list (needed others)
  (do*
    (
     (restgoals goals (cdr restgoals))
     (needed nil (append needed (list (car reststring))))
     (reststring string (cdr reststring)))
    ((not (rule_item_match restgoals reststring))
     (if (null restgoals)
       (list needed reststring)
       nil))))

;;; does the first item of a list of goals match
;;; the first item in a string?

(defun rule_item_match (goals string)
  (and goals
    (or
      (and
        (consp (car goals))     ; category
        (consp (car string))    ; tree
        (equal (caar goals) (caar string)))
      (and
        (atom (car goals))      ; word
        (equal (car goals) (car string))))))

(defun next (string)
  (if (equal (length string) 1)
    (setq parses (cons (car string) parses))
    (do
      (
       (left nil (append left (list (car tape))))
       (tape string (cdr tape)))
      ((null tape))      ; do until end of tape
      (dolist (rule rules)
        (let ((needed_others (initial_segment (cdr rule) tape)))
          (if (null needed_others)
            nil  ; rewrite failed
            (next (append left (list (cons (caar rule) (car needed_others))) (cadr needed_others)))
            ))))))

(defun parse (string)
  (setq parses nil)
  (next string)
  parses)
