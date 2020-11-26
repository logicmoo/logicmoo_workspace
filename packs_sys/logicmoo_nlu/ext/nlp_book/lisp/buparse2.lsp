;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; buparse2.lsp [Chapter  5] bottom-up parser CF-PSG with less redundancy

(defvar parses)
(defvar rules)

(defun next (string pos)
  (if (equal (length string) 1)
    (setq parses (cons (car string) parses))
    (do
      (
       (left nil (append left (list (car tape))))
       (len (length string) (- len 1))
       (newpos pos)
       (tape string (cdr tape)))
      ((null tape))      ;; do until end of tape
      (setq newpos (min len newpos))
      (dolist (rule rules)
        (let ((needed_others (initial_segment (cdr rule) tape)))
          (if (and needed_others (< (length (cadr needed_others)) pos))
            (next (append left (list (cons (caar rule) (car needed_others)))
                (cadr needed_others)) newpos))))
      (if (atom (car tape))
        (setq tape nil)))))  ; quit loop
                     
(defun initial_segment (goals string)
  ;; returns nil or a list (needed others)
  (do*
    (
     (restgoals goals (cdr restgoals))
     (needed nil (append needed (list (car reststring))))
     (reststring string (cdr reststring))
     )
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
        (consp (car goals))       ;; category
        (consp (car string))    ;; tree
        (equal (caar goals) (caar string)))
      (and
        (atom (car goals))        ;; word
        (equal (car goals) (car string))))))

(defun parse (string)
  (setq parses nil)
  (next string (length string))
  parses)
