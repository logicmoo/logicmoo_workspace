;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; bubrecog.lsp [Chapter  5] bottom-up, breadth-first recognition for a CF-PSG

(defvar rules)

;;; as in burecog

(defun rewrite (string LHS RHS)
  ;; returns a new string if the rule can rewrite it - or nil
  (if (null RHS)      ; successful rewrite
    (cons LHS string)
    (if (null string) ; end of string
      nil
      (if (equal (car string) (car RHS))
        (rewrite (cdr string) LHS (cdr RHS))
        nil)))) ; rule does not match

(defun recognize (string)
  (do ((alternatives (list string))) ((null alternatives))
    (setq alternatives (next_states_list alternatives))))

;;; Given a list of states, append the next states from all
;;; of them

(defun next_states_list (list)
  (if (null list)
    '()
    (append
      (next_states (car list))
      (next_states_list (cdr list)))))

;;; Given a single state, make the list of next states

(defun next_states (string)
  (if (equal (length string) 1)
    (print (car string)))
  (do (
     (left () (append left (list (car remaining))))
     (remaining string (cdr remaining))
     (results ()))
    ((null remaining) results)      ; do until end of string
    (dolist (rule rules)
      (let ((newstring (rewrite remaining (car rule) (cdr rule))))
        (if newstring
          (setq results (cons (append left newstring) results)))))))
