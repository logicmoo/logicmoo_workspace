;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fstape.lsp [Chapter  2] tape-moving procedures for finite state networks

;;; single tape moving for recognition

;;; recognition

(defun recognize_move (label tape)
  (if (equal label (car tape))
    (list (cdr tape))
    (if (member (car tape) (assoc label abbreviations))
      (list (cdr tape))
      (if (equal label '|#|)
        (list tape)
        '()))))

;;; single tape moving for generation

(defun generate_move (label tape)
  (if (equal label '|#|)
    (list tape)
    (if (assoc label abbreviations)
      (let ((results '()))
        (dolist (word (cdr (assoc label abbreviations)) results)
          (setq results (cons (append tape (list word)) results))))
      (list (append tape (list label))))))
