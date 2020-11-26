;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fstgen.lsp [Chapter  2] exhaustive generation of sentence pairs from a FST

(uses 'finite)
(uses 'fstape)

(defun generate2 (network)
  (dolist (initialnode (initial_nodes network))
    (generate2_next initialnode '(() ()) network))
  t)

(defun generate2_next (node tape network)
  (if (member node (final_nodes network))
    (print tape)
    (dolist (transition (transitions network))
      (if (equal node (trans_node transition))
        (dolist (newtape (generate2_move (trans_label transition) tape))
          (generate2_next (trans_newnode transition) newtape network))
        '()))))

;;; Tape moving

(defun generate2_move (label tape)
  (if (listp label)    ;; a pair
    (let ((results '()))
      (dolist (newtape1 (generate_move (car label) (car tape)))
        (dolist (newtape2 (generate_move (cadr label) (cadr tape)))
          (setq results (cons (list newtape1 newtape2) results))))
      results)
    (if (equal label '|#|)
      tape            ;; no move character
      (if (assoc label abbreviations)
        (generate2_move_list (cdr (assoc label abbreviations)) tape)
        '()))))

;;; Produce a list of new tapes, given a LIST of labels

(defun generate2_move_list (labels tape)
  (if (null labels)
    '()
    (append
      (generate2_move (car labels) tape)
      (generate2_move_list (cdr labels) tape))))
