;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fstrans.lsp [Chapter  2] finite state transduction

(uses 'finite)
(uses 'fstape)

(defun transduce_move (label tape)
  ;; returns a list of tapes
  (if (listp label)    ; a pair
    (let ((results '()))
      (dolist (newinput (recognize_move (car label) (car tape)))
        (dolist (newoutput (generate_move (cadr label) (cadr tape)))
          (setq results (cons (list newinput newoutput) results))))
      results)
    (if (equal label '|#|)
      (list tape)
      (if (assoc label abbreviations)
        (transduce_move_list (cdr (assoc label abbreviations)) tape)))))                      

;;; Produce a list of new tapes, given a LIST of labels

(defun transduce_move_list (labels tape)
  (if (null labels)
    '()
    (append
      (transduce_move (car labels) tape)
      (transduce_move_list (cdr labels) tape))))

(defun transduce_next (node tape network)
  ;; returns nil or throws an output tape
  (if (and (null (car tape)) (member node (final_nodes network)))
    (throw 'stop (cadr tape))
    (dolist (transition (transitions network))
      (if (equal node (trans_node transition))
        (dolist (newtape (transduce_move (trans_label transition) tape))
          (transduce_next (trans_newnode transition) newtape network))
        nil)))) ; transition from wrong node

(defun transduce (network tape)
  (catch
    'stop
    (dolist (initialnode (initial_nodes network))
      (transduce_next initialnode (list tape nil) network))
    nil))
