;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fsgen.lsp [Chapter  2] exhaustive generation of sentences from a FSTN

(uses 'finite)
(uses 'fstape)

(defun generate_next (node tape network)
  ;; prints out sentences
  (if (member node (final_nodes network))
    (print tape)
    (dolist (transition (transitions network))
      (if (equal node (trans_node transition))
        (dolist (newtape (generate_move (trans_label transition) tape))
          (generate_next (trans_newnode transition) newtape network))
        '())))) ; transition from the wrong node

(defun generate (network)
  ;;  generates valid sentences of the given network
  (dolist (initialnode (initial_nodes network))
          (generate_next initialnode nil network))
          t)
