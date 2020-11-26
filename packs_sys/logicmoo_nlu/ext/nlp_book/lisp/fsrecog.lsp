;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fsrecog.lsp [Chapter  2] finite state recognition

(uses 'finite)
(uses 'fstape)

(defun recognize (network tape)
  ;; returns t if sucessfully recognizes tape - nil otherwise
  (catch
    'stop
    (dolist (initialnode (initial_nodes network))
      (recognize_next initialnode tape network))
    nil))  ; failed to recognize

(defun recognize_next (node tape network)
  ;; throws t or returns nil
  (if (and (null tape) (member node (final_nodes network)))
    (throw 'stop t)               ; success
    (dolist (transition (transitions network))
      ;; try each transition of the network
      (if (equal node (trans_node transition)) ; if it starts at the right node
        (dolist (newtape (recognize_move (trans_label transition) tape))
          ;; try each possible new value of tape
          (recognize_next (trans_newnode transition) newtape network))))))
