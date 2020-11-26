;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; fsbgen.lsp [Chapter  2] breadth-first generation of sentences from a FSTN

(uses 'finite)
(uses 'fstape)

(defun generate (network)
  (let ((agenda (generate_initial_states network)))
    (do () ((null agenda))   ; do until agenda empty
      (setq agenda (generate_next_states_list agenda network)))))
                     
;;; Given a list of states (AGENDA), append together the next states
;;; that arise from all the individual states

(defun generate_next_states_list (agenda network)
  (if (null agenda)
    '()
    (append
      (generate_next_states (car agenda) network)
      (generate_next_states_list (cdr agenda) network))))

;;; calculate the next states from a given state (NODE TAPE)

(defun generate_next_states (node_tape network)
  (if (member (car node_tape) (final_nodes network))
    (print (cadr node_tape)))
  (let ((results '()))
    (dolist (transition (transitions network) results)
      (if (equal (car node_tape) (trans_node transition))
        (dolist (newtape (generate_move (trans_label transition) (cadr node_tape)))
          (setq results (cons (list (trans_newnode transition) newtape) results)))
        '()) ; transition from the wrong node
      )))

;;; create a list of initial states, starting within a given network
;;; and with an empty tape

(defun generate_initial_states (network)
  (let ((results '()))
    (dolist (n (initial_nodes network) results)
      (setq results (cons (cons n '()) results)))))
