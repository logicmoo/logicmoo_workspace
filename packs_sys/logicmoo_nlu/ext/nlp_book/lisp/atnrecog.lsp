;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; atnrecog.lsp [Chapter  3] parsing using an ATN

(uses 'fstape)     ;; for tape-moving functions
(defvar networks)
(defvar abbreviations)

;; Accessing portions of networks

(defun initial_nodes (net)
   (nth 1 (assoc 'Initial net)))

(defun initial_tests (net)
   (nth 2 (assoc 'Initial net)))

(defun initial_actions (net)
   (nth 3 (assoc 'Initial net)))

(defun final_nodes (net)
   (nth 1 (assoc 'Final net)))

(defun final_tests (net)
   (nth 2 (assoc 'Final net)))

(defun final_actions (net)
   (nth 3 (assoc 'Final net)))

(defun transitions (net)
   (cddr net))

(defun trans_node (transition)
   (getf transition 'From))

(defun trans_newnode (transition)
   (getf transition 'to))

(defun trans_label (transition)
   (getf transition 'by))

(defun trans_tests (transition)
   (nth 6 transition))

(defun trans_actions (transition)
   (nth 7 transition))

(defun get_network (name)
  (cadr (assoc name networks)))

(defun regs_used (net)
   (nth 1 (assoc 'Registers net)))

(defun initial_regs (net)
  (list
    (regs_used net)
    (mapcar #'not (regs_used net))))


;;; Stack accessing

(defun stacked_networkname (stack)
  (nth 0 (car stack)))

(defun stacked_node (stack)
  (nth 1 (car stack)))

(defun stacked_regs (stack)
  (nth 2 (car stack)))

(defun stacked_tests (stack)
  (nth 3 (car stack)))

(defun stacked_actions (stack)
  (nth 4 (car stack)))

;;; Top level of ATN interpreter

(defun atn_recognize (networkname tape)
  (catch
    'atn
    (let* (
       (network (get_network networkname))
       (regs_hold
         (doactions (initial_regs network) (initial_actions network) () ())))
      (dolist (initialnode (initial_nodes network))
        (atn_recognize_next
          networkname initialnode tape () (car regs_hold) (cadr regs_hold))))))

;;; tape moving
(defun atn_recognize_move (label tape)
   (recognize_move label tape))

;;; Try all ATN traversals starting at a given node

(defun atn_recognize_next (networkname node tape stack regs hold)
  (if (member node (final_nodes (get_network networkname)))
    (atn_recognize_pop networkname tape stack regs hold))
  (dolist (transition (transitions (get_network networkname)))
    (if (equal (trans_node transition) node)
      (let ((label (trans_label transition))
         (newnode (trans_newnode transition)))
        (if (get_network label)
          ;; interpret label as network name
          (atn_recognize_push label networkname transition tape stack regs hold))
        ;; interpret label as symbol/abbreviation
        (atn_recognize_traverse label networkname transition tape stack regs hold)))))

(defun atn_recognize_pop (networkname tape stack regs hold)
  (if (dotests regs (final_tests (get_network networkname)) hold nil)
    (let (
       (star_newhold
         (dopopactions regs (final_actions (get_network networkname)) hold nil)))
      (if (and (null stack) (null tape))
        ;; end of top-level network
        (throw 'atn (car star_newhold))
        (if (and stack
            ;; end of subsidiary network
            ;; do tests at end of original PUSH
            (dotests
              (stacked_regs stack)
              (stacked_tests stack)
              (cadr star_newhold)        ; hold
              (car star_newhold)))       ; star (result of POP)
          ;; execute actions at end of original PUSH
          (let ( (newregs_newhold
               (doactions
                 (stacked_regs stack)
                 (stacked_actions stack)
                 (cadr star_newhold)            ; hold
                 (car star_newhold))))          ; star
            ;; proceed in original network, using stacked values
            (atn_recognize_next
              (stacked_networkname stack)
              (stacked_node stack)
              tape
              (cdr stack)
              (car newregs_newhold)
              (cadr newregs_newhold))))))))

(defun atn_recognize_push (label networkname transition tape stack regs hold)
  (let ((newnet (get_network label)))
    ;; try tests at start of proposed network
    (if (dotests (initial_regs newnet) (initial_tests newnet) hold nil)
      ;; execute actions at start of new network
      (let ((newregs_newhold
           (doactions
             (initial_regs newnet)
             (initial_actions newnet)
             hold
             nil)))
        ;; explore from all initial nodes
        (dolist (initialnode (initial_nodes newnet))
          (atn_recognize_next
            label
            initialnode
            tape
            (cons           ; new value of stack
              (list
                networkname                     ; network
                (trans_newnode transition)      ; destination node
                regs                            ; registers
                (trans_tests transition)        ; post tests
                (trans_actions transition)      ; post actions
                )
              stack)
            (car newregs_newhold)
            (cadr newregs_newhold)))))))

(defun atn_recognize_traverse (label networkname transition tape stack regs hold)
  ;; try moving the tape
  (dolist (newtape (recognize_move label tape))
    ;; set the star register
    (let ((star (diff_tape newtape tape)))
      ;; try the arc tests
      (if (dotests regs (trans_tests transition) hold star)
        (let (
           (newregs_newhold
             ;; execute the arc actions
             (doactions
               regs
               (trans_actions transition)
               hold
               star)))
          ;; continue from the destination node
          (atn_recognize_next
            networkname
            (trans_newnode transition)
            newtape
            stack
            (car newregs_newhold)
            (cadr newregs_newhold)))))))

(defun diff_tape (newtape oldtape)
  (if (equal newtape oldtape)
    '()
    (car oldtape)))

;;; Actions and Tests

(defun dopopactions (regs expr hold star)
  (apply
    `(lambda ,(cons 'star (cons 'hold (car regs)))
      (list
        (let () ,@expr)
        hold))
    (cons star (cons hold (cadr regs)))))

(defun doactions (regs actions hold star)
  (apply
    `(lambda ,(cons 'star (cons 'hold (car regs)))
      ,@actions
      (list
        (list (quote ,(car regs))
          (list ,@(car regs))) hold))
    (cons star (cons hold (cadr regs)))
    )
  )

(defun dotests (regs tests hold star)
  (apply
    `(lambda ,(cons 'star (cons 'hold (car regs)))
      ,tests)
    (cons star (cons hold (cadr regs)))
    )
  )
