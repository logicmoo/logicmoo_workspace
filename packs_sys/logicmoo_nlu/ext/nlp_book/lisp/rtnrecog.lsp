;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; rtnrecog.lsp [Chapter  3] recognition using RTNs

(uses 'finite)
(uses 'fstape)
(defvar networks)

(defun rtn_recognize (networkname tape)
  (catch
    'rtn
    (dolist (initialnode (initial_nodes (get_network networkname)))
      (rtn_recognize_next networkname initialnode tape ()))
    nil))

(defun get_network (name)
  (cadr (assoc name networks)))

(defun rtn_recognize_next (networkname node tape stack)
  (if (member node (final_nodes (get_network networkname)))
    (rtn_recognize_pop tape stack))
  (dolist (transition (transitions (get_network networkname)))
    (if (equal (trans_node transition) node)
      (let ((label (trans_label transition))
         (newnode (trans_newnode transition)))
        (if (get_network label)
          ;; interpret label as network name
          (rtn_recognize_push label networkname newnode tape stack))
        ;; interpret label as symbol/abbreviation
        (rtn_recognize_traverse label networkname newnode tape stack)))))

(defun rtn_recognize_traverse (label networkname newnode tape stack)
  (dolist (newtape (rtn_recognize_move label tape))
    (rtn_recognize_next networkname newnode newtape stack)))

(defun rtn_recognize_pop (tape stack)
  (if (and (null stack) (null tape))
    (throw 'rtn t)
    (if (not (null stack))    ; not finished
      (rtn_recognize_next
        (caar stack) ; stacked networkname
        (cadar stack) ; stacked node name
        tape
        (cdr stack)))))

(defun rtn_recognize_push (label networkname newnode tape stack)
  (dolist (initialnode (initial_nodes (get_network label)))
    (rtn_recognize_next label initialnode tape
      (cons
        (list networkname newnode)
        stack))))


(defun rtn_recognize_move (label tape)
  (recognize_move label tape))
