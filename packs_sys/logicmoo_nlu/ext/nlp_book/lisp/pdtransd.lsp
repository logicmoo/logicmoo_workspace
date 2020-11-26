;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; pdtransd.lsp [Chapter  3] push-down transduction

(uses 'finite)
(uses 'fstrans)    ;;; for TRANSDUCE_MOVE

(defun rtn_transduce_move (label tape)
  (transduce_move label tape))

(defun get_network (name)
  (cadr (assoc name networks)))

(defun rtn_transduce_next (networkname node tape stack)
  (if (member node (final_nodes (get_network networkname)))
    (rtn_transduce_pop tape stack))
  (dolist (transition (transitions (get_network networkname)))
    (if (equal (trans_node transition) node)
      (let ((label (trans_label transition))
         (newnode (trans_newnode transition)))
        (if (get_network label)
          ;; interpret label as network name
          (rtn_transduce_push label networkname newnode tape stack))
        ;; interpret label as symbol/abbreviation
        (rtn_transduce_traverse label networkname newnode tape stack)))))

(defun rtn_transduce_pop (tape stack)
  (if (and (null stack) (null (car tape)))
    (throw 'rtn (cadr tape))
    (if (not (null stack))
      (rtn_transduce_next (caar stack) (cadar stack) tape (cdr stack)))))

(defun rtn_transduce_push (label networkname newnode tape stack)
  (dolist (initialnode (initial_nodes (get_network label)))
    (rtn_transduce_next label initialnode tape
      (cons
        (list networkname newnode)
        stack))))

(defun rtn_transduce_traverse (label networkname newnode tape stack)
  (dolist (newtape (rtn_transduce_move label tape))
    (rtn_transduce_next networkname newnode newtape stack)))

(defun rtn_transduce (networkname tape)
  (catch
    'rtn
    (dolist (initialnode (initial_nodes (get_network networkname)))
      (rtn_transduce_next networkname initialnode (list tape ()) ()))))
