;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; tsubsume.lsp [Chapter  7] subsumption for terms

(defvar var_map)

(defun termsubsumes (t1 t2)
  (setq var_map nil)
  (tersubs1 t1 t2))

(defun tersubs1 (t1 t2)
  (if (equal t1 t2)
    t
    (if (isvar t1)
      (if (assoc t1 var_map)
        (equal (cadr (assoc t1 var_map)) t2)
        (progn
          (setq var_map
            (cons (list t1 t2) var_map))
          t))
      (if (and (consp t1) (consp t2) (equal (length t1) (length t2)))
        (tersubs_list t1 t2)
        nil))))

(defun tersubs_list (t1 t2) ; lists assumed same length
  (if (null t1)
    t
    (and
      (tersubs1 (car t1) (car t2))
      (tersubs_list (cdr t1) (cdr t2)))))
