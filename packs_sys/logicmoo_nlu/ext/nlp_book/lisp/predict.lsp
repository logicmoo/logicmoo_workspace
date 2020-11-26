;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; predict.lsp [Chapter 10] prediction by plausible inference rules

(uses 'subst)
(uses 'bckinfer)

;;; predict is given a list of assertions (possibly containing
;;; variables) and attempts to predict them simultaneously

(defun predict (assertions)
  (let
    ((substs (back_infer_all assertions)))
    (if (and (null substs) (ground assertions))
      (progn
        (print '(assertions not predicted but unambiguous))
        (dolist (ass assertions)
          (setq infrules (cons (list ass) infrules))))
      (if (equal (length substs) 1)
        (progn
          (print '(assertions uniquely predicted))
          (print (apply_subst (car substs) assertions)))
        (if (null substs)
          (progn
            (print '(assertions ambiguous but no prediction))
            (dolist (ass assertions)
              (setq infrules (cons (list ass) infrules))))
          (progn
            (print '(assertions ambiguous and multiple predictions))
            (dolist (subst substs)
              (print (apply_subst subst assertions)))))))))

;;; example inference rules

(setq infrules
 '(((church (church_of _x)) (town _x))
   ((see people _x) (famous _x) (object _x))
   ((object _x) (spire _x))
   ((spire (spire_of _x)) (church _x))))
