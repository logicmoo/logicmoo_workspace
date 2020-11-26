;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; tunify.lsp [Chapter  7] term unification

(uses 'subst)

(defun termunify (term1 term2)
  (termunify1 term1 term2 empty_subst))

(defun termunify1 (term1 term2 subst)
  (let (
     (realterm1 (lookup_subst term1 subst))
     (realterm2 (lookup_subst term2 subst)))
    (if (equal realterm1 realterm2)
      subst
      (if (isvar realterm2)
        (add_subst realterm2 realterm1 subst)
        (if (isvar realterm1)
          (add_subst realterm1 realterm2 subst)
          (if (and
              (consp realterm1) (consp realterm2)
              (equal (length realterm1) (length realterm2)))
            (termunify_lists realterm1 realterm2 subst)
            nil))))))

(defun termunify_lists (list1 list2 subst) ; lists assumed same length
  (if (null list1)
    subst
    (let ((newsubst (termunify1 (car list1) (car list2) subst)))
      (if newsubst
        (termunify_lists (cdr list1) (cdr list2) newsubst)
        nil))))
