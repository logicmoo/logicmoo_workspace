;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; subst.lsp [Chapter  7] substitution utilities

;;; Procedures and constants provided:
;;;
;;; empty_subst (a substitution)
;;; (lookup_subst item substitution) returns an item
;;; (add_subst variable item substitution) returns a substitution
;;; (apply_subst substitution structure) returns a newstructure
;;; (compose_substs substitution substitution) returns a substitution
;;;
;;; (isvar item) returns t/nil
;;; (rename item) returns a newitem
;;; (newvar) returns a variable
;;; (ground item) returns t/nil
;;;
;;; New distinct variables are created by rename and newvar, and
;;; these make use of embedded vectors for simplicity and to
;;; ensure distinctness.

;;; operations on variable symbols                     

(defun isvar (x)
  (and
    (symbolp x)
    (equal (char (symbol-name x) 0) #\_))
    ;; EQ not guaranteed to work with characters
  )

(defun ground (term)
  (if (isvar term)
    nil
    (if (listp term)
      (dolist (x term t)
        (if (ground x)
          t
          (return nil)))
      t)))

;;; renames an arbitrary list structure
;;; sublist which satisfies isvar is assumed to denote a variable

(defvar rename_assoc)

(defun rename (list)
  (setq rename_assoc nil)
  (ren list))

(defun ren (list)
  (if (isvar list)
    (rename_var list)
    (if (listp list)
      (mapcar #'ren list)
      list)))

(defun rename_var (v)
  (let
    ((freshvar (cadr (assoc v rename_assoc))))
    (if freshvar
      freshvar
      (progn
        (setq freshvar (newvar))
        (setq rename_assoc (cons (list v freshvar) rename_assoc))
        freshvar))))

;;; generate a new variable

(defun newvar ()
   (gensym '_)
)

;;; operations on substitutions                     

;;; empty_subst is represented as a list containing nil, rather than nil,
;;; in order that nil can be used to represent 'false'
(defparameter empty_subst '(()))

(defun lookup_subst (value substitution)
  (if (and (isvar value) (assoc value substitution))
    (lookup_subst
      (cadr (assoc value substitution))
      substitution)
    value))

(defun add_subst (var value substitution)
  (cons (list var value) substitution))

(defun compose_substs (s1 s2)
  (append s1 s2))

;;; apply a substitution to an
;;; arbitrary list structure

(defun apply_subst (subst item)
  (let ((realitem (lookup_subst item subst)))
    (if (listp realitem)
      (apply_subst_to_list subst realitem)
      realitem)))

(defun apply_subst_to_list (subst list)
  (if (null list)
    '()
    (cons
      (apply_subst subst (car list))
      (apply_subst_to_list subst (cdr list)))))
