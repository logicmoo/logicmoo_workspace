;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; dbq.lsp [Chapter  9] database query language evaluator

;;; N.B. (because of negation as failure) some kinds of                   
;;; queries return no bindings but only success/failure

(uses 'subst)
(uses 'tunify)
(defvar database)

;;; return a list of substitutions

(defparameter NO nil)
(defparameter YES (list empty_subst))

(defun query (formula)
  (if (listp formula)
    (case (car formula)
      (all
        (process_all
          (cadr formula)          ; x
          (caddr formula)         ; p1
          (caddr (cdr formula)))) ; p2
      (exists
        (process_exists
          (cadr formula)          ; x
          (caddr formula)         ; p1
          (caddr (cdr formula)))) ; p2
      (and
        (process_and
          (cadr formula)          ; p1
          (caddr formula)))       ; p2
      (or
        (process_or
          (cadr formula)          ; p1
          (caddr formula)))       ; p2
      (not
        (process_not
          (cadr formula)))        ; p1
      (true
        YES)               ; return true
      (printout
        (print (cadr formula)) YES) ; return true
      (otherwise (retrieve_all formula)))
    (retrieve_all formula)))

(defun process_all (x p1 p2)
  (dolist (subst (query p1) YES)
    (if (equal (lookup_subst x subst) x)
      (error "ALL condition doesnt bind variable ~S" (list x p1)))
    (if (null
        (query
          (apply_subst
            (add_subst x (lookup_subst x subst) empty_subst)
            p2)))
      (return NO))))

(defun process_exists (x p1 p2)
  (dolist (subst (query p1) NO)
    (if (equal x (lookup_subst x subst))
      (error "EXISTS condition doesnt bind variable ~S" (list x p1)))
    (if (query
        (apply_subst
          (add_subst x (lookup_subst x subst) empty_subst) p2))
      (return YES))))

(defun process_and (p1 p2)
  (let ((results ()))
    (dolist (subst1 (query p1))
      (dolist (subst2 (query (apply_subst subst1 p2)))
        (setq results
          (cons
            (compose_substs subst1 subst2)
            results))))
    results))

(defun process_or (p1 p2)
  (append (query p1) (query p2)))

(defun process_not (p1)
  (if (null (query p1))
    YES
    NO))

(defun retrieve_all (formula)
  (let ((results ()))
    (dolist (term database results)
      (let ((subst (termunify term formula)))
        (if subst
          (setq results (cons subst results)))))))
