;;; org-if-interpreter.el --- Interpreter for org-if language. -*- lexical-binding: t -*-

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This file contains the interpreter for the org-if language.

;;; Code:

(require 'cl-macs)
(require 'cl-lib)
(require 'org-if-misc)
(require 'org-if-reader)
(require 'subr-x)

(defvar *org-if-funcs* (make-hash-table)
  "Listing of all the functions supplied by org-if.")

(defun org-if-conditional (args)
    "Conditionally evaluate the ARGS."
    (cl-labels ((helper (as)
                        (when (not (null as))
                              (if (= (length as) 1)
                                  (org-if-eval (nth 0 as))
                                  (if (eq 'true (org-if-eval (nth 0 as)))
                                      (org-if-eval (nth 1 as))
                                      (helper (nthcdr 2 as)))))))
      (if (>= (length args) 2)
          (helper args)
          (error "Invalid arguments to org-if: %s"
                 args))))

(defun org-if-insert-message (args)
    "Insert message from ARGS into first major heading."
    (if (stringp args)
      (save-excursion
        (goto-char (org-find-exact-headline-in-buffer "Choices"))
        (open-line 1)
        (insert (concat args "\n")))
      (error "Invalid arguments to print: %s" args)))

(defun org-if-insert-choice (args)
  "Insert link from ARGS into second major heading.
ARGS should be of the form (\"file-path-string\" \"choice description\" [var1 val1 ...])."
  (let* ((link-path      (nth    0 args))
         (link-with-ext  (if (null (file-name-extension link-path))
                             (concat link-path ".org")
                             link-path))
         (link-full-path (expand-file-name (concat (file-name-directory buffer-file-name)
                                                   link-with-ext)))
         (link-desc      (nth    1 args))
         (link-state     (nthcdr 2 args)))
    (if (and (>= (length args) 2)  (cl-evenp (length args))
             (stringp link-path)   (stringp link-desc)
             (or (consp link-state) (null link-state)))
        (progn
          (save-excursion
            (goto-char (org-find-exact-headline-in-buffer "Code"))
            (open-line 1)
            (insert (concat "[[if:"
                            link-full-path
                            (if (not (null link-state))
                                (prin1-to-string link-state)
                                "()")
                            "]["
                            link-desc
                            "]]\n"))))
        (error "Invalid arguments to choice: %s" args))))

(defun org-if-evlis (list)
    "Evaluate every expression in LIST."
    (mapcar #'org-if-eval list))

(defun org-if-getfunc (func)
    "Retrieve the function FUNC from `*org-if-funcs*'."
    (gethash func *org-if-funcs*))

(defun org-if-apply (func args)
  "Call function FUNC with arguments ARGS."
  (apply (org-if-getfunc func)
         (org-if-evlis   args)))

(defmacro org-if-add-func (sym func argtest boolp)
  "Add SYM to `*org-if-funcs*' with primitive function FUNC.
ARGTEST is the test to apply to each function argument.
BOOLP determines whether we should convert nil or non-nil
results into false and true symbols."
  (let ((args     (cl-gensym))
        (results  (cl-gensym)))
    `(puthash ',sym
              #'(lambda (&rest ,args)
                  (if (member nil
                              (mapcar #',argtest
                                      ,args))
                      (error "Invalid argument(s) to %s: %s" ',sym ,args)
                      (let ((,results (apply #',func ,args)))
                        ,(if boolp
                             `(if (null ,results)
                                  'false
                                  'true)
                             results))))
              *org-if-funcs*)))

(org-if-add-func +      +      numberp nil)
(org-if-add-func -      -      numberp nil)
(org-if-add-func *      *      numberp nil)
(org-if-add-func /      /      numberp nil)
(org-if-add-func >      >      numberp t)
(org-if-add-func <      <      numberp t)
(org-if-add-func >=     >=     numberp t)
(org-if-add-func <=     <=     numberp t)
(org-if-add-func <=     <=     numberp t)
(org-if-add-func =
                 (lambda (&rest args)
                   (cond ((numberp (nth 0 args))
                          (apply #'= args))
                         ((stringp (nth 0 args))
                          (apply #'string-equal args))
                         ((or (eq (nth 0 args) 'true)
                              (eq (nth 0 args) 'false))
                          (apply #'eq args))))
                 (lambda (x)
                   (or (numberp x)  (stringp x)
                       (eq x 'true) (eq x 'false)))
                 t)
(org-if-add-func !=
                 (lambda (&rest args)
                   (not (cond ((numberp (nth 0 args))
                               (apply #'= args))
                              ((stringp (nth 0 args))
                               (apply #'string-equal args))
                              ((or (eq (nth 0 args) 'true)
                                   (eq (nth 0 args) 'false))
                               (apply #'eq args)))))
                 (lambda (x)
                   (or (numberp x)  (stringp x)
                       (eq x 'true) (eq x 'false)))
                 t)
(org-if-add-func and
                 (lambda (&rest args)
                   (if (member 'false args)
                       'false
                       'true))
                 (lambda (x)
                   (or (eq x 'true) (eq x 'false)))
                 nil)
(org-if-add-func or
                 (lambda (&rest args)
                   (if (member 'true args)
                       'true
                       'false))
                 (lambda (x)
                   (or (eq x 'true) (eq x 'false)))
                 nil)
(org-if-add-func not
                 (lambda (arg)
                   (if (eq arg 'false)
                       'true
                       'false))
                 (lambda (x)
                   (or (eq x 'true) (eq x 'false)))
                 nil)
(org-if-add-func print org-if-insert-message stringp nil)
(puthash 'reset #'org-if-reset-env *org-if-funcs*)

(defun org-if-eval (exp)
  "Evaluate expression EXP in `*org-if-current-env*'."
  (cond
   ((symbolp exp)        (let ((val (gethash exp
                                             *org-if-current-env*)))
                           (if (not (null val))
                               val
                               (error "Invalid variable: %s" exp))))
   ((integerp exp)       (if (and (>= exp (expt -2 29))
                                  (<= exp (1- (expt 2 29))))
                             exp
                             (error "Integer out of range: %s" exp)))
   ((atom    exp)        exp)
   ((and (consp exp)
         (eq (nth 0 exp)
             'set))      (org-if-set-env     (cdr exp)))
   ((and (consp exp)
         (eq (nth 0 exp)
             'if))       (org-if-conditional (cdr exp)))
   ((and (consp exp)
         (eq (nth 0 exp)
             'choice))   (org-if-insert-choice  (cdr exp)))
   ((consp exp)          (org-if-apply (nth 0 exp) (cdr exp)))
   (t                    (error "Invalid expression: %s" exp))))

(defun org-if-interpret (str)
  "Read & evaluate one or more S-Expressions from string STR."
  (let ((st str))
    (while (not (string-blank-p st))
      (let* ((res (org-if-read st))
             (exp (car res)))
        (setq st (cdr res))
        (when (not (eq exp *org-if-null-token*))
          (print (org-if-eval exp)))))))

(provide 'org-if-interpreter)
;;; org-if-interpreter ends here
