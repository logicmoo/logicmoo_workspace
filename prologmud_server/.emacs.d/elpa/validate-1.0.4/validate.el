;;; validate.el --- Schema validation for Emacs-lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: lisp
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5") (seq "2.16"))
;; Version: 1.0.4

;;; Commentary:
;;
;; This library offers two functions that perform schema validation.
;; Use this is your Elisp packages to provide very informative error
;; messages when your users accidentally misconfigure a variable.
;; For instance, if everything is fine, these do the same thing:
;;
;;   1.  (validate-variable 'cider-known-endpoints)
;;   2.  cider-known-endpoints
;;
;; However, if the user has misconfigured this variable, option
;; 1. will immediately give them an informative error message, while
;; option 2. won't say anything and will lead to confusing errors down
;; the line.
;;
;; The format and language of the schemas is the same one used in the
;; `:type' property of a `defcustom'.
;;
;;     See: (info "(elisp) Customization Types")
;;
;; Both functions throw a `user-error' if the value in question
;; doesn't match the schema, and return the value itself if it
;; matches.  The function `validate-variable' verifies whether the value of a
;; custom variable matches its custom-type, while `validate-value' checks an
;; arbitrary value against an arbitrary schema.
;;
;; Missing features: `:inline', `plist', `coding-system', `color',
;; `hook', `restricted-sexp'.

;;; License:
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'cus-edit)

(defun validate--check-list-contents (values schemas)
  "Check that all VALUES match all SCHEMAS."
  (when schemas
    (if (not (= (length values) (length schemas)))
        "wrong number of elements"
      (seq-find #'identity (seq-mapn #'validate--check values schemas)))))

(defun validate--indent-by-2 (x)
  (replace-regexp-in-string "^" "  " x))

(defun validate--check (value schema)
  "Return nil if VALUE matches SCHEMA.
If they don't match, return an explanation."
  (let ((args (cdr-safe schema))
        (expected-type (or (car-safe schema) schema))
        (props nil))
    (while (and (keywordp (car args)) (cdr args))
      (setq props `(,(pop args) ,(pop args) ,@props)))
    (setq args (or (plist-get props :args)
                   args))
    (let ((r
           (cl-labels ((wtype           ;wrong-type
                        (tt) (unless (funcall (intern (format "%sp" tt)) value)
                               (format "not a %s" tt))))
             ;; TODO: hook (top-level only).
             (cl-case expected-type
               ((sexp other) nil)
               (variable (cond ((wtype 'symbol))
                               ((not (boundp value)) "this symbol has no variable binding")))
               ((integer number float string character symbol function boolean face)
                (wtype expected-type))
               (regexp (cond ((ignore-errors (string-match value "") t) nil)
                             ((wtype 'string))
                             (t "not a valid regexp")))
               (repeat (cond
                        ((or (not args) (cdr args)) (error "`repeat' needs exactly one argument"))
                        ((wtype 'list))
                        (t (let ((subschema (car args)))
                             (seq-some (lambda (v) (validate--check v subschema)) value)))))
               ((const function-item variable-item) (unless (equal value (car args))
                                                      "not the expected value"))
               (file (cond ((wtype 'string))
                           ((file-exists-p value) nil)
                           ((plist-get props :must-match) "file does not exist")
                           ((not (file-writable-p value)) "file is not accessible")))
               (directory (cond ((wtype 'string))
                                ((file-directory-p value) nil)
                                ((file-exists-p value) "path is not a directory")
                                ((not (file-writable-p value)) "directory is not accessible")))
               (key-sequence (and (wtype 'string)
                                  (wtype 'vector)))
               ;; TODO: `coding-system', `color'
               (coding-system (wtype 'symbol))
               (color (wtype 'string))
               (cons (or (wtype 'cons)
                         (validate--check (car value) (car args))
                         (validate--check (cdr value) (cadr args))))
               ((list group) (or (wtype 'list)
                                 (validate--check-list-contents value args)))
               (vector (or (wtype 'vector)
                           (validate--check-list-contents value args)))
               (alist (let ((value-type (plist-get props :value-type))
                            (key-type (plist-get props :key-type)))
                        (cond ((not value-type) (error "`alist' needs a :value-type"))
                              ((not key-type) (error "`alist' needs a :key-type"))
                              ((wtype 'list))
                              (t (validate--check value
                                          `(repeat (cons ,key-type ,value-type)))))))
               ;; TODO: `plist'
               ((choice radio) (if (not (cdr args))
                                   (error "`choice' needs at least one argument")
                                 (let ((gather (mapcar (lambda (x) (validate--check value x)) args)))
                                   (when (seq-every-p #'identity gather)
                                     (concat "all of the options failed\n"
                                             (mapconcat #'validate--indent-by-2 gather "\n"))))))
               ;; TODO: `restricted-sexp'
               (set (or (wtype 'list)
                        (let ((failed (list t)))
                          (dolist (schema args)
                            (let ((elem (seq-find (lambda (x) (not (validate--check x schema)))
                                                value
                                                failed)))
                              (unless (eq elem failed)
                                (setq value (remove elem value)))))
                          (when value
                            (concat "the following values don't match any of the options:\n  "
                                    (mapconcat (lambda (x) (format "%s" x)) value "\n  "))))))))))
      (when r
        (let ((print-length 4)
              (print-level 2))
          (format "Looking for `%S' in `%S' failed because:\n%s"
                  schema value
                  (if (string-match "\\`Looking" r)
                      r
                    (validate--indent-by-2 r))))))))


;;; Exposed API
;;;###autoload
(defun validate-value (value schema &optional noerror)
  "Check that VALUE matches SCHEMA.
If it matches return VALUE, otherwise signal a `user-error'.

If NOERROR is non-nil, return t to indicate a match and nil to
indicate a failure."
  (let ((report (validate--check value schema)))
    (if report
        (unless noerror
          (user-error "%s" report))
      value)))

;;;###autoload
(defun validate-variable (symbol &optional noerror)
  "Check that SYMBOL's value matches its schema.
SYMBOL must be the name of a custom option with a defined
`custom-type'. If SYMBOL has a value and a type, they are checked
with `validate-value'. NOERROR is passed to `validate-value'."
  (let* ((val (symbol-value symbol))
         (type (custom-variable-type symbol)))
    (if type
        (validate-value val type)
      (if noerror val
        (error "Variable `%s' has no custom-type." symbol)))))

;;;###autoload
(defun validate-mark-safe-local (symbol)
  "Mark SYMBOL as a safe local if its custom type is obeyed."
  (put symbol 'safe-local-variable
       (lambda (val)
         (validate-value val (custom-variable-type symbol) 'noerror))))

(defmacro validate-setq (&rest svs)
  "Like `setq', but throw an error if validation fails.
VALUE is validated against SYMBOL's custom type.

\(fn [SYM VAL] ...)"
  (let ((out))
    (while svs
      (let ((symbol (pop svs))
            (value (if (not svs)
                       (error "`validate-setq' takes an even number of arguments")
                     (pop svs))))
        (push `(if (boundp ',symbol)
                   (setq ,symbol (validate-value ,value (custom-variable-type ',symbol)))
                 (user-error "Trying to validate a variable that's not defined yet: `%s'.\nYou need to require the package before validating"
                             ',symbol))
              out)))
    `(progn ,@(reverse out))))

;;;; ChangeLog:

;; 2017-03-05  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit '2bc1a7c5f09de5deb7f27b2b4ed731271f9f3f05'
;; 
;; 2016-11-17  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit '1c9cdd66501a2f32c59347c56cf4a4316e51ad32'
;; 
;; 2016-11-17  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit 'c30247ea4efbddb8efc0c21634b1e78aac4dea6a'
;; 
;; 2016-10-12  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit '16e0e2338b9539610437e420d968c7084d48eb57'
;; 
;; 2016-05-12  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit '06e8bd7d4c31ba5b10cf5c18a13c5370045cea71'
;; 
;; 2016-05-10  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit '7371d05adf4e86f8c6c507d6a8177abac1680d06'
;; 
;; 2016-05-04  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit '3659f0267f1a70a7141b7d53d8a0696d40247c08'
;; 
;; 2016-05-04  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Add 'packages/validate/' from commit
;; 	'95865f28b0f0b6386b8dcdf2b084f1cd79ffab0d'
;; 
;; 	git-subtree-dir: packages/validate git-subtree-mainline:
;; 	76b6d32e155b55a79d23c15f37cc5d6a647e8f83 git-subtree-split:
;; 	95865f28b0f0b6386b8dcdf2b084f1cd79ffab0d
;; 


(provide 'validate)
;;; validate.el ends here
