;;; hook-helpers.el --- Anonymous, modifiable hook functions -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Maintainer: Ian Dunn <dunni@gnu.org>
;; Keywords: development, hooks
;; Package-Requires: ((emacs "25.1"))
;; URL: https://savannah.nongnu.org/projects/hook-helpers-el/
;; Version: 1.1.1
;; Created: 06 May 2016
;; Modified: 11 Feb 2018

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Often times, I see people define a function to be used once in a hook.  If
;; they don’t do this, then it will be an anonymous function.  If the anonymous
;; function is modified, then the function can’t be removed.  With a function
;; outside of the `add-hook' call, it looks messy.

;; Hook Helpers are a solution to this.  A "hook helper" is an anonymous,
;; modifiable function created for the sole purpose of being attached to a hook.
;; This combines the two commonly used methods mentioned above.  The functions
;; don't exist, so they don't get in the way of `describe-function', but they
;; can be removed or modified as needed.

;;; Code:

(eval-when-compile (require 'subr-x))

;; Compatibility for Emacs < 26.1
(unless (fboundp 'when-let*)
  (defalias 'when-let* 'when-let))

(defvar hkhlp--helpers-map nil
  "Map of IDs to helpers.")

(cl-defstruct hook-helper
  id function hooks source-file)

(defun hkhlp-normalize-hook-spec (hook-spec &optional recursed)
  "Turns HOOK-SPEC into a list of cons-cells, each one (HOOK . APPEND)

HOOK is the name of the full variable to use
APPEND is a Boolean"
  (cond
   ((symbolp hook-spec)
    ;; HOOK
    (list (cons hook-spec nil)))
   ((and (consp hook-spec)
         (symbolp (car hook-spec))
         (booleanp (cdr hook-spec)))
    ;; (HOOK . APPEND)
    (list hook-spec))
   ((and (listp hook-spec) (not recursed))
    ;; List of specs
    (mapcar (lambda (spec) (car (hkhlp-normalize-hook-spec spec t))) hook-spec))
   (t
    (warn "Unrecognized hook-spec %s" hook-spec))))

(defun add-hook-helper (id hook-spec)
  "Adds an existing helper ID to HOOK-SPEC."
  (let ((normalized-spec (hkhlp-normalize-hook-spec hook-spec))
        (helper (alist-get id hkhlp--helpers-map)))
    (pcase-dolist (`(,hook . ,append) normalized-spec)
      (add-hook hook (hook-helper-function helper) append)
      (cl-pushnew hook (hook-helper-hooks helper) :test 'equal))))

(defun remove-hook-helper (id hook-spec)
  "Removes the helper ID from each element of HOOK-SPEC."
  (let ((normalized-spec (hkhlp-normalize-hook-spec hook-spec))
        (helper (alist-get id hkhlp--helpers-map)))
    (pcase-dolist (`(,hook . _) normalized-spec)
      (remove-hook hook (hook-helper-function helper))
      (cl-delete hook (hook-helper-hooks helper) :test 'equal))))

(cl-defmethod hkhlp-update-helper ((old hook-helper) (new hook-helper))
  "Updates instances of OLD to NEW.

For each hook HOOK in the original:

  - If HOOK is not in NEW, remove OLD from it
  - Else, update OLD to NEW
"
  (let* ((old-func (hook-helper-function old))
         (new-func (hook-helper-function new))
         (old-hooks (hook-helper-hooks old))
         (new-hooks (hook-helper-hooks new)))
    (dolist (hook old-hooks)
      (let ((hook-val (and (boundp hook) (symbol-value hook))))
        (cond
         ((not hook-val) nil)
         ((member hook new-hooks)
          ;; Update the helper in hooks
          (when-let* ((elt (cl-position old-func hook-val :test 'equal)))
            (setf (nth elt hook-val) new-func)))
         (t
          ;; Delete the helper from the hooks
          (cl-delete old-func (symbol-value hook) :test 'equal)))))))

;;;###autoload
(defmacro create-hook-helper (id args &optional docstring &rest body)
  "Creates a new hook helper ID for the hooks in HOOKS.

If a hook helper with id ID already exists, it's overridden.  All instances of
the helper in its associated hooks are replaced.

See `hkhlp-normalize-hook-spec' for an explanation of HOOKS.

\(fn ID ARGS &optional DOCSTRING &keys HOOKS &rest BODY)"
  (declare (indent defun) (doc-string 3))
  (when (and docstring (not (stringp docstring)))
    ;; Some trickiness, since what appears to be the docstring may really be
    ;; the first element of the body.
    (push docstring body)
    (setq docstring nil))
  ;; Process the key words
  (let ((hook-spec nil))
    (while (keywordp (car body))
      (pcase (pop body)
	(`:hooks (setq hook-spec (pop body)))
	(_ (pop body))))
    `(let* ((id-sym (quote ,id))
            (func (lambda ,args ,docstring ,@body))
            (normalized-hooks (hkhlp-normalize-hook-spec (quote ,hook-spec)))
            (source-file ,(or load-file-name buffer-file-name))
            (helper (make-hook-helper :id id-sym
                                      :function func
                                      :source-file source-file
                                      :hooks (mapcar 'car normalized-hooks))))
       ;; Update an old helper
       (when-let* ((old-helper (alist-get id-sym hkhlp--helpers-map)))
         (hkhlp-update-helper old-helper helper))
       (setf (alist-get id-sym hkhlp--helpers-map) helper)
       ;; Add to the new hook-spec
       (add-hook-helper id-sym (quote ,hook-spec)))))

;;;###autoload
(defmacro define-hook-helper (hook args &optional docstring &rest body)
  "Define a hook helper for the variable HOOK-hook with ARGS as the argument list.

This helper consists of all the code in BODY.  HOOK should not be
quoted.  The keywords are:

:name    Specifies a name to use for the generated function.  As part
         of this macro, a function called hook-helper--HOOK will be
         created.  If NAME is given, then the function becomes
         ‘hook-helper--HOOK/NAME’.

:append  If non-nil, append the hook helper to the hook variable.

:suffix  Allows a user to specify that the hook variable doesn't
         end with ‘-hook’, but instead with another suffix, such as
         ‘-function’.  SUFFIX should be a string, and defaults to ‘hook’
         if not specified.  Note that SUFFIX is not assumed to start with
         a hyphen."
  (declare (indent defun) (doc-string 3))
  ;; From `define-derived-mode'
  (when (and docstring (not (stringp docstring)))
    ;; Some trickiness, since what appears to be the docstring may really be
    ;; the first element of the body.
    (push docstring body)
    (setq docstring nil))
  ;; Process the key words
  (let ((name nil)
        (append nil)
        (suffix "hook"))
    (while (keywordp (car body))
      (pcase (pop body)
	(`:name (setq name (pop body)))
	(`:append (setq append (pop body)))
	(`:suffix (setq suffix (pop body)))
	(_ (pop body))))
    (let* ((suffix-string (if (stringp suffix) suffix (symbol-name suffix)))
           (hook-name (concat (symbol-name hook) "-" suffix-string))
           (func-sym (intern (format "%s%s" hook-name
                                     (if name (concat "/" (symbol-name name)) ""))))
           (hook (intern hook-name)))
      `(create-hook-helper ,func-sym ,args
         ,docstring
         :hooks (,hook . ,append)
         ,@body))))

;;;###autoload
(defmacro define-hook-function (function args &optional docstring &rest body)
  "Define FUNCTION to be a function, then add it to hooks.

The hooks to add are specified by the :hooks keyword.  This is a
simple list of hooks, unquoted, and the new function is added to
each one."
  (declare (indent defun)
           (doc-string 3)
           (obsolete create-hook-helper "1.1"))
  `(create-hook-helper ,function ,args ,docstring ,@body))

;; TODO Link to source file
(cl-defmethod hkhlp--pp ((helper hook-helper) indent)
  (let* ((func (hook-helper-function helper))
         (pp-string (pp-to-string func))
         (id (hook-helper-id helper))
         (id-name (symbol-name id))
         (indent (max (+ (length id-name) 1) indent))
         (indent-first (- indent (length id-name)))
         (pp-lines (split-string pp-string "\n" t)))
    (concat (symbol-name id) (make-string indent-first ?\ ) (car pp-lines) "\n"
            (mapconcat
             (lambda (str)
               (concat (make-string indent ?\ )
                       str))
             (cdr pp-lines)
             "\n")
            "\n\n")))

(defun describe-hook-helpers ()
  "Describe the currently defined hook helpers."
  (interactive)
  (let ((hook-alist nil))
    (pcase-dolist (`(_ . ,helper) hkhlp--helpers-map)
      (dolist (hook (hook-helper-hooks helper))
        (push helper (alist-get hook hook-alist))))
    (with-output-to-temp-buffer "*Hook Helpers*"
      (pcase-dolist (`(,hook . ,helpers) hook-alist)
        (princ (format "%s\n%s\n" hook (make-string 40 ?-)))
        (dolist (helper helpers)
          (princ (hkhlp--pp helper 16)))
        (princ "\n")))))

;; Add font lock for both macros.
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(define-hook-helper\\)\\_>[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face nil t))
   ("(\\(create-hook-helper\\)\\_>[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face nil t))))

(provide 'hook-helpers)

;;; hook-helpers.el ends here
