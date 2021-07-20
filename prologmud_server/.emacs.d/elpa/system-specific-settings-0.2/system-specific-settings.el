;;; system-specific-settings.el --- Apply settings only on certain systems
;; 
;; Filename: system-specific-settings.el
;; Author: Ryan C. Thompson
;; Created: Sun Aug 17 11:34:34 2014 (-0700)
;; Version: 0.2
;; Package-Version: 0.2
;; Package-Commit: 0050d85b2175095aa5ecf580a2fe43c069b0eef3
;; Package-Requires: ()
;; URL: https://github.com/DarwinAwardWinner/emacs-system-specific-settings
;; Keywords: configuration
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; This package defines some macros that you can use in your Emacs
;; configuration to evaluate certain settings only on specific
;; operating systems or specific hosts.
;; 
;; All the relevant macros are autoloaded, so if you install this
;; through package.el, you can just use them in your initialization
;; file.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defun system-specific-settings-do-match (value spec)
  "Match VALUE against SPEC.

Returns a boolean indicating whether VALUE matches SPEC. Matching is done as follows:

* If SPEC is a function, it is called with VALUE as its only
  argument and the result is returned.

* If SPEC is a cons cell with a car of `regexp' and a cdr that is
  a string, the string will be matched against VALUE using
  `string-match-p'. VALUE will be converted from a symbol to a
  string if necessary.

* If SPEC is a conse cell with a car of `not', then VALUE will be
  matched against the cdr and the result will be inverted.

* If SPEC is a list, VALUE is matched recursively against each
  element of SPEC, and this returns non-nil if any element of
  SPEC results in a match.

* If SPEC and VALUE are both either symbols or strings, they are
  compared using `string=' (which compares symbols using their
  print names).

* If SPEC is anything else, it is compared to VALUE using `equal'.

For example, if VALUE is a symbol and SPEC is a list of symbols,
this will return TRUE if VALUE occurs in SPEC.

Note that since matching is done using several different
functions, a match may not always return `t', but it will return
some non-nil value."
  (cond
   ((functionp spec)
    (funcall spec value))
   ((and (consp spec)
         (eq (car spec) 'not))
    (not (system-specific-settings-do-match value (cdr spec))))
   ((and (consp spec)
         (eq (car spec) 'regexp)
         (stringp (cdr spec)))
    (when (symbolp value)
      (setq value (symbol-name value)))
    (string-match-p (cdr spec) value))
   ((ignore-errors (string= value spec)))
   ((listp spec)
    (cl-some (apply-partially #'system-specific-settings-do-match
                              value)
             spec))
   (t
    (equal value spec))))

;;;###autoload
(defmacro if-system-type-match (cond then &rest else)
  "If COND matches `system-type', do THEN, else do ELSE.

Matching is done using `system-specific-settings-do-match'."
  (declare (indent 2))
  `(if (system-specific-settings-do-match system-type ,cond)
       ,then
     ,@else))

;;;###autoload
(defmacro if-system-name-match (cond then &rest else)
  "If COND matches `system-name', do THEN, else do ELSE.

Matching is done using `system-specific-settings-do-match'."
  (declare (indent 2))
  `(if (system-specific-settings-do-match system-name ,cond)
       ,then
     ,@else))

;;;###autoload
(defmacro when-system-type-match (cond &rest body)
  "Eval BODY only if `system-type' matches COND.

Matching is done using `system-specific-settings-do-match'."
  (declare (indent 1))
  `(if-system-type-match ,cond
       (progn ,@body)))

;;;###autoload
(defmacro when-system-name-match (cond &rest body)
  "Eval BODY only if `system-name' matches COND.

Matching is done using `system-specific-settings-do-match'."
  (declare (indent 1))
  `(if-system-name-match ,cond
       (progn ,@body)))

(define-obsolete-function-alias 'eval-on-system-type 'when-system-type-match
  "0.2")
(define-obsolete-function-alias 'eval-on-host 'when-system-name-match
  "0.2")

(provide 'system-specific-settings)

;;; system-specific-settings.el ends here
