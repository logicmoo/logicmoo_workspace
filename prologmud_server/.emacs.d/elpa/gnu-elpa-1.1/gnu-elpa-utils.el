;;; gnu-elpa-utils.el --- Helper functions for `gnu-elpa'  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'gnu-elpa-features)

;;;###autoload (eval-after-load 'package
;;;###autoload   '(unless (assoc "gnu" package-archives)
;;;###autoload      (push '("gnu" . "https://elpa.gnu.org/packages/")
;;;###autoload            package-archives)))

;;;###autoload ;; Skip load-source-file-function which would slow us down by
;;;###autoload ;; a factor 2 (this assumes we were careful to save this file
;;;###autoload ;; so it doesn't need any decoding).
;;;###autoload (let ((load-source-file-function nil))
;;;###autoload   (require 'gnu-elpa-features nil 'noerror))

(defun gnu-elpa--autoloaded-function ()
  (let* ((bt (backtrace-frames))
         ;; (bt-stash bt)
         (trigger-function nil))
    (while bt
      (pcase-let ((`(\_ ,f . ,_) (pop bt)))
        (when (and (symbolp f) (autoloadp (indirect-function f)))
          (setq trigger-function f)
          (setq bt nil))))
    (unless trigger-function
      (error "Can't find the autoload call!"))
    trigger-function))

(defun gnu-elpa--package (func)
  "Return the package that provides function FUNC."
  (let ((thepkg nil))
    (pcase-dolist (`(,prefix . ,pkg) gnu-elpa--autoloads-table)
      (if (string-prefix-p prefix (symbol-name func)) (setq thepkg pkg)))
    thepkg))

(defun gnu-elpa--perform-autoload ()
  "Prompt to install the package that provides the currently autoloaded function.
The relevant function is found by walking the stack until we find a function.
Presumes we're in the process of calling an autoloaded function that's not
yet loaded."
  (let* ((f (gnu-elpa--autoloaded-function))
         (pkg (gnu-elpa--package f)))
    (unless (yes-or-no-p (format "Install package %s? " pkg))
      (error "Abort!"))
    ;; FIXME: These two initializations should be performed by
    ;; `package-install'!
    (unless (bound-and-true-p package--initialized) (package-initialize t))
    (unless package-archive-contents (package-refresh-contents))
    (package-install (intern pkg))))

(provide 'gnu-elpa-utils)
;;; gnu-elpa-utils.el ends here
