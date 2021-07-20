;;; org-if-misc.el --- Miscellaneous functions for org-if-mode -*- lexical-binding: t -*-

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

;;; This file contains miscellaneous functions for org-if-mode.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)

(defvar *org-if-current-file*
  nil
  "This is the current file when `org-if-active-mode' is enabled.")

(defvar *org-if-current-env*
  (make-hash-table)
  "Reference to org-if environment after code in current buffer has executed.")
; Add boolean values to empty environment
(puthash 'true  'true  *org-if-current-env*)
(puthash 'false 'false *org-if-current-env*)

(defvar *org-if-old-env*
  nil
  "Reference to org-if environment as it entered page.
When saving is enabled, this environment will be the one saved.
If the user saves on a page that modifies the environment,
then when the user restores that same code will run twice.
Therefore, we save a copy of the environment as it was when
the user loaded the current page.")

(defgroup org-if
  nil
  "Interactive Fiction Authoring System for Org-Mode."
  :group 'applications)

(defcustom org-if-save-dir "~/.org-if/"
  "Directory where org-if saves data for sessions."
  :group 'org-if
  :type '(directory))

(defun org-if-set-env (list)
  "Set the new state of `*org-if-current-env*' with values from LIST.
LIST should be an even length list of the form (variable1 value1 ...)."
  (cl-labels ((helper (vars)
                    (when (not (null vars))
                      (let ((key (nth 0 vars))
                            (val (nth 1 vars)))
                        (if (and (not (eq key 'true))
                                 (not (eq key 'false)))
                            (puthash key
                                     (org-if-eval val)
                                     *org-if-current-env*)
                            (error "You cannot reassign false and true!")))
                      (helper (nthcdr 2 vars)))))
    (if (cl-evenp (length list))
        (helper list)
        (error "Invalid parameters passed to `org-if-set-env': %s" list))))

(defun org-if-reset-env ()
  "Clear `*org-if-current-env*' and set `*org-if-current-file*' to nil."
  (setf    *org-if-current-file* nil)
  (clrhash *org-if-current-env*)
  (puthash 'false 'false *org-if-current-env*)
  (puthash 'true  'true  *org-if-current-env*))

(defun org-if-goto-first-heading ()
  "Go to the line containing the first major heading in the current buffer.
Major heading start with \"*\"."
  (goto-char (point-min))
  (while (not (equal "* "
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (+ 2 (line-beginning-position)))))
    (forward-line 1)))

(provide 'org-if-misc)
;;; org-if-misc.el ends here
