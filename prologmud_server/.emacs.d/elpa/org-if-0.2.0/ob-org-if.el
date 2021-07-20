;;; ob-template.el --- org-babel functions for template evaluation -*- lexical-binding: t -*-

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file makes org-babel aware of the org-if language.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-eval)
;; possibly require modes required for your language
(require 'org-if-interpreter)
(require 'org-if-mode)

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:org-if
  '((:results . "no"))
  "Default header arguments for org-if SRC blocks.")

;; This is the main function which is called to evaluate a code
;; block.
;;;###autoload
(defun org-babel-execute:org-if (body params)
  "Execute a block of ORG-IF code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing ORG-IF source code block")
  (org-if-interpret body))

(provide 'ob-org-if)
;;; ob-org-if.el ends here
