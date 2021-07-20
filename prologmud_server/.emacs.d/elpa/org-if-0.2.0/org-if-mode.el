;;; org-if-mode.el --- Major mode for org-if -*- lexical-binding: t -*-

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

;; This file defines the major mode org-if-mode for the purposes of
;; syntax highlighting. You should never create a .org-if file.
;; This is only to make org-babel src blocks highlight org-if code correctly.

;;; Code:

(require 'easymenu)
(require 'generic-x)
(require 'org)

;;;###autoload
(define-generic-mode
  'org-if-mode
  '(";")
  '("choice" "if" "print" "reset" "set" "=" "+" "-" "*" "/"
    "<" "<=" "=" ">=" ">" "!=")
  nil
  '("\\.org-if$") ; For testing purposes only
  nil
  "Major mode for ORG-IF programming language.")

(easy-menu-define org-if-menu org-mode-map "Org IF menu"
  '("Org-IF"
    ["Active"      toggle-org-if-active-mode :style toggle :selected org-if-active-mode]
    ["Save & Quit" org-if-save-and-quit      :active org-if-active-mode]
    ["Restore"     org-if-restore            :active (not org-if-active-mode)]))

(provide 'org-if-mode)
;;; org-if-mode.el ends here
