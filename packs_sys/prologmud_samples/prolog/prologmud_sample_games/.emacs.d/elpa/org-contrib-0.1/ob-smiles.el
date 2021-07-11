;;; ob-smiles.el --- Org-mode Babel support for SMILES.
;;; -*- coding: utf-8 -*-

;; Keywords: org babel SMILES
;; Version: 0.0.1
;; Package-Requires: ((smiles-mode "0.0.1") (org "8"))

;; This file is not part of GNU Emacs.
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; I copy code from:
;;; https://kitchingroup.cheme.cmu.edu/blog/2016/03/26/A-molecule-link-for-org-mode

;; Author: John Kitchin [jkitchin@andrew.cmu.edu]
;; Maintainer: stardiviner [numbchild@gmail.com]

;;; Code:

(require 'ob)
(require 'org-element)

;; Org-mode Babel
(defun org-babel-execute:smiles (body params)
  "Execute SMILES babel `BODY' with `PARAMS'."
  (shell-command-to-string
   (format "obabel -:\"%s\" -osvg 2> /dev/null" body)))

;; Org-mode link
(defun molecule-jump (name)
  "Jump to molecule `NAME' definition."
  (org-mark-ring-push)
  (org-link-open-from-string (format "[[%s]]" name)))

(defun molecule-export (path desc backend)
  "Export molecule to HTML format on `PATH' with `DESC' and `BACKEND'."
  (let ((name (save-window-excursion
                (molecule-jump path)
                (org-element-property :name (org-element-context)))))
    (cond
     ((eq 'html backend)
      (format "<a href=\"#%s\">%s</a>" name name)))))

(org-link-set-parameters
 "molecule"
 :follow 'molecule-jump
 :export 'molecule-export)

;; org-mode element
(org-element-map (org-element-parse-buffer)
    'src-block
  (lambda (src)
    (when (string= "smiles" (org-element-property :language src))
      (org-element-property :name src))))


(provide 'ob-smiles)

;;; ob-smiles.el ends here
