;;; reform.el --- ??

;; Copyright (C)  2017 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'psgml-api)

(defun sgml-reformat ()
  (interactive)
  (goto-char (sgml-element-start (sgml-top-element)))
  (unless (bolp)
    (insert "\n"))
  (sgml-map-element-modify
   #'sgml--reformat-element
   (sgml-top-element)))

(defun sgml--reformat-element (el)
  (unless (sgml-element-data-p (sgml-element-parent el))
    (goto-char (sgml-element-end el))
    (unless (eolp)
      (insert "\n")
      (sgml-indent-line)))
  (unless (sgml-element-data-p el)
    (goto-char (sgml-element-stag-end el))
    (unless (eolp)
      (insert "\n")
      (sgml-indent-line)))
  (goto-char (sgml-element-stag-end el))
  (backward-char 1)
  (if (bolp) (delete-char -1)))

