;;; tc15.el --- 

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

(defun psgml-tc15 ()
  ;; Check that completing honors insert-case
  (set-buffer (get-buffer-create "*Tc15*"))
  (erase-buffer)
  (sgml-mode)
  (setq sgml-general-insert-case 'lower)
  (insert "<!DOCTYPE foo [<!ELEMENT foo (#PCDATA)>]>\n<f")
  (sgml-complete)
  (beginning-of-line)
  (let ((case-fold-search nil))
    (assert (looking-at "<foo"))))

(psgml-tc15)
