;;; tc13.el --- 

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

(defun psgml-tc13 ()
  (set-buffer (get-buffer-create "*Tc13*"))
  (erase-buffer)
  (xml-mode)
  (insert "<foo/")
  (ignore-errors
    (let ((el (sgml-parse-to-here)))
      (message "el etag-epos=%s etag-len=%s"
               (sgml-tree-etag-epos el)
               (sgml-tree-etag-len el))))
  (insert ">")
  (let ((el (sgml-parse-to-here)))
    (assert (sgml-off-top-p el))))

(psgml-tc13)
