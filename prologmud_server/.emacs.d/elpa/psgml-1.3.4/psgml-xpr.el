;;; psgml-xpr.el --- Experimental additions for PSGML  -*- lexical-binding:t -*-
;; $Id: psgml-xpr.el,v 2.3 2005/02/27 17:15:19 lenst Exp $

;; Copyright (C) 2003, 2016 Free Software Foundation, Inc.

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Keywords: languages

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


;;; Commentary:
;;
;; 


;;; Code:



;;;; Simplistic JSP Support

(require 'psgml-parse)

(eval-when-compile
  (unless (member "JSP-STAGO" sgml-delimiters)
    (setq sgml-delimiters
          `("JSP-STAGO" "<%"
            "JSP-TAGC"  "%>"
            . ,sgml-delimiters))))

(defun psgml-parse-jps-tag ()
  (when (sgml-parse-delim "JSP-STAGO")
    (search-forward (sgml-delim "JSP-TAGC") nil 'noerror)
    (sgml-set-markup-type 'comment)
    t))

(add-to-list 'sgml-parser-loop-hook 'psgml-parse-jps-tag)
(modify-syntax-entry ?: "_" sgml-parser-syntax)


;;;; Adding appdata to element types

;; * move to psgml-edit - document

(defun sgml-eval-psgml-pi ()
  (interactive)
  (let ((sgml-psgml-pi-enable-outside-dtd t))
    (sgml-parse-to-here)))

(define-key sgml-mode-map "\e\C-x" 'sgml-eval-psgml-pi)



(defun sgml--empty-is-nil (s)
  (if (equal s "")
      nil
    s))

(defun sgml-dl-to-table (border table-width first-col-width)
  (interactive "sBoder: \nsTab Width: \nsFist Col Width: \n")
  (setq border (sgml--empty-is-nil border))
  (setq table-width (sgml--empty-is-nil table-width))
  (setq first-col-width (sgml--empty-is-nil first-col-width))
  (let ((el (sgml-find-element-of (point))))
    (goto-char (sgml-element-etag-start el))
    (let ((end (point-marker)))
      (goto-char (sgml-element-start el))
      (sgml-change-element-name "TABLE")
      (sgml-insert-attribute "BORDER" border)
      (sgml-insert-attribute "WIDTH" table-width)
      (while (search-forward "<" end t)
        (cond
         ((looking-at "dt")
          (backward-char 1)
          (insert "<tr>")
          (sgml-change-element-name "TD")
          (sgml-insert-attribute "WIDTH" first-col-width))
         ((looking-at "tr>\\s-*<td")
          (sgml-down-element)
          (sgml-insert-attribute "WIDTH" first-col-width))
         ((looking-at "dd")
          (sgml-change-element-name "TD")
          (sgml-up-element)
          (insert "</tr>")))))))


(provide 'psgml-xpr)
;;; psgml-xpr.el ends here
