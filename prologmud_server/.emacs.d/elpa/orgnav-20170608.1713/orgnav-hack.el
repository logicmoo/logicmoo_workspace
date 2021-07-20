;;; orgnav-hack.el --- Work around in orgnav that we might remove some day -*- lexical-binding: t -*-

;; Copyright (C) 2016 Facet Framer

;; Author: Facet Framer (facet@facetframer.com)
;; URL: github.com/facetframer/orgnav

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

(define-error 'orgnav-last-error "No more children")

;;; Code:
(defun orgnav-hack-outline-forward-same-level (arg)
  ;;; from outline.el ( fix a bug where this fails
  ;;; if heading is hidden)
  "Got forward ARG headings (pay attention to invisible headings)."
  (interactive "p")
  (outline-back-to-heading 't)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
                              (orgnav-hack--outline-get-next-sibling))))
      (if point-to-move-to
          (progn
            (goto-char point-to-move-to)
            (setq arg (1- arg)))
        (progn
          (setq arg 0)
          (signal 'orgnav-last-error "No following same-level heading"))))))

(defun orgnav-hack--outline-get-next-sibling ()
  ;;; Stolen from outline to work around bug
  "Move to next heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((level (funcall outline-level)))
    (outline-next-heading)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-heading))
    (if (or (eobp) (< (funcall outline-level) level))
        nil
      (point))))

(provide 'orgnav-hack)

;;; orgnav-hack.el ends here
