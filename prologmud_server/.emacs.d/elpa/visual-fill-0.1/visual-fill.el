;;; visual-fill.el --- Auto-refill paragraphs without modifying the buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.1
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

;; This `visual-fill-mode' minor mode basically "unfills" paragraphs within
;; jit-lock, hence without modifying the buffer.  Combined with the normal
;; line-wrapping this performs a kind of "auto refill" which can be more or
;; less sophisticated depending on the line-wrapping used.
;;
;; For best effect, combine it with `visual-line-mode' and
;; `adaptive-wrap-prefix-mode'.

;;; Code:

(defconst visual-fill--space " ")

(defun visual-fill--cleanup (start end)
  (while (and (< start end)
              (setq start (text-property-any start end
                                             'display visual-fill--space)))
    (remove-text-properties
     start
     (setq start (or (text-property-not-all start end
                                            'display visual-fill--space)
                     end))
     '(display nil))))

(defun visual-fill--jit (start end)
  (visual-fill--cleanup start end)
  (goto-char start)
  (forward-line 0)
  (let ((after-sep (looking-at paragraph-separate)))
    (while (< (point) end)
      (forward-line 1)
      (if after-sep
          (setq after-sep (looking-at paragraph-separate))
        (unless (or (setq after-sep (looking-at paragraph-separate))
                    (looking-at paragraph-start))
          (put-text-property (1- (point))
                             (if (looking-at (if adaptive-fill-mode
                                                 adaptive-fill-regexp "[ \t]+"))
                                 (match-end 0)
                               (point))
                             'display visual-fill--space))))))

;;;###autoload
(define-minor-mode visual-fill-mode
  "Auto-refill paragraphs without modifying the buffer."
  :lighter " VFill"
  :global nil
  (jit-lock-unregister #'visual-fill--jit)
  (with-silent-modifications
    (visual-fill--cleanup (point-min) (point-max)))
  (when visual-fill-mode
    (jit-lock-register #'visual-fill--jit)))

;;;; ChangeLog:

;; 2018-10-16  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* visual-fill.el: Fix copyright
;; 
;; 2018-10-16  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* visual-fill.el: Add missing version
;; 
;; 2018-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* visual-fill.el: New packagepackages/visual-fill/visual-fill.el
;; 


(provide 'visual-fill)
;;; visual-fill.el ends here
