;;; quarter-plane.el --- Minor mode for quarter-plane style editing

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Peter J. Weisberg
;; Version: 0.1
;; Keywords: convenience wp

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

;; This package provides Quarter Plane mode, a minor mode which
;; provides Picture mode style editing (treating the screen as a
;; semi-infinite quarter-plane).  Unlike Picture mode, it is a minor
;; modes (see the Emacs manual for the documentation of Picture mode).
;; Type M-x quarter-plane-mode to enable Quarter Plane mode in the
;; current buffer, and M-x global-quarter-plane-mode to enable it
;; globally.

;; In Quarter Plane mode, the commands `right-char', `forward-char',
;; `previous-line', `next-line', and `mouse-set-point' are rebound to
;; Quarter Plane commands.

;; Known issues:

;; Quarter-Plane mode doesn't work in read-only buffers, where it
;; can't insert spaces.

;; The user doesn't really care about the "modifications" of adding
;; whitespace that's going to be trimmed when he exits quarter-plane
;; mode or saves, but it's still part of the undo history.

;; Both of these are due to the disconnect between what the user
;; really wants--movement of the cursor within the window, regardless
;; of where the text is--and what the mode can actually do--add dummy
;; text to give the cursor a place to move to.

;;; Code:

(require 'picture)

(defvar quarter-plane-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap right-char] 'picture-forward-column)
    (define-key map [remap forward-char] 'picture-forward-column)
    (define-key map [remap previous-line] 'picture-move-up)
    (define-key map [remap next-line] 'picture-move-down)
    (define-key map [remap mouse-set-point] 'picture-mouse-set-point)
    map))

(defvar quarter-plane-saved-values nil)
(make-variable-buffer-local 'quarter-plane-saved-values)

;;;###autoload
(define-minor-mode quarter-plane-mode
  "Toggle Quarter-Plane mode on or off.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.

Use point movement commands that act as if the text extended
infinitely down and to the right, inserting spaces as necessary.
Excess whitespace is trimmed when saving or exiting Quarter-Plane mode.

Because it works by inserting spaces, Quarter-Plane mode won't work in
read-only buffers.

\\{quarter-plane-mode-map}"
  :lighter " Plane"
  :group 'picture
  :keymap quarter-plane-mode-map
  (remove-hook 'before-save-hook 'quarter-plane-delete-whitespace t)
  (dolist (symval (prog1 quarter-plane-saved-values
                    (setq quarter-plane-saved-values nil)))
    (set (car symval) (cdr symval)))
  (when quarter-plane-mode
    (add-hook 'before-save-hook 'quarter-plane-delete-whitespace nil t)
    ;; Since quarter-plane-mode is not permanent-local, it should turn itself
    ;; off cleanly.
    (add-hook 'change-major-mode-hook (lambda () (quarter-plane-mode -1)) nil t)
    (dolist (symval '((truncate-lines . t)
                      (show-trailing-whitespace . nil)))
      (push (cons (car symval) (symbol-value (car symval)))
            quarter-plane-saved-values)
      (set (car symval) (cdr symval)))))

;;;###autoload
(define-global-minor-mode global-quarter-plane-mode quarter-plane-mode
  quarter-plane-mode
  :group 'picture)

(defun quarter-plane-delete-whitespace ()
  "Call `delete-trailing-whitespace' if the buffer is not read-only."
  (if (not buffer-read-only)
      (delete-trailing-whitespace)))

(add-hook 'quarter-plane-mode-off-hook 'quarter-plane-delete-whitespace)

;;;; ChangeLog:

;; 2012-03-24  Chong Yidong  <cyd@gnu.org>
;; 
;; 	Commentary fix for quarter-plane.el.
;; 
;; 2011-09-23  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/quarter-plane/quarter-plane.el
;; 	(quarter-plane-saved-symbols): Remove.
;; 	(quarter-plane-saved-values): Give a default value.
;; 	Don't make permanent-local.
;; 	(quarter-plane-mode): Cleanup before re-enabling.
;; 	Set change-major-mode-hook.
;; 	(global-quarter-plane-mode): Use quarter-plane-mode rather than the
;; 	deleted turn-on-quarter-plane-mode.
;; 
;; 2011-09-23  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	New package quarter-plane.
;; 


(provide 'quarter-plane)

;;; quarter-plane.el ends here
