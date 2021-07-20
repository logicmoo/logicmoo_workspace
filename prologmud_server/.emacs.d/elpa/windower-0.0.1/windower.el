;;; windower.el --- Helper functions for window manipulation. -*- lexical-binding: t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.
;;
;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://gitlab.com/ambrevar/windower
;; Version: 0.0.1
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package provides the commands below.  The commands are most useful when
;; bound to accessible bindings, for instance:
;;
;;   (global-set-key (kbd "<s-tab>") 'windower-switch-to-last-buffer)
;;   (global-set-key (kbd "<s-o>") 'windower-toggle-single)
;;   (global-set-key (kbd "s-\\") 'windower-toggle-split)
;;
;;   (global-set-key (kbd "<s-M-left>") 'windower-move-border-left)
;;   (global-set-key (kbd "<s-M-down>") 'windower-move-border-below)
;;   (global-set-key (kbd "<s-M-up>") 'windower-move-border-above)
;;   (global-set-key (kbd "<s-M-right>") 'windower-move-border-right)
;;
;;   (global-set-key (kbd "<s-S-left>") 'windower-swap-left)
;;   (global-set-key (kbd "<s-S-down>") 'windower-swap-below)
;;   (global-set-key (kbd "<s-S-up>") 'windower-swap-above)
;;   (global-set-key (kbd "<s-S-right>") 'windower-swap-right)
;;
;; For Evil users:
;;   (global-set-key (kbd "s-M-h") 'windower-move-border-left)
;;   (global-set-key (kbd "s-M-j") 'windower-move-border-below)
;;   (global-set-key (kbd "s-M-k") 'windower-move-border-above)
;;   (global-set-key (kbd "s-M-l") 'windower-move-border-right)
;;
;;   (global-set-key (kbd "s-H") 'windower-swap-left)
;;   (global-set-key (kbd "s-J") 'windower-swap-below)
;;   (global-set-key (kbd "s-K") 'windower-swap-above)
;;   (global-set-key (kbd "s-L") 'windower-swap-right)

;;; Code:

(defvar windower-border-move-distance 5
  "Default distance for `windower-move-border'.")

;;;###autoload
(defun windower-move-border (&optional distance direction)
  "Move current window's border towards DIRECTION.
DIRECTION is one of 'left, 'right, 'above or 'below."
  (interactive)
  (setq distance (or distance windower-border-move-distance))
  (let ((f (if (window-in-direction direction)
               #'enlarge-window
             #'shrink-window)))
    (funcall f distance (pcase direction
                          ((or 'left 'right) 'horizontal)
                          (_ nil)))))

;;;###autoload
(defun windower-move-border-left (distance)
  "Move window border to the left."
  (interactive "P")
  (windower-move-border distance 'left))

;;;###autoload
(defun windower-move-border-right (distance)
  "Move window border to the right."
  (interactive "P")
  (windower-move-border distance 'right))

;;;###autoload
(defun windower-move-border-above (distance)
  "Move window border upward."
  (interactive "P")
  (windower-move-border distance 'above))

;;;###autoload
(defun windower-move-border-below (distance)
  "Move window border downward."
  (interactive "P")
  (windower-move-border distance 'below))

;;;###autoload
(defun windower-swap (&optional w1 w2)
  "If 2 windows are up, swap them.
Else if W1 is a window, swap it with current window.
If W2 is a window too, swap both."
  (interactive)
  (unless (or (= 2 (count-windows))
              (windowp w1)
              (windowp w2))
    (error "Ambiguous window selection"))
  (let* ((w1 (or w1 (car (window-list))))
         (w2 (or w2
                 (if (eq w1 (car (window-list)))
                     (nth 1 (window-list))
                   (car (window-list)))))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (with-temp-buffer
      ;; Some buffers like EXWM buffers can only be in one live buffer at once.
      ;; Switch to a dummy buffer in w2 so that we don't display any buffer twice.
      (set-window-buffer w2 (current-buffer))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1))
    (set-window-start w1 s2)
    (set-window-start w2 s1))
  (select-window w1))

;;;###autoload
(defun windower-swap-left ()
  "Swap current window with the window to the left."
  (interactive)
  (windower-swap (window-in-direction 'left)))

;;;###autoload
(defun windower-swap-below ()
  "Swap current window with the window below."
  (interactive)
  (windower-swap (window-in-direction 'below)))

;;;###autoload
(defun windower-swap-above ()
  "Swap current window with the window above."
  (interactive)
  (windower-swap (window-in-direction 'above)))

;;;###autoload
(defun windower-swap-right ()
  "Swap current window with the window to the right."
  (interactive)
  (windower-swap (window-in-direction 'right)))

;;;###autoload
(defun windower-switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; TODO: Store window configurations in a buffer-name-indexed alist? Not
;;; sure that would ever be useful.
(defvar windower--last-configuration nil
  "Last window configuration before calling `delete-other-windows'.")

;;;###autoload
(defun windower-toggle-single ()
  "Un-maximize current window.
If multiple windows are active, save window configuration and
delete other windows.  If only one window is active and a window
configuration was previously save, restore that configuration."
  (interactive)
  (if (= (count-windows) 1)
      (when windower--last-configuration
        (set-window-configuration windower--last-configuration))
    (setq windower--last-configuration (current-window-configuration))
    (delete-other-windows)))

;;;###autoload
(defun windower-toggle-split ()
  "Switch between vertical and horizontal split.
It only works for frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  #'split-window-horizontally
                #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;;; ChangeLog:



(provide 'windower)
;;; windower.el ends here
