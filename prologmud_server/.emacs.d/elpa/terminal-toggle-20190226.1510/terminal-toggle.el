;;; terminal-toggle.el --- simple pop-up terminal -*- lexical-binding: t; -*-

;; Copright (C) 2019 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/terminal-toggle.el
;; Package-Version: 20190226.1510
;; Package-Commit: f824d634aef3600cb7a8e2ddf9e8444c6607c160
;; Keywords: outlines
;; Package-Requires: ((emacs "24") (popwin "1.0.0"))
;; Version: 0.1

;;; Commentary:

;; A simple terminal that pops-up (bottom/top/left/right) and then removes
;; itself when it loses focus without killing the buffer.  Essentially it is
;; a simple hide/show for a terminal.

;;; Code:

(require 'popwin)

(defgroup terminal-toggle nil
  "Group for setting terminal options"
  :prefix "terminal-toggle-"
  :group 'emacs)

(defcustom terminal-toggle--term-title "myterm"
  "Name of buffer to hide/show."
  :type 'string
  :group 'terminal-toggle)

(defcustom terminal-toggle--term-command "ansi-term"
  "Terminal command to launch."
  :type 'string
  :group 'terminal-toggle)

(defcustom terminal-toggle--term-shell "/usr/bin/zsh"
  "Terminal shell to launch."
  :type 'string
  :group 'terminal-toggle)

(defconst terminal-toggle--term-name
  (if (and (string= (substring terminal-toggle--term-title 0 1) "*")
               (string= (substring terminal-toggle--term-title -1) "*"))
      terminal-toggle--term-title
    (concat "*" terminal-toggle--term-title "*"))
  "Internal buffer name with asterisks.")

(defun terminal-toggle-is-open ()
  "Terminal exists."
  (get-buffer terminal-toggle--term-name))

(defun terminal-toggle-is-showing ()
  "Terminal state (whether is visible)."
  (get-buffer-window terminal-toggle--term-name))

(defun terminal-toggle-launch ()
  "Launch ansi terminal."
  (funcall (intern terminal-toggle--term-command) terminal-toggle--term-shell
           (substring terminal-toggle--term-name 1 -1))
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun terminal-toggle-set-visible ()
  "Show an already opened terminal."
  (popwin:popup-buffer terminal-toggle--term-name))

(defun terminal-toggle-set-hidden ()
  "Hide the terminal."
  (delete-window (get-buffer-window terminal-toggle--term-name)))

;;;###autoload
(defun terminal-toggle ()
  "Show/launch or hide terminal."
  (interactive)
  (if (terminal-toggle-is-open)
      (if (terminal-toggle-is-showing)
          (terminal-toggle-set-hidden)
        (terminal-toggle-set-visible))
    (progn (terminal-toggle-launch)
           (terminal-toggle-set-visible))))

(provide 'terminal-toggle)

;;; terminal-toggle.el ends here

