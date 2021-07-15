;;; lxc.el --- lxc integration with Emacs

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes
;; Package-Version: 20140410.2022
;; Package-Commit: 88bed56c954d1edd9ff5ce0ced2c02dcf9f71835
;; Version: 0.0.1
;; Url: https://github.com/nicferrier/emacs-lxc

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

;; Some simple tools for dealing with LXC containers in Emacs.

;;; Code:

(defconst lxc/list-command
  "sudo lxc-ls --fancy-format name,state,pid,ipv4 --fancy"
  "The command we use for listing containers.")

(defun lxc/list ()
  "Make the list of containers using `lxc/list-command.'"
  (let ((cmd lxc/list-command))
    (->> (-drop 2 (split-string (shell-command-to-string cmd) "\n"))
      (-filter (lambda (s) (> (length s) 0)))
      (-map (lambda (s)
              (-filter
               (lambda (s) (> (length s) 0))
               (split-string s " ")))))))

(defun lxc/get-id ()
  (if (not (eq major-mode 'lxc-list-table-mode))
      (error "lxc-start: need an ID")
      (list
       (buffer-substring-no-properties
        (line-beginning-position)
        (save-excursion
          (goto-char (line-beginning-position))
          (- 
           (or
            (re-search-forward " " (line-end-position) t)
            (line-end-position)) 1))))))

(defun lxc-start (id)
  "Start the specified LXC container."
  (interactive (lxc/get-id))
  (shell-command (format "sudo lxc-start -d -n %s" id))
  (when (called-interactively-p 'any)
    (tabulated-list-print)))

(defun lxc-kill (id)
  "Stop the specified LXC container.

This doesn't actually do an LXC KILL, it uses LXC clean shut
down."
  (interactive (lxc/get-id))
  (shell-command (format "sudo lxc-stop -n %s" id))
  (when (called-interactively-p 'any)
    (tabulated-list-print)))

(defun lxc/table-entrys ()
  "Make the table entries for `tabulated-list-mode'."
  (-map (lambda (e)
          (list (car e) (apply 'vector e)))
   (lxc/list)))

(define-derived-mode
    lxc-list-table-mode tabulated-list-mode "LXC containers"
    "Major mode for listing your LXC containers."
    (setq tabulated-list-entries 'lxc/table-entrys)
    ;; Make a key to start and stop lxc's
    (define-key lxc-list-table-mode-map (kbd "s") 'lxc-start)
    (define-key lxc-list-table-mode-map (kbd "k") 'lxc-kill)
    (define-key lxc-list-table-mode-map (kbd "O") 'lxc-open)
    (setq tabulated-list-format
          (loop for col in (list "name" "state" "pid" "IP")
             vconcat (list (list col 20 nil))))
    (tabulated-list-init-header))

;;;###autoload
(defun list-lxc ()
  "Show the list of LXC containers you have."
  (interactive)
  (with-current-buffer (get-buffer-create "*lxc-list*")
    (lxc-list-table-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(defun lxc-open (name)
  "Open a tramp session to the container."
  (interactive
   (list
    (progn
      (goto-char (line-beginning-position))
      (re-search-forward "^\\([a-zA-Z0-9-]+\\)" (line-end-position) t)
      (let ((name (match-string 1))) name))))
  (let* ((container (kva name (lxc/list)))
         (ip (elt container 2)))
    ;; where do we get the username from?
    (find-file-other-window (format "/ssh:ubuntu@%s:" ip))))

(provide 'lxc)
;;; lxc.el ends here
