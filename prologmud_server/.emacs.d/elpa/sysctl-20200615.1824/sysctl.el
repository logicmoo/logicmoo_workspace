;;; sysctl.el --- Manage sysctl though org-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Dante Catalfamo

;; Author: Dante Catalfamo
;; Version: 0.3.3
;; Package-Version: 20200615.1824
;; Package-Commit: d8c2e18de1d7a3b2999a4d5054c0bbf30cb10fed
;; Package-Requires: ((emacs "26"))
;; URL: https://github.com/dantecatalfamo/sysctl.el
;; Keywords: sysctl, tools, unix

;; This file is not part of GNU Emacs.

;;; Commentary:

;; View and edit sysctl in a hierarchical structure.
;; Works on Linux, FreeBSD, OpenBSD, and macOS.
;; Will work over TRAMP via SSH as well, including multiple hops.

;;; Commands

;; `sysctl' Generate the sysctl buffer

;;; Keybindings

;; C-c C-c Set the value of current position in the sysctl tree
;; C-c C-k Refresh the value of current position in the sysctl tree

;;; Bugs

;; Only the first line of a multiline value gets shown


;;; Code:

(require 'subr-x)
(require 'org)

(defvar sysctl-buffer-prefix "sysctl"
  "Default prefix of sysctl buffers.")

(defun sysctl--run-command (args)
  "Run shell commands ARGS and return output as a string, only exists as a TRAMP issue work around."
  (let ((shell-file-name "/bin/sh"))
    (shell-command-to-string args)))

(defun sysctl-run (args)
  "Run `sysctl' with the ARGS arguments, run with root if AS-ROOT is non-nil."
  (sysctl--run-command (concat "sysctl " args)))

(defun sysctl-separator ()
  "System-dependant sysctl separator."
  (pcase (sysctl--run-command "uname -s")
    ("OpenBSD\n" "=")
    ("Linux\n" " = ")
    (_ ": ")))  ; Darwin and FreeBSD both use ": "

(defun sysctl-split-line (line separator)
  "Split LINE into key and value, splitting with SEPARATOR."
  (let (key value)
    (when (string-match (concat "\\(^\\w+\\..*?\\)" separator "\\(.*\\)$") line)
        (save-match-data
          (setq key (split-string (match-string 1 line) "\\.")))
      (setq value (match-string 2 line))
      (cons key value))))

(defun sysctl-split-lines (lines)
  "Split LINES into lines, keys, values."
  (let (output
        (separator (sysctl-separator))
        (split-lines (split-string lines "\n")))
    (dolist (line split-lines output)
      (if-let (split-line (sysctl-split-line line separator))
          (push split-line output)))
    (nreverse output)))

(defun sysctl--readonly-previous-line ()
  "Set the current line to read-only."
  (forward-line -1)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (setq begin (if (eq begin 1) 1 (1- begin)))
    (set-text-properties begin end '(read-only t))
    (forward-line)))

(defun sysctl-construct-tree (lines-list)
  "Turn LINES-LIST into an org hierarchy."
  (let (path)
    (dolist (line lines-list)
      (let ((line-path (car line))
            (line-value (cdr line))
            (depth 1))
        (while line-path
          (unless (string= (nth (- depth 1) path)
                           (car line-path))
            (insert (concat (make-string depth ?*) " " (car line-path) "\n"))
            (sysctl--readonly-previous-line))
          (setq line-path (cdr line-path)
                depth (1+ depth)))
        (setq path (car line))
        (insert (concat line-value "\n"))))))

(defun sysctl-construct-path ()
  "Construct path from the current leaf node on the sysctl tree."
    (save-excursion
      (let (path)
        (unless (org-at-heading-p)
          (outline-previous-heading)
          (push (substring-no-properties (org-get-heading t t t t)) path)
          (while (org-up-heading-safe)
            (push (substring-no-properties (org-get-heading t t t t)) path))
          (concat (string-join path "."))))))

(defun sysctl-construct-command ()
  "Construct a sysctl command from the current position in the tree."
  (save-excursion
    (when-let ((path (sysctl-construct-path))
               (value (string-trim (thing-at-point 'line t))))
      (concat path "=" value))))

(defun sysctl-superuser-cmd ()
  "Return the system's super user command."
  (pcase (sysctl--run-command "uname -s")
    ("OpenBSD\n" "doas")
    (_ "sudo")))

(defun sysctl-construct-tramp ()
  "Construct the TRAMP path required to run a command as root."
  (if (string= (sysctl--run-command "whoami") "root\n")
      default-directory
    (if (string-prefix-p "/ssh" default-directory)
        (let (ssh-host)
          (save-match-data
            (when (string-match ".*:\\(.+\\):" default-directory)
              (setq ssh-host (match-string 1 default-directory))
              (string-match "\\(.*\\):[^:]+$" default-directory)
              (concat (match-string 1 default-directory) "|" (sysctl-superuser-cmd) ":" ssh-host ":"))))
      (concat "/" (sysctl-superuser-cmd) "::"))))

(defun sysctl-set-value ()
  "Set the value of the current leaf on the tree in sysctl."
  (interactive)
  (if-let ((sysctl-cmd (sysctl-construct-command)))
      (if (y-or-n-p (concat "Set " sysctl-cmd "?"))
          (let ((default-directory (sysctl-construct-tramp)))
            (message (string-trim (sysctl-run sysctl-cmd)))
            (sysctl-refresh-value))
        (message "Not set."))
    (message "The point must be on a value.")))

(defun sysctl-refresh-value ()
  "Get the current value for a certain leaf node in the sysctl tree."
  (interactive)
  (if-let* ((path (sysctl-construct-path))
            (line (sysctl-run path))
            (value (cdr (sysctl-split-line line (sysctl-separator)))))
      (progn (delete-region (line-beginning-position) (line-end-position))
             (insert value))
    (when (called-interactively-p 'interactive)
      (message "The point must be on a value."))))

(defun sysctl-buffer-name ()
  "Format sysctl buffer name for current host."
    (if-let ((host (file-remote-p default-directory 'host)))
        (format "*%s %s*" sysctl-buffer-prefix host)
      (format "*%s %s*" sysctl-buffer-prefix (system-name))))

(defvar sysctl-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") #'sysctl-set-value)
        (define-key map (kbd "C-c C-k") #'sysctl-refresh-value)
        map))

(define-derived-mode sysctl-mode org-mode "Sysctl"
  "Mode for managing sysctl configs.")

;;;###autoload
(defun sysctl ()
  "Construct an Org buffer from the sysctl tree."
  (interactive)
  (switch-to-buffer (sysctl-buffer-name))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (sysctl-construct-tree (sysctl-split-lines (sysctl-run "-a")))
  (sysctl-mode)
  (when flyspell-mode
    (flyspell-mode-off)))

(provide 'sysctl)
;;; sysctl.el ends here
