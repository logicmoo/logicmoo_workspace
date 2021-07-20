;;; org-mobile-sync.el --- automatically sync org-mobile on changes

;; Copyright (C) 2013 steckerhalter

;; Author: steckerhalter
;; Package-Requires: ((emacs "24.3.50") (org "8.0"))
;; Package-Version: 20180606.524
;; Package-Commit: 06764b943a528827df1e2acc6bc7806cc2c1351f
;; URL: https://framagit.org/steckerhalter/org-mobile-sync
;; Keywords: org-mode org mobile sync todo

;;; Commentary:

;; Adds delayed `org-mobile-push' upon saving files that are part of
;; `org-mobile-files-alist'. Watches the `org-mobile-capture-file' for
;; changes with `file-notify.el' and then invokes `org-mobile-pull'.

;;; Requirements:

;; Emacs 24.3.50 with `file-notify-support' is required for it to work.

;;; Usage:

;; (require 'org-mobile-sync)
;; (org-mobile-sync-mode 1)

;;; Code:

(require 'filenotify)
(require 'org-mobile)

(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defvar org-mobile-watch-descriptor nil
  "Descriptor used by adding a watcher for `org-mobile-capture-file'.")

(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(defun org-mobile-push-function ()
  (when (eq major-mode 'org-mode)
    (dolist (file (org-mobile-files-alist))
      (if (string= (file-truename (expand-file-name (car file)))
                   (file-truename (buffer-file-name)))
          (org-mobile-push-with-delay 30)))
    ))

(defun org-mobile-sync--callback (ev)
  "Handle file-notify callbacks.
Argument EV contains the watch data."
  (org-mobile-pull)
  )

(defun org-mobile-sync-setup ()
  (org-mobile-pull)               ;pull changes on activation
  (add-hook 'after-save-hook 'org-mobile-push-function)
  (setq org-mobile-watch-descriptor
        (file-notify-add-watch
         (file-truename
          (concat
           (file-name-as-directory org-mobile-directory)
           org-mobile-capture-file))
         '(change attribute-change)
         #'org-mobile-sync--callback))
  )

(defun org-mobile-sync-teardown ()
  (remove-hook 'after-save-hook 'org-mobile-push-function)
  (file-notify-rm-watch org-mobile-watch-descriptor)
  )

;;;###autoload
(define-minor-mode org-mobile-sync-mode
  "Toggle org-mobile-sync mode globally.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."
  :global t

  (if org-mobile-sync-mode
      (org-mobile-sync-setup)
    (org-mobile-sync-teardown))
  )

(provide 'org-mobile-sync)
;;; org-mobile-sync.el ends here
