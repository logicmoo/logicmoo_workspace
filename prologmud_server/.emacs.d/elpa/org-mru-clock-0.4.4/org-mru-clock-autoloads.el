;;; org-mru-clock-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-mru-clock" "org-mru-clock.el" (0 0 0 0))
;;; Generated autoloads from org-mru-clock.el

(autoload 'org-mru-clock-to-history "org-mru-clock" "\
Ensure `org-clock-history' filled with agenda tasks.
Optional argument N as in `org-mru-clock'.

\(fn &optional N)" t nil)

(autoload 'org-mru-clock-select-recent-task "org-mru-clock" "\
Select a task that was recently associated with clocking.
Like `org-clock-select-task', but ensures `org-clock-history' is
filled first.  Optional argument N as in `org-mru-clock'.

\(fn &optional N)" t nil)

(autoload 'org-mru-clock-in "org-mru-clock" "\
Use completion to clock in to a task recently associated with clocking.
See `org-mru-clock-completing-read' for the completion function
used.  Optional argument N as in `org-mru-clock'.

If `org-mru-clock-capture-if-no-match' is non-nil, we may create
a new task from the text entered.

\(fn &optional N)" t nil)

(register-definition-prefixes "org-mru-clock" '("org-mru-clock"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-mru-clock-autoloads.el ends here
