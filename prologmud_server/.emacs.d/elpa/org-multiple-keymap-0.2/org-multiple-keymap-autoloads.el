;;; org-multiple-keymap-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-multiple-keymap" "org-multiple-keymap.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-multiple-keymap.el

(autoload 'org-multiple-keymap-minor-mode "org-multiple-keymap" "\
Toggle `org-multiple-keymap-minor-mode'.
With a prefix argument ARG, enable `org-multiple-keymap-minor-mode' if
ARG is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Key bindings (heading):
\\{org-mukey-heading-map}

Key bindings (timestamp):
\\{org-mukey-timestamp-map}

Key bindings (priority):
\\{org-mukey-priority-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-multiple-keymap" '("org-mu"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-multiple-keymap-autoloads.el ends here
