;;; org-themis-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-themis" "org-themis.el" (0 0 0 0))
;;; Generated autoloads from org-themis.el

(autoload 'org-themis-mode "org-themis" "\
Experimental project management mode for `org-mode'

This is a minor mode.  If called interactively, toggle the
`Org-Themis mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-themis-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'org-themis-mode-enable "org-themis" "\
Enable `org-themis-mode'." nil nil)

(autoload 'org-themis-mode-disable "org-themis" "\
Disable `org-themis-mode'." nil nil)

(register-definition-prefixes "org-themis" '("org-themis-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-themis-autoloads.el ends here
