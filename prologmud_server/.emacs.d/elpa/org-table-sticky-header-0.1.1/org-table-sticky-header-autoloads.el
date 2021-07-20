;;; org-table-sticky-header-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-table-sticky-header" "org-table-sticky-header.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-table-sticky-header.el

(autoload 'org-table-sticky-header-mode "org-table-sticky-header" "\
Sticky header for org-mode tables.

This is a minor mode.  If called interactively, toggle the
`Org-Table-Sticky-Header mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-table-sticky-header-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-table-sticky-header" '("org-table-sticky-header-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-table-sticky-header-autoloads.el ends here
