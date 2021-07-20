;;; orgtbl-show-header-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orgtbl-show-header" "orgtbl-show-header.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from orgtbl-show-header.el

(autoload 'orgtbl-show-header-of-current-column "orgtbl-show-header" "\
In a table, show the header of the column the point is in." t nil)

(autoload 'orgtbl-show-header "orgtbl-show-header" "\
Show current header while navigating in the table.

This is a minor mode.  If called interactively, toggle the
`Orgtbl-Show-Header mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `orgtbl-show-header'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "orgtbl-show-header" '("orgtbl-show-header-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orgtbl-show-header-autoloads.el ends here
