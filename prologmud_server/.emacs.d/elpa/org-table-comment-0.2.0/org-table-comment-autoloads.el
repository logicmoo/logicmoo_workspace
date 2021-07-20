;;; org-table-comment-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-table-comment" "org-table-comment.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-table-comment.el

(defalias 'org-table-comment-mode 'orgtbl-comment-mode)

(autoload 'orgtbl-comment-mode "org-table-comment" "\
Orgtbl comment mode.  Changes how orgtbl works for modes that don't support block comment regions (like emacs-lisp).

This is a minor mode.  If called interactively, toggle the
`Orgtbl-Comment mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `orgtbl-comment-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Currently supports radio tables through overlay interface.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-table-comment" '("org-table-comment-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-table-comment-autoloads.el ends here
