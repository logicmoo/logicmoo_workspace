;;; org-special-block-extras-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-special-block-extras" "org-special-block-extras.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-special-block-extras.el

(autoload 'org-special-block-extras-mode "org-special-block-extras" "\
Provide 30 new custom blocks & 34 link types for Org-mode.

This is a minor mode.  If called interactively, toggle the
`Org-Special-Block-Extras mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-special-block-extras-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-special-block-extras" '("org-special-block-extras-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-special-block-extras-autoloads.el ends here
