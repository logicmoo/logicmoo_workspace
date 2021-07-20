;;; winnow-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "winnow" "winnow.el" (0 0 0 0))
;;; Generated autoloads from winnow.el

(autoload 'winnow-mode "winnow" "\
Filter compilation results by matching/excluding lines.

This is a minor mode.  If called interactively, toggle the
`Winnow mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `winnow-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This is invaluable for excluding or limiting to matching `ag-mode' results.

\\{winnow-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "winnow" '("winnow-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; winnow-autoloads.el ends here
