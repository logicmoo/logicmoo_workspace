;;; impatient-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "impatient-mode" "impatient-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from impatient-mode.el

(autoload 'impatient-mode "impatient-mode" "\
Serves the buffer live over HTTP.

This is a minor mode.  If called interactively, toggle the
`impatient mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `impatient-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "impatient-mode" '("httpd/imp" "imp"))

;;;***

;;;### (autoloads nil nil ("impatient-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; impatient-mode-autoloads.el ends here
