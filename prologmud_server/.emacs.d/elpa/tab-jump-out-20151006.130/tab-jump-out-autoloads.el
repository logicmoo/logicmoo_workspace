;;; tab-jump-out-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tab-jump-out" "tab-jump-out.el" (0 0 0 0))
;;; Generated autoloads from tab-jump-out.el

(autoload 'tab-jump-out "tab-jump-out" "\
Use tab to jump out.

\(fn ARG)" t nil)

(autoload 'tab-jump-out-mode "tab-jump-out" "\
A minor mode that allows you to jump out with tab.

This is a minor mode.  If called interactively, toggle the
`Tab-Jump-Out mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tab-jump-out-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "tab-jump-out" '("tab-jump-out-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tab-jump-out-autoloads.el ends here
