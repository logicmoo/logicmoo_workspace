;;; toc-org-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "toc-org" "toc-org.el" (0 0 0 0))
;;; Generated autoloads from toc-org.el

(autoload 'toc-org-enable "toc-org" "\
Enable toc-org in this buffer." nil nil)

(autoload 'toc-org-mode "toc-org" "\
Toggle `toc-org' in this buffer.

This is a minor mode.  If called interactively, toggle the
`Toc-Org mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `toc-org-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "toc-org" '("toc-org-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; toc-org-autoloads.el ends here
