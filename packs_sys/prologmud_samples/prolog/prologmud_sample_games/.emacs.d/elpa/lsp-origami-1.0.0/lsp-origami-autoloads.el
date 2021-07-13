;;; lsp-origami-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-origami" "lsp-origami.el" (0 0 0 0))
;;; Generated autoloads from lsp-origami.el

(autoload 'lsp-origami-try-enable "lsp-origami" "\
Turn on `origami-mode' locally and try to enable `lsp-origami-mode'." t nil)

(autoload 'lsp-origami-mode "lsp-origami" "\
Toggle code folding support for origami.

This is a minor mode.  If called interactively, toggle the
`Lsp-Origami mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-origami-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "lsp-origami" '("lsp-origami--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-origami-autoloads.el ends here
