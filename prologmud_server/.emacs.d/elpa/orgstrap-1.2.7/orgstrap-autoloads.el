;;; orgstrap-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orgstrap" "orgstrap.el" (0 0 0 0))
;;; Generated autoloads from orgstrap.el

(autoload 'orgstrap--confirm-eval-portable "orgstrap" "\
A backwards compatible, portable implementation for confirm-eval.
This should be called by `org-confirm-babel-evaluate'.  As implemented
the only LANG that is supported is emacs-lisp or elisp.  The argument
_BODY is rederived for portability and thus not used.

\(fn LANG BODY)" nil nil)

(defalias 'orgstrap--confirm-eval #'orgstrap--confirm-eval-portable)

(autoload 'orgstrap-mode "orgstrap" "\
A regional minor mode for `org-mode' that automatically runs orgstrap blocks.
When visiting an Org file or activating `org-mode', if orgstrap prop line local
variables are detect then use the installed orgstrap implementation to run the
orgstrap block.  If orgstrap embedded local variables are present, they will not
be executed.  `orgstrap-mode' is not a normal minor mode since it does not run
any hooks and when enabled only adds a function to `org-mode-hook'.  ARG is the
universal prefix argument.

\(fn &optional ARG)" t nil)

(autoload 'orgstrap-edit-mode "orgstrap" "\
Minor mode for editing with orgstrapped files.

This is a minor mode.  If called interactively, toggle the
`Orgstrap-Edit mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `orgstrap-edit-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'orgstrap-init "orgstrap" "\
Initialize orgstrap in a buffer and enable command `orgstrap-edit-mode'.
PREFIX-ARGUMENT is essentially minimal from other functions, when non-nil
the minimal local variables will be used if possible.

\(fn &optional PREFIX-ARGUMENT)" t nil)

(register-definition-prefixes "orgstrap" '("orgstrap-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orgstrap-autoloads.el ends here
