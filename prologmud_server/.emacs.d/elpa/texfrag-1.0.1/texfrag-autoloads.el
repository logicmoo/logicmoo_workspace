;;; texfrag-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "texfrag" "texfrag.el" (0 0 0 0))
;;; Generated autoloads from texfrag.el
:

(autoload 'texfrag-mode "texfrag" "\
Preview LaTeX fragments in current buffer with the help of the
`preview' package.

This is a minor mode.  If called interactively, toggle the
`TeXfrag mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `texfrag-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'texfrag-global-mode 'globalized-minor-mode t)

(defvar texfrag-global-mode nil "\
Non-nil if Texfrag-Global mode is enabled.
See the `texfrag-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `texfrag-global-mode'.")

(custom-autoload 'texfrag-global-mode "texfrag" nil)

(autoload 'texfrag-global-mode "texfrag" "\
Toggle Texfrag mode in all buffers.
With prefix ARG, enable Texfrag-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Texfrag mode is enabled in all buffers where
`texfrag-global-mode-fun' would do it.

See `texfrag-mode' for more information on Texfrag mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "texfrag" '("texfrag-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; texfrag-autoloads.el ends here
