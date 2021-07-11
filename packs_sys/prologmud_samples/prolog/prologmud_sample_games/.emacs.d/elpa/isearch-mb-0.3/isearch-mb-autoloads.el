;;; isearch-mb-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "isearch-mb" "isearch-mb.el" (0 0 0 0))
;;; Generated autoloads from isearch-mb.el

(defvar isearch-mb-mode nil "\
Non-nil if Isearch-Mb mode is enabled.
See the `isearch-mb-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `isearch-mb-mode'.")

(custom-autoload 'isearch-mb-mode "isearch-mb" nil)

(autoload 'isearch-mb-mode "isearch-mb" "\
Control isearch from the minibuffer.

This is a minor mode.  If called interactively, toggle the
`Isearch-Mb mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'isearch-mb-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

During an isearch-mb session, the following keys are available:
\\{isearch-mb-minibuffer-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "isearch-mb" '("isearch-mb-"))

;;;***

;;;### (autoloads nil nil ("isearch-mb-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; isearch-mb-autoloads.el ends here
