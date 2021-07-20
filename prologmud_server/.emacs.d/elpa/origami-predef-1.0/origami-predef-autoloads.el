;;; origami-predef-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "origami-predef" "origami-predef.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from origami-predef.el

(defvar origami-predef-global-mode nil "\
Non-nil if Origami-Predef-Global mode is enabled.
See the `origami-predef-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `origami-predef-global-mode'.")

(custom-autoload 'origami-predef-global-mode "origami-predef" nil)

(autoload 'origami-predef-global-mode "origami-predef" "\
Apply initial folding when finding (opening) a file buffer

This is a minor mode.  If called interactively, toggle the
`Origami-Predef-Global mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'origami-predef-global-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "origami-predef" '("origami-predef-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; origami-predef-autoloads.el ends here
