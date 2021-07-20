;;; vtm-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vtm" "vtm.el" (0 0 0 0))
;;; Generated autoloads from vtm.el

(defvar vtm-edit-mode nil "\
Non-nil if Vtm-Edit mode is enabled.
See the `vtm-edit-mode' command
for a description of this minor mode.")

(custom-autoload 'vtm-edit-mode "vtm" nil)

(autoload 'vtm-edit-mode "vtm" "\
Enable editing the vtm file.

This is a minor mode.  If called interactively, toggle the
`Vtm-Edit mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'vtm-edit-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'vtm-mode "vtm" "\
Elisp Mode starter for vtm files.

\(fn)" t nil)

(register-definition-prefixes "vtm" '("vtm-"))

;;;***

;;;### (autoloads nil nil ("example.el" "vtm-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vtm-autoloads.el ends here
