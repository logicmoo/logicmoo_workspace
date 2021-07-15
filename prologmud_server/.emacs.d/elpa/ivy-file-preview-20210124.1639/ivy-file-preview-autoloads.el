;;; ivy-file-preview-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-file-preview" "ivy-file-preview.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ivy-file-preview.el

(defvar ivy-file-preview-mode nil "\
Non-nil if Ivy-File-Preview mode is enabled.
See the `ivy-file-preview-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-file-preview-mode'.")

(custom-autoload 'ivy-file-preview-mode "ivy-file-preview" nil)

(autoload 'ivy-file-preview-mode "ivy-file-preview" "\
Minor mode 'ivy-file-preview-mode'.

This is a minor mode.  If called interactively, toggle the
`Ivy-File-Preview mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'ivy-file-preview-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ivy-file-preview" '("ivy-file-preview-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-file-preview-autoloads.el ends here
