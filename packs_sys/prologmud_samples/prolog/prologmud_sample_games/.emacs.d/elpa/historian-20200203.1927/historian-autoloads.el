;;; historian-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "historian" "historian.el" (0 0 0 0))
;;; Generated autoloads from historian.el

(autoload 'historian-save "historian" "\
Save the historian history to `historian-save-file'." t nil)

(autoload 'historian-load "historian" nil t nil)

(autoload 'historian-clear "historian" nil t nil)

(defvar historian-mode nil "\
Non-nil if Historian mode is enabled.
See the `historian-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `historian-mode'.")

(custom-autoload 'historian-mode "historian" nil)

(autoload 'historian-mode "historian" "\
historian minor mode

This is a minor mode.  If called interactively, toggle the
`Historian mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'historian-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "historian" '("historian-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; historian-autoloads.el ends here
