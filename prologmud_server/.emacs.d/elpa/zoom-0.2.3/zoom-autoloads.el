;;; zoom-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zoom" "zoom.el" (0 0 0 0))
;;; Generated autoloads from zoom.el

(defvar zoom-mode nil "\
Non-nil if Zoom mode is enabled.
See the `zoom-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `zoom-mode'.")

(custom-autoload 'zoom-mode "zoom" nil)

(autoload 'zoom-mode "zoom" "\
Perform `zoom' automatically as the selected window changes.

This is a minor mode.  If called interactively, toggle the `Zoom
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'zoom-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'zoom "zoom" "\
Zoom the current window and balance the others according to `zoom-size'." t nil)

(register-definition-prefixes "zoom" '("zoom-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zoom-autoloads.el ends here
