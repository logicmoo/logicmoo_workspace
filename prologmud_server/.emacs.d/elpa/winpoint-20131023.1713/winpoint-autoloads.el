;;; winpoint-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "winpoint" "winpoint.el" (0 0 0 0))
;;; Generated autoloads from winpoint.el

(defalias 'window-point-remember-mode 'winpoint-mode)

(defvar winpoint-mode nil "\
Non-nil if Winpoint mode is enabled.
See the `winpoint-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `winpoint-mode'.")

(custom-autoload 'winpoint-mode "winpoint" nil)

(autoload 'winpoint-mode "winpoint" "\
Remember positions in a buffer per-window, not per-buffer.
That is, when you have the same buffer open in two different
windows, and you switch the buffer in one window and back again,
the position is the same as it was when you switched away, not
the same as in the other window.

This is a minor mode.  If called interactively, toggle the
`Winpoint mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'winpoint-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "winpoint" '("filter" "winpoint-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; winpoint-autoloads.el ends here
