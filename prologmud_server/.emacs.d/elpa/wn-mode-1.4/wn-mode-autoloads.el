;;; wn-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wn-mode" "wn-mode.el" (0 0 0 0))
;;; Generated autoloads from wn-mode.el

(autoload 'wn-select-nth "wn-mode" "\
Select window number N in current frame.

\(fn N)" t nil)

(defvar wn-mode nil "\
Non-nil if Wn mode is enabled.
See the `wn-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `wn-mode'.")

(custom-autoload 'wn-mode "wn-mode" nil)

(autoload 'wn-mode "wn-mode" "\
A minor mode that enables quick selection of windows.

This is a minor mode.  If called interactively, toggle the `Wn
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'wn-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "wn-mode" '("wn-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wn-mode-autoloads.el ends here
