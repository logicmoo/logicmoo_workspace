;;; terminal-focus-reporting-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "terminal-focus-reporting" "terminal-focus-reporting.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from terminal-focus-reporting.el

(defvar terminal-focus-reporting-mode nil "\
Non-nil if Terminal-Focus-Reporting mode is enabled.
See the `terminal-focus-reporting-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `terminal-focus-reporting-mode'.")

(custom-autoload 'terminal-focus-reporting-mode "terminal-focus-reporting" nil)

(autoload 'terminal-focus-reporting-mode "terminal-focus-reporting" "\
Minor mode for terminal focus reporting integration.

This is a minor mode.  If called interactively, toggle the
`Terminal-Focus-Reporting mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'terminal-focus-reporting-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "terminal-focus-reporting" '("terminal-focus-reporting-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; terminal-focus-reporting-autoloads.el ends here
