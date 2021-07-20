;;; tab-bar-echo-area-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tab-bar-echo-area" "tab-bar-echo-area.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tab-bar-echo-area.el

(autoload 'tab-bar-echo-area-display-tab-names "tab-bar-echo-area" "\
Display tab names in the echo area." t nil)

(defalias 'tab-bar-echo-area-print-tab-names 'tab-bar-echo-area-display-tab-names)

(autoload 'tab-bar-echo-area-display-tab-name "tab-bar-echo-area" "\
Display the current tab's name in the echo area." t nil)

(defalias 'tab-bar-echo-area-print-tab-name 'tab-bar-echo-area-display-tab-name)

(defvar tab-bar-echo-area-mode nil "\
Non-nil if Tab-Bar-Echo-Area mode is enabled.
See the `tab-bar-echo-area-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tab-bar-echo-area-mode'.")

(custom-autoload 'tab-bar-echo-area-mode "tab-bar-echo-area" nil)

(autoload 'tab-bar-echo-area-mode "tab-bar-echo-area" "\
Alternative to function `tab-bar-mode': display tab names in the echo area after tab bar-related functions.

This is a minor mode.  If called interactively, toggle the
`Tab-Bar-Echo-Area mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'tab-bar-echo-area-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "tab-bar-echo-area" '("tab-bar-echo-area-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tab-bar-echo-area-autoloads.el ends here
