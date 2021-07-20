;;; org-wild-notifier-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-wild-notifier" "org-wild-notifier.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-wild-notifier.el

(autoload 'org-wild-notifier-check "org-wild-notifier" "\
Parse agenda view and notify about upcomming events." t nil)

(defvar org-wild-notifier-mode nil "\
Non-nil if Org-Wild-Notifier mode is enabled.
See the `org-wild-notifier-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-wild-notifier-mode'.")

(custom-autoload 'org-wild-notifier-mode "org-wild-notifier" nil)

(autoload 'org-wild-notifier-mode "org-wild-notifier" "\
Toggle org notifications globally.
When enabled parses your agenda once a minute and emits notifications
if needed.

This is a minor mode.  If called interactively, toggle the
`Org-Wild-Notifier mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'org-wild-notifier-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-wild-notifier" '("org-wild-notifier-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-wild-notifier-autoloads.el ends here
