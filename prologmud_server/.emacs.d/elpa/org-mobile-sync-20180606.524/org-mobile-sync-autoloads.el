;;; org-mobile-sync-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-mobile-sync" "org-mobile-sync.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-mobile-sync.el

(defvar org-mobile-sync-mode nil "\
Non-nil if Org-Mobile-Sync mode is enabled.
See the `org-mobile-sync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-mobile-sync-mode'.")

(custom-autoload 'org-mobile-sync-mode "org-mobile-sync" nil)

(autoload 'org-mobile-sync-mode "org-mobile-sync" "\
Toggle org-mobile-sync mode globally.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode.

This is a minor mode.  If called interactively, toggle the
`Org-Mobile-Sync mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'org-mobile-sync-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-mobile-sync" '("org-mobile-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-mobile-sync-autoloads.el ends here
