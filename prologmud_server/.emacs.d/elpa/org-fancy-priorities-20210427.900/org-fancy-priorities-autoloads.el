;;; org-fancy-priorities-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-fancy-priorities" "org-fancy-priorities.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-fancy-priorities.el

(autoload 'org-fancy-priorities-mode "org-fancy-priorities" "\
Customize the appearance of org-mode priorities.
This mode does not alter your files in any way, it
only changes the way that priorities are shown in your editor.

This is a minor mode.  If called interactively, toggle the
`Org-Fancy-Priorities mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-fancy-priorities-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-fancy-priorities" '("org-fancy-priorities-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-fancy-priorities-autoloads.el ends here
