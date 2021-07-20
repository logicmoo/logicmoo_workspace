;;; org-elp-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-elp" "org-elp.el" (0 0 0 0))
;;; Generated autoloads from org-elp.el

(autoload 'org-elp-activate "org-elp" "\
Activate previewing buffer and idle timer." t nil)

(autoload 'org-elp-deactivate "org-elp" "\
Deactivate previewing and remove the idle timer." t nil)

(autoload 'org-elp-mode "org-elp" "\
org-elp mode: display latex fragment while typing.

This is a minor mode.  If called interactively, toggle the
`org-elp mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-elp-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-elp" '("org-elp-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-elp-autoloads.el ends here
