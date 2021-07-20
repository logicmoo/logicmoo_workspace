;;; org-context-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-context" "org-context.el" (0 0 0 0))
;;; Generated autoloads from org-context.el

(autoload 'org-context-agenda-from "org-context" "\


\(fn FILE-OR-BUFFER KEY)" nil nil)

(defvar org-context-mode nil "\
Non-nil if Org-Context mode is enabled.
See the `org-context-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-context-mode'.")

(custom-autoload 'org-context-mode "org-context" nil)

(autoload 'org-context-mode "org-context" "\
Minor mode to activate `org-context'.

This is a minor mode.  If called interactively, toggle the
`Org-Context mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'org-context-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'org-context-activate 'org-context-mode "0.0.5")

(register-definition-prefixes "org-context" '("org-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-context-autoloads.el ends here
