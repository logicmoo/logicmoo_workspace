;;; org-if-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ob-org-if" "ob-org-if.el" (0 0 0 0))
;;; Generated autoloads from ob-org-if.el

(autoload 'org-babel-execute:org-if "ob-org-if" "\
Execute a block of ORG-IF code with org-babel.
This function is called by `org-babel-execute-src-block'

\(fn BODY PARAMS)" nil nil)

(register-definition-prefixes "ob-org-if" '("org-babel-default-header-args:org-if"))

;;;***

;;;### (autoloads nil "org-if-active" "org-if-active.el" (0 0 0 0))
;;; Generated autoloads from org-if-active.el

(defvar org-if-active-mode nil "\
Non-nil if Org-If-Active mode is enabled.
See the `org-if-active-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-if-active-mode'.")

(custom-autoload 'org-if-active-mode "org-if-active" nil)

(autoload 'org-if-active-mode "org-if-active" "\
This mode toggles whether the org-if system is active.

This is a minor mode.  If called interactively, toggle the
`Org-If-Active mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'org-if-active-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'activate-org-if "org-if-active" "\
Activate org-if-active minor-mode.
When NO-NAVIGATE-P is specified, do not go to file \"index.org\" in current directory.

\(fn &optional NO-NAVIGATE-P)" t nil)

(autoload 'deactivate-org-if "org-if-active" "\
Deactivate org-if-active minor-mode." t nil)

(autoload 'toggle-org-if-active-mode "org-if-active" "\
Toggle `org-if-active-mode'." t nil)

(autoload 'org-if-save-and-quit "org-if-active" "\
Save state of current org-if session in a file in `org-if-save-dir'.
Then quit." t nil)

(autoload 'org-if-restore "org-if-active" "\
Restore state of `*org-if-current-env*' and `*org-if-current-file*' from save.
Also restore last visited file." t nil)

(register-definition-prefixes "org-if-active" '("org-if-"))

;;;***

;;;### (autoloads nil "org-if-interpreter" "org-if-interpreter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-if-interpreter.el

(register-definition-prefixes "org-if-interpreter" '("*org-if-funcs*" "org-if-"))

;;;***

;;;### (autoloads nil "org-if-link" "org-if-link.el" (0 0 0 0))
;;; Generated autoloads from org-if-link.el

(register-definition-prefixes "org-if-link" '("org-if-open"))

;;;***

;;;### (autoloads nil "org-if-misc" "org-if-misc.el" (0 0 0 0))
;;; Generated autoloads from org-if-misc.el

(register-definition-prefixes "org-if-misc" '("*org-if-" "org-if-"))

;;;***

;;;### (autoloads nil "org-if-mode" "org-if-mode.el" (0 0 0 0))
;;; Generated autoloads from org-if-mode.el

(autoload 'org-if-mode "org-if-mode" "\
Major mode for ORG-IF programming language.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-if-reader" "org-if-reader.el" (0 0 0 0))
;;; Generated autoloads from org-if-reader.el

(register-definition-prefixes "org-if-reader" '("*org-if-" "org-if-"))

;;;***

;;;### (autoloads nil nil ("org-if-pkg.el" "org-if.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-if-autoloads.el ends here
