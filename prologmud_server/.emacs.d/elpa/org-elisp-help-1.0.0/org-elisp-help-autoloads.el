;;; org-elisp-help-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-elisp-help" "org-elisp-help.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-elisp-help.el

(autoload 'org-elisp-help-function-insert-link "org-elisp-help" "\
Prompt for a function and insert a \"elisp-function\" link at point.

\(fn FUNCTION &optional DESCRIPTION)" t nil)

(autoload 'org-elisp-help-variable-insert-link "org-elisp-help" "\
Prompt for a variable and insert a \"elisp-variable\" link at point.

\(fn VARIABLE &optional DESCRIPTION)" t nil)

(register-definition-prefixes "org-elisp-help" '("org-elisp-help-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-elisp-help-autoloads.el ends here
