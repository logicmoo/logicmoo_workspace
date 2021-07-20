;;; yankpad-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yankpad" "yankpad.el" (0 0 0 0))
;;; Generated autoloads from yankpad.el

(autoload 'yankpad-insert "yankpad" "\
Insert an entry from the yankpad.
Uses `yankpad-category', and prompts for it if it isn't set." t nil)

(autoload 'company-yankpad "yankpad" "\
Company backend for yankpad.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "yankpad" '("company-yankpad--name-or-key" "yankpad-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yankpad-autoloads.el ends here
