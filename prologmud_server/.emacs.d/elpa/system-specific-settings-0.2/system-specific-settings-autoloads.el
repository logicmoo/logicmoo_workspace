;;; system-specific-settings-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "system-specific-settings" "system-specific-settings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from system-specific-settings.el

(autoload 'if-system-type-match "system-specific-settings" "\
If COND matches `system-type', do THEN, else do ELSE.

Matching is done using `system-specific-settings-do-match'.

\(fn COND THEN &rest ELSE)" nil t)

(function-put 'if-system-type-match 'lisp-indent-function '2)

(autoload 'if-system-name-match "system-specific-settings" "\
If COND matches `system-name', do THEN, else do ELSE.

Matching is done using `system-specific-settings-do-match'.

\(fn COND THEN &rest ELSE)" nil t)

(function-put 'if-system-name-match 'lisp-indent-function '2)

(autoload 'when-system-type-match "system-specific-settings" "\
Eval BODY only if `system-type' matches COND.

Matching is done using `system-specific-settings-do-match'.

\(fn COND &rest BODY)" nil t)

(function-put 'when-system-type-match 'lisp-indent-function '1)

(autoload 'when-system-name-match "system-specific-settings" "\
Eval BODY only if `system-name' matches COND.

Matching is done using `system-specific-settings-do-match'.

\(fn COND &rest BODY)" nil t)

(function-put 'when-system-name-match 'lisp-indent-function '1)

(register-definition-prefixes "system-specific-settings" '("system-specific-settings-do-match"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; system-specific-settings-autoloads.el ends here
