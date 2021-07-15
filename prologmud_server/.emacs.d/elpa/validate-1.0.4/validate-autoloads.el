;;; validate-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "validate" "validate.el" (0 0 0 0))
;;; Generated autoloads from validate.el

(autoload 'validate-value "validate" "\
Check that VALUE matches SCHEMA.
If it matches return VALUE, otherwise signal a `user-error'.

If NOERROR is non-nil, return t to indicate a match and nil to
indicate a failure.

\(fn VALUE SCHEMA &optional NOERROR)" nil nil)

(autoload 'validate-variable "validate" "\
Check that SYMBOL's value matches its schema.
SYMBOL must be the name of a custom option with a defined
`custom-type'. If SYMBOL has a value and a type, they are checked
with `validate-value'. NOERROR is passed to `validate-value'.

\(fn SYMBOL &optional NOERROR)" nil nil)

(autoload 'validate-mark-safe-local "validate" "\
Mark SYMBOL as a safe local if its custom type is obeyed.

\(fn SYMBOL)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "validate" '("validate-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; validate-autoloads.el ends here
