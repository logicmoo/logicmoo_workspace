;;; setup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "setup" "setup.el" (0 0 0 0))
;;; Generated autoloads from setup.el

(autoload 'setup-make-docstring "setup" "\
Return a docstring for `setup'.

\(fn)" nil nil)

(autoload 'setup "setup" "\
Configure feature or subsystem NAME.
BODY may contain special forms defined by `setup-define', but
will otherwise just be evaluated as is.
NAME may also be a macro, if it can provide a symbol.

\(fn NAME &rest BODY)" nil t)

(function-put 'setup 'lisp-indent-function 'defun)

(put 'setup 'function-documentation '(setup-make-docstring))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "setup" '("setup-")))

;;;***

;;;### (autoloads nil nil ("setup-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; setup-autoloads.el ends here
