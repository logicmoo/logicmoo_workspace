;;; symbolist-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "symbolist" "symbolist.el" (0 0 0 0))
;;; Generated autoloads from symbolist.el

(autoload 'symbolist-regexp "symbolist" "\
Show a buffer listing all Emacs Lisp symbols that match REGEXP.

Symbol matching respects the value of `case-fold-search'.

\(fn REGEXP)" t nil)

(autoload 'symbolist-prefix "symbolist" "\
Show a buffer listing all Emacs Lisp symbols that start with PREFIX.

Symbol matching respects the value of `case-fold-search'.

\(fn PREFIX)" t nil)

(register-definition-prefixes "symbolist" '("symbolist-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; symbolist-autoloads.el ends here
