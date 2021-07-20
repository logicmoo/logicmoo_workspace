;;; zop-to-char-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zop-to-char" "zop-to-char.el" (0 0 0 0))
;;; Generated autoloads from zop-to-char.el

(autoload 'zop-to-char "zop-to-char" "\
An enhanced version of `zap-to-char'.

Argument ARG, when given is index of occurrence to jump to.  For
example, if ARG is 2, `zop-to-char' will jump to second occurrence
of given character.  If ARG is negative, jump in backward direction.

\(fn ARG)" t nil)

(autoload 'zop-up-to-char "zop-to-char" "\
An enhanced version of `zap-up-to-char'.

Argument ARG, when given is index of occurrence to jump to.  For
example, if ARG is 2, `zop-up-to-char' will jump to second
occurrence of given character.  If ARG is negative, jump in
backward direction.

\(fn ARG)" t nil)

(register-definition-prefixes "zop-to-char" '("zop-to-char-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zop-to-char-autoloads.el ends here
