;;; lex-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lex" "lex.el" (0 0 0 0))
;;; Generated autoloads from lex.el

(register-definition-prefixes "lex" '("copy-char-table" "lex-"))

;;;***

;;;### (autoloads nil "lex-parse-re" "lex-parse-re.el" (0 0 0 0))
;;; Generated autoloads from lex-parse-re.el

(autoload 'lex-parse-re "lex-parse-re" "\
Parse STRING as a regular expression.
LEXER specifies the regexp syntax to use.  It can be `ere', or `bre'
and it defaults to `bre'.

\(fn STRING &optional LEXER)" nil nil)

(register-definition-prefixes "lex-parse-re" '("lex--"))

;;;***

;;;### (autoloads nil nil ("lex-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lex-autoloads.el ends here
