;;; org-parser-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-parser" "org-parser.el" (0 0 0 0))
;;; Generated autoloads from org-parser.el

(autoload 'org-parser-parse-buffer "org-parser" "\
Parse BUFFER into a list of structure items.

\(fn BUFFER)" nil nil)

(autoload 'org-parser-parse-file "org-parser" "\
Parse FILENAME into a list of structure items.

\(fn FILENAME)" nil nil)

(autoload 'org-parser-parse-string "org-parser" "\
Parse STRING into a list of structure items.

\(fn STRING)" nil nil)

(register-definition-prefixes "org-parser" '("org-parser--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-parser-autoloads.el ends here
