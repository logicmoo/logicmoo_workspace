;;; searcher-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "searcher" "searcher.el" (0 0 0 0))
;;; Generated autoloads from searcher.el

(autoload 'searcher-search-in-project "searcher" "\
Search STR-OR-REGEX from the root of project directory.

\(fn STR-OR-REGEX)" nil nil)

(autoload 'searcher-search-in-path "searcher" "\
Search STR-OR-REGEX from PATH.

\(fn PATH STR-OR-REGEX)" nil nil)

(autoload 'searcher-search-in-file "searcher" "\
Search STR-OR-REGEX in FILE.

\(fn FILE STR-OR-REGEX)" nil nil)

(register-definition-prefixes "searcher" '("searcher-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; searcher-autoloads.el ends here
