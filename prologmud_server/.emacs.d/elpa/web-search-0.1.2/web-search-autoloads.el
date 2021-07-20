;;; web-search-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "web-search" "web-search.el" (0 0 0 0))
;;; Generated autoloads from web-search.el

(autoload 'web-search "web-search" "\
Search for QUERY on website(s).

Without prefix argument, search on `web-search-default-provider'.
One C-u, choose a provider.
Two C-u, choose a tag (notes that a tag can match multiple providers).

\(fn QUERY &optional PROVIDERS TAG)" t nil)

(register-definition-prefixes "web-search" '("web-search-"))

;;;***

;;;### (autoloads nil nil ("web-search-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; web-search-autoloads.el ends here
