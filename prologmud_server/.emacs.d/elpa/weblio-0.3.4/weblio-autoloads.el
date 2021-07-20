;;; weblio-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "weblio" "weblio.el" (0 0 0 0))
;;; Generated autoloads from weblio.el

(autoload 'weblio-lookup-region "weblio" "\
Look up selected region in weblio.jp.
Display the results in a fresh buffer, *weblio*

Argument START start of region.
Argument END end of region.

\(fn START END)" t nil)

(autoload 'weblio-lookup-word "weblio" "\
Look up WORD in the weblio.jp dictionary.

\(fn WORD)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; weblio-autoloads.el ends here
