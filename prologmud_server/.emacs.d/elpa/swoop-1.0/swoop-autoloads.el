;;; swoop-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "swoop" "swoop.el" (0 0 0 0))
;;; Generated autoloads from swoop.el

(autoload 'swoop "swoop" "\
Search through words within the current buffer.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-multi "swoop" "\
Search words across currently opened multiple buffers.
Ignore non file buffer.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-pcre-regexp "swoop" "\
Use PCRE like regexp to swoop.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-migemo "swoop" "\
Japanese words matching with the alphabet.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-line-length-over80 "swoop" "\
Get over 80 colomn number linees." t nil)

(autoload 'swoop-from-isearch "swoop" "\
During isearch, switch over to swoop." t nil)

(autoload 'swoop-function "swoop" "\
Show function list in buffer judging from `major-mode' and regexp.
Currently `c-mode' only.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-from-evil-search "swoop" "\
During evil-search, switch over to swoop." t nil)

(register-definition-prefixes "swoop" '("swoop-"))

;;;***

;;;### (autoloads nil "swoop-edit" "swoop-edit.el" (0 0 0 0))
;;; Generated autoloads from swoop-edit.el

(register-definition-prefixes "swoop-edit" '("swoop-"))

;;;***

;;;### (autoloads nil "swoop-lib" "swoop-lib.el" (0 0 0 0))
;;; Generated autoloads from swoop-lib.el

(register-definition-prefixes "swoop-lib" '("swoop-"))

;;;***

;;;### (autoloads nil nil ("swoop-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; swoop-autoloads.el ends here
