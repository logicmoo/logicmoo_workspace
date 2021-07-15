;;; srv-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "srv" "srv.el" (0 0 0 0))
;;; Generated autoloads from srv.el

(autoload 'srv-lookup "srv" "\
Perform SRV lookup of TARGET and return list of connection candidiates.
TARGET is a string of the form \"_Service._Proto.Name\".

Returns a list with elements of the form (HOST . PORT), where HOST is
a hostname and PORT is a numeric port.  The caller is supposed to
make connection attempts in the order given, starting from the beginning
of the list.  The list is empty if no SRV records were found.

\(fn TARGET)" nil nil)

(register-definition-prefixes "srv" '("srv--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; srv-autoloads.el ends here
