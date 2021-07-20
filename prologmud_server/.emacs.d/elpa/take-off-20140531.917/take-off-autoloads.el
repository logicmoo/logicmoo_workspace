;;; take-off-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "take-off" "take-off.el" (0 0 0 0))
;;; Generated autoloads from take-off.el

(autoload 'take-off-start "take-off" "\
Start a web server that allows remote web access to emacs.

\(fn PORT)" t nil)

(autoload 'take-off-stop "take-off" "\
Stop the web server." t nil)

(register-definition-prefixes "take-off" '("take-off-"))

;;;***

;;;### (autoloads nil nil ("take-off-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; take-off-autoloads.el ends here
