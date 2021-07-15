;;; lsp-javacomp-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-javacomp" "lsp-javacomp.el" (0 0 0 0))
;;; Generated autoloads from lsp-javacomp.el

(autoload 'lsp-javacomp-install-server "lsp-javacomp" "\
Download the JavaComp server JAR file if it does not exist.

If PROMPT-EXISTS is non-nil, show a message if the server jar
file already exists.

\(fn &optional PROMPT-EXISTS)" t nil)

(autoload 'lsp-javacomp-update-server "lsp-javacomp" "\
Update JavaComp jar file to the latest version." t nil)

(register-definition-prefixes "lsp-javacomp" '("lsp-javacomp-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-javacomp-autoloads.el ends here
