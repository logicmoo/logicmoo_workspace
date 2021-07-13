;;; lsp-sourcekit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-sourcekit" "lsp-sourcekit.el" (0 0 0 0))
;;; Generated autoloads from lsp-sourcekit.el

(autoload 'lsp-sourcekit--find-executable-with-xcrun "lsp-sourcekit" "\
sourcekit-lsp may be installed behind xcrun; if we can't find
the `lsp-sourcekit-executable' on PATH, try it with xcrun." nil nil)

(with-eval-after-load 'lsp-mode (lsp-dependency 'sourcekit-lsp (list :system 'lsp-sourcekit-executable) (list :system #'lsp-sourcekit--find-executable-with-xcrun)) (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection (apply-partially #'lsp-package-path 'sourcekit-lsp)) :major-modes '(swift-mode) :server-id 'sourcekit-ls)))

(register-definition-prefixes "lsp-sourcekit" '("lsp-sourcekit-ex"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-sourcekit-autoloads.el ends here
