;;; lsp-p4.el --- P4 support for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Dmitri Makarov

;; Author: Dmitri Makarov
;; Version: 1.0
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: lsp, p4
;; URL: https://github.com/dmakarov/p4ls

;;; Commentary:

;;; To enable lsp-p4 include the following lisp code in init.el after
;;; loading lsp-mode
;;;
;;;    (with-eval-after-load 'lsp-mode
;;;      (require 'p4lang-mode)
;;;      (require 'lsp-p4)
;;;      (add-hook 'p4lang-mode-hook #'lsp)
;;;
;;; See `lsp-clients-p4lsd-executable' to customize the path to p4lsd.

;;; Code:

(require 'lsp-mode)

(defcustom lsp-clients-p4lsd-executable "p4lsd"
  "The p4lsd executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'."
  :group 'lsp-p4
  :risky t
  :type 'file)

(defcustom lsp-clients-p4lsd-args '()
  "Extra arguments for the p4lsd executable."
  :group 'lsp-p4
  :risky t
  :type '(repeat string))

(defun lsp-clients--p4lsd-command ()
  "Generate the language server startup command."
  `(,lsp-clients-p4lsd-executable ,@lsp-clients-p4lsd-args))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   'lsp-clients--p4lsd-command)
                  :major-modes '(p4lang-mode)
                  :priority -1
                  :server-id 'p4lsd))

(provide 'lsp-p4)

;;; lsp-p4.el ends here
