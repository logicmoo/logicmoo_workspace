;;; helm-xref-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-xref" "helm-xref.el" (0 0 0 0))
;;; Generated autoloads from helm-xref.el

(autoload 'helm-xref-show-xrefs "helm-xref" "\
Function to display XREFS.

Needs to be set the value of `xref-show-xrefs-function'.

\(fn XREFS ALIST)" nil nil)

(autoload 'helm-xref-show-xrefs-27 "helm-xref" "\
Function to display XREFS.

Needs to be set the value of `xref-show-xrefs-function'.

\(fn FETCHER ALIST)" nil nil)

(autoload 'helm-xref-show-defs-27 "helm-xref" "\
Function to display list of definitions.

\(fn FETCHER ALIST)" nil nil)

(if (< emacs-major-version 27) (setq xref-show-xrefs-function 'helm-xref-show-xrefs) (progn (setq xref-show-xrefs-function 'helm-xref-show-xrefs-27) (setq xref-show-definitions-function 'helm-xref-show-defs-27)))

(register-definition-prefixes "helm-xref" '("helm-xref-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-xref-autoloads.el ends here
