;;; sisu-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sisu-mode" "sisu-mode.el" (0 0 0 0))
;;; Generated autoloads from sisu-mode.el

(autoload 'sisu-mode "sisu-mode" "\
Major mode for editing SiSU files.
SiSU document structuring, publishing in multiple formats and search.
URL `http://www.sisudoc.org/'

\(fn)" t nil)
 (add-to-list 'auto-mode-alist '("\\.ss[imt]\\'" . sisu-mode))

(register-definition-prefixes "sisu-mode" '("sisu-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sisu-mode-autoloads.el ends here
