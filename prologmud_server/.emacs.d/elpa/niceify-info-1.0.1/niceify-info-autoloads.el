;;; niceify-info-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "niceify-info" "niceify-info.el" (0 0 0 0))
;;; Generated autoloads from niceify-info.el

(autoload 'niceify-info "niceify-info" "\
Apply niceification functions to Info buffers.

This function is intended to be called from
`Info-selection-hook', q.v., but can be safely evaluated by hand
in an Info buffer as well." t nil)

(register-definition-prefixes "niceify-info" '("niceify-info"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; niceify-info-autoloads.el ends here
