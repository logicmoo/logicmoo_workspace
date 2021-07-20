;;; nexus-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-nexus" "ac-nexus.el" (0 0 0 0))
;;; Generated autoloads from ac-nexus.el

(autoload 'ac-source-lein-set-up "ac-nexus" "\
Add an nexus completion source to the front of `ac-sources' for the current buffer,
if the current `(buffer-filename)' = \"project.clj\"" t nil)

(register-definition-prefixes "ac-nexus" '("ac-" "nexus-ac-artifact-prefix-length"))

;;;***

;;;### (autoloads nil "nexus" "nexus.el" (0 0 0 0))
;;; Generated autoloads from nexus.el

(register-definition-prefixes "nexus" '("nexus-"))

;;;***

;;;### (autoloads nil "nexus-widget" "nexus-widget.el" (0 0 0 0))
;;; Generated autoloads from nexus-widget.el

(register-definition-prefixes "nexus-widget" '("nexus-widget-"))

;;;***

;;;### (autoloads nil nil ("nexus-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nexus-autoloads.el ends here
