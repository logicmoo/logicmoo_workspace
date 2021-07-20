;;; org-generate-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-generate" "org-generate.el" (0 0 0 0))
;;; Generated autoloads from org-generate.el

(autoload 'org-generate-edit "org-generate" "\
Open `org-generate-file'." t nil)

(autoload 'org-generate "org-generate" "\
Gerenate files from org document using TARGET definition.

\(fn TARGET)" t nil)

(register-definition-prefixes "org-generate" '("org-generate-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-generate-autoloads.el ends here
