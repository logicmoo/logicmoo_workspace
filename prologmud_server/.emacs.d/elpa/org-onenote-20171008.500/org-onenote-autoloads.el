;;; org-onenote-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-onenote" "org-onenote.el" (0 0 0 0))
;;; Generated autoloads from org-onenote.el

(autoload 'org-onenote-start-authenticate "org-onenote" "\
Start OAuth2 authenticate." t nil)

(autoload 'org-onenote-insert-section-map-at-pt "org-onenote" "\
Insert section map at current point.  You may use it as your config." t nil)

(autoload 'org-onenote-submit-page "org-onenote" "\
Submit it." t nil)

(autoload 'org-onenote-delete-page "org-onenote" "\
Delete page from server by optional ID, default current page.

\(fn &optional ID)" t nil)

(register-definition-prefixes "org-onenote" '("org-onenote-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-onenote-autoloads.el ends here
