;;; org-dashboard-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-dashboard" "org-dashboard.el" (0 0 0 0))
;;; Generated autoloads from org-dashboard.el

(autoload 'org-dashboard-display "org-dashboard" nil t nil)

(autoload 'org-dblock-write:block-dashboard "org-dashboard" "\
Generate a progress report inside an org dynamic block.

Progress information is retrieved by searching files in
`org-dashboard-files' for headings containing a \"progress cookie\",
e.g.:

  ** [50%] release v0.1
  *** TODO publish on github
  *** DONE import in git

See Info node `(org) Breaking down tasks'.

\(fn PARAMS)" nil nil)

(register-definition-prefixes "org-dashboard" '("org-dashboard-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-dashboard-autoloads.el ends here
