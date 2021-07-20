;;; org-redmine-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-redmine" "org-redmine.el" (0 0 0 0))
;;; Generated autoloads from org-redmine.el

(autoload 'org-redmine-get-issue "org-redmine" "\


\(fn ISSUE-ID)" t nil)

(autoload 'org-redmine-anything-show-issue-all "org-redmine" "\
Display recent issues using `anything'

\(fn &optional ME)" t nil)

(autoload 'org-redmine-helm-show-issue-all "org-redmine" "\
Display recent issues using `helm'

\(fn &optional ME)" t nil)

(register-definition-prefixes "org-redmine" '("org-redmine-" "orutil-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-redmine-autoloads.el ends here
