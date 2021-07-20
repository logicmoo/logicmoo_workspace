;;; org2web-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org2web" "org2web.el" (0 0 0 0))
;;; Generated autoloads from org2web.el

(autoload 'org2web-add-project "org2web" "\
Add `project-config' to `org2web-projects'

\(fn PROJECT-CONFIG)" nil nil)

(autoload 'org2web-select-project "org2web" "\
Let user select a project then return its name.

\(fn PROMPT &optional PROJECT-NAME)" nil nil)

(autoload 'org2web-publish "org2web" "\


\(fn &optional PROJECT-NAME PUBLISHING-DIRECTORY JOB-NUMBER UPDATE-TOP-N)" t nil)

(autoload 'org2web-new-post "org2web" "\
Setup a new post.

PROJECT-NAME: which project do you want to export
CATEGORY:     this post belongs to
FILENAME:     the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid.

\(fn &optional PROJECT-NAME CATEGORY FILENAME INSERT-FALLBACK-TEMPLATE)" t nil)

(register-definition-prefixes "org2web" '("org2web-"))

;;;***

;;;### (autoloads nil "org2web-config" "org2web-config.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org2web-config.el

(register-definition-prefixes "org2web-config" '("org2web-get-"))

;;;***

;;;### (autoloads nil "org2web-devtools" "org2web-devtools.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org2web-devtools.el

(register-definition-prefixes "org2web-devtools" '("org2web-devtools-repository-directory"))

;;;***

;;;### (autoloads nil "org2web-el2org" "org2web-el2org.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org2web-el2org.el

(register-definition-prefixes "org2web-el2org" '("org2web-el2org-"))

;;;***

;;;### (autoloads nil "org2web-export" "org2web-export.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org2web-export.el

(register-definition-prefixes "org2web-export" '("org2web-"))

;;;***

;;;### (autoloads nil "org2web-resource" "org2web-resource.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org2web-resource.el

(register-definition-prefixes "org2web-resource" '("org2web-prepare-theme-resources"))

;;;***

;;;### (autoloads nil "org2web-template" "org2web-template.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org2web-template.el

(register-definition-prefixes "org2web-template" '("org2web-"))

;;;***

;;;### (autoloads nil "org2web-util" "org2web-util.el" (0 0 0 0))
;;; Generated autoloads from org2web-util.el

(register-definition-prefixes "org2web-util" '("org2web-"))

;;;***

;;;### (autoloads nil "org2web-vars" "org2web-vars.el" (0 0 0 0))
;;; Generated autoloads from org2web-vars.el

(register-definition-prefixes "org2web-vars" '("org2web-"))

;;;***

;;;### (autoloads nil "org2web-webserver" "org2web-webserver.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org2web-webserver.el

(register-definition-prefixes "org2web-webserver" '("org2web-webserver-"))

;;;***

;;;### (autoloads nil nil ("org2web-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org2web-autoloads.el ends here
