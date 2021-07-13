;;; liberime-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "liberime" "liberime.el" (0 0 0 0))
;;; Generated autoloads from liberime.el

(autoload 'liberime-open-user-data-dir "liberime" "\
Open user data dir with external app." t nil)

(autoload 'liberime-open-shared-data-dir "liberime" "\
Open shared data dir with external app." t nil)

(autoload 'liberime-open-package-directory "liberime" "\
Open liberime library directory with external app." t nil)

(autoload 'liberime-open-package-readme "liberime" "\
Open liberime library README.org." t nil)

(autoload 'liberime-build "liberime" "\
Build liberime-core module." t nil)

(autoload 'liberime-load "liberime" "\
Load liberime-core module." t nil)

(autoload 'liberime-deploy "liberime" "\
Deploy liberime to affect config file change." t nil)

(autoload 'liberime-set-page-size "liberime" "\
Set rime page-size to PAGE-SIZE or by default 10.
you also need to call `liberime-deploy' to make it take affect
you only need to do this once.

\(fn PAGE-SIZE)" t nil)

(autoload 'liberime-select-schema-interactive "liberime" "\
Select a rime schema interactive." t nil)

(autoload 'liberime-sync "liberime" "\
Sync rime user data.
User should specify sync_dir in installation.yaml file of
`liberime-user-data-dir' directory." t nil)

(register-definition-prefixes "liberime" '("liberime-"))

;;;***

;;;### (autoloads nil nil ("liberime-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; liberime-autoloads.el ends here
