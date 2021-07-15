;;; org-edna-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-edna" "org-edna.el" (0 0 0 0))
;;; Generated autoloads from org-edna.el

(autoload 'org-edna--load "org-edna" "\
Setup the hooks necessary for Org Edna to run.

This means adding to `org-trigger-hook' and `org-blocker-hook'.

\(fn)" nil nil)

(autoload 'org-edna--unload "org-edna" "\
Unload Org Edna.

Remove Edna's workers from `org-trigger-hook' and
`org-blocker-hook'.

\(fn)" nil nil)

(defvar org-edna-mode nil "\
Non-nil if Org-Edna mode is enabled.
See the `org-edna-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-edna-mode'.")

(custom-autoload 'org-edna-mode "org-edna" nil)

(autoload 'org-edna-mode "org-edna" "\
Toggle Org Edna mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-edna" '("org-e")))

;;;***

;;;### (autoloads nil nil ("org-edna-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-edna-autoloads.el ends here
