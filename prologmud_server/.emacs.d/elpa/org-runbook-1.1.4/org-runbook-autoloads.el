;;; org-runbook-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-runbook" "org-runbook.el" (0 0 0 0))
;;; Generated autoloads from org-runbook.el

(autoload 'org-runbook-execute "org-runbook" "\
Prompt for command completion and execute the selected command." t nil)

(autoload 'org-runbook-view "org-runbook" "\
Prompt for command completion and view the selected command." t nil)

(autoload 'org-runbook-goto "org-runbook" "\
Prompt for command completion and goto the selected command's location." t nil)

(autoload 'org-runbook-repeat "org-runbook" "\
Repeat the last command for the current projectile project.

Use `default-directory' if projectile is unavailable." t nil)

(autoload 'org-runbook-targets "org-runbook" "\
Return the runbook commands corresponding to the current buffer." nil nil)

(autoload 'org-runbook-switch-to-major-mode-file "org-runbook" "\
Switch current buffer to the file corresponding to the current buffer's major mode." t nil)

(autoload 'org-runbook-switch-to-projectile-file "org-runbook" "\
Switch current buffer to the file corresponding to the current buffer's projectile mode." t nil)

(autoload 'org-runbook-capture-target-major-mode-file "org-runbook" "\
Switch current buffer to the file corresponding to the current buffer's major mode." nil nil)

(autoload 'org-runbook-capture-target-projectile-file "org-runbook" "\
Target for appending at the end of the runbook corresponding to the current buffer's projectile project." nil nil)

(register-definition-prefixes "org-runbook" '("org-runbook-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-runbook-autoloads.el ends here
