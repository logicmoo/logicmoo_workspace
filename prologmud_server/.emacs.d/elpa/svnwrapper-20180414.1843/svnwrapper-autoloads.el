;;; svnwrapper-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "svnwrapper" "svnwrapper.el" (0 0 0 0))
;;; Generated autoloads from svnwrapper.el

(autoload 'svnwrapper-svn-status-mode "svnwrapper" "\
Major mode for the output of 'svn status'.

\(fn)" t nil)
(add-to-list 'auto-mode-alist
 '("\\.svnstatus\\'" . svnwrapper-svn-status-mode))

(register-definition-prefixes "svnwrapper" '("svnwrapper-svn-status-font-lock-keywords"))

;;;***

;;;### (autoloads nil nil ("svnwrapper-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; svnwrapper-autoloads.el ends here
