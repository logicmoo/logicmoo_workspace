;;; vc-hgcmd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vc-hgcmd" "vc-hgcmd.el" (0 0 0 0))
;;; Generated autoloads from vc-hgcmd.el
 (defun vc-hgcmd-registered (file)
  (when (vc-find-root file ".hg")
    (load "vc-hgcmd" nil t)
    (vc-hgcmd-registered file)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vc-hgcmd" '("vc-hgcmd-")))

;;;***

;;;### (autoloads nil nil ("vc-hgcmd-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vc-hgcmd-autoloads.el ends here
