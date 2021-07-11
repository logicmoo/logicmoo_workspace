;;; load-dir-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "load-dir" "load-dir.el" (0 0 0 0))
;;; Generated autoloads from load-dir.el

(autoload 'load-dirs "load-dir" "\
Load all Emacs Lisp files in `load-dirs'.
Will not load a file twice (use `load-dir-reload' for that).
Recurses into subdirectories if `load-dir-recursive' is t.

\(fn)" t nil)

(autoload 'load-dirs-reload "load-dir" "\
Load all Emacs Lisp files in `load-dirs'.
Clears the list of loaded files and just calls `load-dir-load'.

\(fn)" t nil)

(add-hook 'after-init-hook 'load-dirs)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "load-dir" '("load-dir")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; load-dir-autoloads.el ends here
