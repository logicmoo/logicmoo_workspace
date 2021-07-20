;;; wisp-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wisp-mode" "wisp-mode.el" (0 0 0 0))
;;; Generated autoloads from wisp-mode.el

(autoload 'wisp-mode "wisp-mode" "\
Major mode for whitespace-to-lisp files.

  \\{wisp-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.w\\'" . wisp-mode))

(add-hook 'wisp-mode-hook (lambda nil (setq electric-indent-inhibit t)))

(register-definition-prefixes "wisp-mode" '("wisp-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wisp-mode-autoloads.el ends here
