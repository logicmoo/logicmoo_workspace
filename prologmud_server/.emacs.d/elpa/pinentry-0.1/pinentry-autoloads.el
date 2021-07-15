;;; pinentry-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pinentry" "pinentry.el" (0 0 0 0))
;;; Generated autoloads from pinentry.el

(autoload 'pinentry-start "pinentry" "\
Start a Pinentry service.

Once the environment is properly set, subsequent invocations of
the gpg command will interact with Emacs for passphrase input.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pinentry" '("pinentry-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pinentry-autoloads.el ends here
