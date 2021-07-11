;;; ebdb-gnorb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ebdb-gnorb" "ebdb-gnorb.el" (0 0 0 0))
;;; Generated autoloads from ebdb-gnorb.el

(cl-defstruct gnorb-ebdb-link subject date group id)

(eieio-defclass-autoload 'gnorb-ebdb-field-messages '(ebdb-field-user) "ebdb-gnorb" :human-readable)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ebdb-gnorb" '("ebdb-gnorb-lapsed-days" "gnorb-ebdb-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ebdb-gnorb-autoloads.el ends here
