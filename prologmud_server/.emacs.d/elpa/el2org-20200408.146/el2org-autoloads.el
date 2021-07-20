;;; el2org-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "el2org" "el2org.el" (0 0 0 0))
;;; Generated autoloads from el2org.el

(autoload 'el2org-generate-readme "el2org" "\
Generate README from current emacs-lisp file.

If BACKEND is set then use-it else use `el2org-default-backend'.
If FILE-EXT is nil deduce it from BACKEND.

\(fn &optional BACKEND FILE-EXT)" t nil)

(autoload 'el2org-generate-html "el2org" "\
Generate html file from current elisp file and browse it." t nil)

(autoload 'el2org-generate-org "el2org" "\
Generate org file from current elisp file." t nil)

(register-definition-prefixes "el2org" '("el2org-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; el2org-autoloads.el ends here
