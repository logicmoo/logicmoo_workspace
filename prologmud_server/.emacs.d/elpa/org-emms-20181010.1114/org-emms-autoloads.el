;;; org-emms-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-emms" "org-emms.el" (0 0 0 0))
;;; Generated autoloads from org-emms.el

(autoload 'org-emms-insert-link "org-emms" "\
Insert org link using completion.
Prompt for a file name and link description.  With a prefix ARG, prompt
for a track position.

\(fn ARG)" t nil)

(autoload 'org-emms-insert-track "org-emms" "\
Insert current selected track as an org link." t nil)

(autoload 'org-emms-insert-track-position "org-emms" "\
Insert current track position as an org link." t nil)

(register-definition-prefixes "org-emms" '("org-emms-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-emms-autoloads.el ends here
