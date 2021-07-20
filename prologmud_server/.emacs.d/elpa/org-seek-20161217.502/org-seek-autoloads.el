;;; org-seek-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-seek" "org-seek.el" (0 0 0 0))
;;; Generated autoloads from org-seek.el

(autoload 'org-seek-string "org-seek" "\
Full context searching STRING using ag in a given DIRECTORY.

By default STRING is the symbol under point unless called with a
prefix, prompts for flags to pass to ag.

\(fn STRING DIRECTORY)" t nil)

(autoload 'org-seek-regexp "org-seek" "\
Full context searching REGEXP using ag in a given DIRECTORY.

By default REGEXP is the symbol under point unless called with a
prefix, prompts for flags to pass to ag.

\(fn REGEXP DIRECTORY)" t nil)

(autoload 'org-seek-headlines "org-seek" "\
Search STRING in Org files headlines using ag in a given DIRECTORY.

By default STRING is the symbol under point unless called with a
prefix, prompts for flags to pass to ag.

\(fn STRING DIRECTORY)" t nil)

(register-definition-prefixes "org-seek" '("org-seek-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-seek-autoloads.el ends here
