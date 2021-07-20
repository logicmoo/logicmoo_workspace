;;; orglue-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orglue" "orglue.el" (0 0 0 0))
;;; Generated autoloads from orglue.el

(autoload 'orglue-decompose-last-org-bracket-link "orglue" "\
Find LINK-STRING backward and decompose it.
Decompose means that [[URL][DESCRIPTION]] is converted into
URL
DESCRIPTION
.

On conversion, URL is normalized by
``orglue-normalize-webpage-title'' and DESCRIPTION is
normalized by ``orglue-normalize-webpage-url''." t nil)

(autoload 'orglue-indent-rigidly-to-current-level "orglue" "\
Same with ``indent-rigidly'', if not in ``org-mode''.
Takes three arguments, START, END and ARG.
If in ``org-mode'' and ARG is 4 (called with universal-prefix),
adjust indent level best suited to org-style.

\(fn START END ARG)" t nil)

(autoload 'org-table-renumber "orglue" "\
Renumber current columns on org-table.
No effect if current columns contain any non-number chars." t nil)

(defalias 'orglue-evernote-insert-selected-note-as-org-links 'epic-insert-selected-note-as-org-links)

(autoload 'orglue-ns-insert-file "orglue" nil t nil)

(autoload 'orglue-ns-insert-text "orglue" nil t nil)

(autoload 'orglue-screencapture-and-insert "orglue" nil t nil)

(autoload 'orglue-headline-string "orglue" nil t nil)

(register-definition-prefixes "orglue" '("helm-c-source-org-projects" "orglue-"))

;;;***

;;;### (autoloads nil "orglue-publish" "orglue-publish.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from orglue-publish.el

(register-definition-prefixes "orglue-publish" '("orglue-"))

;;;***

;;;### (autoloads nil nil ("orglue-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orglue-autoloads.el ends here
