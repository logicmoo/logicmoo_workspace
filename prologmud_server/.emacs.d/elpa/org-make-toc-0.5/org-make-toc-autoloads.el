;;; org-make-toc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-make-toc" "org-make-toc.el" (0 0 0 0))
;;; Generated autoloads from org-make-toc.el

(autoload 'org-make-toc "org-make-toc" "\
Make or update table of contents in current buffer." t nil)

(autoload 'org-make-toc-at-point "org-make-toc" "\
Make or update table of contents at current entry." t nil)

(autoload 'org-make-toc-insert "org-make-toc" "\
Insert \":CONTENTS:\" drawer at point." t nil)

(autoload 'org-make-toc-set "org-make-toc" "\
Set TOC PROPERTIES of entry at point.

\(fn PROPERTIES)" t nil)

(autoload 'org-make-toc-mode "org-make-toc" "\
Add the `org-make-toc' command to the `before-save-hook' in the current Org buffer.
With prefix argument ARG, turn on if positive, otherwise off.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-make-toc" '("org-make-toc-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-make-toc-autoloads.el ends here
