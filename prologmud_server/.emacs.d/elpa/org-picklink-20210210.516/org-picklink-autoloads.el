;;; org-picklink-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-picklink" "org-picklink.el" (0 0 0 0))
;;; Generated autoloads from org-picklink.el

(defvar org-picklink-links nil "\
Record all links info.")

(autoload 'org-picklink-store-link "org-picklink" "\
Store id link of current headline.

If BREADCRUMBS is t, breadcurmbs will be included in link
description.

\(fn &optional BREADCRUMBS)" t nil)

(autoload 'org-picklink-quit-window "org-picklink" "\
Quit org agenda window and insert links to org mode buffer." t nil)

(autoload 'org-picklink-store-link-and-quit-window "org-picklink" "\
Store link then quit org agenda window." t nil)

(autoload 'org-picklink "org-picklink" "\
Open org agenda window as a link selector.

if region is actived, ‘org-agenda’ will search string
in region and replace it with selected link.

When SEARCH-TAG is t, use `org-tags-view' instead
of `org-search-view'.

This command only useful in org mode buffer.

\(fn &optional SEARCH-TAG)" t nil)

(register-definition-prefixes "org-picklink" '("org-picklink-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-picklink-autoloads.el ends here
