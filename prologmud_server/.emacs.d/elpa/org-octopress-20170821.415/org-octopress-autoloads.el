;;; org-octopress-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-octopress" "org-octopress.el" (0 0 0 0))
;;; Generated autoloads from org-octopress.el

(register-definition-prefixes "org-octopress" '("org-octopress"))

;;;***

;;;### (autoloads nil "ox-jekyll" "ox-jekyll.el" (0 0 0 0))
;;; Generated autoloads from ox-jekyll.el

(autoload 'org-jekyll-export-as-html "ox-jekyll" "\
Export current buffer to a HTML buffer adding some YAML front matter.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-jekyll-export-to-html "ox-jekyll" "\
Export current buffer to a HTML file adding some YAML front matter.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-jekyll-publish-to-html "ox-jekyll" "\
Publish an org file to HTML with YAML front matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(autoload 'org-jekyll-insert-export-options-template "ox-jekyll" "\
Insert a settings template for Jekyll exporter.

\(fn &optional TITLE DATE SETUPFILE CATEGORIES TAGS PUBLISHED LAYOUT)" t nil)

(register-definition-prefixes "ox-jekyll" '("org-jekyll-"))

;;;***

;;;### (autoloads nil nil ("org-octopress-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-octopress-autoloads.el ends here
