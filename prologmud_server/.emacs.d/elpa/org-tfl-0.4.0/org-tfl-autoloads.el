;;; org-tfl-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-tfl" "org-tfl.el" (0 0 0 0))
;;; Generated autoloads from org-tfl.el

(autoload 'org-tfl-jp "org-tfl" "\
Plan journey FROM TO VIA at DATETIME.

TIMEIS if t, DATETIME is the departing time.

\(fn FROM TO VIA DATETIME TIMEIS)" t nil)

(autoload 'org-tfl-jp-org "org-tfl" "\
Plan journey FROM TO VIA at DATETIME.

This creates a subheading and a link to update the result.
The leave of the subheading is the journey result.
TIMEIS if t, DATETIME is the departing time.

\(fn FROM TO VIA DATETIME TIMEIS)" t nil)

(register-definition-prefixes "org-tfl" '("org-t" "url-http-"))

;;;***

;;;### (autoloads nil nil ("org-tfl-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-tfl-autoloads.el ends here
