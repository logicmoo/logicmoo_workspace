;;; orgit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orgit" "orgit.el" (0 0 0 0))
;;; Generated autoloads from orgit.el

(autoload 'orgit-link-set-parameters "orgit" "\


\(fn TYPE &rest PARAMETERS)" nil nil)

(with-eval-after-load 'magit (define-key magit-mode-map [remap org-store-link] 'orgit-store-link))

(autoload 'orgit-store-link "orgit" "\
Like `org-store-link' but store links to all selected commits, if any.

\(fn ARG)" t nil)

(with-eval-after-load 'org (orgit-link-set-parameters "orgit" :store 'orgit-status-store :follow 'orgit-status-open :export 'orgit-status-export :complete 'orgit-status-complete-link))

(autoload 'orgit-status-store "orgit" "\
Store a link to a Magit-Status mode buffer.
When the region selects one or more commits, then do nothing.
In that case `orgit-rev-store' stores one or more links instead." nil nil)

(autoload 'orgit-status-open "orgit" "\


\(fn REPO)" nil nil)

(autoload 'orgit-status-export "orgit" "\


\(fn PATH DESC FORMAT)" nil nil)

(autoload 'orgit-status-complete-link "orgit" "\


\(fn &optional ARG)" nil nil)

(with-eval-after-load 'org (orgit-link-set-parameters "orgit-log" :store 'orgit-log-store :follow 'orgit-log-open :export 'orgit-log-export :complete 'orgit-log-complete-link))

(autoload 'orgit-log-store "orgit" "\
Store a link to a Magit-Log mode buffer.
When the region selects one or more commits, then do nothing.
In that case `orgit-rev-store' stores one or more links instead." nil nil)

(autoload 'orgit-log-open "orgit" "\


\(fn PATH)" nil nil)

(autoload 'orgit-log-export "orgit" "\


\(fn PATH DESC FORMAT)" nil nil)

(autoload 'orgit-log-complete-link "orgit" "\


\(fn &optional ARG)" nil nil)

(with-eval-after-load 'org (orgit-link-set-parameters "orgit-rev" :store 'orgit-rev-store :follow 'orgit-rev-open :export 'orgit-rev-export :complete 'orgit-rev-complete-link))

(autoload 'orgit-rev-store "orgit" "\
Store a link to a Magit-Revision mode buffer.
With a prefix argument instead store the name of a tag or branch
that points at the revision, if any.

If `orgit-store-reference' is non-nil, then the meaning of the
prefix argument is reversed.

When the region selects one or more commits, e.g. in a log, then
store links to the Magit-Revision mode buffers for these commits." nil nil)

(autoload 'orgit-rev-open "orgit" "\


\(fn PATH)" nil nil)

(autoload 'orgit-rev-export "orgit" "\


\(fn PATH DESC FORMAT)" nil nil)

(autoload 'orgit-rev-complete-link "orgit" "\


\(fn &optional ARG)" nil nil)

(register-definition-prefixes "orgit" '("orgit-"))

;;;***

;;;### (autoloads nil nil ("orgit-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orgit-autoloads.el ends here
