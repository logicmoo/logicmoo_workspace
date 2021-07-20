;;; orgit-forge-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orgit-forge" "orgit-forge.el" (0 0 0 0))
;;; Generated autoloads from orgit-forge.el

(with-eval-after-load "org" (orgit-link-set-parameters "orgit-topic" :store 'orgit-topic-store :follow 'orgit-topic-open :export 'orgit-topic-export :complete 'orgit-topic-complete-link))

(autoload 'orgit-topic-store "orgit-forge" "\
Store a link to a Forge-Topic mode buffer.

When the region selects a topic, then store a link to the
Forge-Topic mode buffer for that topic." nil nil)

(autoload 'orgit-topic-open "orgit-forge" "\


\(fn ID)" nil nil)

(autoload 'orgit-topic-export "orgit-forge" "\


\(fn ID DESC FORMAT)" nil nil)

(autoload 'orgit-topic-complete-link "orgit-forge" "\


\(fn &optional ARG)" nil nil)

(register-definition-prefixes "orgit-forge" '("orgit-topic-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orgit-forge-autoloads.el ends here
