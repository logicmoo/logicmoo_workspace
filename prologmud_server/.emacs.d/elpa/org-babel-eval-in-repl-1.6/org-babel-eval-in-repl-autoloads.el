;;; org-babel-eval-in-repl-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eval-in-repl-ess" "eval-in-repl-ess.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from eval-in-repl-ess.el

(register-definition-prefixes "eval-in-repl-ess" '("ober-"))

;;;***

;;;### (autoloads nil "eval-in-repl-matlab" "eval-in-repl-matlab.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-matlab.el

(register-definition-prefixes "eval-in-repl-matlab" '("ober-eval-matlab"))

;;;***

;;;### (autoloads nil "org-babel-eval-in-repl" "org-babel-eval-in-repl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-babel-eval-in-repl.el

(autoload 'ober-eval-in-repl "org-babel-eval-in-repl" "\
Execute source code in a REPL. (The range to execute is determined by `eval-in-repl'.)" t nil)

(autoload 'ober-eval-block-in-repl "org-babel-eval-in-repl" "\
Execute source code in a REPL. (The whole content in the block is evaluated)" t nil)

(register-definition-prefixes "org-babel-eval-in-repl" '("ober-"))

;;;***

;;;### (autoloads nil nil ("org-babel-eval-in-repl-pkg.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-babel-eval-in-repl-autoloads.el ends here
