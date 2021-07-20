;;; use-package-hydra-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "use-package-hydra" "use-package-hydra.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-hydra.el

(defalias 'use-package-normalize/:hydra 'use-package-hydra--normalize "\
Normalize for the definition of one or more hydras.")

(autoload 'use-package-handler/:hydra "use-package-hydra" "\
Generate defhydra with NAME for `:hydra' KEYWORD.
ARGS, REST, and STATE are prepared by `use-package-normalize/:hydra'.

\(fn NAME KEYWORD ARGS REST STATE)" nil nil)

(register-definition-prefixes "use-package-hydra" '("use-package-hydra--n"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; use-package-hydra-autoloads.el ends here
