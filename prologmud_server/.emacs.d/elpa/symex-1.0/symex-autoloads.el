;;; symex-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "symex" "symex.el" (0 0 0 0))
;;; Generated autoloads from symex.el

(autoload 'symex-mode "symex" "\
An evil way to edit Lisp symbolic expressions as trees.

This is a minor mode.  If called interactively, toggle the `symex
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `symex-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'symex-initialize "symex" "\
Initialize symex mode.

This registers symex mode for use in all recognized Lisp modes, and also
advises functions to enable or disable features based on user configuration." nil nil)

(autoload 'symex-mode-interface "symex" "\
The main entry point for editing symbolic expressions using symex mode.

Enter the symex evil state and show a hydra menu for accessing various
features." t nil)

(register-definition-prefixes "symex" '("symex-"))

;;;***

;;;### (autoloads nil "symex-computations" "symex-computations.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-computations.el

(register-definition-prefixes "symex-computations" '("symex-"))

;;;***

;;;### (autoloads nil "symex-custom" "symex-custom.el" (0 0 0 0))
;;; Generated autoloads from symex-custom.el

(register-definition-prefixes "symex-custom" '("symex-"))

;;;***

;;;### (autoloads nil "symex-data" "symex-data.el" (0 0 0 0))
;;; Generated autoloads from symex-data.el

(register-definition-prefixes "symex-data" '("symex-"))

;;;***

;;;### (autoloads nil "symex-dsl" "symex-dsl.el" (0 0 0 0))
;;; Generated autoloads from symex-dsl.el

(register-definition-prefixes "symex-dsl" '("deftraversal" "symex-"))

;;;***

;;;### (autoloads nil "symex-evaluator" "symex-evaluator.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from symex-evaluator.el

(register-definition-prefixes "symex-evaluator" '("symex-"))

;;;***

;;;### (autoloads nil "symex-evil" "symex-evil.el" (0 0 0 0))
;;; Generated autoloads from symex-evil.el

(register-definition-prefixes "symex-evil" '("symex-"))

;;;***

;;;### (autoloads nil "symex-evil-support" "symex-evil-support.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-evil-support.el

(register-definition-prefixes "symex-evil-support" '("symex--define-evil-key"))

;;;***

;;;### (autoloads nil "symex-hydra" "symex-hydra.el" (0 0 0 0))
;;; Generated autoloads from symex-hydra.el

(register-definition-prefixes "symex-hydra" '("hydra-symex" "symex-"))

;;;***

;;;### (autoloads nil "symex-interface-arc" "symex-interface-arc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-arc.el

(register-definition-prefixes "symex-interface-arc" '("symex-"))

;;;***

;;;### (autoloads nil "symex-interface-clojure" "symex-interface-clojure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-clojure.el

(register-definition-prefixes "symex-interface-clojure" '("symex-"))

;;;***

;;;### (autoloads nil "symex-interface-common-lisp" "symex-interface-common-lisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-common-lisp.el

(register-definition-prefixes "symex-interface-common-lisp" '("symex-"))

;;;***

;;;### (autoloads nil "symex-interface-elisp" "symex-interface-elisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-elisp.el

(register-definition-prefixes "symex-interface-elisp" '("symex-"))

;;;***

;;;### (autoloads nil "symex-interface-racket" "symex-interface-racket.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-racket.el

(register-definition-prefixes "symex-interface-racket" '("symex-"))

;;;***

;;;### (autoloads nil "symex-interface-scheme" "symex-interface-scheme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-scheme.el

(register-definition-prefixes "symex-interface-scheme" '("symex-"))

;;;***

;;;### (autoloads nil "symex-interop" "symex-interop.el" (0 0 0 0))
;;; Generated autoloads from symex-interop.el

(register-definition-prefixes "symex-interop" '("symex-"))

;;;***

;;;### (autoloads nil "symex-misc" "symex-misc.el" (0 0 0 0))
;;; Generated autoloads from symex-misc.el

(register-definition-prefixes "symex-misc" '("symex-"))

;;;***

;;;### (autoloads nil "symex-primitives" "symex-primitives.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from symex-primitives.el

(register-definition-prefixes "symex-primitives" '("symex-"))

;;;***

;;;### (autoloads nil "symex-transformations" "symex-transformations.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-transformations.el

(register-definition-prefixes "symex-transformations" '("symex-"))

;;;***

;;;### (autoloads nil "symex-traversals" "symex-traversals.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from symex-traversals.el

(register-definition-prefixes "symex-traversals" '("symex-"))

;;;***

;;;### (autoloads nil "symex-ui" "symex-ui.el" (0 0 0 0))
;;; Generated autoloads from symex-ui.el

(register-definition-prefixes "symex-ui" '("symex--toggle-highlight"))

;;;***

;;;### (autoloads nil "symex-utils" "symex-utils.el" (0 0 0 0))
;;; Generated autoloads from symex-utils.el

(register-definition-prefixes "symex-utils" '("symex--"))

;;;***

;;;### (autoloads nil nil ("symex-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; symex-autoloads.el ends here
