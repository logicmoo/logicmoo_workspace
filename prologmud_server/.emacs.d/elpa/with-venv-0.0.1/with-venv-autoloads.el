;;; with-venv-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "with-venv" "with-venv.el" (0 0 0 0))
;;; Generated autoloads from with-venv.el

(autoload 'with-venv-dir "with-venv" "\
Set python environment to DIR and execute BODY.

This macro does not check if DIR is a valid python environemnt.
If dir is nil, execute BODY as usual.

\(fn DIR &rest BODY)" nil t)

(function-put 'with-venv-dir 'lisp-indent-function '1)

(autoload 'with-venv "with-venv" "\
Execute BODY with venv enabled.

This function tries to find suitable venv dir, or run BODY as usual when no
suitable environment was found.

\(fn &rest BODY)" nil t)

(function-put 'with-venv 'lisp-indent-function '0)

(autoload 'with-venv-advice-add "with-venv" "\
Setup advice so that FUNC use `with-env' macro when executing.

\(fn FUNC)" nil nil)

(autoload 'with-venv-advice-remove "with-venv" "\
Remove advice FUNC added by `with-venv-advice-add'.

\(fn FUNC)" nil nil)

(register-definition-prefixes "with-venv" '("with-venv-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; with-venv-autoloads.el ends here
