;;; hook-helpers-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hook-helpers" "hook-helpers.el" (0 0 0 0))
;;; Generated autoloads from hook-helpers.el

(autoload 'create-hook-helper "hook-helpers" "\
Creates a new hook helper ID for the hooks in HOOKS.

If a hook helper with id ID already exists, it's overridden.  All instances of
the helper in its associated hooks are replaced.

See `hkhlp-normalize-hook-spec' for an explanation of HOOKS.

\(fn ID ARGS &optional DOCSTRING &keys HOOKS &rest BODY)" nil t)

(function-put 'create-hook-helper 'lisp-indent-function 'defun)

(function-put 'create-hook-helper 'doc-string-elt '3)

(autoload 'define-hook-helper "hook-helpers" "\
Define a hook helper for the variable HOOK-hook with ARGS as the argument list.

This helper consists of all the code in BODY.  HOOK should not be
quoted.  The keywords are:

:name    Specifies a name to use for the generated function.  As part
         of this macro, a function called hook-helper--HOOK will be
         created.  If NAME is given, then the function becomes
         ‘hook-helper--HOOK/NAME’.

:append  If non-nil, append the hook helper to the hook variable.

:suffix  Allows a user to specify that the hook variable doesn't
         end with ‘-hook’, but instead with another suffix, such as
         ‘-function’.  SUFFIX should be a string, and defaults to ‘hook’
         if not specified.  Note that SUFFIX is not assumed to start with
         a hyphen.

\(fn HOOK ARGS &optional DOCSTRING &rest BODY)" nil t)

(function-put 'define-hook-helper 'lisp-indent-function 'defun)

(function-put 'define-hook-helper 'doc-string-elt '3)

(autoload 'define-hook-function "hook-helpers" "\
Define FUNCTION to be a function, then add it to hooks.

The hooks to add are specified by the :hooks keyword.  This is a
simple list of hooks, unquoted, and the new function is added to
each one.

\(fn FUNCTION ARGS &optional DOCSTRING &rest BODY)" nil t)

(function-put 'define-hook-function 'lisp-indent-function 'defun)

(function-put 'define-hook-function 'doc-string-elt '3)

(make-obsolete 'define-hook-function 'create-hook-helper '"1.1")

(register-definition-prefixes "hook-helpers" '("add-hook-helper" "describe-hook-helpers" "hkhlp-" "remove-hook-helper"))

;;;***

;;;### (autoloads nil nil ("hook-helpers-pkg.el" "hook-helpers-tests.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hook-helpers-autoloads.el ends here
