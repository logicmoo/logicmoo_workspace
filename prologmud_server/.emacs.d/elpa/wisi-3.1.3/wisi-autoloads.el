;;; wisi-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wisi" "wisi.el" (0 0 0 0))
;;; Generated autoloads from wisi.el

(register-definition-prefixes "wisi" '("wisi-"))

;;;***

;;;### (autoloads nil "wisi-fringe" "wisi-fringe.el" (0 0 0 0))
;;; Generated autoloads from wisi-fringe.el

(register-definition-prefixes "wisi-fringe" '("wisi-fringe-"))

;;;***

;;;### (autoloads nil "wisi-parse-common" "wisi-parse-common.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wisi-parse-common.el

(register-definition-prefixes "wisi-parse-common" '("wisi-"))

;;;***

;;;### (autoloads nil "wisi-prj" "wisi-prj.el" (0 0 0 0))
;;; Generated autoloads from wisi-prj.el

(autoload 'wisi-prj-xref-backend "wisi-prj" "\
For `xref-backend-functions'; return the current wisi project." nil nil)

(autoload 'wisi-prj-select-cache "wisi-prj" "\
Select project matching PRJ-FILE in `wisi-prj--cache' as current project,
parsing and caching if needed. Also add DOMINATING-FILE (default
current buffer file name) to `wisi-prj--dominating-alist' (for
`wisi-prj-select-dominating'.)

\(fn PRJ-FILE INIT-PRJ &optional DOMINATING-FILE)" nil nil)

(autoload 'wisi-prj-select-dominating "wisi-prj" "\
Unless it is already current, select a wisi-prj matching DOMINATING-FILE.
DOMINATING-FILE defaults to the current buffer file name.
Useful before running `compilation-start', to ensure the correct
project is current.

\(fn &optional DOMINATING-FILE)" nil nil)

(autoload 'wisi-prj-current-cached "wisi-prj" "\
For `project-find-functions'; return the current project from `wisi-prj--cache'.

\(fn DIR)" nil nil)

(autoload 'wisi-prj-select-file "wisi-prj" "\
Set PRJ-FILE as current project, add DEFAULT-PRJ to `wisi-prj--default'.
Also add DOMINATING-FILE (default current buffer file name) to
`wisi-prj--dominating-alist' (for `wisi-prj-select-dominating'.)

\(fn PRJ-FILE DEFAULT-PRJ &optional DOMINATING-FILE)" nil nil)

(autoload 'wisi-prj-current-parse "wisi-prj" "\
For `project-find-functions'; parse the current project file, select and return the project

\(fn DIR)" nil nil)

(autoload 'wisi-prj-cache-dominating "wisi-prj" "\
Parse prj-file, add to `wisi-prj--cache'.
Also add (DOMINATING-FILE . PRJ-FILE) to `wisi-prj--dominating-alist'.
DOMINATING-FILE defaults to (buffer-file-name). 

\(fn PRJ-FILE DEFAULT-PRJ &optional DOMINATING-FILE)" nil nil)

(autoload 'wisi-prj-find-dominating-cached "wisi-prj" "\
For `project-find-functions'; return the cached project
matching `wisi-prj--dominating' (nil if none). Select it if it is
not the current project.

\(fn DIR)" nil nil)

(autoload 'wisi-prj-set-dominating "wisi-prj" "\
Add (DOM-FILE . PRJ-FILE) to `wisi-prj--dominating-alist',
and (PRJ-FILE . DEFAULT-PRJ) to `wisi-prj--default'.
DOM-FILE defaults to (buffer-file-name).
For example, call this in the Local Vars of a Makefile to
associate a project with that Makefile.

\(fn PRJ-FILE DEFAULT-PRJ &optional DOM-FILE)" nil nil)

(autoload 'wisi-prj-find-dominating-parse "wisi-prj" "\
For `project-find-functions'; parse, select, and return the project
file matching `wisi-prj--dominating'.

\(fn DIR)" nil nil)

(autoload 'wisi-prj-dtrt-parse-file "wisi-prj" "\
Depending on wisi-prj function in `project-find-functions',
Do The Right Thing to make PRJ-FILE active and selected; return the project.

\(fn PRJ-FILE DEFAULT-PRJ DOMINATING-FILE &optional DIR)" nil nil)

(autoload 'wisi-prj-find-function-set-p "wisi-prj" "\
Return non-nil if a wisi-prj function is present in `project-find-functions'." nil nil)

(register-definition-prefixes "wisi-prj" '("wisi-"))

;;;***

;;;### (autoloads nil "wisi-process-parse" "wisi-process-parse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wisi-process-parse.el

(autoload 'wisi-process-parse-get "wisi-process-parse" "\
Return a ‘wisi-process--parser’ object matching PARSER label.
If label found in ‘wisi-process--alist’, return that.
Otherwise add PARSER to ‘wisi-process--alist’, return it.

\(fn PARSER)" nil nil)

(register-definition-prefixes "wisi-process-parse" '("wisi-"))

;;;***

;;;### (autoloads nil "wisi-run-indent-test" "wisi-run-indent-test.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wisi-run-indent-test.el

(register-definition-prefixes "wisi-run-indent-test" '("large-frame" "run-test" "skip-" "test-"))

;;;***

;;;### (autoloads nil "wisi-skel" "wisi-skel.el" (0 0 0 0))
;;; Generated autoloads from wisi-skel.el

(autoload 'wisi-skel-hippie-try "wisi-skel" "\
For `hippie-expand-try-functions-list'.

\(fn OLD)" nil nil)

(register-definition-prefixes "wisi-skel" '("wisi-skel-"))

;;;***

;;;### (autoloads nil "wisi-tests" "wisi-tests.el" (0 0 0 0))
;;; Generated autoloads from wisi-tests.el

(register-definition-prefixes "wisi-tests" '("test-syntax-" "wisi-test"))

;;;***

;;;### (autoloads nil "wisitoken-parse_table-mode" "wisitoken-parse_table-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from wisitoken-parse_table-mode.el

(autoload 'wisitoken-parse_table-mode "wisitoken-parse_table-mode" "\
Provides navigation in wisi-generate parse table output.

This is a minor mode.  If called interactively, toggle the
`Wisitoken-Parse_Table mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `wisitoken-parse_table-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("\\.parse_table.*\\'" . wisitoken-parse_table-mode))

(register-definition-prefixes "wisitoken-parse_table-mode" '("wisitoken-parse_table--xref-backend"))

;;;***

;;;### (autoloads nil nil ("wisi-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wisi-autoloads.el ends here
