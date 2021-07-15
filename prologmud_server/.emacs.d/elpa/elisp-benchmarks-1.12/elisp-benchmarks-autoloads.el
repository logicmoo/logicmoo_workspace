;;; elisp-benchmarks-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elisp-benchmarks" "elisp-benchmarks.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from elisp-benchmarks.el

(autoload 'elisp-benchmarks-run "elisp-benchmarks" "\
Run all the benchmarks and present the results.
If non nil SELECTOR is a regexp to match the benchmark names to be executed.
The test is repeated RUNS number of times.  If RUNS is nil `elb-runs' is used as
default.
RECOMPILE all the benchmark folder when non nil.

\(fn &optional SELECTOR (RECOMPILE t) RUNS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-benchmarks" '("elb-")))

;;;***

;;;### (autoloads nil nil ("elisp-benchmarks-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elisp-benchmarks-autoloads.el ends here
