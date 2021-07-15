;;; easy-kill-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "easy-kill" "easy-kill.el" (0 0 0 0))
;;; Generated autoloads from easy-kill.el

(autoload 'easy-kill "easy-kill" "\
Kill thing at point in the order of region, url, email and line.
Temporally activate additional key bindings as follows:

  letters => select or expand selection according to `easy-kill-alist';
  1..9    => expand selection by that number;
  0       => shrink to the initial selection;
  +,=/-   => expand or shrink selection;
  @       => append selection to previous kill;
  ?       => help;
  C-w     => kill selection;
  C-SPC   => turn selection into an active region;
  C-g     => abort;
  others  => save selection and exit.

\(fn &optional N)" t nil)

(defalias 'easy-mark-sexp 'easy-mark "\
Use `easy-mark' instead. The alias may be removed in future.")

(autoload 'easy-mark "easy-kill" "\
Similar to `easy-kill' (which see) but for marking.

\(fn &optional N)" t nil)

(register-definition-prefixes "easy-kill" '("easy-"))

;;;***

;;;### (autoloads nil nil ("easy-kill-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; easy-kill-autoloads.el ends here
