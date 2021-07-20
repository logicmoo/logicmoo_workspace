;;; orgtbl-ascii-plot-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orgtbl-ascii-plot" "orgtbl-ascii-plot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from orgtbl-ascii-plot.el

(autoload 'orgtbl-ascii-plot "orgtbl-ascii-plot" "\
Draws an ascii bars plot in a column, out of values found in another column.
  A numeric prefix may be given to override the default 12 characters wide plot.
    

\(fn &optional ASK)" t nil)

(autoload 'orgtbl-ascii-plot-bindings "orgtbl-ascii-plot" nil nil nil)

(if (functionp 'org-defkey) (orgtbl-ascii-plot-bindings) (setq org-load-hook (cons 'orgtbl-ascii-plot-bindings (if (boundp 'org-load-hook) org-load-hook))))

(register-definition-prefixes "orgtbl-ascii-plot" '("orgtbl-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orgtbl-ascii-plot-autoloads.el ends here
