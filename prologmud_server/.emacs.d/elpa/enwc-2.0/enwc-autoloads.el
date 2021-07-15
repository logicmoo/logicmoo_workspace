;;; enwc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "enwc" "enwc.el" (0 0 0 0))
;;; Generated autoloads from enwc.el

(autoload 'enwc "enwc" "\
The main front-end to ENWC.
This sets up the buffer and scans for networks.
In order to use this, one must have already run
`enwc-setup'.

\\{enwc-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "enwc" '("enwc-")))

;;;***

;;;### (autoloads nil "enwc-backend" "enwc-backend.el" (0 0 0 0))
;;; Generated autoloads from enwc-backend.el

(autoload 'enwc-register-backend "enwc-backend" "\
Register the backend KEY with DEFINITION.

Signals an error if a backend with KEY already exists and FORCEP is nil.

\(fn DEFINITION &optional FORCEP)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "enwc-backend" '("enwc-")))

;;;***

;;;### (autoloads nil "enwc-edit" "enwc-edit.el" (0 0 0 0))
;;; Generated autoloads from enwc-edit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "enwc-edit" '("enwc-")))

;;;***

;;;### (autoloads nil "enwc-nm" "enwc-nm.el" (0 0 0 0))
;;; Generated autoloads from enwc-nm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "enwc-nm" '("enwc-nm-")))

;;;***

;;;### (autoloads nil "enwc-wicd" "enwc-wicd.el" (0 0 0 0))
;;; Generated autoloads from enwc-wicd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "enwc-wicd" '("enwc-wicd-")))

;;;***

;;;### (autoloads nil nil ("enwc-cm.el" "enwc-pkg.el" "enwc-test.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; enwc-autoloads.el ends here
