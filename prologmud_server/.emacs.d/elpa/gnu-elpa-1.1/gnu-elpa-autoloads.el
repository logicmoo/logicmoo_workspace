;;; gnu-elpa-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gnu-elpa-maint" "gnu-elpa-maint.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from gnu-elpa-maint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-elpa-maint" '("gnu-elpa--")))

;;;***

;;;### (autoloads nil "gnu-elpa-utils" "gnu-elpa-utils.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from gnu-elpa-utils.el
 (eval-after-load 'package
  '(unless (assoc "gnu" package-archives)
     (push '("gnu" . "https://elpa.gnu.org/packages/")
           package-archives)))
 ;; Skip load-source-file-function which would slow us down by
 ;; a factor 2 (this assumes we were careful to save this file
 ;; so it doesn't need any decoding).
 (let ((load-source-file-function nil))
  (require 'gnu-elpa-features nil 'noerror))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-elpa-utils" '("gnu-elpa--")))

;;;***

;;;### (autoloads nil nil ("gnu-elpa-features.el" "gnu-elpa-pkg.el"
;;;;;;  "gnu-elpa.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gnu-elpa-autoloads.el ends here
