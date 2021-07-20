;;; zephir-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zephir-face" "zephir-face.el" (0 0 0 0))
;;; Generated autoloads from zephir-face.el

(let ((loads (get 'zephir-faces 'custom-loads))) (if (member '"zephir-face" loads) nil (put 'zephir-faces 'custom-loads (cons '"zephir-face" loads))))

;;;***

;;;### (autoloads nil "zephir-mode" "zephir-mode.el" (0 0 0 0))
;;; Generated autoloads from zephir-mode.el

(let ((loads (get 'zephir 'custom-loads))) (if (member '"zephir-mode" loads) nil (put 'zephir 'custom-loads (cons '"zephir-mode" loads))))

(autoload 'zephir-mode "zephir-mode" "\


\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(register-definition-prefixes "zephir-mode" '("zephir-"))

;;;***

;;;### (autoloads nil nil ("zephir-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zephir-mode-autoloads.el ends here
