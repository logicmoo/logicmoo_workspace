;;; smalltalk-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gst-mode" "gst-mode.el" (0 0 0 0))
;;; Generated autoloads from gst-mode.el

(autoload 'gst "gst-mode" "\
Invoke GNU Smalltalk.

\(fn COMMAND-LINE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gst-mode" '("smalltalk-" "send-to-smalltalk" "gst-")))

;;;***

;;;### (autoloads nil "smalltalk-mode" "smalltalk-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from smalltalk-mode.el

(autoload 'smalltalk-mode "smalltalk-mode" "\
Major mode for editing Smalltalk code.

Commands:
\\{smalltalk-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.st\\'" . smalltalk-mode))

(add-to-list 'auto-mode-alist (cons "\\.star\\'" (catch 'archive-mode (dolist (mode-assoc auto-mode-alist 'archive-mode) (and (string-match (car mode-assoc) "Starfile.zip") (functionp (cdr mode-assoc)) (throw 'archive-mode (cdr mode-assoc)))))))

(add-to-list (if (boundp 'inhibit-local-variables-regexps) 'inhibit-local-variables-regexps 'inhibit-first-line-modes-regexp) "\\.star\\'")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smalltalk-mode" '("smalltalk-")))

;;;***

;;;### (autoloads nil nil ("smalltalk-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smalltalk-mode-autoloads.el ends here
