;;; zenscript-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zenscript-common" "zenscript-common.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from zenscript-common.el

(register-definition-prefixes "zenscript-common" '("zenscript-"))

;;;***

;;;### (autoloads nil "zenscript-completion" "zenscript-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from zenscript-completion.el

(register-definition-prefixes "zenscript-completion" '("zenscript-"))

;;;***

;;;### (autoloads nil "zenscript-highlighting" "zenscript-highlighting.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from zenscript-highlighting.el

(register-definition-prefixes "zenscript-highlighting" '("zenscript-"))

;;;***

;;;### (autoloads nil "zenscript-indentation" "zenscript-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from zenscript-indentation.el

(register-definition-prefixes "zenscript-indentation" '("zenscript-"))

;;;***

;;;### (autoloads nil "zenscript-language" "zenscript-language.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from zenscript-language.el

(add-hook 'zenscript-parse-error-hook #'zenscript--highlight-error)

(autoload 'forward-zenscript-identifier "zenscript-language" "\
Move forward until encountering the end of a ZenScript identifier.
With argument ARG, do this that many times.
If ARG is omitted or nil, move point forward one identifier.

\(fn &optional ARG)" t nil)

(autoload 'backward-zenscript-identifier "zenscript-language" "\
Move backward until encountering the beginning of a ZenScript identifier.
With argument ARG, do this that many times.
If ARG is omitted or nil, move point backward one identifier.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "zenscript-language" '("zenscript-"))

;;;***

;;;### (autoloads nil "zenscript-mode" "zenscript-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from zenscript-mode.el

(autoload 'zenscript-view-docs "zenscript-mode" "\
Open the CraftTweaker docs in your default browser." t nil)

(autoload 'zenscript-search-docs "zenscript-mode" "\
Search for a string in the CraftTweaker docs in your default browser." t nil)

(autoload 'zenscript-mode "zenscript-mode" "\
Major mode for ZenScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.zs\\'" . zenscript-mode))

(register-definition-prefixes "zenscript-mode" '("zenscript-"))

;;;***

;;;### (autoloads nil "zenscript-parser" "zenscript-parser.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from zenscript-parser.el

(register-definition-prefixes "zenscript-parser" '("zenscript-"))

;;;***

;;;### (autoloads nil nil ("zenscript-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zenscript-mode-autoloads.el ends here
