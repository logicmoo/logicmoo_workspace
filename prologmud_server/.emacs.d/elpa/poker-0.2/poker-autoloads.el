;;; poker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "poker" "poker.el" (0 0 0 0))
;;; Generated autoloads from poker.el

(autoload 'poker "poker" "\
Play a game of texas hold 'em poker.

\(fn INITIAL-STACK MIN-BET PLAYERS)" t nil)

(define-key menu-bar-games-menu [poker] '(menu-item "Texas hold 'em poker" poker :help "Play Texas hold 'em poker"))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poker" '("poker-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; poker-autoloads.el ends here
