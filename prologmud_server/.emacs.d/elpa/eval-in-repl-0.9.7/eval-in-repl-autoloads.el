;;; eval-in-repl-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eval-in-repl" "eval-in-repl.el" (0 0 0 0))
;;; Generated autoloads from eval-in-repl.el

(register-definition-prefixes "eval-in-repl" '("eir-"))

;;;***

;;;### (autoloads nil "eval-in-repl-cider" "eval-in-repl-cider.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-cider.el

(autoload 'eir-eval-in-cider "eval-in-repl-cider" "\
eval-in-repl for cider." t nil)

(register-definition-prefixes "eval-in-repl-cider" '("eir-"))

;;;***

;;;### (autoloads nil "eval-in-repl-elm" "eval-in-repl-elm.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from eval-in-repl-elm.el

(autoload 'eir-eval-in-elm "eval-in-repl-elm" "\
Provides eval-in-repl for Elm." t nil)

(register-definition-prefixes "eval-in-repl-elm" '("eir-send-to-elm"))

;;;***

;;;### (autoloads nil "eval-in-repl-erlang" "eval-in-repl-erlang.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-erlang.el

(autoload 'eir-eval-in-erlang "eval-in-repl-erlang" "\
Provides eval-in-repl for Erlang." t nil)

(register-definition-prefixes "eval-in-repl-erlang" '("eir-send-to-erlang"))

;;;***

;;;### (autoloads nil "eval-in-repl-geiser" "eval-in-repl-geiser.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-geiser.el

(autoload 'eir-eval-in-geiser "eval-in-repl-geiser" "\
eval-in-repl for Geiser." t nil)

(register-definition-prefixes "eval-in-repl-geiser" '("eir-send-to-geiser"))

;;;***

;;;### (autoloads nil "eval-in-repl-hy" "eval-in-repl-hy.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from eval-in-repl-hy.el

(autoload 'eir-eval-in-hy "eval-in-repl-hy" "\
eval-in-repl for Hy." t nil)

(register-definition-prefixes "eval-in-repl-hy" '("eir-"))

;;;***

;;;### (autoloads nil "eval-in-repl-ielm" "eval-in-repl-ielm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-ielm.el

(autoload 'eir-eval-in-ielm "eval-in-repl-ielm" "\
eval-in-repl for IELM." t nil)

(register-definition-prefixes "eval-in-repl-ielm" '("eir-"))

;;;***

;;;### (autoloads nil "eval-in-repl-iex" "eval-in-repl-iex.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from eval-in-repl-iex.el

(autoload 'eir-eval-in-iex "eval-in-repl-iex" "\
Provides eval-in-repl for Elixir." t nil)

(register-definition-prefixes "eval-in-repl-iex" '("eir-send-to-iex"))

;;;***

;;;### (autoloads nil "eval-in-repl-javascript" "eval-in-repl-javascript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-javascript.el

(autoload 'eir-eval-in-javascript "eval-in-repl-javascript" "\
eval-in-repl for Javascript." t nil)

(register-definition-prefixes "eval-in-repl-javascript" '("eir-send-to-javascript"))

;;;***

;;;### (autoloads nil "eval-in-repl-lua" "eval-in-repl-lua.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from eval-in-repl-lua.el

(autoload 'eir-eval-in-lua "eval-in-repl-lua" "\
eval-in-repl for Lua." t nil)

(register-definition-prefixes "eval-in-repl-lua" '("eir-send-to-lua" "eval-in-repl-run-lua"))

;;;***

;;;### (autoloads nil "eval-in-repl-ocaml" "eval-in-repl-ocaml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-ocaml.el

(autoload 'eir-eval-in-ocaml "eval-in-repl-ocaml" "\
eval-in-repl for OCaml." t nil)

(register-definition-prefixes "eval-in-repl-ocaml" '("eir-send-to-ocaml"))

;;;***

;;;### (autoloads nil "eval-in-repl-prolog" "eval-in-repl-prolog.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-prolog.el

(autoload 'eir-eval-in-prolog "eval-in-repl-prolog" "\
eval-in-repl for SWI Prolog." t nil)

(register-definition-prefixes "eval-in-repl-prolog" '("eir-send-to-prolog"))

;;;***

;;;### (autoloads nil "eval-in-repl-python" "eval-in-repl-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-python.el

(autoload 'eir-eval-in-python "eval-in-repl-python" "\
eval-in-repl for Python." t nil)

(register-definition-prefixes "eval-in-repl-python" '("eir-"))

;;;***

;;;### (autoloads nil "eval-in-repl-racket" "eval-in-repl-racket.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-racket.el

(autoload 'eir-eval-in-racket "eval-in-repl-racket" "\
eval-in-repl for Racket." t nil)

(register-definition-prefixes "eval-in-repl-racket" '("eir-send-to-racket"))

;;;***

;;;### (autoloads nil "eval-in-repl-ruby" "eval-in-repl-ruby.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-ruby.el

(autoload 'eir-eval-in-ruby "eval-in-repl-ruby" "\
eval-in-repl for Ruby." t nil)

(register-definition-prefixes "eval-in-repl-ruby" '("eir-send-to-ruby"))

;;;***

;;;### (autoloads nil "eval-in-repl-scheme" "eval-in-repl-scheme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-scheme.el

(autoload 'eir-eval-in-scheme "eval-in-repl-scheme" "\
eval-in-repl for Scheme." t nil)

(register-definition-prefixes "eval-in-repl-scheme" '("eir-send-to-scheme"))

;;;***

;;;### (autoloads nil "eval-in-repl-shell" "eval-in-repl-shell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-shell.el

(autoload 'eir-eval-in-shell "eval-in-repl-shell" "\
eval-in-repl for shell." t nil)

(register-definition-prefixes "eval-in-repl-shell" '("eir-send-to-shell"))

;;;***

;;;### (autoloads nil "eval-in-repl-slime" "eval-in-repl-slime.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eval-in-repl-slime.el

(autoload 'eir-eval-in-slime "eval-in-repl-slime" "\
eval-in-repl for SLIME." t nil)

(register-definition-prefixes "eval-in-repl-slime" '("eir-send-to-slime"))

;;;***

;;;### (autoloads nil "eval-in-repl-sly" "eval-in-repl-sly.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from eval-in-repl-sly.el

(autoload 'eir-eval-in-sly "eval-in-repl-sly" "\
eval-in-repl for Sly." t nil)

(register-definition-prefixes "eval-in-repl-sly" '("eir-send-to-sly"))

;;;***

;;;### (autoloads nil "eval-in-repl-sml" "eval-in-repl-sml.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from eval-in-repl-sml.el

(autoload 'eir-eval-in-sml "eval-in-repl-sml" "\
eval-in-repl for Standard ML." t nil)

(register-definition-prefixes "eval-in-repl-sml" '("eir-send-to-sml"))

;;;***

;;;### (autoloads nil nil ("eval-in-repl-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eval-in-repl-autoloads.el ends here
