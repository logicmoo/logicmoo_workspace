(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(tuareg)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'lsp-mode)

; '(load "~/.emacs.d/elpa/lsp-mode-7.0.1/lsp-mode.el")

;#+BEGIN_SRC emacs-lisp -n :async :results verbatim code
  (load "~/.emacs.d/prolog-lsp.el")
;#+END_SRC

;#+BEGIN_SRC emacs-lisp -n :async :results verbatim code
  (add-hook 'prolog-mode-hook #'lsp)
;#+END_SRC

(prolog-mode)

(setq vscode-mode-dir "~/vscode-mode")
(add-to-list 'load-path vscode-mode-dir)
(require 'vscode-init)

(add-to-list 'emacs-startup-hook 'global-vscode-mode-on)

