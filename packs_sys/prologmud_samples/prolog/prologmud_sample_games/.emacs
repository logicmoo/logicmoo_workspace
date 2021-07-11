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

;#+BEGIN_SRC emacs-lisp -n :async :results verbatim code
  (load "~/.emacs.d/prolog-lsp.el")
;#+END_SRC

;#+BEGIN_SRC emacs-lisp -n :async :results verbatim code
  (add-hook 'prolog-mode-hook #'lsp)
;#+END_SRC

