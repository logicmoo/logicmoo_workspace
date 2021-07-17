(Require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories
   '("/opt/logicmoo_workspace/prologmud_server"))
 '(package-selected-packages '(tuareg))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'lsp-mode)

; '(load "~/.emacs.d/elpa/lsp-mode-7.0.1/lsp-mode.el")

;#+beGIN_SRC emacs-lisp -n :async :results verbatim code
  (load "~/.emacs.d/prolog-lsp.el")
;#+END_SRC

;#+BEGIN_SRC emacs-lisp -n :async :results verbatim code
  (add-hook 'prolog-mode-hook #'lsp)
;#+END_SRC

; optional but gives a symbolic name which may be easier to work with
; and allow modes which may already know about C-home to take advantage
; of the binding
(if (not key-translation-map)
    (setq key-translation-map (make-sparse-keymap)))
(define-key key-translation-map "\e[1;5~" [C-home])

; bind the key (or check before if the default binding isn't suitable)
(global-set-key [(control home)] 'beginning-of-buffer)


(global-set-key (kbd "<C-tab>") 'my-switch-buffer)
(defun my-switch-buffer ()
  "Switch buffers, but don't record the change until the last one."
  (interactive)
  (let ((blist (copy-sequence (buffer-list)))
        current
        (key-for-this (this-command-keys))
        (key-for-this-string (format-kbd-macro (this-command-keys)))
        done)
    (while (not done)
      (setq current (car blist))
      (setq blist (append (cdr blist) (list current)))
      (when (and (not (get-buffer-window current))
                 (not (minibufferp current)))
        (switch-to-buffer current t)
        (message "Type %s to continue cycling" key-for-this-string)
        (when (setq done (not (equal key-for-this (make-vector 1 (read-event)))))
          (switch-to-buffer current)
          (clear-this-command-keys t)
          (setq unread-command-events (list last-input-event)))))))

(define-key function-key-map [(control shift iso-lefttab)] [(control shift tab)])
(define-key function-key-map [(meta shift iso-lefttab)] [(meta shift tab)])
(define-key function-key-map [(meta control shift iso-lefttab)] [(meta control shift tab)])


(defun switch-to-prev-window ()
  (interactive)
  (other-window -1))

(global-set-key [(control shift tab)] 'switch-to-prev-window)

(cua-mode 1)               
(transient-mark-mode 1)    
(setq shift-select-mode t) 
(global-linum-mode 1)      
(show-paren-mode 1)        
(desktop-save-mode 1)
(define-key input-decode-map "\e[1;2D" [S-left])  
(define-key input-decode-map "\e[1;2C" [S-right])  
(define-key input-decode-map "\e[1;2B" [S-down])  
(define-key input-decode-map "\e[1;2A" [S-up])  
(define-key input-decode-map "\e[1;2F" [S-end])  
(define-key input-decode-map "\e[1;2H" [S-home])

(prolog-mode)

(setq vscode-mode-dir "~/vscode-mode")
(add-to-list 'load-path vscode-mode-dir)
'(require 'vscode-init)

(add-to-list 'emacs-startup-hook 'global-vscode-mode-on)

(load (concat vscode-mode-dir "/external/easy-moving-from-vscode-to-emacs-1.el"))

(defun vscode-mode-install-required-packages ()
 ""
 (interactive)
 (if '(yes-or-no-p "Are you sure you want to install required packages for vscode-mode? ")
  (progn
   (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
   (load (concat vscode-mode-dir "/external/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name.el"))
   (when (not package-archive-contents)
    (package-refresh-contents))
   (ensure-package-installed 'multiple-cursors 'projectile 'helm 'dumb-jump 'company)
   )))

(if '(yes-or-no-p "Install required packages for vscode-mode? ")
 (vscode-mode-install-required-packages))

(load (concat vscode-mode-dir "/external/code/defuns.el"))
(load (concat vscode-mode-dir "/external/easy-moving-from-vscode-to-emacs-2.el"))
(load (concat vscode-mode-dir "/vscode-mode.el"))

(provide 'vscode-init)

(vscode-mode)
(prolog-mode)

