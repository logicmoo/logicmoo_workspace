(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories '("/opt/logicmoo_workspace/prologmud_server"))
 '(package-selected-packages
   '(tree-mode keytar ac-helm lsp-dart lsp-grammarly lsp-ltex lsp-mode lsp-ui lyrics m-buffer nexus ng2-mode nginx-mode niceify-info nix-buffer nix-env-install nix-haskell-mode nix-mode nix-modeline nix-sandbox nix-update nixos-options omnibox omnisharp on-screen open-in-msvs orca org-alert org-autolist org-babel-eval-in-repl org-brain org-capture-pop-frame org-chef org-clock-split org-context org-cua-dwim org-dashboard org-dotemacs org-download org-preview-html org-projectile org-projectile-helm org-protocol-jekyll org-radiobutton org-sync org-sync-snippets org-table-comment org-table-sticky-header svg-clock svg-mode-line-themes svg-tag-mode svnwrapper swagger-to-org swap-buffers swap-regions swoop sws-mode sx sxiv symbol-overlay symbolist symbolword-mode symex symon symon-lingr sysctl syslog-mode system-specific-settings systemd systemtap-mode terminal-focus-reporting terminal-here terminal-toggle tern tern-auto-complete tern-context-coloring toc-mode toc-org todoist usage-memo use-package use-package-chords use-package-el-get use-package-ensure-system-package use-package-hydra use-proxy use-ttf utimeclock utop uuid uuidgen v-mode v2ex-mode vs-light-theme vscdark-theme vscode-dark-plus-theme vscode-icon vterm vterm-toggle vtm vue-html-mode web web-beautify web-completion-data web-mode web-mode-edit-element web-narrow-mode web-search webkit-color-picker weblio weblogger widgetjs wiki-nav wiki-summary wikinfo wikinforg wilt win-switch windata window-end-visible window-jump window-layout window-number window-numbering windower windresize winds windsize windswap windwow winnow winpoint winring winum wisitoken-grammar-mode wisp-mode wispjs-mode with-emacs with-namespace with-proxy with-shell-interpreter with-simulated-input with-venv wn-mode wolfram xmind-org xml+ xml-format yaml yaml-imenu yaml-mode yaml-tomato yandex-weather yang-mode yankpad yapfify zen-and-art-theme zen-mode zenburn-theme zencoding-mode zenity-color-picker zeno-theme zenscript-mode zephir-mode zimports zombie zoom zoom-window zop-to-char xelb language-detection s tuareg))
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

'(vscode-mode)
(prolog-mode)


(package-install 's) (require 's)
(package-install 'language-detection) (require 'language-detection)

(defun check-if-prolog ()
  (if (string-equal (format "%s" (language-detection-buffer)) "prolog")
      (prolog-mode)))

(add-hook 'perl-mode-hook 'check-if-prolog)


(require 'lsp-mode)

(require 'lsp-ui)

(prolog-mode)
(require 'vscode-init)

(use-package lsp-mode
    :hook
    ((prolog-mode) . lsp)
    :bind
    (:map lsp-mode-map
          ("C-c r" . lsp-rename))
    :config

    ;; LSP UI tools
    (use-package lsp-ui
      :preface
      (defun ladicle/toggle-lsp-ui-doc ()
        (interactive)
        (if lsp-ui-doc-mode
            (progn
              (lsp-ui-doc-mode -1)
              (lsp-ui-doc--hide-frame))
          (lsp-ui-doc-mode 1)))
      :bind
      (:map lsp-mode-map
            ("C-c C-r" . lsp-ui-peek-find-references)
            ("C-c C-j" . lsp-ui-peek-find-definitions)
            ("C-c i" . lsp-ui-peek-find-implementation)
            ("C-c m" . lsp-ui-imenu)
            ("C-c s" . lsp-ui-sideline-mode)
            ("C-c d" . ladicle/toggle-lsp-ui-doc))
      :hook
      (lsp-mode . lsp-ui-mode)))



