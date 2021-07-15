(defgroup vscode nil
 "VSCode for Emacs."
 :prefix "vscode-mode-"
 :group 'emulations
 :link '(url-link :tag "Github" "https://github.com/planlogic/vscode-mode"))

(global-set-key "\C-cvv" 'global-vscode-mode-toggle)


(defun global-vscode-mode-toggle ()
 ""
 (interactive)
 (if global-vscode-mode
  (global-vscode-mode-off)
  (global-vscode-mode-on)))

(defun global-vscode-mode-on ()
 ""
 (interactive)
 (global-vscode-mode 1)
 (global-hl-line-mode 1)
 (global-display-line-numbers-mode 1)
 (see "turned on global vscode mode"))

(defun global-vscode-mode-off ()
 ""
 (interactive)
 (global-vscode-mode 0)
 (global-hl-line-mode 0)
 (global-display-line-numbers-mode 0)
 (see "turned off global vscode mode"))

(defun vscode-minor-mode-on ()
 ""
 (interactive)
 (vscode-minor-mode 1))

(define-globalized-minor-mode global-vscode-mode vscode-minor-mode vscode-minor-mode-on)

(defvar vscode-minor-mode-map
 (let ((map (make-sparse-keymap)))
  ;; Toggle between text and image display or editing
  (define-key map "\C-z" 'undo)
  (define-key map [f1] 'help)
  (define-key map (kbd "M-<f4>") 'save-buffers-kill-terminal)
  (define-key map "\M-fx" 'save-buffers-kill-terminal)
  (define-key map (kbd "<S-Insert>") 'clipboard-yank)

  (define-key map "\C-cvr" 'vscode-mode-regenerate-matchmaking)

  ;; ;; auto-generated manually editted keys from vscode-keybindings-parser.el - vscode-mode-regenerate-matchmaking
  ;; ;; subject to correction/expansion etc.
  ;; (define-key map (kbd "C-k z") 'repeat)
  ;; ;; (define-key map (kbd "C-j")
  ;; (define-key map (kbd "S-M-0") 'ctl-x-4-prefix)
  ;; (define-key map (kbd "C-p") 'ffap)
  ;; (define-key map (kbd "C-e") 'ffap)
  ;; (define-key map (kbd "C-S-n") 'set-goal-column)
  ;; (define-key map (kbd "C-S-s") 'write-file)
  ;; (define-key map (kbd "C-k s") 'save-some-buffers)
  ;; (define-key map (kbd "C-s") 'save-buffer)
  ;; (define-key map (kbd "C-w") 'save-buffers-kill-terminal)
  ;; (define-key map (kbd "C-S-w") 'save-buffers-kill-terminal)
  ;; (define-key map (kbd "C-k w") 'delete-window)
  ;; ;; (define-key map (kbd "C-k C-w")
  ;; (define-key map (kbd "C-<f4>") 'kill-buffer)
  ;; (define-key map (kbd "C-w") 'kill-buffer)
  ;; (define-key map (kbd "<up>") 'previous-line)
  ;; (define-key map (kbd "C-<up>") 'previous-line)
  ;; (define-key map (kbd "<down>") 'next-line)
  ;; (define-key map (kbd "C-<down>") 'next-line)
  ;; ;; (define-key map (kbd "C-S-SPC")
  ;; (define-key map (kbd "S-RET") 'isearch-backward)
  ;; (define-key map (kbd "S-<f3>") 'isearch-backward)
  ;; (define-key map (kbd "RET") 'isearch-forward)
  ;; (define-key map (kbd "<f3>") 'isearch-forward)
  ;; ;; (define-key map (kbd "C-/")
  ;; (define-key map (kbd "C-a") 'mark-whole-buffer)
  ;; (define-key map (kbd "ESC") 'keyboard-quit)
  ;; (define-key map (kbd "S-ESC") 'keyboard-quit)

  map)
 "Keymap used by `doc-minor-view-mode'.")

(define-minor-mode vscode-minor-mode
 "VSCode minor mode for using VSCode bindings in Emacs.
\\{vscode-mode-map}"
 nil " VSC" vscode-minor-mode-map
 :group 'vscode
 (if vscode-minor-mode
  (progn
   (cua-mode 1)
   ;; (see "cua on")
   )
  (progn
   (cua-mode 0)
   ;; (see "cua off")
   )
  ))

(add-to-list 'load-path vscode-mode-dir)

(require 'vscode-keybindings-parser)

(require 'hl-line) 

(provide 'vscode-mode)
