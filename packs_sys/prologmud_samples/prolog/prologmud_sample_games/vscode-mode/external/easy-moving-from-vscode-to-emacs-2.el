;; Start From https://krsoninikhil.github.io/2018/12/15/easy-moving-from-vscode-to-emacs/

(defun duplicate-line-orig ()
 (interactive)
 (save-mark-and-excursion
  (beginning-of-line)
  (insert (thing-at-point 'line t))))

(defun duplicate-line ()
 (interactive)
 (let ((col (current-column)))
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (move-to-column col)))

(global-set-key (kbd "C-S-d") 'duplicate-line)

;;;;;;;;;;;;;;; section

(defun move-line-down ()
 (interactive)
 (let ((col (current-column)))
  (save-excursion
   (forward-line)
   (transpose-lines 1))
  (forward-line)
  (move-to-column col)))

(defun move-line-up ()
 (interactive)
 (let ((col (current-column)))
  (save-excursion
   (forward-line)
   (transpose-lines -1))
  (forward-line -1)
  (move-to-column col)))

(global-set-key (kbd "C-S-j") 'move-line-down)
 (global-set-key (kbd "C-S-k") 'move-line-up)

;;;;;;;;;;;;;;; section

(condition-case nil
 (progn
  (require 'multiple-cursors)
  (global-set-key (kbd "C-|") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (define-key mc/keymap (kbd "<return>") nil)
  )
 (error nil))

;;;;;;;;;;;;;;; section

(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;; section

(condition-case nil
 (progn
  (require 'projectile)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (projectile-global-mode)
  )
 (error nil))

(condition-case nil
 (progn
  (require 'helm)
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (helm-autoresize-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-M-x-fuzzy-match t)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (helm-mode 1)
  )
 (error nil))

;;;;;;;;;;;;;;; section

;; https://emacs.stackexchange.com/questions/212/is-there-a-way-to-use-query-replace-from-grep-ack-ag-output-modes/243#243

;;;;;;;;;;;;;;; section

(global-set-key (kbd "<f8>") 'speedbar)

;;;;;;;;;;;;;;; section

(condition-case nil
 (progn
  (dumb-jump-mode)
  )
 (error nil))

;;;;;;;;;;;;;;; section

(global-set-key (kbd "<f9>") 'flymd-flyit)

;;;;;;;;;;;;;;; section

;; https://github.com/krsoninikhil/dotfiles/tree/master/.emacs.d
(condition-case nil
 (progn
  (add-to-list 'load-path "~/.emacs.d/user-lisp")
  (require 'defuns)
  )
 (error nil))

;; End From https://krsoninikhil.github.io/2018/12/15/easy-moving-from-vscode-to-emacs/
