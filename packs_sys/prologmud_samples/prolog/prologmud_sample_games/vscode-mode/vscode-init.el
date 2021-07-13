(load (concat vscode-mode-dir "/frdcsa-excerpts.el"))
(load (concat vscode-mode-dir "/external/easy-moving-from-vscode-to-emacs-1.el"))

(defun vscode-mode-install-required-packages ()
 ""
 (interactive)
 (if (yes-or-no-p "Are you sure you want to install required packages for vscode-mode? ")
  (progn
   (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
   (load (concat vscode-mode-dir "/external/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name.el"))
   (when (not package-archive-contents)
    (package-refresh-contents))
   (ensure-package-installed 'multiple-cursors 'projectile 'helm 'dumb-jump 'company)
   )))

(if (yes-or-no-p "Install required packages for vscode-mode? ")
 (vscode-mode-install-required-packages))

(load (concat vscode-mode-dir "/external/code/defuns.el"))
(load (concat vscode-mode-dir "/external/easy-moving-from-vscode-to-emacs-2.el"))
(load (concat vscode-mode-dir "/vscode-mode.el"))

(provide 'vscode-init)
