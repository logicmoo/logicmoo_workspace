# vscode-mode
VSCode compatibility globalized minor mode that provides VSCode-style keybindings in Emacs.



USE ONLY AT YOUR OWN RISK:

This mode installs ELisp packages.
This mode is UNDER CONSTRUCTION.



Replace the /var/lib/myfrdcsa/collaborative/git/vscode-mode with the
correct path to the vscode-mode dir, and add to .emacs or whatever
startup file you use:

```
(setq vscode-mode-dir "/var/lib/myfrdcsa/collaborative/git/vscode-mode")
(add-to-list 'load-path vscode-mode-dir)
(require 'vscode-init)
```

Optionally, also add this to the following to get it to run on startup:

```
(add-to-list 'emacs-startup-hook 'global-vscode-mode-on)
```

Then enable global-vscode-mode with C-c v
