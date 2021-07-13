;; Start From https://krsoninikhil.github.io/2018/12/15/easy-moving-from-vscode-to-emacs/

(require 'package)
(setq package-archives
 '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
   ("MELPA Stable" . "https://stable.melpa.org/packages/")
   ("MELPA"        . "https://melpa.org/packages/"))
 package-archive-priorities
 '(("GNU ELPA"     . 10)
   ("MELPA Stable" . 5)
   ("MELPA"        . 0)))
(package-initialize)

;; End From https://krsoninikhil.github.io/2018/12/15/easy-moving-from-vscode-to-emacs/
