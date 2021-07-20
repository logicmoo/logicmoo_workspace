;;; org-shoplist-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-shoplist" "org-shoplist.el" (0 0 0 0))
;;; Generated autoloads from org-shoplist.el

(autoload 'org-shoplist-init "org-shoplist" "\
Setting the todo-keywords for current file." t nil)

(autoload 'org-shoplist-unmark-all "org-shoplist" "\
Unmark all recipes which are marked with ‘ORG-SHOPLIST-KEYWORD’." t nil)

(autoload 'org-shoplist-recipe-set-factor "org-shoplist" "\
Set ‘FACTOR’ with property-name ‘ORG-SHOPLIST-FACTOR-PROPERTY-NAME’ on current recipe.

\(fn FACTOR)" t nil)

(autoload 'org-shoplist-factor-down "org-shoplist" "\
Decrement the factor-property of current header." t nil)

(autoload 'org-shoplist-factor-up "org-shoplist" "\
Increment the factor-property of current header." t nil)

(register-definition-prefixes "org-shoplist" '("org-shoplist"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-shoplist-autoloads.el ends here
