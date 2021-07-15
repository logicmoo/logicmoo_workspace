;;; orgalist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orgalist" "orgalist.el" (0 0 0 0))
;;; Generated autoloads from orgalist.el

(autoload 'orgalist-mode "orgalist" "\
Toggle Org-like lists and their relative commands.

With a prefix argument ARG, enable Auto Fill mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Orgalist mode is enabled, any line beginning with \"-\",
\"+\", \"1.\" or \"a.\" followed by a space starts a list.  You
can then operate locally on the list, e.g., to insert new items,
move items or sort them.  See below for details.

Moreover, you can add check-boxes to items

  - [ ] A checkbox, toggled with `C-c C-c'

turn an unordered list into a description list

  - term :: description

and control numbering in an ordered list

  4. [@4] a forced numbered item

key             binding
---             -------
M-<RET>         `orgalist-insert-item'
M-<UP>          `orgalist-previous-item'
M-<DOWN>        `orgalist-next-item'
M-S-<UP>        `orgalist-move-item-up'
M-S-<DOWN>      `orgalist-move-item-down'
M-<LEFT>        `orgalist-outdent-item'
M-<RIGHT>       `orgalist-indent-item'
M-S-<LEFT>      `orgalist-outdent-item-tree'
M-S-<RIGHT>     `orgalist-indent-item-tree'
C-c -           `orgalist-cycle-bullet'
C-c ^           `orgalist-sort-items'
C-c C-c         `orgalist-check-item'

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orgalist" '("orgalist-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orgalist-autoloads.el ends here
