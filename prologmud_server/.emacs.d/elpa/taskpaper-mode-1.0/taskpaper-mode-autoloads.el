;;; taskpaper-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "taskpaper-mode" "taskpaper-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from taskpaper-mode.el

(autoload 'taskpaper-add-entry "taskpaper-mode" "\
Add entry TEXT to LOCATION in FILE.

When FILE is specified, visit it and set this buffer as target
buffer, otherwise fall back to the current buffer.

Prompt user for entry TEXT and add it as child of the top-level
LOCATION item. The entry is filed below the target location as a
subitem. Depending on `taskpaper-reverse-note-order', it will be
either the first or last subitem. When the location is omitted,
the item is simply filed at the end of the file, as top-level
item.

\(fn &optional TEXT LOCATION FILE)" t nil)

(autoload 'taskpaper-mode "taskpaper-mode" "\
Major mode for editing and querying files in TaskPaper format.
TaskPaper mode is implemented on top of Outline mode. Turning on
TaskPaper mode runs the normal hook `text-mode-hook', and then
`outline-mode-hook' and `taskpaper-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.taskpaper\\'" . taskpaper-mode))

(autoload 'taskpaper-agenda-search "taskpaper-mode" "\
Promt for query string and build agenda." t nil)

(autoload 'taskpaper-agenda-select "taskpaper-mode" "\
Promts for query selection and build agenda." t nil)

(register-definition-prefixes "taskpaper-mode" '("taskpaper-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; taskpaper-mode-autoloads.el ends here
