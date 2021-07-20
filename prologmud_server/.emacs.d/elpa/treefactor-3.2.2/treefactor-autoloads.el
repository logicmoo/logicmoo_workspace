;;; treefactor-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "treefactor" "treefactor.el" (0 0 0 0))
;;; Generated autoloads from treefactor.el

(autoload 'treefactor-throw "treefactor" "\
Refile text/file to target in next window COUNT times.
Select a line from target list using `isearch' then `avy'.

\(fn &optional COUNT)" t nil)

(autoload 'treefactor-up "treefactor" "\
Refile file or text one step upwards, COUNT times.

Text will go to an Inbox.org of the same directory level, or one
higher if already in an Inbox.org

File will go one directory level higher beneath a 0-Inbox/,
unless already under 0-Inbox/, in which case two higher beneath a
0-Inbox/

\(fn &optional COUNT)" t nil)

(autoload 'treefactor-delete-this-buffer-and-file "treefactor" "\
Delete file visited by current buffer and kill buffer." t nil)

(autoload 'treefactor-org-duplicate-heading-to-other-window "treefactor" "\
Append heading at point to end of next window's buffer." t nil)

(autoload 'treefactor-org-store-link-fold-drawer "treefactor" "\
Store an org link to a heading, and fold the drawer." t nil)

(autoload 'treefactor-org-dired-zinks "treefactor" "\
Make Zinks.org.  Insert org-id link.

Link title's path is relative to `vc-root-dir' if present,
else `user-home-directory'." t nil)

(register-definition-prefixes "treefactor" '("treefactor-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; treefactor-autoloads.el ends here
