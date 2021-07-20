;;; org-dropbox-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-dropbox" "org-dropbox.el" (0 0 0 0))
;;; Generated autoloads from org-dropbox.el

(let ((loads (get 'org-dropbox 'custom-loads))) (if (member '"org-dropbox" loads) nil (put 'org-dropbox 'custom-loads (cons '"org-dropbox" loads))))

(defconst org-dropbox-version "1.2" "\
Version of the org-dropbox package.")

(autoload 'org-dropbox-version "org-dropbox" "\
Version of the org-dropbox package." t nil)

(autoload 'org-dropbox-mode "org-dropbox" "\
Minor mode adding Dropbox notes to datetree.

This is a minor mode.  If called interactively, toggle the
`Org-Dropbox mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-dropbox-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-dropbox-autoloads.el ends here
