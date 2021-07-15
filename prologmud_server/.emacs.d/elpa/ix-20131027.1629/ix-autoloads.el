;;; ix-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ix" "ix.el" (0 0 0 0))
;;; Generated autoloads from ix.el

(autoload 'ix-delete "ix" "\
Delete a post, this requires you to be logged in. Specify the
   post id or the url

\(fn IX-URL)" t nil)

(autoload 'ix "ix" "\
Paste the region at http://ix.io, url returned is saved to the
   kill ring. It is recommended to set up a username and token by
   configuring the variables `ix-user' and `ix-token'

\(fn START END)" t nil)

(autoload 'ix-browse "ix" "\
Browse a paste from http://ix.io, given an input of either a
   post identifier or the complete url. The output is displayed in a
   special buffer, *ix*. If the buffer needs to be saved for some reason
   it has to be done manually

\(fn IX-URL)" t nil)

(register-definition-prefixes "ix" '("ix-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ix-autoloads.el ends here
