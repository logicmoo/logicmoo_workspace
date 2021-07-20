;;; org-kindle-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-kindle" "org-kindle.el" (0 0 0 0))
;;; Generated autoloads from org-kindle.el

(autoload 'org-kindle--read-device-info "org-kindle" "\
Match name in shell command lsusb listed out devices." nil nil)

(autoload 'org-kindle--detect-format "org-kindle" "\
Detect plugged in device's ebook format." nil nil)

(autoload 'org-kindle--mount-path "org-kindle" "\
Get Linux general mount path." nil nil)

(autoload 'org-kindle--detect-directory "org-kindle" "\
Detect plugged in device directory of saving ebook." nil nil)

(autoload 'org-kindle-send-to-device "org-kindle" "\
Send `org-mode' ebook file: link to external devices with corresponding formats." t nil)

(register-definition-prefixes "org-kindle" '("org-kindle-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-kindle-autoloads.el ends here
