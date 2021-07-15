;;; cpio-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cpio-affiliated-buffers" "cpio-affiliated-buffers.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cpio-affiliated-buffers.el

(register-definition-prefixes "cpio-affiliated-buffers" '("*cab-" "OBS-" "cab-"))

;;;***

;;;### (autoloads nil "cpio-bin" "cpio-bin.el" (0 0 0 0))
;;; Generated autoloads from cpio-bin.el

(register-definition-prefixes "cpio-bin" '("*cpio-bin-" "cpio-bin-"))

;;;***

;;;### (autoloads nil "cpio-crc" "cpio-crc.el" (0 0 0 0))
;;; Generated autoloads from cpio-crc.el

(register-definition-prefixes "cpio-crc" '("*cpio-crc-" "cpio-"))

;;;***

;;;### (autoloads nil "cpio-dired" "cpio-dired.el" (0 0 0 0))
;;; Generated autoloads from cpio-dired.el

(register-definition-prefixes "cpio-dired" '("*cpio-dir" "*mon-re*" "cpio-" "snarf-defuns" "sort-defuns"))

;;;***

;;;### (autoloads nil "cpio-entry-contents-mode" "cpio-entry-contents-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cpio-entry-contents-mode.el

(register-definition-prefixes "cpio-entry-contents-mode" '("*cpio-entry-contents-mode-map*" "cpio-entry-contents-" "entry-setup"))

;;;***

;;;### (autoloads nil "cpio-generic" "cpio-generic.el" (0 0 0 0))
;;; Generated autoloads from cpio-generic.el

(register-definition-prefixes "cpio-generic" '("*cg-in" "OBS-cg-hex-format-" "cg-" "cpio-" "encode-human-time" "month-to-number" "test-encode-human-time" "with-writable-buffer"))

;;;***

;;;### (autoloads nil "cpio-mode" "cpio-mode.el" (0 0 0 0))
;;; Generated autoloads from cpio-mode.el

(autoload 'cpio-mode-find-file-hook "cpio-mode" "\
find-file hook to detect if a file is likely a cpio archive.
If it is, then put it under cpio-mode." nil nil)

(autoload 'cpio-discern-archive-type "cpio-mode" "\
Return a symbol reflecting the type of the cpio archive in the current buffer.
Values are `bin', `newc', `odc', `crc', `tar', `ustar', `hpbin', `hpodc',
or nil if the current buffer does not begin with a cpio entry header." nil nil)

(autoload 'cpio-mode "cpio-mode" "\
Treat cpio archives like file systems with a dired UI.

\(fn)" t nil)

(register-definition-prefixes "cpio-mode" '("*cpio-" "OBS-cpio-" "cpio-"))

;;;***

;;;### (autoloads nil "cpio-modes" "cpio-modes.el" (0 0 0 0))
;;; Generated autoloads from cpio-modes.el

(register-definition-prefixes "cpio-modes" '("*cpio-modes-" "UNUSED-*cpio-low-mode-bits*" "cpio-" "s-i"))

;;;***

;;;### (autoloads nil "cpio-newc" "cpio-newc.el" (0 0 0 0))
;;; Generated autoloads from cpio-newc.el

(register-definition-prefixes "cpio-newc" '("*cpio-newc-" "*locations-delay*" "cpio-newc-" "insert-table-header-maybe" "locations"))

;;;***

;;;### (autoloads nil "cpio-odc" "cpio-odc.el" (0 0 0 0))
;;; Generated autoloads from cpio-odc.el

(register-definition-prefixes "cpio-odc" '("*cpio-odc-" "cpio-odc-"))

;;;***

;;;### (autoloads nil nil ("cpio-entry-header.el" "cpio-hpbin.el"
;;;;;;  "cpio-hpodc.el" "cpio-mode-pkg.el" "cpio-tar.el" "cpio-ustar.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cpio-mode-autoloads.el ends here
