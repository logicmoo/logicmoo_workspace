;;; svg-clock-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "svg-clock" "svg-clock.el" (0 0 0 0))
;;; Generated autoloads from svg-clock.el

(autoload 'svg-clock-insert "svg-clock" "\
Insert a self-updating image displaying an analog clock at point.
Optional argument SIZE the size of the clock in pixels.
Optional argument FOREGROUND the foreground color.
Optional argument BACKGROUND the background color.
Optional argument OFFSET the offset in seconds between current and displayed
time.
Optional argument NO-SECONDS says whether to do a seconds hand.
Optional argument NO-FACE says whether to decorate the face.

\(fn &optional SIZE FOREGROUND BACKGROUND OFFSET NO-SECONDS NO-FACE)" nil nil)

(autoload 'svg-clock "svg-clock" "\
Start/stop the svg clock.

\(fn &key SIZE FOREGROUND BACKGROUND NO-SECONDS NO-FACE)" t nil)

(register-definition-prefixes "svg-clock" '("svg-clock-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; svg-clock-autoloads.el ends here
