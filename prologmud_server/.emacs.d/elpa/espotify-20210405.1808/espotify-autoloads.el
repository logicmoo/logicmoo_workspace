;;; espotify-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "espotify" "espotify.el" (0 0 0 0))
;;; Generated autoloads from espotify.el

(autoload 'espotify-format-item "espotify" "\
Format the search result ITEM as a string with additional metadata.
The metadata will be accessible via `espotify-candidate-metadata'.

\(fn ITEM)" nil nil)

(autoload 'espotify-candidate-metadata "espotify" "\
Extract from CAND (as returned by `espotify-format-item') its metadata.

\(fn CAND)" nil nil)

(autoload 'espotify-play-uri "espotify" "\
Use a DBUS call to play a URI denoting a resource.

\(fn URI)" nil nil)

(autoload 'espotify-play-candidate "espotify" "\
If CAND is a formatted item string and it has a URL, play it.

\(fn CAND)" nil nil)

(autoload 'espotify-play-pause "espotify" "\
Toggle default Spotify player via DBUS." t nil)

(autoload 'espotify-next "espotify" "\
Tell default Spotify player to play next track via DBUS." t nil)

(autoload 'espotify-previous "espotify" "\
Tell default Spotify player to play previous track via DBUS." t nil)

(autoload 'espotify-show-candidate-info "espotify" "\
Show low-level info (an alist) about CANDIDATE.

\(fn CANDIDATE)" nil nil)

(autoload 'espotify-play-candidate-album "espotify" "\
Play album associated with selected CANDIDATE.

\(fn CANDIDATE)" nil nil)

(autoload 'espotify-yank-candidate-url "espotify" "\
Add to kill ring the Spotify URL of this CANDIDATE.

\(fn CANDIDATE)" nil nil)

(register-definition-prefixes "espotify" '("espotify-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; espotify-autoloads.el ends here
