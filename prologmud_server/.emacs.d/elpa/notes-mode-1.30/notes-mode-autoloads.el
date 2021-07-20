;;; notes-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "notes-aux" "notes-aux.el" (0 0 0 0))
;;; Generated autoloads from notes-aux.el

(autoload 'notes-format-date "notes-aux" "\
Format the TIME up to be a notes-format date.
If no TIME is specified, use today's date.

\(fn &optional TIME)" nil nil)

(register-definition-prefixes "notes-aux" '("notes-file-to-"))

;;;***

;;;### (autoloads nil "notes-emacs" "notes-emacs.el" (0 0 0 0))
;;; Generated autoloads from notes-emacs.el

(register-definition-prefixes "notes-emacs" '("notes-platform-"))

;;;***

;;;### (autoloads nil "notes-first" "notes-first.el" (0 0 0 0))
;;; Generated autoloads from notes-first.el

(register-definition-prefixes "notes-first" '("notes-first-"))

;;;***

;;;### (autoloads nil "notes-index-mode" "notes-index-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from notes-index-mode.el

(autoload 'notes-index-todays-link "notes-index-mode" "\
* Open the notes file for today." t nil)

(autoload 'notes-index-mode "notes-index-mode" "\
Notes-index-mode with mouse support.

You may wish to change notes-bold-face and notes-use-font-lock.

There should be no need to add notes-index-mode to auto-mode-alist
since the index generation functions add code to the index file
which invokes notes-index-mode.

Key bindings are:
\\{notes-index-mode-map}

\(fn)" t nil)

(register-definition-prefixes "notes-index-mode" '("notes-index-"))

;;;***

;;;### (autoloads nil "notes-mode" "notes-mode.el" (0 0 0 0))
;;; Generated autoloads from notes-mode.el

(autoload 'notes-underline-line "notes-mode" "\
* Create a row of dashes as long as this line, or adjust the current underline." t nil)

(autoload 'notes-mode "notes-mode" "\
Enable notes-mode for a buffer.

Inside a notes buffer one can click on URLs and follow them to
other notes files.

See the file notes-variables.el for all customization options.
To change options, (require 'notes-variables) in your .emacs
and then change things.

Subjects in notes mode are lines beginning with an asterisk
and underlined with dashes.  Subjects can be completed 
with \\[notes-complete-subject] and are automatically underlined.

You may wish to add this code to your .emacs file:
    (add-to-list 'auto-mode-alist
  	(cons \"/9[0-9][0-9][0-9][0-9][0-9].?\\\\'\" 'notes-mode))
    (define-key global-map [?\\C-c ?n] 'notes-index-todays-link)
to automatically enter notes mode.

I have two suggestions for how to organize your notes files.
First, I collect my notes into a separate file per day.  (If you have
fewer notes, you may find once-per-week or month more suitable.)
Second, at the beginning of each file I have a subject \"* Today\".
Since every file has this subject, I can use its prev and next links
to easily move around the collection of files.

The key-bindings of this mode are:
\\{notes-mode-map}

\(fn)" t nil)

(register-definition-prefixes "notes-mode" '("notes-"))

;;;***

;;;### (autoloads nil "notes-url" "notes-url.el" (0 0 0 0))
;;; Generated autoloads from notes-url.el

(autoload 'notes-w3-url "notes-url" "\
Open a notes-url.  Handle simple URLs here, or call notes-w3-alternate-url.
Takes the URL as an argument.  Optionally you specify
WHERE the information should appear (either 'otherwindow or not,
defaults to not).
BEST-EFFORT causes notes-w3-url allows the tag portion of the URL to not
match.  If there's no tag match, it looks for the nearest matching prefix.

URLs optionally can begin with an URL: tag, which will be ignored.

notes-w3-url handles only <file://localhost/...> (or <file:///...>) URLs.
Other URLs it hands off to the routine in notes-w3-alternate-url
for processing.  If you use w3-mode, then
    (setq notes-w3-alternate-url 'w3-follow-link)
will have w3 handle tough URLs.

\(fn URL &optional WHERE BEST-EFFORT)" nil nil)

(autoload 'notes-w3-follow-link "notes-url" "\
* Follow the URL at the point.
Takes a PT to look at and a WHERE to open the URL ('otherwindow or nil).
This code works hard to recognize URLs based on context information.
URLs can be quoted by whitespace, beginning and end of lines,
or the official < and >.

As a special case we also recognize (and skip) the text \"prev:\"
and \"next:\" before the URL.  Notes-mode uses these fields to link
entries.

\(fn PT &optional WHERE)" t nil)

(autoload 'notes-w3-follow-link-mouse "notes-url" "\
* Follow the URL where the mouse is.

\(fn E)" t nil)

(register-definition-prefixes "notes-url" '("notes-"))

;;;***

;;;### (autoloads nil "notes-variables" "notes-variables.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from notes-variables.el

(register-definition-prefixes "notes-variables" '("notes-"))

;;;***

;;;### (autoloads nil "notes-xemacs" "notes-xemacs.el" (0 0 0 0))
;;; Generated autoloads from notes-xemacs.el

(register-definition-prefixes "notes-xemacs" '("notes-platform-"))

;;;***

;;;### (autoloads nil nil ("notes-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; notes-mode-autoloads.el ends here
