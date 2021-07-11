;;; org-contrib-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ob-arduino" "ob-arduino.el" (0 0 0 0))
;;; Generated autoloads from ob-arduino.el

(autoload 'org-babel-execute:arduino "ob-arduino" "\
org-babel arduino hook.

\(fn BODY PARAMS)" nil nil)

(eval-after-load 'org '(add-to-list 'org-src-lang-modes '("arduino" . arduino)))

(register-definition-prefixes "ob-arduino" '("ob-arduino:" "org-babel-default-header-args:sclang"))

;;;***

;;;### (autoloads nil "ob-clojure-literate" "ob-clojure-literate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ob-clojure-literate.el

(defvar ob-clojure-literate-auto-jackin-p nil "\
Auto jack in ob-clojure project.
Don't auto jack in by default for not rude.")

(custom-autoload 'ob-clojure-literate-auto-jackin-p "ob-clojure-literate" t)

(autoload 'ob-clojure-literate-specify-session "ob-clojure-literate" "\
Specify ob-clojure header argument :session with value selected from a list of available sessions." t nil)

(autoload 'ob-clojure-literate-auto-jackin "ob-clojure-literate" "\
Auto setup ob-clojure-literate scaffold and jack-in Clojure project." t nil)

(autoload 'ob-clojure-literate-enable "ob-clojure-literate" "\
Enable Org-mode buffer locally for `ob-clojure-literate'." nil nil)

(autoload 'ob-clojure-literate-disable "ob-clojure-literate" "\
Disable Org-mode buffer locally for `ob-clojure-literate'." nil nil)

(if ob-clojure-literate-auto-jackin-p (ob-clojure-literate-auto-jackin))

(autoload 'ob-clojure-literate-mode "ob-clojure-literate" "\
A minor mode to toggle `ob-clojure-literate'.

This is a minor mode.  If called interactively, toggle the
`Ob-Clojure-Literate mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `ob-clojure-literate-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ob-clojure-literate" '("ob-clojure-literate-"))

;;;***

;;;### (autoloads nil "ob-csharp" "ob-csharp.el" (0 0 0 0))
;;; Generated autoloads from ob-csharp.el

(register-definition-prefixes "ob-csharp" '("org-babel-"))

;;;***

;;;### (autoloads nil "ob-eukleides" "ob-eukleides.el" (0 0 0 0))
;;; Generated autoloads from ob-eukleides.el

(register-definition-prefixes "ob-eukleides" '("org-"))

;;;***

;;;### (autoloads nil "ob-fomus" "ob-fomus.el" (0 0 0 0))
;;; Generated autoloads from ob-fomus.el

(register-definition-prefixes "ob-fomus" '("org-babel-"))

;;;***

;;;### (autoloads nil "ob-julia" "ob-julia.el" (0 0 0 0))
;;; Generated autoloads from ob-julia.el

(register-definition-prefixes "ob-julia" '("org-babel-"))

;;;***

;;;### (autoloads nil "ob-mathematica" "ob-mathematica.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ob-mathematica.el

(register-definition-prefixes "ob-mathematica" '("org-babel-"))

;;;***

;;;### (autoloads nil "ob-mathomatic" "ob-mathomatic.el" (0 0 0 0))
;;; Generated autoloads from ob-mathomatic.el

(register-definition-prefixes "ob-mathomatic" '("org-babel-"))

;;;***

;;;### (autoloads nil "ob-oz" "ob-oz.el" (0 0 0 0))
;;; Generated autoloads from ob-oz.el

(register-definition-prefixes "ob-oz" '("org-babel-" "oz-send-string-expression"))

;;;***

;;;### (autoloads nil "ob-php" "ob-php.el" (0 0 0 0))
;;; Generated autoloads from ob-php.el

(autoload 'org-babel-execute:php "ob-php" "\
Orgmode Babel PHP evaluate function for `BODY' with `PARAMS'.

\(fn BODY PARAMS)" nil nil)

(eval-after-load 'org '(add-to-list 'org-src-lang-modes '("php" . php)))

(register-definition-prefixes "ob-php" '("ob-php:inf-php-buffer" "org-babel-"))

;;;***

;;;### (autoloads nil "ob-redis" "ob-redis.el" (0 0 0 0))
;;; Generated autoloads from ob-redis.el

(autoload 'org-babel-execute:redis "ob-redis" "\
org-babel redis hook.

\(fn BODY PARAMS)" nil nil)

(eval-after-load 'org '(add-to-list 'org-src-lang-modes '("redis" . redis)))

(register-definition-prefixes "ob-redis" '("ob-redis:default-db"))

;;;***

;;;### (autoloads nil "ob-sclang" "ob-sclang.el" (0 0 0 0))
;;; Generated autoloads from ob-sclang.el

(autoload 'org-babel-execute:sclang "ob-sclang" "\
Org-mode Babel sclang hook for evaluate `BODY' with `PARAMS'.

\(fn BODY PARAMS)" nil nil)

(register-definition-prefixes "ob-sclang" '("org-babel-default-header-args:sclang"))

;;;***

;;;### (autoloads nil "ob-smiles" "ob-smiles.el" (0 0 0 0))
;;; Generated autoloads from ob-smiles.el

(register-definition-prefixes "ob-smiles" '("molecule-" "org-babel-execute:smiles"))

;;;***

;;;### (autoloads nil "ob-spice" "ob-spice.el" (0 0 0 0))
;;; Generated autoloads from ob-spice.el

(autoload 'org-babel-execute:spice "ob-spice" "\
Execute a block of Spice code `BODY' with org-babel and `PARAMS'.

\(fn BODY PARAMS)" nil nil)

(register-definition-prefixes "ob-spice" '("ob-spice-concat" "org-babel-expand-body:spice"))

;;;***

;;;### (autoloads nil "ob-stata" "ob-stata.el" (0 0 0 0))
;;; Generated autoloads from ob-stata.el

(register-definition-prefixes "ob-stata" '("org-babel-"))

;;;***

;;;### (autoloads nil "ob-tcl" "ob-tcl.el" (0 0 0 0))
;;; Generated autoloads from ob-tcl.el

(register-definition-prefixes "ob-tcl" '("org-babel-"))

;;;***

;;;### (autoloads nil "ob-vbnet" "ob-vbnet.el" (0 0 0 0))
;;; Generated autoloads from ob-vbnet.el

(register-definition-prefixes "ob-vbnet" '("org-babel-"))

;;;***

;;;### (autoloads nil "ol-bookmark" "ol-bookmark.el" (0 0 0 0))
;;; Generated autoloads from ol-bookmark.el

(register-definition-prefixes "ol-bookmark" '("org-bookmark-"))

;;;***

;;;### (autoloads nil "ol-elisp-symbol" "ol-elisp-symbol.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ol-elisp-symbol.el

(register-definition-prefixes "ol-elisp-symbol" '("org-elisp-symbol-"))

;;;***

;;;### (autoloads nil "ol-git-link" "ol-git-link.el" (0 0 0 0))
;;; Generated autoloads from ol-git-link.el

(register-definition-prefixes "ol-git-link" '("org-git"))

;;;***

;;;### (autoloads nil "ol-man" "ol-man.el" (0 0 0 0))
;;; Generated autoloads from ol-man.el

(register-definition-prefixes "ol-man" '("org-man-"))

;;;***

;;;### (autoloads nil "ol-mew" "ol-mew.el" (0 0 0 0))
;;; Generated autoloads from ol-mew.el

(register-definition-prefixes "ol-mew" '("org-mew-"))

;;;***

;;;### (autoloads nil "ol-notmuch" "ol-notmuch.el" (0 0 0 0))
;;; Generated autoloads from ol-notmuch.el

(register-definition-prefixes "ol-notmuch" '("org-notmuch-"))

;;;***

;;;### (autoloads nil "ol-vm" "ol-vm.el" (0 0 0 0))
;;; Generated autoloads from ol-vm.el

(register-definition-prefixes "ol-vm" '("org-vm-"))

;;;***

;;;### (autoloads nil "ol-wl" "ol-wl.el" (0 0 0 0))
;;; Generated autoloads from ol-wl.el

(register-definition-prefixes "ol-wl" '("org-wl-"))

;;;***

;;;### (autoloads nil "org-annotate-file" "org-annotate-file.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-annotate-file.el

(autoload 'org-annotate-file "org-annotate-file" "\
Visit `org-annotate-file-storage-file` and add a new annotation section.
The annotation is opened at the new section which will be referencing
the point in the current file." t nil)

(autoload 'org-annotate-file-show-section "org-annotate-file" "\
Add or show annotation entry in STORAGE-FILE and return the buffer.
The annotation will link to ANNOTATED-BUFFER if specified,
  otherwise the current buffer is used.

\(fn STORAGE-FILE &optional ANNOTATED-BUFFER)" nil nil)

(register-definition-prefixes "org-annotate-file" '("org-annotate-file-"))

;;;***

;;;### (autoloads nil "org-attach-embedded-images" "org-attach-embedded-images.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-attach-embedded-images.el

(autoload 'org-attach-embedded-images-in-subtree "org-attach-embedded-images" "\
Save the displayed images as attachments and insert links to them." t nil)

(register-definition-prefixes "org-attach-embedded-images" '("org-attach-embedded-images--"))

;;;***

;;;### (autoloads nil "org-bibtex-extras" "org-bibtex-extras.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-bibtex-extras.el

(register-definition-prefixes "org-bibtex-extras" '("obe-"))

;;;***

;;;### (autoloads nil "org-checklist" "org-checklist.el" (0 0 0 0))
;;; Generated autoloads from org-checklist.el

(register-definition-prefixes "org-checklist" '("org-"))

;;;***

;;;### (autoloads nil "org-choose" "org-choose.el" (0 0 0 0))
;;; Generated autoloads from org-choose.el

(register-definition-prefixes "org-choose" '("org-choose-"))

;;;***

;;;### (autoloads nil "org-collector" "org-collector.el" (0 0 0 0))
;;; Generated autoloads from org-collector.el

(register-definition-prefixes "org-collector" '("and-rest" "org-"))

;;;***

;;;### (autoloads nil "org-contacts" "org-contacts.el" (0 0 0 0))
;;; Generated autoloads from org-contacts.el

(autoload 'org-contacts "org-contacts" "\
Create agenda view for contacts matching NAME.

\(fn NAME)" t nil)

(register-definition-prefixes "org-contacts" '("erc-nicknames-list" "org-co"))

;;;***

;;;### (autoloads nil "org-depend" "org-depend.el" (0 0 0 0))
;;; Generated autoloads from org-depend.el

(register-definition-prefixes "org-depend" '("org-depend-"))

;;;***

;;;### (autoloads nil "org-effectiveness" "org-effectiveness.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-effectiveness.el

(register-definition-prefixes "org-effectiveness" '("org-effectiveness-"))

;;;***

;;;### (autoloads nil "org-eldoc" "org-eldoc.el" (0 0 0 0))
;;; Generated autoloads from org-eldoc.el

(autoload 'org-eldoc-load "org-eldoc" "\
Set up org-eldoc documentation function." t nil)

(add-hook 'org-mode-hook #'org-eldoc-load)

(register-definition-prefixes "org-eldoc" '("org-eldoc-"))

;;;***

;;;### (autoloads nil "org-eval" "org-eval.el" (0 0 0 0))
;;; Generated autoloads from org-eval.el

(register-definition-prefixes "org-eval" '("org-eval-"))

;;;***

;;;### (autoloads nil "org-eval-light" "org-eval-light.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-eval-light.el

(register-definition-prefixes "org-eval-light" '("org-eval-light-"))

;;;***

;;;### (autoloads nil "org-expiry" "org-expiry.el" (0 0 0 0))
;;; Generated autoloads from org-expiry.el

(register-definition-prefixes "org-expiry" '("org-expiry-"))

;;;***

;;;### (autoloads nil "org-interactive-query" "org-interactive-query.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-interactive-query.el

(register-definition-prefixes "org-interactive-query" '("org-agenda-query-"))

;;;***

;;;### (autoloads nil "org-invoice" "org-invoice.el" (0 0 0 0))
;;; Generated autoloads from org-invoice.el

(register-definition-prefixes "org-invoice" '("org-"))

;;;***

;;;### (autoloads nil "org-learn" "org-learn.el" (0 0 0 0))
;;; Generated autoloads from org-learn.el

(register-definition-prefixes "org-learn" '("calculate-new-optimal-factor" "determine-next-interval" "get-optimal-factor" "initial-" "inter-repetition-interval" "modify-" "org-" "set-optimal-factor"))

;;;***

;;;### (autoloads nil "org-license" "org-license.el" (0 0 0 0))
;;; Generated autoloads from org-license.el

(register-definition-prefixes "org-license" '("org-license-"))

;;;***

;;;### (autoloads nil "org-link-edit" "org-link-edit.el" (0 0 0 0))
;;; Generated autoloads from org-link-edit.el

(autoload 'org-link-edit-forward-slurp "org-link-edit" "\
Slurp N trailing blobs into link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The [[https://orgmode.org/][Org mode site]]

A blob is a block of non-whitespace characters.  When slurping
forward, trailing punctuation characters are not considered part
of a blob.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp leading blobs instead of trailing blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-backward-slurp "org-link-edit" "\
Slurp N leading blobs into link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  [[https://orgmode.org/][The Org mode]] site

A blob is a block of non-whitespace characters.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp trailing blobs instead of leading blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-forward-barf "org-link-edit" "\
Barf N trailing blobs from link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The [[https://orgmode.org/][Org]] mode site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf leading blobs instead of trailing blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-backward-barf "org-link-edit" "\
Barf N leading blobs from link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The Org [[https://orgmode.org/][mode]] site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf trailing blobs instead of leading blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-transport-next-link "org-link-edit" "\
Move the next link to point.

If the region is active, use the selected text as the link's
description.  Otherwise, use the word at point.

With prefix argument PREVIOUS, move the previous link instead of
the next link.

Non-interactively, use the text between BEG and END as the
description, moving the next (or previous) link relative to BEG
and END.  By default, refuse to overwrite an existing
description.  If OVERWRITE is `ask', prompt for confirmation
before overwriting; for any other non-nil value, overwrite
without asking.

\(fn &optional PREVIOUS BEG END OVERWRITE)" t nil)

(register-definition-prefixes "org-link-edit" '("org-link-edit--"))

;;;***

;;;### (autoloads nil "org-mac-iCal" "org-mac-iCal.el" (0 0 0 0))
;;; Generated autoloads from org-mac-iCal.el

(register-definition-prefixes "org-mac-iCal" '("omi-" "org-mac-iCal"))

;;;***

;;;### (autoloads nil "org-mac-link" "org-mac-link.el" (0 0 0 0))
;;; Generated autoloads from org-mac-link.el

(autoload 'org-mac-grab-link "org-mac-link" "\
Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point." t nil)

(autoload 'org-mac-firefox-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-firefox-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-vimperator-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-vimperator-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-chrome-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-chrome-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-brave-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-brave-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-safari-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-safari-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-together-get-selected "org-mac-link" nil t nil)

(autoload 'org-mac-together-insert-selected "org-mac-link" nil t nil)

(autoload 'org-mac-finder-item-get-selected "org-mac-link" nil t nil)

(autoload 'org-mac-finder-insert-selected "org-mac-link" nil t nil)

(autoload 'org-mac-addressbook-item-get-selected "org-mac-link" nil t nil)

(autoload 'org-mac-addressbook-insert-selected "org-mac-link" nil t nil)

(autoload 'org-mac-skim-get-page "org-mac-link" nil t nil)

(autoload 'org-mac-skim-insert-page "org-mac-link" nil t nil)

(autoload 'org-mac-acrobat-get-page "org-mac-link" nil t nil)

(autoload 'org-mac-acrobat-insert-page "org-mac-link" nil t nil)

(autoload 'org-mac-outlook-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject of the
messages in Microsoft Outlook.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-outlook-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject
of the active mail in Microsoft Outlook.app and make a link out
of it." t nil)

(autoload 'org-mac-outlook-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all mac-outlook:// links within
heading's first level.  If heading doesn't exist, create it at
point-max.  Insert list of mac-outlook:// links to flagged mail
after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(autoload 'org-mac-evernote-note-insert-selected "org-mac-link" "\
Insert a link to the notes currently selected in Evernote.app.
This will use AppleScript to get the note id and the title of the
note(s) in Evernote.app and make a link out of it/them." t nil)

(autoload 'org-mac-devonthink-item-insert-selected "org-mac-link" "\
Insert a link to the item(s) currently selected in DEVONthink Pro Office.
This will use AppleScript to get the `uuid'(s) and the name(s) of the
selected items in DEVONthink Pro Office and make link(s) out of it/them." t nil)

(autoload 'org-mac-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Mail.app.
This will use AppleScript to get the message-id and the subject of the
messages in Mail.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Mail.app.
This will use AppleScript to get the message-id and the subject of the
active mail in Mail.app and make a link out of it." t nil)

(autoload 'org-mac-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all message:// links within heading's first
level.  If heading doesn't exist, create it at point-max.  Insert
list of message:// links to flagged mail after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(register-definition-prefixes "org-mac-link" '("as-get-s" "org-"))

;;;***

;;;### (autoloads nil "org-mairix" "org-mairix.el" (0 0 0 0))
;;; Generated autoloads from org-mairix.el

(register-definition-prefixes "org-mairix" '("org-"))

;;;***

;;;### (autoloads nil "org-notify" "org-notify.el" (0 0 0 0))
;;; Generated autoloads from org-notify.el

(register-definition-prefixes "org-notify" '("org-notify-"))

;;;***

;;;### (autoloads nil "org-panel" "org-panel.el" (0 0 0 0))
;;; Generated autoloads from org-panel.el

(register-definition-prefixes "org-panel" '("orgpan-"))

;;;***

;;;### (autoloads nil "org-passwords" "org-passwords.el" (0 0 0 0))
;;; Generated autoloads from org-passwords.el

(autoload 'org-passwords-mode "org-passwords" "\
Mode for storing passwords

\(fn)" t nil)

(autoload 'org-passwords "org-passwords" "\
Open the password file. Open the password file defined by the
variable `org-password-file' in read-only mode and kill that
buffer later according to the value of the variable
`org-passwords-time-opened'. It also adds the `org-password-file'
to the auto-mode-alist so that it is opened with its mode being
`org-passwords-mode'.

With prefix arg ARG, the command does not set up a timer to kill the buffer.

With a double prefix arg \\[universal-argument] \\[universal-argument], open the file for editing.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-passwords" '("org-passwords-"))

;;;***

;;;### (autoloads nil "org-registry" "org-registry.el" (0 0 0 0))
;;; Generated autoloads from org-registry.el

(autoload 'org-registry-show "org-registry" "\
Show Org files where there are links pointing to the current
buffer.

\(fn &optional VISIT)" t nil)

(autoload 'org-registry-visit "org-registry" "\
If an Org file contains a link to the current location, visit
this file." t nil)

(autoload 'org-registry-initialize "org-registry" "\
Initialize `org-registry-alist'.
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'.

\(fn &optional FROM-SCRATCH)" t nil)

(autoload 'org-registry-insinuate "org-registry" "\
Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit." t nil)

(autoload 'org-registry-update "org-registry" "\
Update the registry for the current Org file." t nil)

(register-definition-prefixes "org-registry" '("org-registry-"))

;;;***

;;;### (autoloads nil "org-screen" "org-screen.el" (0 0 0 0))
;;; Generated autoloads from org-screen.el

(register-definition-prefixes "org-screen" '("org-screen"))

;;;***

;;;### (autoloads nil "org-screenshot" "org-screenshot.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-screenshot.el

(autoload 'org-screenshot-take "org-screenshot" "\
Take a screenshot and insert link to it at point, if image
display is already on (see \\[org-toggle-inline-images])
screenshot will be displayed as an image

Screen area for the screenshot is selected with the mouse, left
click on a window screenshots that window, while left click and
drag selects a region. Pressing any key cancels the screen shot

With `C-u' universal argument waits one second after target is
selected before taking the screenshot. With double `C-u' wait two
seconds.

With triple `C-u' wait 3 seconds, and also rings the bell when
screenshot is done, any more `C-u' after that increases delay by
2 seconds

\(fn &optional DELAY)" t nil)

(autoload 'org-screenshot-rotate-prev "org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-rotate-next "org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-show-unused "org-screenshot" "\
Open A Dired buffer with unused screenshots marked" t nil)

(register-definition-prefixes "org-screenshot" '("org-screenshot-"))

;;;***

;;;### (autoloads nil "org-secretary" "org-secretary.el" (0 0 0 0))
;;; Generated autoloads from org-secretary.el

(register-definition-prefixes "org-secretary" '("join" "org-sec-"))

;;;***

;;;### (autoloads nil "org-static-mathjax" "org-static-mathjax.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-static-mathjax.el

(register-definition-prefixes "org-static-mathjax" '("org-static-mathjax-"))

;;;***

;;;### (autoloads nil "org-sudoku" "org-sudoku.el" (0 0 0 0))
;;; Generated autoloads from org-sudoku.el

(register-definition-prefixes "org-sudoku" '("org-sudoku-"))

;;;***

;;;### (autoloads nil "org-toc" "org-toc.el" (0 0 0 0))
;;; Generated autoloads from org-toc.el

(autoload 'org-toc-show "org-toc" "\
Show the table of contents of the current Org-mode buffer.

\(fn &optional DEPTH POSITION)" t nil)

(register-definition-prefixes "org-toc" '("org-"))

;;;***

;;;### (autoloads nil "org-track" "org-track.el" (0 0 0 0))
;;; Generated autoloads from org-track.el

(autoload 'org-track-fetch-package "org-track" "\
Fetch Org package depending on `org-track-fetch-package-extension'.
If DIRECTORY is defined, unpack the package there, i.e. add the
subdirectory org-mode/ to DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'org-track-compile-org "org-track" "\
Compile all *.el files that come with org-mode.
Generate the autoloads file `org-loaddefs.el'.

DIRECTORY is where the directory org-mode/ lives (i.e. the
          parent directory of your local repo.

\(fn &optional DIRECTORY)" t nil)

(register-definition-prefixes "org-track" '("org-track-"))

;;;***

;;;### (autoloads nil "org-velocity" "org-velocity.el" (0 0 0 0))
;;; Generated autoloads from org-velocity.el

(register-definition-prefixes "org-velocity" '("org-velocity"))

;;;***

;;;### (autoloads nil "org-wikinodes" "org-wikinodes.el" (0 0 0 0))
;;; Generated autoloads from org-wikinodes.el

(register-definition-prefixes "org-wikinodes" '("org-wikinodes-"))

;;;***

;;;### (autoloads nil "orgtbl-sqlinsert" "orgtbl-sqlinsert.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from orgtbl-sqlinsert.el

(register-definition-prefixes "orgtbl-sqlinsert" '("orgtbl-"))

;;;***

;;;### (autoloads nil "ox-bibtex" "ox-bibtex.el" (0 0 0 0))
;;; Generated autoloads from ox-bibtex.el

(register-definition-prefixes "ox-bibtex" '("org-bibtex-"))

;;;***

;;;### (autoloads nil "ox-confluence" "ox-confluence.el" (0 0 0 0))
;;; Generated autoloads from ox-confluence.el

(register-definition-prefixes "ox-confluence" '("org-confluence-"))

;;;***

;;;### (autoloads nil "ox-deck" "ox-deck.el" (0 0 0 0))
;;; Generated autoloads from ox-deck.el

(register-definition-prefixes "ox-deck" '("org-deck-"))

;;;***

;;;### (autoloads nil "ox-extra" "ox-extra.el" (0 0 0 0))
;;; Generated autoloads from ox-extra.el

(register-definition-prefixes "ox-extra" '("org-" "ox-extras"))

;;;***

;;;### (autoloads nil "ox-freemind" "ox-freemind.el" (0 0 0 0))
;;; Generated autoloads from ox-freemind.el

(autoload 'org-freemind-export-to-freemind "ox-freemind" "\
Export current buffer to a Freemind Mindmap file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(register-definition-prefixes "ox-freemind" '("org-freemind-"))

;;;***

;;;### (autoloads nil "ox-groff" "ox-groff.el" (0 0 0 0))
;;; Generated autoloads from ox-groff.el

(register-definition-prefixes "ox-groff" '("org-groff-"))

;;;***

;;;### (autoloads nil "ox-koma-letter" "ox-koma-letter.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ox-koma-letter.el

(autoload 'org-koma-letter-export-as-latex "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org KOMA-LETTER Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-latex "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-pdf "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(register-definition-prefixes "ox-koma-letter" '("org-koma-letter-"))

;;;***

;;;### (autoloads nil "ox-rss" "ox-rss.el" (0 0 0 0))
;;; Generated autoloads from ox-rss.el

(autoload 'org-rss-export-as-rss "ox-rss" "\
Export current buffer to an RSS buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RSS Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-export-to-rss "ox-rss" "\
Export current buffer to an RSS file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-publish-to-rss "ox-rss" "\
Publish an org file to RSS.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(register-definition-prefixes "ox-rss" '("org-rss-"))

;;;***

;;;### (autoloads nil "ox-s5" "ox-s5.el" (0 0 0 0))
;;; Generated autoloads from ox-s5.el

(register-definition-prefixes "ox-s5" '("org-s5-"))

;;;***

;;;### (autoloads nil "ox-taskjuggler" "ox-taskjuggler.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ox-taskjuggler.el

(autoload 'org-taskjuggler-export "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-and-process "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file and process it.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return a list of reports.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-process-and-open "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-taskjuggler-export-and-process' and open the generated
reports with a browser.

If you are targeting TaskJuggler 2.4 (see
`org-taskjuggler-target-version') the processing and display of
the reports is done using the TaskJuggler GUI.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

(register-definition-prefixes "ox-taskjuggler" '("org-taskjuggler-"))

;;;***

;;;### (autoloads nil nil ("org-contrib-pkg.el" "org-contrib.el"
;;;;;;  "org-contribdir.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-contrib-autoloads.el ends here
