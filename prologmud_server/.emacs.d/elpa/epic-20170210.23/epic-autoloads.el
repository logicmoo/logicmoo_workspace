;;; epic-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "epic" "epic.el" (0 0 0 0))
;;; Generated autoloads from epic.el

(autoload 'epic-create-notebook "epic" "\
Create NAME notebook in Evernote.

\(fn NAME)" t nil)

(autoload 'epic-create-note-from-region "epic" "\
Create a note article of Evernote from the text between BEG to END.
 Set TITLE (string), NOTEBOOK (stirng), and TAGS (list of string)
 to the article, and store it to Evernote.

\(fn BEG END TITLE NOTEBOOK TAGS)" t nil)

(autoload 'epic-open-notebook-in-collection-window "epic" "\
Open Evernote notebook window specified by NOTEBOOK-NAME.

\(fn NOTEBOOK-NAME)" t nil)

(autoload 'epic-create-note-from-org-buffer "epic" "\
Export current org buffer into a note of Evernote in the form of HTML.
The original org buffer is also attached to the exported note.
Epic inserts the link string in the org buffer like:
  #+EPIC_LINK: evernote:///view/123456/s1/xxxxxxxx-xxxx-.../
to keep the correspondence with the previously exported note.
Using this link, epic will export to the same note as before (nullify the
previously exported note and re-export into it)." t nil)

(autoload 'epic-insert-selected-note-as-org-links "epic" "\
Capture selected notes in Evernote, and insert org-style links." t nil)

(autoload 'epic-helm "epic" "\
Insert or jump using helm.el package.
Insert the name of selected tag of Evernote with the prefix of `#'.
Insert the name of selected notebook of Evernote with the prefix of `@'.
Pop to Evernote App and open the selected notebook." t nil)

(autoload 'epic-mew-create-note "epic" "\
Import a mail article into the local Evernote app.
 The mail article must be selected and displayed
 by typing ``.'' (mew-summary-analyze-again) in the mew-summary buffer.

\(fn TITLE NOTEBOOK TAGS)" t nil)

(autoload 'epic-mew-forward-to-evernote "epic" "\
Foward a mail to Evernote with the original headers." t nil)

(register-definition-prefixes "epic" '("epic" "helm-c-source-evernote-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; epic-autoloads.el ends here
