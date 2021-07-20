;;; org-outlook-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-outlook" "org-outlook.el" (0 0 0 0))
;;; Generated autoloads from org-outlook.el

(autoload 'org-outlook-open "org-outlook" "\
Open the Outlook item identified by ID.  ID should be an Outlook GUID.

\(fn ID)" nil nil)

(autoload 'org-outlook-create-vbs "org-outlook" "\
Creates Visual Basic Code for Org-protocol" t nil)

(eval-after-load "org" '(org-add-link-type "outlook" 'org-outlook-open))

(autoload 'org-protocol-outlook "org-outlook" "\
Process an org-protocol://outlook:// style url.

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

This function detects the Message ID, Subject, Sender and
optional text separated by '/'.  For example either

org-protocol://outlook:/ID/Subject/SenderName/SenderAddress

or

org-protocol://outlook:/o/ID/Subject/SenderName/SenderAddress

works.

By default, it uses the character
`org-protocol-outlook-default-template-key', which should be associated
with a template in `org-capture-templates'.

To use this plugin:
- Copy the outlook macro (below) into outlook
- Modify the outlook capture template (o) to capture the email as
  a task. An example is below.


 (\"o\" \"org-outlook\" entry (file \"~/org/refile.org\") \"* TODO Email %c %?
  %i
  %U\" :clock-in t :clock-resume t)

You may also use the following placeholders

Placeholders Replacement 
%:link URL of the email
%:description The title of the message
%:title The title of the message 
%:initial Selected text.
%:sender Sender's name
%:sender-email Sender's Email

- (optional) Modify the folder/location that outlook moves mail into (moving
  mail off the server changes the message ID.  Once off the
  server, the ID remains the same unless you move it back...)
- (optional) Modify the capture template used (I use ``o'')
- (optional) Make the macro CreateTaskFromItem accessable
  anywhere from outlook by adding it to the quick access toolbar
  and/or the standard toolbar.

\(fn INFO)" nil nil)

(org-outlook-enable-msg-dnd)

(autoload 'org-protocol-do-outlook-capture "org-outlook" "\
Support `org-capture' and `org-remember' alike.
CAPTURE-FUNC is either the symbol `org-remember' or `org-capture'.

\(fn INFO CAPTURE-FUNC)" nil nil)

(eval-after-load "org-protocol" '(progn (if (not (boundp 'org-protocol-protocol-alist)) (setq org-protocol-protocol-alist nil)) (add-to-list 'org-protocol-protocol-alist '("outlook" :protocol "outlook" :function org-protocol-outlook :kill-client t))))

(register-definition-prefixes "org-outlook" '("org-"))

;;;***

;;;### (autoloads nil nil ("org-outlook-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-outlook-autoloads.el ends here
